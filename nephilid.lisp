(in-package #:nephilid)

(setf cl-who:*attribute-quote-char* #\")

(defparameter *resources* '())

(defparameter *allowed-fns* (make-hash-table)
  "Only functions that appear in this hash table are permitted to be
evaluated by client remote calls. When a function is defined with DEFREACTION
its naming symbol is registered here.")

(defun registered-p (symbol)
  (gethash symbol *allowed-fns*))

(defclass nephilid-resource (hunchensocket:websocket-resource)
  ((route :initarg :route :initform (error "A route must be specified") :reader resource-route))
  (:default-initargs :client-class 'nephilid-client)
  (:documentation ""))

(defmethod stop-clients ((resource nephilid-resource))
  (mapc (lambda (c) (setf (client-listening c) nil))
        (hunchensocket:clients resource)))

(defclass nephilid-client (hunchensocket:websocket-client)
  ((resource :initarg :resource
             :initform (car hunchensocket:*websocket-dispatch-table*))
   (listening :initform t
              :initarg :listening
              :accessor client-listening)
   (action-queue :initform nil
                :accessor client-action-queue))
  (:documentation "The WebSocket client connection to the browser."))

(defclass nephilid-action ()
  ((function :initarg :function
             :reader action-function)
   (params :initarg :params
           :reader action-params))
  (:documentation "Data structure for client/server communication.  To call a
function on the server, a client sends a serialized representation of a
nephilid-action. This contains the symbol of the function that should be called,
and a list of parameters to pass to the function."))

(defgeneric serialize-action (action)
  (:documentation "This will be JSON most likely, but could be XML or something else.")
  (:method ((action nephilid-action))
    (with-output-to-string (s-stream)
      (flet ((print-symbol-with-package (symbol)
               ;; Write string representation of symbol including package prefix
               ;; Thanks to phoe on #lisp for this trick
               (let ((*package* (find-package :keyword)))
                 (write-string (str:concat "\"" (prin1-to-string symbol) "\"")
                               s-stream))))
        (with-accessors ((f action-function) (p action-params)) action
          (json:with-object (s-stream)
            (json:as-object-member ('function s-stream) (print-symbol-with-package f))
            (json:encode-object-member 'params p s-stream)))))))

(defun deserialize-action-json (string)
  "Convert a JSON string back into a nephilid-action"
  (let ((parsed (cl-json:decode-json-from-string string)))
    (make-instance 'nephilid-action
                   :function (read-from-string (alexandria:assoc-value parsed :function))
                   :params (alexandria:assoc-value parsed :params))))

(defgeneric call-action-function (action)
  (:documentation "Call the function with parameters specified in the action.")
  ;; This should make sure that only REGISTERED functions are called, for security
  (:method ((action nephilid-action))
    (with-accessors ((f action-function) (p action-params)) action
      (if (registered-p f)
          (apply f p)
          (error "Client tried to call unregistered function ~a" f)))))

(defmacro defaction (name selector args &body body)
  (setf (gethash name *allowed-fns*) t)
  (let ((result (gensym)))
    `(defun ,name ,args
       (let ((,result ,@body))
         (make-instance 'nephilid-reaction
                        :selector ,selector
                        :content ,result)))))

;;; Allow calls to remote lisp functions to be embedded in generated HTML
;;; as JSON objects that will be sent to the server to be invoked

(defun remote-call-reader (stream char)
  (declare (ignore char))
  (format t "Current package: ~a~&" *package*)
  (list (quote embed-action) (read stream t nil t)))

(set-macro-character #\$ 'remote-call-reader)

(defmacro embed-action (expr)
  (let ((fn     (car expr))
        (params (cdr expr)))
    (unless (registered-p fn)
      (error "~a is not a registered action. Embedded function calls must be ~
              defined with DEFACTION" fn))
    (alexandria:with-gensyms (json call)
      `(let ((,json (serialize-action (make-instance 'nephilid-action
                                                     :function ',fn
                                                     :params ',params)))
             ;; Need to interpolate json into JS method call like this
             ;; so Parenscript doesn't re-escape string delimiters
             (,call (ps:ps (ps:chain *nephilid-action-client*
                                     (send "~a")))))
         (format nil ,call ,json)))))

;;;
;;; Action functions called on the server send reactions to the client.
;;;

(defclass nephilid-reaction ()
  ((selector :initform nil
             :initarg :selector
             :accessor reaction-selector)
   (content :initform nil
            :initarg :content
            :accessor reaction-content))
  (:documentation "Holds the output and destination of a function call on the server.
When the client receives a serialized nephilid-reaction, it looks at the selector and
decides what to do with the content, for example replacing the innerHTML of element(s)
indicated by the selector with the content."))

(defgeneric serialize-reaction (reaction)
  (:documentation "Convert reaction to serialized format, usually JSON")
  (:method ((reaction nephilid-reaction))
    (cl-json:encode-json-to-string reaction)))

(defun initialize-server (root-route)
  (let ((resource (make-instance 'nephilid-resource :route root-route)))
    (pushnew resource *resources*)
    (pushnew (lambda (request)
               (find (hunchentoot:script-name* request)
                     *resources* :test #'string= :key #'resource-route))
             hunchensocket:*websocket-dispatch-table*)))

(defmethod hunchensocket:text-message-received ((resource nephilid-resource) client message)
  "All messages sent from the client should be in the same format: "
  (let ((result (call-action-function (deserialize-action-json message))))
    (if (eq (type-of result) 'nephilid-reaction)
        (hunchensocket::send-message client (serialize-reaction result))
        (error "Expected NEPHILID-REACTION, got ~a" (type-of result)))))

(defmethod hunchensocket:client-connected ((resource nephilid-resource) client)
  (hunchensocket::send-message client
                               (serialize-reaction (make-instance 'nephilid-reaction
                                                                  :content "Nephilid 0.1.0"
                                                                  :selector nil))))

;;;
;;; Client-side machinery
;;;

(defaction initial nil ()
  "Nephilid 0.1.0")

(define-parenscript process-reaction ()
  (ps:lambda (reaction)
    (ps:let ((element  (ps:chain document (get-element-by-id (ps:@ reaction selector))))
             (content  (ps:@ reaction content)))
      (ps:setf (ps:@ element |innerHTML|) content))))
  
(define-parenscript initialize-action-client (uri)
  (ps:defvar *nephilid-action-client* (ps:new (-web-socket (ps:lisp uri))))
  (ps:with-slots (onopen onerror onmessage) *nephilid-action-client*
    (ps:setf onerror (ps:lambda (error) (ps:chain console (log (+ "Websocket error: " error))))
             onmessage (ps:lambda (reaction)
                         (ps:chain console (log reaction))
                         (ps:let* ((json     (ps:chain |json| (parse (ps:@ reaction data))))
                                   (selector (ps:@ json selector))
                                   (content  (ps:@ json content)))
                           (if selector
                               (ps:setf (ps:@ (ps:chain document
                                                        (query-selector selector))
                                              |innerHTML|)
                                        content)
                               (ps:chain console (log content))))))))
