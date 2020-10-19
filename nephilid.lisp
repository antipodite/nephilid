(in-package #:nephilid)

(setf cl-who:*attribute-quote-char* #\")

(defparameter *resources* '())
(defparameter *allowed-fns* (make-hash-table))

(defun registered-p (symbol) t)
  ;(gethash symbol *allowed-fns*))

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
    (cl-json:encode-json-to-string action)))

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

(defclass nephilid-reaction ()
  ((selector :initform nil
             :initarg :selector
             :accessor reaction-selector)
   (content :initform nil
            :initarg :content
            :accessor reaction-content)))

(defgeneric serialize-reaction (reaction))

(defmethod serialize-reaction ((reaction nephilid-reaction))
  (cl-json:encode-json-to-string reaction))

(defun initialize-server (root-route)
  (let ((resource (make-instance 'nephilid-resource :route root-route)))
    (pushnew resource *resources*)
    (pushnew (lambda (request)
               (find (hunchentoot:script-name* request)
                     *resources* :test #'string= :key #'resource-route))
             hunchensocket:*websocket-dispatch-table*)))

(defmethod hunchensocket:client-connected ((resource nephilid-resource) client)
  (hunchensocket::send-message client "Nephilid 0.1.0"))

(defmethod hunchensocket:text-message-received ((resource nephilid-resource) client message)
  "All messages sent from the client should be in the same format: "
  (let ((result (handler-case
                    (call-action-function (deserialize-action-json message))
                  (json:json-syntax-error () nil))))
    (when result
      (hunchensocket::send-message client result))))

(define-parenscript send-message (message)
  (ps:chain *nephilid-action-client* (send (ps:lisp message))))

;;; Allow calls to remote lisp functions to be embedded in generated HTML
;;; as JSON objects that will be sent to the server to be invoked

(defun remote-call-reader (stream char)
  (list (quote build-remote-call) (read stream t nil t)))

(set-macro-character #\$ 'remote-call-reader)

(defmacro build-remote-call (expr)
  (let ((fn (write-to-string (car expr)))
        (params (cons 'ps:array (cdr expr))))
    `(ps:ps
       (ps:chain *nephilid-action-client*
                 (send (ps:chain |json|
                                 (stringify (ps:create "function" (ps:lisp ,fn)
                                                       "params"   (ps:lisp ',params)))))))))
  
(define-parenscript initialize-action-client (uri)
  (ps:defvar *nephilid-action-client* (ps:new (-web-socket (ps:lisp uri))))
  (ps:with-slots (onopen onerror onmessage) *nephilid-action-client*
    (ps:setf onopen (ps:lambda () (ps:chain *nephilid-action-client* (send "Ping")))
             onerror (ps:lambda (error) (ps:chain console (log (+ "Websocket error: " error))))
             onmessage (ps:lambda (message) (ps:chain console (log (ps:@ message data)))))))


