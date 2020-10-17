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
   (event-queue :initform nil
                :accessor client-event-queue))
  (:documentation "The WebSocket client connection to the browser."))

(defclass nephilid-event ()
  ((function :initarg :function
             :reader event-function)
   (params :initarg :params
           :reader event-params))
  (:documentation "Data structure for client/server communication.  To call a
function on the server, a client sends a serialized representation of a
nephilid-event. This contains the symbol of the function that should be called,
and a list of parameters to pass to the function."))

(defgeneric serialize-event (event)
  (:documentation "This will be JSON most likely, but could be XML or something else.")
  (:method ((event nephilid-event))
    (cl-json:encode-json-to-string event)))

(defun deserialize-event-json (string)
  "Convert a JSON string back into a nephilid-event"
  (let ((parsed (cl-json:decode-json-from-string string)))
    (make-instance 'nephilid-event
                   :function (read-from-string (alexandria:assoc-value parsed :function))
                   :params (alexandria:assoc-value parsed :params))))

(defgeneric call-event-function (event)
  (:documentation "Call the function with parameters specified in the event.")
  ;; This should make sure that only REGISTERED events are called, for security
  (:method ((event nephilid-event))
    (with-accessors ((f event-function) (p event-params)) event
      (if (registered-p f)
          (apply f p)
          (error "Client tried to call unregistered function ~a" f)))))

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
                    (call-event-function (deserialize-event-json message))
                  (json:json-syntax-error () nil))))
    (when result
      (hunchensocket::send-message client result))))

(define-parenscript send-message (message)
  (ps:chain *nephilid-event-client* (send (ps:lisp message))))

(define-parenscript initialize-event-client (uri)
  (ps:defvar *nephilid-event-client* (ps:new (-web-socket (ps:lisp uri))))
  (ps:with-slots (onopen onerror onmessage) *nephilid-event-client*
    (ps:setf onopen (ps:lambda () (ps:chain *nephilid-event-client* (send "Ping")))
             onerror (ps:lambda (error) (ps:chain console (log (+ "Websocket error: " error))))
             onmessage (ps:lambda (message) (ps:chain console (log (ps:@ message data)))))))

;; (define-parenscript define-ws-handler (name event function 
