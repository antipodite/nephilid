(in-package #:nephilid-demo)

(defparameter *ws-acceptor* (make-instance 'hunchensocket:websocket-acceptor :name 'ws :port 1289))
(defparameter *http-acceptor* (make-instance 'easy-routes-acceptor :name 'http :port 8181))

(nephilid::initialize-server "/nephilid")

(defun change-text (selector)
  (cl-json:encode-json-to-string `((selector . ,selector)
                                   (content  . ,(format nil "crips")))))

(defroute root ("/" :acceptor http) ()
  (with-html-string
    (:doctype)
    (:html
     (:head (:title "Nephilid demo")
            (:script (nephilid::initialize-action-client "ws://localhost:1289/nephilid")))
     (:body
      (:p "hello nephilid")
      (:span (:button :id "clickme"
                      :onclick $(format t "sup G!!!")
                      "Click me"))
      (:span (:button :id "colour"`
                      "I should change colour :^)"))))))
