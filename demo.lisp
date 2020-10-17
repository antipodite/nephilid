(in-package #:nephilid-demo)

(defparameter *ws-acceptor* (make-instance 'hunchensocket:websocket-acceptor :name 'ws :port 1289))
(defparameter *http-acceptor* (make-instance 'easy-routes-acceptor :name 'http :port 8181))

(nephilid::initialize-server "/nephilid")

(defroute root ("/" :acceptor http) ()
  (with-html-string
    (:doctype)
    (:html
     (:head (:title "Nephilid demo")
            (:script (nephilid::initialize-event-client "ws://localhost:1289/nephilid")))
     (:body
      (:p "hello nephilid")
      (:span (:button :onclick (nephilid::send-message "hello") "Click me"))
      (:span (:button "I should change colour :^)"))))))
