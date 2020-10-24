(in-package #:nephilid-demo)

(defparameter *ws-acceptor* (make-instance 'hunchensocket:websocket-acceptor :name 'ws :port 1289))
(defparameter *http-acceptor* (make-instance 'easy-routes-acceptor :name 'http :port 8181))

(nephilid::initialize-server "/nephilid")

(nephilid::defaction test "#colour" ()
  (with-html-string
    (:p "Gamelan semar pegulingan")))

(defroute root ("/" :acceptor http) ()
  (with-html-string
    (:doctype)
    (:html
     (:head (:title "Nephilid demo")
            (:script (nephilid::initialize-action-client "ws://localhost:1289/nephilid")))
     (:body
      (:p "hello nephilid")
      (:span (:input :id "text"))
      (:span (:button :id "clickme"
                      :onclick $(test)
                      "Click me"))
      (:span (:button :id "colour"
                      "I should change colour :^)"))
      (:p "Gamelan ensembles are found predominantly on the islands of Java and Bali and comprise mainly bronze metallophones and gongs. They are led by drummers (one in Javanese ensembles, two in Balinese ensembles) who play double-headed drums and give rhythmic signals to the other performers regarding tempo and dynamic changes.  Gamelan has spread internationally since the 1960s and today it is played in many ensembles associated with university music departments and embassies around the world. The Indonesian and international gamelan scene features both traditional and contemporary music. It is also strongly associated with the sophisticated puppetry and dance traditions of Indonesia.")))))
