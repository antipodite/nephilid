(defsystem :nephilid
  :version 0.0.1
  :description "An experimental web framework for Hunchentoot, using server-side
  rendering and the EventSource browser API."
  :license "GNU GPL v3"
  :depends-on ("hunchentoot"
               "hunchensocket"
               "spinneret"
               "cl-who"
               "easy-routes"
               "cl-json"
               "babel"
               "parenscript"
               "ps-utils"
               "alexandria"
               "str")
  :components ((:file "package")
               (:file "nephilid")))
