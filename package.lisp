(defpackage #:nephilid
  (:use #:common-lisp)
  (:import-from #:easy-routes
                #:easy-routes-acceptor
                #:easy-routes-ssl-acceptor
                #:defroute)
  (:import-from #:spinneret
                #:with-html
                #:with-html-string)
  (:import-from #:ps-utils
                #:define-parenscript
                #:pflet)
  (:export #:initialize))

(defpackage #:nephilid-demo
  (:use #:common-lisp #:nephilid #:easy-routes #:spinneret))
