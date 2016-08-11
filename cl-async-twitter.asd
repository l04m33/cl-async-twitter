(asdf:defsystem #:cl-async-twitter
  :description "Twitter client for Common Lisp and cl-async"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async
               #:cl-async-oauth
               #:blackbird
               #:babel
               #:cl-json
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "package")
                             (:file "twitter-api" :depends-on ("package")))))
  :in-order-to ((test-op (test-op #:cl-async-twitter-test))))
