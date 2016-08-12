(asdf:defsystem #:cl-async-twitter-test
  :description "Tests for cl-async-twitter"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async-twitter
               #:prove
               #+sbcl #:sb-cover)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module "test"
                :pathname "test"
                :components (#+sbcl (:file "test-with-coverage")
                             (:test-file "twitter-api-test")
                             (:test-file "message-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
