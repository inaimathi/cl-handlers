;;;; cl-handlers.asd

(asdf:defsystem #:cl-handlers
  :description "Describe cl-handlers here"
  :author "inaimathi <leo.zovic@gmail.com>"
  :license "Expat <http://directory.fsf.org/wiki/License:Expat>"
  :serial t
  :depends-on (#:alexandria #:split-sequence)
  :components ((:module
                src :components
                ((:file "package")
                 (:file "cl-handlers")))))

(asdf:defsystem #:cl-handlers-test
  :description "Test suite for :cl-handlers"
  :author "inaimathi <leo.zovic@gmail.com>"
  :license "Expat <http://directory.fsf.org/wiki/License:Expat>"
  :serial t
  :depends-on (#:cl-handlers #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module 
                test :components
                ((:file "package")
                 (:test-file "trie")
                 (:test-file "handler-internals")
                 (:test-file "handlers"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
