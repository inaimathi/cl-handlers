(in-package :cl-handlers-test)

(defun found-fn? (route)
  (functionp (cl-handlers::find-handler route)))

(prove:suite
 (ok
  (with-handler-table (empty)
    (define-handler (a/b) () "Testing")
    (found-fn? "/a/b"))
  "Correctly route static URIs")

 (ok
  (with-handler-table (empty)
    (define-handler (a/<foo>/b) ((foo :integer))
      (* 2 foo))
    (found-fn? "/a/2/b"))
  "Correctly route variable URIs")

 (ok
  (with-handler-table (empty)
    (define-handler (a/<foo>/b/<bar>) ((foo :integer) (bar :integer))
      (+ foo bar))
    (found-fn? "/a/2/b/4"))
  "Correctly route variable URIs with multiple variables")

 (is-error
  (with-handler-table (empty)
    (define-handler (a/b) ((foo :integer) (foo :integer))
      (* 2 foo)))
  error
  "No duplicate parameters are allowed")

 (is-error
  (with-handler-table (empty)
    (define-handler (a/<foo>) () "Testing"))
  error
  "You have to declare path variables"))
