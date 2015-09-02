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
    (define-handler (a/-foo=integer/b) ()
      (* 2 foo))
    (found-fn? "/a/2/b"))
  "Correctly route variable URIs")

 (ok
  (with-handler-table (empty)
    (define-handler (a/-foo=integer/b/-bar=integer) ()
      (+ foo bar))
    (found-fn? "/a/2/b/4"))
  "Correctly route variable URIs with multiple variables")

 (ok
  (with-handler-table (empty)
    (define-handler (a/-foo) ((foo :integer)) foo)
    (found-fn? "/a/2"))
  "You can type path parameters with arg-style declarations")

 (ok
  (with-handler-table (empty)
    (define-handler (a/-foo=integer) ((foo :integer)) foo))
  "You can type path parameters with both inline and arg-style declarations")

 (is-error
  (with-handler-table (empty)
    (define-handler (a) ((foo :integer) (foo :integer)) foo))
  'error
  "No duplicate declarations are allowed")

 (is-error
  (with-handler-table (empty)
    (define-handler (a/-foo=integer/b/-foo=integer) ((foo :integer))
      foo))
  'error
  "NO duplicate declarations are allowed"))
