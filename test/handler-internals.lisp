(in-package :cl-handlers-test)

;; NOTE:
;; Some of this file tests internal components.
;;
;; Even in the case of exported symbols, it tests their internal
;; mechanics, rather than external behavior. 
;;
;; Do not rely on these equalities holding for your programs.

(prove:suite
 (is-expand
  (cl-handlers::make-handler ((foo :integer) (bar :integer) (baz :integer))
    (+ foo bar baz))
  (LAMBDA ($param-table)
    (LET ((FOO (STRING-> :INTEGER (FUNCALL $param-table :FOO)))
	  (BAR (STRING-> :INTEGER (FUNCALL $param-table :BAR)))
	  (BAZ (STRING-> :INTEGER (FUNCALL $param-table :BAZ))))
      (MAKE-INSTANCE 'CL-HANDLERS::RESPONSE :BODY (PROGN (+ FOO BAR BAZ)))))
  "Make-handler expands as expected")

 (is-expand
  (cl-handlers::make-handler () "This is a Test")
  (LAMBDA ($param-table)
    (DECLARE (IGNORE $param-table))
    (MAKE-INSTANCE 'CL-HANDLERS::RESPONSE :BODY (PROGN "This is a Test")))
  "make-handler expands to a let-less body that ignores its table argument when given no parameters")

 (is-expand
  (with-handler-table (empty)
    :test)
  (LET ((CL-HANDLERS::*HANDLER-TABLE* (EMPTY)))
    :TEST)
  "with-handler-table expands as expected"))
