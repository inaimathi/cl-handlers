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
  (cl-handlers::make-handler () ((foo :integer) (bar :integer) (baz :integer))
    (+ foo bar baz))
  (LAMBDA ($param-table $header-cb $body-cb)
    (LET (($res-code 200)
	  ($res-headers (LIST :CONTENT-TYPE "text/plain")))
      (CL-HANDLERS::WITH-LOCAL-IGNORABLES ((CL-HANDLERS::REQUEST-HEADER
					    (CL-HANDLERS::NAME)
					    (FUNCALL $header-cb
						     CL-HANDLERS::NAME))
					   (CL-HANDLERS::READ-BODY! NIL
								    (FUNCALL $body-cb))
					   (CL-HANDLERS::SET-RESPONSE-CODE!
					    (CL-HANDLERS::CODE)
					    (SETF $res-code
                                                  CL-HANDLERS::CODE)
					    NIL)
					   (CL-HANDLERS::SET-RESPONSE-HEADER!
					    (CL-HANDLERS::NAME
					     CL-HANDLERS::VALUE)
					    (SETF (GETF $res-headers
							CL-HANDLERS::NAME)
                                                  CL-HANDLERS::VALUE)
					    NIL))
	(LET ((FOO (STRING-> :INTEGER (FUNCALL $param-table :FOO)))
	      (BAR (STRING-> :INTEGER (FUNCALL $param-table :BAR)))
	      (BAZ (STRING-> :INTEGER (FUNCALL $param-table :BAZ))))
	  (LET (($res-body (PROGN (+ FOO BAR BAZ))))
	    (LIST $res-code $res-headers (LIST $res-body)))))))
  "Make-handler expands as expected")

 ;; (is-expand
 ;;  (cl-handlers::make-handler () () "This is a Test")
 ;;  (LET (($res-code 200)
 ;;        ($res-headers (LIST :CONTENT-TYPE "text/plain")))
 ;;    (CL-HANDLERS::WITH-LOCAL-IGNORABLES ((CL-HANDLERS::REQUEST-HEADER
 ;;                                          (CL-HANDLERS::NAME)
 ;;                                          (FUNCALL $header-cb CL-HANDLERS::NAME))
 ;;                                         (CL-HANDLERS::READ-BODY! NIL (FUNCALL $body-cb))
 ;;                                         (CL-HANDLERS::SET-RESPONSE-CODE!
 ;;                                          (CL-HANDLERS::CODE)
 ;;                                          (SETF $res-code
 ;; 						CL-HANDLERS::CODE)
 ;;                                          NIL)
 ;;                                         (CL-HANDLERS::SET-RESPONSE-HEADER!
 ;;                                          (CL-HANDLERS::NAME
 ;;                                           CL-HANDLERS::VALUE)
 ;;                                          (SETF (GETF $res-headers CL-HANDLERS::NAME)
 ;; 						CL-HANDLERS::VALUE)
 ;;                                          NIL))
 ;;      (LET (($res-body (PROGN "This is a Test")))
 ;;        (LIST $res-code $res-headers (LIST $res-body)))))
 ;;  "make-handler expands to a let-less body that ignores its table argument when given no parameters")

 (is-expand
  (with-handler-table (empty)
    :test)
  (LET ((CL-HANDLERS::*HANDLER-TABLE* (EMPTY)))
    :TEST)
  "with-handler-table expands as expected"))
