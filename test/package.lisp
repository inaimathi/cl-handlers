(defpackage #:cl-handlers-test
  (:use #:cl #:cl-handlers #:prove))

(defmacro prove:suite (&rest forms)
  "Hacking around the manual plan count and finalize call. 
When https://github.com/fukamachi/prove/issues/14 is addressed, I'll remove it."
  `(progn
     (prove:plan ,(length forms))
     ,@forms
     (prove:finalize)))
