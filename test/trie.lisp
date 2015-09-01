(in-package :cl-handlers-test)

;; NOTE:
;; This file contains tests of cl-handlers internals. None of these functions are exported, and
;; are therefore always subject to change.
;;
;; Do not rely on their behavior.

(defun route-check (uri &rest handlers)
  (multiple-value-list
   (cl-handlers::trie-lookup
    (split-sequence:split-sequence #\/ uri :remove-empty-subseqs t)
    (reduce
     (lambda (memo sym)
       (cl-handlers::trie-insert!
        (split-sequence:split-sequence #\/ (symbol-name sym) :remove-empty-subseqs t)
        :handler-fn
        memo))
     handlers :initial-value (cl-handlers::make-trie)))))

(prove:suite
 (is (route-check "/A/B" 'a/b)
     '(:handler-fn ())
     "Purely static routes return the handler, with a NIL as extra bindings")

 (is (route-check "/a/b" 'a/b)
     '(:handler-fn ())
     "Static route segments are case-insensitive")

 (is (route-check "/A/2" 'a/-two)
     '(:handler-fn ((:two . "2")))
     "Routes with a variable bind its name to the appropriate path segment")

 (is (route-check "/A/testing" 'a/-two)
     '(:handler-fn ((:two . "testing")))
     "Variable route values preserve case")

 (is (route-check "/A/2" 'a/-two=integer)
     '(:handler-fn ((:two . "2")))
     "Variables with in-line types route properly")
 
 (is (route-check "/A/2/B/C" 'a/-two/b/-three)
     '(:handler-fn ((:three . "C") (:two . "2")))
     "Routes with multiple variables bind them all")

 (is (route-check "/A/2/B/sea" 'a/-two=integer/b/-three=string)
     '(:handler-fn ((:three . "sea") (:two . "2")))
     "Routes with multiple in-line-typed variables bind them all")

 (is (route-check "/NOPE" 'foo/bar)
     '(nil)
     "Mismatching handlers fail")

 (is (route-check "/FOO/2/BAR" 'foo/-two/c)
     '(nil)
     "Mismatching handlers fail, even if some prefix segments match")
 
 (is (route-check "/FOO/2/TEST"
                  'foo/-bar/nope
                  'foo/-baz/uhhh
                  'foo/-mumble/test)
     '(:handler-fn ((:mumble . "2")))
     "Matches on the prefix and postfix of a variable path")

 (is (route-check "/FOO/2/TEST/3/FOUR"
                  'foo/-bar/nope/-a/four
                  'foo/-baz/uhhh/-b/four
                  'foo/-mumble/test/-c/four)
     '(:handler-fn ((:c . "3") (:mumble . "2")))
     "Ditto on paths with multiple variables")

 (is (first
      (route-check "/FOO/2/TEST"
                   'foo/-bar/nope
                   'foo/-baz/uhhh
                   'foo/-mumble/test
                   'foo/-bar/test
                   'foo/-baz/test))
     :handler-fn
     "If there are AMBIGUOUS matches, one will be chosen, but which one is up to the fates"))
