;;;; cl-handlers.lisp

(in-package #:cl-handlers)

(defparameter *handler-table* nil)

;;;;;;;;;; Basic parser stuff
(define-condition from-string-error (error)
  ((thing :initarg :thing :initform nil :reader thing)
   (expected-type :initarg :expected-type :initform nil :reader expected-type))
  (:report (lambda (condition stream)
	     (format stream "Failed to convert ~s to a ~a"
		     (thing condition) (expected-type condition)))))

(define-condition from-string-unknown-type (from-string-error) ()
  (:report (lambda (condition stream)
	     (format stream "Unknown type ~a" (expected-type condition)))))

(defmethod from-string (expected-type thing)
  (error (make-instance 'from-string--unknown-type :expected-type expected-type)))
(defmethod from-string ((expected-type (eql :string)) thing) thing)
(defmethod from-string ((expected-type (eql :integer)) thing) (parse-integer thing))
(defmethod from-string ((expected-type (eql :keyword)) thing) (intern thing :keyword))

(defun string-> (expected-type thing)
  (handler-case
      (from-string expected-type thing)
    (from-string-error (e) (error e))
    (error () (error (make-instance 'from-string-error :thing thing :expected-type expected-type)))))

;;;;;;;;;; Making a handler
(defmacro make-handler ((&rest params) &body body)
  (with-gensyms (param-table)
    `(lambda (,param-table)
       (let ,(loop for (name type) in params
		collect `(,name (string-> ,type (funcall ,param-table ,(intern (symbol-name name) :keyword)))))
	 ,@body))))

;; (make-handler ((foo :string) (bar :integer) (baz :integer) (mumble :integer))
;;   (declare (ignore foo))
;;   (+ bar baz mumble))

;;;;;;;;;; The handler table structure
;;;;; A simple Trie
(defun trie (val map) (list val map))
(defun empty-trie () (trie nil nil))
(defun trie-value (trie) (first trie))
(defun trie-table (trie) (second trie))
(defun trie-assoc (key-part trie)
  (cdr (assoc key-part (trie-table trie))))
(defun trie-lookup (key-parts trie)
  (if (null key-parts)
      (trie-value trie)
      (let ((next (trie-assoc (first key-parts) trie)))
	(when next
	  (trie-lookup (rest key-parts) next)))))
(defun trie-alist-insert (k trie alist)
  (cons (cons k trie)
	(remove k alist :key #'car)))
(defun trie-insert (key value trie)
  (if key
      (let* ((k (first key))
	     (next (trie-assoc k trie)))
	(trie (trie-value trie)
	      (trie-alist-insert
	       k 
	       (trie-insert 
		(rest key) value
		(or next (trie nil nil)))
	       (trie-table trie))))
      (trie value (trie-table trie))))

;;;;; And using it
(defparameter *handler-table* (empty-trie))

(defun process-uri (uri)
  (flet ((var? (str)
	   (and (eql #\< (char elem 0))
		(eql #\> (char elem (- (length elem) 1))))))
    (loop for elem in (split-sequence:split-sequence #\/ (symbol-name uri) :remove-empty-subseqs t)
       if (var? elem) collect (intern (subseq elem 1 (- (length elem) 1)) :keyword)
       else collect (intern elem))))

(defun insert-handler (table uri handler-fn)
  (trie-insert (process-uri uri) handler-fn table))
(defun insert-handler! (uri handler-fn)
  (setf *handler-table* (insert-handler *handler-table* uri handler-fn)))

(defun find-handler (table uri handler-fn))

(defmacro define-handler ((uri &key (handler-table '*handler-table*)) (&rest params) &body body)
  `(trie-insert ,handler-table ',uri (make-handler ,params ,@body)))

;; (define-handler (something/<foo>/<bar>/blah :handler-table *tbl*) ((foo :string) (bar :integer) (baz :integer) (mumble :integer))
;;   (declare (ignore foo))
;;   (+ bar baz mumble))
