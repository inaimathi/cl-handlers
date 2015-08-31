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
  (error (make-instance 'from-string-unknown-type :expected-type expected-type)))
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
       ,@(if params
             `((let ,(loop for (name type) in params
                        collect `(,name (string-> ,type (funcall ,param-table ,(intern (symbol-name name) :keyword)))))
                 ,@body))
             `((declare (ignore ,param-table))
               ,@body)))))

;;;;;;;;;; The handler table structure
;;;;; A minimal, custom Trie
;;;;;;;; (It needs to allow for variables at each level, including prospective matching of the rest of a URI segment)
(defstruct trie
  (value nil)
  (map (make-hash-table :test 'equal))
  (vars (make-hash-table)))

(defun any-vars? (trie)
  (> (hash-table-count (trie-vars trie)) 0))

(defun path-var? (str)
  (and (eql #\< (char str 0))
       (eql #\> (char str (- (length str) 1)))))

(defun var-sym (str)
  (intern (subseq (string-upcase str) 1 (- (length str) 1)) :keyword))

(defun trie-insert! (key value trie)
  (labels ((rec (key-parts trie)
             (cond ((null key-parts)
                    (setf (trie-value trie) value))
                   ((path-var? (first key-parts))
                    (next! (var-sym (first key-parts)) (rest key-parts) (trie-vars trie)))
                   (t
                    (next! (first key-parts) (rest key-parts) (trie-map trie)))))
           (next! (k rest map)
             (let ((next (gethash k map)))
               (if next
                   (rec rest next)
                   (rec rest (setf (gethash k map) (make-trie)))))))
    (rec key trie)
    trie))

(defun trie-lookup (key trie)
  (labels ((rec (key-parts trie bindings)
             (if key-parts
                 (let ((next (gethash (string-upcase (first key-parts)) (trie-map trie))))
                   (cond (next
                          (rec (rest key-parts) next bindings))
                         ((any-vars? trie)
                          (loop for k being the hash-keys of (trie-vars trie)
                             for v being the hash-values of (trie-vars trie)
                             do (multiple-value-bind (val bindings)
                                    (rec (rest key-parts) v (cons (cons k (first key-parts)) bindings))
                                  (when val
                                    (return-from trie-lookup (values val bindings))))))
                         (t
                          nil)))
                 (values (trie-value trie) bindings ))))
    (rec key trie nil)))

;;;;; And using it
(defparameter *handler-table* (make-trie))

(defun process-uri (uri)
  (split-sequence:split-sequence #\/ (symbol-name uri) :remove-empty-subseqs t))

(defun insert-handler! (uri handler-fn)
  (trie-insert! uri handler-fn *handler-table*)
  *handler-table*)

(defun find-handler (uri-string)
  (trie-lookup
   (split-sequence:split-sequence #\/ uri-string :remove-empty-subseqs t)
   *handler-table*))

(defun empty () (make-trie))

(defmacro with-handler-table (tbl &body body)
  `(let ((*handler-table* ,tbl))
     ,@body))

(defmacro define-handler ((uri) (&rest params) &body body)
  (let* ((processed (process-uri uri))
         (path-vars (loop for elem in processed
                       if (path-var? elem) collect (var-sym elem)))
         (param-names (mapcar (lambda (p) (intern (symbol-name (car p)) :keyword)) params)))
    (assert (null (set-difference path-vars param-names))
            nil "You haven't declared some of your path variables")
    (assert (equal param-names (remove-duplicates param-names))
            nil "You have some duplicate param-names")
    `(insert-handler! ',processed (make-handler ,params ,@body))))
