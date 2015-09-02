;;;; cl-handlers.lisp

(in-package #:cl-handlers)

(defparameter *handler-table* nil)

;;;;;;;;;; Basic utility
(defun split-at (elem seq)
  (split-sequence:split-sequence elem seq :remove-empty-subseqs t))

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

(defun path-var? (str) (eql #\- (char str 0)))

(defun var-key (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (intern (car pair) :keyword)))

(defun trie-insert! (key value trie)
  (labels ((rec (key-parts trie)
             (cond ((null key-parts)
                    (setf (trie-value trie) value))
                   ((path-var? (first key-parts))
                    (next! (var-key (first key-parts)) (rest key-parts) (trie-vars trie)))
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
                 (values (trie-value trie) bindings))))
    (rec key trie nil)))

;;;;; And using it
(defparameter *handler-table* (make-trie))

(defun process-uri (uri)
  (split-at #\/ (symbol-name uri)))

(defun insert-handler! (uri handler-fn)
  (trie-insert! uri handler-fn *handler-table*)
  *handler-table*)

(defun find-handler (uri-string)
  (trie-lookup
   (split-at #\/ uri-string)
   *handler-table*))

(defun empty () (make-trie))

(defmacro with-handler-table (tbl &body body)
  `(let ((*handler-table* ,tbl))
     ,@body))

(define-condition untyped-parameter (error)
  ((param-name :initarg :param-name :initform nil :reader param-name))
  (:report (lambda (condition stream)
	     (format stream "Parameter ~s has no declared type" (param-name condition)))))

(define-condition parameter-type-mismatch (error)
  ((param-name :initarg :param-name :initform nil :reader param-name)
   (type-a :initarg :type-a :initform nil :reader type-a)
   (type-b :initarg :type-b :initform nil :reader type-b))
  (:report (lambda (condition stream)
	     (format stream "Declaring ~s to be of type ~a, but already declared to be of type ~a"
		     (param-name condition) (type-a condition) (type-b condition)))))

(defun no-dupes? (lst) 
  (equal lst (remove-duplicates lst)))

(defun parse-var (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (list (intern (car pair))
	  (when (second pair) (intern (second pair) :keyword)))))

(defmacro define-handler ((uri) (&rest params) &body body)
  (let* ((processed (process-uri uri))
	 (path-vars (loop for v in processed when (path-var? v) collect (parse-var v))))
    ;; TODO - check for valid type annotations (try to parse an empty string, and check for a from-string-unknown-type error
    ;; TODO - make the errors report the specific dupes (maybe craft a special error class too).
    (assert (no-dupes? (mapcar #'car path-vars))
	    nil "You have a duplicate path variable: ~s" (mapcar #'car path-vars))
    (assert (no-dupes? (mapcar #'car params))
	    nil "You have duplicate parameters: ~s" (mapcar #'car params))
    (let ((tbl (make-hash-table)))
      (loop for (name type) in (append path-vars params)
	 do (let ((tp (gethash name tbl)))
	      (cond ((null tp) (setf (gethash name tbl) type))
		    ((not (eq type tp))
		     (error 
		      (make-instance 
		       'parameter-type-mismatch
		       :param-name name :type-a type :type-b tp))))))
      `(insert-handler! 
	',processed 
	(make-handler 
	    ,(loop for k being the hash-keys of tbl
		for v being the hash-values of tbl
		when (null v) do (error (make-instance 'untyped-parameter :param-name k))
		collect (list k v))
	  ,@body)))))

;;;;;;;;;; Serving handlers
(defun process-params (params)
  (loop for pair in (split-at #\& params)
     collect (let ((split (split-at #\= pair)))
	       (cons (intern (string-upcase (first split)) :keyword)
		     (second split)))))

(defmethod serve ((server (eql :woo)))
  (woo:run
   (lambda (env)
     (multiple-value-bind (handler bindings) (find-handler (getf env :path-info))
       (if handler
	   (let ((params (append bindings (process-params (getf env :query-string))))
		 (method (getf env :request-method))
		 (headers (getf env :headers)))
	     (format t "~{~s~^  ~}~%" (list handler params method headers))
	     (list 200 '(:content-type "text/plain")
		   (list (funcall handler (lambda (k) (cdr (assoc k params)))))))
	   '(404 (:content-type "text/plain") ("Nope, not found...")))))))

;; (with-handler-table (empty)
;;   (define-handler (test) () "Hello world!")
;;   (define-handler (add) ((a :integer) (b :integer))
;;     (write-to-string (+ a b)))
;;   (serve :woo))
