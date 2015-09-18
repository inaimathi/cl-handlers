;;;; cl-handlers.lisp
(in-package #:cl-handlers)

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
;; TODO - we can optimize a lot of the internals here based on what body actually ends up being
;;        eg. don't bother declaring intermediate vars and setters if they're not used.

(defun local-symbol (sym)
  (intern (symbol-name sym)))

(defmacro with-local-ignorables ((&rest functions) &body body)
  `(flet ,(loop for (name args body) in functions
	     collect `(,(local-symbol name) ,args ,body))
     (declare (ignorable
	       ,@(loop for (name args body) in functions
		    append `(#',(local-symbol name)))))
     ,@body))

(defmacro make-handler ((&key (content-type "text/plain")) (&rest params) &body body)
  (with-gensyms (param-table body-cb header-cb res-code res-headers)
    `(lambda (,param-table ,header-cb ,body-cb)
       (let ((,res-code 200)
	     (,res-headers (list :content-type ,content-type)))
	 (with-local-ignorables
	     ((request-header (name) (funcall ,header-cb name))
	      (read-body! () (funcall ,body-cb))
	      (set-response-code! (code)
		;; TODO - validate somehow
		(setf ,res-code code)
		nil)
	      (set-response-header! (name value)
		;; TODO -validate somehow
		(setf (getf ,res-headers name) value)
		nil))
	   ,(if params
		`(let ,(loop for (name type) in params
			  collect `(,name (string-> ,type (funcall ,param-table ,(intern (symbol-name name) :keyword)))))
		   (let ((res (progn ,@body)))
		     (list ,res-code ,res-headers (list res))))
		`(let ((res (progn ,@body)))
		     (list ,res-code ,res-headers (list res)))))))))

;;;;;;;;;; Handler definition
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

(defmacro define-handler ((uri &key (method :get) (content-type "text/plain")) (&rest params) &body body)
  (let* ((processed (process-uri uri))
	 (path-vars (loop for v in processed when (path-var? v) collect (parse-var v))))
    (assert (member method '(:get :post :put :delete :head))
	    nil "~s is not one of ~s" method '(:get :post :put :delete :head))
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
	',(cons method processed)
	(make-handler
	    (:content-type ,content-type)
	    ,(loop for k being the hash-keys of tbl
		for v being the hash-values of tbl
		when (null v) do (error (make-instance 'untyped-parameter :param-name k))
		collect (list k v))
	  ,@body)))))

(defun define-error-handler (response-code body &key (content-type "text/plain"))
  (assert (and (numberp response-code) (or (>= 417 response-code 400) (>= 505 response-code 500))) nil 
	  "The response-code must be a number specifying a code 400/500 error")
  (insert-error! response-code (list response-code (list :content-type content-type) (list body))))

;;;;;;;;;; Serving handlers
;;;;; General serving utility
(defun process-params (params)
  (loop for pair in (split-at #\& params)
     collect (let ((split (split-at #\= pair)))
	       (cons (intern (string-upcase (first split)) :keyword)
		     (second split)))))

(defun handle-body (method content-type content-length stream)
  (if (and (eq method :post) content-length (> content-length 0))
      (flet ((read! ()
	       (let ((seq (make-string content-length)))
		 (read-sequence seq stream)
		 seq)))
	(if (string= content-type "application/x-www-form-urlencoded")
	    (values 
	     (process-params (read!))
	     (constantly ""))
	    (values nil #'read!)))
      (values nil (constantly ""))))

(defun make-app (&key (handler-table *handler-table*))
  (lambda (env)
    (let ((method (getf env :request-method))
	  (uri (getf env :path-info))
	  (params (process-params (getf env :query-string)))
	  (headers (getf env :headers)))
      (multiple-value-bind (handler extra-bindings) (find-handler method uri :handler-table handler-table)
	(if handler
	    (multiple-value-bind (post-params body-cb) 
		(handle-body 
		 method 
		 (getf env :content-type)
		 (getf env :content-length)
		 (getf env :raw-body))
	      (handler-case
		  (let ((processed (append extra-bindings params post-params)))
		    (funcall
		     handler
		     (lambda (k) (cdr (assoc k processed)))
		     (lambda (k) (gethash (string-downcase (symbol-name k)) headers))
		     body-cb))
		(from-string-error () (find-error 400 :handler-table handler-table))
		(error () (find-error 500 :handler-table handler-table))))
	    (find-error 404 :handler-table handler-table))))))


;;;;;;;;;; Testing stuff
;; (with-handler-table (empty)
;;   (define-error-handler 404 "Bwowwoddawowow! Nothing fucking here!")
;;   (define-handler (test) () "Hello world!")
;;   (define-handler (add) ((a :integer) (b :integer))
;;     (write-to-string (+ a b)))
;;   (define-handler (add :method :post) ((a :integer) (b :integer))
;;     (write-to-string (+ a b)))
;;   (define-handler (add/-a=integer/-b=integer) ()
;;     (write-to-string (+ a b)))
;;   ;; (woo:run (make-app) :port 5000 :worker-num 5)
;;   ;; (clack:clackup (make-app) :server :hunchentoot :port 5000 :use-thread nil)
;;   )
