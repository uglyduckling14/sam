#|
=================================================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: ca.lisp
Authors: Charles Martin, Vladimir Kulyukin

bugs to vladimir dot kulyukin at aggiemail dot usu dot edu
=================================================================
|#

(in-package  :user)

(defparameter *ca-tracing* t)

(defun dbg-trace (ctrl &rest args)
  (when *ca-tracing*
    (let ((level *print-level*))
      (setf *print-level* 2)
      (apply #'format t ctrl args)
      (setf *print-level* level))))

;;; ----------------------------------------------------------------------

(defstruct (request (:print-function print-request))
  id
  done
  test
  actions
  bindings)

(defun print-request (request s d)
  (declare (ignore d))
  (prin1 (request-id request) s))

(defun add-request (forms id bdgs rlist)
  (cons 
   (make-request
    :id id
    :done    (cadr (assq 'done    forms))
    :test    (cadr (assq 'test    forms))
    :actions (cdr  (assq 'actions forms))
    :bindings bdgs)
   rlist))

;;; ----------------------------------------------------------------------

(defvar *ca-test-fns* (make-hash-table :test #'eql))

(defun get-test-fn (name)
  (multiple-value-bind (test-fn bool)
      (gethash name *ca-test-fns*)
    (if bool
        test-fn
      (error "invalid CA test ~s" name))))

(defmacro define-ca-test (name args &rest body)
  `(setf (gethash ',name *ca-test-fns*) #'(lambda ,args ,@body)))

;;; ----------------------------------------------------------------------

(defvar *ca-action-fns* (make-hash-table :test #'eql))

(defun get-action-fn (name)
  (multiple-value-bind (action-fn bool)
      (gethash name *ca-action-fns*)
    (if bool
        action-fn
      (error "invalid CA action ~s" name))))

(defmacro define-ca-action (name args &rest body)
  `(setf (gethash ',name *ca-action-fns*) #'(lambda ,args ,@body)))

;;; ----------------------------------------------------------------------

(defvar *ca-lexicon* (make-hash-table :test #'eql))

(defun ca-lexicon (word)
  (gethash word *ca-lexicon*))

(defmacro define-ca-word (word &rest actions)
  `(setf (gethash ',word *ca-lexicon*) ',actions))

(defun ca-erase-lexicon ()
  (clrhash *ca-lexicon*))

;;; ----------------------------------------------------------------------
  
(defun ca (words)
  (ca-read words '() '()))

;;; clist is a list of concepts
;;; rlist is a list of requests
;;; (defparameter *my-clist* nil)
;;; (defparameter *my-rlist* nil)
(defun ca-read (words clist rlist)
  (cond 
   ((null words)
    (dbg-trace "~&; ----- Done -----~%")
    (values clist rlist))
   (t
    (let ((word (first words)))
      (dbg-trace "~&; ----- Reading ~s -----~%" word)
      ;;; (format t "CLIST = ~s ~%" clist)
      ;;; (format t "RLIST = ~s ~%" rlist)
      (multiple-value-bind (b w c r)
          (ca-actions (list word) (ca-lexicon word) 
                      (new-bindings) (rest words) clist rlist)
        (declare (ignore b))
	;;; (format t "~%Processing requests~%")
	;;; (format t "CLIST = ~s ~%" c)
	;;; (format t "RLIST = ~s ~%" r)
	;;; (setq *my-clist* c)
	;;; (setq *my-rlist* r)
        (ca-requests w c r))))))

(defun ca-actions (word actions bdgs words clist rlist)
  (with-iterator (-*- (actions actions) (i 0) (bdgs bdgs)
                      (words words) (clist clist) (rlist rlist))
    (if (null actions)
        (values bdgs words clist rlist)
      (multiple-value-bind (xbdgs words clist rlist)
          (ca-action (cons i word) (first actions) bdgs words clist rlist)
        (womp-bindings! bdgs xbdgs)
        (-*- (rest actions) (1+ i)
             bdgs words clist rlist)))))

;;; the with-iterator example:
;;; > (with-iterator (-*- (x '(1 2 3))) 
;;;      (cond ((null x) t) 
;;;      (t (print (first x)) 
;;;         (-*- (rest x)))))
;;; 1 
;;; 2 
;;; 3 
;;; T

(defun ca-requests (words clist rlist)
  (with-iterator (-*- (more rlist))
    (cond 
      ((null more) (ca-read words clist rlist))
      (t
       (let* ((request (first more))
	      (id      (request-id request))
	      (bdgs    (request-bindings request))
	      (done    (request-done request))
	      (test    (request-test request))
	      (actions (request-actions request)))
	 (cond 
	   ((and done 
		 (ca-test (cons 'done id) done bdgs words clist rlist))
		 ;;; (format t "~%Removing ~s from rlist~%" request)
	    (ca-requests words clist (remove request rlist)))
	   (t
	    (let ((test-bdgs
		   (if test
		       (ca-test (cons 'test id) test bdgs words clist rlist)
		       bdgs)))
	      (cond 
		((null test-bdgs) (-*- (rest more)))
		(t
		 (womp-bindings! bdgs test-bdgs)
		 (multiple-value-bind (b w c r)
		     (ca-actions id actions bdgs
				 words clist (remove request rlist))
		   (declare (ignore b))
     		       ;;; (format t "~%Removing ~s from rlist~%" request)
		   (ca-requests w c r))))))))))))

;;; ----------------------------------------------------------------------

(defun ca-action (id action bdgs words clist rlist)
  (let ((name (first action)))
    (dbg-trace "~&; Action ~s: ~s" (reverse id) action)
    (let ((id (cons name id))
          (fn (get-action-fn name)))
      (apply fn id bdgs words clist rlist (rest action)))))

;;; before
;#'(LAMBDA (ID B W C R LIMIT VAR PATTERN) 
;    (DECLARE (IGNORE ID W R))
;    (LET ((CON (AND LIMIT (GET-BINDING B LIMIT))))
;      (WITH-ITERATOR (-*- (CPTR C))
;	(COND ((NULL CPTR) NIL) ((EQ (CAR CPTR) CON) NIL)
;	      (T
;	       (LET ((BDGS (CD-MATCH PATTERN (CAR CPTR) B)))
;		 (COND ((NULL BDGS) (-*- (CDR CPTR))) ((NULL VAR) BDGS)
;		       (T (WITH-BINDING BDGS VAR (CAR CPTR))))))))))

(defun ca-test (id test bdgs words clist rlist)
  (let ((name (first test)))
    (dbg-trace "~&; Test   ~s: ~s" (reverse id) test)
    (let ((id (cons name id))
          (fn (get-test-fn name)))
      (let ((result
             (apply fn id bdgs words clist rlist (rest test))))
        (dbg-trace (if result " succeeds" " fails"))
        result))))

;;; ----------------------------------------------------------------------
;;; Processing CDs for SAM

(let ((primitive-acts
       '(propel move ingest expel grasp ptrans atrans
	 speak attend mtrans mbuild)))

  (defun primitive-act-p (cd)
    (not (null (member (cd-header cd) primitive-acts 
		       :test #'eq))))

  )

(defun sents-to-cds (sents &optional (clist nil) (rlist nil))
  (multiple-value-bind (cl rl)
		       (process-sentences-aux sents clist rlist)
		       (print "======")
		       (print cl)
		       (print "======")
		       (mapcar #'ca-cd-to-sam-cd
			       (remove-if #'(lambda (x) (not (primitive-act-p x)))
					  cl))))
	    
(defun process-sentences-aux (sents &optional (clist nil) (rlist nil))
  (if (null sents)
      nil
      (multiple-value-bind (cl rl)
			   (ca-read (first sents) nil nil)
			   (print cl)
			   (append cl 
				   (process-sentences-aux (rest sents)
							  nil nil)))))

(defun ca-cd-to-sam-cd (cd)
  (let ((role-fillers (cd-role-fillers cd))
	(slots nil))
    (cond
      ((null role-fillers) (list (cd-header cd)))
      (t (dolist (role (cd-roles cd))
	   (let ((role-filler (memq role role-fillers)))
	     (setf slots 
		   (cons (list (first role-filler)
			       (ca-cd-to-sam-cd (second role-filler)))
			 slots))))
	 (cons (cd-header cd) slots)))))

;;; ----------------------------------------------------------------------
;;; end of file








