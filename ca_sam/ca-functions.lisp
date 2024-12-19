#|
==========================================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: ca-functions.lisp  
bugs to vladimir kulyukin in canvas.
==========================================================
|#

(in-package :user)

;;; ca-functions
;;; ----------------------------------------------------------------------
;;; Tests:
;;;
;;; (TRUE &optional test)
;;; (FALSE &optional test)
;;; (AND test ...)
;;; (OR test ...)
;;; (NOT test)
;;; (BOUND? var)
;;; (CLIST? var)
;;; (EQ? var1 var2)
;;; (MATCH? pattern var)
;;; (WORDS? word ...)
;;; (BEFORE limit var pattern)
;;; (AFTER limit var pattern)
;;; (PATTERN pattern ...)
;;;
;;; Actions:
;;;
;;; (NOP action)
;;; (SEQUENCE action ...)
;;; (IF test then-action &optional else-action)
;;; (MARK var)
;;; (WORDS word ...)
;;; (GOBBLE number)
;;; (REQUEST { (DONE test) } { (TEST test) } { (ACTIONS action ...) } )
;;; (CONCEPT var pattern)
;;; (REMOVE var)
;;; (MODIFY var role ... pattern role ...)
;;; 
;;; ----------------------------------------------------------------------
;;; TESTS
;;; ----------------------------------------------------------------------

(defconstant *ca-mark-header* '<CA>)

(defun ca-mark () (new-cd *ca-mark-header* nil))

(defun ca-mark? (cd) (eq (cd-header cd) *ca-mark-header*))

;;; ----------------------------------------------------------------------

(define-ca-test true (id b w c r &optional test)
  (or (and test (ca-test id test b w c r)) b))

(define-ca-test false (id b w c r &optional test)
  (if test (ca-test id test b w c r))
  nil)

;;; ----------------------------------------------------------------------

(define-ca-test and (id b w c r &rest clauses)
  (with-iterator
   (-*- (clauses clauses) (i 0) (bdgs b))
   (cond 
    ((null clauses) bdgs)
    (t
     (let ((bdgs1 (ca-test (cons i id) (car clauses) bdgs w c r)))
       (cond ((null bdgs) nil)
             (t
              (-*- (cdr clauses) (1+ i) bdgs1))))))))

(define-ca-test or (id b w c r &rest clauses)
   (with-iterator
    (-*- (clauses clauses) (i 0))
    (cond 
     ((null clauses) nil)
     (t
      (let ((bdgs (ca-test (cons i id) (car clauses) b w c r)))
        (cond 
         ((null bdgs) (-*- (cdr clauses) (1+ i)))
         (t bdgs)))))))

(define-ca-test not (id b w c r test)
  (let ((bdgs (ca-test id test b w c r)))
    (cond ((null bdgs) b)
          (t nil))))

;;; ----------------------------------------------------------------------

(define-ca-test bound? (id b w c r var &rest path)
  (declare (ignore id w c r))
  (if (cd-path-filler (get-binding-val b var) path) b nil))

(define-ca-test clist? (id b w c r var)
  (declare (ignore id w r))
  (if (memq (get-binding-val b var) c) b nil))

(define-ca-test eq? (id b w c r var1 var2)
  (declare (ignore id w c r))
  (if (eq (get-binding-val b var1) (get-binding-val b var2)) b nil))

(define-ca-test match? (id b w c r pattern var)
  (declare (ignore id w c r))
  (cd-match pattern (get-binding-val b var) b))

;;; > (every #'= '(1 2 3) '(1 2 3))
;;; T
;;; > (every #'= '(1 2 3 4) '(1 2 3))
;;; T
;;; > (every #'(lambda (x y) (< (+ x y) 10)) '(1 2 3) '(1 2 3))
(define-ca-test words? (id b w c r &rest words)
  (declare (ignore id c r))
  (if (and (>= (length w) (length words))
           (every #'eq w words))
    b nil))

;;; ----------------------------------------------------------------------

;;; id - the id of a request, e.g., (BEFORE TEST REQUEST 1 -EAT-)
;;; b  - bindings, e.g., (T ?ACT (INGEST))
;;; w  - words left to process, e.g., (an apple)
;;; c  - concept list, e.g., ((INGEST) (<CA>) (HUMAN :NAME (JACK) :SEX (MALE)))
;;; r  - request list, e.g., ((REQUEST 2 -EAT-) (REQUEST 1 -EAT-) (REQUEST 1 -PAST-))
;;; limit - the reference point at which the test starts, e.g., ?ACT
;;; var - variable, e.g., ?ACTOR
;;; pattern - a cd pattern, e.g., (HUMAN)
(define-ca-test before (id b w c r limit var pattern)
  (declare (ignore id w r))
  (let ((con (and limit (get-binding-val b limit))))
    ; (format t "~%before")
    ; (format t "~%id = ~s" id)
    ; (format t "~%b = ~s" b)
    ; (format t "~%w = ~s" w)
    ; (format t "~%c = ~s" c)
    ; (format t "~%r = ~s" r)
    ; (format t "~%limit = ~s" limit)
    ; (format t "~%var = ~s" var)
    ; (format t "~%pattern ~s" pattern)
    ; (format t "~%con = ~s" con)
    (with-iterator (-*- (cptr c))
      ;(format t "~%cptr = ~s" cptr)
      (cond 
	((null cptr) nil)
	;;; vladimir kulyukin: commented it out for now.
	;;;((eq (car cptr) con)
	;;; (format t "~%(eq (car ptr) con) is true")
	;;; nil)
	(t
	 (let ((bdgs (cd-match pattern (car cptr) b)))
	   (cond 
	     ((null bdgs) (-*- (cdr cptr)))
	     ((null var) bdgs)
	     (t
	      (with-binding bdgs var (car cptr))))))))))

;;; ----------------------------------------------------------------------

(define-ca-test after (id b w c r limit var pattern)
  (declare (ignore id w r))
  (let* ((con (and limit (get-binding-val b limit)))
         (cx (nreverse (ldiff c (memq con c)))))
    (with-iterator
     (-*- (cptr cx))
     (cond 
      ((null cptr) nil)
      (t
       (let ((bdgs (cd-match pattern (car cptr) b)))
         (cond 
          ((null bdgs) (-*- (cdr cptr)))
          ((null var) bdgs)
          (t
           (with-binding bdgs var (car cptr))))))))))

;;; ----------------------------------------------------------------------

(define-ca-test pattern (id b w c r &rest pattern)
  (declare (ignore id w r))
  (with-iterator
   (-*- (pat (reverse pattern))
        (i (1- (length pattern)))
        (cx c) (bx b))
   (cond 
    ((null pat) bx)
    ((null cx) nil)
    ((ca-mark? (car cx))
     (-*- pat i (cdr cx) bx))
    (t
     (let ((bdgs (cd-match (car pat) (car cx) bx)))
       (cond ((null bdgs) nil)
             (t
              (-*- (cdr pat) (1- i) (cdr cx)
                   (with-binding bdgs i (car cx))))))))))

;;; ----------------------------------------------------------------------
;;; ACTIONS
;;; ----------------------------------------------------------------------

;;; b = bindings
;;; w = words
;;; c = concept list
;;; r = request list
(define-ca-action nop (id b w c r &optional action)
  (declare (ignore id action))
  (values b w c r))

(define-ca-action sequence (id b w c r &rest actions)
  (ca-actions id actions b w c r))

(define-ca-action if (id b w c r test then &optional else)
  (let ((btest (ca-test (cons 'test id) test b w c r)))
    (cond (btest
           (womp-bindings! b btest)
           (ca-action (cons 'then id) then b w c r))
          (else
           (ca-action (cons 'else id) else b w c r))
          (t
           (values b w c r)))))

;;; ----------------------------------------------------------------------

(define-ca-action mark (id b w c r var)
  (declare (ignore id))
  (let ((mark (ca-mark)))
    (womp-bindings! b (with-binding b var mark))
    (values b w (cons mark c) r)))

;;; ----------------------------------------------------------------------

;;; the words are destructively added to the beginning of w.
;;; w remains unchanged. words is destructively modified.
(define-ca-action words (id b w c r &rest words)
  (declare (ignore id))
  (values b (nconc words w) c r))

;;; removes n first words from w.
(define-ca-action gobble (id b w c r n)
  (declare (ignore id))
  (values b (nthcdr n w) c r))

;;; ----------------------------------------------------------------------

(define-ca-action request (id b w c r &rest forms)
  (values b w c (add-request forms id b r)))

;;; ----------------------------------------------------------------------

;;; the concept action takes the following arguments:
;;; 1. id
;;; 2. bindings
;;; 3. words
;;; 4. concept list
;;; 5. request list
;;; 6. var
;;; 7. pattern
;;; 
;;; the concept action does the following:
;;; 1. the pattern is insantiated with bindings
;;; 2. the concept is added to the list of concepts
;;; 3. if var is given, with-binding adds the binding (var con) to
;;;    the bindings.
;;; For example, 
;;; (WITH-BINDING (T ?ACT (INGEST)) ?X (HUMAN :NAME (JACK) :SEX (MALE)))
;;; returns (T ?X (HUMAN :NAME (JACK) :SEX (MALE)) ?ACT (INGEST))
;;; figure out what womp-bindings does.

(define-ca-action concept (id b w c r var pattern)
  (declare (ignore id))
  (let* ((con (cd-instantiate pattern b))
         (cx (cons con c)))
    (if var (womp-bindings! b (with-binding b var con)))
    (values b w cx r)))

(define-ca-action remove (id b w c r var)
  (declare (ignore id))
  (values b w (remove (get-binding-val b var) c) r))

(define-ca-action modify (id b w c r var1 &rest args1)
  (declare (ignore id))
  (let* ((var2 (find-if-not #'keywordp args1))
         (args2 (memq var2 args1))
         (path1 (ldiff args1 args2))
         (path2 (cdr args2))
         (base2 (if (consp var2) 
		    (cd-instantiate var2 b) 
		  (get-binding-val b var2)))
         (cd2 (cd-path-filler base2 path2))
         (cd1 (cd-womp-path-filler! (get-binding-val b var1) path1 cd2)))
    (womp-bindings! b (with-binding b var1 cd1))
    (values b w c r)))

;;; ----------------------------------------------------------------------
;;; end of file
