#|
===============================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: cd.lisp

Description: Conceptual Dependency in Common Lisp.
See "Inside Computer Understanding " by Schank and 
Riesbeck for theoretical details.

bugs to vladimir dot kulyukin in canvas 
================================================
|#

(in-package  :user)

(defparameter *isa-hierarchy* nil)

(defun isa? (spec abst)
  (or (eql spec abst)
      (with-iterator (-*- (spec spec))
        (let ((absts (rest (assoc spec *isa-hierarchy*))))
          (or (find abst absts)
              (some #'-*- absts))))))

;;; ----------------------------------------------------------------------

(defun var? (x)
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

;;; > (new-cd 'ingest nil)
;;; (INGEST)
(defun new-cd (header role-fillers) (cons header role-fillers))

;;; > (setq cd1 '(INGEST :OBJECT (APPLE :REF (INDEF)) 
;;;                      :TIME (PAST) 
;;;                      :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE)))
;;; > (cd-header cd1)
;;; INGEST
(defun cd-header (cd) (car cd))
;;; > (cd-role-fillers cd1)
;;; (:OBJECT (APPLE :REF (INDEF)) :TIME (PAST) :ACTOR
;;;          (HUMAN :NAME (JOHN) :SEX (MALE)))
(defun cd-role-fillers (cd) (cdr cd))

;;; > (cd-roles cd1)
;;; (:OBJECT :TIME :ACTOR)
(defun cd-roles (cd)
  (do ((rf (cd-role-fillers cd) (cddr rf))
       (l '() (cons (car rf) l)))
      ((null rf) (nreverse l))))

;;; Example: 
;;; > (cd-role-filler '(ingest :actor ?actor :object ?object) :actor)
;;; ?actor
;;; > (cd-role-filler cd1 :actor)
;;; (HUMAN :NAME (JOHN) :SEX (MALE))
(defun cd-role-filler (cd role)
  (cadr (memq role (cd-role-fillers cd))))

;;; Example: 
;;; > (setq cd1 '(INGEST :OBJECT (APPLE :REF (INDEF)) 
;;;                      :TIME (PAST) 
;;;                      :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE)))
;;; > (cd-path-filler cd1 '(:object :ref))
;;; > (indef)
;;; > (cd-path-filler cd1 '(:actor :sex))
;;; > (male)

(defun cd-path-filler (cd path)
  (cond ((null cd) nil)
        ((null path) cd)
        (t
         (cd-path-filler (cd-role-filler cd (car path))
                         (cdr path)))))
;;; Example:
;;; > (cd-without-role cd1 :actor)
;;; (INGEST :OBJECT (APPLE :REF (INDEF)) :TIME (PAST))
(defun cd-without-role (cd role)
  "remove role from cd and return the cd w/o the remove role."
  (let ((sublist (memq role (cd-role-fillers cd))))
    (cond ((null sublist) cd)
          (t
           (nconc (ldiff cd sublist) (cddr sublist))))))

;;; Example:
;;; > (setq cd2 '(ingest :actor (human :name (john) :sex (male))))
;;; (INGEST :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE)))
;;; > (cd-with-role-filler cd2 :object '(apple :ref (indef)))
;;; (INGEST :OBJECT (APPLE :REF (INDEF)) :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE)))
(defun cd-with-role-filler (cd role filler)
  (let ((sublist (memq role (cd-role-fillers cd))))
    (cond ((null filler) (cd-without-role cd role))
          ((null sublist)
           (new-cd (cd-header cd)
                   (cons role (cons filler (cd-role-fillers cd)))))
          ((eq (cadr sublist) filler) cd)
          (t
           (nconc (ldiff cd sublist)
                  (cons role (cons filler (cddr sublist))))))))

(defun cd-with-path-filler (cd path filler)
  (with-iterator (-*- (cd cd) (path path))
    (cond ((null path) filler)
          (t
           (let* ((role (car path))
                  (fx (-*- (cd-role-filler cd role) (cdr path))))
             (cd-with-role-filler cd role fx))))))

;;; ----------------------------------------------------------------------

(defun new-bindings () (new-cd t '()))

;;; bdgs looks like (T ?ACT (INGEST :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE))) ?X (<CA>))
;;;(defun get-binding (bdgs var)
;;;  (cd-role-filler bdgs var))

(defun get-binding-val (bdgs var)
  "returns the value of var in bindings bdgs."
  (cd-role-filler bdgs var))

(defun with-binding (bdgs var value)
  (cd-with-role-filler bdgs var value))

;;; ----------------------------------------------------------------------

;;; if cd contains the role role,
;;; the filler is modified to be filler.
;;; Example: (cd-womp-role-filler! '(X :A ?A :B ?B) :A 'FOO)
;;;          '(X :A FOO :B ?B)
;;; > (cd-womp-role-filler! '(CD :ROLE1 ?R1 :ROLE2 ?R2) :R1 'FILLER1)
;;; (CD :R1 FILLER1 :ROLE1 ?R1 :ROLE2 ?R2)
(defun cd-womp-role-filler! (cd role filler)
  (setf (cdr cd) (cdr (cd-with-role-filler cd role filler)))
  cd)

(defun cd-womp-path-filler! (cd path filler)
  (cond ((null path) filler)
        (t
         (let ((role (car path)))
           (cd-womp-role-filler!
            cd role (cd-womp-path-filler!
                     (cd-role-filler cd role)
                     (cdr path) filler))))))

(defun womp-bindings! (old new)
  (setf (rest old) (rest new)))

;;; ----------------------------------------------------------------------

;;; bindings in bdgs look like
;;; (T ?ACT (INGEST :ACTOR (HUMAN :NAME (JOHN) :SEX (MALE))) ?X (<CA>))
#|
Example:
> (setf bindings 
       (cd-match '(ingest :actor ?actor :object ?obj :time ?t)
                 '(ingest :actor (human :name (jack) :sex (male)) 
                          :object (apple :ref (def)) :time (past)) 
                 (new-bindings)))
(T ?T (PAST) ?OBJ (APPLE :REF (DEF)) ?ACTOR (HUMAN :NAME (JACK) :SEX (MALE)))

> (cd-instantiate '(ingest :actor ?actor) bindings)
(INGEST :ACTOR (HUMAN :NAME (JACK) :SEX (MALE)))

> (cd-instantiate '(ingest :actor ?actor :time ?t) bindings)
(INGEST :ACTOR (HUMAN :NAME (JACK) :SEX (MALE)) :TIME (PAST))

> (cd-instantiate '(ingest :actor ?actor :time ?t :object ?obj) bindings)
(INGEST :ACTOR (HUMAN :NAME (JACK) :SEX (MALE)) 
        :TIME (PAST) 
        :OBJECT (APPLE :REF (DEF)))
|#

(defun cd-instantiate (pat bdgs)
  (cond 
   ((null pat) nil)
   ((var? pat)
    (cd-instantiate (cd-role-filler bdgs pat) bdgs))
   (t
    (new-cd (cd-header pat)
            (cd-instantiate-roles pat bdgs)))))

(defun cd-instantiate-roles (pat bdgs)
  (with-iterator 
   (-*- (roles (cd-roles pat)) (inst '()))
   (cond 
    ((null roles) (nreverse inst))
    (t
     (let* ((role (car roles))
            (filler (cd-instantiate (cd-role-filler pat role) bdgs)))
       (cond 
        ((null filler)
         (-*- (cdr roles) inst))
        (t
         (-*- (cdr roles) (cons filler (cons role inst))))))))))

;;; ----------------------------------------------------------------------

;;; examples:
;;; > (cd-match '(ingest :actor ?actor) '(ingest :actor (human :name (jack))) (new-bindings))
;;; (T ?ACTOR (HUMAN :NAME (JACK)))
;;; > (cd-match '(ingest :actor ?actor :object ?object)
;;;             '(ingest :actor (human :name (jack) :sex (male)) :object (apple :ref (indef)))
;;;             (new-bindings))
;;; (T ?OBJECT (APPLE :REF (INDEF)) ?ACTOR (HUMAN :NAME (JACK) :SEX (MALE)))
(defun cd-match (pat con bdgs)
  (cond 
   ((null con) bdgs)
   ((var? pat) (cd-match-var pat con bdgs))
   ((or (atom pat) (atom con))
    (error "pattern or constant is not a CD"))
   ((isa? (cd-header con) (cd-header pat))
    (cd-match-roles pat con bdgs))
   (t nil)))

(defun cd-match-var (var con bdgs)
  (let ((value (get-binding-val bdgs var)))
    (cond 
     ((null value)
      (with-binding bdgs var con))
     (t
      (let ((matched (cd-match value con bdgs)))
        (and matched
             (let ((merger (cd-merge value con)))
               (with-binding matched var merger))))))))

(defun cd-match-roles (pat con bdgs)
  (with-iterator
   (-*- (roles (cd-roles pat)) (bdgs bdgs))
   (cond 
    ((null bdgs) nil)
    ((null roles) bdgs)
    (t
     (let* ((role (car roles))
            (new-bdgs
             (cd-match (cd-role-filler pat role)
                       (cd-role-filler con role)
                               bdgs)))
       (-*- (cdr roles) new-bdgs))))))

;;; ----------------------------------------------------------------------

(defun cd-merge (cd1 cd2)
  (cond ((equal cd1 cd2) cd1)
  (t
   (multiple-value-bind (spec abst)
     (cond ((isa? (cd-header cd1) (cd-header cd2)) (values cd1 cd2))
           ((isa? (cd-header cd2) (cd-header cd1)) (values cd2 cd1))
           (t
            (error "cannot merge ~s and ~s" (cd-header cd1) (cd-header cd2))))
     (cond ((null (cd-role-fillers abst)) spec)
           ((null (cd-role-fillers spec))
            (if (eq (cd-header spec) (cd-header abst))
              abst
              (new-cd (cd-header spec) (cd-role-fillers abst))))
           (t
            (cd-merge-roles spec abst)))))))

(defun cd-merge-roles (spec abst)
  (let ((sroles (cd-roles spec))
        (aroles (cd-roles abst)))
    (do ((roles sroles (cdr roles))
         (cd spec (let* ((role (car roles))
                         (afiller (cd-role-filler abst role)))
                    (if (null afiller) cd
                        (let* ((sfiller (cd-role-filler spec role))
                               (filler (cd-merge sfiller afiller)))
                          (cd-with-role-filler cd role filler))))))
        ((null roles)
         (do ((roles (nset-difference aroles sroles) (cdr roles))
              (cd cd (let* ((role (car roles))
                            (filler (cd-role-filler abst role)))
                       (cd-with-role-filler cd role filler))))
             ((null roles) cd))))))

;;; ----------------------------------------------------------------------
;;; end of file
