;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; for macro for SAM

(in-package :user)

(provide :for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;    The FOR Macro
;;

;;; (FOR &rest <for-clauses>) : Macro
;;;   Simplifies syntax for mapping over lists.  Syntax is as
;;; follows:
;;;
;;;   (FOR ((<variable1 :IN <list1>)
;;;         (<variable2 :IN <list2>)
;;;         ...)
;;;     :WHEN <when-expression>
;;;     <FOR-keyword> <expr1> <expr2> ...)
;;;
;;;   The execution of FOR is as follows:
;;;
;;;   1) Evaluate each list(i) form
;;;
;;;   2) Assign the car of each list to the corresponding
;;;      variable(i) and evaluate the body
;;;
;;;   3) Repeat step 2 on the cdr's of list(i) until some list(i)
;;;      runs out of elements or the value of the FOR is determined.
;;;      The table below shows the final return values for each call
;;;      to FOR.
;;;
;;;   4) If there is a :WHEN, then, on each iteration, evaluate the
;;;      expression after the :WHEN and before the <FOR-keyword>.
;;;      If the value of the :WHEN expression is true, evaluate the
;;;      body of the FOR, otherwise skip to the next iteration.
;;;
;;;   FOR-keyword | value returned from FOR
;;;   ============================================================
;;;   :ALWAYS     | true if all values of body are true
;;;   :DO         | implementation dependent (MAPC in CL)
;;;   :FILTER     | a list of non-NIL values of body
;;;   :FIRST      | the first non-NIL value of body
;;;   :SAVE       | a list of all the values of body
;;;   :SPLICE     | a list of all values of body, APPENDed together

(defvar *for-keys* (make-hash-table))

(defun for-key (key) 
  (gethash key *for-keys*))

(defsetf for-key (key) (proc)
  `(setf (gethash ,key *for-keys*) ,proc))

(defmacro for (&rest for-clauses)
  (let ((when-part (member ':when for-clauses)))
    (for-expander (for-var-forms for-clauses)
                  (and when-part (cadr when-part))
                  (for-body for-clauses))))

(defun for-var-forms (l)
  (and l 
       (listp (car l))
       (cons (car l) (for-var-forms (cdr l)))))

(defun for-body (l)
  (and l
       (or (and (for-key (car l)) l)
           (for-body (cdr l)))))

(defun for-expander (var-forms when-form body-forms)
  (assert (not (null var-forms)) () "FOR requires at least one variable")
  (assert (not (null body-forms)) () "FOR requires a body")
  (let ((vars (mapcar #'car var-forms))
        (lists (mapcar #'caddr var-forms))
        (mapfn-body (funcall (for-key (car body-forms))
                             when-form
                             `(progn ,@(cdr body-forms)))))
    `(,(car mapfn-body)
      #'(lambda ,vars ,(cadr mapfn-body))
      ,@lists)))

(defmacro define-for-key (key vars mapfn body)
  `(progn (setf (for-key ',key)
                #'(lambda ,vars (list ,mapfn ,body)))
          ',key))

;;;
;;; Define FOR keyword forms
;;;

(define-for-key :always (test body) 'every
  (cond (test `(or (not ,test) ,body))
        (t body)))

(define-for-key :do (test body) 'mapc
  (cond (test `(and ,test ,body))
        (t body)))

(define-for-key :filter (test body) 'mapcan
  (let ((fbody `(let ((x ,body)) (and x (list x)))))
    (cond (test `(and ,test ,fbody))
          (t fbody))))

(define-for-key :first (test body) 'some
  (cond (test `(and ,test ,body))
        (t body)))

(define-for-key :save (test body)
  (cond (test 'mapcan)
        (t 'mapcar))
  (cond (test `(and ,test (list ,body)))
        (t body)))

(define-for-key :splice (test body) 'mapcan
  `(copy-list
    ,(cond (test `(and ,test ,body))
           (t body))))
