#|
=============================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: ca-utilities.lisp
bugs to vladimir kulyukin in canvas 
=============================================
|#

(in-package :user)

(defmacro with-iterator ((name . init) &rest body)
  `(labels ((,name ,(mapcar #'car init) ,@body))
     (,name ,@(mapcar #'second init))))

(defun assq (x l) (assoc x l :test #'eq))

;;; (memq 1 '(1 2 3)) ==> (1 2 3)
;;; (memq 1 '(2 1 3)) ==> (1 3))
;;; (memq 1 '(2 1 3 1)) ==> (1 3 1)
(defun memq (x l) 
  "returns the sublist of l starting with the first
occurence of x in l; NIL otherwise"
  (member x l :test #'eq))

;;; ----------------------------------------------------------------------
;;; end of file
