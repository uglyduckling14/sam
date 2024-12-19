#|
===========================================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: ca-loader.lisp

Bugs to vladimir kulyukin in canvas. 
===========================================================
|#

(in-package :user)

;;; change this to the directory where the ca files are.
(defparameter *ca-dir* "C:\\Users\\esper\\Downloads\\CS-5600\\hw09\\ca_sam\\")

(defparameter *files* '("ca-utilities" "cd" "ca" "ca-functions" "ca-lexicon" "for" "sam"))

(defun ca-loader (files &key (mode :lisp))
  (dolist (a-file files t)
    (load (concatenate 'string
            *ca-dir*
            a-file
            (ecase mode
              (:lisp ".lisp")
              (:cl  ".cl")
              (:fasl ".fasl"))))))
