#|
===========================================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: sam.lisp
Bugs to vladimir dot kulyukin via Canvas 
===========================================================
|#

(in-package :user)

(require :cd-functions "cd-functions")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Global Variables
;;;

(defvar *current-script* nil
  "The currently active script")

(defvar *possible-next-events* nil
  "Temporally ordered list of events which have not
been seen in the input yet.")

(defvar *data-base* nil
  "Pointer to CD database (a list of CDs)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Top Level Functions
;;;


(defun sam (cds)
  "Takes a list of CDs and passes each to PROCESS-CD.  In the end, 
the current script, if there is any, is added to the database and 
the database is pretty-printed."
  (clear-scripts)
  (user-trace "~%")
  (dolist (cd cds)
    (user-trace "~&Input is") (print-cd cd)
    (process-cd cd)
    (user-trace "~%"))
  (user-trace "~%Story done - final script header")
  (print-cd (add-cd *current-script*))
  (user-trace "~%Database contains:")
  (print-cd *data-base*)
  (values))

(defun process-cd (cd)
  (let ((rslt nil))
    (setq rslt (integrate-into-script cd))
    (cond
      (rslt rslt)
      (t 
       (setq rslt (suggest-new-script cd))
       (cond
	 (rslt rslt)
	 (t
	  (user-trace "~%Adding")
	  (print-cd (add-cd cd))
	  (user-trace "~%- not linked to any script")))))))

(defun clear-scripts ()
  "Resets all globals to NIL"
  (setq *data-base* nil)
  (setf *current-script* nil)
  (setf *possible-next-events* nil))

(defun add-cd (cd)
  "Adds a new CD to the end of the DB list"
  (setq *data-base* (cons-end *data-base* cd))
  cd)

(defun integrate-into-script (cd)
  "Attempts to integrate an incoming statement into the 
currently active script, by finding the first event in 
*possible-next-events* that matches the statement.  If
such an event is found, update the database."
  (do* ((new-bindings nil)
        (events *possible-next-events* (cdr events))
        (event (first events) (first events)))
      ((or (null event) new-bindings)
       new-bindings)
    (setq new-bindings (match event cd *current-script*))
    (when new-bindings
      (setq *current-script* new-bindings)
      (user-trace "~%Matches")
      (print-cd event)
      (add-script-info event))))

(defun add-script-info (position)
  "ADD-SCRIPT-INFO is given an event in a script (the 
one that matched the input in INTEGRATE-INTO-SCRIPT).
Each script event up through position is instantiated
and added to the database."
  (do* ((events *possible-next-events* (cdr events))
        (event (first events) (first events))
        (at-position-p nil))
      ((or (null event) at-position-p)
       (setf *possible-next-events* events))
    (user-trace "~%Adding script CD")
    (print-cd (add-cd (instantiate event *current-script*)))
    (setq at-position-p (equal event position))))

(defun suggest-new-script (cd)
  "Takes a CD form, adds it to the database, and checks 
the predicates of the form and its subforms until a link
to a script is found (if any).  Thus, in (PTRANS (ACTOR 
(PERSON)) (OBJECT (PERSON)) (TO (STORE))), the first 
script is found under STORE.  If there was a previous 
script, add it to the database before switching to another
script, but do not instantiate any events that were left 
in *possible-next-events*."
  (let ((new-script (find-script cd)))
    (when new-script
        (and *current-script* (add-cd *current-script*))
        (user-trace "~%New script ~s" new-script)
        (setq *current-script* (list new-script))
        (setq *possible-next-events* (events-script new-script))
        (integrate-into-script cd))))

(defun find-script (cd)
  (cond ((atom cd) (associated-script cd))
        (t (or (associated-script (header-cd cd))
               (let ((script nil))
                 (some #'(lambda (pair)
                           (setq script (find-script (filler-pair pair))))
                       (roles-cd cd))
                 script)))))

(defun print-cd (cd)
  "Pretty-prints a CD form indented 4 spaces."
  (user-trace "~&~4T~s~%" cd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Data Structures for Scripts
;;;

;; Script names are atoms with the EVENTS property 
;; of the atom pointing to a list of events.

(defun events-script (x)
  "Returns sequence of events stored under a script."
  (get x :events))

(defsetf events-script (script) (events)
  `(setf (get ,script :events) ,events))

(defun associated-script (x)
  "Returns script associated with predicate x."
  (get x :associated-script))

(defsetf associated-script (x) (script)
  `(setf (get ,x :associated-script) ,script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Example Scripts
;;;

;;; ================= RESTAURANT story and script ==================

(defparameter *restaurant-story-cds*
  '((ptrans 
     (:actor  (human (:name (jack))))
     (:object (human (:name (jack))))
     (:to     (restaurant))
     (:time   (past)))
    (ingest 
     (:actor  (human (:name (jack))))
     (:object (lobster))
     (:time   (past)))
    (ptrans 
     (:actor  (human (:name (jack))))
     (:object (human (:name (jack))))
     (:from   (restaurant))
     (:to     (home))
     (:time   (past)))))

(setf (events-script '$restaurant)
      '((ptrans (:actor ?client)
                (:object ?client)
                (:to ?restaurant)
	        (:time ?time))
	(ptrans (:actor ?client)
	        (:object ?client)
                (:to (table))
                (:time ?time))
        (mtrans (:actor ?client)
                (:object (menu))
	        (:to ?client)
                (:time ?time))
        (mbuild (:actor ?client)
                (:object (ingest (:actor ?client)
                                 (:object ?meal)))
                (:time ?time))
	(mtrans (:actor ?client)
	        (:object (ingest (:actor ?client)
				 (:object ?meal)))
	        (:to (server))
                (:time ?time))
	(ptrans (:actor (server))
	        (:object ?meal)
	        (:to ?client)
                (:time ?time))
        (ingest (:actor ?client)
                (:object ?meal)
                (:time ?time))
        (atrans (:actor ?client)
                (:object (money))
                (:from ?client)
                (:to ?restaurant)
                (:time ?time))
        (ptrans (:actor  ?client)
                (:object ?client)
                (:from   ?restaurant)
                (:to     ?elsewhere)
                (:time   ?time))))

(setf (associated-script 'restaurant) '$restaurant)

;;; ================= SHOPPING story and script ==================

(defparameter *kite-story-cds*
  '((ptrans 
     (:actor  (human (:name (ann)))
     (:object (human (:name (ann))))
     (:to     (store))
     (:time   (past))))
    (atrans 
     (:object (kite))
     (:to     (human (:name (ann))))
     (:time   (past)))
    (ptrans 
     (:actor  (human (:name (ann))))
     (:object (human (:name (ann))))
     (:to     (home))
     (:time   (past)))))

(setf (events-script '$SHOPPING)
      '(
         (ptrans (:actor ?shopper)
                 (:object ?shopper)
                 (:to (store))
                 (:time ?time))
         (atrans (:actor ?store)
                 (:object ?item)
                 (:from ?store)
                 (:to ?shopper)
                 (:time ?time))
         (ptrans (:actor ?shopper)
                 (:object ?shopper)
                 (:from (store))
                 (:to (home))
                 (:time ?time))
       )
)

(setf (associated-script 'store) '$shopping)



