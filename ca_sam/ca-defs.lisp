#|
==============================================
-*- Mode: Lisp; Syntax: Common-Lisp -*-

File: ca-defs.lisp
Author: Vladimir Kulyukin
Description: Word definitions for CA and SAM
===============================================
|#

(in-package :user)

(define-ca-word 
  jack
  (concept nil (human :name (jack))))

(define-ca-word
  john
  (concept nil (human :name (john))))

(define-ca-word
  ann
  (concept nil (human :name (ann))))

(define-ca-word 
  ate
  (concept ?act (ingest :time (past)))
  (request (test (before ?act ?actor (animate)))
           (actions (modify ?act :actor ?actor)))
  (request (test (after ?act ?food (food)))
	   (actions (modify ?act :object ?food))))

(define-ca-word
  bought
  ;;; your code here
  )

(define-ca-word
  kite
  ;;; your code here
  )

(define-ca-word
  store
  ;;; your code here
  )

(define-ca-word
  went
  (concept ?act (ptrans :time (past)))
  (request (test    (before ?act ?actor (animate)))
	   (actions (modify ?act :actor ?actor)
		    (modify ?act :object ?actor)))
  (request (test    (after ?to  ?loc (location)))
	   (actions (modify ?act :to ?loc)))
  (request (test (and (after ?act ?to (to))
		      (after ?to  ?loc (location))))
	   (actions (modify ?act :to ?loc))))

(define-ca-word
  restaurant
  (concept nil (restaurant)))

(define-ca-word
  home
  (concept nil (home)))

(define-ca-word
  to
  (concept nil (to)))

(define-ca-word 
  apple
  (concept nil (apple)))

(define-ca-word
  pear
  (concept nil (pear)))

(define-ca-word 
  an
  (mark ?x)
  (request (test (after ?x ?con (concept)))
	   (actions (modify ?con :ref (indef))))
  (request (test (after ?x ?con (concept)))
	   (actions (modify ?con :number (singular)))))

(define-ca-word
  a
  (mark ?x)
  (request (test (after ?x ?loc (location)))
	   (actions (modify ?loc :ref (indef))))
  (request (test (after ?x ?loc (location)))
	   (actions (modify ?loc :number (singular))))
  (request (test (after ?x ?obj (phys-obj)))
	   (actions (modify ?obj :ref (indef))))
  (request (test (after ?x ?obj (phys-obj)))
	   (actions (modify ?obj :number (singular)))))

(define-ca-word
  lobster
  (concept nil (lobster)))

(define-ca-word
  bought
  (concept ?act (atrans :time (past)))
  (request (test (before ?act ?actor (animate)))
           (actions (modify ?act :actor ?actor)))
  (request (test (after ?act ?object (phys-obj)))
           (actions (modify ?act :object ?object)))
  (request (test (after ?act ?recipient (animate)))
           (actions (modify ?act :to ?recipient))))

(define-ca-word
  kite
  (concept nil (kite)))

(define-ca-word
  store
  (concept nil (store)))

(setf *shopping-story*
 '((ann went to a store)
   (ann bought a kite)
   (ann went home)))
(defun define-shopping-knowledge ()
  "Sets up knowledge related to the shopping script."
  ;; Define movement (PTRANS) to locations, e.g., to and from store
  (setf (associated-script 'store) '$shopping)
  
  ;; Define knowledge for PTRANS actions for the shopping script
  (define-ca-word
    go
    (concept ?act (ptrans :time (present)))
    (request (test (before ?act ?actor (animate)))
             (actions (modify ?act :actor ?actor)))
    (request (test (after ?act ?loc (location)))
             (actions (modify ?act :to ?loc))))
  
  ;; Add knowledge related to the shopper's transaction (ATRANS)
  (define-ca-word
    purchase
    (concept ?act (atrans :time (present)))
    (request (test (before ?act ?actor (animate)))
             (actions (modify ?act :actor ?actor)))
    (request (test (after ?act ?object (phys-obj)))
             (actions (modify ?act :object ?object)))
    (request (test (after ?act ?recipient (animate)))
             (actions (modify ?act :to ?recipient))))
  
  ;; Define additional items if necessary
  (define-ca-word
    cashier
    (concept nil (person :role (cashier))))
  
  (define-ca-word
    shopper
    (concept nil (person :role (shopper))))
  
  ;; Set up the sequence for shopping
  (setf (events-script '$shopping)
        '((ptrans (:actor ?client)
                  (:object ?client)
                  (:to ?store)
                  (:time ?time))
          (mtrans (:actor ?client)
                  (:object (list :object (store-item)))
                  (:to ?client)
                  (:time ?time))
          (atrans (:actor ?client)
                  (:object ?item)
                  (:from ?store)
                  (:to ?client)
                  (:time ?time))
          (ptrans (:actor ?client)
                  (:object ?client)
                  (:from ?store)
                  (:to ?home)
                  (:time ?time))))
)

;;; end-of-file

