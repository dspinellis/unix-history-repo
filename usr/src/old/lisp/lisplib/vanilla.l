;; (c) copywrite 1982, Massachusetts Institute of Technology

;; This flavor system is derived from the original Lisp machine
;; flavor system.  As such its distribution may be restricted to
;; Lisp machine software license holders.

(environment-lmlisp (eval compile load) (files flavorm))

(setq |SCCS-vanilla| "@(#) vanilla.l	1.1	83/01/27 @(#)")

;This is a flavor which is automatically made a component of nearly all
;other flavors.  It provides some basic facilities such as PRINT
;and DESCRIBE.

(DEFFLAVOR SI:VANILLA-FLAVOR () ()
  :NO-VANILLA-FLAVOR  ;No instance variables, no other flavors
  (:DOCUMENTATION :MIXIN "The default base flavor.
This flavor provides the normal handlers for the :PRINT, :DESCRIBE, and :WHICH-OPERATIONS
operations.  Only esoteric hacks should give the :NO-VANILLA-FLAVOR option to DEFFLAVOR to
prevent this inclusion."))


(DEFMETHOD (SI:VANILLA-FLAVOR :PRINT-SELF) (STREAM &REST IGNORE)
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPEP)))

(DEFMETHOD (SI:VANILLA-FLAVOR :DESCRIBE) ()
  (FORMAT T "~&~S, an object of flavor ~S,~% has instance variable values:~%"
	    SELF (:TYPEP SELF))
  (DO ((IVARS (FLAVOR-ALL-INSTANCE-VARIABLES (INSTANCE-FLAVOR SELF))
	      (CDR IVARS))
       (I 0 (1+ I)))
      ((NULL IVARS))
; SMH@EMS VVV
;   (FORMAT T "~S~%" (%INSTANCE-REF SELF I))
    (FORMAT T " ~S:" (CAR IVARS))
    (MSG (|B| (MAX 1 (DIFF 30 (NWRITN)))))
    (FORMAT T "~S~%" (INT:FCLOSURE-STACK-STUFF (VREF SELF (+ 3 I))))
; SMH@EMS ^^^
))

;The default response to :WHICH-OPERATIONS is a list of all operations
;handled.  The list is consed up just once.  It is computed by examination
;of the method hash table, since that has no duplications.
;This goes to some pains to produce a cdr-coded list, for fast MEMQ'ing.
(DEFMETHOD (SI:VANILLA-FLAVOR :WHICH-OPERATIONS) ()
  (LET ((FL (INSTANCE-FLAVOR SELF)))
    (OR (FLAVOR-WHICH-OPERATIONS FL)
	(LET ((HT (FLAVOR-METHOD-HASH-TABLE FL))
	      W-O)
	  (DECLARE (SPECIAL W-O))
	  (MAPHASH #'(LAMBDA (OP IGNORE)
			     (DECLARE (SPECIAL W-O))
			     (PUSH OP W-O))
			 HT)
	  (SETF (FLAVOR-WHICH-OPERATIONS FL) W-O)
	  W-O))))

#-Franz
(DEFMETHOD (SI:VANILLA-FLAVOR :OPERATION-HANDLED-P) (OP)
  (LET ((FL (INSTANCE-FLAVOR SELF)))
    (IF (ARRAYP (FLAVOR-METHOD-HASH-TABLE FL))
	(MULTIPLE-VALUE-BIND (NIL DEFINEDP)
	    (WITHOUT-INTERRUPTS
	      (GETHASH OP (FLAVOR-METHOD-HASH-TABLE FL)))
	  DEFINEDP)
      (LET ((WO (OR (FLAVOR-WHICH-OPERATIONS FL) (FUNCALL-SELF ':WHICH-OPERATIONS))))
	(NOT (NOT (MEMQ OP WO)))))))

#+Franz					; 8Jul84 SMH@EMS
(DEFMETHOD (SI:VANILLA-FLAVOR :OPERATION-HANDLED-P) (OP)
	   (LET ((WO (OR (FLAVOR-WHICH-OPERATIONS (INSTANCE-FLAVOR SELF))
			 (FUNCALL-SELF ':WHICH-OPERATIONS))))
		(NOT (NOT (MEMQ OP WO)))))

#-Franz
(DEFMETHOD (SI:VANILLA-FLAVOR :SEND-IF-HANDLES) (OP &REST TO-SEND)
  (LET ((FL (INSTANCE-FLAVOR SELF)))
    (IF (ARRAYP (FLAVOR-METHOD-HASH-TABLE FL))
	(MULTIPLE-VALUE-BIND (FN-LOCATION DEFINEDP)
	    (GETHASH OP (FLAVOR-METHOD-HASH-TABLE FL))
	  (IF DEFINEDP (LEXPR-FUNCALL (CAR FN-LOCATION) OP TO-SEND)))
	(LET ((WO (OR (FLAVOR-WHICH-OPERATIONS FL)
		      (FUNCALL-SELF ':WHICH-OPERATIONS))))
	  (AND (MEMQ OP WO)
	       (LEXPR-FUNCALL-SELF OP TO-SEND))))))

#+Franz					; 8Jul84 SMH@EMS
(DEFMETHOD (SI:VANILLA-FLAVOR :SEND-IF-HANDLES) (OP &REST TO-SEND)
	   (LET ((WO (OR (FLAVOR-WHICH-OPERATIONS (INSTANCE-FLAVOR SELF))
			 (FUNCALL-SELF ':WHICH-OPERATIONS))))
		(AND (MEMQ OP WO)
		     (LEXPR-FUNCALL-SELF OP TO-SEND))))

(DEFMETHOD (SI:VANILLA-FLAVOR :GET-HANDLER-FOR) (OP)
  (GET-HANDLER-FOR SELF OP))

;Useful methods for debugging.
;They all cause the instance variables of SELF to be bound as specials.
(DEFMETHOD (SI:VANILLA-FLAVOR :EVAL-INSIDE-YOURSELF) (FORM)
  (EVAL FORM))

(DEFMETHOD (SI:VANILLA-FLAVOR :FUNCALL-INSIDE-YOURSELF) (FUNCTION &REST ARGS)
  (APPLY FUNCTION ARGS))

(DEFMETHOD (SI:VANILLA-FLAVOR :BREAK) ()
  (*BREAK T SELF))

;;; This flavor is a useful mixin that provides messages for a property list protocol.

(DEFFLAVOR SI:PROPERTY-LIST-MIXIN ((PROPERTY-LIST (LIST 'PROPERTY-LIST))) ()
  :SETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :MIXIN "A mixin that provides property list messages."))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :GET) (INDICATOR)
  (GET PROPERTY-LIST INDICATOR))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :GETL) (INDICATOR-LIST)
  (GETL PROPERTY-LIST INDICATOR-LIST))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :PUTPROP) (PROPERTY INDICATOR)
  (PUTPROP PROPERTY-LIST PROPERTY INDICATOR))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :REMPROP) (INDICATOR)
  (REMPROP PROPERTY-LIST INDICATOR))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :PUSH-PROPERTY) (PROPERTY INDICATOR)
  (PUSH PROPERTY (GET PROPERTY-LIST INDICATOR)))

(DEFMETHOD (SI:PROPERTY-LIST-MIXIN :PLIST) () PROPERTY-LIST)
