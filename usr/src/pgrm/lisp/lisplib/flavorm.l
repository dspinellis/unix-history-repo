;; (c) Copywrite 1983, Massachusetts Institute of Technology
(setq rcs-flavorm-
   "$Header: flavorm.l,v 1.2 85/03/24 11:25:34 sklower Exp $")

;; This file contains some of the support macros that are need by the
;; flavor system.

(environment-maclisp)
(declare (macros t))

; The data-structure on the FLAVOR property of a flavor-name
(DEFSTRUCT (FLAVOR :NAMED)
  FLAVOR-BINDINGS		;List of locatives to instance variable
				; internal value cells.  MUST BE CDR-CODED!!
				;Fixnums can also appear.  They say to skip
				;whatever number of instance variable slots.
  FLAVOR-METHOD-HASH-TABLE	;The hash table for methods of this flavor.
				; NIL means method-combination not composed yet.
  FLAVOR-NAME			;Symbol which is the name of the flavor.
				; This is returned by TYPEP.
  FLAVOR-LOCAL-INSTANCE-VARIABLES	;Names and initializations,
					; does not include inherited ones.
  FLAVOR-ALL-INSTANCE-VARIABLES	;Just names, only valid when "flavor 
				; combination" composed.  Corresponds directly
				; to FLAVOR-BINDINGS and the instances.
  FLAVOR-METHOD-TABLE		;Defined below.
  ;; End of locations depended on in many other files.
  FLAVOR-DEPENDS-ON		;List of names of flavors incorporated into this flavor.
  FLAVOR-DEPENDED-ON-BY		;List of names of flavors which incorporate this one.
				;The above are only immediate dependencies.
  FLAVOR-INCLUDES		;List of names of flavors to include at the end
				; rather than as immediate depends-on's.
  FLAVOR-DEPENDS-ON-ALL		;Names of all flavors depended on, to all levels, including
				; this flavor itself.  NIL means flavor-combination not
				; composed yet.  This is used by TYPEP of 2 arguments.
  (FLAVOR-WHICH-OPERATIONS NIL)	;List of operations handled, created when needed.
				; This is NIL if it has not been computed yet.
  ;; Redundant copy of :DEFAULT-HANDLER property, for speed in calling it.
  (FLAVOR-DEFAULT-HANDLER NIL)
  (FLAVOR-GETTABLE-INSTANCE-VARIABLES NIL)
  (FLAVOR-SETTABLE-INSTANCE-VARIABLES NIL)
  (FLAVOR-INITABLE-INSTANCE-VARIABLES NIL)
				;Alist from init keyword to name of variable
  (FLAVOR-INIT-KEYWORDS NIL)			;option
  (FLAVOR-PLIST NIL)		;Esoteric things stored here as properties
				;Known: :ORDERED-INSTANCE-VARIABLES, :DEFAULT-HANDLER
				; :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES, :ACCESSOR-PREFIX,
				; :REQUIRED-INSTANCE-VARIABLES, :REQUIRED-METHODS,
				; :REQUIRED-FLAVORS, :SELECT-METHOD-ORDER,
				; :DEFAULT-INIT-PLIST, :DOCUMENTATION, :NO-VANILLA-FLAVOR
				; :GETTABLE-INSTANCE-VARIABLES :SETTABLE-INSTANCE-VARIABLES
				; ADDITIONAL-INSTANCE-VARIABLES
				; COMPILE-FLAVOR-METHODS
				; MAPPED-COMPONENT-FLAVORS
				; INSTANCE-VARIABLE-INITIALIZATIONS
				; ALL-INITABLE-INSTANCE-VARIABLES
				; REMAINING-DEFAULT-PLIST
				; REMAINING-INIT-KEYWORDS
				;The convention on these is supposed to be that
				;ones in the keyword packages are allowed to be
				;used by users.
				;Some of these are not used by the flavor system, they are
				;just remembered on the plist in case anyone cares.  The
				;flavor system does all its handling of them during the
				;expansion of the DEFFLAVOR macro.
  )

(defsubst instancep (x)
  (and (fclosurep x) (eq (fclosure-function x) #'flavor-dispatch)))

(defvar self ()
  "Self referential pointer for flavors")

(defmacro send (object message &rest args) 
  (if (eq object 'self)
      `(send-self ,message ,@args)
      `(send-internal ,object ,message ,@args)))

(defmacro lexpr-send (object &rest args)
  (if (eq object 'self)
      `(lexpr-send-self ,@args)
      `(lexpr-funcall #'send-internal ,object ,@args)))

;; These two functions are used when sending a message to yourself, for 
;; extra efficiency.  They avoid the variable unbinding and binding 
;; required when entering a closure.
(defmacro send-self (message &rest args)
  `(funcall (or (gethash ,message (flavor-method-hash-table .own-flavor.))
		(flavor-default-handler .own-flavor.))
	    ,message . ,args))
(defmacro funcall-self (&rest args) `(send-self . ,args))

(defmacro lexpr-send-self (message &rest args)
  `(lexpr-funcall (or (gethash ,message
			       (flavor-method-hash-table .own-flavor.))
		      (flavor-default-handler .own-flavor.))
		  ,message . ,args))
(defmacro lexpr-funcall-self (&rest args) `(lexpr-send-self . ,args))

(defsetf send (e v)
  (if (or (atom (caddr e))
	  (neq (car (caddr e)) 'quote))
      (ferror () "Don't know how to setf this ~S" e))
  (cond ((eq (cadr (caddr e)) ':get)
	 `(send ,(cadr e) ':putprop ,v ,(cadddr e)))
	(t
	 `(send ,(cadr e) ',(intern (format () ":set-~A"
					    (remove-colon (cadr (caddr e)))))
			  ,v))))

(putprop 'flavorm t 'version)
