; Tasteful Flavors	-*- Mode: Lisp; Package: SI; Base:8 -*-

;; (c) copywrite 1982, Massachusetts Institute of Technology

;; This flavor system is derived from the original Lisp machine
;; flavor system.  As such its distribution may be restricted to
;; Lisp machine software license holders.

(environment-lmlisp (eval compile load) (files flavorm))

(setq |SCCS-flavors| "@(#) flavors.l	1.1	83/03/14 @(#)")

(DECLARE (SPECIAL ERRPORT)
	 (MACROS T))

; A flavor-name is a symbol which names a type of objects defined
; by the combination of several flavors.  The SI:FLAVOR
; property is a data-structure (of type FLAVOR) defining the
; nature of the flavor, as defined below.

; Flavors come in essentially three kinds.  The first kind defines a class
; of flavors, and provides the basic instance variables and methods for
; that class.  This kind typically includes only VANILLA-FLAVOR as a
; component, and uses the :REQUIRED-INSTANCE-VARIABLES and
; :REQUIRED-METHODS options.  The second kind of flavor represents a
; particular option that may be combined in (a "mix-in").  The third
; kind of flavor is the kind that can usefully be instantiated; it is
; a combination of one of the first kind and several of the second kind,
; to achieve the behavior desired for a particular application.

; The following symbols are interesting to outsiders:
; DEFFLAVOR - macro for defining a flavor
; DEFMETHOD - macro for defining a method
; DEFWRAPPER - macro for defining a flavor-wrapper
; INSTANTIATE-FLAVOR - create an object of a specified flavor
; MAKE-INSTANCE - easier to call version of INSTANTIATE-FLAVOR
; COMPILE-FLAVOR-METHODS - macro which does the right thing in the compiler
; RECOMPILE-FLAVOR - function to recompile a flavor and maybe any flavors
;		that depend on it.  Usually this happens automatically.
; FUNCALL-SELF - a macro which, assuming you are a flavor instance, will
;		call yourself without bothering about rebinding the
;		variables.  Will do something totally random if SELF
;		isn't a flavor instance.
; LEXPR-FUNCALL-SELF - LEXPR-FUNCALL version of above
; *ALL-FLAVOR-NAMES* - list of all symbols which have been used as the 
;		name of a flavor
; *FLAVOR-COMPILATIONS* - list of all methods which had to be compiled
;		this is useful for finding flavors which weren't compiled 
;		in qfasl files or which need to be recompiled to bring 
;		them up to date.
; *FLAVOR-COMPILE-TRACE* - if non-NIL, a FORMAT destination for messages about
;		recompilation of combined methods
; FLAVOR-ALLOWS-INIT-KEYWORD-P - determine whether a certain flavor allows
;		a certain keyword in its init-plist.
; FLAVOR-ALLOWED-INIT-KEYWORDS - returns all the init keywords a flavor 
;		handles.

; Roads not taken:
;  o Changing the size of all extant instances of a flavor.
;  o Nothing to stop you from instantiating a flavor of the first or
;    second kind.  In practice you will usually get an error if you try it.

; This macro is used to define a flavor.  Use DEFMETHOD to define
; methods (responses to messages sent to an instance of a flavor.)
(DEFMACRO DEFFLAVOR (NAME INSTANCE-VARIABLES COMPONENT-FLAVORS &REST OPTIONS)
  ;INSTANCE-VARIABLES can be symbols, or lists of symbol and initialization.
  ;COMPONENT-FLAVORS are searched from left to right for methods,
  ; and contribute their instance variables.
  ;OPTIONS are:
  ; (:GETTABLE-INSTANCE-VARIABLES v1 v2...) - enables automatic generation of methods
  ;   for retrieving the values of those instance variables
  ; :GETTABLE-INSTANCE-VARIABLES - (the atomic form) does it for all instance
  ;   variables local to this flavor (declared in this DEFFLAVOR).
  ; (:SETTABLE-INSTANCE-VARIABLES v1 v2...) - enables automatic generation of methods
  ;   for changing the values of instance variables
  ;   The atomic form works too.
  ; (:REQUIRED-INSTANCE-VARIABLES v1 v2...) - any flavor incorporating this
  ;   flavor and actually instantiated must have instance variables with
  ;   the specified names.  This is used for defining general types of
  ;   flavors.
  ; (:REQUIRED-METHODS m1 m2...) - any flavor incorporating this
  ;   flavor and actually instantiated must have methods for the specified
  ;   operations.  This is used for defining general types of flavors.
  ; (:REQUIRED-FLAVORS f1 f2...) - similar,  for component flavors
  ;   rather than methods.
  ; (:INITABLE-INSTANCE-VARIABLES v1 v2...) - these instance variables
  ;   may be initialized via the options to INSTANTIATE-FLAVOR.
  ;   The atomic form works too.
  ;   Settable instance variables are also INITABLE.
  ; (:INIT-KEYWORDS k1 k2...) - specifies keywords for the :INIT operation
  ;   which are legal to give to this flavor.  Just used for error checking.
  ; (:DEFAULT-INIT-PLIST k1 v1 k2 v2...) - specifies defaults to be put
  ;   into the init-plist, if the keywords k1, k2, ... are not already
  ;   specified, when instantiating.  The values v1, v2, ... get evaluated
  ;   when and if they are used.
  ; (:DEFAULT-HANDLER function) - causes function to be called if a message
  ;   is sent for which there is no method.  Defaults to a function which
  ;   gives an error.
  ; (:INCLUDED-FLAVORS f1 f2...) - specifies flavors to be included in this
  ;   flavor.  The difference between this and specifying them as components
  ;   is that included flavors go at the end, so they act as defaults.  This
  ;   makes a difference when this flavor is depended on by other flavors.
  ; :NO-VANILLA-FLAVOR - do not include VANILLA-FLAVOR.
  ;   Normally it is included automatically.  This is for esoteric hacks.
  ; (:ORDERED-INSTANCE-VARIABLES v1 v2...) - requires that in any instance,
  ;   instance variables with these names must exist and come first.  This might
  ;   be for instance variable slots specially referenced by microcode.
  ;   The atomic form works too.
  ; (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES v1 v2...) - defines defsubsts which
  ;   act like defstruct accessors for the variables; that is, using these with
  ;   an argument of an instance gets the value of that variable in that instance.
  ;   The name of the defsubst is the flavor-name, hyphen, the variable name.
  ;   If the instance variable is ordered, the accessor will know its index
  ;   in the instance and access it directly, otherwise it will call 
  ;   SYMEVAL-IN-CLOSURE at run-time.
  ;   The atomic form works too.
  ; (:ACCESSOR-PREFIX sym) - uses "sym" as the prefix on the names of the above
  ;   defsubsts instead of "flavor-".
  ; (:SELECT-METHOD-ORDER m1 m2...) - specifies that the keywords m1, m2, ... are
  ;   are important and should have their methods first in the select-method
  ;   table for increased efficiency.
  ; (:METHOD-COMBINATION (type order operation1 operation2...)...)
  ;   Specify ways of combining methods from different flavors.  :DAEMON NIL is the
  ;   the default.  order is usually :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST,
  ;   but this depends on type.
  ; (:DOCUMENTATION <args>...)
  ;   The list of args is simply put on the flavor's :DOCUMENTATION property.
  ;   The standard for this is that the arguments may include keyword symbols and
  ;   a documentation string.  To be specified more later.
  ; There may be more.
  (LET ((COPIED-OPTIONS (COPYLIST OPTIONS)))
    (DEFFLAVOR1 NAME INSTANCE-VARIABLES COMPONENT-FLAVORS COPIED-OPTIONS)
    ;; The following is done to determine all the instance variables
    ;; that need to be declared special.
    (IF (NOT (NULL (GETD 'LISZT)))
	(COMPOSE-FLAVOR-COMBINATION (GET-FLAVOR NAME)))
   `(PROGN 'COMPILE
     ;; Define flavor at load time.
     ;; Must come before the compile-time COMPOSE-AUTOMATIC-METHODS,
     ;; which puts methods in the QFASL file.
     (EVAL-WHEN (LOAD)
       (DEFFLAVOR1 ',NAME ',INSTANCE-VARIABLES ',COMPONENT-FLAVORS
		   ',COPIED-OPTIONS))
     ,@(COMPOSE-AUTOMATIC-METHODS (GET NAME 'FLAVOR))
;; Make any instance-variable accessor macros.
     ,@(DO ((VS (DO ((OPTS OPTIONS (CDR OPTS)))
		    ((NULL OPTS) NIL)
		  (AND (LISTP (CAR OPTS))
		       (EQ (CAAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
		       (RETURN (CDAR OPTS)))
		  (AND (EQ (CAR OPTS) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
		       (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				       INSTANCE-VARIABLES))))
		(CDR VS))
	    (PREFIX (OR (CADR (ASSQ ':ACCESSOR-PREFIX OPTIONS))
			(CONCAT NAME "-")))
	    (ORDS (DO ((OPTS OPTIONS (CDR OPTS)))
		      ((NULL OPTS) NIL)
		    (AND (LISTP (CAR OPTS))
			 (EQ (CAAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			 (RETURN (CDAR OPTS)))
		    (AND (EQ (CAR OPTS) ':ORDERED-INSTANCE-VARIABLES)
			 (RETURN (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
					 INSTANCE-VARIABLES)))))
	    (RES NIL (CONS `(DEFSUBST ,(INTERN (CONCAT PREFIX (CAR VS)))
				      (,NAME)
			      ,(IF (MEMQ (CAR VS) ORDS)
; SMH@EMS VVV			   `(VREF ,NAME
;					  ,(+ 9 (* 3 (FIND-POSITION-IN-LIST
;						      (CAR VS) ORDS))))
				   `(INT:FCLOSURE-STACK-STUFF
				     (VREF ,NAME ,(+ 3 (FIND-POSITION-IN-LIST
							(CAR VS) ORDS))))
; SMH@EMS ^^^
				   `(SYMEVAL-IN-FCLOSURE ,NAME ',(CAR VS))))
			   RES)))
	   ((NULL VS) RES))
     ',NAME)))

(DEFMACRO DEFUN-METHOD (FSPEC FLAVOR-NAME ARGLIST &BODY BODY)
  `(DEFUN ,FSPEC ,ARGLIST
     (DECLARE (SPECIAL SELF .OWN-FLAVOR.
		       ,@(FLAVOR-ALL-INSTANCE-VARIABLES
			  (GET-FLAVOR FLAVOR-NAME))))
     . ,BODY))

(DEFMACRO INSTANCE-VARIABLE-BOUNDP (X)
  `(BOUNDP ',X))

(DEFVAR *ALL-FLAVOR-NAMES* NIL)	;List of names of all flavors (mostly for editor)

(DEFVAR *USE-OLD-COMBINED-METHODS* T)
 ;;T means recycle old, NIL means generate new. 
 ;; This is an implicit argument to certain routines. 

(DEFVAR *FLAVOR-PENDING-DEPENDS* NIL)	;Used by DEFFLAVOR1

(DEFVAR *FLAVOR-COMPILATIONS* NIL)	;List of methods compiled

(DEFVAR *FLAVOR-COMPILE-TRACE* NIL)

(DEFSUBST INSTANCE-FLAVOR (INSTANCE)
  (SYMEVAL-IN-FCLOSURE INSTANCE '.OWN-FLAVOR.))

(DEFSUBST INSTANCE-FUNCTION (INSTANCE)
  (FCLOSURE-FUNCTION INSTANCE))

(DEFUN GET-FLAVOR (FLAVOR-OR-NAME &AUX TEMP)
  (COND ((:TYPEP FLAVOR-OR-NAME 'FLAVOR) FLAVOR-OR-NAME)
	((SYMBOLP FLAVOR-OR-NAME)
	 (SETQ TEMP (GET FLAVOR-OR-NAME 'FLAVOR))
	 (CHECK-ARG FLAVOR-OR-NAME (:TYPEP TEMP 'FLAVOR)
		    "the name of a flavor")
	 TEMP)
	(T (CHECK-ARG FLAVOR-OR-NAME (:TYPEP TEMP 'FLAVOR)
		      "the name of a flavor"))))

;;(DEFSUBST INSTANCEP (X)
;;  (AND (FCLOSUREP X) (EQ (FCLOSURE-FUNCTION X) #'FLAVOR-DISPATCH)))

(DEFUN INSTANCE-TYPEP (OB TYPE)
  (IF (NULL TYPE)
      (FLAVOR-NAME (INSTANCE-FLAVOR OB))
      (NOT (NULL (MEMQ TYPE (FLAVOR-DEPENDS-ON-ALL
			     (INSTANCE-FLAVOR OB)))))))
  

;These properties are not discarded by redoing a DEFFLAVOR.
(DEFCONST DEFFLAVOR1-PRESERVED-PROPERTIES
	  '(ADDITIONAL-INSTANCE-VARIABLES
	    COMPILE-FLAVOR-METHODS
	    MAPPED-COMPONENT-FLAVORS
	    INSTANCE-VARIABLE-INITIALIZATIONS
	    ALL-INITABLE-INSTANCE-VARIABLES
	    REMAINING-DEFAULT-PLIST
	    REMAINING-INIT-KEYWORDS))

;These are instance variables that don't belong to this flavor or its components
;but can be accessed by methods of this flavor.
(DEFSUBST FLAVOR-ADDITIONAL-INSTANCE-VARIABLES (FLAVOR)
  (GET (FLAVOR-PLIST FLAVOR) 'ADDITIONAL-INSTANCE-VARIABLES))

;The next four are distillations of info taken from this flavor and its components,
;used for instantiating this flavor.  See COMPOSE-FLAVOR-INITIALIZATIONS.
(DEFSUBST FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS (FLAVOR)
  (GET (FLAVOR-PLIST FLAVOR) 'INSTANCE-VARIABLE-INITIALIZATIONS))

(DEFSUBST FLAVOR-REMAINING-DEFAULT-PLIST (FLAVOR)
  (GET (FLAVOR-PLIST FLAVOR) 'REMAINING-DEFAULT-PLIST))

(DEFSUBST FLAVOR-REMAINING-INIT-KEYWORDS (FLAVOR)
  (GET (FLAVOR-PLIST FLAVOR) 'REMAINING-INIT-KEYWORDS))

(DEFSUBST FLAVOR-ALL-INITABLE-INSTANCE-VARIABLES (FLAVOR)
  (GET (FLAVOR-PLIST FLAVOR) 'ALL-INITABLE-INSTANCE-VARIABLES))

(DEFUN (FLAVOR :NAMED-STRUCTURE-INVOKE) (OPERATION &OPTIONAL SELF &REST ARGS)
  (SELECTQ OPERATION
	   (:WHICH-OPERATIONS '(:PRINT-SELF :DESCRIBE))
	   (:PRINT-SELF
	    (SI:PRINTING-RANDOM-OBJECT (SELF (CAR ARGS))
	       (FORMAT (CAR ARGS) "FLAVOR ~S" (FLAVOR-NAME SELF))))
	   (:DESCRIBE (DESCRIBE-FLAVOR SELF))
	   (OTHERWISE
	    (FERROR NIL "~S UNKNOWN OPERATION FOR FLAVOR" OPERATION))))

;Format of flavor-method-table:
; New format of a flavor-method-table entry is:
;   (message combination-type combination-order meth...)
; A meth is:
;   (function-spec definition plist)
; Thus the second element of a meth is actually a function-cell.
; The meth's are stored in permanent-storage-area so that they will be compact.
;    [That might not be the best area, the select-methods, and component
;     lists, and instanc-variable lists, and which-operations's, are also there.]
; A magic-list entry is:
;   (message combination-type combination-order (method-type function-spec...)...)
; In the magic-list, there can be more than one method listed under a method-type,
; the base flavor always comes first.  The :COMBINED methods are elided from
; the magic-list.
;
; Special method-types:
;   NIL - no type specified
;   :DEFAULT - like NIL but only taken if there are no type-NIL methods
;   :WRAPPER - wrappers are remembered this way
;   :COMBINED - a daemon-caller; the symbol has a COMBINED-METHOD-DERIVATION property
;		whose value is the complete method table entry from the magic-list.
;		The CDDDR is canonicalized; each contained list of method symbols is
;		of course ordered by the order in which flavors are combined (base
;		flavor first).  Canonical order is alphabetical by method-type.
; Non-special method-types:
;   :BEFORE, :AFTER - these are used by the default combination-type, :DAEMON
;
; Special hair for wrappers: changing a wrapper can invalidate the combined method
; without changing anything in the flavor-method-table entry.  Rather than having
; it automatically recompile, which turns out to be a pain when the wrapper was
; just reloaded or changed trivially, it will fail to recompile and you must use
; RECOMPILE-FLAVOR with a 3rd argument of NIL.
;
; A combination-type of NIL means it has not been explicitly specified.

; Method-combination functions.  Found on the SI:METHOD-COMBINATION property
; of the combination-type.  These are passed the flavor structure, and the
; magic-list entry, and must return the function spec to use as the handler.
; It should also define or compile thew definition for that function spec if nec.
; This function interprets combination-type-arg,
; which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.

;This is an a-list from method type to function to write the code to go
;in the combined method.  Users can add to this.
(DEFCONST *SPECIALLY-COMBINED-METHOD-TYPES*
	  '((:WRAPPER PUT-WRAPPER-INTO-COMBINED-METHOD)))

;Definitions of a meth (the datum which stands for a method)

(DEFSTRUCT (METH :LIST :CONC-NAME (:CONSTRUCTOR NIL))
		;No constructor because defstruct doesn't let me specify the area
  FUNCTION-SPEC
  DEFINITION
  (PLIST NIL))

; If there is no definition, it contains DTP-NULL and a pointer to the meth

; Extract the method-type of a meth
(DEFMACRO METH-METHOD-TYPE (METH)
  `(AND (CDDDR (METH-FUNCTION-SPEC ,METH))
	(THIRD (METH-FUNCTION-SPEC ,METH))))

; Return a meth of specified type from a list of meth's.
(DEFUN METH-LOOKUP (METHOD-TYPE METH-LIST)
  (LOOP FOR METH IN METH-LIST
	WHEN (EQ (METH-METHOD-TYPE METH) METHOD-TYPE)
	  RETURN METH))

(DEFUN NULLIFY-METHOD-DEFINITION (METH)
  (SETF (METH-DEFINITION METH) NIL))

(DEFUN METH-DEFINEDP (METH)
  (NOT (NULL (METH-DEFINITION METH))))

;Function to define or redefine a flavor (used by DEFFLAVOR macro).
;Note that to ease initialization problems, the flavors depended upon need
;not be defined yet.  You will get an error the first time you try to create
;an instance of this flavor if a flavor it depends on is still undefined.
;When redefining a flavor, we reuse the same FLAVOR defstruct so that
;old instances continue to get the latest methods, unless you change
;something incompatibly, in which case you will get a warning.
(DEFUN DEFFLAVOR1 (FLAVOR-NAME INSTANCE-VARIABLES COMPONENT-FLAVORS OPTIONS
		   &AUX FFL ALREADY-EXISTS INSTV IDENTICAL-COMPONENTS
			GETTABLE SETTABLE INITABLE OLD-DEFAULT-HANDLER
			OLD-DEFAULT-INIT-PLIST OLD-LOCAL-IVS OLD-INITABLE-IVS
			OLD-INIT-KWDS
			INIT-KEYWORDS INCLUDES METH-COMB
			(PL (LIST 'FLAVOR-PLIST)))
  (COND ((NOT (MEMQ FLAVOR-NAME *ALL-FLAVOR-NAMES*))
	 (PUSH FLAVOR-NAME *ALL-FLAVOR-NAMES*)))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (SETQ INSTV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
		      INSTANCE-VARIABLES))
  (DOLIST (IV INSTV)
    (IF (OR (NULL IV) (NOT (SYMBOLP IV)))
	(FERROR () "~S, which is not a symbol, was specified as an instance variable" IV)))
  (DOLIST (CF COMPONENT-FLAVORS)
    (IF (OR (NULL CF) (NOT (SYMBOLP CF)))
	(FERROR () "~S, which is not a symbol, was specified as a component flavor" CF)))
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (COND ((SETQ ALREADY-EXISTS (GET FLAVOR-NAME 'FLAVOR))
	 (DOLIST (PROP DEFFLAVOR1-PRESERVED-PROPERTIES)
	   (PUTPROP PL (GET (FLAVOR-PLIST ALREADY-EXISTS) PROP)
		    PROP))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (DO ((L OPTIONS (CDR L))
       (OPTION) (ARGS))
      ((NULL L))
    (IF (ATOM (CAR L))
	(SETQ OPTION (CAR L) ARGS NIL)
	(SETQ OPTION (CAAR L) ARGS (CDAR L)))
    (SELECTQ OPTION
	(:GETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ GETTABLE (OR ARGS INSTV)))
	(:SETTABLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ SETTABLE (OR ARGS INSTV)))
	((:INITABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (SETQ INITABLE (OR ARGS INSTV)))
	(:SPECIAL-INSTANCE-VARIABLES)  ; Ignored since all IVs are special
	(:INIT-KEYWORDS
	  (SETQ INIT-KEYWORDS ARGS))
	(:INCLUDED-FLAVORS
	  (SETQ INCLUDES ARGS))
	(:NO-VANILLA-FLAVOR
	  (PUTPROP PL T OPTION))
	(:ORDERED-INSTANCE-VARIABLES
	  ;Don't validate.  User may reasonably want to specify non-local instance
	  ;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
	  ;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':ORDERED-INSTANCE-VARIABLES))
	(:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
	  (VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
	  (PUTPROP PL (OR ARGS INSTV) ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES))
	(:METHOD-COMBINATION
	  (SETQ METH-COMB ARGS))
	(:DEFAULT-HANDLER
	  (PUTPROP PL (CAR ARGS) OPTION))
	((:REQUIRED-INSTANCE-VARIABLES :REQUIRED-METHODS :REQUIRED-FLAVORS :DOCUMENTATION
	  :DEFAULT-INIT-PLIST :SELECT-METHOD-ORDER :ACCESSOR-PREFIX)
	  (PUTPROP PL ARGS OPTION))
	(OTHERWISE (FERROR () "~S unknown option to DEFFLAVOR" OPTION))))
  ;; All settable instance variables should also be gettable and INITABLE.
  (DOLIST (V SETTABLE)
    (OR (MEMQ V GETTABLE)
	(PUSH V GETTABLE))
    (OR (MEMQ V INITABLE)
	(PUSH V INITABLE)))
  ;; See whether there are any changes in component flavor structure from last time
  (SETQ IDENTICAL-COMPONENTS
	(AND ALREADY-EXISTS
	     (EQUAL COMPONENT-FLAVORS (FLAVOR-DEPENDS-ON ALREADY-EXISTS))
	     (EQUAL INCLUDES (FLAVOR-INCLUDES ALREADY-EXISTS))
	     (EQUAL (GET PL ':REQUIRED-FLAVORS)
		    (GET (FLAVOR-PLIST ALREADY-EXISTS) ':REQUIRED-FLAVORS))))	
  (AND ALREADY-EXISTS
       (SETQ OLD-DEFAULT-HANDLER (GET (FLAVOR-PLIST ALREADY-EXISTS)
				      ':DEFAULT-HANDLER)
	     OLD-DEFAULT-INIT-PLIST (GET (FLAVOR-PLIST ALREADY-EXISTS)
					 ':DEFAULT-INIT-PLIST)
	     OLD-LOCAL-IVS (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INITABLE-IVS (FLAVOR-INITABLE-INSTANCE-VARIABLES ALREADY-EXISTS)
	     OLD-INIT-KWDS (FLAVOR-INIT-KEYWORDS ALREADY-EXISTS))) 
  ;; If the flavor is being redefined, and the number or order of instance 
  ;; variables is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give 
  ;; a warning and disconnect from the old FLAVOR defstruct so that old 
  ;; instances will retain the old information.  The instance variables can 
  ;; get changed either locally or by rearrangement of the component flavors.
  (AND ALREADY-EXISTS
       (IF (AND (EQUAL (GET PL ':ORDERED-INSTANCE-VARIABLES)
		       (GET (FLAVOR-PLIST ALREADY-EXISTS)
			    ':ORDERED-INSTANCE-VARIABLES))
		(OR (EQUAL (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS)
			   INSTANCE-VARIABLES)
		    (EQUAL (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X)))
				   (FLAVOR-LOCAL-INSTANCE-VARIABLES ALREADY-EXISTS))
			   INSTV))
		(OR IDENTICAL-COMPONENTS
		    (EQUAL (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       COMPONENT-FLAVORS INCLUDES)
			   (FLAVOR-RELEVANT-COMPONENTS ALREADY-EXISTS
						       (FLAVOR-DEPENDS-ON ALREADY-EXISTS)
						       (FLAVOR-INCLUDES ALREADY-EXISTS)))))
	   NIL
	   (SETQ ALREADY-EXISTS (PERFORM-FLAVOR-REDEFINITION FLAVOR-NAME))))
  ;; Make the information structure unless the flavor already exists.
  (LET ((FL (OR ALREADY-EXISTS
		(GET FLAVOR-NAME 'UNDEFINED-FLAVOR)
		(MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME))))
    (SETF (FLAVOR-LOCAL-INSTANCE-VARIABLES FL) INSTANCE-VARIABLES)
    (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
    (SETF (FLAVOR-PLIST FL) PL)
    (IF GETTABLE
	(SETF (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL) GETTABLE))
    (IF SETTABLE
	(SETF (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL) SETTABLE))
    (SETF (FLAVOR-INITABLE-INSTANCE-VARIABLES FL)
	  (LOOP FOR V IN INITABLE COLLECT (CONS (CORRESPONDING-KEYWORD V) V)))
    (SETF (FLAVOR-INIT-KEYWORDS FL) INIT-KEYWORDS)
    (SETF (FLAVOR-INCLUDES FL) INCLUDES)
    ;; First remove old method-combination declarations, then add new ones
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
      (COND ((LOOP FOR DECL IN METH-COMB NEVER (MEMQ (CAR MTE) (CDDR DECL)))
	     (SETF (SECOND MTE) NIL)
	     (SETF (THIRD MTE) NIL))))
    (DOLIST (DECL METH-COMB)
      (LET ((TYPE (CAR DECL)) (ORDER (CADR DECL)) ELEM)
	;; Don't error-check TYPE now, its definition might not be loaded yet
	(DOLIST (MSG (CDDR DECL))
	  (OR (SETQ ELEM (ASSQ MSG (FLAVOR-METHOD-TABLE FL)))
	      (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
	  (SETF (SECOND ELEM) TYPE)
	  (SETF (THIRD ELEM) ORDER))))
    ;; Make this a depended-on-by of its depends-on, or remember to do it 
    ;; later in the case of depends-on's not yet defined.
    (DOLIST (COMPONENT-FLAVOR COMPONENT-FLAVORS)
      (COND ((SETQ FFL (GET COMPONENT-FLAVOR 'FLAVOR))
	     (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		 (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
	    (T (PUSH (CONS COMPONENT-FLAVOR FLAVOR-NAME)
		     *FLAVOR-PENDING-DEPENDS*))))
    ;; Likewise for its includes
    (DOLIST (INCLUDED-FLAVOR (FLAVOR-INCLUDES FL))
      (COND ((SETQ FFL (GET INCLUDED-FLAVOR 'FLAVOR))
	     (OR (MEMQ FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))
		 (PUSH FLAVOR-NAME (FLAVOR-DEPENDED-ON-BY FFL))))
	    (T (PUSH (CONS INCLUDED-FLAVOR FLAVOR-NAME)
		     *FLAVOR-PENDING-DEPENDS*))))
    ;; If someone depends on this flavor, which wasn't defined until now, 
    ;; link them up.  If that flavor was flavor-composed, recompose it now.
    (DOLIST (X *FLAVOR-PENDING-DEPENDS*)
      (COND ((EQ (CAR X) FLAVOR-NAME)
	     (OR (MEMQ (CDR X) (FLAVOR-DEPENDED-ON-BY FL))
		 (PUSH (CDR X) (FLAVOR-DEPENDED-ON-BY FL)))
	     (SETQ *FLAVOR-PENDING-DEPENDS*
		   (DELQ X *FLAVOR-PENDING-DEPENDS*)))))
    (PUTPROP FLAVOR-NAME FL 'FLAVOR)
    ;; Now, if the flavor was redefined in a way that changes the methods but
    ;; doesn't invalidate old instances, we have to propagate some changes.
    (IF (AND ALREADY-EXISTS
	     (NOT IDENTICAL-COMPONENTS))
	(PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION FLAVOR-NAME))
    FLAVOR-NAME))

;Check for typos in user-specified lists of instance variables.
;This assumes that only locally-specified (not inherited) instance variables
;may be mentioned in DEFFLAVOR declaration clauses.
(DEFUN VALIDATE-INSTANCE-VARIABLES-SPEC (VARS-SPECD VARS-ALLOWED FLAVOR-NAME
						    OPTION &AUX BAD)
  (DOLIST (VAR VARS-SPECD)
    (OR (MEMQ VAR VARS-ALLOWED) (PUSH VAR BAD)))
  (COND (BAD (FORMAT ERRPORT "~&ERROR: Flavor ~S has misspelled :~A ~%~S"
		     FLAVOR-NAME OPTION (NREVERSE BAD)))))

;List of those components which affect the names, number, and ordering of the
;instance variables.  Don't worry about undefined components, by definition
;they must be different from the already-existing flavor, so the right
;thing will happen.  (I wonder what that comment means?  Undefined components
;will not even appear in the list.)
(DEFUN FLAVOR-RELEVANT-COMPONENTS (FL COMPONENT-FLAVORS INCLUDED-FLAVORS)
  (SETF (FLAVOR-DEPENDS-ON FL) COMPONENT-FLAVORS)
  (SETF (FLAVOR-INCLUDES FL) INCLUDED-FLAVORS)
  (DEL-IF-NOT #'(LAMBDA (FLAVOR)	;Splice out the uninteresting ones
		  (FLAVOR-LOCAL-INSTANCE-VARIABLES FLAVOR))
	      (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) NIL)))

;; Now that default structs are vectors, and plain copy works for vectors,
;; this has been removed and replaced by copy. - SMH@EMS
;(DEFUN COPY-HUNK-CONTENTS (H1 H2)
;  (LOOP FOR I FROM 0 TO (1- (HUNKSIZE H2))
;	DO (SETF (CXR I H2) (CXR I H1))))

;Propagate things from an old flavor to a new one which we construct,
;for compiling a file.
(DEFUN FLAVOR-REDEFINITION-FOR-COMPILATION (OLD-FLAVOR NEW-COMPONENTS-P)
  NEW-COMPONENTS-P
  (LET ((NEW-FLAVOR (MAKE-FLAVOR FLAVOR-NAME (FLAVOR-NAME OLD-FLAVOR))))
    ;(COPY-HUNK-CONTENTS OLD-FLAVOR NEW-FLAVOR) ; SMH@EMS
    (SETQ NEW-FLAVOR (COPY OLD-FLAVOR))	; Now works only if vector.
    ;; Do copy any combined methods.  If we have dependents also in this file
    ;; and they have COMPILE-FLAVOR-METHODS in this file,
    ;; they will want to see our combined methods in case they can use them.
    (COPY-METHOD-TABLE OLD-FLAVOR NEW-FLAVOR NIL)
    (SETF (FLAVOR-DEPENDS-ON-ALL NEW-FLAVOR) NIL)	;Will need to be flavor-composed again
    ;; Cause an error if these are looked at before they are valid.
    (SETF (FLAVOR-ALL-INSTANCE-VARIABLES NEW-FLAVOR) 'NOT-COMPUTED)
    (SETF (FLAVOR-DEPENDED-ON-BY NEW-FLAVOR) 'COMPILATION)
    (SETF (FLAVOR-METHOD-HASH-TABLE NEW-FLAVOR) NIL)	;Will need to be method-composed again
    (SETF (FLAVOR-WHICH-OPERATIONS NEW-FLAVOR) NIL)
    NEW-FLAVOR))

(DEFUN COPY-METHOD-TABLE (OLD-FLAVOR NEW-FLAVOR DISCARD-COMBINED-METHODS)
  (LET ((L (COPYLIST (FLAVOR-METHOD-TABLE OLD-FLAVOR))))
    (DO ((TAIL L (CDR TAIL)))
	((NULL TAIL))
      ;; Copy the method-table element, including the list of METH's.
      (SETF (CAR TAIL) (COPYLIST (CAR TAIL)))
      (IF DISCARD-COMBINED-METHODS
	  ;; Flush from the copy all combined methods.
	  (DO ((TAIL2 (CDDDR (CAR TAIL)) (CDR TAIL2)))
	      ((NULL TAIL2))
	    (AND (EQ (METH-METHOD-TYPE (CAR TAIL2)) ':COMBINED)
		 (SETF (CDDDAR TAIL)
		       (DELQ (CAR TAIL2) (CDDDAR TAIL))))))
      ;; Now copy each METH that we didn't delete.
      ;; Copying a METH is not trivial because it can contain a DTP-NULL.
      (DO ((TAIL2 (CDDDR (CAR TAIL)) (CDR TAIL2)))
	  ((NULL TAIL2))
	(LET ((NEW-METH (LIST (FIRST (CAR TAIL2))
			      NIL
			      (COPYLIST (THIRD (CAR TAIL2))))))
	  (IF (METH-DEFINEDP (CAR TAIL2))
	      (SETF (METH-DEFINITION NEW-METH) (METH-DEFINITION (CAR TAIL2)))
	    (NULLIFY-METHOD-DEFINITION NEW-METH))
	  (SETF (CAR TAIL2) NEW-METH))))
    (SETF (FLAVOR-METHOD-TABLE NEW-FLAVOR) L)))

;Record a flavor definition, during compiling a file.
;Instead of setting the name's FLAVOR property, we put an entry on the
;FLAVORS element in the FILE-LOCAL-DECLARATIONS, where COMPILATION-FLAVOR looks.
(DEFVAR FILE-LOCAL-DECLARATIONS ())

(DEFUN COMPILATION-DEFINE-FLAVOR (FLAVOR-NAME FL)
  (LET ((FLL (ASSQ 'FLAVORS FILE-LOCAL-DECLARATIONS)))
    (COND ((NULL FLL)
	   (PUSH (NCONS 'FLAVORS) FILE-LOCAL-DECLARATIONS)
	   (SETQ FLL (CAR FILE-LOCAL-DECLARATIONS))))
    (PUTPROP FLL FL FLAVOR-NAME)))

;Call here when a flavor has been changed in a way that is not compatible
;with old instances of this flavor or its dependents.
;Arranges for those old instances to keep the old flavor structures and 
;methods.  Return new copy of the FLAVOR defstruct, and propagate to those 
;that depend on it.  Note that we tell copy-method-table to discard our 
;combined methods.  This is because they point to METHs in our method table,
;so we must make new combined methods that point at our new method table.
(DEFUN PERFORM-FLAVOR-REDEFINITION (FLAVOR-NAME &AUX FL NFL)
  (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
  (COND ((FLAVOR-METHOD-HASH-TABLE FL)
	 (SETQ NFL (MAKE-FLAVOR))
	 ; (COPY-HUNK-CONTENTS FL NFL) ; SMH@EMS
	 (SETQ NFL (COPY FL))		; Now works only if FL is a vector!
	 (COPY-METHOD-TABLE FL NFL T)			   ;Copy, but discard combined methods
	 (SETQ FL NFL)
	 (SETF (FLAVOR-PLIST FL) (COPYLIST (FLAVOR-PLIST FL)))
	 (PUTPROP FLAVOR-NAME FL 'FLAVOR)
	 (FORMAT ERRPORT "~&Flavor ~S changed incompatibly, old instances will not get the new version.~%"
		 FLAVOR-NAME))
	;; Even if this flavor wasn't instantiated,
	;; probably some of its dependents were,
	;; and their hash tables and combined methods point to our method table.
	(T (COPY-METHOD-TABLE FL FL T)))
  (SETF (FLAVOR-DEPENDS-ON-ALL FL) NIL)	;Will need to be flavor-composed again
  (SETF (FLAVOR-METHOD-HASH-TABLE FL) NIL)	;Will need to be method-composed again
  (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (PERFORM-FLAVOR-REDEFINITION FN))
  FL)

;This one is when the old instances don't have to be discarded, but recomposition
;does have to occur because something was changed in the order of flavor combination
(DEFUN PERFORM-FLAVOR-METHOD-ONLY-REDEFINITION (FLAVOR-NAME)
  (LET ((FDEFINE-FILE-PATHNAME NIL))	;Don't give warnings for combined methods
    ;; Reverse the list so that this flavor comes first, followed by directest descendents.
    (DOLIST (FN (REVERSE (FLAVOR-DEPENDED-ON-BY-ALL (GET FLAVOR-NAME 'FLAVOR)
						    (LIST FLAVOR-NAME))))
      (LET ((FL (GET FN 'FLAVOR)))
	(IF (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
	(IF (FLAVOR-METHOD-HASH-TABLE FL) (COMPOSE-METHOD-COMBINATION FL))))))

(DEFUN DESCRIBE-FLAVOR (FLAVOR-NAME &AUX FL)
  (SETQ FL (IF (SYMBOLP FLAVOR-NAME) (GET-FLAVOR FLAVOR-NAME)
	       FLAVOR-NAME))
  (CHECK-ARG FLAVOR-NAME (:TYPEP FL 'FLAVOR)
	     "a flavor or the name of one")
  (FORMAT T "~&Flavor ~S directly depends on flavors: ~:[none~;~1G~{~S~^, ~}~]~%"
	    FLAVOR-NAME (FLAVOR-DEPENDS-ON FL))
  (AND (FLAVOR-INCLUDES FL)
       (FORMAT T " and directly includes ~{~S~^, ~}~%" (FLAVOR-INCLUDES FL)))
  (AND (FLAVOR-DEPENDED-ON-BY FL)
       (FORMAT T " and is directly depended on by ~{~S~^, ~}~%" (FLAVOR-DEPENDED-ON-BY FL)))
  (AND (FLAVOR-DEPENDS-ON-ALL FL)	;If this has been computed, show it
       (FORMAT T " and directly or indirectly depends on ~{~S~^, ~}~%"
	         (FLAVOR-DEPENDS-ON-ALL FL)))
  (AND (FLAVOR-METHOD-HASH-TABLE FL)	;If has been composed
       (FORMAT T "Flavor ~S has instance variables ~:S~%"
	         FLAVOR-NAME (FLAVOR-ALL-INSTANCE-VARIABLES FL)))
  (COND ((NOT (NULL (FLAVOR-METHOD-TABLE FL)))
	 (FORMAT T "Not counting inherited methods, the methods for ~S are:~%"
		 FLAVOR-NAME)
	 (DOLIST (M (FLAVOR-METHOD-TABLE FL))
	   (FORMAT T "   ")
	   (DO ((TPL (SUBSET 'METH-DEFINEDP (CDDDR M)) (CDR TPL)))
	     ((NULL TPL))
	     (IF (METH-METHOD-TYPE (CAR TPL))
		 (FORMAT T "~A " (METH-METHOD-TYPE (CAR TPL))))
	     (FORMAT T "~A" (CAR M))
	     (IF (CDR TPL) (PRINC ", ")))
	   ;; Print the method combination type if there is any.
	   (AND (CADR M)
		(FORMAT T "    :~A~@[ :~A~]" (CADR M) (CADDR M)))
	   (TERPRI))))
  (AND (FLAVOR-ALL-INSTANCE-VARIABLES FL)
       (FORMAT T "Instance variables: ~{~S~^, ~}~%" (FLAVOR-ALL-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to get instance variables: ~{~S~^, ~}~%"
	         (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Automatically-generated methods to set instance variables: ~{~S~^, ~}~%"
	         (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)))
  (AND (FLAVOR-INITABLE-INSTANCE-VARIABLES FL)
       (FORMAT T "Instance variables that may be set by initialization: ~{~S~^, ~}~%"
	         (MAPCAR #'CDR (FLAVOR-INITABLE-INSTANCE-VARIABLES FL))))
  (AND (FLAVOR-INIT-KEYWORDS FL)
       (FORMAT T "Keywords in the :INIT message handled by this flavor: ~{~S~^, ~}~%"
	         (FLAVOR-INIT-KEYWORDS FL)))
  (COND ((FLAVOR-PLIST FL)
	 (FORMAT T "Properties:~%")
	 (DO L (CDR (FLAVOR-PLIST FL)) (CDDR L) (NULL L)
	   (FORMAT T "~5X~S:	~S~%" (CAR L) (CADR L)))))
  (COND ((NULL (FLAVOR-METHOD-HASH-TABLE FL))
	 (FORMAT T "Flavor ~S does not yet have a method hash table~%" FLAVOR-NAME))
	(T (FORMAT T "Flavor ~S has method hash table:~%" FLAVOR-NAME)
	   (PRINT (FLAVOR-METHOD-HASH-TABLE FL)))))

;; This is the standard way of defining a method of a class,
;; so that the code will be compiled.  
;; If in place of the lambda-list you have a symbol, and the body
;; is null, that symbol is a function which stands in for the method.
(DEFMACRO DEFMETHOD (SPEC LAMBDA-LIST . BODY)
  (LET ((CLASS-NAME (CAR SPEC))
	(FUNCTION-SPEC (CONS ':METHOD SPEC))
	FUNCTION-NAME)
    (SETQ FUNCTION-NAME (METHOD-FUNCTION-NAME FUNCTION-SPEC))
    `(PROGN 'COMPILE
       (EVAL-WHEN (COMPILE LOAD EVAL)
	  (FLAVOR-NOTICE-METHOD ',FUNCTION-SPEC))
       ;; At load-time, define the method function
       ,(COND ((AND (SYMBOLP LAMBDA-LIST) (NOT (NULL LAMBDA-LIST))
		    (NULL BODY))
	       #-Franz `(FDEFINE ',FUNCTION-SPEC ',LAMBDA-LIST)
	       #+Franz `(DEFUN ,FUNCTION-NAME (OPERATION . ,LAMBDA-LIST)
			       (,lambda-list (operation . ,lambda-list))))
	      ((GET CLASS-NAME 'FLAVOR)
	       `(DEFUN ,FUNCTION-NAME (OPERATION . ,LAMBDA-LIST)
		  (DECLARE (SPECIAL SELF .OWN-FLAVOR.
				    ,@(FLAVOR-ALL-INSTANCE-VARIABLES
				       (GET-FLAVOR CLASS-NAME))))
		  . ,BODY))
	      (T ;; The non-flavor class system
		(FERROR () "Old Class system is not SUPPORTED")))
       ',FUNCTION-SPEC)))

(DEFUN REMOVE-COLON (SYMBOL)
  (IF (= (GETCHARN SYMBOL 1) #/:)
      (CONCAT (SUBSTRING SYMBOL 2))
      SYMBOL))

; This lets you specify code to be wrapped around the invocation of the
; various methods for an operation.  For example,
; (DEFWRAPPER (FOO-FLAVOR :OPERATION) ((ARG1 ARG2) . BODY)
;   `(WITH-FOO-LOCKED (SELF)
;      (PRE-FROBULATE SELF ARG1 ARG2)
;      ,@BODY
;      (POST-FROBULATE SELF ARG2 ARG1)))
;Note that the wrapper needs to be defined at both compile and run times
;so that compiling combined methods as part of the qfasl file works.

#+Franz
(defmacro destructuring-bind (template values . body)
 `(let ((,template ,values)) . ,body))

(DEFMACRO DEFWRAPPER
  ((FLAVOR-NAME OPERATION) (DEFMACRO-LAMBDA . GUTS) &BODY BODY)
  (LET ((FUNCTION-SPEC `(:METHOD ,FLAVOR-NAME :WRAPPER ,OPERATION))
	function-name)
       (setq function-name (method-function-name function-spec))
       `(PROGN ;; 'COMPILE
	       ;; Unfortunately, in Franz wrappers should not be compiled
	       ;; since the actual definition is needed by macrocall.
	       ;; Macrocall is clearly a crock!
	    ;; The following optimization could go away if defmacro were
	    ;; very smart.
	    ,(IF (AND (SYMBOLP DEFMACRO-LAMBDA)
		      (EQUAL DEFMACRO-LAMBDA 'IGNORE))
		 `(DEFMACRO ,function-name (IGNORE . ,GUTS) . ,BODY)
		 `(DEFMACRO ,function-name (ARGLISTNAME . ,GUTS)
		    `(DESTRUCTURING-BIND ,',DEFMACRO-LAMBDA (CDR ,ARGLISTNAME)
					 ,,@BODY)))
	 (flavor-notice-method ',function-spec))))

;This just exists to be called at compile-time from the DEFMETHOD macro,
;so that any combined methods generated by COMPILE-FLAVOR-METHODS will
;know that this method will be around at run time and should be called.
(DEFUN FLAVOR-NOTICE-METHOD (FUNCTION-SPEC)
  (LET ((METH (FLAVOR-METHOD-ENTRY FUNCTION-SPEC NIL T)))
    (COND ((NOT (EQ (METH-DEFINITION METH)
		    (METHOD-FUNCTION-NAME FUNCTION-SPEC)))
	   (SETF (METH-DEFINITION METH) (METHOD-FUNCTION-NAME FUNCTION-SPEC))
	   (RECOMPILE-FLAVOR (SECOND FUNCTION-SPEC)
			     (CAR (LAST FUNCTION-SPEC)))))))

(DEFUN METHOD-FUNCTION-NAME (FUNCTION-SPEC)
  (LET ((FLAVOR (SECOND FUNCTION-SPEC))
	(METHOD-TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL (CDDDR FUNCTION-SPEC))
	(SETQ MESSAGE (THIRD FUNCTION-SPEC) METHOD-TYPE NIL))
    (IF (NULL METHOD-TYPE)
	(INTERN (FORMAT () "~A-~A-method" FLAVOR (REMOVE-COLON MESSAGE)))
	(INTERN
	 (FORMAT () "~A-~A-~A-method"
		 FLAVOR (REMOVE-COLON METHOD-TYPE) (REMOVE-COLON MESSAGE))))))

;Find or create a method-table entry for the specified method.
;DONT-CREATE is NIL if method is to be created if necessary.
;	The flavor is "created" too, as an UNDEFINED-FLAVOR property
;	of the flavor name, just to record any properties of methods.
;COPY-FLAVOR-IF-UNDEFINED-METH says we are going to alter the METH
;for compilation if it is not defined, so the flavor should be copied in that case.
(DEFUN FLAVOR-METHOD-ENTRY (FUNCTION-SPEC DONT-CREATE
				&OPTIONAL COPY-FLAVOR-IF-UNDEFINED-METH)
			   		  ;; Huh? Unused! -SMH
  (LET ((FLAVOR-NAME (SECOND FUNCTION-SPEC))
	(TYPE (THIRD FUNCTION-SPEC))
	(MESSAGE (FOURTH FUNCTION-SPEC)))
    (IF (NULL MESSAGE) (SETQ MESSAGE TYPE TYPE NIL))	;If no type
    (IF (OR (NULL MESSAGE) (NEQ (FIRST FUNCTION-SPEC) ':METHOD)
	    (> (LENGTH FUNCTION-SPEC) 4)
	    (NOT (SYMBOLP FLAVOR-NAME)) (NOT (SYMBOLP TYPE))
	    (NOT (SYMBOLP MESSAGE)))
	(FERROR () "~S is not a valid function-spec" FUNCTION-SPEC))
    (LET* ((FL (OR (GET-FLAVOR FLAVOR-NAME)
		   (GET FLAVOR-NAME 'UNDEFINED-FLAVOR)
		   (AND (NOT DONT-CREATE)
			(PUTPROP FLAVOR-NAME
				 (MAKE-FLAVOR FLAVOR-NAME FLAVOR-NAME)
				 'UNDEFINED-FLAVOR))))
	   (MTE (AND FL (ASSQ MESSAGE (FLAVOR-METHOD-TABLE FL))))
	   (METH (METH-LOOKUP TYPE (CDDDR MTE))))
      (AND (NULL MTE) (NOT DONT-CREATE)
	   ;; Message not previously known about, put into table
	   FL
	   (PUSH (SETQ MTE (LIST* MESSAGE NIL NIL NIL)) (FLAVOR-METHOD-TABLE FL)))
      ;; Message known, search for the type entry
      (COND (METH)	;Known by flavor
	    (DONT-CREATE NIL)		;Not to be created
	    ((NULL FL) NIL)	;Create, but no flavor defined
	    (T ;; Type not known, create a new meth with an unbound definition cell
	     (LET ((METH (LIST FUNCTION-SPEC NIL NIL)))
	       (NULLIFY-METHOD-DEFINITION METH)
	       (PUSH METH (CDDDR MTE))
	       METH))))))

;;; See if a certain method exists in a flavor
(DEFUN FLAVOR-METHOD-EXISTS (FL TYPE OPERATION &AUX MTE)
  (AND (SETQ MTE (ASSQ OPERATION (FLAVOR-METHOD-TABLE FL)))
       (LET ((METH (METH-LOOKUP TYPE (CDDDR MTE))))
	 (AND METH (METH-DEFINEDP METH)))))

;;; Forcibly remove a method definition from a flavor's method table
;;; Syntax is identical to the beginning of a defmethod for the same method.
(DEFMACRO UNDEFMETHOD (SPEC)
  `(FUNDEFINE '(:METHOD . ,SPEC)))

;Make an object of a particular flavor, taking the init-plist options
;as a rest argument and sending the :INIT message if the flavor
;handles it.
(DEFUN MAKE-INSTANCE (FLAVOR-NAME &REST INIT-OPTIONS)
  (INSTANTIATE-FLAVOR FLAVOR-NAME (CONS 'INSTANCE-OPTIONS INIT-OPTIONS)
		      'MAYBE))

(DEFUN FLAVOR-DISPATCH (MESSAGE &REST ARGUMENTS &AUX FUN)
  (DECLARE (SPECIAL .OWN-FLAVOR.))
  (SETQ FUN (OR (GETHASH MESSAGE (FLAVOR-METHOD-HASH-TABLE .OWN-FLAVOR.))
		(FLAVOR-DEFAULT-HANDLER .OWN-FLAVOR.)))
  (IF (NOT (NULL FUN))
      (LEXPR-FUNCALL FUN MESSAGE ARGUMENTS)
      (FLAVOR-UNCLAIMED-MESSAGE MESSAGE ARGUMENTS)))      

;; The first six slots are for SELF and .OWN-FLAVOR. The values are in the
;; third slot.
; SMH@EMS VVV
; Perforce, %instance-ref no longer used.
;	(DEFSUBST %INSTANCE-REF (INSTANCE INDEX)
;	  (VREF INSTANCE (+ 9. (* 3 INDEX))))
;	(DEFSUBST INSTANCE-FLAVOR (INSTANCE) (VREF INSTANCE 6))
; The previous instance-flavor ought always to be good, if inefficient.
;	(DEFSUBST INSTANCE-FLAVOR (INSTANCE) (VREF INSTANCE 3))
; SMH@EMS ^^^

;Make an object of a particular flavor.
;If the flavor hasn't been composed yet, must do so now.
; Delaying it until the first time it is needed aids initialization,
; e.g. up until now we haven't depended on the depended-on flavors being defined yet.
;Note that INIT-PLIST can be modified, if the :DEFAULT-INIT-PLIST option was
; used or the init methods modify it.
(DEFUN INSTANTIATE-FLAVOR (FLAVOR-NAME INIT-PLIST
		           &OPTIONAL SEND-INIT-MESSAGE-P
				     RETURN-UNHANDLED-KEYWORDS-P ;as second value
			   &AUX FL FFL UNHANDLED-KEYWORDS INSTANCE VARS N TEM)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-TABLE FL) (COMPOSE-METHOD-COMBINATION FL))
  (SETQ VARS (FLAVOR-ALL-INSTANCE-VARIABLES FL))
;; Make the instance object, then fill in its various fields
  (SETQ INSTANCE
	(PROGV `(SELF .OWN-FLAVOR. ,@VARS)
	       `(NIL ,FL)
	       (FCLOSURE `(SELF .OWN-FLAVOR. ,@VARS)
			 #'FLAVOR-DISPATCH)))
  (LOOP FOR I FROM 0 TO (LENGTH VARS)
	WITH IVS = (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL)
	WHEN (= I (CAAR IVS))
; SMH@EMS VVV
;	DO (PROGN (SETF (%INSTANCE-REF INSTANCE I)
;			(FAST-EVAL (CADAR IVS)))
;		  (POP IVS)))
	DO (PROGN (INT:FCLOSURE-STACK-STUFF (VREF INSTANCE (+ 3 I))
					    (FAST-EVAL (CADAR IVS)))
		  (POP IVS)))
; SMH@EMS ^^^
  (SET-IN-FCLOSURE INSTANCE 'SELF INSTANCE)
  (LET ((VAR-KEYWORDS (FLAVOR-ALL-INITABLE-INSTANCE-VARIABLES FL))
	(REMAINING-KEYWORDS (FLAVOR-REMAINING-INIT-KEYWORDS FL)))
    (COND (VAR-KEYWORDS
	   ;; First, process any user-specified init keywords that
	   ;; set instance variables.  When we process the defaults,
	   ;; we will see that these are already set, and will
	   ;; refrain from evaluating the default forms.  At the same time,
	   ;; we record any init keywords that this flavor doesn't handle.
	   (DO ((PL (CDR INIT-PLIST) (CDDR PL))) ((NULL PL))
	     (COND ((MEMQ (CAR PL) VAR-KEYWORDS)
		    (SET-IN-FCLOSURE INSTANCE (REMOVE-COLON (CAR PL))
				     (CADR PL)))
		   ((NOT (MEMQ (CAR PL) REMAINING-KEYWORDS))
		    (PUSH (CAR PL) UNHANDLED-KEYWORDS))))
	   ;; Now stick any default init plist items that aren't handled by 
	   ;; that onto the actual init plist.
	   (DO ((PL (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDDR PL)))
	       ((NULL PL))
	     (OR (MEMQ-ALTERNATED (CAR PL) (CDR INIT-PLIST))
		 (PUTPROP INIT-PLIST (FAST-EVAL (CADR PL)) (CAR PL)))))
	  (T
	   ;; Put defaults into the INIT-PLIST
	   (FLAVOR-DEFAULT-INIT-PLIST FLAVOR-NAME INIT-PLIST)
	   ;; For each init keyword, either initialize the corresponding 
	   ;; variable, remember that it will be handled later by an :INIT 
	   ;; method, or give an error for not being handled.
	   (DO L (CDR INIT-PLIST) (CDDR L) (NULL L)
	       (LET ((KEYWORD (CAR L)) (ARG (CADR L)))
		 (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS)))
		     ((NULL FFLS) (PUSH KEYWORD UNHANDLED-KEYWORDS))
		   (SETQ FFL (GET (CAR FFLS) 'FLAVOR))
		   (COND ((SETQ TEM (ASSQ KEYWORD (FLAVOR-INITABLE-INSTANCE-VARIABLES FFL)))
			  (SET-IN-FCLOSURE INSTANCE (REMOVE-COLON KEYWORD)
					   ARG)
			  (RETURN))
			 ((MEMQ KEYWORD (FLAVOR-INIT-KEYWORDS FFL))
			  (RETURN)))))))))
  ;; Complain if any keywords weren't handled, unless our caller
  ;; said it wanted to take care of this.
  (AND (NOT RETURN-UNHANDLED-KEYWORDS-P)
       UNHANDLED-KEYWORDS
       (FERROR () "Flavor ~S does not handle the init keyword~P ~{~S~^, ~}"
	       FLAVOR-NAME
	       (LENGTH UNHANDLED-KEYWORDS)
	       UNHANDLED-KEYWORDS))
  (AND (EQ SEND-INIT-MESSAGE-P 'MAYBE)
       (NOT (GET-HANDLER-FOR INSTANCE ':INIT))
       (SETQ SEND-INIT-MESSAGE-P NIL))
  (AND SEND-INIT-MESSAGE-P
       (SEND INSTANCE ':INIT INIT-PLIST))
  (VALUES INSTANCE UNHANDLED-KEYWORDS))

(DEFUN MEMQ-ALTERNATED (ELT LIST)
  (DO ((L LIST (CDDR L))) ((NULL L) NIL)
    (IF (EQ (CAR L) ELT) (RETURN L))))

(DEFUN FAST-EVAL (EXP)
  (COND ((OR (NUMBERP EXP) (STRINGP EXP)
	     (MEMQ EXP '(T NIL)))
	 EXP)
	((SYMBOLP EXP) (SYMEVAL EXP))
	((AND (LISTP EXP) (EQ (CAR EXP) 'QUOTE))
	 (CADR EXP))
	(T (EVAL EXP))))

(DEFUN FLAVOR-DEFAULT-INIT-PLIST (FLAVOR-NAME
				  &OPTIONAL (INIT-PLIST (NCONS NIL))
				  &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
	     "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET FFL 'FLAVOR))
    (DO L (GET (FLAVOR-PLIST FFL) ':DEFAULT-INIT-PLIST) (CDDR L) (NULL L)
      (DO ((M (CDR INIT-PLIST) (CDDR M)))
	  ((NULL M) (PUTPROP INIT-PLIST (EVAL (CADR L)) (CAR L)))
	(AND (EQ (CAR M) (CAR L)) (RETURN)))))
  INIT-PLIST)

;Returns non-NIL if the flavor allows the specified keyword in its init-plist,
;NIL if it doesn't.  The return value is the name of the component flavor
;that actually handles it.
(DEFUN FLAVOR-ALLOWS-INIT-KEYWORD-P (FLAVOR-NAME KEYWORD)
  (MAP-OVER-COMPONENT-FLAVORS 0 T T
      #'(LAMBDA (FL IGNORE KEYWORD)
	  (AND (OR (ASSQ KEYWORD (FLAVOR-INITABLE-INSTANCE-VARIABLES FL))
		   (MEMQ KEYWORD (FLAVOR-INIT-KEYWORDS FL)))
	       (FLAVOR-NAME FL)))
      FLAVOR-NAME NIL KEYWORD))

;;; Given the name of a flavor, return a list of all of the symbols that
;;; are valid init-options for the flavor, sorted alphabetically.
;;; Primary for inquiries by humans.
(DEFUN FLAVOR-ALLOWED-INIT-KEYWORDS (FLAVOR-NAME)
  (LET ((INIT-KEYWORDS NIL))
    (DECLARE (SPECIAL INIT-KEYWORDS))
    (MAP-OVER-COMPONENT-FLAVORS 0 T NIL
	#'(LAMBDA (FLAVOR IGNORE)
	    (DECLARE (SPECIAL INIT-KEYWORDS))
	    (SETQ INIT-KEYWORDS
		  (NCONC (MAPCAR #'(LAMBDA (KWD)
				     (IF (LISTP KWD) (CAR KWD) KWD))
				 (FLAVOR-LOCAL-INIT-KEYWORDS FLAVOR))
			 INIT-KEYWORDS)))
	FLAVOR-NAME NIL)
    (SORT (ELIMINATE-DUPLICATES INIT-KEYWORDS) #'ALPHALESSP)))

(DEFUN FLAVOR-LOCAL-INIT-KEYWORDS (FLAVOR)
  (APPEND (FLAVOR-INITABLE-INSTANCE-VARIABLES FLAVOR)
	  (FLAVOR-INIT-KEYWORDS FLAVOR)))

(DEFUN ELIMINATE-DUPLICATES (LIST &AUX L)
  (DOLIST (E LIST) (OR (MEMQ E L) (PUSH E L)))
  L)

; Function to map over all components of a specified flavor.  We must do the
;  DEPENDS-ON's to all levels first, then the INCLUDES's at all levels and
;  what they depend on.
; Note that it does the specified flavor itself as well as all its components.
; Note well: if there are included flavors, this does not do them in the
;  right order.  Also note well: if there are multiple paths to a component,
;  it will be done more than once.
; RECURSION-STATE is 0 except when recursively calling itself.
; ERROR-P is T if not-yet-defflavored flavors are to be complained about,
;  NIL if they are to be ignored.  This exists to get rid of certain
;  bootstrapping problems.
; RETURN-FIRST-NON-NIL is T if the iteration should terminate as soon
;  as FUNCTION returns a non-null result.
; At each stage FUNCTION is applied to the flavor (not the name), the
;  STATE, and any ARGS.  STATE is updated to whatever the function returns.
; The final STATE is the final result of this function.
; RECURSION-STATE is:
;  0	top-level
;  1	first-pass over just depends-on's
;  6  	second-pass, this flavor reached via depends-on's so don't do it again
;  2	second-pass, this flavor reached via includes's so do it.
(DEFVAR SOME-COMPONENT-UNDEFINED NIL)   ;If we find an undefined component, we put its name here.

(DEFUN MAP-OVER-COMPONENT-FLAVORS (RECURSION-STATE ERROR-P
				   RETURN-FIRST-NON-NIL FUNCTION FLAVOR-NAME
				   STATE &REST ARGS)
  (PROG (FL)
   (*CATCH 'MAP-OVER-COMPONENT-FLAVORS
    (COND ((OR ERROR-P (GET-FLAVOR FLAVOR-NAME))
	   (CHECK-ARG FLAVOR-NAME (SETQ FL (GET-FLAVOR FLAVOR-NAME))
		      "a defined flavor")
	   ;; First do this flavor, unless this is the second pass and it shouldn't be done
	   (OR (BIT-TEST 4 RECURSION-STATE)
	       (SETQ STATE (LEXPR-FUNCALL FUNCTION FL STATE ARGS)))
	   ;; After each call to the function, see if we're supposed to be done now
	   (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		(*THROW 'MAP-OVER-COMPONENT-FLAVORS NIL))
	   ;; Now do the depends-on's.
	   (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
	     (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
					    (IF (ZEROP RECURSION-STATE) 1 RECURSION-STATE)
					    ERROR-P RETURN-FIRST-NON-NIL
					    FUNCTION COMPONENT-FLAVOR STATE ARGS))
	     (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		  (*THROW 'MAP-OVER-COMPONENT-FLAVORS NIL)))
	   ;; Unless this is the first pass, do the includes.
	   (OR (BIT-TEST 1 RECURSION-STATE)
	       (DOLIST (COMPONENT-FLAVOR (FLAVOR-INCLUDES FL))
		 (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
						2 ERROR-P RETURN-FIRST-NON-NIL
						FUNCTION COMPONENT-FLAVOR STATE ARGS))
		 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		      (*THROW 'MAP-OVER-COMPONENT-FLAVORS NIL))))
	   ;; If this is the top-level, run the second pass on its depends-on's
	   ;; which doesn't do them but does do what they include.
	   (OR (NOT (ZEROP RECURSION-STATE))
	       (DOLIST (COMPONENT-FLAVOR (FLAVOR-DEPENDS-ON FL))
		 (SETQ STATE (LEXPR-FUNCALL #'MAP-OVER-COMPONENT-FLAVORS
					    6 ERROR-P RETURN-FIRST-NON-NIL
					    FUNCTION COMPONENT-FLAVOR STATE ARGS))
		 (AND RETURN-FIRST-NON-NIL (NOT (NULL STATE))
		      (*THROW 'MAP-OVER-COMPONENT-FLAVORS NIL)))))
	  ((NULL SOME-COMPONENT-UNDEFINED)
	   (SETQ SOME-COMPONENT-UNDEFINED FLAVOR-NAME)))))
  STATE)

;Call this when a flavor has been changed, it updates that flavor's compiled
; information and that of any that depend on it.
;If a compilation is in progress the compilations performed
; will get output as part of that compilation.
;SINGLE-OPERATION is NIL to do all operations, or the name of an operation
; which needs incremental compilation.
;USE-OLD-COMBINED-METHODS can be NIL to force regeneration of all combined methods.
; This is used if a wrapper has changed or there was a bug in the method-combining routine.
;DO-DEPENDENTS controls whether flavors that depend on this one are also compiled.
(DEFUN RECOMPILE-FLAVOR (FLAVOR-NAME
		         &OPTIONAL (SINGLE-OPERATION NIL) (*USE-OLD-COMBINED-METHODS* T)
				   (DO-DEPENDENTS T)
			 &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  ;; Only update the method combination if it has been done before, else 
  ;; doesn't matter
  (COND ((FLAVOR-METHOD-HASH-TABLE FL)
	 (OR (FLAVOR-DEPENDS-ON-ALL FL)
	     (COMPOSE-FLAVOR-COMBINATION FL))
	 (COMPOSE-METHOD-COMBINATION FL SINGLE-OPERATION)))
  (IF DO-DEPENDENTS
      (LET ((FDEFINE-FILE-PATHNAME NIL))	;Don't give warnings for combined methods
	(DOLIST (FN (FLAVOR-DEPENDED-ON-BY-ALL FL))
	  (IF (FLAVOR-METHOD-HASH-TABLE (GET FN 'FLAVOR))
	      (RECOMPILE-FLAVOR FN SINGLE-OPERATION *USE-OLD-COMBINED-METHODS* NIL))))))

;Make a list of all flavors that depend on this one, not including this flavor itself.
;This is a list of the names, not the defstructs.
(DEFUN FLAVOR-DEPENDED-ON-BY-ALL (FL &OPTIONAL (LIST-SO-FAR NIL) &AUX FFL)
  (DOLIST (FN (FLAVOR-DEPENDED-ON-BY FL))
    (OR (MEMQ FN LIST-SO-FAR)
	(NOT (SETQ FFL (GET FN 'FLAVOR)))
	(SETQ LIST-SO-FAR (FLAVOR-DEPENDED-ON-BY-ALL FFL (CONS FN LIST-SO-FAR)))))
  LIST-SO-FAR)

;This function takes care of flavor-combination.  It sets up the list
;of all component flavors, in appropriate order, and the list of all
;instance variables.  It generally needs to be called only once for a
;flavor, and must be called before method-combination can be dealt with.
(DEFVAR FLAVORS-BEING-COMPOSED NIL)

(DEFUN COMPOSE-FLAVOR-COMBINATION (FL &AUX FLS VARS ORDS REQS SIZE
				   (SOME-COMPONENT-UNDEFINED NIL)
				   (FLAVORS-BEING-COMPOSED
				     (CONS FL FLAVORS-BEING-COMPOSED)))
  ;; Make list of all component flavors' names.
  ;; This list is in outermost-first order.
  ;; Would be nice for this not to have to search to all levels, but for
  ;; the moment that is hard, so I won't do it.
  ;; Included-flavors are hairy: if not otherwise in the list of components, they
  ;; are stuck in after the rightmost component that includes them, along with
  ;; any components of their own not otherwise in the list.
  (SETQ FLS (COPYLIST (COMPOSE-FLAVOR-INCLUSION (FLAVOR-NAME FL) T)))
  ;; Don't mark this flavor as "composed" if there were errors.
  (OR SOME-COMPONENT-UNDEFINED
      (SETF (FLAVOR-DEPENDS-ON-ALL FL) FLS))
  ;; Vanilla-flavor may have been put in by magic, so maintain the dependencies
  ;; in case new methods get added to it later.
  (LET ((VAN (GET-FLAVOR 'SI:VANILLA-FLAVOR))
	(FLAV (FLAVOR-NAME FL)))
    (AND (NOT (NULL VAN))
	 (NEQ FLAV 'SI:VANILLA-FLAVOR)
	 (MEMQ 'SI:VANILLA-FLAVOR FLS)
	 (NOT (MEMQ FLAV (FLAVOR-DEPENDED-ON-BY VAN)))
	 (PUSH FLAV (FLAVOR-DEPENDED-ON-BY VAN))))
  ;; Compute what the instance variables will be, and in what order.
  ;; Also collect the required but not present instance variables, which go onto the
  ;; ADDITIONAL-INSTANCE-VARIABLES property.  The instance variables of the
  ;; :REQUIRED-FLAVORS work the same way.  Such instance variables are ok
  ;; for our methods to access.
  (DOLIST (F FLS)
    (SETQ F (GET-FLAVOR F))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES F))
      (OR (ATOM V) (SETQ V (CAR V)))
      (OR (MEMQ V VARS) (PUSH V VARS)))
    (SETQ REQS (UNION REQS
		      (GET (FLAVOR-PLIST F) ':REQUIRED-INSTANCE-VARIABLES)))
    ;; Any variables our required flavors have or require, we require.
    (DOLIST (FF (GET (FLAVOR-PLIST F) ':REQUIRED-FLAVORS))
      (COND ((AND (NOT (MEMQ FF FLS))
		  (SETQ FF (GET-FLAVOR FF))
		  (NOT (MEMQ FF (CDR FLAVORS-BEING-COMPOSED))))
	     (OR (FLAVOR-DEPENDS-ON-ALL FF) (COMPOSE-FLAVOR-COMBINATION FF))
	     (SETQ REQS
		   (UNION REQS (FLAVOR-ALL-INSTANCE-VARIABLES FF)
			  (GET (FLAVOR-PLIST FF) 'ADDITIONAL-INSTANCE-VARIABLES))))))
    (LET ((ORD (GET (FLAVOR-PLIST F) ':ORDERED-INSTANCE-VARIABLES)))
      ;; Merge into existing order requirement.  Shorter of the two must be
      ;; a prefix of the longer, and we take the longer.
      (DO ((L1 ORD (CDR L1))
	   (L2 ORDS (CDR L2)))
	  (NIL)
	(COND ((NULL L1) (RETURN NIL))
	      ((NULL L2) (RETURN (SETQ ORDS ORD)))
	      ((NEQ (CAR L1) (CAR L2))
	       (FERROR () ":ORDERED-INSTANCE-VARIABLES conflict, ~S vs ~S"
		           (CAR L1) (CAR L2)))))))
  ;; Must not merge this with the previous loop,
  ;; to avoid altering order of instance variables
  ;; if a DEFFLAVOR is redone.
  (DOLIST (F FLS)
    (SETQ F (GET-FLAVOR F)))
  ;; This NREVERSE makes it compatible with the old code.  There is no other reason for it.
  (SETQ VARS (NREVERSE VARS))
  ;; Apply ordering requirement by moving those variables to the front.
  (DOLIST (V ORDS)
    (OR (MEMQ V VARS)
	(FERROR () "Flavor ~S lacks instance variable ~S which has an order requirement"
		(FLAVOR-NAME FL) V))
    (SETQ VARS (DELQ V VARS)))
  (SETQ VARS (APPEND ORDS VARS))
  (SETF (FLAVOR-ALL-INSTANCE-VARIABLES FL) (COPYLIST VARS))
  ;; If there are any instance variables required but not present, save them
  ;; so that they can be declared special in methods.
  (DOLIST (V VARS)
    (SETQ REQS (DELQ V REQS)))
  (AND REQS (PUTPROP (FLAVOR-PLIST FL)
		     (COPYLIST REQS)
		     'ADDITIONAL-INSTANCE-VARIABLES))
  NIL)

(DEFUN COMPOSE-FLAVOR-INCLUSION (FLAVOR ERROR-P)
  (MULTIPLE-VALUE-BIND (FLS ADDITIONS) (COMPOSE-FLAVOR-INCLUSION-1 FLAVOR NIL ERROR-P)
    ;; The new additions may themselves imply more components
    (DO L ADDITIONS (CDR L) (NULL L)
      (LET ((MORE-FLS (COMPOSE-FLAVOR-INCLUSION-1 (CAR L) FLS ERROR-P)))
	(DOLIST (F MORE-FLS)
	  ;; This hair inserts F before (after) the thing that indirectly included it
	  ;; and then puts that next on ADDITIONS so it gets composed also
	  (LET ((LL (MEMQ (CAR L) FLS)))
	    (RPLACA (RPLACD LL (CONS (CAR LL) (CDR LL))) F)
	    (RPLACD L (CONS F (CDR L)))))))
    ;; Now attach vanilla-flavor if desired
    (OR (LOOP FOR FLAVOR IN FLS
	      THEREIS (GET (FLAVOR-PLIST (GET-FLAVOR FLAVOR))
			   ':NO-VANILLA-FLAVOR))
	(PUSH 'SI:VANILLA-FLAVOR FLS))
    (NREVERSE FLS)))

(local-declare ((special other-components))
(DEFUN COMPOSE-FLAVOR-INCLUSION-1 (FLAVOR OTHER-COMPONENTS ERROR-P)
  ;; First, make a backwards list of all the normal (non-included) components
  (LET ((FLS (MAP-OVER-COMPONENT-FLAVORS 1 ERROR-P NIL
	       #'(LAMBDA (FL LIST)
		   (SETQ FL (FLAVOR-NAME FL))
		   (OR (MEMQ FL LIST)
		       (MEMQ FL OTHER-COMPONENTS)
		       (PUSH FL LIST))
		   LIST)
	       FLAVOR NIL))
	(ADDITIONS NIL))
    ;; If there are any inclusions that aren't in the list, plug
    ;; them in right after (before in backwards list) their last (first) includer
    (DO L FLS (CDR L) (NULL L)
      (DOLIST (FL (FLAVOR-INCLUDES (GET-FLAVOR (CAR L))))
	(OR (MEMQ FL FLS)
	    (MEMQ FL OTHER-COMPONENTS)
	    (PUSH (CAR (RPLACA (RPLACD L (CONS (CAR L) (CDR L))) FL)) ADDITIONS))))
    (OR (MEMQ FLAVOR FLS)
	(SETQ FLS (NCONC FLS
			 (NREVERSE
			   (LOOP FOR FL IN (FLAVOR-INCLUDES (GET-FLAVOR FLAVOR))
				 UNLESS (OR (MEMQ FL FLS) (MEMQ FL OTHER-COMPONENTS))
				   COLLECT FL
				   AND DO (PUSH FL ADDITIONS))))))
    (VALUES FLS ADDITIONS))))

;Once the flavor-combination stuff has been done, do the method-combination stuff.
;The above function usually only gets called once, but this function gets called
;when a new method is added.
;Specify SINGLE-OPERATION to do this for just one operation, for incremental update.
;NOTE WELL: If a meth is in the method-table at all, it is considered to be defined
; for purposes of compose-method-combination.  Thus merely putprop'ing a method,
; or calling flavor-notice-method, will make the flavor think that method exists
; when it is next composed.  This is necessary to make compile-flavor-methods work.
; (Putprop must create the meth because loading does putprop before fdefine.)
(DEFUN COMPOSE-METHOD-COMBINATION (FL &OPTIONAL (SINGLE-OPERATION NIL)
				   &AUX TEM MAGIC-LIST ORDER DEF HT
				        MSG ELEM HANDLERS FFL PL)
  ;; If we are doing wholesale method composition,
  ;; compose the flavor bindings list also.
  ;; This way it is done often enough, but not at every defmethod.
  (IF (NOT SINGLE-OPERATION)
      (COMPOSE-FLAVOR-INITIALIZATIONS FL))
  ;; Look through all the flavors depended upon and collect the following:
  ;; A list of all the operations handled and all the methods for each, called MAGIC-LIST.
  ;; The default handler for unknown operations.
  ;; The declared order of entries in the select-method alist.
  ;; Also generate any automatically-created methods not already present.
  ;; MAGIC-LIST is roughly the same format as the flavor-method-table, see its comments.
  ;; Each magic-list entry is (message comb-type comb-order (type function-spec...)...)
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS)))
      ((NULL FFLS))
    (SETQ FFL (GET-FLAVOR (CAR FFLS))
	  PL (FLAVOR-PLIST FFL))
    (COND ((NOT SINGLE-OPERATION)
	   (AND (SETQ TEM (GET PL ':SELECT-METHOD-ORDER))
		(SETQ ORDER (NCONC ORDER (COPYLIST TEM))))))
    ;; Add data from flavor method-table to magic-list
    ;; But skip over combined methods, they are not relevant here
    (DOLIST (MTE (FLAVOR-METHOD-TABLE FFL))
      (SETQ MSG (CAR MTE)) 
     (COND ((OR (NOT SINGLE-OPERATION) (EQ MSG SINGLE-OPERATION))
	     ;; Well, we're supposed to concern ourselves with this operation
	     (SETQ ELEM (ASSQ MSG MAGIC-LIST))	;What we already know about it
	     (COND ((DOLIST (METH (CDDDR MTE))
		      (OR (EQ (METH-METHOD-TYPE METH) ':COMBINED)
			  (NOT (METH-DEFINEDP METH))
			  (RETURN T)))
		    ;; OK, this flavor really contributes to handling this operation
		   (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
		    ;; For each non-combined method for this operation, add it to the front
		    ;; of the magic-list element, thus they are in base-flavor-first order.
		    (DOLIST (METH (CDDDR MTE))
		      (LET ((TYPE (METH-METHOD-TYPE METH)))
			(COND ((EQ TYPE ':COMBINED))
			      ((NOT (METH-DEFINEDP METH)))
			      ((NOT (SETQ TEM (ASSQ TYPE (CDDDR ELEM))))
			       (PUSH (LIST TYPE (METH-FUNCTION-SPEC METH)) (CDDDR ELEM)))
			      ;; Don't let the same method get in twice (how could it?)
			      ((NOT (MEMQ (METH-FUNCTION-SPEC METH) (CDR TEM)))
			       (PUSH (METH-FUNCTION-SPEC METH) (CDR TEM))))))))
	     ;; Pick up method-combination declarations
	     (AND (CADR MTE) (CADR ELEM)	;If both specify combination-type, check
		  (OR (NEQ (CADR MTE) (CADR ELEM)) (NEQ (CADDR MTE) (CADDR ELEM)))
		  (FERROR ()
		      "Method-combination mismatch ~S-~S vs. ~S-~S, check your DEFFLAVOR's"
		      (CADR MTE) (CADDR MTE) (CADR ELEM) (CADDR ELEM)))
	     (COND ((CADR MTE)			;Save combination-type when specified
		    (OR ELEM (PUSH (SETQ ELEM (LIST* MSG NIL NIL NIL)) MAGIC-LIST))
		    (SETF (CADR ELEM) (CADR MTE))
		    (SETF (CADDR ELEM) (CADDR MTE)))) ))))
  ;; This NREVERSE tends to put base-flavor methods last
  (SETQ MAGIC-LIST (NREVERSE MAGIC-LIST))
  ;; Re-order the magic-list according to any declared required order
  (DOLIST (MSG (NREVERSE ORDER))
    (AND (SETQ TEM (ASSQ MSG MAGIC-LIST))
	 (SETQ MAGIC-LIST (CONS TEM (DELQ TEM MAGIC-LIST 1)))))
  ;; Map over the magic-list.  For each entry call the appropriate 
  ;; method-combining routine, which will return a function spec for 
  ;; the handler to use for this operation.
  (DOLIST (MTE MAGIC-LIST)
    ;; Punt if there are no methods at all (just a method-combination declaration)
    (COND ((CDDDR MTE)
	   ;; Process the :DEFAULT methods; if there are any untyped methods the
	   ;; default methods go away, otherwise they become untyped methods.
	   (AND (SETQ TEM (ASSQ ':DEFAULT (CDDDR MTE)))
		(IF (ASSQ NIL (CDDDR MTE))
		    (SETF (CDDDR MTE) (DELQ TEM (CDDDR MTE)))
		    (RPLACA TEM NIL)))
	   (OR (SETQ TEM (GET (OR (CADR MTE) ':DAEMON) 'METHOD-COMBINATION))
	       (FERROR () "~S unknown method combination type for ~S operation"
		           (CADR MTE) (CAR MTE)))
	   (PUSH (FUNCALL TEM FL MTE) HANDLERS))
	  (T (SETQ MAGIC-LIST (DELQ MTE MAGIC-LIST 1)))))
  ;; Get back into declared order.  We now have a list of function specs for handlers.
  (SETQ HANDLERS (NREVERSE HANDLERS))
  (COND (SINGLE-OPERATION
	  ;; If doing SINGLE-OPERATION, put it into the hash table.
	  ;; If the operation is becoming defined and wasn't, or vice versa,
	  ;; must recompute the which-operations list.
	  (OR (COND ((NULL HANDLERS)		;Deleting method
		     (NOT (REMHASH SINGLE-OPERATION
				   (FLAVOR-METHOD-HASH-TABLE FL))))
		    (T
		     (MULTIPLE-VALUE-BIND (NIL PREVIOUSLY-PRESENT)
		       (SWAPHASH SINGLE-OPERATION
				 (SETQ DEF (METHOD-FUNCTION-NAME
					    (CAR HANDLERS)))
				 (FLAVOR-METHOD-HASH-TABLE FL))
		       PREVIOUSLY-PRESENT)))
	      (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL)))
	;; Working on all operations at once.
	(T
	 (SETQ HT (MAKE-HASH-TABLE
		   ':SIZE (FIX (TIMES 1.5 (LENGTH MAGIC-LIST)))))
	 ;; If flavor currently has no hash table, it can't hurt to set 
	 ;; it early
	 (OR (FLAVOR-METHOD-HASH-TABLE FL)
	     (SETF (FLAVOR-METHOD-HASH-TABLE FL) HT))
	 (DO ((HANDLERS HANDLERS (CDR HANDLERS))
	      (ML MAGIC-LIST (CDR ML)))
	   ((NULL ML))
	   (PUTHASH (CAAR ML) (SETQ DEF (METHOD-FUNCTION-NAME (CAR HANDLERS)))
		    HT)
	   (SETF (FLAVOR-METHOD-HASH-TABLE FL) HT)
	   (SETF (FLAVOR-WHICH-OPERATIONS FL) NIL))	;This will have to be recomputed
	 ;; Make sure that the required variables and methods are present.
	 (VERIFY-REQUIRED-FLAVORS-METHODS-AND-IVARS FL MAGIC-LIST)))
  NIL)

(DEFUN VERIFY-REQUIRED-FLAVORS-METHODS-AND-IVARS (FL MAGIC-LIST)
  (DO ((FFLS (FLAVOR-DEPENDS-ON-ALL FL) (CDR FFLS))
       (MISSING-METHODS NIL)
       (MISSING-INSTANCE-VARIABLES NIL)
       (MISSING-FLAVORS NIL)
       (REQUIRING-FLAVOR-ALIST NIL))
      ((NULL FFLS)
       (AND (OR MISSING-INSTANCE-VARIABLES MISSING-METHODS MISSING-FLAVORS)
	    (FERROR () "Flavor ~S is missing ~
				~:[~2*~;instance variable~P ~{~S~^, ~} ~]~
				~:[~3*~;~:[~;and ~]method~P ~{~S~^, ~}~]~
				~:[~3*~;~:[~;and ~]component flavor~P ~{~S~^, ~}~]
Requiring Flavor alist: ~S"
		    (FLAVOR-NAME FL)
		    MISSING-INSTANCE-VARIABLES
		    (LENGTH MISSING-INSTANCE-VARIABLES)
		    MISSING-INSTANCE-VARIABLES
		    MISSING-METHODS
		    MISSING-INSTANCE-VARIABLES
		    (LENGTH MISSING-METHODS)
		    MISSING-METHODS
		    MISSING-FLAVORS
		    (OR MISSING-INSTANCE-VARIABLES MISSING-METHODS)
		    (LENGTH MISSING-FLAVORS)
		    MISSING-FLAVORS
		    REQUIRING-FLAVOR-ALIST)))
    (LET ((PL (FLAVOR-PLIST (GET (CAR FFLS) 'FLAVOR))))
      (DOLIST (REQM (GET PL ':REQUIRED-METHODS))
	(OR (ASSQ REQM MAGIC-LIST)
	    (MEMQ REQM MISSING-METHODS)
	    (PROGN (PUSH REQM MISSING-METHODS)
		   (PUSH (CONS (FIRST FFLS) REQM) REQUIRING-FLAVOR-ALIST))))
      (DOLIST (REQV (GET PL ':REQUIRED-INSTANCE-VARIABLES))
	(OR (MEMQ REQV (FLAVOR-ALL-INSTANCE-VARIABLES FL))
	    (MEMQ REQV MISSING-INSTANCE-VARIABLES)
	    (PROGN (PUSH REQV MISSING-INSTANCE-VARIABLES)
		   (PUSH (CONS (FIRST FFLS) REQV) REQUIRING-FLAVOR-ALIST))))
      (DOLIST (REQF (GET PL ':REQUIRED-FLAVORS))
	(OR (MEMQ REQF (FLAVOR-DEPENDS-ON-ALL FL))
	    (MEMQ REQF MISSING-FLAVORS)
	    (PROGN (PUSH REQF MISSING-FLAVORS)
		   (PUSH (CONS (FIRST FFLS) REQF) REQUIRING-FLAVOR-ALIST)))))))

;This is the default handler for flavors.
(DEFUN FLAVOR-UNCLAIMED-MESSAGE (MESSAGE ARGS)
  (DECLARE (SPECIAL SELF))
  (FORMAT T "The object ")
  (PRINT SELF)
  (FERROR ':UNCLAIMED-MESSAGE " received a ~S message, which went unclaimed.
The rest of the message was ~S~%" MESSAGE ARGS))

;Return an alist of operations and their handlers.
(DEFUN FLAVOR-METHOD-ALIST (FL)
  (IF (SYMBOLP FL) (SETQ FL (GET FL 'FLAVOR)))
  (IF FL
      (LET ((HT (FLAVOR-METHOD-HASH-TABLE FL))
	    (ALIST NIL))
	(AND HT
	     (MAPHASH #'(LAMBDA (OP METH-LOCATIVE &REST IGNORE)
			   (DECLARE (SPECIAL ALIST))
			   (PUSH (CONS OP (CAR METH-LOCATIVE)) ALIST))
		      HT))
	    ALIST)))

;; Make the instance-variable getting and setting methods
;; Updated 7Jul84 SMH@MIT-EMS:  As an apparent efficiency hack, the original
;; Lisp Machine code pushed each defmethod only if **just-compiling** were set
;; or the method were not yet defined.  The **just-compiling** switch has
;; unfortunately disappeared from the Franz version.  This caused
;; REcompilations of a flavor by a single instance of Liszt to omit all
;; automatic methods.  The bypass of the defmethod if the method is already
;; defined has thus been deleted.
(DEFUN COMPOSE-AUTOMATIC-METHODS (FL &AUX VV FORMS)
       (DOLIST (V (FLAVOR-GETTABLE-INSTANCE-VARIABLES FL))
	       (SETQ VV (CORRESPONDING-KEYWORD V))
	       (LET ((METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
		    (PUSH `(DEFMETHOD (,(FLAVOR-NAME FL) ,VV) () ,V)
			  FORMS)))
       (DOLIST (V (FLAVOR-SETTABLE-INSTANCE-VARIABLES FL))
	       (SETQ VV (INTERN (FORMAT () ":set-~A" V)))
	       (LET ((METH `(:METHOD ,(FLAVOR-NAME FL) ,VV)))
		    (PUSH `(DEFMETHOD (,(FLAVOR-NAME FL) ,VV) (VALUE)
				      (SETQ ,V VALUE))
			  FORMS)))
       (NREVERSE FORMS))

;Given a symbol return the corresponding one in the keyword package
(DEFUN CORRESPONDING-KEYWORD (SYMBOL)
  (IF (= #/: (GETCHARN SYMBOL 1)) SYMBOL
      (INTERN (CONCAT ":" SYMBOL))))

;Figure out the information needed to instantiate a flavor quickly.

;We store these three properties on the flavor:
;INSTANCE-VARIABLE-INITIALIZATIONS - alist of (ivar-index . init-form)
;REMAINING-DEFAULT-PLIST - a default plist from which kwds that init ivars 
;			   have been removed.
;ALL-INITABLE-INSTANCE-VARIABLES - 
;	a list parallel to FLAVOR-ALL-INSTANCE-VARIABLES which has either
;	the keyword to init with or NIL.
;REMAINING-INIT-KEYWORDS - 
;	the init keywords that are handled and don't just init ivars.

;We also set up the FLAVOR-DEFAULT-HANDLER of the flavor.

(DEFUN COMPOSE-FLAVOR-INITIALIZATIONS (FL &AUX ALIST
				       (REMAINING-DEFAULT-PLIST (LIST NIL))
				       ALL-INITABLE-IVARS)
  (SETQ ALL-INITABLE-IVARS (MAKE-LIST
			    (LENGTH (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
  ;; First make the mask saying which ivars can be inited by init keywords.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (LET ((FFL (GET-FLAVOR FFL)))
      (OR (FLAVOR-DEFAULT-HANDLER FL)
	  (SETF (FLAVOR-DEFAULT-HANDLER FL)
		(GET (FLAVOR-PLIST FFL) ':DEFAULT-HANDLER)))
      (DOLIST (IIV (FLAVOR-INITABLE-INSTANCE-VARIABLES FFL))
	(LET ((INDEX (FIND-POSITION-IN-LIST (CDR IIV)
			(FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	  (AND INDEX
	       (SETF (NTH INDEX ALL-INITABLE-IVARS)
		     (CAR IIV)))))))
  ;; Then look at all the default init plists, for anything there that
  ;; initializes an instance variable.  If it does, make an entry on ALIST.
  ;; Any that doesn't initialize a variable, put on the "remaining" list.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET-FLAVOR FFL))
    (DO ((L (GET (FLAVOR-PLIST FFL) ':DEFAULT-INIT-PLIST) (CDDR L))) ((NULL L))
      (LET* ((KEYWORD (CAR L)) (ARG (CADR L))
	     (INDEX (FIND-POSITION-IN-LIST KEYWORD ALL-INITABLE-IVARS)))
	(IF INDEX
	    (OR (ASSQ INDEX ALIST)
		(PUSH (LIST INDEX ARG)
		      ALIST))
	  ;; This keyword does not just initialize an instance variable.
	    (OR (MEMQ-ALTERNATED KEYWORD (CDR REMAINING-DEFAULT-PLIST))
		(PUTPROP REMAINING-DEFAULT-PLIST ARG KEYWORD))))))
  ;; Then, look for default values provided in list of instance vars.
  (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
    (SETQ FFL (GET-FLAVOR FFL))
    (DOLIST (V (FLAVOR-LOCAL-INSTANCE-VARIABLES FFL))
      (AND (NOT (ATOM V))
	   ;; When we find one, put it in if there is no init for that variable yet.
	   (LET ((INDEX (FIND-POSITION-IN-LIST (CAR V)
			       (FLAVOR-ALL-INSTANCE-VARIABLES FL))))
	     (AND (NOT (ASSQ INDEX ALIST))
		  (PUSH (LIST INDEX
			      (CADR V))
			ALIST)))))) 
  (SETF (FLAVOR-INSTANCE-VARIABLE-INITIALIZATIONS FL)
	(SORTCAR ALIST #'LESSP))
  (SETF (FLAVOR-REMAINING-DEFAULT-PLIST FL) (CDR REMAINING-DEFAULT-PLIST))
  (SETF (FLAVOR-ALL-INITABLE-INSTANCE-VARIABLES FL) ALL-INITABLE-IVARS)
  (SETF (FLAVOR-REMAINING-INIT-KEYWORDS FL)
	(LOOP FOR K IN (FLAVOR-ALLOWED-INIT-KEYWORDS FL)
	      UNLESS (MEMQ K ALL-INITABLE-IVARS)
	      COLLECT K)))

; Method-combination functions.  Found on the SI:METHOD-COMBINATION property
; of the combination-type.  These are passed the flavor structure, and the
; magic-list entry, and must return the function-spec for the handler
; to go into the select-method, defining any necessary functions.
; This function interprets combination-type-arg,
; which for many combination-types is either :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.

; :DAEMON combination
; The primary method is the outermost untyped-method (or :DEFAULT).
; The :BEFORE methods are called base-flavor-last, the :AFTER methods are called
; base-flavor-first.  An important optimization is not to generate a combined-method
; if there is only a primary method.  You are allowed to omit the primary method
; if there are any daemons (I'm not convinced this is really a good idea) in which
; case the method's returned value will be NIL.
(DEFUN (:DAEMON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS) PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS)))))

(DEFUN DAEMON-COMBINATION (PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
			   &OPTIONAL OR-METHODS AND-METHODS)
  (LET ((INNER-CALL (AND PRIMARY-METHOD (METHOD-CALL PRIMARY-METHOD))))
    (IF (AND INNER-CALL AFTER-METHODS)
	(SETQ INNER-CALL `(MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
			    ,INNER-CALL)))
    (AND OR-METHODS (SETQ INNER-CALL
			  `(OR ,@(MAPCAR 'METHOD-CALL OR-METHODS)
			       ,INNER-CALL)))
    (AND AND-METHODS (SETQ INNER-CALL
			   `(AND ,@(MAPCAR 'METHOD-CALL AND-METHODS)
				 ,INNER-CALL)))
    `(PROGN 
       ,@(MAPCAR 'METHOD-CALL BEFORE-METHODS)
       ,(IF AFTER-METHODS
	    ;; Kludge to return a few multiple values
	    `(PROG (.VAL1. .VAL2. .VAL3.)
		   ,INNER-CALL
		   ,@(MAPCAR 'METHOD-CALL AFTER-METHODS)
		   (RETURN .VAL1. .VAL2. .VAL3.))
	    ;; No :AFTER methods, hair not required
	    ;; You are allowed to not have a primary method
	    INNER-CALL))))

(DEFUN METHOD-CALL (METHOD)
  `(LEXPR-FUNCALL #',(METHOD-FUNCTION-NAME METHOD) .DAEMON-CALLER-ARGS.))

; :DAEMON-WITH-OVERRIDE combination
; This is the same as :DAEMON (the default), except that :OVERRIDE type methods
; are combined with the :BEFORE-primary-:AFTER methods in an OR.  This allows
; overriding of the main methods function.  For example, a combined method as follows
; might be generated: (OR (FOO-OVERRIDE-BAR-METHOD) (PROGN (FOO-BEFORE-BAR-METHOD)))
(DEFUN (:DAEMON-WITH-OVERRIDE METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL
						  '(:BEFORE :AFTER :OVERRIDE) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
	(OVERRIDE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY
					       ':OVERRIDE T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
	     (NULL OVERRIDE-METHODS)
	     PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(OR ,@(MAPCAR 'METHOD-CALL OVERRIDE-METHODS)
	       ,(DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS))))))

; :DAEMON-WITH-OR combination
; This is the same as :DAEMON (the default), except that :OR type methods
; are combined with the primary methods inside an OR, and used in place of
; the primary method in :DAEMON type combination.
; For example, the following combined method might be generated:
; (PROGN (FOO-BEFORE-BAR-METHOD)
;	 (PROG (.VAL1. .VAL2. .VAL3.)
;	       (OR (FOO-OR-BAR-METHOD)
;		   (BAZ-OR-BAR-METHOD)
;		   (MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
;		     (BUZZ-PRIMARY-METHOD)))
;	       (FOO-AFTER-BAR-METHOD)
;	       (RETURN .VAL1. .VAL2. .VAL3.)))

(DEFUN (:DAEMON-WITH-OR METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER :OR) T
						  ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
	(OR-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':OR T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
	     (NULL OR-METHODS)
	     PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
			      OR-METHODS)))))

; :DAEMON-WITH-AND combination
; This is the same as :DAEMON (the default), except that :AND type methods
; are combined with the primary methods inside an AND, and used in place of
; the primary method in :DAEMON type combination.
; For example, the following combined method might be generated:
; (PROGN (FOO-BEFORE-BAR-METHOD)
;	 (PROG (.VAL1. .VAL2. .VAL3.)
;	       (AND (FOO-AND-BAR-METHOD)
;		    (BAZ-AND-BAR-METHOD)
;		    (MULTIPLE-VALUE (.VAL1. .VAL2. .VAL3.)
;		      (BUZZ-PRIMARY-METHOD)))
;	       (FOO-AFTER-BAR-METHOD)
;	       (RETURN .VAL1. .VAL2. .VAL3.)))

(DEFUN (:DAEMON-WITH-AND METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((PRIMARY-METHOD (CAR (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL '(:BEFORE :AFTER :AND)
						  T ':BASE-FLAVOR-LAST)))
	(BEFORE-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':BEFORE T T
					     ':BASE-FLAVOR-LAST))
	(AFTER-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AFTER T T
					    ':BASE-FLAVOR-FIRST))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY))
	(AND-METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY ':AND T T NIL)))
    ;; Remove shadowed primary methods from the magic-list-entry so that it won't look like
    ;; we depend on them (which could cause extraneous combined-method recompilation).
    (LET ((MLE (ASSQ NIL (CDDDR MAGIC-LIST-ENTRY))))
      (AND (CDDR MLE)
	   (SETF (CDR MLE) (LIST PRIMARY-METHOD))))
    (OR (AND (NOT WRAPPERS-P) (NULL BEFORE-METHODS) (NULL AFTER-METHODS)
	     (NULL AND-METHODS)
	     PRIMARY-METHOD)
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  (DAEMON-COMBINATION PRIMARY-METHOD BEFORE-METHODS AFTER-METHODS
			      NIL AND-METHODS)))))

; :LIST combination
; No typed-methods allowed.  Returns a list of the results of all the methods.
; There will always be a combined-method, even if only one method to be called.
(DEFUN (:LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	    (CONS 'LIST (MAPCAR 'METHOD-CALL
				(GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL))))))

; :INVERSE-LIST combination
; No typed-methods allowed.  Apply each method to an element of the list.  Given
; the result of a :LIST-combined method with the same ordering, and corresponding
; method definitions, the result that emerged from each component flavor gets handed
; back to that same flavor.  The combined-method returns no particular value.
(DEFUN (:INVERSE-LIST METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
      (MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	 `(LET ((.FOO. (CADR .DAEMON-CALLER-ARGS.)))
	    . ,(DO ((ML (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL) (CDR ML))
		    (R NIL))
		   ((NULL ML) (NREVERSE R))
		 (PUSH `(FUNCALL #',(CAR ML)
			  (CAR .DAEMON-CALLER-ARGS.) (CAR .FOO.))
		       R)
		 (AND (CDR ML) (PUSH '(SETQ .FOO. (CDR .FOO.)) R)))))))

; Combination types PROGN, AND, OR, MAX, MIN, +, APPEND, NCONC
; These just call all the untyped methods, inside the indicated special form.
; As an optimization, if there is only one method it is simply called.
; ?? There should be hair where methods with an extra keyword in them
; get to act as conditionals controlling which other methods get called,
; if anyone can ever specify exactly what this means.
(DEFPROP :PROGN SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :AND SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :OR SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :MAX SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :MIN SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :+ SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :APPEND SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)
(DEFPROP :NCONC SIMPLE-METHOD-COMBINATION METHOD-COMBINATION)

; The following "tasteless" crock is necessary to make all work in Franz:
(eval-when (load eval) (loop for (to . from) in
			     '((:progn . progn)
			       (:and . and)
			       (:or . or)
			       (:max . max)
			       (:min . min)
			       (:+ . +)
			       (:append . append)
			       (:nconc . nconc))
			      do
			      (putd to (getd from))))

(DEFUN SIMPLE-METHOD-COMBINATION (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL NIL))
	(WRAPPERS-P (SPECIALLY-COMBINED-METHODS-PRESENT MAGIC-LIST-ENTRY)))
    (OR (AND (NOT WRAPPERS-P) (NULL (CDR METHODS)) (CAR METHODS))
	(HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	   (CONS (CADR MAGIC-LIST-ENTRY)
		 (MAPCAR 'METHOD-CALL
			 METHODS))))))

; :PASS-ON combination
; The values from the individual methods are the arguments to the next one;
; the values from the last method are the values returned by the combined
; method.  Format is 
;    (:METHOD-COMBINATION (:PASS-ON (ORDERING . ARGLIST)) . OPERATION-NAMES)
; ORDERING is :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST.  ARGLIST can have 
; &AUX and &OPTIONAL.

(DEFUN (:PASS-ON METHOD-COMBINATION) (FL MAGIC-LIST-ENTRY)
  (LET ((METHODS (GET-CERTAIN-METHODS MAGIC-LIST-ENTRY NIL NIL NIL
				      (CAADDR MAGIC-LIST-ENTRY)))
	(ARGLIST (CDADDR MAGIC-LIST-ENTRY))
	ARGS REST-ARG-P)
    (DO ((L ARGLIST (CDR L))
	 (ARG)
	 (NL NIL))
	((NULL L)
	 (SETQ ARGS (NREVERSE NL)))
      (SETQ ARG (CAR L))
      (AND (LISTP ARG)
	   (SETQ ARG (CAR ARG)))
      (COND ((EQ ARG '&REST)
	     (SETQ REST-ARG-P T))
	    ((EQ ARG '&AUX))
	    (T
	     (PUSH ARG NL))))      
    (OR (HAVE-COMBINED-METHOD FL MAGIC-LIST-ENTRY)
	(MAKE-COMBINED-METHOD FL MAGIC-LIST-ENTRY
	  `(DESTRUCTURING-BIND ,(CONS '.OPERATION. ARGLIST) SI:.DAEMON-CALLER-ARGS.
	     . ,(DO ((METHS METHODS (CDR METHS))
		     (LIST NIL)
		     (METH))
		    ((NULL METHS)
		     (NREVERSE LIST))
		  (SETQ METH `(,(IF REST-ARG-P
				    'LEXPR-FUNCALL
				  'FUNCALL)
			       #',(CAR METHS) .OPERATION. . ,ARGS))
		  (AND (CDR METHS)
		       (SETQ METH (IF (NULL (CDR ARGS))
				      `(SETQ ,(CAR ARGS) ,METH)
				    `(MULTIPLE-VALUE ,ARGS ,METH))))
		  (PUSH METH LIST)))))))

; This function does most of the analysis of the magic-list-entry needed by
; method-combination functions, including most error checking.
; Returns a list of the method symbols for METHOD-TYPE extracted from 
; MAGIC-LIST-ENTRY.  This value is shared with the data structure, don't 
; bash it.  OTHER-METHODS-ALLOWED is a list of method types not to complain
;about (T = allow all).
;   NO-METHODS-OK = NIL means to complain if the returned value would be NIL.
;   ORDERING-DECLARATION is :BASE-FLAVOR-FIRST, :BASE-FLAVOR-LAST, or NIL 
;    meaning take one of those symbols from the MAGIC-LIST-ENTRY."

(DEFUN GET-CERTAIN-METHODS (MAGIC-LIST-ENTRY METHOD-TYPE OTHER-METHODS-ALLOWED
			    NO-METHODS-OK ORDERING-DECLARATION
			    &AUX (METHODS NIL))
  ;; Find the methods of the desired type, and barf at any extraneous methods
  (DOLIST (X (CDDDR MAGIC-LIST-ENTRY))
    (COND ((EQ (CAR X) METHOD-TYPE) (SETQ METHODS (CDR X)))
	  ((ASSQ (CAR X) *SPECIALLY-COMBINED-METHOD-TYPES*) ) ;Wrappers ignored at this level
	  ((OR (EQ OTHER-METHODS-ALLOWED T) (MEMQ (CAR X) OTHER-METHODS-ALLOWED)) )
	  (T (FERROR () "~S ~S method(s) illegal when using :~A method-combination"
		         (CAR X) (CAR MAGIC-LIST-ENTRY)
			 (OR (CADR MAGIC-LIST-ENTRY) ':DAEMON)))))
  ;; Complain if no methods supplied
  (AND (NULL METHODS) (NOT NO-METHODS-OK)
       (FERROR () "No ~S ~S method(s) supplied to :~A method-combination"
	           METHOD-TYPE (CAR MAGIC-LIST-ENTRY) (CADR MAGIC-LIST-ENTRY)))
  ;; Get methods into proper order.  Don't use NREVERSE!
  (SELECTQ (OR ORDERING-DECLARATION (SETQ ORDERING-DECLARATION (CADDR MAGIC-LIST-ENTRY)))
    (:BASE-FLAVOR-FIRST )
    (:BASE-FLAVOR-LAST (SETQ METHODS (REVERSE METHODS)))
    (OTHERWISE (FERROR () "~S invalid method combination order;
 must be :BASE-FLAVOR-FIRST or :BASE-FLAVOR-LAST"
		           ORDERING-DECLARATION)))
  METHODS)

(DEFUN SPECIALLY-COMBINED-METHODS-PRESENT (MLE)
  (LOOP FOR (TYPE) IN (CDDDR MLE)
	THEREIS (ASSQ TYPE *SPECIALLY-COMBINED-METHOD-TYPES*)))

;; It is up to the caller to decide that a combined-method is called for at all.
;; If one is, this function decides whether it already exists OK or needs
;; to be recompiled.  Returns the symbol for the combined method if it is
;; still valid, otherwise returns NIL.
;; Always canonicalizes the magic-list-entry, since it will be needed
;; canonicalized later.
(DEFUN HAVE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY
			     &AUX OPERATION-NAME CMS MTE OLD-MLE OLD-CMS TEM OMETH)
  ;; Canonicalize the magic-list-entry so can compare with EQUAL
  (SETF (CDDDR MAGIC-LIST-ENTRY)		;Canonicalize before comparing
	(SORTCAR (CDDDR MAGIC-LIST-ENTRY) #'ALPHALESSP))	;Sort by method-type
  (SETQ OPERATION-NAME (CAR MAGIC-LIST-ENTRY))
  ;; See if we can inherit one in either the current or future (being-compiled) world,
  ;; or use an existing combined method of this flavor.
  ;; Get the :COMBINED method function spec for this flavor.  Note that if a suitable
  ;; one can be inherited, we will do so.
  ;; *USE-OLD-COMBINED-METHODS* controls whether we reuse an existing one for this
  ;; flavor; if we inherit one it will always be up-to-date already.
  ;; If all OK, return the function spec, else return NIL if new combined method must be made.
  (OR (DOLIST (FFL (FLAVOR-DEPENDS-ON-ALL FL))
	(LET ((FLAVOR1 (GET-FLAVOR FFL)))
	  (AND (OR (NEQ FLAVOR1 FL) *USE-OLD-COMBINED-METHODS*)
	       ;; ^ Combined methods of this flavor can be used only if permitted.
	       (SETQ MTE (ASSQ OPERATION-NAME (FLAVOR-METHOD-TABLE FLAVOR1)))
	       (SETQ OMETH (METH-LOOKUP ':COMBINED (CDDDR MTE)))
	       (METH-DEFINEDP OMETH)
	       (METH-DEFINITION OMETH)
	       (SETQ CMS (METH-FUNCTION-SPEC OMETH))
	       (EQUAL MAGIC-LIST-ENTRY
		      (SETQ TEM (GET (METH-PLIST OMETH) 'COMBINED-METHOD-DERIVATION)))
	       (RETURN CMS)))
	;Save first combined-method seen for tracing, it's the one we would
	;have been most likely to inherit
	(OR OLD-CMS (NULL CMS) (NULL TEM)
	    (SETQ OLD-CMS CMS OLD-MLE TEM)))

      ;; Have to make a new combined method.  Trace if desired, but return NIL in any case.
      (PROGN
	(COND (*FLAVOR-COMPILE-TRACE*
	       (FORMAT *FLAVOR-COMPILE-TRACE*
		       "~&~S's ~S combined method needs to be recompiled~%to come from "
		       (FLAVOR-NAME FL) OPERATION-NAME)
	       (PRINT-COMBINED-METHOD-DERIVATION MAGIC-LIST-ENTRY *FLAVOR-COMPILE-TRACE*)
	       (COND (OLD-CMS
		      (FORMAT *FLAVOR-COMPILE-TRACE*
			      "~%rather than using ~S which comes from " OLD-CMS)
		      (PRINT-COMBINED-METHOD-DERIVATION OLD-MLE *FLAVOR-COMPILE-TRACE*))
		     ((NOT *USE-OLD-COMBINED-METHODS*)
		      (FORMAT *FLAVOR-COMPILE-TRACE* "~%because of forced recompilation.")))))
	NIL)))


(DEFUN PRINT-COMBINED-METHOD-DERIVATION (MLE STREAM)
  (LOOP FOR (TYPE . FUNCTION-SPECS) IN (CDDDR MLE)
	DO (LOOP FOR FUNCTION-SPEC IN FUNCTION-SPECS DO (FORMAT STREAM "~S " FUNCTION-SPEC)))
  (IF (OR (CADR MLE) (CADDR MLE))
      (FORMAT STREAM "with method-combination ~S ~S" (CADR MLE) (CADDR MLE))))

;; This function creates a combined-method, and returns the appropriate function spec.
;; Its main job in life is to take care of wrappers.  Note the combined method
;; always takes a single &REST argument named .DAEMON-CALLER-ARGS.
;; FORM is a single form to be used as the body.
(DEFUN MAKE-COMBINED-METHOD (FL MAGIC-LIST-ENTRY FORM &AUX FSPEC WRAPPERS)
  ;; Get the function spec which will name the combined-method
  (SETQ FSPEC `(:METHOD ,(FLAVOR-NAME FL) :COMBINED ,(CAR MAGIC-LIST-ENTRY)))
  ;; Put the wrappers around the form.  The base-flavor wrapper goes on the inside.
  (SETQ WRAPPERS (GET-SPECIALLY-COMBINED-METHODS MAGIC-LIST-ENTRY FL))
  (DOLIST (METHOD WRAPPERS)
    (SETQ FORM (FUNCALL (CADR (ASSQ (CADDR METHOD) *SPECIALLY-COMBINED-METHOD-TYPES*))
			METHOD FORM)))
  ;; Remember that it's going to be there, for HAVE-COMBINED-METHOD
  (FLAVOR-NOTICE-METHOD FSPEC)
  ;; Compile the function.  It will be inserted into the flavor's tables either
  ;; now or when the QFASL file is loaded.
  (COMPILE-AT-APPROPRIATE-TIME
    FL
    FSPEC
    `(LAMBDA (&REST .DAEMON-CALLER-ARGS.)
       .DAEMON-CALLER-ARGS.
       ,FORM)
    `(FUNCTION-SPEC-PUTPROP ',FSPEC
			    ',MAGIC-LIST-ENTRY
			    'COMBINED-METHOD-DERIVATION))
  FSPEC)


(LOCAL-DECLARE ((SPECIAL *FL*))
(DEFUN GET-SPECIALLY-COMBINED-METHODS (MLE *FL*)
  (SORT (LOOP FOR (TYPE . FSPECS) IN (CDDDR MLE)
	      WHEN (ASSQ TYPE *SPECIALLY-COMBINED-METHOD-TYPES*)
	        APPEND FSPECS)
	#'(LAMBDA (FS1 FS2)
	    (LOOP WITH FL1 = (CADR FS1) AND FL2 = (CADR FS2)
		  FOR SUP IN (FLAVOR-DEPENDS-ON-ALL *FL*)
		  WHEN (EQ SUP FL2) RETURN T	;Base flavor earlier in list
		  WHEN (EQ SUP FL1) RETURN NIL)))))

(DEFUN PUT-WRAPPER-INTO-COMBINED-METHOD (WRAPPER-NAME FORM)
  (LET ((DEF (COND #-Franz ((DECLARED-DEFINITION WRAPPER-NAME))
		   ;; What would the above mean in Franz?
		   ((getd (method-function-name WRAPPER-NAME)))
		   (T (FERROR () "~S supposed to be a wrapper macro, but missing!"
			      WRAPPER-NAME)))))
    (COND ((not (and (dtpr DEF)
		     (eq (CAR DEF) 'MACRO)))
	   (FERROR () "~S, supposed to be a wrapper macro, is poorly formed. Definiton is ~s"
		   WRAPPER-NAME DEF)))
  ;; Here we just put the wrapper in as a macro.  It will be expanded by the compiler.
    `(MACROCALL ,WRAPPER-NAME .DAEMON-CALLER-ARGS. ,FORM)))

;Sort of a macro version of funcall, for wrappers
(DEFMACRO MACROCALL (&REST X)
  (LET ((MACRO (COND #-Franz ((DECLARED-DEFINITION (CAR X)))
		     ((method-function-name (CAR X)))
		     (T (FERROR () "Unable to find definition of wrapper ~s at expand time"
				(CAR X))))))
    (IF (AND (LISTP MACRO) (EQ (CAR MACRO) 'MACRO))
	(FUNCALL (cons 'lambda (CDR MACRO)) X)
	;--- Temporary code so I can test things in the kludge environment
	(IF (AND (SYMBOLP MACRO) (LISTP (getd MACRO))
		 (EQ (CAR (getd MACRO)) 'MACRO))
	    (FUNCALL (cons 'lambda (CDR (getd MACRO))) X)
	    (FERROR () "~S evaluated to ~S, which is not a macro"
		    (CAR X) MACRO)))))

;; Given a functional object, return its subfunction to do the given 
;; operation or NIL.   Returns NIL if it does not reduce to a select-method 
;; or if it does not handle that."
(DEFUN GET-HANDLER-FOR (FUNCTION OPERATION &OPTIONAL (SUPERIORS-P T) &AUX TEM)
  (COND ((SYMBOLP FUNCTION)
	 (COND ((SETQ TEM (GET FUNCTION 'FLAVOR))
		(GET-FLAVOR-HANDLER-FOR TEM OPERATION))))
	((:TYPEP FUNCTION 'FLAVOR)
	 (GET-FLAVOR-HANDLER-FOR (FLAVOR-NAME FUNCTION) OPERATION))
	((INSTANCEP FUNCTION)
; SMH@EMS VVV
;	 (GET-FLAVOR-HANDLER-FOR (FLAVOR-NAME (VREF FUNCTION 6))
;				 OPERATION)
	 (GET-FLAVOR-HANDLER-FOR
	  (FLAVOR-NAME (INT:FCLOSURE-STACK-STUFF (VREF FUNCTION 2)))
				 OPERATION)
; SMH@EMS ^^^
)))

;;; Get the function that would handle an operation for a flavor
(DEFUN GET-FLAVOR-HANDLER-FOR (FLAVOR-NAME OPERATION &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR))
	     "the name of a flavor")
  ;; Do any composition (compilation) of combined stuff, if not done already
  (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
  (OR (FLAVOR-METHOD-HASH-TABLE FL) (COMPOSE-METHOD-COMBINATION FL))
  (GETHASH OPERATION (FLAVOR-METHOD-HASH-TABLE FL)))

(DEFUN SYMEVAL-IN-INSTANCE (INSTANCE VAR)
  (CHECK-ARG INSTANCE INSTANCEP "an instance")
  (SYMEVAL-IN-FCLOSURE INSTANCE VAR))

(DEFSETF SYMEVAL-IN-INSTANCE (E V) `(SET-IN-INSTANCE ,(CADR E) ,(CADDR E) ,V))

(DEFUN SET-IN-INSTANCE (INSTANCE VAR VAL)
  (CHECK-ARG INSTANCE INSTANCEP "an instance")
  (SET-IN-FCLOSURE INSTANCE VAR VAL))

;Interface to the compiler.
(DEFUN COMPILE-AT-APPROPRIATE-TIME (FL NAME LAMBDA-EXP &OPTIONAL FORM-TO-EVAL)
  (PUTD (METHOD-FUNCTION-NAME NAME)
	(LAMBDACVT (CDR LAMBDA-EXP))))

;This macro takes flavor names as "arguments".  It causes the compiler
;to include the appropriate methods in the qfasl file, provided all the
;component flavors are defined.
(DEFMACRO COMPILE-FLAVOR-METHODS (&REST FLAVOR-NAMES)
  `(PROGN 'COMPILE
     (EVAL-WHEN (COMPILE)
       . ,(MAPCAN #'(LAMBDA (FLAVOR-NAME)
		      (NCONC (AND (GET FLAVOR-NAME 'FLAVOR)
				  (NCONS `(PUTPROP (FLAVOR-PLIST
						    (GET ',FLAVOR-NAME 'FLAVOR))
						   T
						   'COMPILE-FLAVOR-METHODS)))
			     (NCONS `(COMPILE-FLAVOR-METHODS-1 ',FLAVOR-NAME))))
		  FLAVOR-NAMES))
     (EVAL-WHEN (LOAD EVAL)
       . ,(MAPCAR #'(LAMBDA (FLAVOR-NAME) `(COMPILE-FLAVOR-METHODS-2 ',FLAVOR-NAME))
		  FLAVOR-NAMES))))

;; Cause the combined-methods to get compiled.
;; Executed only from the compiler, and does something
;; only if compiling to a file.
(DEFUN COMPILE-FLAVOR-METHODS-1 (FLAVOR-NAME &AUX FL)
  (IF (JUST-COMPILING)
      (LET ((*JUST-COMPILING* T)
	    (*USE-OLD-COMBINED-METHODS* NIL))
	(COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME 'COMPILE-FLAVOR-METHODS)
	       (SETQ FL (GET-FLAVOR FLAVOR-NAME))
	       ;; Make sure we are not hacking the installed flavor object,
	       ;; in case there is no defflavor or defmethod for the flavor in this file.
	       (AND (EQ FL (GET FLAVOR-NAME 'FLAVOR))
		    (COMPILATION-DEFINE-FLAVOR
		      FLAVOR-NAME
		      (SETQ FL (FLAVOR-REDEFINITION-FOR-COMPILATION FL NIL))))
	       (OR (FLAVOR-DEPENDS-ON-ALL FL)
		   (COMPOSE-FLAVOR-COMBINATION FL))
	       (COMPOSE-METHOD-COMBINATION FL NIL))))))

;; Do the composition now.  This should normally just generate data-structure
;; as the methods should already all have been compiled, unless something has changed.
(DEFUN COMPILE-FLAVOR-METHODS-2 (FLAVOR-NAME &AUX FL)
  (CHECK-ARG FLAVOR-NAME (SETQ FL (GET FLAVOR-NAME 'FLAVOR)) "the name of a flavor")
  (PUTPROP (FLAVOR-PLIST FL) T 'COMPILE-FLAVOR-METHODS)
  (COND ((FLAVOR-COMPONENTS-DEFINED-P FLAVOR-NAME)
	 (OR (FLAVOR-DEPENDS-ON-ALL FL) (COMPOSE-FLAVOR-COMBINATION FL))
	 (OR (FLAVOR-METHOD-HASH-TABLE FL)
	     (COMPOSE-METHOD-COMBINATION FL))))
  FLAVOR-NAME)

;Returns T if all components of this flavor are defined
(DEFUN FLAVOR-COMPONENTS-DEFINED-P (FLAVOR-NAME &OPTIONAL COMPLAINT &AUX FL)
  (COND ((SETQ FL (GET-FLAVOR FLAVOR-NAME))
	 (OR (NOT (NULL (FLAVOR-DEPENDS-ON-ALL FL)))	;Already composed, be fast
	     (AND (DO ((L (FLAVOR-DEPENDS-ON FL) (CDR L))) ((NULL L) T)
		    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL)))
		  (DO ((L (FLAVOR-INCLUDES FL) (CDR L))) ((NULL L) T)
		    (OR (FLAVOR-COMPONENTS-DEFINED-P (CAR L)) (RETURN NIL))))))
	(COMPLAINT (FORMAT ERRPORT "~&~A - ~S undefined flavor" COMPLAINT FLAVOR-NAME)
		   NIL)
	(T NIL)))

(EVAL-WHEN (EVAL LOAD) (LOAD 'VANILLA))

;; Local Modes:
;; Mode: Lisp
;; Case Search: 1
;; End:
