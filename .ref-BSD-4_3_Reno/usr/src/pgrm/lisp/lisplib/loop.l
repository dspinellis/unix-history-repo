(setq rcs-loop-
   "$Header: /usr/lib/lisp/loop.l,v 1.1 83/01/29 18:38:49 jkf Exp $")

;;;   LOOP  -*- Mode:LISP; Package:System-Internals; Base:8; Lowercase:T -*-
;;;   **********************************************************************
;;;   ****** Universal ******** LOOP Iteration Macro ***********************
;;;   **********************************************************************
;;;   **** (C) COPYRIGHT 1980, 1981 MASSACHUSETTS INSTITUTE OF TECHNOLOGY **
;;;   ******** THIS IS A READ-ONLY FILE! (ALL WRITES RESERVED) *************
;;;   **********************************************************************

;;;; LOOP Iteration Macro

;The master copy of this file is on ML:LSB1;LOOP >
;The current Lisp machine copy is on AI:LISPM2;LOOP >
;The FASL and QFASL should also be accessible from LIBLSP; on all machines.
;(Is this necessary anymore? LOOP is now in the Lisp Machine system and
; is accessible on LISP; and distributed with PDP10 Maclisp.)
;Duplicate source is usually also maintained on MC:LSB1;LOOP >
;Printed documentation is available as MIT-LCS Technical Memo 169,
; "LOOP Iteration Macro", from:
;	Publications
;	MIT Laboratory for Computer Science
;	545 Technology Square
;	Cambridge, MA 02139
; the text of which appears in only slightly modified form in the Lisp
; Machine manual.

; Bugs/complaints/suggestions/solicitations-for-documentation to BUG-LOOP
; at any ITS site (MIT-ML preferred).


; **********************************************************************
; *************************** NOTE WELL ********************************
; **********************************************************************
;Incremental compiling of things in this file will generate wrong code
; unless you first evaluate the 'feature' stuff on the next page
; ("readtime environment setup").  (This mainly of Lispm interest.)
;This source sincerely believes that it can run compatibly, WITHOUT ANY
; TEXTUAL MODIFICATIONS AT ALL, in PDP-10 Maclisp, Multics Maclisp, Lisp
; Machine Lisp (Zetalisp), VAX NIL, and Franz Lisp.  PLEASE do not make
; changes to this file (the master copy) if you are in any way unsure
; of the implications in a dialect you are not very familiar with;  let
; a LOOP maintainer take the responsibility for breaking the master copy
; and maintaining some semblance of sanity among the disparities.  Note
; in particular that LOOP also runs in the PDP10 Maclisp -> Vax NIL
; cross-compiler;  that environment requires LOOP to produce code which
; can at the same time be interpreted in Maclisp, and compiled for NIL.


; Bootstrap up our basic primitive environment.
; This includes backquote, sharpsign, defmacro, let.

(eval-when (eval compile)
  (cond ((status feature Multics)
	   (defun include-for-multics macro (x)
	     (cons '%include (cdr x))))
	('t #-Franz (macro include-for-multics (x) ())
	    #+Franz (defmacro include-for-multics (x) nil))))

(include-for-multics lisp_prelude)
(include-for-multics lisp_dcls)

#+Franz (environment-maclisp)


;;;; Readtime Environment Setup

;Now set up the readtime conditionalization environment.   This won't work
; in any compiler that reads the whole file before compiling anything.
; It is a good idea to pretend that case matters in ALL contexts.
; This is in fact true in Franz at the present.  Case matters to Multics
; in symbols, except for <frob> in (status feature <frob>).
(eval-when (eval compile)
  #+NIL (progn
	   (defmacro loop-featurep (f)
	     `(featurep ',f target-features))
	   (defmacro loop-nofeaturep (f)
	     `(nofeaturep ',f target-features))
	   (defmacro loop-set-feature (f)
	     `(set-feature ',f target-features))
	   (defmacro loop-set-nofeature (f)
	     `(set-nofeature ',f target-features))
	   )
  #-NIL (progn
	   (defmacro loop-featurep (f)
	     `(status feature ,f))
	   (defmacro loop-nofeaturep (f)
	     ; Multics doesn't have (status nofeature)...
	     `(not (status feature ,f)))
	   (defmacro loop-set-feature (f)
	     `(sstatus feature ,f))
	   (defmacro loop-set-nofeature (f)
	     ; Does this work on Multics???  I think not but we don't use.
	     `(sstatus nofeature ,f))
	   )
  ;Note:  NEVER in this file is "PDP-10" a valid feature or substring of
  ; a feature.  It is NEVER hyphenated.  Keep it that way.  (This because
  ; of continuous lossage with not setting up one or the other of the
  ; hyphenated/non-hyphenated one.)
  (cond ((and (loop-featurep PDP10)
	      (loop-featurep NILAID))
	   ;Compiling a PDP10 -> NIL cross-compiling LOOP.
	   ; We check the PDP10 feature first sort of gratuitously so that
	   ; other implementations don't think we are asking about an undefined
	   ; feature name.  (Vax-NIL specifically.)
	   (loop-set-feature For-NIL)
	   (loop-set-nofeature For-Maclisp)
	   (loop-set-nofeature For-PDP10)
	   (loop-set-feature Run-in-Maclisp)
	   (loop-set-feature Run-on-PDP10)
	   (loop-set-nofeature Franz))
	((and (loop-featurep Maclisp) (loop-nofeaturep For-NIL))
	   ; Standard in-Maclisp for-Maclisp.
	   (loop-set-feature For-Maclisp)
	   (loop-set-feature Run-In-Maclisp)
	   (cond ((loop-nofeaturep Multics)
		    (loop-set-feature For-PDP10)
		    (loop-set-feature PDP10)
		    (loop-set-feature Run-on-PDP10))))
	((loop-featurep NIL)
	   ; Real NIL
	   (loop-set-nofeature PDP10)
	   (loop-set-nofeature Multics)
	   (loop-set-nofeature Run-on-PDP10)
	   (loop-set-nofeature For-PDP10)
	   (loop-set-nofeature Run-In-Maclisp)
	   (loop-set-nofeature For-Maclisp))
	((loop-featurep Lispm))
	((loop-featurep franz)
	   ;The "natural" case of features in franz is all lower.
	   ; Since that is unlike the others used in here, we synonymize
	   ; the obvious other choice.
	   (loop-set-feature Franz))
	('t (break loop-implementation-unknown)))
  (cond ((or (loop-featurep Lispm) (loop-featurep For-PDP10))
	   (loop-set-feature Hairy-Collection))
	('t (loop-set-nofeature Hairy-Collection)))
  (cond ((or (loop-featurep For-NIL) (loop-featurep For-PDP10))
	   (loop-set-feature System-Destructuring))
	('t (loop-set-nofeature System-Destructuring)))
  (cond ((or (loop-featurep For-NIL) (loop-featurep Lispm))
	   (loop-set-feature Named-PROGs))
	('t (loop-set-nofeature Named-PROGs)))
  ;In the following two features, "Local" means the Lisp LOOP will be
  ; running in, not the one it is being compiled in.  "Targeted" means
  ; the Lisp it will be producing code for.  (All from the point of view
  ; of the running LOOP, you see.)
  (cond ((or (loop-featurep For-NIL) (loop-featurep Lispm))
	   (loop-set-feature Targeted-Lisp-has-Packages))
	('t (loop-set-nofeature Targeted-Lisp-has-Packages)))
  (cond ((or (loop-featurep Franz) (loop-featurep Run-in-Maclisp))
	   (loop-set-nofeature Local-Lisp-has-Packages))
	('t (loop-set-feature Local-Lisp-has-Packages)))
  (cond ((loop-featurep For-NIL) (loop-set-feature Vector-Destructuring))
	('t (loop-set-nofeature Vector-Destructuring)))
  ;Meaningful-Type-Declarations means that the declarations are (1)
  ; implemented by the compiler and (2) used for something.
  ; Assume minimally maclisp-like FIXNUM and FLONUM dcls, for local
  ; variables or function results.
  (cond ((loop-featurep Run-in-Maclisp)
	   (loop-set-feature Meaningful-Type-Declarations))
	('t (loop-set-nofeature Meaningful-Type-Declarations)))
  ;Hair for 3600 cross-compilation?
  (cond ((and (loop-featurep Lispm) (not (loop-featurep 3600.)))
	   (loop-set-feature Loop-Small-Floatp))
	('t (loop-set-nofeature Loop-Small-Floatp)))
  ; -> insert more conditionals here <-
  ())

#+Franz
(eval-when (eval compile)
  (setsyntax #// 143.) ; Make slash be slash
  (setsyntax #/\ 2.) ; make backslash alphabetic
  )


#+Run-on-PDP10
(eval-when (compile)
  ;Note this hack used when compiled only.
  ;Its purpose in life is to save a bit of space in the load-time environment,
  ; since loop doesn't actually need the PDP10 Maclisp doublequoted crocks
  ; to remember their origin as "strings".
  (setsyntax #/" 'macro
	     '(lambda ()
		(do ((ch (tyi) (tyi)) (l () (cons ch l)))
		    ((= ch #/")
		     (list squid (list 'quote (implode (nreverse l)))))
		  (and (= ch #//) (setq ch (tyi)))))))


;;;; Other basic header stuff


; Following isn't needed on Lispm, as loop is installed there (ie, these
; symbols are already in GLOBAL).
#+(and Targeted-Lisp-has-Packages (not Lispm))
(mapc 'globalize
      '("LOOP"					; Major macro
	"LOOP-FINISH"				; Handy macro
	"DEFINE-LOOP-MACRO"
	"DEFINE-LOOP-PATH"			; for users to define paths
	"DEFINE-LOOP-SEQUENCE-PATH"		; this too
	))

#+(or For-NIL For-PDP10)
(herald LOOP)


;;;; Macro Environment Setup

;Wrapper for putting around DEFMACRO etc. forms to determine whether
; they are defined in the compiled output file or not.  (It is assumed
; that DEFMACRO forms will be.)  Making loop-macro-progn output for loading
; is convenient if loop will have incremental-recompilation done on it.
; (Note, of course, that the readtime environment is NOT set up.)

#+Lispm
(defmacro loop-macro-progn (&rest forms)
    `(progn 'compile ,@forms))
#-Lispm
(eval-when (eval compile)
    (defmacro loop-macro-progn (&rest forms)
	`(eval-when (eval compile) ,@forms)))


; Hack up the stuff for data-types.  DATA-TYPE? will always be a macro
; so that it will not require the data-type package at run time if
; all uses of the other routines are conditionalized upon that value.
(eval-when (eval compile)
  ; Crock for DATA-TYPE? derives from DTDCL.  We just copy it rather
  ; than load it in, which requires knowing where it comes from (sigh).
  ; 
  #-Local-Lisp-has-Packages
    (defmacro data-type? (x) `(get ,x ':data-type))
  #+Local-Lisp-has-Packages
    (defmacro data-type? (frob)
      (let ((foo (gensym)))
	`((lambda (,foo)
	    ; NIL croaks if () given to GET...
	    (and #+NIL (symbolp ,foo) #-NIL 't
		 (or (get ,foo ':data-type)
		     (and (setq ,foo (intern-soft (get-pname ,foo) ""))
			  (get ,foo ':data-type)))))
	  ,frob))))

(declare (*lexpr variable-declarations)
	 ; Multics defaults to free-functional-variable since it is declared
	 ; special & used as function before it is defined:
	 (*expr loop-when-it-variable)
	 (*expr initial-value primitive-type)
       #+(or Maclisp Franz) (macros t) ; Defmacro dependency
       #+Run-in-Maclisp
	 (muzzled t)	; I know what i'm doing
	 )

#+Run-on-PDP10
(declare (mapex ())
	 (genprefix loop/|-)
	 (special squid)
       #+(and Run-in-Maclisp For-NIL) ; patch it up
         (*expr stringp vectorp vref vector-length)
         )

#-Run-on-PDP10
(declare
  #+Lispm (setq open-code-map-switch t)
  #+Run-in-Maclisp (mapex t)
  #+Run-in-Maclisp (genprefix loop-iteration/|-))

#+Run-on-PDP10
(mapc '(lambda (x)
	   (or (getl x '(subr lsubr fsubr macro fexpr expr autoload))
	       ; This dtdcl will sort of work for NIL code generation,
	       ; if declarations will ignored.
	       (putprop x '((lisp) dtdcl fasl) 'autoload)))
      '(data-type? variable-declarations initial-value primitive-type))

(loop-macro-progn
 (defmacro loop-copylist* (l)
    #+Lispm `(copylist* ,l)
    #-Lispm `(append ,l ())))


;;;; Random Macros

; Error macro.  Note that in the PDP10 version we call LOOP-DIE rather
; than ERROR -- there are so many occurences of it in this source that
; it is worth breaking off that function, since calling the lsubr ERROR
; takes more inline code.
(loop-macro-progn
 (defmacro loop-simple-error (unquoted-message &optional (datum () datump))
    #+(and Run-In-Maclisp (not Multics))
      (progn (cond ((symbolp unquoted-message))
		   ((and (not (atom unquoted-message))
			 compiler-state
			 (eq (car unquoted-message) squid)
			 (not (atom (setq unquoted-message
					  (cadr unquoted-message))))
			 (eq (car unquoted-message) 'quote)
			 (symbolp (cadr unquoted-message)))
		      (setq unquoted-message (cadr unquoted-message)))
		   ('t (error '|Uloze -- LOOP-SIMPLE-ERROR|
			      (list 'loop-simple-error
				    unquoted-message datum))))
	     (cond (datump `(loop-die ',unquoted-message ,datum))
		   ('t `(error ',unquoted-message))))
    #+(or Franz Multics)
      (progn (or (memq (typep unquoted-message) '(string symbol))
		 (error '|Uloze -- | (list 'loop-simple-error
					   unquoted-message datum)))
	     `(error ,(let ((l (list "lisp:  " unquoted-message
				     (if datump " -- " ""))))
			#+Franz (get_pname (apply 'uconcat l))
			#-Franz (apply 'catenate l))
		     . ,(and datump (list datum))))
    #-(or Run-In-Maclisp Franz)
      `(ferror () ,(if datump (string-append "~S " unquoted-message)
		       unquoted-message)
	       . ,(and datump (list datum)))))


#+(and Run-in-Maclisp (not Multics))
(defun loop-die (arg1 arg2)
    (error arg1 arg2))


; This is a KLUDGE.  But it apparently saves an average of two inline
; instructions per call in the PDP10 version...  The ACS prop is
; fairly gratuitous.

#+Run-on-PDP10
(progn 'compile
   (lap-a-list 
     '((lap loop-pop-source subr)
       (args loop-pop-source (() . 0))
	   (hlrz a @ (special loop-source-code))
	   (hrrz b @ (special loop-source-code))
	   (movem b (special loop-source-code))
	   (popj p)
       nil))
   (eval-when (compile)
       (defprop loop-pop-source 2 acs)
       ))

#-Run-on-PDP10
(loop-macro-progn
 (defmacro loop-pop-source () '(pop loop-source-code)))

(loop-macro-progn
 (defmacro object-that-cares-p (x)
   #+Lispm `(listp ,x)
   #+(or NIL PDP10) `(pairp ,x)
   #-(or Lispm NIL PDP10) `(eq (typep ,x) 'list)))


;;;; Variable defining macros

;There is some confusion among lisps as to whether or not a file containing
; a DEFVAR will declare the variable when the compiled file is loaded
; into a compiler.  LOOP assumes that DEFVAR does so (this is needed for
; various user-accessible variables).  DEFIVAR is for "private" variables.
; Note that this is moot for Lispm due to incremental-recompilation support
; anyway.
;Multics lcp has some bug whereby DECLARE and (EVAL-WHEN (COMPILE) ...)
; don't get hacked properly inside of more than one level of
; (PROGN 'COMPILE ...).  Thus we hack around DEFVAR and DEFIVAR to bypass
; this lossage.
;Franz DEFVAR does not make the declaration on loading, so we redefine it.

#+(or Multics Franz)
(loop-macro-progn
 (defmacro defvar (name &optional (init nil initp) documentation
		   &aux (dclform `(and #+Franz (getd 'special)
				       #-Franz (status feature compiler)
				       (special ,name))))
    ; For some obscure reason, (DECLARE ...) doesn't take effect within 2
    ; (PROGN 'COMPILE ...)s, but (EVAL-WHEN (COMPILE) ...) does, on Multics.
    (eval dclform) ; sigh
    (cond ((not initp) dclform)
	  (t `(progn 'compile
		     ,dclform
		     (or (boundp ',name) (setq ,name ,init)))))))

(loop-macro-progn
 ; A DEFVAR alternative - "DEFine Internal VARiable".
 (defmacro defivar (name &optional (init () initp))
    ; The Lispm choice here is based on likelihood of incremental compilation.
    #+Lispm `(defvar ,name ,@(and initp `(,init)))
    #+Multics (progn (apply 'special (list name))
		     (if initp `(or (boundp ',name) (setq ,name ,init))
			 `(progn 'compile)))
    #-(or Lispm Multics)
      `(progn 'compile
	      (declare (special ,name))
	      . ,(and initp `((or (boundp ',name) (setq ,name ,init)))))))

#+Franz
;Defconst is like defvar but always initializes.
; It happens in this case that we really don't care about the global
; declaration on loading, so actually treat it more like DEFIVAR.
; (This is now in Multics and PDP10 Maclisp, thanks to Maclisp Extensions
; Manual.)
(loop-macro-progn
  (defmacro defconst (name init &optional documentation)
    `(progn 'compile (declare (special ,name)) (setq ,name ,init))))



;;;; Setq Hackery

; Note:  LOOP-MAKE-PSETQ is NOT flushable depending on the existence
; of PSETQ, unless PSETQ handles destructuring.  Even then it is
; preferable for the code LOOP produces to not contain intermediate
; macros, especially in the PDP10 version.

(defun loop-make-psetq (frobs)
    (and frobs
	 (loop-make-setq
	    (list (car frobs)
		  (if (null (cddr frobs)) (cadr frobs)
		      `(prog1 ,(cadr frobs)
			      ,(loop-make-psetq (cddr frobs))))))))

#-System-Destructuring
(progn 'compile

(defvar si:loop-use-system-destructuring?
    ())

(defivar loop-desetq-temporary)

; Do we want this???  It is, admittedly, useful...
;(defmacro loop-desetq (&rest x)
;  (let ((loop-desetq-temporary ()))
;     (let ((setq-form (loop-make-desetq x)))
;	(if loop-desetq-temporary
;	    `((lambda (,loop-desetq-temporary) ,setq-form) ())
;	    setq-form))))


(defun loop-make-desetq (x)
   (if si:loop-use-system-destructuring?
       (cons (do ((l x (cddr l))) ((null l) 'setq)
	       (or (and (not (null (car l))) (symbolp (car l)))
		   (return 'desetq)))
	     x)
       (do ((x x (cddr x)) (r ()) (var) (val))
	   ((null x) (and r (cons 'setq r)))
	 (setq var (car x) val (cadr x))
	 (cond ((and (not (atom var))
		     (not (atom val))
		     (not (and (memq (car val)
				     '(car cdr cadr cddr caar cdar))
			       (atom (cadr val)))))
		  (setq x (list* (or loop-desetq-temporary
				     (setq loop-desetq-temporary (gensym)))
				 val var loop-desetq-temporary (cddr x)))))
	 (setq r (nconc r (loop-desetq-internal (car x) (cadr x)))))))

(defun loop-desetq-internal (var val)
  (cond ((null var) ())
	((atom var) (list var val))
	('t (nconc (loop-desetq-internal (car var) `(car ,val))
		   (loop-desetq-internal (cdr var) `(cdr ,val))))))
); End desetq hackery for #-System-Destructuring


(defun loop-make-setq (pairs)
    (and pairs
	 #-System-Destructuring
	   (loop-make-desetq pairs)
	 #+System-Destructuring
	   (cons (do ((l pairs (cddr l))) ((null l) 'setq)
		   (or (and (car l) (symbolp (car l))) (return 'desetq)))
		 pairs)))


(defconst loop-keyword-alist			;clause introducers
     '(
      #+Named-PROGs
	(named loop-do-named)
	(initially loop-do-initially)
	(finally loop-do-finally)
	(nodeclare loop-nodeclare)
	(do loop-do-do)
	(doing loop-do-do)
	(return loop-do-return)
	(collect loop-do-collect list)
	(collecting loop-do-collect list)
	(append loop-do-collect append)
	(appending loop-do-collect append)
	(nconc loop-do-collect nconc)
	(nconcing loop-do-collect nconc)
	(count loop-do-collect count)
	(counting loop-do-collect count)
	(sum loop-do-collect sum)
	(summing loop-do-collect sum)
	(maximize loop-do-collect max)
	(minimize loop-do-collect min)
	(always loop-do-always or)
	(never loop-do-always and)
	(thereis loop-do-thereis)
	(while loop-do-while or while)
	(until loop-do-while and until)
	(when loop-do-when ())
	(if loop-do-when ())
 	(unless loop-do-when t)
	(with loop-do-with)))


(defconst loop-iteration-keyword-alist
    `((for loop-do-for)
      (as loop-do-for)
      (repeat loop-do-repeat)))


(defconst loop-for-keyword-alist			;Types of FOR
     '( (= loop-for-equals)
        (first loop-for-first)
	(in loop-list-stepper car)
	(on loop-list-stepper ())
	(from loop-for-arithmetic from)
	(downfrom loop-for-arithmetic downfrom)
	(upfrom loop-for-arithmetic upfrom)
	(below loop-for-arithmetic below)
	(to loop-for-arithmetic to)
	(being loop-for-being)))

#+Named-PROGs
(defivar loop-prog-names)

(defvar loop-path-keyword-alist ())		; PATH functions
(defivar loop-named-variables)			; see SI:LOOP-NAMED-VARIABLE
(defivar loop-collection-crocks)		; see LOOP-DO-COLLECT etc
(defivar loop-variables)			;Variables local to the loop
(defivar loop-declarations)			; Local dcls for above
(defivar loop-nodeclare)			; but don't declare these
(defivar loop-variable-stack)
(defivar loop-declaration-stack)
#-System-Destructuring
(defivar loop-desetq-crocks)			; see loop-make-variable
#-System-Destructuring
(defivar loop-desetq-stack)			; and loop-translate-1
(defivar loop-prologue)				;List of forms in reverse order
(defivar loop-before-loop)
(defivar loop-body)				;..
(defivar loop-after-body)			;.. for FOR steppers
(defivar loop-epilogue)				;..
(defivar loop-after-epilogue)			;So COLLECT's RETURN comes after FINALLY
(defivar loop-conditionals)			;If non-NIL, condition for next form in body
  ;The above is actually a list of entries of the form
  ;(cond (condition forms...))
  ;When it is output, each successive condition will get
  ;nested inside the previous one, but it is not built up
  ;that way because you wouldn't be able to tell a WHEN-generated
  ;COND from a user-generated COND.
  ;When ELSE is used, each cond can get a second clause

(defivar loop-when-it-variable)			;See LOOP-DO-WHEN
(defivar loop-never-stepped-variable)		; see LOOP-FOR-FIRST
(defivar loop-emitted-body?)			; see LOOP-EMIT-BODY,
						; and LOOP-DO-FOR
(defivar loop-iteration-variables)		; LOOP-MAKE-ITERATION-VARIABLE
(defivar loop-iteration-variablep)		; ditto
(defivar loop-collect-cruft)			; for multiple COLLECTs (etc)
(defivar loop-source-code)
(defvar loop-duplicate-code ())  ; see LOOP-OPTIMIZE-DUPLICATED-CODE-ETC


;;;; Token Hackery

;Compare two "tokens".  The first is the frob out of LOOP-SOURCE-CODE,
;the second a symbol to check against.

; Consider having case-independent comparison on Multics.
#+(or Multics Franz)
(progn 'compile
    (defmacro si:loop-tequal (x1 x2)
	`(eq ,x1 ,x2))
    (defmacro si:loop-tmember (x l)
	`(memq ,x ,l))
    (defmacro si:loop-tassoc (x l)
	`(assq ,x ,l)))


#+Lispm
(progn 'compile
   (defun si:loop-tequal (x1 x2)
	(and (symbolp x1) (string-equal x1 x2)))
   (defun si:loop-tassoc (kwd alist)
	(and (symbolp kwd) (ass #'string-equal kwd alist)))
   (defun si:loop-tmember (kwd list)
	(and (symbolp kwd) (mem #'string-equal kwd list))))


#+Run-on-PDP10
(progn 'compile
   #+For-NIL
     (defun si:loop-tequal (x1 x2)
	 (eq x1 x2))
   #-For-NIL
     (progn 'compile
	(eval-when (load compile)
	   (cond ((status feature complr)
		    ; Gross me out!
		    (setq macrolist
			  (cons '(si:loop-tequal
				    . (lambda (x) (cons 'eq (cdr x))))
				(delq (assq 'si:loop-tequal macrolist)
				      macrolist)))
		    (*expr si:loop-tmember si:loop-tassoc))))
	(defun si:loop-tequal (x1 x2)
	   (eq x1 x2)))
     (defun si:loop-tmember (kwd list)
	 (memq kwd list))
     (defun si:loop-tassoc (kwd alist)
	 (assq kwd alist))
     )

#+(and For-NIL (not Run-in-Maclisp))
(progn 'compile
  ; STRING-EQUAL only accepts strings.  GET-PNAME can be open-coded
  ; however.
  (defun si:loop-tequal (kwd1 kwd2)
      (and (symbolp kwd1) (string-equal (get-pname kwd1) (get-pname kwd2))))
  (defun si:loop-tassoc (kwd alist)
    (cond ((symbolp kwd)
	     (setq kwd (get-pname kwd))
	     (do ((l alist (cdr l))) ((null l) ())
	       (and (string-equal kwd (get-pname (caar l)))
		    (return (car l)))))))
  (defun si:loop-tmember (token list)
     (cond ((symbolp token)
	      (setq token (get-pname token))
	      (do ((l list (cdr l))) ((null l))
		(and (string-equal token (get-pname (car l)))
		     (return l)))))))


#+(or For-PDP10 For-NIL)
(eval-when (eval compile) (setq defmacro-displace-call ()))

(defmacro define-loop-macro (keyword)
    (or (eq keyword 'loop)
	(si:loop-tassoc keyword loop-keyword-alist)
	(si:loop-tassoc keyword loop-iteration-keyword-alist)
	(loop-simple-error "not a loop keyword - define-loop-macro" keyword))
    (subst keyword 'keyword
	   '(eval-when (compile load eval)
	      #+(or For-NIL Run-on-PDP10)
	        (progn (flush-macromemos 'keyword ())
		       (flush-macromemos 'loop ()))
	      #-Run-in-Maclisp
	        (progn
		  #+Franz
		    (putd 'keyword
			  '(macro (macroarg) (loop-translate macroarg)))
		  #-Franz
		    (fset-carefully 'keyword '(macro . loop-translate)))
	      #+Run-in-Maclisp
	        (progn (defprop keyword loop-translate macro))
	      )))

#+(or For-PDP10 For-NIL)
(eval-when (eval compile) (setq defmacro-displace-call 't))

(define-loop-macro loop)

#+Run-in-Maclisp
(defun (loop-finish macro) (form)
    ;This definition solves two problems:
    ; (1) wasted address space
    ; (2) displacing of a form which might tend to be pure.
    ; There is little point in macro-memoizing a constant anyway.
    (and (cdr form) (loop-simple-error "Wrong number of args" form))
    '(go end-loop))

#-Run-in-Maclisp
(defmacro loop-finish () 
    '(go end-loop))


(defun loop-translate (x)
    #-(or For-NIL Run-on-PDP10) (displace x (loop-translate-1 x))
    #+(or For-NIL Run-on-PDP10)
      (or (macrofetch x) (macromemo x (loop-translate-1 x) 'loop)))


(defun loop-end-testify (list-of-forms)
    (if (null list-of-forms) ()
	`(and ,(if (null (cdr (setq list-of-forms (nreverse list-of-forms))))
		   (car list-of-forms)
		   (cons 'or list-of-forms))
	      (go end-loop))))

(defun loop-optimize-duplicated-code-etc (&aux before after groupa groupb a b
					       lastdiff)
    (do ((l1 (nreverse loop-before-loop) (cdr l1))
	 (l2 (nreverse loop-after-body) (cdr l2)))
	((equal l1 l2)
	   (setq loop-body (nconc (delq '() l1) (nreverse loop-body))))
      (push (car l1) before) (push (car l2) after))
    (cond ((not (null loop-duplicate-code))
	     (setq loop-before-loop (nreverse (delq () before))
		   loop-after-body (nreverse (delq () after))))
	  ('t (setq loop-before-loop () loop-after-body ()
		    before (nreverse before) after (nreverse after))
	      (do ((bb before (cdr bb)) (aa after (cdr aa)))
		  ((null aa))
		(cond ((not (equal (car aa) (car bb))) (setq lastdiff aa))
		      ((not (si:loop-simplep (car aa)))	;Mustn't duplicate
		       (return ()))))
	      (cond (lastdiff  ;Down through lastdiff should be duplicated
		     (do () (())
		       (and (car before) (push (car before) loop-before-loop))
		       (and (car after) (push (car after) loop-after-body))
		       (setq before (cdr before) after (cdr after))
		       (and (eq after (cdr lastdiff)) (return ())))
		     (setq loop-before-loop (nreverse loop-before-loop)
			   loop-after-body (nreverse loop-after-body))))
	      (do ((bb (nreverse before) (cdr bb))
		   (aa (nreverse after) (cdr aa)))
		  ((null aa))
		(setq a (car aa) b (car bb))
		(cond ((and (null a) (null b)))
		      ((equal a b)
		         (loop-output-group groupb groupa)
			 (push a loop-body)
			 (setq groupb () groupa ()))
		      ('t (and a (push a groupa)) (and b (push b groupb)))))
	      (loop-output-group groupb groupa)))
    (and loop-never-stepped-variable
	 (push `(setq ,loop-never-stepped-variable ()) loop-after-body))
    ())


(defun loop-output-group (before after)
    (and (or after before)
	 (let ((v (or loop-never-stepped-variable
		      (setq loop-never-stepped-variable
			    (loop-make-variable (gensym) ''t ())))))
	    (push (cond ((not before) `(or ,v (progn . ,after)))
			((not after) `(and ,v (progn . ,before)))
			('t `(cond (,v . ,before) ('t . ,after))))
		  loop-body))))


(defun loop-translate-1 (loop-source-code)
  (and (eq (car loop-source-code) 'loop)
       (setq loop-source-code (cdr loop-source-code)))
  (do ((loop-iteration-variables ())
       (loop-iteration-variablep ())
       (loop-variables ())
       (loop-nodeclare ())
       (loop-named-variables ())
       (loop-declarations ())
     #-System-Destructuring
       (loop-desetq-crocks ())
       (loop-variable-stack ())
       (loop-declaration-stack ())
     #-System-destructuring
       (loop-desetq-stack ())
       (loop-prologue ())
       (loop-before-loop ())
       (loop-body ())
       (loop-emitted-body? ())
       (loop-after-body ())
       (loop-epilogue ())
       (loop-after-epilogue ())
       (loop-conditionals ())
       (loop-when-it-variable ())
       (loop-never-stepped-variable ())
     #-System-Destructuring
       (loop-desetq-temporary ())
     #+Named-PROGs
       (loop-prog-names ())
       (loop-collect-cruft ())
       (loop-collection-crocks ())
       (keyword)
       (tem)
       (progvars))
      ((null loop-source-code)
       (and loop-conditionals
	    (loop-simple-error "Hanging conditional in loop macro"
			       (caadar loop-conditionals)))
       (loop-optimize-duplicated-code-etc)
       (loop-bind-block)
       (setq progvars loop-collection-crocks)
     #-System-Destructuring
       (and loop-desetq-temporary (push loop-desetq-temporary progvars))
       (setq tem `(prog #+Named-PROGs ,.loop-prog-names
			,progvars
		      #+Hairy-Collection
		        ,.(do ((l loop-collection-crocks (cddr l))
			       (v () (cons `(loop-collect-init
					        ,(cadr l) ,(car l))
					    v)))
			      ((null l) v))
		      ,.(nreverse loop-prologue)
		      ,.loop-before-loop
		   next-loop
		      ,.loop-body
		      ,.loop-after-body
		      (go next-loop)
		      ; Multics complr notices when end-loop is not gone
		      ; to.  So we put in a dummy go.  This does not generate
		      ; extra code, at least in the simple example i tried,
		      ; but it does keep it from complaining about unused
		      ; go tag.
	    #+Multics (go end-loop)
		   end-loop
		      ,.(nreverse loop-epilogue)
		      ,.(nreverse loop-after-epilogue)))
       (do ((vars) (dcls) #-System-Destructuring (crocks))
	   ((null loop-variable-stack))
	 (setq vars (car loop-variable-stack)
	       loop-variable-stack (cdr loop-variable-stack)
	       dcls (car loop-declaration-stack)
	       loop-declaration-stack (cdr loop-declaration-stack)
	       tem (ncons tem))
	 #-System-Destructuring
	   (and (setq crocks (pop loop-desetq-stack))
		(push (loop-make-desetq crocks) tem))
	 (and dcls (push (cons 'declare dcls) tem))
	 (cond ((do ((l vars (cdr l))) ((null l) ())
		  (and (not (atom (car l)))
		       (or (null (caar l)) (not (symbolp (caar l))))
		       (return 't)))
		  (setq tem `(let ,(nreverse vars) ,.tem)))
	       ('t (let ((lambda-vars ()) (lambda-vals ()))
		     (do ((l vars (cdr l)) (v)) ((null l))
		       (cond ((atom (setq v (car l)))
				(push v lambda-vars)
				(push () lambda-vals))
			     ('t (push (car v) lambda-vars)
				 (push (cadr v) lambda-vals))))
		     (setq tem `((lambda ,lambda-vars ,.tem)
				 ,.lambda-vals))))))
       tem)
    (if (symbolp (setq keyword (loop-pop-source)))
	(if (setq tem (si:loop-tassoc keyword loop-keyword-alist))
	    (apply (cadr tem) (cddr tem))
	    (if (setq tem (si:loop-tassoc
			     keyword loop-iteration-keyword-alist))
		(loop-hack-iteration tem)
		(if (si:loop-tmember keyword '(and else))
		    ; Alternative is to ignore it, ie let it go around to the
		    ; next keyword...
		    (loop-simple-error
		       "secondary clause misplaced at top level in LOOP macro"
		       (list keyword (car loop-source-code)
			     (cadr loop-source-code)))
		    (loop-simple-error
		       "unknown keyword in LOOP macro" keyword))))
	(loop-simple-error
	   "found where keyword expected in LOOP macro" keyword))))


(defun loop-bind-block ()
   (cond ((not (null loop-variables))
	    (push loop-variables loop-variable-stack)
	    (push loop-declarations loop-declaration-stack)
	    (setq loop-variables () loop-declarations ())
	    #-System-Destructuring
	      (progn (push loop-desetq-crocks loop-desetq-stack)
		     (setq loop-desetq-crocks ())))))


;Get FORM argument to a keyword.  Read up to atom.  PROGNify if necessary.
(defun loop-get-form ()
  (do ((forms (ncons (loop-pop-source)) (cons (loop-pop-source) forms))
       (nextform (car loop-source-code) (car loop-source-code)))
      ((atom nextform)
       (if (null (cdr forms)) (car forms)
	   (cons 'progn (nreverse forms))))))


;Note that this function is not absolutely general.  For instance, in Maclisp,
; the functions < and > can only take 2 args, whereas greaterp and lessp
; may take any number.  Also, certain of the generic functions behave
; differently from the type-specific ones in "degenerate" cases, like
; QUOTIENT or DIFFERENCE of one arg.
;And of course one always must be careful doing textual substitution.
(defun loop-typed-arith (substitutable-expression data-type)
  #-(or Lispm Franz)
    (if (setq data-type (car (si:loop-tmember (if (data-type? data-type)
						  (primitive-type data-type)
						  data-type)
					      '(fixnum flonum))))
	(sublis (cond ((eq data-type 'fixnum)
		         #+For-NIL
			   '((plus . +&) (add1 . 1+&)
			     (difference . -&) (sub1 . 1-&)
			     (quotient . //&) (remainder . \&) (times . *&)
			     (zerop . 0p) (plusp . +p) (minusp . -p)
			     (greaterp . >&) (lessp . <&)
			     (min . min&) (max . max&))
			 #-For-NIL
			   '((plus . +) (add1 . 1+)
			     (difference . -) (sub1 . 1-)
			     (quotient . //) (remainder . \) (times . *)
			     (greaterp . >) (lessp . <)))
		      ('t #+For-NIL
			    '((plus . +$) (difference . -$)
			      (add1 . 1+$) (sub1 . 1-$)
			      (quotient . //$) (times . *$)
			      (greaterp . >$) (lessp . <$)
			      (max . max$) (min . min$))
			  #-For-NIL
			    '((plus . +$) (difference . -$)
			      (add1 . 1+$) (sub1 . 1-$)
			      (quotient . //$) (times . *$)
			      (greaterp . >) (lessp . <))))
		substitutable-expression)
	substitutable-expression)
  #+Lispm
    (progn data-type substitutable-expression)
  #+Franz
    (if (si:loop-tequal data-type 'fixnum)
	(sublis '((add1 . 1+) (sub1 . 1-) (plus . +) (difference . -)
		  (times . *) (quotient . //) (remainder . \))
		substitutable-expression)
	substitutable-expression)
  )


(defun loop-typed-init (data-type)
    (cond ((data-type? data-type) (initial-value data-type))
	  ((setq data-type (car (si:loop-tmember
				   data-type '(fixnum flonum integer number
					       #+Loop-Small-Floatp
					         small-flonum))))
	     (cond ((eq data-type 'flonum) 0.0)
		 #+Loop-Small-Floatp
		   ((eq data-type 'small-flonum)
		      #.(and (loop-featurep Loop-Small-Floatp)
			     (small-float 0)))
		   ('t 0)))))


(defun loop-make-variable (name initialization dtype)
  (cond ((null name)
	   (cond ((not (null initialization))
		    (push (list #+Lispm 'ignore
				#+Multics (setq name (gensym))
				#-(or Lispm Multics) ()
				initialization)
			  loop-variables)
		    #+Multics (push `(progn ,name) loop-prologue))))
	(#-Vector-Destructuring (atom name)
	 #+Vector-Destructuring (symbolp name)
	   (cond (loop-iteration-variablep
		    (if (memq name loop-iteration-variables)
			(loop-simple-error
			   "Duplicated iteration variable somewhere in LOOP"
			   name)
			(push name loop-iteration-variables)))
		 ((assq name loop-variables)
		    (loop-simple-error
		       "Duplicated var in LOOP bind block" name)))
	 #-Vector-Destructuring
	   (or (symbolp name)
	       (loop-simple-error "Bad variable somewhere in LOOP" name))
	   (loop-declare-variable name dtype)
	   ; We use ASSQ on this list to check for duplications (above),
	   ; so don't optimize out this list:
	   (push (list name (or initialization (loop-typed-init dtype)))
		 loop-variables))
	(initialization
	   #+System-Destructuring
	     (progn (loop-declare-variable name dtype)
		    (push (list name initialization) loop-variables))
	   #-System-Destructuring
	     (cond (si:loop-use-system-destructuring?
		      (loop-declare-variable name dtype)
		      (push (list name initialization) loop-variables))
		   ('t (let ((newvar (gensym)))
			  (push (list newvar initialization) loop-variables)
			  ; LOOP-DESETQ-CROCKS gathered in reverse order.
			  (setq loop-desetq-crocks
				(list* name newvar loop-desetq-crocks))
			  (loop-make-variable name () dtype)))))
	('t
	  #-Vector-Destructuring
	    (let ((tcar) (tcdr))
	      (if (atom dtype) (setq tcar (setq tcdr dtype))
		  (setq tcar (car dtype) tcdr (cdr dtype)))
	      (loop-make-variable (car name) () tcar)
	      (loop-make-variable (cdr name) () tcdr))
	  #+Vector-Destructuring
	    (cond ((object-that-cares-p name)
		     (let ((tcar) (tcdr))
			(if (object-that-cares-p dtype)
			    (setq tcar (car dtype) tcdr (cdr dtype))
			    (setq tcar (setq tcdr dtype)))
			(loop-make-variable (car name) () tcar)
			(loop-make-variable (cdr name) () tcdr)))
		  ((vectorp name)
		     (do ((i 0 (1+ i))
			  (n (vector-length name))
			  (dti 0 (1+ dti))
			  (dtn (and (vectorp dtype) (vector-length dtype))))
			 ((= i n))
		       #+Run-in-Maclisp (declare (fixnum i n dti))
		       (loop-make-variable
			  (vref name i) ()
			  (if (null dtn) dtype
			      (and (< dti dtn) (vref dtype dti))))))
		  ('t (loop-simple-error
		         "bad variable somewhere in LOOP" name)))
	  ))
  name)


(defun loop-make-iteration-variable (name initialization dtype)
    (let ((loop-iteration-variablep 't))
       (loop-make-variable name initialization dtype)))


(defun loop-declare-variable (name dtype)
    (cond ((or (null name) (null dtype)) ())
	  ((symbolp name)
	     (cond ((memq name loop-nodeclare))
		 #+Multics
		   ; local type dcls of specials lose.  This doesn't work
		   ; for locally-declared specials.
		   ((get name 'special))
		   ((data-type? dtype)
		      (setq loop-declarations
			    (append (variable-declarations dtype name)
				    loop-declarations)))
		#+Meaningful-Type-Declarations
		   ((si:loop-tmember dtype '(fixnum flonum))
		      (push `(,dtype ,name) loop-declarations))))
	  ((object-that-cares-p name)
	      (cond ((object-that-cares-p dtype)
		       (loop-declare-variable (car name) (car dtype))
		       (loop-declare-variable (cdr name) (cdr dtype)))
		    ('t (loop-declare-variable (car name) dtype)
			(loop-declare-variable (cdr name) dtype))))
	#+Vector-Destructuring
	  ((vectorp name)
	     (do ((i 0 (1+ i))
		  (n (vector-length name))
		  (dtn (and (vectorp dtype) (vector-length dtype)))
		  (dti 0 (1+ dti)))
		 ((= i n))
	       #+Meaningful-Type-Declarations (declare (fixnum i n dti))
	       (loop-declare-variable
		  (vref name i)
		  (if (null dtn) dtype (and (< dti dtn) (vref dtype dti))))))
	  ('t (loop-simple-error "can't hack this"
				 (list 'loop-declare-variable name dtype)))))


#+For-PDP10
(declare (special squid))

(defun loop-constantp (form)
    (or (numberp form)
	#+For-NIL (or (null form) (vectorp form))
	#-For-NIL (memq form '(t ()))
	#-For-PDP10 (stringp form)
	(and (not (atom form))
	     #-Run-on-PDP10 (eq (car form) 'quote)
	     #+Run-on-PDP10 (or (eq (car form) 'quote)
				; SQUID implies quoting.
				(and compiler-state (eq (car form) squid))))
	))

(defun loop-maybe-bind-form (form data-type?)
    ; Consider implementations which will not keep EQ quoted constants
    ; EQ after compilation & loading.
    ; Note FUNCTION is not hacked, multiple occurences might cause the
    ; compiler to break the function off multiple times!
    ; Hacking it probably isn't too important here anyway.  The ones that
    ; matter are the ones that use it as a stepper (or whatever), which
    ; handle it specially.
    (if (loop-constantp form) form
	(loop-make-variable (gensym) form data-type?)))


(defun loop-optional-type ()
    (let ((token (car loop-source-code)))
	(and (not (null token))
	     (or (not (atom token))
		 (data-type? token)
		 (si:loop-tmember token '(fixnum flonum integer number notype
					  #+Loop-Small-Floatp small-flonum)))
	     (loop-pop-source))))


;Incorporates conditional if necessary
(defun loop-make-conditionalization (form)
  (cond ((not (null loop-conditionals))
	   (rplacd (last (car (last (car (last loop-conditionals)))))
		   (ncons form))
	   (cond ((si:loop-tequal (car loop-source-code) 'and)
		    (loop-pop-source)
		    ())
		 ((si:loop-tequal (car loop-source-code) 'else)
		    (loop-pop-source)
		    ;; If we are already inside an else clause, close it off
		    ;; and nest it inside the containing when clause
		    (let ((innermost (car (last loop-conditionals))))
		      (cond ((null (cddr innermost)))	;Now in a WHEN clause, OK
			    ((null (cdr loop-conditionals))
			     (loop-simple-error "More ELSEs than WHENs"
						(list 'else (car loop-source-code)
						      (cadr loop-source-code))))
			    ('t (setq loop-conditionals (cdr (nreverse loop-conditionals)))
				(rplacd (last (car (last (car loop-conditionals))))
					(ncons innermost))
				(setq loop-conditionals (nreverse loop-conditionals)))))
		    ;; Start a new else clause
		    (rplacd (last (car (last loop-conditionals)))
			    (ncons (ncons ''t)))
		    ())
		 ('t ;Nest up the conditionals and output them
		     (do ((prev (car loop-conditionals) (car l))
			  (l (cdr loop-conditionals) (cdr l)))
			 ((null l))
		       (rplacd (last (car (last prev))) (ncons (car l))))
		     (prog1 (car loop-conditionals)
			    (setq loop-conditionals ())))))
	('t form)))

(defun loop-pseudo-body (form &aux (z (loop-make-conditionalization form)))
   (cond ((not (null z))
	    (cond (loop-emitted-body? (push z loop-body))
		  ('t (push z loop-before-loop) (push z loop-after-body))))))

(defun loop-emit-body (form)
  (setq loop-emitted-body? 't)
  (loop-pseudo-body form))


#+Named-PROGs
(defun loop-do-named ()
    (let ((name (loop-pop-source)))
       (or (and name (symbolp name))
	   (loop-simple-error "Bad name for your loop construct" name))
       (and (cdr (setq loop-prog-names (cons name loop-prog-names)))
	    (loop-simple-error "Too many names for your loop construct"
			       loop-prog-names))))

(defun loop-do-initially ()
  (push (loop-get-form) loop-prologue))

(defun loop-nodeclare (&aux (varlist (loop-pop-source)))
    (or (and varlist (eq (typep varlist) 'list))
	(loop-simple-error "Bad varlist to nodeclare loop clause" varlist))
    (setq loop-nodeclare (append varlist loop-nodeclare)))

(defun loop-do-finally ()
  (push (loop-get-form) loop-epilogue))

(defun loop-do-do ()
  (loop-emit-body (loop-get-form)))

(defun loop-do-return ()
   (loop-pseudo-body `(return ,(loop-get-form))))


;;;; List Collection

; The way we collect (list-collect) things is to bind two variables.
; One is the final result, and is accessible for value during the
; loop compuation.  The second is the "tail".  In implementations where
; we can do so, the tail var is initialized to a locative of the first,
; such that it can be updated with RPLACD.  In other implementations,
; the update must be conditionalized (on whether or not the tail is NIL).

; For PDP10 Maclisp:
; The "value cell" of a special variable is a (pseudo) list cell, the CDR
; of which is the value.  Hence the abovementioned tail variable gets
; initialized to this.  (It happens to be the CDAR of the symbol.)
; For local variables in compiled code, the Maclisp compiler implements
; a (undocumented private) form of the
; "(setq tail (variable-location var))" construct;  specifically, it
; is of the form  (#.gofoo var tail).  This construct must appear in
; the binding environment those variables are bound in, currently.
; Note that this hack only currently works for local variables, so loop
; has to check to see if the variable is special.  It is anticipated,
; however, that the compiler will be able to do this all by itself
; at some point.

#+For-PDP10
  (progn 'compile
     (cond ((status feature complr)
	      (setq loop-specvar-hack ((lambda (obarray)
					   (implode '(s p e c v a r s)))
				       sobarray))
	      (defun loop-collect-init-compiler (form)
		(cond ((memq compiler-state '(toplevel maklap))
		         ; We are being "toplevel" macro expanded.
			 ; We MUST expand into something which can be
			 ; evaluated without loop, in the interpreter.
			 `(setq ,(caddr form) (munkam (value-cell-location
						         ',(cadr form)))))
		      ((or specials
			   (get (cadr form) 'special)
			   (assq (cadr form) (symeval loop-specvar-hack)))
		         `(setq ,(caddr form) (cdar ',(cadr form))))
		      (t (cons gofoo (cdr form)))))
	      (push '(loop-collect-init . loop-collect-init-compiler)
		    macrolist)))
     (defun loop-collect-init fexpr (x)
	(set (cadr x) (cdar (car x)))))

#+(and Hairy-Collection (not For-PDP10))
(defmacro loop-collect-init (var1 var2)
   #+Lispm ;*****  Remove kludgey fboundp when everyone up-to-date *****
	   `(setq ,var2 ,(if (fboundp 'variable-location)
			     `(variable-location ,var1)
			     `(value-cell-location ',var1)))
   #-Lispm `(setq ,var2 (munkam (value-cell-location ',var1))))


(defun loop-do-collect (type)
  (let ((var) (form) (tem) (tail) (dtype) (cruft) (rvar)
	(ctype (cond ((memq type '(max min)) 'maxmin)
		     ((memq type '(nconc list append)) 'list)
		     ((memq type '(count sum)) 'sum)
		     ('t (loop-simple-error
			    "unrecognized LOOP collecting keyword" type)))))
    (setq form (loop-get-form) dtype (loop-optional-type))
    (cond ((si:loop-tequal (car loop-source-code) 'into)
	     (loop-pop-source)
	     (setq rvar (setq var (loop-pop-source)))))
    ; CRUFT will be (varname ctype dtype var tail (optional tem))
    (cond ((setq cruft (assq var loop-collect-cruft))
	     (cond ((not (eq ctype (car (setq cruft (cdr cruft)))))
		      (loop-simple-error
		         "incompatible LOOP collection types"
			 (list ctype (car cruft))))
		   ((and dtype (not (eq dtype (cadr cruft))))
		      ;Conditional should be on data-type reality
		      #+Run-in-Maclisp
		        (loop-simple-error
			   "Unequal data types in multiple collections"
			   (list dtype (cadr cruft) (car cruft)))
		      #-Run-in-Maclisp
		        (ferror () "~A and ~A Unequal data types into ~A"
				dtype (cadr cruft) (car cruft))))
	     (setq dtype (car (setq cruft (cdr cruft)))
		   var (car (setq cruft (cdr cruft)))
		   tail (car (setq cruft (cdr cruft)))
		   tem (cadr cruft))
	     (and (eq ctype 'maxmin)
		  (not (atom form)) (null tem)
		  (rplaca (cdr cruft) (setq tem (loop-make-variable
						   (gensym) () dtype)))))
	  ('t (and (null dtype)
		   (setq dtype (cond ((eq type 'count) 'fixnum)
				     ((memq type '(min max sum)) 'number))))
	     (or var (push `(return ,(setq var (gensym)))
			   loop-after-epilogue))
	     (or (eq ctype 'list) (loop-make-iteration-variable var () dtype))
	     (setq tail 
		   (cond ((eq ctype 'list)
			    #-Hairy-Collection
			      (setq tem (loop-make-variable (gensym) () ()))
			    (car (setq loop-collection-crocks
				       (list* (gensym) var
					      loop-collection-crocks))))
			 ((eq ctype 'maxmin)
			    (or (atom form)
				(setq tem (loop-make-variable
					     (gensym) () dtype)))
			    (loop-make-variable (gensym) ''t ()))))
	     (push (list rvar ctype dtype var tail tem)
		   loop-collect-cruft)))
    (loop-emit-body
	(caseq type
	  (count (setq tem `(setq ,var (,(loop-typed-arith 'add1 dtype)
					,var)))
		 (if (member form '(t 't)) tem `(and ,form ,tem)))
	  (sum `(setq ,var (,(loop-typed-arith 'plus dtype) ,form ,var)))
	  ((max min)
	     (let ((forms ()) (arglist ()))
		; TEM is temporary, properly typed.
		(and tem (setq forms `((setq ,tem ,form)) form tem))
		(setq arglist (list var form))
		(push (if (si:loop-tmember dtype '(fixnum flonum
						   #+Loop-Small-Floatp
						     small-flonum))
			  ; no contagious arithmetic
			  `(and (or ,tail
				    (,(loop-typed-arith
				         (if (eq type 'max) 'lessp 'greaterp)
					 dtype)
				     . ,arglist))
				(setq ,tail () . ,arglist))
			  ; potentially contagious arithmetic -- must use
			  ; MAX or MIN so that var will be contaminated
			  `(setq ,var (cond (,tail (setq ,tail ()) ,form)
					    ((,type . ,arglist)))))
		      forms)
		(if (cdr forms) (cons 'progn (nreverse forms)) (car forms))))
	  (t (caseq type
		(list (setq form (list 'list form)))
		(append (or (and (not (atom form)) (eq (car form) 'list))
			    (setq form #+Lispm `(copylist* ,form)
				       #-Lispm `(append ,form ())))))
	   #+Hairy-Collection
	     (let ((q `(rplacd ,tail ,form)))
		(cond ((and (not (atom form)) (eq (car form) 'list)
			    (not (null (cdr form))))
		         ; RPLACD of cdr-coded list:
			 #+Lispm
			   (rplaca (cddr q)
				   (if (cddr form) `(list* ,@(cdr form) ())
				       `(ncons ,(cadr form))))
			 `(setq ,tail ,(loop-cdrify (cdr form) q)))
		      ('t `(and (cdr ,q)
				(setq ,tail (last (cdr ,tail)))))))
	   #-Hairy-Collection
	     (let ((q `(cond (,tail (cdr (rplacd ,tail ,tem)))
			     ((setq ,var ,tem)))))
		(if (and (not (atom form)) (eq (car form) 'list) (cdr form))
		    `(setq ,tem ,form ,tail ,(loop-cdrify (cddr form) q))
		    `(and (setq ,tem ,form) (setq ,tail (last ,q))))))))))


(defun loop-cdrify (arglist form)
    (do ((size (length arglist) (- size 4)))
	((< size 4)
	 (if (zerop size) form
	     (list (cond ((= size 1) 'cdr) ((= size 2) 'cddr) ('t 'cdddr))
		   form)))
      #+Meaningful-Type-Declarations (declare (fixnum size))
      (setq form (list 'cddddr form))))


(defun loop-do-while (cond kwd &aux (form (loop-get-form)))
    (and loop-conditionals (loop-simple-error
			      "not allowed inside LOOP conditional"
			      (list kwd form)))
    (loop-pseudo-body `(,cond ,form (go end-loop))))


(defun loop-do-when (negate?)
  (let ((form (loop-get-form)) (cond))
    (cond ((si:loop-tequal (cadr loop-source-code) 'it)
	     ;WHEN foo RETURN IT and the like
	     (setq cond `(setq ,(loop-when-it-variable) ,form))
	     (setq loop-source-code		;Plug in variable for IT
		   (list* (car loop-source-code)
			  loop-when-it-variable
			  (cddr loop-source-code))))
	  ('t (setq cond form)))
    (and negate? (setq cond `(not ,cond)))
    (setq loop-conditionals (nconc loop-conditionals `((cond (,cond)))))))

(defun loop-do-with ()
  (do ((var) (equals) (val) (dtype)) (())
    (setq var (loop-pop-source) equals (car loop-source-code))
    (cond ((si:loop-tequal equals '=)
	     (loop-pop-source)
	     (setq val (loop-get-form) dtype ()))
	  ((or (si:loop-tequal equals 'and)
	       (si:loop-tassoc equals loop-keyword-alist)
	       (si:loop-tassoc equals loop-iteration-keyword-alist))
	     (setq val () dtype ()))
	  ('t (setq dtype (loop-pop-source) equals (car loop-source-code))
	      (cond ((si:loop-tequal equals '=)
		       (loop-pop-source)
		       (setq val (loop-get-form)))
		    ((and (not (null loop-source-code))
			  (not (si:loop-tassoc equals loop-keyword-alist))
			  (not (si:loop-tassoc
				  equals loop-iteration-keyword-alist))
			  (not (si:loop-tequal equals 'and)))
		       (loop-simple-error "Garbage where = expected" equals))
		    ('t (setq val ())))))
    (loop-make-variable var val dtype)
    (if (not (si:loop-tequal (car loop-source-code) 'and)) (return ())
	(loop-pop-source)))
  (loop-bind-block))

(defun loop-do-always (pred)
  (let ((form (loop-get-form)))
    (loop-emit-body `(,pred ,form (return ())))
    (push '(return 't) loop-after-epilogue)))

;THEREIS expression
;If expression evaluates non-nil, return that value.
(defun loop-do-thereis ()
   (loop-emit-body `(and (setq ,(loop-when-it-variable) ,(loop-get-form))
			 (return ,loop-when-it-variable))))


;;;; Hacks

#+Meaningful-Type-Declarations
  (declare (fixnum (loop-simplep-1 notype)))

(defun si:loop-simplep (expr)
    (if (null expr) 0
	(*catch 'si:loop-simplep
	    (let ((ans (si:loop-simplep-1 expr)))
	       #+Meaningful-Type-Declarations (declare (fixnum ans))
	       (and (< ans 20.) ans)))))

(defvar si:loop-simplep
  (append '(> < greaterp lessp plusp minusp typep zerop
	    plus difference + - add1 sub1 1+ 1-
	    +$ -$ 1+$ 1-$ boole rot ash ldb equal atom
	    setq prog1 prog2 and or =)
	  #+(or Lispm NIL) '(aref ar-1 ar-2 ar-3)
	  #+Lispm '#.(and (loop-featurep Lispm)
			  (mapcar 'ascii '(#/ #/ #/)))
	  #+For-NIL '(vref vector-length 1+& 1-& +& -& +p -p 0p *& //& \&
                      si:xref char string-length)
	  ))

(defun si:loop-simplep-1 (x)
  (let ((z 0))
    #+Meaningful-Type-Declarations (declare (fixnum z))
    (cond ((loop-constantp x) 0)
	  ((atom x) 1)
	  ((eq (car x) 'cond)
	     (do ((cl (cdr x) (cdr cl))) ((null cl))
	       (do ((f (car cl) (cdr f))) ((null f))
		 (setq z (+ (si:loop-simplep-1 (car f)) z 1))))
	     z)
	  ((symbolp (car x))
	     (let ((fn (car x)) (tem ()))
	       (cond ((setq tem (get fn 'si:loop-simplep))
		        (if (fixp tem) (setq z tem)
			    (setq z (funcall tem x) x ())))
		     ((memq fn '(null not eq go return progn)))
		     (#+Run-on-PDP10
		        (or (not (minusp (+internal-carcdrp fn)))
				      (eq fn 'cxr))
		      #-Run-on-PDP10 (memq fn '(car cdr))
		        (setq z 1))
		   #-Run-on-PDP10
		     ((memq fn '(caar cadr cdar cddr)) (setq z 2))
		   #-Run-on-PDP10
		     ((memq fn '(caaar caadr cadar caddr
				 cdaar cdadr cddar cdddr))
		        (setq z 3))
		   #-Run-on-PDP10
		     ((memq fn '(caaaar caaadr caadar caaddr
				 cadaar cadadr caddar cadddr
				 cdaaar cdaadr cdadar cdaddr
				 cddaar cddadr cdddar cddddr))
		        (setq z 4))
		     ((memq fn si:loop-simplep)
		        (setq z 2))
		     (#+(or Lispm For-PDP10 For-NIL)
		        (not (eq (setq tem (macroexpand-1 x)) x))
		      #+Franz (not (eq (setq tem (macroexpand x)) x))
		      #+Multics
		        (setq tem (get (car x) 'macro))
		      #+Multics (setq tem (funcall tem x))
		      (setq z (si:loop-simplep-1 tem) x ()))
		     ('t (*throw 'si:loop-simplep ())))
	       (do ((l (cdr x) (cdr l))) ((null l))
		 (setq z (+ (si:loop-simplep-1 (car l)) 1 z)))
	       z))
	  ('t (*throw 'si:loop-simplep ())))))


;;;; The iteration driver
(defun loop-hack-iteration (entry)
  (do ((last-entry entry)
       (source loop-source-code loop-source-code)
       (pre-step-tests ())
       (steps ())
       (post-step-tests ())
       (pseudo-steps ())
       (pre-loop-pre-step-tests ())
       (pre-loop-steps ())
       (pre-loop-post-step-tests ())
       (pre-loop-pseudo-steps ())
       (tem) (data) (foo) (bar))
      (())
    ; Note we collect endtests in reverse order, but steps in correct
    ; order.  LOOP-END-TESTIFY does the nreverse for us.
    (setq tem (setq data (apply (cadr entry) (cddr entry))))
    (and (car tem) (push (car tem) pre-step-tests))
    (setq steps (nconc steps (loop-copylist* (car (setq tem (cdr tem))))))
    (and (car (setq tem (cdr tem))) (push (car tem) post-step-tests))
    (setq pseudo-steps
	  (nconc pseudo-steps (loop-copylist* (car (setq tem (cdr tem))))))
    (setq tem (cdr tem))
    (and (or loop-conditionals loop-emitted-body?)
	 (or tem pre-step-tests post-step-tests pseudo-steps)
	 (let ((cruft (list (car entry) (car source)
			    (cadr source) (caddr source))))
	    (if loop-emitted-body?
		(loop-simple-error
		   "Iteration is not allowed to follow body code" cruft)
		(loop-simple-error
		   "Iteration starting inside of conditional in LOOP"
		   cruft))))
    (or tem (setq tem data))
    (and (car tem) (push (car tem) pre-loop-pre-step-tests))
    (setq pre-loop-steps
	  (nconc pre-loop-steps (loop-copylist* (car (setq tem (cdr tem))))))
    (and (car (setq tem (cdr tem))) (push (car tem) pre-loop-post-step-tests))
    (setq pre-loop-pseudo-steps
	  (nconc pre-loop-pseudo-steps (loop-copylist* (cadr tem))))
    (cond ((or (not (si:loop-tequal (car loop-source-code) 'and))
	       (and loop-conditionals
		    (not (si:loop-tassoc (cadr loop-source-code)
					 loop-iteration-keyword-alist))))
	     (setq foo (list (loop-end-testify pre-loop-pre-step-tests)
			     (loop-make-psetq pre-loop-steps)
			     (loop-end-testify pre-loop-post-step-tests)
			     (loop-make-setq pre-loop-pseudo-steps))
		   bar (list (loop-end-testify pre-step-tests)
			     (loop-make-psetq steps)
			     (loop-end-testify post-step-tests)
			     (loop-make-setq pseudo-steps)))
	     (cond ((not loop-conditionals)
		      (setq loop-before-loop (nreconc foo loop-before-loop)
			    loop-after-body (nreconc bar loop-after-body)))
		   ('t ((lambda (loop-conditionals)
			   (push (loop-make-conditionalization
				    (cons 'progn (delq () foo)))
				 loop-before-loop))
			(mapcar '(lambda (x)	;Copy parts that will get rplacd'ed
				   (cons (car x)
					 (mapcar '(lambda (x) (loop-copylist* x)) (cdr x))))
				loop-conditionals))
		       (push (loop-make-conditionalization
			        (cons 'progn (delq () bar)))
			     loop-after-body)))
	     (loop-bind-block)
	     (return ())))
    (loop-pop-source) ; flush the "AND"
    (setq entry (cond ((setq tem (si:loop-tassoc
				    (car loop-source-code)
				    loop-iteration-keyword-alist))
		         (loop-pop-source)
			 (setq last-entry tem))
		      ('t last-entry)))))


;FOR variable keyword ..args..
(defun loop-do-for ()
  (let ((var (loop-pop-source))
	(data-type? (loop-optional-type))
	(keyword (loop-pop-source))
	(first-arg (loop-get-form))
	(tem ()))
    (or (setq tem (si:loop-tassoc keyword loop-for-keyword-alist))
	(loop-simple-error
	   "Unknown keyword in FOR or AS clause in LOOP"
	   (list 'for var keyword)))
    (lexpr-funcall (cadr tem) var first-arg data-type? (cddr tem))))


(defun loop-do-repeat ()
    (let ((var (loop-make-variable (gensym) (loop-get-form) 'fixnum)))
       `((not (,(loop-typed-arith 'plusp 'fixnum) ,var))
         () ()
         (,var (,(loop-typed-arith 'sub1 'fixnum) ,var)))))


; Kludge the First
(defun loop-when-it-variable ()
    (or loop-when-it-variable
	(setq loop-when-it-variable
	      (loop-make-variable (gensym) () ()))))



(defun loop-for-equals (var val data-type?)
  (cond ((si:loop-tequal (car loop-source-code) 'then)
	   ;FOR var = first THEN next
	   (loop-pop-source)
	   (loop-make-iteration-variable var val data-type?)
	   `(() (,var ,(loop-get-form)) () ()
	     () () () ()))
	('t (loop-make-iteration-variable var () data-type?)
	    (let ((varval (list var val)))
	      (cond (loop-emitted-body?
		     (loop-emit-body (loop-make-setq varval))
		     '(() () () ()))
		    (`(() ,varval () ())))))))

(defun loop-for-first (var val data-type?)
    (or (si:loop-tequal (car loop-source-code) 'then)
	(loop-simple-error "found where THEN expected in FOR ... FIRST"
			   (car loop-source-code)))
    (loop-pop-source)
    (loop-make-iteration-variable var () data-type?)
    `(() (,var ,(loop-get-form)) () () () (,var ,val) () ()))


(defun loop-list-stepper (var val data-type? fn)
    (let ((stepper (cond ((si:loop-tequal (car loop-source-code) 'by)
			    (loop-pop-source) (loop-get-form))
			 ('t '(function cdr))))
	  (var1 ()) (stepvar ()) (step ()) (et ()) (pseudo ()))
       (setq step (if (or (atom stepper)
			  (not (memq (car stepper) '(quote function))))
		      `(funcall ,(setq stepvar (gensym)))
		      (list (cadr stepper))))
       (cond ((and (atom var)
		   ;; (eq (car step) 'cdr)
		   (not fn))
	        (setq var1 (loop-make-iteration-variable var val data-type?)))
	     ('t (loop-make-iteration-variable var () data-type?)
		 (setq var1 (loop-make-variable (gensym) val ()))
		 (setq pseudo (list var (if fn (list fn var1) var1)))))
       (rplacd (last step) (list var1))
       (and stepvar (loop-make-variable stepvar stepper ()))
       (setq stepper (list var1 step) et `(null ,var1))
       (if (not pseudo) `(() ,stepper ,et () () () ,et ())
	   (if (eq (car step) 'cdr) `(,et ,pseudo () ,stepper)
	       `((null (setq . ,stepper)) () () ,pseudo ,et () () ,pseudo)))))


(defun loop-for-arithmetic (var val data-type? kwd)
  ; Args to loop-sequencer:
  ; indexv indexv-type variable? vtype? sequencev? sequence-type
  ; stephack? default-top? crap prep-phrases
  (si:loop-sequencer
     var (or data-type? 'fixnum) () () () () () () `(for ,var ,kwd ,val)
     (cons (list kwd val)
	   (loop-gather-preps
	      '(from upfrom downfrom to upto downto above below by)
	      ()))))


(defun si:loop-named-variable (name)
    (let ((tem (si:loop-tassoc name loop-named-variables)))
       (cond ((null tem) (gensym))
	     ('t (setq loop-named-variables (delq tem loop-named-variables))
		 (cdr tem)))))

#+Run-in-Maclisp ;Gross me out
(and (status feature #+Multics Compiler #-Multics complr)
     (*expr si:loop-named-variable))


; Note:  path functions are allowed to use loop-make-variable, hack
; the prologue, etc.
(defun loop-for-being (var val data-type?)
   ; FOR var BEING something ... - var = VAR, something = VAL.
   ; If what passes syntactically for a pathname isn't, then
   ; we trap to the DEFAULT-LOOP-PATH path;  the expression which looked like
   ; a path is given as an argument to the IN preposition.  Thus,
   ; by default, FOR var BEING EACH expr OF expr-2
   ; ==> FOR var BEING DEFAULT-LOOP-PATH IN expr OF expr-2.
   (let ((tem) (inclusive?) (ipps) (each?) (attachment))
     (if (or (si:loop-tequal val 'each) (si:loop-tequal val 'the))
	 (setq each? 't val (car loop-source-code))
	 (push val loop-source-code))
     (cond ((and (setq tem (si:loop-tassoc val loop-path-keyword-alist))
		 (or each? (not (si:loop-tequal (cadr loop-source-code)
						'and))))
	      ;; FOR var BEING {each} path {prep expr}..., but NOT
	      ;; FOR var BEING var-which-looks-like-path AND {ITS} ...
	      (loop-pop-source))
	   ('t (setq val (loop-get-form))
	       (cond ((si:loop-tequal (car loop-source-code) 'and)
			;; FOR var BEING value AND ITS path-or-ar
			(or (null each?)
			    (loop-simple-error
			       "Malformed BEING EACH clause in LOOP" var))
			(setq ipps `((of ,val)) inclusive? 't)
			(loop-pop-source)
			(or (si:loop-tmember (setq tem (loop-pop-source))
					     '(its his her their each))
			    (loop-simple-error
			       "found where ITS or EACH expected in LOOP path"
			       tem))
			(if (setq tem (si:loop-tassoc
					 (car loop-source-code)
					 loop-path-keyword-alist))
			    (loop-pop-source)
			    (push (setq attachment `(in ,(loop-get-form)))
				  ipps)))
		     ((not (setq tem (si:loop-tassoc
					(car loop-source-code)
					loop-path-keyword-alist)))
			; FOR var BEING {each} a-r ...
			(setq ipps (list (setq attachment (list 'in val)))))
		     ('t ; FOR var BEING {each} pathname ...
			 ; Here, VAL should be just PATHNAME.
			 (loop-pop-source)))))
     (cond ((not (null tem)))
	   ((not (setq tem (si:loop-tassoc 'default-loop-path
					   loop-path-keyword-alist)))
	      (loop-simple-error "Undefined LOOP iteration path"
				 (cadr attachment))))
     (setq tem (funcall (cadr tem) (car tem) var data-type?
			(nreconc ipps (loop-gather-preps (caddr tem) 't))
			inclusive? (caddr tem) (cdddr tem)))
     (and loop-named-variables
	  (loop-simple-error "unused USING variables" loop-named-variables))
     ; For error continuability (if there is any):
     (setq loop-named-variables ())
     ;; TEM is now (bindings prologue-forms . stuff-to-pass-back)
     (do ((l (car tem) (cdr l)) (x)) ((null l))
       (if (atom (setq x (car l)))
	   (loop-make-iteration-variable x () ())
	   (loop-make-iteration-variable (car x) (cadr x) (caddr x))))
     (setq loop-prologue (nconc (reverse (cadr tem)) loop-prologue))
     (cddr tem)))


(defun loop-gather-preps (preps-allowed crockp)
   (do ((token (car loop-source-code) (car loop-source-code)) (preps ()))
       (())
     (cond ((si:loop-tmember token preps-allowed)
	      (push (list (loop-pop-source) (loop-get-form)) preps))
	   ((si:loop-tequal token 'using)
	      (loop-pop-source)
	      (or crockp (loop-simple-error
			    "USING used in illegal context"
			    (list 'using (car loop-source-code))))
	      (do ((z (car loop-source-code) (car loop-source-code)) (tem))
		  ((atom z))
		(and (or (atom (cdr z))
			 (not (null (cddr z)))
			 (not (symbolp (car z)))
			 (and (cadr z) (not (symbolp (cadr z)))))
		     (loop-simple-error
		        "bad variable pair in path USING phrase" z))
		(cond ((not (null (cadr z)))
		         (and (setq tem (si:loop-tassoc
					   (car z) loop-named-variables))
			      (loop-simple-error
			         "Duplicated var substitition in USING phrase"
				 (list tem z)))
			 (push (cons (car z) (cadr z)) loop-named-variables)))
		(loop-pop-source)))
	   ('t (return (nreverse preps))))))

(defun loop-add-path (name data)
    (setq loop-path-keyword-alist
	  (cons (cons name data)
		; Don't change this to use DELASSQ in PDP10, the lsubr
		; calling sequence makes that lose.
		(delq (si:loop-tassoc name loop-path-keyword-alist)
		      loop-path-keyword-alist)))
    ())

#+Run-on-PDP10
(declare ; Suck my obarray...
	 (own-symbol define-loop-path define-loop-sequence-path))

(defmacro define-loop-path (names &rest cruft)
  (setq names (if (atom names) (list names) names))
  #-For-Maclisp
    (let ((forms (mapcar #'(lambda (name) `(loop-add-path ',name ',cruft))
			 names)))
       `(eval-when (eval load compile)
	    #+For-NIL (flush-macromemos 'loop ())
	    ,@forms))
  #+For-Maclisp
    (subst (do ((l)) ((null names) l)
	     (setq l (cons `(setq loop-path-keyword-alist
				  (cons '(,(car names) . ,cruft)
					(delq (assq ',(car names)
						    loop-path-keyword-alist)
					      loop-path-keyword-alist)))
			   l)
		   names (cdr names)))
	   'progn
	   '(eval-when (eval load compile)
	     #-For-PDP10 (or (boundp 'loop-path-keyword-alist)
			      (setq loop-path-keyword-alist ()))
	     #+For-PDP10 (and (or (boundp 'loop-path-keyword-alist)
				   (setq loop-path-keyword-alist ()))
			       (flush-macromemos 'loop ()))
	       . progn)))


(defun si:loop-sequencer (indexv indexv-type
			  variable? vtype?
			  sequencev? sequence-type?
			  stephack? default-top?
			  crap prep-phrases)
   (let ((endform) (sequencep) (test)
	 (step ; Gross me out!
	       (add1 (or (loop-typed-init indexv-type) 0)))
	 (dir) (inclusive-iteration?) (start-given?) (limit-given?))
     (and variable? (loop-make-iteration-variable variable? () vtype?))
     (do ((l prep-phrases (cdr l)) (prep) (form) (odir)) ((null l))
       (setq prep (caar l) form (cadar l))
       (cond ((si:loop-tmember prep '(of in))
		(and sequencep (loop-simple-error
				  "Sequence duplicated in LOOP path"
				  (list variable? (car l))))
		(setq sequencep 't)
		(loop-make-variable sequencev? form sequence-type?))
	     ((si:loop-tmember prep '(from downfrom upfrom))
	        (and start-given?
		     (loop-simple-error
		        "Iteration start redundantly specified in LOOP sequencing"
			(append crap l)))
		(setq start-given? 't)
		(cond ((si:loop-tequal prep 'downfrom) (setq dir 'down))
		      ((si:loop-tequal prep 'upfrom) (setq dir 'up)))
		(loop-make-iteration-variable indexv form indexv-type))
	     ((cond ((si:loop-tequal prep 'upto)
		       (setq inclusive-iteration? (setq dir 'up)))
		    ((si:loop-tequal prep 'to)
		       (setq inclusive-iteration? 't))
		    ((si:loop-tequal prep 'downto)
		       (setq inclusive-iteration? (setq dir 'down)))
		    ((si:loop-tequal prep 'above) (setq dir 'down))
		    ((si:loop-tequal prep 'below) (setq dir 'up)))
		(and limit-given?
		     (loop-simple-error
		       "Endtest redundantly specified in LOOP sequencing path"
		       (append crap l)))
		(setq limit-given? 't)
		(setq endform (loop-maybe-bind-form form indexv-type)))
	     ((si:loop-tequal prep 'by)
		(setq step (if (loop-constantp form) form
			       (loop-make-variable (gensym) form 'fixnum))))
	     ('t ; This is a fatal internal error...
		 (loop-simple-error "Illegal prep in sequence path"
				    (append crap l))))
       (and odir dir (not (eq dir odir))
	    (loop-simple-error
	       "Conflicting stepping directions in LOOP sequencing path"
	       (append crap l)))
       (setq odir dir))
     (and sequencev? (not sequencep)
	  (loop-simple-error "Missing OF phrase in sequence path" crap))
     ; Now fill in the defaults.
     (setq step (list indexv step))
     (cond ((memq dir '(() up))
	      (or start-given?
		  (loop-make-iteration-variable indexv 0 indexv-type))
	      (and (or limit-given?
		       (cond (default-top?
			        (loop-make-variable
				   (setq endform (gensym)) () indexv-type)
				(push `(setq ,endform ,default-top?)
				      loop-prologue))))
		   (setq test (if inclusive-iteration? '(greaterp . args)
				  '(not (lessp . args)))))
	      (push 'plus step))
	   ('t (cond ((not start-given?)
		        (or default-top?
			    (loop-simple-error
			       "Don't know where to start stepping"
			       (append crap prep-phrases)))
			(loop-make-iteration-variable indexv 0 indexv-type)
			(push `(setq ,indexv
				     (,(loop-typed-arith 'sub1 indexv-type)
				      ,default-top?))
			      loop-prologue)))
	       (cond ((and default-top? (not endform))
		        (setq endform (loop-typed-init indexv-type)
			      inclusive-iteration? 't)))
	       (and (not (null endform))
		    (setq test (if inclusive-iteration? '(lessp . args)
				   '(not (greaterp . args)))))
	       (push 'difference step)))
     (and (member (caddr step)
		  #+Loop-Small-Floatp
		    '(1 1.0 #.(and (loop-featurep Loop-Small-Floatp)
				   (small-float 1)))
		  #-Loop-Small-Floatp '(1 1.0))
	  (rplacd (cdr (rplaca step (if (eq (car step) 'plus) 'add1 'sub1)))
		  ()))
     (rplaca step (loop-typed-arith (car step) indexv-type))
     (setq step (list indexv step))
     (setq test (loop-typed-arith test indexv-type))
     (setq test (subst (list indexv endform) 'args test))
     (and stephack? (setq stephack? `(,variable? ,stephack?)))
     `(() ,step ,test ,stephack?
       () () ,test ,stephack?)))


; Although this function is no longer documented, the "SI:" is needed
; because compiled files may reference it that way (via
; DEFINE-LOOP-SEQUENCE-PATH).
(defun si:loop-sequence-elements-path (path variable data-type
				       prep-phrases inclusive?
				       allowed-preps data)
    allowed-preps ; unused
    (let ((indexv (si:loop-named-variable 'index))
	  (sequencev (si:loop-named-variable 'sequence))
	  (fetchfun ()) (sizefun ()) (type ()) (default-var-type ())
	  (crap `(for ,variable being the ,path)))
       (cond ((not (null inclusive?))
	        (rplacd (cddr crap) `(,(cadar prep-phrases) and its ,path))
		(loop-simple-error "Can't step sequence inclusively" crap)))
       (setq fetchfun (car data)
	     sizefun (car (setq data (cdr data)))
	     type (car (setq data (cdr data)))
	     default-var-type (cadr data))
       (list* () () ; dummy bindings and prologue
	      (si:loop-sequencer
	         indexv 'fixnum
		 variable (or data-type default-var-type)
		 sequencev type
		 `(,fetchfun ,sequencev ,indexv) `(,sizefun ,sequencev)
		 crap prep-phrases))))


#+Run-on-PDP10
(defun (define-loop-sequence-path macro) (x)
    `(define-loop-path ,(cadr x) si:loop-sequence-elements-path
	(of in from downfrom to downto below above by)
	. ,(cddr x)))

#-Run-on-PDP10
(defmacro define-loop-sequence-path (path-name-or-names fetchfun sizefun
				     &optional sequence-type element-type)
    `(define-loop-path ,path-name-or-names
	si:loop-sequence-elements-path
	(of in from downfrom to downto below above by)
	,fetchfun ,sizefun ,sequence-type ,element-type))


;;;; NIL interned-symbols path

#+For-NIL
(progn 'compile
(defun loop-interned-symbols-path (path variable data-type prep-phrases
				   inclusive? allowed-preps data
				   &aux statev1 statev2 statev3
					(localp (car data)))
   allowed-preps	; unused
   (and inclusive? (loop-simple-error
		      "INTERNED-SYMBOLS path doesn't work inclusively"
		      variable))
   (and (not (null prep-phrases))
	(or (cdr prep-phrases)
	    (not (si:loop-tmember (caar prep-phrases) '(in of))))
	(ferror () "Illegal prep phrase(s) in ~A path of ~A - ~A"
		path variable prep-phrases))
   (loop-make-variable variable () data-type)
   (loop-make-variable
      (setq statev1 (gensym))
      `(loop-find-package
	  ,@(and prep-phrases `(,(cadar prep-phrases))))
      ())
   (loop-make-variable (setq statev2 (gensym)) () ())
   (loop-make-variable (setq statev3 (gensym)) () ())
   (push `(multiple-value (,statev1 ,statev2 ,statev3)
	       (loop-initialize-mapatoms-state ,statev1 ',localp))
	 loop-prologue)
   `(() () (multiple-value (() ,statev1 ,statev2 ,statev3)
	      (,(if localp 'loop-test-and-step-mapatoms-local
		    'loop-test-and-step-mapatoms)
	       ,statev1 ,statev2 ,statev3))
     (,variable (loop-get-mapatoms-symbol ,statev1 ,statev2 ,statev3)) () ()))

(defun loop-find-package (&optional (pkg () pkgp))
  #+Run-in-Maclisp
    (if pkgp pkg obarray)
  #-Run-in-Maclisp
    (if pkgp (pkg-find-package pkg) package))

(defun loop-find-package-translate (form)
  ; Note that we can only be compiling for nil-nil, so we only need
  ; to consider that.  The run-in-maclisp conditionals in the functions
  ; are for the benefit of running interpreted code.
  (values (if (null (cdr form)) 'package `(pkg-find-package ,(cadr form))) 't))

(putprop 'loop-find-package
	 '(loop-find-package-translate)
	 'source-trans)

#-Run-in-Maclisp
(defun loop-initialize-mapatoms-state (pkg localp)
    (let* ((symtab (si:package-symbol-table pkg))
	   (len (vector-length symtab)))
       (values pkg len (if localp symtab (cons (ncons pkg) ())))))

#+Run-in-Maclisp
(defun loop-initialize-mapatoms-state (ob ())
    (values ob (ncons nil) 511.))

#-Run-in-Maclisp
(defun loop-test-and-step-mapatoms (pkg index location &aux val)
    (prog (symtab)
	 (setq symtab (si:package-symbol-table pkg))
      lp (cond ((-p (setq index (1-& index)))
                  ;(do ((l (si:package-super-packages pkg) (cdr l)))
		  ;    ((null l) (cdr location))
		  ;  (or (memq (car l) (car location))
		  ;      (memq (car l) (cdr location))
		  ;      (rplacd location (cons (car l) (cdr location)))))
                  (let ((p (si:package-super-package pkg)))
                    (or (memq p (car location))
                        (memq p (cdr location))
                        (rplacd location (cons p (cdr location)))))
		  (or (cdr location) (return (setq val 't)))
		  (rplacd location
			  (prog1 (cddr location)
				 (rplaca location
					 (rplacd (cdr location)
						 (car location)))))
		  (setq pkg (caar location))
		  (setq symtab (si:package-symbol-table pkg))
		  (setq index (vector-length symtab))
		  (go lp))
	       ((symbolp (vref symtab index)) (return ()))
	       ('t (go lp))))
    (values val pkg index location))

#+Run-in-Maclisp
(defun loop-test-and-step-mapatoms (ob list index)
    (loop-test-and-step-mapatoms-local ob list index))

#-Run-in-Maclisp
(defun loop-test-and-step-mapatoms-local (pkg index symtab &aux val)
    (prog ()
      lp (cond ((-p (setq index (1-& index))) (return (setq val 't)))
	       ((symbolp (vref symtab index)) (return ()))
	       ('t (go lp))))
    (values val pkg index symtab))

#+Run-in-Maclisp
(defun loop-test-and-step-mapatoms-local (ob list index &aux val)
    (declare (fixnum index))
    (prog () 
     lp (cond ((not (null (cdr list)))
	         (rplaca list (cadr list))
		 (rplacd list (cddr list))
		 (return ()))
	      ((minusp (setq index (1- index))) (return (setq val 't)))
	      ('t ; If this is going to run in multics maclisp also the
		  ; arraycall should be hacked to have type `obarray'.
		  (rplacd list (arraycall t ob index))
		  (go lp))))
    (values val ob list index))

#-Run-in-Maclisp
(defun loop-get-mapatoms-symbol (pkg index something-or-other)
    ;Note there is a potential bug/timing screw in here.  We should be
    ; looking in the symbol-table saved initially, not the current one.
    ; There just isn't enough state saved (sigh).
    (declare (ignore something-or-other))
    (vref (si:package-symbol-table pkg) index))

#+Run-in-Maclisp
(defun loop-get-mapatoms-symbol (ob list index)
    (declare (ignore ob index))
    (car list))

(and #+Run-in-Maclisp (status feature complr)
     (*expr loop-get-mapatoms-symbol
	    loop-initialize-mapatoms-state
	    loop-test-and-step-mapatoms
	    loop-test-and-step-mapatoms-local))
)


;;;; Maclisp interned-symbols path

#+For-Maclisp
(defun loop-interned-symbols-path (path variable data-type prep-phrases
				   inclusive? allowed-preps data
				   &aux indexv listv ob)
   allowed-preps data	; unused vars
   (and inclusive? (loop-simple-error
		      "INTERNED-SYMBOLS path doesn't work inclusively"
		      variable))
   (and (not (null prep-phrases))
	(or (cdr prep-phrases)
	    (not (si:loop-tmember (caar prep-phrases) '(in of))))
	(loop-simple-error
	   "Illegal prep phrase(s) in INTERNED-SYMBOLS LOOP path"
	   (list* variable 'being path prep-phrases)))
   (loop-make-variable variable () data-type)
   (loop-make-variable
      (setq ob (gensym)) (if prep-phrases (cadar prep-phrases) 'obarray) ())
   ; Multics lisp does not store single-char-obs in the obarray buckets.
   ; Thus, we need to iterate over the portion of the obarray
   ; containing them also.  (511. = (ascii 0))
   (loop-make-variable
      (setq indexv (gensym)) #+Multics 639. #-Multics 511. 'fixnum)
   (loop-make-variable (setq listv (gensym)) () ())
   `(() ()
     (and #-Multics (null ,listv)
	  #+Multics (or (> ,indexv 510.) (null ,listv))
	  (prog ()
	   lp (cond ((minusp (setq ,indexv (1- ,indexv))) (return t))
		    ((setq ,listv (arraycall ; The following is the kind of
					     ; gratuity that pisses me off:
					     #+Multics obarray #-Multics t
					     ,ob ,indexv))
		       (return ()))
		    ((go lp)))))
     (,variable
       #+Multics (cond ((> ,indexv 510.) ,listv)
		       (t (prog2 () (car ,listv) (setq ,listv (cdr ,listv)))))
       #-Multics (car ,listv))
      ()
     #+Multics () #-Multics (,listv (cdr ,listv))))


;;;; Lispm interned-symbols path

#+Lispm
(progn 'compile

 (defun loop-interned-symbols-path (path variable data-type prep-phrases
				    inclusive? allowed-preps data
				    &aux statev1 statev2 statev3
					 (localp (car data)))
    path data-type allowed-preps			; unused vars
    (and inclusive? (loop-simple-error
		       "INTERNED-SYMBOLS path doesn't work inclusively"
		       variable))
    (and (not (null prep-phrases))
	 (or (cdr prep-phrases)
	     (not (si:loop-tmember (caar prep-phrases) '(in of))))
	   (ferror () "Illegal prep phrase(s) in ~A path of ~A - ~A"
		   path variable prep-phrases))
    (loop-make-variable variable () data-type)
    (loop-make-variable
       (setq statev1 (gensym))
       (if prep-phrases `(pkg-find-package ,(cadar prep-phrases)) 'package)
       ())
    (loop-make-variable (setq statev2 (gensym)) () ())
    (loop-make-variable (setq statev3 (gensym)) () ())
    (push `(multiple-value (,statev1 ,statev2 ,statev3)
		  (loop-initialize-mapatoms-state ,statev1 ,localp))
	    loop-prologue)
    `(() () (multiple-value (nil ,statev1 ,statev2 ,statev3)
	       (,(if localp 'loop-test-and-step-mapatoms-local
		     'loop-test-and-step-mapatoms)
		,statev1 ,statev2 ,statev3)) 
      (,variable (loop-get-mapatoms-symbol ,statev1 ,statev2 ,statev3))
      () ()))

 (defun loop-initialize-mapatoms-state (pkg localp)
    ; Return the initial values of the three state variables.
    ; This scheme uses them to be:
    ; (1)  Index into the package (decremented as we go)
    ; (2)  Temporary (to hold the symbol)
    ; (3)  the package
    localp ; ignored
    (prog ()
       (return (array-dimension-n 2 pkg) () pkg)))

 (defun loop-test-and-step-mapatoms (index temp pkg)
    temp ; ignored
    (prog ()
     lp (cond ((< (setq index (1- index)) 0)
	         (cond ((setq pkg (pkg-super-package pkg))
			  (setq index (array-dimension-n 2 pkg))
			  (go lp))
		       (t (return t))))
	      ((numberp (ar-2 pkg 0 index))
	         (return nil index (ar-2 pkg 1 index) pkg))
	      (t (go lp)))))

 (defun loop-test-and-step-mapatoms-local (index temp pkg)
    temp ; ignored
    (prog ()
     lp (cond ((minusp (setq index (1- index))) (return t))
	      ((numberp (ar-2 pkg 0 index))
	         (return () index (ar-2 pkg 1 index) pkg))
	      (t (go lp)))))

 (defun loop-get-mapatoms-symbol (index temp pkg)
    index pkg ; ignored
    temp)
 )

; We don't want these defined in the compilation environment because
; the appropriate environment hasn't been set up.  So, we just bootstrap
; them up.
(mapc '(lambda (x)
	  (mapc '(lambda (y)
		    (setq loop-path-keyword-alist
			  (cons (cons y (cdr x))
				(delq (si:loop-tassoc
				         y loop-path-keyword-alist)
				      loop-path-keyword-alist))))
		(car x)))
      '(
      #+(or For-NIL For-Maclisp Lispm)
	((interned-symbols interned-symbol)
	   loop-interned-symbols-path (in))
      #+(or For-NIL Lispm)
	((local-interned-symbols local-interned-symbol)
	   loop-interned-symbols-path (in) t)
	))

#-Multics ; none defined yet
(mapc '(lambda (x)
	 (mapc '(lambda (y)
		  (setq loop-path-keyword-alist
			(cons `(,y si:loop-sequence-elements-path
				(of in from downfrom to downto below above by)
				. ,(cdr x))
			      (delq (si:loop-tassoc
				      y loop-path-keyword-alist)
				    loop-path-keyword-alist))))
	       (car x)))
      '(#+Lispm
        ((array-element array-elements) aref array-active-length)
	; These NIL guys are set up by NILAID in the PDP10 version but no one
	; sets them up on the VAX.  Anyway redundancy won't hurt unless i
	; break something.
	#+(and For-NIL (not Run-in-Maclisp))
	  ((vector-element vector-elements) vref vector-length vector)
        #+(and For-NIL (not Run-in-Maclisp))
	  ((bit bits) bit bits-length bits fixnum)
	#+(and For-NIL (not Run-in-Maclisp))
	  ((character characters) char string-length string character)
	)
      )

; Sigh. (c.f. loop-featurep, note macro-expansion lossage.)
; Note that we end up doing both in the PDP10 NIL version.
#+(or (not For-NIL) Run-in-Maclisp)
  (or (status feature loop) (sstatus feature loop))
#+For-NIL
  (set-feature 'loop 'local)

