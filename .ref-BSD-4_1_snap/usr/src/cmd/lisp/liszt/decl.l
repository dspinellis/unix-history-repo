(include "chead.l")
(Liszt-file decl "@(#)decl.l	1.9	3/15/82")

;;; ----	d e c l		declaration handling
;;;
;;;				-[Wed May 12 13:45:33 1982 by jkf]-



(setq original-readtable readtable)
(setq raw-readtable (makereadtable t))

;--- special handlers
(putprop 'and	'cc-and		'fl-exprcc)
(putprop 'arg  	'cc-arg		'fl-exprcc)
(putprop 'assq 	'cm-assq	'fl-exprm)
(putprop 'atom 	'cc-atom	'fl-exprcc)
(putprop 'bigp  'cc-bigp	'fl-exprcc)
(putprop 'bcdcall 'c-bcdcall	'fl-expr)
(putprop 'Internal-bcdcall 'c-Internal-bcdcall 'fl-expr)
(putprop 'bcdp	'cc-bcdp	'fl-exprcc)
(putprop 'boole 'c-boole	'fl-expr)
(putprop '*catch 'c-*catch 	'fl-expr)
(putprop 'comment 'cc-ignore	'fl-exprcc)
(putprop 'cond	'c-cond  	'fl-expr)
(putprop 'cons	'c-cons		'fl-expr)
(putprop 'cxr 	'cc-cxr		'fl-exprcc)
(putprop 'declare 'c-declare	'fl-expr)
(putprop 'do	'c-do     	'fl-expr)
(putprop 'liszt-internal-do 	'c-do     	'fl-expr)
(putprop 'dtpr	'cc-dtpr	'fl-exprcc)
(putprop 'eq	'cc-eq    	'fl-exprcc)
(putprop 'equal 'cc-equal	'fl-exprcc)
(putprop 'errset 'c-errset 	'fl-expr)
(putprop 'fixp	 'cc-fixp	'fl-exprcc)
(putprop 'floatp 'cc-floatp	'fl-exprcc)
(putprop 'funcall 'c-funcall 	'fl-expr)
(putprop 'function 'cc-function 'fl-exprcc)
(putprop 'get	 'c-get		'fl-expr)
(putprop 'getd 	'cm-getd 	'fl-exprm)
(putprop 'getdata 'cm-getdata 	'fl-exprm)
(putprop 'getdisc 'cm-getdisc 	'fl-exprm)
(putprop 'go	'c-go	  	'fl-expr)
(putprop 'list	'c-list	  	'fl-expr)
(putprop 'map	'cm-map	  	'fl-exprm)
(putprop 'mapc	'cm-mapc  	'fl-exprm)
(putprop 'mapcan 'cm-mapcan 	'fl-exprm)
(putprop 'mapcar 'cm-mapcar 	'fl-exprm)
(putprop 'mapcon 'cm-mapcon 	'fl-exprm)
(putprop 'maplist 'cm-maplist 	'fl-exprm)
(putprop 'memq	  'cc-memq	'fl-exprcc)
(putprop 'ncons	'cm-ncons	'fl-exprm)
(putprop 'not	'cc-not   	'fl-exprcc)
(putprop 'null	'cc-not   	'fl-exprcc)
(putprop 'numberp 'cc-numberp	'fl-exprcc)
(putprop 'or	'cc-or    	'fl-exprcc)
(putprop 'prog	'c-prog   	'fl-expr)
(putprop 'progn	'cm-progn 	'fl-exprm)
(putprop 'prog1 'cm-prog1	'fl-exprm)
(putprop 'prog2	'cm-prog2 	'fl-exprm)
(putprop 'progv 'c-progv	'fl-expr)
(putprop 'quote	'cc-quote 	'fl-exprcc)
(putprop 'return 'c-return 	'fl-expr)
(putprop 'rplaca 'c-rplaca 	'fl-expr)
(putprop 'rplacd 'c-rplacd 	'fl-expr)
(putprop 'rplacx 'c-rplacx 	'fl-expr)
(putprop '*rplacx 'c-rplacx 	'fl-expr)
(putprop 'setarg 'c-setarg	'fl-expr)
(putprop 'setq	'cc-setq  	'fl-exprcc)
(putprop 'stringp 'cc-stringp 	'fl-exprcc)
(putprop 'symbolp 'cc-symbolp	'fl-exprcc)
(putprop 'symeval 'cm-symeval	'fl-exprm)
(putprop '*throw 'c-*throw 	'fl-expr)
(putprop 'typep   'cc-typep	'fl-exprcc)
(putprop 'zerop   'cm-zerop	'fl-exprm)

; functions which expect fixnum operands 


(putprop '+ 'c-fixnumop  'fl-expr)
(putprop '+ 'addl3 'fixop)

(putprop '- 'c-fixnumop 'fl-expr)
(putprop '- 'subl3 'fixop)

(putprop '* 'c-fixnumop 'fl-expr)
(putprop '* 'mull3 'fixop)

(putprop '/ 'c-fixnumop 'fl-expr)
(putprop '/ 'divl3 'fixop)

;-- boole's derivatives
(putprop 'fixnum-BitOr 'c-fixnumop 'fl-expr)
(putprop 'fixnum-BitOr 'bisl3 'fixop)

(putprop 'fixnum-BitAndNot 'c-fixnumop 'fl-expr)
(putprop 'fixnum-BitAndNot 'bicl3 'fixop)

(putprop 'fixnum-BitXor 'c-fixnumop 'fl-expr)
(putprop 'fixnum-BitXor 'xorl3 'fixop)

(putprop '1+ 'cc-oneplus 'fl-exprcc)
(putprop '1- 'cc-oneminus 'fl-exprcc)

(putprop '\\	'c-\\	'fl-expr)   ; done in the old way, should be modified

; these have typically fixnum operands, but not always 


; these without the & can be both fixnum or both flonum
;
(putprop 	'< 	'cm-< 	'fl-exprm)
(putprop 	'<& 	'cc-<& 	'fl-exprcc)

(putprop 	'> 	'cm-> 	'fl-exprm)
(putprop 	'>& 	'cc->& 	'fl-exprcc)

(putprop 	'= 	'cm-=		'fl-exprm)
(putprop 	'=&	'cm-=&		'fl-exprm)

; functions which can only be compiled
(putprop 'assembler-code 'c-assembler-code 'fl-expr)
(putprop 'fixnum-cxr 'cm-fixnum-cxr 'fl-exprm)
(putprop 'internal-fixnum-box 'c-internal-fixnum-box 'fl-expr)
(putprop 'offset-cxr 'cc-offset-cxr 'fl-exprcc)
(putprop 'internal-bind-vars 'c-internal-bind-vars 'fl-expr)
(putprop 'internal-unbind-vars 'c-internal-unbind-vars 'fl-expr)

; functions which can be converted to fixnum functions if
; proper declarations are done
(mapc
   '(lambda (arg) (putprop (car arg) (cdr arg) 'if-fixnum-args))
   '((lessp . <&) (greaterp . >&) (= . =&) (equal . =&)
     (plus . +)  (difference . -) (times . *) (quotient . /)
     (add1 . 1+) (sub1 . 1-)))

;--- doevalwhen, process evalwhen directive. This is inadequate.
;
(def doevalwhen 
      (lambda (v-f)
	      (prog (docom dolod)
		    (setq docom (memq 'compile (cadr v-f))
			  
			  dolod (memq 'load (cadr v-f)))
		    (mapc '(lambda (frm) (cond (docom (eval frm)))
					 (cond (dolod 
						((lambda (internal-macros) 
							 (liszt-form frm))
						 t))))
			  (cddr v-f)))))


;---- declare - the compiler version of the declare function
;	process the declare forms given. We evaluate each arg
;
(defun declare fexpr (forms)
   (cond ((status feature complr)
	  (do ((i forms (cdr i)))
	      ((null i))
	      (cond ((and (atom (caar i))
			  (getd (caar i)))
		     (eval (car i))) ; if this is a function
		    (t (comp-warn "Unknown declare attribute: " (car i))))))))

;---> handlers for declare forms
; declaration information for declarations which occur outside of
; functions is stored on the property list for rapid access.
; The indicator to look under is the value of one of the symbols:
;	g-functype, g-vartype, g-bindtype, or g-calltype
;  The value of the property is the declared function, declaration, binding
;	or call type for that variable.
; For local declarations, the information is kept on the g-decls stack.
; It is an assq list, the car of which is the name of the variable or
; function name, the cdr of which is the particular type.  To tell
; whether the particular type is a function type declaration, check the
; property list of the particular type for a 'functype' indicator.
; Likewise, to see if a particular type is a variable declaration, look
; for a 'vartype' indicator on the particular type's property list.
;
(defmacro declare-handler (args name type toplevind)
   `(mapc '(lambda (var)
	      (cond ((symbolp var)
		     (cond (g-compfcn	; if compiling a function
			      (Push g-decls (cons var ',name)))
			   (t          ; if at top level
			      (putprop var ',name ,toplevind))))))
	  ,args))

   
(defun *fexpr fexpr (args)
   (declare-handler args nlambda functype g-functype))

(defun nlambda fexpr (args)
   (declare-handler args nlambda functype g-functype))

(defun *expr fexpr (args)
   (declare-handler args lambda functype g-functype))

(defun lambda fexpr (args)
   (declare-handler args lambda functype g-functype))

(defun *lexpr fexpr (args)
   (declare-handler args lexpr functype g-functype))

(defun special fexpr (args)
   (declare-handler args special bindtype g-bindtype))

(defun unspecial fexpr (args)
   (declare-handler args unspecial bindtype g-bindtype))

(defun fixnum fexpr (args)
   (declare-handler args fixnum vartype g-vartype))

(defun flonum fexpr (args)
   (declare-handler args flonum vartype g-vartype))

(defun notype fexpr (args)
   (declare-handler args notype vartype g-vartype))



;--- special case, this is only allowed at top level.  It will
; be removed when vectors are fully supported
(def macarray 
  (nlambda (v-l)
	   (mapc '(lambda (x)
			  (If (dtpr x)
			      then (putprop (car x) (cdr x) g-arrayspecs)
				   (putprop (car x) 'array  g-functype)
			      else (comp-err "Bad macerror form" x)))
		 v-l)))


(def macros 
  (nlambda (args) (setq macros (car args))))

(def specials
  (nlambda (args) (setq special (car args))))

;--- another top level only.
;
(def localf
  (nlambda (args) (mapc '(lambda (ar)
				 (If (null (get ar g-localf))
				     then (putprop ar 
						   (cons (d-genlab) -1)
						   g-localf)))
			args)))

; g-decls is a stack of forms like
;  ((foo . special) (bar . fixnum) (pp . nlambda))
; there are 4 types of cdr's:
;	function types (lambda, nlambda, lexpr)
;	variable types (fixnum, flonum, notype)
;	call types     (localf, <unspecified>)
;	bind types     (special, unspecial)
;
(mapc '(lambda (x) (putprop x t 'functype)) '(lambda nlambda lexpr))
(mapc '(lambda (x) (putprop x t 'vartype))  '(fixnum flonum notype))
(mapc '(lambda (x) (putprop x t 'calltype)) '(localf))
(mapc '(lambda (x) (putprop x t 'bindtype)) '(special unspecial))

;---> end declare form handlers






;--- d-makespec :: declare a variable to be special
;
(defun d-makespec (vrb)
  (putprop vrb 'special g-bindtype))
