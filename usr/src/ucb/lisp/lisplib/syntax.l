(setq rcs-syntax-
   "$Header: /usr/lib/lisp/syntax.l,v 1.1 83/01/29 18:40:24 jkf Exp $")

;;
;; syntax.l				-[Sat Jan 29 18:28:58 1983 by jkf]-
;;
;; contains the user callable setsyntax function
;;


;--- setsyntax :: new version of setsyntax
;  this version allows symbolic syntax codes.
;
(declare
   (special syntax:symbolic-to-old-fixnum ;; for upward compatibility
	    				; use this to map from old
					; fixnums to symbolic names
	    syntax:symbolic-bits-to-fixnum ;; bit definitions of symbolic
	    				   ;bits.  see h/chars.h
	    syntax:code-to-bits  	;; used at runtime to
	    				; interpret symbolic names
	    readtable			;; current readtable
   ))


(def setsyntax
   (lexpr (n)
	  (cond ((not (or (equal n 2) (equal n 3)))
		 (error "setsyntax: 2 or 3 args required, not " n)))
	  ; determine the correct code
	  (prog (given ch number)
	     (setq given (arg 2)
		   ch    (arg 1))
	     (cond ((and (not (numberp ch))
			 (not (symbolp ch)))
		    (error "setsyntax: first arg must be a number or symbol: "
			   ch)))
	     (cond ((numberp given)
		    ; using the old fixnum values (we suppose)
		    (cond ((setq number
				 (rassq given syntax:symbolic-to-old-fixnum))
			   (setq given (car number))) ; use symbolic name
			  (t (error "setsyntax: fixnum code is not defined: "
				    given)))))
	     (cond ((symbolp given)
		    ; convert from common names to our symbolic names
		    (cond ((eq 'macro given)
			   (setq given 'vmacro))
			  ((eq 'splicing given)
			   (setq given 'vsplicing-macro)))
		    ; now see if the symbolic name is defined
		    (cond ((setq number (assq given syntax:code-to-bits))
			   (setq number (cdr number)))
			  (t (error "setsyntax: unknown symbolic code: "
				    given))))
		   (t (error "setsyntax: second arg not symbol or fixnum: "
			     given)))
	     ; now call the low level code to set the value.
	     (int:setsyntax (arg 1) number)		;;; change to *
	     ; the final argument is placed on the property list of the
	     ; first argument, with the indicator being the current readtable,
	     ; thus you can have more than one macro function for each
	     ; character for each readtable.
	     (cond ((equal n 3)
		    (cond ((numberp ch) (setq ch (ascii ch))))   ; need symbol
		    (putprop ch (arg 3) readtable))))
   t))
	     

(def getsyntax
   (lambda (ch)
      (let ((res (int:getsyntax ch))   ; this will be modified too
	    (symb))
	 (cond ((setq symb (rassq res syntax:code-to-bits))
		(car symb))
	       (t (error "getsyntax: no symbolic code corresponds to: "
			 res))))))


;--- add-syntax-class : add a new symbolic syntax class
; name is the name which we will use to refer to it.
; bits are a list of symbolic bit names for it.
; modifies global variable: syntax:code-to-bits
;
(def add-syntax-class
   (lambda (name bits)
      (cond ((not (symbolp name))
	     (error "add-syntax-class: illegal name: " name)))
      (cond ((not (dtpr bits))
	     (error "add-syntax-class: illegal bits: " bits)))
      (do ((xx bits (cdr xx))
	   (this 0)
	   (num 0))
	  ((null xx)
	   (cond ((setq this (assq name syntax:code-to-bits))
		  (rplacd this num))	; replace old value
		 (t (setq syntax:code-to-bits (cons (cons name num)
						    syntax:code-to-bits)))))
	  (cond ((setq this (assq (car xx) syntax:symbolic-bits-to-fixnum))
		 ;(format t "num:~d, oth:~a, comb:~d~%"
			; num (cdr this) (apply 'boole `(7 ,num ,(cdr this))))
		 (setq num (boole 7 num (cdr this)))
		 ;(format t "res: ~d~%" num)
		 )   ; logical or
		(t (error "illegal syntax code " (car xx)))))
      name))

(setq syntax:symbolic-to-old-fixnum
       '((vnumber . 0) (vsign . 1) (vcharacter . 2)
	 (vsingle-character-symbol . 66.)
	 (vleft-paren . 195.) (vright-paren . 196.)
	 (vperiod . 133.)
	 (vleft-bracket . 198.) (vright-bracket . 199.) (veof . 200.)
	 (vsingle-quote . 201.) (vsymbol-delimiter . 138.)
	 (vstring-delimiter . 137.)
	 (villegal . 203.) (vseparator . 204.)
	 (vsplicing-macro . 205.) (vmacro . 206.)
	 (vescape . 143.))
   syntax:symbolic-bits-to-fixnum 
       '(; character classes
	   (cnumber . 0) (csign . 1) (ccharacter . 2)
	   (cleft-paren . 3)
	   (cright-paren . 4) (cperiod . 5) (cleft-bracket . 6)
	   (cright-bracket . 7)
	   (csingle-quote . 9.) (csymbol-delimiter . 10.) (cillegal . 11.)
	   (cseparator . 12.) (csplicing-macro . 13.)
	   (cmacro . 14.) (cescape . 15.) (csingle-character-symbol . 16.)
	   (cstring-delimiter . 17.)
	   (csingle-macro . 18.) (csingle-splicing-macro . 19.)
	   (cinfix-macro . 20.)
	   (csingle-infix-macro . 21.)
	  ; escape bits
	   (escape-when-unique . 64.)
	   (escape-when-first . 128.)
	   (escape-always . 192.)
	  ; separator
	   (separator . 32.))
   syntax:code-to-bits nil)
       
(add-syntax-class 'vnumber  	'(cnumber))
(add-syntax-class 'vsign 	'(csign))
(add-syntax-class 'vcharacter 	'(ccharacter))
(add-syntax-class 'vleft-paren 	'(cleft-paren escape-always separator))
(add-syntax-class 'vright-paren	'(cright-paren escape-always separator))
(add-syntax-class 'vperiod 	'(cperiod escape-when-unique))
(add-syntax-class 'vleft-bracket '(cleft-bracket escape-always separator))
(add-syntax-class 'vright-bracket '(cright-bracket escape-always separator))
(add-syntax-class 'vsingle-quote '(csingle-quote escape-always separator))
(add-syntax-class 'vsymbol-delimiter 	'(csymbol-delimiter escape-always))
(add-syntax-class 'villegal 	'(cillegal escape-always separator))
(add-syntax-class 'vseparator 	'(cseparator escape-always separator))
(add-syntax-class 'vsplicing-macro '(csplicing-macro escape-always separator))
(add-syntax-class 'vmacro 	'(cmacro escape-always separator))
(add-syntax-class 'vescape 	'(cescape escape-always))
(add-syntax-class 'vsingle-character-symbol
   		  '(csingle-character-symbol separator))
(add-syntax-class 'vstring-delimiter	'(cstring-delimiter escape-always))
(add-syntax-class 'vsingle-macro '(csingle-macro escape-when-unique))
(add-syntax-class 'vsingle-splicing-macro
     		 '(csingle-splicing-macro escape-when-unique))
(add-syntax-class 'vinfix-macro '(cinfix-macro escape-always separator))
(add-syntax-class 'vsingle-infix-macro
                  '(csingle-infix-macro escape-when-unique))


