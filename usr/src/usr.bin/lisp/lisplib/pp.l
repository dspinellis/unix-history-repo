(setq rcs-pp-
   "$Header: /usr/lib/lisp/RCS/pp.l,v 1.2 83/08/15 22:27:54 jkf Exp $")

;;
;; pp.l					-[Mon Aug 15 10:52:13 1983 by jkf]-
;;
;; pretty printer for franz lisp
;;

(declare (macros t))

(declare (special poport pparm1 pparm2 lpar rpar form linel))
; (declare (localf $patom1 $prd1 $prdf charcnt condclosefile))

; =======================================
; pretty printer top level routine pp
;
;
; calling form- (pp arg1 arg2 ... argn)
; the args may be names of functions, atoms with associated values
; or output descriptors.
; if argi is:
;    an atom - it is assumed to be a function name, if there is no
;	       function property associated with it,then it is assumed
;		to be an atom with a value
;    (P port)-  port is the output port where the results of the
;	        pretty printing will be sent.
;		poport is the default if no (P port) is given.
;    (F fname)- fname is  a file name to write the results in
;    (A atmname) - means, treat this as an atom with a value, dont
;		check if it is the name of a function.
;    (E exp)-   evaluate exp without printing anything
;    other -	pretty-print the expression as is - no longer an error
;
;    Also, rather than printing only a function defn or only a value, we will
;    let prettyprops decide which props to print.  Finally, prettyprops will
;    follow the CMULisp format where each element is either a property
;    or a dotted pair of the form (prop . fn) where in order to print the
;    given property we call (fn id val prop).  The special properties
;    function and value are used to denote those "properties" which
;    do not actually appear on the plist.
;
; [history of this code: originally came from Harvard Lisp, hacked to
; work under franz at ucb, hacked to work at cmu and finally rehacked
; to work without special cmu macros]

(declare (special $outport$ $fileopen$ prettyprops))

(setq prettyprops '((comment . pp-comment)
		    (function . pp-function)
		    (value . pp-value)))

; printret is like print yet it returns the value printed, this is used
; by pp		
(def printret
  (macro ($l$)
	 `(progn (print ,@(cdr $l$)) ,(cadr $l$))))

(def pp
  (nlambda ($xlist$)
	(prog ($gcprint $outport$ $cur$ $fileopen$ $prl$ $atm$)

	      (setq $gcprint nil)			; don't print
							; gc messages in pp.

	      (setq $outport$ poport)			; default port
	      ; check if more to do, if not close output file if it is
	      ; open and leave


   toploop    (cond ((null (setq $cur$ (car $xlist$)))
		     (condclosefile)
		     (terpr)
		     (return t)))

	      (cond ((dtpr $cur$)
		     (cond ((equal 'P (car $cur$))	; specifying a port
			    (condclosefile)		; close file if open
			    (setq $outport$ (eval (cadr $cur$))))

			   ((equal 'F (car $cur$))	; specifying a file
			    (condclosefile)		; close file if open
			    (setq $outport$ (outfile (cadr $cur$))
				  $fileopen$ t))

						
			   ((equal 'E (car $cur$))
			    (eval (cadr $cur$)))

			   (t (pp-form $cur$ $outport$)))	;-DNC inserted
		     (go botloop)))


      (mapc (function
	     (lambda (prop)
		     (prog (printer)
			   (cond ((dtpr prop)
				  (setq printer (cdr prop))
				  (setq prop (car prop)))
				 (t (setq printer 'pp-prop)))
			   (cond ((eq 'value prop)
				  (and (boundp $cur$)
				       (apply printer
					      (list $cur$
						    (eval $cur$)
						    'value))
				       (terpr $outport$)))
				 ((eq 'function prop)
				  (and (getd $cur$)
				       (cond ((not (bcdp (getd $cur$)))
					      (apply printer
						     (list $cur$
							   (getd $cur$)
							   'function)))
					     ; restore message about
					     ; bcd since otherwise you
					     ; just get nothing and
					     ; people were complaining.
					     ; - dhl.
					     #-cmu
					     (t
					      (msg N 
						   "pp: function " 
						   (or $cur$)
						   " is machine coded (bcd) "))
					     )
				       (terpri $outport$)))
				 ((get $cur$ prop)
				  (apply printer
					 (list $cur$
					       (get $cur$ prop)
					       prop))
				  (terpri $outport$))))))
	    prettyprops)


 botloop      (setq $xlist$ (cdr $xlist$))

	      (go toploop))))

(setq pparm1 50 pparm2 100)

;   -DNC These "prettyprinter parameters" are used to decide when we should
;	quit printing down the right margin and move back to the left -
;	Do it when the leftmargin > pparm1 and there are more than pparm2
;	more chars to print in the expression

; cmu prefers dv instead of setq

#+cmu
(def pp-value (lambda (i v p)
		      (terpri $outport$)
		      (pp-form (list 'dv i v) $outport$)))

#-cmu
(def pp-value (lambda (i v p)
		      ;;(terpr $outport$) ;; pp-form does an initial terpr.
		      ;;			we don't need two.
		      (pp-form `(setq ,i ',v) $outport$)))

(def pp-function (lambda (i v p)
			 #+cmu (terpri $outport$)
			 ;;
			 ;; add test for traced functions and don't
			 ;; print the trace mess, just the original
			 ;; function.  - dhl.
			 ;;
			 ;; this test might belong in the main pp
			 ;; loop but fits in easily here. - dhl
			 ;;
			 (cond ((and (dtpr v)
				     (dtpr (cadr v))
				     (memq (caadr v)
					   '(T-nargs T-arglist))
				     (cond ((bcdp (get i 'trace-orig-fcn))
					    #-cmu
					    (msg N 
						 "pp: function " 
						 (or i) 
						 " is machine coded (bcd) ")
					    t)
					   (t (pp-form 
					       (list 'def i 
						     (get i 'trace-orig-fcn))
					       $outport$)
					      t))))
			       ; this function need to return t, but
			       ; pp-form returns nil sometimes.
			       (t (pp-form (list 'def i v) $outport$)
				  t))))

(def pp-prop (lambda (i v p)
		     #+cmu (terpri $outport$)
		     (pp-form (list 'defprop i v p) $outport$)))

(def condclosefile 
  (lambda nil
	  (cond ($fileopen$
		 (terpr $outport$)
		 (close $outport$)
		 (setq $fileopen$ nil)))))

;
; these routines are meant to be used by pp but since
; some people insist on using them we will set $outport$ to nil
; as the default
(setq $outport$ nil)



(defun pp-form (value &optional ($outport$ poport oport-p) (lmar 0))
 ($prdf value lmar 0))

; this is for compatability with old code, will remove soon -- jkf
(def $prpr (lambda (x) (pp-form x $outport$)))



(declare (special rmar))	; -DNC this used to be m - I've tried to
				; to fix up the pretty printer a bit.  It
				; used to mess up regularly on (a b .c) types
				; of lists.  Also printmacros have been added.

(def $prdf
  (lambda (l lmar rmar)
    (prog nil
;
;			- DNC - Here we try to fix the tendency to print a
;			  thin column down the right margin by allowing it
;			  to move back to the left if necessary.
;
	  (cond ((and (>& lmar pparm1) (>& (flatc l (1+ pparm2)) pparm2))
		 (terpri $outport$)
		 (patom "; <<<<< start back on the left <<<<<" $outport$)
		 ($prdf l 5 0)
		 (terpri $outport$)
		 (patom "; >>>>> continue on the right >>>>>" $outport$)
		 (terpri $outport$)
		 (return nil)))
          (tab lmar $outport$)
     a    (cond ((and (dtpr l)
		      (atom (car l))
		      (or (and (get (car l) 'printmacro)
			       (funcall (get (car l) 'printmacro)
					l lmar rmar))
			  (and (get (car l) 'printmacrochar)
			       (printmacrochar (get (car l) 'printmacrochar)
					       l lmar rmar))))
		 (return nil))
;
;				-DNC - a printmacro is a lambda (l lmar rmar)
;				attached to the atom.  If it returns nil then
;				we assume it did not apply and we continue.
;				Otherwise we assume it did the job.
;
                ((or (not (dtpr l))
;                    (*** at the moment we just punt hunks etc)
                     (and (atom (car l)) (atom (cdr l))))
                 (return (printret l $outport$)))
                ((<& (+ rmar (flatc l (charcnt $outport$)))
		    (charcnt $outport$))
		 ;
		 ;	This is just a heuristic - if print can fit it in then figure that
;	the printmacros won't hurt.  Note that despite the pretentions there
;	is no guarantee that everything will fit in before rmar - for example
;	atoms (and now even hunks) are just blindly printed.	- DNC
;
                 (printaccross l lmar rmar))
                ((and ($patom1 lpar)
                      (atom (car l))
                      (not (atom (cdr l)))
                      (not (atom (cddr l))))
                 (prog (c)
                       (printret (car l) $outport$)
                       ($patom1 '" ")
                       (setq c (nwritn $outport$))
                  a    ($prd1 (cdr l) c)
                       (cond
                        ((not (atom (cdr (setq l (cdr l)))))
                         (terpr $outport$)
                         (go a)))))
                (t
                 (prog (c)
                       (setq c (nwritn $outport$))
                  a    ($prd1 l c)
                       (cond
                        ((not (atom (setq l (cdr l))))
                         (terpr $outport$)
                         (go a))))))
     b    ($patom1 rpar))))

(def $prd1
  (lambda (l n)
    (prog nil
          ($prdf (car l)
                 n
                 (cond ((null (setq l (cdr l))) (|1+| rmar))
                       ((atom l) (setq n nil) (plus 4 rmar (pntlen l)))
                       (t rmar)))
          (cond
           ((null n) ($patom1 '" . ") (return (printret l $outport$))))
;         (*** setting n is pretty disgusting)
;         (*** the last arg to $prdf is the space needed for the suffix)
;		;Note that this is still not really right - if the prefix
;		takes several lines one would like to use the old rmar 
;		until the last line where the " . mumble)" goes.
	)))

; -DNC here's the printmacro for progs - it replaces some hackery that
; used to be in the guts of $prdf.

(def printprog
  (lambda (l lmar rmar)
    (prog (col)
          (cond ((cdr (last l)) (return nil)))
          (setq col (add1 lmar))
          (princ '|(| $outport$)
          (princ (car l) $outport$)
          (princ '| | $outport$)
          (print (cadr l) $outport$)
          (mapc '(lambda (x)
			 (cond ((atom x)
				(tab col $outport$)
				(print x $outport$))
                          (t ($prdf x (+ lmar 6) rmar))))
		(cddr l))
          (princ '|)| $outport$)
          (return t))))

(putprop 'prog 'printprog 'printmacro)

;;
;;	simpler version which
;;	should look nice for lambda's also.(inside mapcar's) -dhl
;;
(defun print-lambda (l lmar rmar)
  (prog (col)
	(cond ((cdr (last l)) (return nil)))
	(setq col (add1 lmar))
	(princ '|(| $outport$)
	       (princ (car l) $outport$)
	       (princ '| | $outport$)
	       (print (cadr l) $outport$)
	       (let ((c (cond ((eq (car l) 'lambda)
			       8)
			      (t 9))))
		    (mapc '(lambda (x)
				   ($prdf x (+ lmar c) rmar))
			  (cddr l)))
	       (princ '|)| $outport$)
	(terpr $outport$)
	(tab lmar $outport$)
	(return t)))

(putprop 'lambda 'print-lambda 'printmacro)
(putprop 'nlambda 'print-lambda 'printmacro)

; Here's the printmacro for def.  The original $prdf had some special code
; for lambda and nlambda.

(def printdef
  (lambda (l lmar rmar)
    (cond ((and (zerop lmar)		; only if we're really printing a defn
                (zerop rmar)
                (cadr l)
                (atom (cadr l))
                (dtpr (caddr l))
                (null (cdddr l))
                (memq (caaddr l) '(lambda nlambda macro lexpr))
                (null (cdr (last (caddr l)))))
           (princ '|(| $outport$)
           (princ 'def $outport$)
           (princ '| | $outport$)
           (princ (cadr l) $outport$)
           (terpri $outport$)
           (princ '|  (| $outport$)
           (princ (caaddr l) $outport$)
           (princ '| | $outport$)
           (princ (cadaddr l) $outport$)
           (terpri $outport$)
           (mapc  '(lambda (x) ($prdf x 4 0)) (cddaddr l))
           (princ '|))| $outport$)
           t))))

(putprop 'def 'printdef 'printmacro)

; There's a version of this hacked into the printer (where it don't belong!)
; Note that it must NOT apply to things like (quote a b).

;
; adding printmacrochar so that it can be used by other read macros
; which create things of the form (tag lisp-expr) like quote does,
; I know this is restrictive but it is helpful in the frl source. - dhl.
;
;

(def printmacrochar
  (lambda (macrochar l lmar rmar)
    (cond ((or (null (cdr l)) (cddr l)) nil)
          (t (princ macrochar $outport$) 
             ($prdf (cadr l) (add1 lmar) rmar)
             t))))

(putprop 'quote '|'| 'printmacrochar)

(def printaccross
  (lambda (l lmar rmar)
    (prog nil
;         (*** this is needed to make sure the printmacros are executed)
          (princ '|(| $outport$)
     l:   (cond ((null l))
                ((atom l) (princ '|. | $outport$) (princ l $outport$))
                (t ($prdf (car l) (nwritn $outport$) rmar)
                   (setq l (cdr l))
                   (cond (l (princ '| | $outport$)))
                   (go l:))))))

