(declare (special poport pparm1 pparm2 lpar rpar form linel))
; =======================================
; pretty printer top level routine pp
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
;
;--------  -DNC : This is altered to fit better with CMULisp as follows -----
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

(declare (special $outport$ $fileopen$ prettyprops))

(setq prettyprops '((comment . pp-comment)
		    (function . pp-function)
		    (value . pp-value)))

; printret is like print yet it returns the value printed, this is used
; by pp		- DNC : That's what print should have done all along!!!
(def printret
  (macro ($l$)
	 `(progn (print ,@(cdr $l$)) ,(cadr $l$))))

(def pp
  (nlambda ($xlist$)
	(prog ($outport$ $cur$ $fileopen$ $prl$ $atm$)

	      (setq $outport$ poport)			; default port
	      ; check if more to do, if not close output file if it is
	      ; open and leave


   toploop    (cond ((null (setq $cur$ (car $xlist$)))
		     (condclosefile)
		     (return t)))

	      (cond ((dtpr $cur$)
		     (cond ((equal 'P (car $cur$))	; specifying a port
			    (condclosefile)		; close file if open
			    (setq $outport$ (eval (cadr $cur$))))

			   ((equal 'F (car $cur$))	; specifying a file
			    (condclosefile)		; close file if open
			    (setq $outport$ (outfile (cadr $cur$))
				  $fileopen$ t))

							; -DNC : inserted
			   ((equal 'E (car $cur$))
			    (eval (cadr $cur$)))

; -DNC :see doc above	   ((equal 'A (car $cur$))	; declaring atomness
;			    (setq $atm$ t)
;			    (setq $cur$ (cadr $cur$))
;			    (go midstuff))
;
;			   ((eq 'V (car $cur$))		; print value only
;			    (setq $atm$ 'value)
;			    (setq $cur$ (cadr $cur$))
;			    (go midstuff))
;
;			   (t (msg N "bad arg to pp: " (or $cur$))))
			   (t (terpri $outport$)
			      ($prpr $cur$)))	;-DNC inserted
		     (go botloop)))
; midstuff     ; process the atom or function
	      
;	      (cond ((eq 'value $atm$)
;		     (setq $prl$ (eval $cur$)))
;
;		    ((or $atm$ (null (getd $cur$)))	; check if is atom
;		     (cond ((boundp $cur$)		; yes, see if bound
;			    (setq $prl$ (list 'setq $cur$ (list 'quote 
;								(eval $cur$)))))
;			   (t (msg N "pp: atom " (or $cur$) " is unbound")
;			      (go botloop))))
;
;		    ((bcdp (getd $cur$))		; is a fcn, see if bcd
;		     (msg N "pp: function " (or $cur$) " is machine coded (bcd) ")
;		     (go botloop))
;
;		    (t (setq $prl$ (list 'def $cur$ (getd $cur$)))))
;
;	      ; now print it
;
;	      ($prpr $prl$)
;	      (terpr $outport$)
;	      (setq $atm$ nil)				; clear flag
;	------ That was the old code, here comes the new -DNC  -------

      (mapc (function (lambda(prop)
	      (prog (printer)
		(cond ((dtpr prop)
		       (setq printer (cdr prop))
		       (setq prop (car prop)))
		      (t (setq printer 'pp-prop)))
		(cond ((eq 'value prop)
		       (cond ((boundp $cur$)
			      (apply printer 
				     (list $cur$ (eval $cur$) 'value)))))
		      ((eq 'function prop)
		       (cond ((and (getd $cur$) (not (bcdp (getd $cur$))))
			      (apply printer
				     (list $cur$ (getd $cur$) 'function)))))
		      ((get $cur$ prop)
		       (apply printer (list $cur$ (get $cur$ prop) prop)))))))
	 prettyprops)


 botloop      (setq $xlist$ (cdr $xlist$))

	      (go toploop))))

(setq pparm1 50 pparm2 100)

;   -DNC These "prettyprinter parameters" are used to decide when we should
;	quit printing down the right margin and move back to the left -
;	Do it when the leftmargin > pparm1 and there are more than pparm2
;	more chars to print in the expression

(def pp-value (lambda (i v p)
   (terpri $outport$) ($prpr (list 'dv i v))))
(def pp-function (lambda (i v p)
   (terpri $outport$) ($prpr (list 'def i v))))
(def pp-prop (lambda (i v p) 
   (terpri $outport$) ($prpr (list 'defprop i v p))))

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


(def $prpr 
  (lambda (x)
	  (cond ((not (boundp '$outport$)) (setq $outport$ poport)))
	  (terpr $outport$)
	  ($prdf x 0 0)))


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
	  (cond ((and (> lmar pparm1) (> (flatc l (add1 pparm2)) pparm2))
		 (terpri $outport$)
		 (princ '"; <<<<< start back on the left <<<<<" $outport$)
		 ($prdf l 5 0)
		 (terpri $outport$)
		 (princ '"; >>>>> continue on the right >>>>>" $outport$)
		 (terpri $outport$)
		 (return nil)))
          (tab lmar $outport$)
     a    (cond ((and (dtpr l)
                      (atom (car l))
                      (get (car l) 'printmacro)
                      (apply (get (car l) 'printmacro)
                             (list l lmar rmar)))
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
                ((lessp (add rmar (flatc l (chrct $outport$)))
                        (chrct $outport$))
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
          (for-each x
                    (cddr l)
                    (cond ((atom x) (tab col $outport$) (print x $outport$))
                          (t ($prdf x (+ lmar 6) rmar))))
          (princ '|)| $outport$)
          (return t))))

(putprop 'prog 'printprog 'printmacro)

; Here's the printmacro for def.  The original $prdf had some special code
; for lambda and nlambda.

(def printdef
  (lambda (l lmar rmar)
    (cond ((and (zerop lmar)		; only if we're really printing a defn
                (zerop rmar)
                (cadr l)
                (atom (cadr l))
                (caddr l)
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
           (for-each x (cddaddr l) ($prdf x 4 0))
           (princ '|))| $outport$)
           t))))

(putprop 'def 'printdef 'printmacro)

; There's a version of this hacked into the printer (where it don't belong!)
; Note that it must NOT apply to things like (quote a b).

(def printquote
  (lambda (l lmar rmar)
    (cond ((or (null (cdr l)) (cddr l)) nil)
          (t (princ '|'| $outport$) 
             ($prdf (cadr l) (add1 lmar) rmar)
             t))))

(putprop 'quote 'printquote 'printmacro)

; This one is nuked till I have time to fix it... LWE 3/8/81
(def print***
  (lambda (l lmar rmar)
    (cond ((and (null (cdr (last l))) (forall x l (atom x)))
	   (patom '|(| $outport$)
	   (print (car l) $outport$)
	   (for-each x (cdr l)
		(cond ((< (chrct $outport$) (+ (flatc x 20) rmar)) 
		       (tab lmar $outport$)))
		(patom '| | $outport$)
		(print x $outport$))
	   (patom '|)| $outport$)
	   t))))

; (putprop '*** 'print*** 'printmacro)		LWE Nuked because of macro
; (putprop 'comment 'print*** 'printmacro)          bugs 3/8/81.  Will fix.

(def printaccross
  (lambda (l lmar rmar)
    (prog nil
          (*** this is needed to make sure the printmacros are executed)
          (princ '|(| $outport$)
     l:   (cond ((null l))
                ((atom l) (princ '|. | $outport$) (princ l $outport$))
                (t ($prdf (car l) (nwritn $outport$) rmar)
                   (setq l (cdr l))
                   (cond (l (princ '| | $outport$)))
                   (go l:))))))


;(def $dinc (lambda () (- (linelength $outport$) (chrct $outport$))))
;
; -DNC : This was wrong and stupid - it's replaced by (nwritn $outport)

;$tocolumn is replaced by tab
;(def $tocolumn
;  (lambda (n)
;	  (cond ((greaterp (setq n (- n (nwritn $outport$))) 0)
;		 (do ((i 0 (1+ i)))
;		     ((eq i n))
;		     (patom '" " $outport$))))))

(def tab (lexpr (n)
  (prog (nn prt) (setq nn (arg 1))
		(cond ((> n 1)(setq prt (arg 2))))
		(cond ((> (nwritn prt) nn) (terpri prt)))
		(printblanks (- nn (nwritn prt)) prt))))

; ========================================
;
;	(charcnt port) 
; returns the number of characters left on the current line
; on the given port
;
; =======================================


(def charcnt
     (lambda (port) (- linel (nwritn port))))

(putd 'chrct (getd 'charcnt))

(def $patom1 (lambda (x) (patom x $outport$)))
