


;--- msg - arg1 ...  arguments of the form described below
;	B - print out a blank
;	N - print out a newline (terpr)
;	(B n) - print out n blanks
;	(P p) - henceforth print on port p
;	atom - patom this exactly (no evaluation)
;	other - evaluate and patom this expression.
;
(def msg
  (macro (lis)
	 `(progn ,@(msgmake (cdr lis) 'nil))))

(eval-when (eval compile load)
  (def msgmake
       (lambda (forms outport)
	       ((lambda (thisform)
			
			(cond ((null forms) `((drain ,@outport)))
			      ((and (eq 'B thisform) (setq thisform '" ") nil))
			      ((eq 'N thisform) (cons `(terpr ,@outport)
						      (msgmake (cdr forms) outport)))
			      ((atom thisform) (cons `(patom ',thisform 
							     ,@outport)
						     (msgmake (cdr forms) outport)))
			      ((eq 'P (car thisform)) (msgmake (cdr forms)
							       `(,@(cdr thisform))))
			      
			      ((eq 'B (car thisform)) (cons `(printblanks ,@(cdr thisform)
									  ,outport)
							    (msgmake (cdr forms) outport)))
			      (t (cons `(patom ,thisform ,@outport)
				       (msgmake (cdr forms) outport)))))
		(car forms)))))

(def printblanks
  (lambda (n prt)
	  (do ((i n (sub1 i)))
	      ((lessp i 1))
	      (patom '" " prt))))




; ==============================================
;
;	(linelength [numb])
;
; sets the linelength (actually just varib linel) to the
; number given: numb
; if numb is not given, the current line length is returned
; =================================================

(setq linel 80)
(def linelength
     (nlambda (form)
	      (cond ((null form) linel )
		    ((numberp (car form)) (setq linel (car form)))
		    (t linel))))

; ========================================
;
;	(charcnt port) 
; returns the number of characters left on the current line
; on the given port
;
; =======================================


(def charcnt
     (lambda (port) (diff linel (nwritn port))))

(def nthcdr
 (lambda (n x)
  (cond ((equal n 0) x)
        ((lessp n 0) (cons nil x))
        (t (nthcdr (sub1 n) (cdr x) )))))

;r	lambda: (nthrest numb list)
;-	args:	numb - integer
;-		list - list
;-	returns:the rest of the list beginning at the numb'th element.
;-		for convience, (nthrest 0 list) equals (nthrest 1 list)
;-		equals list.  This is designed to be similar to nthelem
;-		which returns the nth element of a list.

(def nthrest
  (lambda (number list)
	  (cond ((lessp number 2)  list)
		(t (nthrest (sub1 number) (cdr list))))))


;;==============================
;  (assqr val alist)
; acts much like assq, it looks for val in the cdr of elements of
; the alist and returns the element if found.
; fix this when the compiler works
(eval-when nil (def assqr 
    (lambda (val alist)
	(do ((al alist (cdr al)))
	    ((null al) nil)
	    (cond ((eq val (cdar al)) (return (car al))))))))


; ====================
; (listp 'x) is t if x is a non-atom or nil
; ====================
(def listp (lambda (val) (or (dtpr val) (null val))))



;--- memcar - VAL : lispval
;	    - LIS : list
;	returns t if VAL found as the car of a top level element.
;temporarily turn this off till the compiler can handle it.
(eval-when nil (def memcar 
  (lambda (a l)
	  (do ((ll l (cdr ll)))
	      ((null ll) nil)
	      (cond ((equal (caar ll) a) (return (cdar ll))))))))

; =================================
;
;	(memcdr 'val 'listl)
;
; the list listl is searched for a list
; with cdr equal to val. if found, the
; car of that list is returned.
; ==================================
;fix this when compiler works ok
(eval-when nil (def memcdr 
  (lambda (a l)
	  (do ((ll l (cdr ll)))
	      ((null ll) nil)
	      (cond ((equal (cdar ll) a) (return (caar l))))))))


(def apply* 
  (nlambda ($x$)
	(eval (cons (eval (car $x$)) (cdr $x$)))))





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
(declare (special $outport$ $fileopen$ ))

; printret is like print yet it returns the value printed, this is used
; by pp
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

			   ((equal 'A (car $cur$))	; declaring atomness
			    (setq $atm$ t)
			    (setq $cur$ (cadr $cur$))
			    (go midstuff))

			   ((eq 'V (car $cur$))		; print value only
			    (setq $atm$ 'value)
			    (setq $cur$ (cadr $cur$))
			    (go midstuff))

			   (t (msg N "bad arg to pp: " (or $cur$))))
		     (go botloop)))
 midstuff     ; process the atom or function
	      
	      (cond ((eq 'value $atm$)
		     (setq $prl$ (eval $cur$)))

		    ((or $atm$ (null (getd $cur$)))	; check if is atom
		     (cond ((boundp $cur$)		; yes, see if bound
			    (setq $prl$ (list 'setq $cur$ (list 'quote 
								(eval $cur$)))))
			   (t (msg N "pp: atom " (or $cur$) " is unbound")
			      (go botloop))))

		    ((bcdp (getd $cur$))		; is a fcn, see if bcd
		     (msg N "pp: function " (or $cur$) " is machine coded (bcd) ")
		     (go botloop))

		    (t (setq $prl$ (list 'def $cur$ (getd $cur$)))))

	      ; now print it

	      ($prpr $prl$)
	      (terpr $outport$)
	      (setq $atm$ nil)				; clear flag

 botloop      (setq $xlist$ (cdr $xlist$))

	      (go toploop))))



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


(declare (special m))

(def $prdf 
  (lambda (l n m)
	  (prog ()
		($tocolumn n)
	   a    (cond ((or (atom l)
			   (lessp (add m (flatsize l (chrct $outport$)))
				  (chrct $outport$)))
		       (return (printret l $outport$)))
		      ((and ($patom1 lpar)
			    (lessp 2 (length l))
			    (atom (car l)))
		       (prog (c f g h)
			     (setq g
				   (cond ((member (car l) '(lambda nlambda))
					  -7)
					 (t
					  0)))
			     (setq f (equal (printret (car l) $outport$) 'prog))
			     ($patom1 ' " ")
			     (setq c ($dinc))
			   a ($prd1
			      (cdr l)
			      (add
			       c
			       (cond ((setq h (and f
						   (cadr l)
						   (atom (cadr l))))
				      -5)
				     (t g))))
			     (cond ((cdr (setq l (cdr l)))
				    (cond ((or (null h) (atom (cadr l)))
					   (terpr $outport$)))
				    (go a)))))
		      ((prog (c)
			     (setq c ($dinc))
			 a   ($prd1 l c)
			     (cond ((setq l (cdr l))
				    (terpr $outport$)
				    (go a))))))
	  b	($patom1 rpar))))



(def $prd1 
  (lambda (l n)
	  (prog ()
		($prdf (car l)
		       n
		       (cond ((null (setq l (cdr l))) (add m 1))
			     ((atom l) (setq n nil) (plus 4 m (pntlen l)))
			     (t m)))
		(cond ((null n)
		       ($patom1 ' " . ")
		       (return (printret l $outport$)))))))





(def $dinc (lambda () (diff (linelength $outport$) (chrct $outport$))))


(def $tocolumn
  (lambda (n)
	  (cond ((greaterp (setq n (diff n (nwritn $outport$))) 0)
		 (do ((i 0 (add1 i)))
		     ((equal i n))
		     (patom '" " $outport$))))))

; ========================================
;
;	(charcnt port) 
; returns the number of characters left on the current line
; on the given port
;
; =======================================


(def charcnt
     (lambda (port) (diff linel (nwritn port))))

(putd 'chrct (getd 'charcnt))

(def $patom1 (lambda (x) (patom x $outport$)))
