(setq rcs-common1-
   "$Header: common1.l,v 1.9 84/01/06 14:21:46 sklower Exp $")

;;
;; common1.l				-[Sun Sep  4 14:04:15 1983 by jkf]-
;;
;;   common lisp functions.  These are the most common lisp functions
;; [which don't have to be defined in common0.l in order to support 
;;  the macros]
;;

(declare (macros t))		;; compile macros in this file

;--- Section 0 - variables
(declare (special Standard-Input Standard-Output Standard-Error
		  lisp-library-directory))

(or (boundp 'lisp-library-directory)
   (setq lisp-library-directory '/usr/lib/lisp))


;--- Section 0 - equivalences
; 
(defmacro make-equivalent (a b)
   `(progn (putd ',a (getd ',b))
	   (putprop ',a (get ',b 'fcn-info) 'fcn-info)))

(make-equivalent abs absval)
(make-equivalent add sum)
(make-equivalent bcdcall funcall)
(make-equivalent chrct charcnt)
(make-equivalent diff difference)
(make-equivalent numbp  numberp)
(make-equivalent remainder mod)
(make-equivalent terpri terpr)
(make-equivalent typep type)
(make-equivalent symeval eval)
(make-equivalent < lessp)
(make-equivalent <& lessp)	; fixnum version
(make-equivalent = equal)
(make-equivalent =& equal) 	; fixnum version
(make-equivalent > greaterp)
(make-equivalent >& greaterp) 	; fixnum version
(make-equivalent *dif difference)
(make-equivalent \\ mod) 
(make-equivalent \1+$ add1)
(make-equivalent \1-$ sub1)
(make-equivalent *$ times)
(make-equivalent /$ quotient)
(make-equivalent +$ add)
(make-equivalent -$ difference)

;--- Section I - functions and macros


;--- max - arg1 arg2 ... : sequence of numbe
;	returns the maximum
;
(def max
  (lexpr (nargs)
	 (do ((i nargs (1- i))
	      (max (arg 1)))
	     ((< i 2) max)
	     (cond ((greaterp (arg i) max) (setq max (arg i)))))))


;--- catch form [tag]  
;  catch is now a macro which translates to (*catch 'tag form)
;
(def catch
  (macro (l)
	 `(*catch ',(caddr l) ,(cadr l))))

;--- throw form [tag]
;  throw isnow a macro
;
(def throw
  (macro (l)
	 `(*throw ',(caddr l) ,(cadr l))))


      
;--- desetq
;	- pattern - pattern containing vrbl names
;	- expr 	  - expression to be evaluated
;
(defmacro desetq (&rest forms &aux newgen destrs)
  (do ((xx forms (cddr xx))
       (res)
       (patt)
       (expr))
      ((null xx) (cond ((null (cdr res)) (car res))
		       (t (cons 'progn (nreverse res)))))
      (setq patt (car xx) expr (cadr xx))
      (setq res 
	    (cons (cond ((atom patt) `(setq ,patt ,expr))	;trivial case
			(t (setq newgen (gensym)
				 destrs (de-compose patt '(r)))
			   `((lambda (,newgen)
				     ,@(mapcar '(lambda (frm)
							`(setq  ,(cdr frm) 
								(,(car frm) ,newgen)))
					       destrs)
				     ,newgen)
			     ,expr)))
		  res))))

;--- sassoc
;	- x : form
;	- y : assoc list
;	- fcn : function or lambda expression
; If (assoc x y) is non nil, then we apply the function fcn to nil.  
; This must be written as a macro if we expect to handle the case of
; a lambda expression as fcn in the compiler.  
;
(defmacro sassoc (x y fcn)
  (cond ((or (atom fcn) (not (eq 'quote (car fcn))))
	 `(or (assoc ,x ,y)
	      (funcall ,fcn)))
	(t `(or (assoc ,x ,y)
		(,(cadr fcn))))))

;--- sassq
;	- x : form
;	- y : assoc list
;	- fcn : function or lambda expression
; like sassoc above except it uses assq instead of assoc.
;
(defmacro sassq (x y fcn)
  (cond ((or (atom fcn) (not (eq 'quote (car fcn))))
	 `(or (assq ,x ,y)
	      (funcall ,fcn)))
	(t `(or (assq ,x ,y)
		(,(cadr fcn))))))

		    

;--- signp - test - unevaluated atom
;	   - value - evaluated value
; test can be l, le, e, n, ge or g   with the obvious meaning
; we return t if value compares to 0 by test

(defmacro signp (tst val)
  (setq tst  (cond ((eq 'l tst)  `(minusp signp-arg))
		   ((eq 'le tst) `(not (greaterp signp-arg 0)))
		   ((eq 'e tst)  `(zerop signp-arg))
		   ((eq 'n tst)  `(not (zerop signp-arg)))
		   ((eq 'ge tst) `(not (minusp signp-arg)))
		   ((eq 'g tst)  `(greaterp signp-arg 0))
		   (t (error "bad arg to signp " tst))))
  (cond ((atom val) `(and (numberp ,val) ,(subst val 'signp-arg tst)))
	(t `((lambda (signp-arg) (and (numberp signp-arg) ,tst))
	     ,val))))



;--- unwind-protect
;  The form of a call to unwind-protect is
;   (unwind-protect pform
;		    form1 form2 ...)
; and it works as follows:
;  pform is evaluated, if nothing unusual happens, form1 form2 etc are
;	then evaluated and unwind-protect returns the value of pform.
;  if while evaluating pform, a throw or error caught by an errset which
;   would cause control to pass through the unwind-protect, then
;   form1 form2 etc are evaluated and then the error or throw continues.
; Thus, no matter what happens, form1, form2 etc will be evaluated.
;
(defmacro unwind-protect (protected &rest conseq &aux (localv (gensym 'G)))
  `((lambda (,localv)
	    (setq ,localv (*catch 'ER%unwind-protect ,protected))
	    ,@conseq
	    (cond ((and (dtpr ,localv) (eq 'ER%unwind-protect (car ,localv)))
		   (I-throw-err (cdr ,localv)))
		  (t ,localv)))
    nil))


;----Section III -- Interrupt handlers 
; 

(def sys:fpeint-serv
   (lambda (x$) (error "Floating Exception ")))

(def sys:int-serv
   (lambda (dummy) (patom '"Interrupt:  ") (drain) (break)))


(signal 8 'sys:fpeint-serv)
(signal 2 'sys:int-serv)


;---- Section IV - interrupt handlers
;
(cond ((null (boundp '$gcprint))
       (setq $gcprint nil)))		; dont print gc stats by default

(cond ((null (boundp '$gccount$))
       (setq $gccount$ 0)))

;--- prtpagesused - [arg] : type of page allocated last time.
;	prints a summary of pages used for certain selected types
;	of pages.  If arg is given we put a star beside that type
;	of page.  This is normally called after a gc.
;
(def prtpagesused
  (lambda (space tottime gctime)
	  (patom "[")
	  (do ((curtypl (cond ((memq space '(list fixnum ))
			       '(list fixnum))
			      (t (cons space '(list fixnum))))
			(cdr curtypl))
	       (temp))
	      ((null curtypl) (print 'ut:)
	       (print (max 0 (quotient (times 100 (difference tottime gctime))
				       tottime)))
	       (patom "%]") (terpr))
	      (setq temp (car curtypl))
	      (cond ((greaterp (cadr (opval temp)) 0)
		     (cond ((eq space temp)
			    (patom '*)))
		     (patom temp)
		     (patom '":")
		     (print (cadr (opval temp)))
		     (patom '"{")
		     (print (fix (quotient 
				  (times 100.0
					 (car (opval temp)))
				  (* (cadr (opval temp))
					 (caddr (opval temp))))))
		     (patom '"%}")
		     (patom '"; "))))))

(declare (special gcafter-panic-mode $gccount$ $gc_midlim $gc_minalloc 
		  $gc_pct $gc_lowlim $gcprint ptimeatlastgc))

(setq gcafter-panic-mode nil)
(setq $gc_minalloc 10)
(setq $gc_lowlim 60)
(setq $gc_midlim 85)
(setq $gc_pct    .10)
(setq ptimeatlastgc (ptime))

;--- gcafter - [s] : type of item which ran out forcing garbage collection.
;	This is called after each gc.
; the form of an opval element is  (number_of_items_in_use
;				    number_of_pages_allocated
;				    number_of_items_per_page)
;
;
(def gcafter 
  (nlambda (s)
	   (prog (x pct amt-to-allocate thisptime diffptime difftottime
		    diffgctime)
		 (cond ((null s) (return)))  
		 (cond ((null (boundp '$gccount$)) (setq $gccount$ 0)))
		 (setq $gccount$ (1+ $gccount$))
		 (setq x (opval (car s)))
		 (setq thisptime (ptime)
		       difftottime (max  (difference (car thisptime)
						     (car ptimeatlastgc))
					 1)
		       diffgctime (difference (cadr thisptime)
					      (cadr ptimeatlastgc))
		       ptimeatlastgc thisptime)
		 ; pct is the percentage of space used
		 (setq pct (quotient (times 100 (car x))
				     (max 1 (times (cadr x) (caddr x)))))
		 (setq amt-to-allocate
		       (cond (gcafter-panic-mode 
			      (cond ((greaterp pct 95) 
				     (patom "[Storage space totally exhausted]")
				     (terpr)
				     (error "Space exhausted when allocating "
					    (car s)))
				    (t 0)))
			     ((greaterp pct $gc_midlim)
			      (max $gc_minalloc (fix (times $gc_pct (cadr x)))))
			     ((greaterp pct $gc_lowlim)
			      $gc_minalloc)
			     ((lessp (cadr x) 100)
			      $gc_minalloc)
			     (t 0)))
		 (cond ((and (null gcafter-panic-mode) (greaterp amt-to-allocate
								 0))
			(cond ((atom (errset (allocate (car s) amt-to-allocate)))
			       (cond ($gcprint 
					(patom "[Now in storage allocation panic mode]")
					(terpr)))
			       (setq gcafter-panic-mode t)))))

		 (cond ($gcprint (prtpagesused (car s) difftottime diffgctime)
				 (comment (cond ((and (getd 'gcstat)
					     (eq $gcprint '$all))
					(print (gcstat))
					(terpr)))))))))

;----Section V - the functions
; 


;--- bigp - x : lispval
;	returns t if x is a bignum
;
(def bigp (lambda (arg) (equal (type arg) 'bignum)))

;--- comment - any
; 	ignores the rest of the things in the list
(def comment
  (nlambda (x) 'comment))


;--- copy - l : list (will work if atom but will have no effect)
;	makes a copy of the list.
; will also copy vector and vectori's, if their property list
; doesn't have the 'unique' flag
;
(def copy
   (lambda (l)
      (cond ((dtpr l) (cons (copy (car l)) (copy (cdr l))))
	    ((vectorp l)
	     (if (vget l 'unique)
		then l
		else (let ((size (vsize l)))
			(do ((newv (new-vector size))
			     (i 0 (1+ i)))
			    ((not (<& i size))
			     (vsetprop newv (copy (vprop l)))
			     newv)
			    (vset newv i (copy (vref l i)))))))
	    ((vectorip l)
	     (if (vget l 'unique)
		then l
		else (let ((size (vsize-byte l)))
			(do ((newv (new-vectori-byte size))
			     (i 0 (1+ i)))
			    ((not (<& i size))
			     (vsetprop newv (copy (vprop l)))
			     newv)
			    (vseti-byte newv i (vrefi-byte l i))))))
	    (t l))))


;--- copysymbol - sym : symbol to copy
;		- flag : t or nil
;  generates an uninterned symbol with the same name as sym.  If flag is t
; then the value, function binding and property list of sym are placed
; in the uninterned symbol.
;
(def copysymbol 
  (lambda (sym flag)
	  ((lambda (newsym)
		   (cond (flag (cond ((boundp sym) (set newsym (eval sym))))
			       (putd newsym (getd sym))
			       (setplist newsym (plist sym))))

		   newsym)
	   (uconcat sym))))


;--- cvttointlisp -- convert reader syntax to conform to interlisp
;
(def cvttointlisp
  (lambda nil
	  (setsyntax '\% 'vescape)		; escape character
	  (setsyntax '\\ 'vcharacter)		; normal character
	  (setsyntax '\` 'vcharacter)		; normal character
	  (setsyntax '\, 'vcharacter)		; normal character
	  (sstatus uctolc t)			; one case
	  ))


;--- cvttomaclisp - converts the readtable to a maclisp character syntax
;
(def cvttomaclisp
  (lambda nil
	  (setsyntax '\/ 'vescape)		; escape
	  (setsyntax '\\ 'vcharacter)		; normal char
	  (setsyntax '\[ 'vcharacter)		; normal char
	  (setsyntax '\] 'vcharacter)		; normal char
	  (sstatus uctolc t)))

(declare (special readtable))
;--- cvttoucilisp - converts the readtable to a ucilisp character syntax
;
(def cvttoucilisp
  (lambda nil
	  (sstatus uctolc t)		; upper case to lower case
  					; change backquote character.
					; to ` and ! and !@ from ` , and ,@
  					; undo comma.
	(setsyntax '\! 'splicing (get '\, readtable))
	(setsyntax '\, 'vcharacter)
	  	; 
  		; ~ as comment character, not ; and / instead of \ for escape
  	(setsyntax '\~ 'splicing 'zapline)
	(setsyntax '\; 'vcharacter)
	(setsyntax '\/ 'vescape)
	(setsyntax '\\   'vcharacter)))


;--- cvttofranzlisp - converts the readtable to the standard franz readtable
; this just does the obvious conversions, assuming that the user was
; in the maclisp syntax before.
(def cvttofranzlisp
   (lambda nil
      (setsyntax '\/ 'vcharacter)
      (setsyntax '\\ 'vescape)
      (setsyntax '\[ 'vleft-bracket)
      (setsyntax '\] 'vright-bracket)
      (sstatus uctolc nil)))

;--- defprop - like putprop except args are not evaled
;
(def defprop 
    (nlambda (argl)
	(putprop (car argl) (cadr argl) (caddr argl) )))

;--- delete
;	- val - lispval
;	- lst - list
;	- n   - Optional arg, number of occurances to delete
; removes up to n occurances of val from the top level of lst.
; if n is not given, all occurances will be removed.
;
(def delete
  (lexpr (nargs)
	 (prog (val lst cur ret nmb)
	       (cond ((= nargs 2)
		      (setq nmb -1))
		     ((= nargs 3) 
		      (setq nmb (arg 3)))
		     (t (error " wrong number of args to delete "
			       (cons 'delete (listify nargs)))))
	       (setq val (arg 1) lst (arg 2))
	       (cond ((and (atom lst) (not (null lst))) 	
		      (error " non-list arg to delete " 
			       (cons 'delete (listify nargs)))))
	       (setq cur (cons nil lst)
		     ret cur)
	   loop
	       (cond ((or (atom lst) (zerop nmb))
		      (return (cdr ret)))
		     ((equal val (car lst))
		      (rplacd cur (cdr lst))
		      (setq nmb (1- nmb)))
		     (t (setq cur (cdr cur))))
	       (setq lst (cdr lst))
	       (go loop))))

;--- delq 
;  same as delete except eq is used for testing.
;
(def delq
  (lexpr (nargs)
	 (prog (val lst cur ret nmb)
	       (cond ((= nargs 2)
		      (setq nmb -1))
		     ((= nargs 3) 
		      (setq nmb (arg 3)))
		     (t (error " wrong number of args to delq "
			       (cons 'delq (listify nargs)))))
	       (setq val (arg 1) lst (arg 2))
	       (cond ((and (atom lst) (not (null lst))) 	
		      (error " non-list arg to delq " 
			       (cons 'delq (listify nargs)))))
	       (setq cur (cons nil lst)
		     ret cur)
	   loop
	       (cond ((or (atom lst) (zerop nmb))
		      (return (cdr ret)))
		     ((eq val (car lst))
		      (rplacd cur (cdr lst))
		      (setq nmb (1- nmb)))
		     (t (setq cur (cdr cur))))
	       (setq lst (cdr lst))
	       (go loop))))

;--- evenp : num   -  return 
;
;
(def evenp
  (lambda (n)
	  (cond ((not (zerop (boole 4 1 n))) t))))

;--- ex [name] : unevaluated name of file to edit.
;	the ex editor is forked to edit the given file, if no
;	name is given the previous name is used
;
(def ex (nlambda (x) (exvi 'ex x nil)))

(declare (special edit_file))

(def exvi 
  (lambda (cmd x doload) 
	   (prog (handy handyport bigname)
		 (cond ((null x) (setq x (list edit_file)))
		       (t (setq edit_file (car x))))		 
		 (setq bigname (concat (car x) ".l"))
		 (cond ((setq handyport (car (errset (infile bigname) nil)))
			(close handyport)
			(setq handy bigname))
		       (t (setq handy (car x))))
		 (setq handy (concat cmd " '+set lisp' " handy))
		 (setq handy (list 'process handy))
		 (eval handy)
		 (cond (doload (load edit_file))))))

;--- exec - arg1 [arg2 [arg3 ...] ] unevaluated atoms
;	A string of all the args concatenated together seperated by 
;	blanks is forked as a process.
;
(def exec
 (nlambda (list)
     (do ((xx list (cdr xx))
	  (res "" (concat res " " (car xx))))
	 ((null xx) (*process res)))))

;--- exl - [name] : unevaluated name of file to edit and load.
;	If name is not given the last file edited will be used.
;	After the file is edited it will be `load'ed into lisp.
;
(def exl (nlambda (x) (exvi 'ex x t)))

;----- explode functions -------
; These functions, explode , explodec and exploden, implement the 
; maclisp explode functions completely.
; They have a similar structure and are written with efficiency, not
; beauty in mind (and as a result they are quite ugly)
; The basic idea in all of them is to keep a pointer to the last
; thing added to the list, and rplacd the last cons cell of it each time.
;
;--- explode - arg : lispval
;	explode returns a list of characters which print would use to
; print out arg.  Slashification is included.
;
(def explode
  (lambda (arg)
	  (cond ((atom arg) (aexplode arg))
		((vectorp arg)
		 (aexplode (concat "vector[" (vsize arg) "]")))
		((vectorip arg)
		 (aexplode (concat "vectori[" (vsize-byte arg) "]")))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '|(| (explode (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '|)| )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(| | |.| | | ,@(explode ll) 
						     ,@(ncons '|)|)))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '| | (explode (car ll))))
		       (rplacd xx sofar))))))

;--- explodec - arg : lispval
; returns the list of character which would be use to print arg assuming that
; patom were used to print all atoms.
; that is, no slashification would be used.
;
(def explodec
  (lambda (arg)
	  (cond ((atom arg) (aexplodec arg))
		((vectorp arg)
		 (aexplodec (concat "vector[" (vsize arg) "]")))
		((vectorip arg)
		 (aexplodec (concat "vectori[" (vsize-byte arg) "]")))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '|(| (explodec (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '|)| )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(| | |.| | | ,@(explodec ll) 
						     ,@(ncons '|)|)))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '| | (explodec (car ll))))
		       (rplacd xx sofar))))))

;--- exploden - arg : lispval
;	returns a list just like explodec, except we return fixnums instead
; of characters.
;
(def exploden
  (lambda (arg)
	  (cond ((atom arg) (aexploden arg))
		((vectorp arg)
		 (aexploden (concat "vector[" (vsize arg) "]")))
		((vectorip arg)
		 (aexploden (concat "vectori[" (vsize-byte arg) "]")))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons 40. (exploden (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons 41.)) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(32. 46. 32. ,@(exploden ll) 
						     ,@(ncons 41.)))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons 32. (exploden (car ll))))
		       (rplacd xx sofar))))))

;-- expt  - x
;	  - y
;
;	   y
; returns x
;
(defun expt (x y)
  (cond ((equal x 1) x)
	((zerop x) x)   ; Maclisp does this 
	((lessp y 0) (quotient 1.0 (expt x (times -1 y))))
	((floatp y) 
	 (exp (times y (log x)))) ; bomb out for (-3)^4 or (-3)^4.0 or 0^y.
	((bigp y)
	 (error "expt: Can't compute number to a bignum power" y))
	(t ; y is integer, y>= 0
	   (prog (res)
		 (setq res 1)
		 loop
		 (cond ((equal y 0) (return res))
		       ((oddp y)(setq res (times  res x) y (1- y)))
		       (t (setq x (times x x) y (/ y 2))))
		 (go loop)))))



;--- ffasl :: fasl in a fortran file
;  arg #
;   1	- fnam : file name
;   2	- entry : entry point name
;   3	- fcn  : entry name
;   4   - disc : optional discipline
;   5   - lib  ; optional library specifier
;
(defun ffasl (fnam entry fcn &optional (disc 'subroutine) (lib " "))
  (cfasl fnam entry fcn disc (concat lib " -lI77 -lF77 -lm")))


;
; filepos function (maclisp compatibility)
;
(defun filepos n
  (cond ((zerop n) nil)
	((onep n)
	 (fseek (arg 1) 0 1))
	((equal n 2)
	 (fseek (arg 1) (arg 2) 0))))

;--- fixp - l : lispval
;	returns t if l is a fixnum or bignum
;
(defun fixp (x) (or (equal (type x) 'fixnum)
		    (equal (type x) 'bignum)))



;--- flatsize - l : lispval
;	     the second arg should be:
;	      - n : limit for what we care about
;	     but we dont care about this at present, since we have
;	     to explode the whole thing anyway.
;	returns the number of characters which print would
;	use to print l
;
(defun flatsize n
  (length (explode (arg 1))))


;--- floatp - l : lispval
;	returns t if l is a flonum
;
(defun floatp (x) (equal 'flonum (type x)))


;--- getchar,getcharn   - x : atom
;	     		- n : fixnum
; returns the n'th character of x's pname (the first corresponds to n=1)
; if n is negative then it counts from the end of the pname
; if n is out of bounds, nil is returned

(def getchar
  (lambda (x n)
	  (concat (substring x n 1))))


(def getcharn
  (lambda (x n)
	  (substringn x n 0)))


(def getl 
  (lambda (atm lis)
	  (do ((ll (cond ((atom atm) (plist atm))
			 (t (cdr atm)))
		   (cddr ll)))
	      ((null ll) nil)
	      (cond ((memq (car ll) lis) (return ll))))))


;--- help
; retrive selected portions of the Franz Lisp manual.
; There are four types of help offered:
; (help) prints a description of the other three options
; (help tc) prints a table of contents.
; (help n) {where n is a number or b or c} prints the whole chapter.
; (help fcn) prints info on function fcn
;
; An index to the functions is kept in the documentation directory.
; The index has entries like (append ch2.r).  
; When asked to print info on a function, it locates the chapter
; using the index then asks more to locate the definition.
;
(declare (localf locatefunction))

(defun help fexpr (lis)
  (cond ((null lis) 
 (patom "type (help fnc) for info on function fnc")(terpr)
 (patom "type (help n) to see chapter n")(terpr)
 (patom "type (help tc) for a table of contents")(terpr))
	(t (do ((ll lis (cdr ll))
		(fcn))
	       ((null ll))
	       (cond ((not (atom (setq fcn (car ll))))
		      (patom "Bad option to help ")(print fcn)(terpr))
		     ((and (stringp fcn) (setq fcn (concat fcn)) nil))
		     ((eq fcn 'tc)
		      (patom "Table of contents")(terpr)
 (patom "1 - intro; 2 - data structure; 3 - arithmetic; 4 - special")(terpr)
 (patom "5 - i/o; 6 - system; 7 - reader; 8 - functions; 9 - arrays")(terpr)
 (patom "10 - exceptions; 11 - trace package; 12 - Liszt;")(terpr)
 (patom "14 - step package; 15 - fixit package") (terpr)
 (patom "b - special symbols; c - gc & debugging & top level ")(terpr))
		     ((or (and (numberp fcn) (lessp fcn 16) (greaterp fcn -1))
			  (memq fcn '(b c)))
		      (apply 'process 
			 (ncons (concat "/usr/ucb/ul "
					lisp-library-directory
					"/manual/ch"
				       fcn ".r | /usr/ucb/more -f" ))))
		     ((locatefunction fcn))
		     (t (patom "Unknown function: ")(print fcn)(terpr)))))))

(declare (special readtable))

(defun locatefunction (fc)
  (let (x inf )
       (cond ((null (get 'append 'helplocation)) 
	      (patom "[Reading help index]")(drain)
	      (setq inf (infile (concat lisp-library-directory
					"/manual/helpindex")))
	      (do ((readtable (makereadtable t))
		   (x (read inf) (read inf)))
		  ((null x) (close inf) (terpr))
		  (cond ((null (cddr x))
			 (putprop (car x) (cadr x) 'helplocation))
			(t (putprop (concat (car x) " " (cadr x))
				    (caddr x)
				    'helplocation))))))
       (cond ((setq x (get fc 'helplocation))
	      (apply 'process (ncons (concat "/usr/ucb/ul "
					     lisp-library-directory
					     "/manual/"
					     x 
					     " | /usr/ucb/more -f \"+/(" 
					     fc 
					     "\"")))
	      t))))

;
; (hunk 'g_arg1 [...'g_argn])
;
; This function makes a hunk. The hunk is preinitialized to the
; arguments present. The size of the hunk is determined by the
; number of arguments present.
;

(defun hunk n
  (prog (size)
	(setq size -1)
	(cond ((> n 128) (error "hunk: size is too big" n))
	      ((eq n 1) (setq size 0))
	      ((eq n 0) (return nil))	; hunk of zero length
	      (t (setq size (1- (haulong (1- n))))))
	(setq size (*makhunk size))
	(do
	 ((argnum 0 (1+ argnum)))
	 ((eq argnum n))
	 (*rplacx argnum size (arg (1+ argnum))))
	(return size)))


;--- last - l : list
;	returns the last cons cell of the list, NOT the last element
;
(def last 
  (lambda (a)
	  (do ((ll a (cdr ll)))
	      ((null (cdr ll))  ll))))

;---- load 
; load will either load (read-eval)  or fasl in the file.
; it is affected by these global flags
;  tilde-expansion :: expand filenames preceeded by ~ just like
;	csh does (we do the expansion here so each i/o function we call
;	doesn't have to do it).
;  load-most-recent :: if there is a choice between a .o and a .l file,
;	load the youngest one
;
(declare (localf load-a-file))
(declare (special gcdisable load-most-recent tilde-expansion))

(or (boundp 'load-most-recent) (setq load-most-recent nil))
(or (boundp 'tilde-expansion) (setq tilde-expansion t))

(defun load (filename &rest fasl-args)
  (cond ((not (or (symbolp filename) (stringp filename))) 
	 (error "load: illegal filename " filename)))
  (let ( load-only fasl-only no-ext len search-path name pred shortname explf
	 faslfile loadfile)

       
       (cond (tilde-expansion (setq filename (tilde-expand filename))))
		
       ; determine the length of the filename, ignoring the possible
       ; list of directories.  set explf to the reversed exploded filename
       (setq len (do ((xx (setq explf (nreverse (exploden filename))) (cdr xx))
		      (i 0 (1+ i)))
		     ((null xx) i)
		     (cond ((eq #// (car xx)) (return i)))))

       (cond ((> len 2)
	      (cond ((eq (cadr explf) #/.)
		     (cond ((eq (car explf) #/o)
			    (setq fasl-only t))
			   ((eq (car explf) #/l)
			    (setq load-only t))
			   (t (setq no-ext t))))
		    (t (setq no-ext t))))
	     (t (setq no-ext t)))

       ; a short name is less or equal 12 characters.  If a name is not
       ; short, then load will not try to append .l or .o
       (cond ((or (< len 13) (status feature long-filenames))
	      (setq shortname t)))

       (cond ((and (> len 0) (eq (getchar filename 1) '/))
	      (setq search-path '(||)))
	     (t (setq search-path (status load-search-path))))
       (do ((xx search-path (cdr xx)))
	   ((null xx) (error "load: file not found " filename))
	   (setq pred (cond ((memq (car xx) '(|| |.|)) '||)
			    (t (concat (car xx) "/"))))
	   (cond (no-ext
		  (cond ((and shortname
			      load-most-recent
			      (probef
				 (setq faslfile (concat pred filename ".o")))
			      (probef
				 (setq loadfile (concat pred filename ".l"))))
			 ; both an object and a source file exist.
			 ; load the last modified one (fasl wins in ties)
			 (let ((faslstat (filestat faslfile))
			       (loadstat (filestat loadfile)))
			    (cond ((< (filestat:mtime faslstat)
				      (filestat:mtime loadstat))
				   (return (load-a-file loadfile)))
				  (t (return
					(fasl-a-file faslfile
						     (car fasl-args)
						     (cadr fasl-args)))))))
			((and shortname
			      (probef (setq name
					    (concat pred filename ".o"))))
			 (return (fasl-a-file name (car fasl-args)
					      (cadr fasl-args))))
			((and shortname
			      (probef (setq name
					    (concat pred filename ".l"))))
			 (return (load-a-file name)))
			((probef (setq name (concat pred filename)))
			 (cond (fasl-args (return
					     (fasl-a-file name
							  (car fasl-args)
							  (cadr fasl-args))))
			       (t (return (load-a-file name)))))))
		 (fasl-only
		  (cond ((probef (setq name (concat  pred  filename)))
			 (return (fasl-a-file name (car fasl-args)
					      (cadr fasl-args))))))
		 (load-only
		  (cond ((probef (setq name (concat pred filename)))
			 (return (load-a-file name)))))))))

;--- tilde-expand :: given a ~filename, expand it
;
(defun tilde-expand (name)
   (cond ((or (symbolp name) (stringp name))
	  (cond ((eq (getcharn name 1) #/~)
		 (let ((form (exploden name)))
		    (do ((xx (cdr form) (cdr xx))
			 (res)
			 (val))
			((or (null xx) (eq (car xx) #//))
			 ;; if this is the current user, just get value
			 ;; from environment variable HOME
			 (cond ((or (null res)
				    (equal (setq res (implode (nreverse res)))
					   (getenv 'USER)))
				(setq val (getenv 'HOME)))
			       (t (setq val (username-to-dir res))))
			 (cond ((null val)
				(error "tilde-expand: unknown user " res))
			       (t (concat val (implode xx)))))
			(setq res (cons (car xx) res)))))
		(t name)))
	 (t (error "tilde-expand: illegal argument " name))))

      

;--- fasl-a-file
; The arguments are just like those to fasl.  This fasl's a file
; and if the translink's are set, it does the minimum work necessary to rebind
; the links (so that the new functions just fasl'ed in will be used).
; 
(defun fasl-a-file (name map warnflag)
   (let ((translinkarg (status translink)))
      (prog1
	 (fasl name map warnflag)
	 (cond ((and translinkarg (setq translinkarg (status translink)))
		; if translink was set before and is still set
		(cond ((eq translinkarg t)
		       (sstatus translink nil)  ; clear all links
		       (sstatus translink t))	; set to make links
		      (t ; must be 'on'
			 (sstatus translink on) ; recompute all links
			 ))))))) 

(declare (special $ldprint))	; print message before loading
(declare (special prinlevel prinlength))

(defun load-a-file (fname)
   (cond ($ldprint (patom "[load ")(patom fname)(patom "]")(terpr)))
   (let ((translinkarg (status translink)))
      (prog1
	 (let ((Piport (infile fname))
	       ; (gcdisable t)	; too dangerous: removed for now
	       ; don't gc when loading, it slows things down
	       (eof (list nil)))
	    (do ((form (errset (read Piport eof)) (errset (read Piport eof)))
		 (lastform "<no form read successfully>"))
		((eq eof (car form)) (close Piport) t)
		(cond ((null form)
		       (error "load aborted due to read error after form "
			      lastform))
		      (t (setq lastform (car form))
			 (eval (car form))))))
	 (cond ((and translinkarg (setq translinkarg (status translink)))
		; if translink was set before and is still set
		(cond ((eq translinkarg t)
		       (sstatus translink nil)  ; clear all links
		       (sstatus translink t))	; set to make links
		      (t ; must be 'on'
			 (sstatus translink on) ; recompute all links
			 )))))))

(funcall 'sstatus (list 'load-search-path (list '|.| lisp-library-directory)))
;--- include - read in the file name given, the name not evaluated
;
(def include (nlambda (l) (load (car l))))

;--- includef - read in the file name given and eval the first arg
;
(def includef (lambda (l) (load l)))


;--- list-to-bignum
;  convert a list of fixnums to a bignum.
; there is a function bignum-to-list but it is written in C
;
;(author: kls)
;
(def list-to-bignum
 (lambda (x) (cond (x (scons (car x) (list-to-bignum (cdr x))))
		   (t nil))))



;--- macroexpand - form 
;	expands out all macros it can
;
(def macroexpand
  (lambda (form)
    (prog nil
  top (cond ((atom form) (return form))
	    ((atom (car form))
	     (return
	      (let ((nam (car form)) def disc)
		   (setq def (getd nam))
		   (setq disc (cond ((bcdp def) (getdisc def))
				    ((arrayp def) 'array)
				    ((dtpr def) (car def))))
		   (cond ((and (null def)
			       (get nam 'macro-autoload))
			  (setq disc 'macro)))
		   (cond ((memq disc '(array lambda lexpr nil))
			  (cons nam (mapcar 'macroexpand (cdr form))))
			 ((eq disc 'macro)
			  (setq form (apply nam form))
			  (go top))
			 ((eq nam 'prog)
			  (cons nam
				(cons (cadr form)
				      (mapcar 'macroexpand (cddr form)))))
			 (t form)))))
	    (t (return (cons (macroexpand (car form))
			     (mapcar 'macroexpand (cdr form)))))))))




;
; (makhunk 'n)
;
; This function is similar to hunk, except that:
;
; n can be a fixnum, which specifies the length of the hunk.
;	The hunk is preinitialized to nil's
; n can be a list which is used to preinitialize the hunk.
;
(defun makhunk (n)
  (prog (size Hunk)
	(setq size -1)
	(cond ((numberp n)
;
; If n is a number then build a nil hunk of the right size
;
	       (cond ((greaterp n 128) (error "makhunk: size is too big" n))
		     ((= n 1) (setq size 0))
		     (t (setq size (1- (haulong (1- n))))))
	       (cond ((minusp size) (return nil)))
	       (setq Hunk (*makhunk size))
	       (do ((i 0 (1+ i)))
		   ((=& i n))
		   (*rplacx i Hunk nil))
	       (return Hunk))
;
; If it isn't a number, then try hunk on it
;
	      (t (return (apply 'hunk n))))))

;--- member - VAL : lispval
;	    - LIS : list
;	returns that portion of LIS beginning with the first occurance
;	of VAL  if  VAL is found at the top level of list LIS.
;	uses equal for comparisons.
;
(def member 
  (lambda ($a$ $l$)
	  (do ((ll $l$ (cdr ll)))
	      ((null ll) nil)
	      (cond ((equal $a$ (car ll)) (return ll))))))

;--- memq - arg : (probably a symbol)
;	  - lis : list
; returns part of lis beginning with arg if arg is in lis
;	
; [ defintion moved to top of file to allow backquote macro to work ]

;--- min - arg1 ... numbers 
;
; 	returns minimum of n numbers. 
;

(def min
  (lexpr (nargs)
	 (do ((i nargs (1- i))
	      (min (arg 1)))
	     ((lessp i 2) min)
	     (cond ((lessp (arg i) min) (setq min (arg i)))))))


;
(def oddp
  (lambda (n)
	  (cond ((not (zerop (boole 1 1 n))) t))))

;--- plusp : x - number
; returns t iff x is greater than zero

(def plusp
  (lambda (x)
	  (greaterp x 0)))


;--- princ : l - any s-expression
;	    [p] - port to write to
; prints using patom for atoms (unslashified)
;
(def princ
  (lexpr (n)
	 (prog (port val)
	       (cond ((eq n 2) (setq port (arg 2))))
	       (cond ((dtpr (setq val (arg 1)))
		      (cond ((and (eq 'quote (car val))
				  (dtpr (cdr val))
				  (null (cddr val)))
			     (patom "'" port)
			     (princ (cadr val) port))
			    (t 
			     (patom "(" port)
			     (do ((xx val))
				 ((null xx) (patom ")" port))
				 (princ (car xx) port)
				 (cond ((null (setq xx (cdr xx))))
				       ((not (dtpr xx))
					(patom " . " port)
					(princ xx port)
					(setq xx nil))
				       (t (patom " " port)))))))
		     (t (patom val port)))
	       (return t))))

;--- prog1 : return the first value computed in a list of forms
;
(def prog1
  (lexpr (n)
	 (arg 1)))

;--- reverse : l - list
;	returns the list reversed using cons to create new list cells.
;
(def reverse 
  (lambda (x)
	  (cond ((null x) nil)
		(t (do ((cur (cons (car x) nil) 
			     (cons (car res) cur))
			(res (cdr x) (cdr res)))
		       ((null res) cur))))))


;--- shell - invoke a new c shell
;
(def shell 
  (lambda nil 
	  ((lambda (shellname)
		   (cond ((lessp (flatc shellname) 1) (setq shellname 'csh)))
		   (apply 'process (ncons shellname)))
	   (getenv 'SHELL))))



; S L O A D  stuff
;
(defvar $sldprint t)
(declare (special sload-print))
(setq sload-print nil)

(defmacro sl-print (&rest args)
   `(cond ((and sload-print
		(getd sload-print))
	   (funcall sload-print . ,args))
	  (t (print . ,args))))

;--- sload : fn - file name (must include the .l)
;	loads in the file printing each result as it is seen
;
(defun sload (&rest files)
   (mapc '(lambda (fn)
	     (prog (por eof argnum result)
		(cond ((setq por (infile fn))
		       (and $sldprint
			    (progn (princ "[sload ")
				   (princ fn)
				   (princ "]")
				   (terpr))))
		      (t (patom "bad file name: ")
			 (patom fn)
			 (terpr)
			 (return nil)))
		(setq eof (gensym))
		(do ((input (read por eof) (read por eof)))
		    ((eq eof input) (close por))
		    (and $sldprint
			 (cond ((and (dtpr input)
				     (setq argnum
					   (get (car input) 'sloadprintarg)))
				(print (nth argnum input)))
			       (t (print input))))
		    (setq result (eval input))
		    (and (eq 'value $sldprint)
			 (progn (princ ": ")
				(sl-print result)))
		    (and $sldprint
			 (terpr)))
		(return t)))
	 files))

(defprop def 1 sloadprintarg)
(defprop defun 1 sloadprintarg)

(defprop setq 1 sloadprintarg)
(defprop declare 1 sloadprintarg)





;---- bubble merge sort 
; it recursively splits the list to sort until the list is small.  At that
; point it uses a bubble sort.  Finally the sorted lists are merged.

(declare (special sort-function))

;--- sort :: sort a lisp list
; args: lst - list of items
;       fcn - function to compare two items.
; returns: the list with such that for each pair of adjacent elements,
;	   either the elements are equal, or fcn applied to the two 
; 	   args returns a non nil value.
;
(defun sort (lst fcn)
  (setq sort-function (cond (fcn)   ; store function name in global cell
			    (t 'alphalessp)))
  ; (setq sort-compares 0)		; count number of comparisons
  (sortmerge lst (length lst)))


;--- sortmerge :: utility routine to sort
; args: lst - list of items to sort
;	nitems - a rough idea of how many items are in the list
;
; result - sorted list (see the result of sort above)
;
(defun sortmerge (lst nitems)
  (prog (tmp tmp2)
	(cond ((greaterp nitems 7)
	       ; do a split and merge
	       (setq tmp (splitlist lst (setq tmp2 (quotient nitems 2))))
	       (return (mergelists (sortmerge (car tmp) tmp2)
			      (sortmerge (cdr tmp) tmp2))))
	      (t ; do a bubble sort
		 (do ((l lst (cdr l))
		      (fin))
		     ((null l))
		     (do ((ll lst (cdr ll)))
			 ((eq fin (cdr ll)) (setq fin ll))
			 ;(setq sort-compares (1+ sort-compares))
			 (cond ((not (funcall sort-function (car ll) (cadr ll)))
				(rplaca ll (prog1 (cadr ll)
						 (rplaca (cdr ll)
							 (car ll))))))))
		 (return lst)))))

;--- splitlist :: utility routine to split a list
; args : lst - list to split
;        spliton - number of items to put in the first list
;
; returns: a cons cell whose car is the first part of the list
;	   and whose cdr is the second part.
;
(defun splitlist (lst spliton)
  (prog (second)
	(do ((i spliton (sub1 i))
	     (l lst))
	    ((or (null (cdr l)) (zerop i))
	     (setq second (cdr l))
	     (rplacd l nil))
	    (setq l (cdr l)))
	(return (cons lst second))))


;--- mergelists ::utility routine to merge two lists based on predicate function
; args: ls1 - lisp list
; 	ls2 - lisp list
;	sort-function (global) - compares items of the lists
;
; returns: a sorted list containing the elements of the two lists.
; 
(defun mergelists  (ls1 ls2)
  (prog (result current)
	; initialize
	(setq current (setq result (cons nil nil)))
loop	(cond ((null ls1)
	       (rplacd current ls2)
	       (return (cdr result)))
	      ((null ls2)
	       (rplacd current ls1)
	       (return (cdr result)))
	      ((funcall sort-function (car ls1) (car ls2))
	       ;(setq sort-compares (1+ sort-compares))
	       (rplacd current ls1)
	       (setq current ls1
		     ls1 (cdr ls1)))
	      (t ;(setq sort-compares (1+ sort-compares))
		 (rplacd current ls2)
		 (setq current ls2
		       ls2 (cdr ls2))))
	(go loop)))

;--- end bubble merge sort
(declare (localf exchange2))

(defun sortcar (a fun)
   (prog (n)
       (if (null fun) then (setq fun 'alphalessp))
       (cond ((null a) (return nil)) ;no elements
	     (t (setq n (length a))
		(do i 1 (1+ i) (greaterp i n) (sortcarhelp a fun))
		(return a)))))

(defun sortcarhelp (a fun)
  (cond ((null (cdr a)) a)
        ((funcall fun (caadr a) (caar a))  
	 (exchange2 a)
	 (sortcarhelp (cdr a) fun))
	(t (sortcarhelp (cdr a) fun))))


(defun exchange2 (a)
  (prog (temp)
	(setq temp (cadr a))
	(rplaca (cdr a) (car a))
	(rplaca a temp)))

;--- sublis: alst - assoc list ((a . val) (b . val2) ...)
;	     exp  - s-expression
; for each atom in exp which corresponds to a key in alst, the associated
; value from alst is substituted.  The substitution is done by adding
; list cells, no struture mangling is done.  Only the minimum number
; of list cells will be created.
;
(def sublis
  (lambda (alst exp)
     (let (tmp)
	  (cond ((atom exp) 
		 (cond ((setq tmp (assoc exp alst))
			(cdr tmp))
		       (t exp)))
		((setq tmp (sublishelp alst exp))
		 (car tmp))
		(t exp)))))

;--- sublishelp : alst - assoc list
;		  exp  - s-expression
; this function helps sublis work.  it is different from sublis in that
; it return nil if no change need be made to exp, or returns a list of
; one element which is the changed exp.
;
(def sublishelp
  (lambda (alst exp)
     (let (carp cdrp)
	  (cond ((atom exp)
		 (cond ((setq carp (assoc exp alst))
			(list (cdr carp)))
		       (t nil)))
		(t (setq carp (sublishelp alst (car exp))
			 cdrp (sublishelp alst (cdr exp)))
		   (cond ((not (or carp cdrp)) nil)		; no change
			 ((and carp (not cdrp))			; car change
			  (list (cons (car carp) (cdr exp))))	
			 ((and (not carp) cdrp)			; cdr change
			  (list (cons (car exp) (car cdrp))))	
			 (t					; both change 
			  (list (cons (car carp) (car cdrp))))))))))


;--- subst : new - sexp
;	     old - sexp
;	     pat - sexp
; substitutes in patrn all occurrences equal to old with new and returns the
; result
; MUST be put in the manual

(declare (special new old))

(def subst 
  (lambda (new old pat)
	  (cond ((symbolp old) (substeq pat))
		(t (substequal pat)))))

;use this function for substituting for symbols
(def substeq
  (lambda (pat)
	  (cond ((eq old pat) new)
		((atom pat) pat)
		(t (cons (substeq (car pat))(substeq (cdr pat)))))))

(def substequal
  (lambda (pat)
	  (cond ((equal old pat) new)
		((atom pat) pat)
		(t (cons (substequal (car pat))
			 ; in interlisp, the next line would be
			 ;(substeq (cdr pat))
			 ; for maclisp compatibility, we do this.
			 (substequal (cdr pat)))))))

(declare (unspecial new old))
;--- vi: arg is unevaluated name of function to run vi on
;
(def vi (nlambda (x) (exvi 'vi x nil)))

;--- vil : arg is unevaluated, edits file and then loads it
;
(def vil (nlambda (x) (exvi 'vi x t)))

;--- *quo : returns integer part of x/y
; x and y must be fixnums.
;
(putd '*quo (getd 'quotient))

;--- xcons : a - sexp
;	     b - sexp
; returns (b . a)   that is, it is an exchanged cons
;
(def xcons  (lambda (a b) (cons b a)))






;--- mode lines, must be last lines of the file
; vi: set lisp :
;
