
(setsyntax '\; 'splicing 'zapline)

;---------------- auxfns0 ---------------
; this file contains the definitions of the most common functions.
; It should only be loaded in Opus 30 Franz Lisp.
; These functions should be loaded into every lisp.
;
;------------------------------------------------
; preliminaries:

(eval-when (eval load)
  (cond ((null (getd 'back=quotify)) (load 'backquote))))

(eval-when (compile)
  (setq macros t))


;--- declare - ignore whatever is given, this is for the compiler
;
(def declare (nlambda (x) nil))

;-----------------------------------------------
; functions contained herein:

; ----------------------------------
;	macros

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


; defmacro for franz, written 20sep79 by jkf

(declare (special defmacrooptlist))

;--- defmacro - name - name of macro being defined
;	      - pattrn - formal arguments plus other fun stuff
;	      - body   - body of the macro
; This is an intellegent macro creator.  The pattern may contain
; symbols which are formal paramters, lists which show how the
; actual paramters will appear in the args, and these key words
;  &rest name  - the rest of the args (or nil if there are no other args)
;		 is bound to name
;  &optional name - bind the next arg to name if it exists, otherwise
;		    bind it to nil
;  &optional (name init) - bind the next arg to name if it exists, otherwise
;		    bind it to init evaluted. (the evaluation is done left
;		    to right for optional forms)
;  &optional (name init given) - bind the next arg to name and given to t
;		    if the arg exists, else bind name to the value of
;		    init and given to nil.
;
; Method of operation:
;  the list returned from defmcrosrc has the form ((cxxr name) ...)
;	where cxxr is the loc of the macro arg and name is it formal name
;  defmcrooptlist has the form ((initv cxxr name) ...)
; which is use for &optional args with an initial value.
;  here cxxr looks like cdd..dr which will test of the arg exists.
;
; the variable defmacro-for-compiling determines if the defmacro forms
; will be compiled. If it is t, then we return (progn 'compile (def xx..))
; to insure that it is compiled
;
(cond ((null (boundp 'defmacro-for-compiling))   ; insure it has a value
       (setq defmacro-for-compiling nil)))

(def defmacro
  (macro (args)
    ((lambda (tmp tmp2 defmacrooptlist body)
       (setq tmp (defmcrosrch (caddr args) '(d r) nil)
	     body
	     `(def ,(cadr args)
		   (macro (defmacroarg)
		     ((lambda ,(mapcar 'cdr tmp)
			      ,@(mapcar 
				   '(lambda (arg)
				      `(cond ((setq ,(caddr arg)
						    (,(cadr arg) 
						      defmacroarg))
					      ,@(cond ((setq tmp2 (cadddr arg))
						       `((setq ,tmp2 t))))
					      (setq ,(caddr arg)
						    (car ,(caddr arg))))
					     (t (setq ,(caddr arg)
						      ,(car arg)))))
					defmacrooptlist)
			      ,@(cdddr args))
		      ,@(mapcar '(lambda (arg) 
					 (cond ((car arg)
						`(,(car arg) defmacroarg))))
			       tmp)))))
	     (cond (defmacro-for-compiling `(progn 'compile ,body))
		   (t body)))
     nil nil nil nil)))

(def defmcrosrch
  (lambda (pat form sofar)
	  (cond ((null pat) sofar)
		((atom pat) (cons (cons (concatl `(c ,@form)) pat)
				  sofar))
		((eq (car pat) '&rest)
		 (defmcrosrch (cadr pat) form sofar))
		((eq (car pat) '&optional)
		 (defmcrooption (cdr pat) form sofar))
		(t (append (defmcrosrch (car pat) (cons 'a form) nil)
			   (defmcrosrch (cdr pat) (cons 'd form) sofar))))))

(def defmcrooption
  (lambda (pat form sofar)
    ((lambda (tmp tmp2)
	  (cond ((null pat) sofar)
		((eq (car pat) '&rest)
		 (defmcrosrch (cadr pat) form sofar))
		(t (cond ((atom (car pat))
			  (setq tmp (car pat)))
			 (t (setq tmp (caar pat))
			    (setq defmacrooptlist 
				  `((,(cadar pat) 
				        ,(concatl `(c ,@form))
				        ,tmp
				        ,(setq tmp2 (caddar pat)))
				    . ,defmacrooptlist))))
		   (defmcrooption 
			(cdr pat) 
			(cons 'd form) 
			`( (,(concatl `(ca ,@form)) . ,tmp)
			   ,@(cond (tmp2 `((nil . ,tmp2))))
			  . ,sofar)))))
     nil nil)))

;-----------------
; functions which must be defined first

(def FPEINT 
      (lambda (x$) (patom '"Floating Exception:  ") (drain poport) (break)))

(def INT 
      (lambda (dummy) (patom '"Interrupt:  ") (drain poport) (break)))


(signal 8 'FPEINT)
(signal 2 'INT)


(cond ((null (boundp '$gcprint$))
       (setq $gcprint$ nil)))		; dont print gc stats by default

(cond ((null (boundp '$gccount$))
       (setq $gccount$ 0)))

;--- prtpagesused - [arg] : type of page allocated last time.
;	prints a summary of pages used for certain selected types
;	of pages.  If arg is given we put a star beside that type
;	of page.  This is normally called after a gc.
;
(def prtpagesused
  (nlambda (arg)
	  (patom '"[")
	  (do ((curtypl '(list fixnum symbol string ) (cdr curtypl))
	       (temp))
	      ((null curtypl) (patom '"]") (terpr poport))
	      (setq temp (car curtypl))
	      (cond ((greaterp (cadr (opval temp)) 0)
		     (cond ((eq (car arg) temp)
			    (patom '*)))
		     (patom temp)
		     (patom '":")
		     (print (cadr (opval temp)))
		     (patom '"{")
		     (print (fix (quotient 
				  (times 100.0
					 (car (opval temp)))
				  (times (cadr (opval temp))
					 (caddr (opval temp))))))
		     (patom '"%}")
		     (patom '"; "))))))

;--- gcafter - [s] : type of item which ran out forcing garbage collection.
;	This is called after each gc.
;
(def gcafter 
  (nlambda (s)
	   (prog (x)
		 (cond ((null s) (return)))
		 (cond ((null (boundp '$gccount$)) (setq $gccount$ 0)))
		 (setq $gccount$ (add1 $gccount$))
		 (setq x (opval (car s)))
		 (cond ((greaterp 
			 (quotient (car x)
				   (times 1.0 (cadr x) (caddr x)))
			 .65)
			(allocate (car s) 20))
		       (t (allocate (car s) 10)))
		 (cond ($gcprint$ (apply 'prtpagesused s))))))

;--------------------------------
; functions in alphabetical order

;--- append - x : list
;	    - y : list 
;
(def append2args 
  (lambda (x y)
	  (prog (l l*)
		(cond ((null x) (return y))
		      ((atom x) (err (list '"Non-list to append:" x))))
		(setq l* (setq l (cons (car x) nil)))
	loop	(cond ((atom x) (err (list '"Non-list to append:" x)))
		      ((setq x (cdr x))
		       (setq l* (cdr (rplacd l* (cons (car x) nil))))
		       (go loop)))
		(rplacd l* y)
		(return l))))

(def append
  (lexpr (nargs)
	 (cond ((zerop nargs) nil)
	       (t (do ((i (sub1 nargs) (sub1 i))
		       (res (arg nargs)))
		      ((zerop i) res)
		      (setq res (append2args (arg i) res)))))))



;--- append1 - x : list
;	     - y : lispval
;	puts y at the end of list x
;
(def append1 (lambda (x y) (append x (list y))))


;--- assoc - x : lispval
;	   - l : list
;	l is a list of lists. The list is examined and the first
;	sublist whose car equals x is returned.
;
(def assoc
  (lambda (val alist)
	  (do ((al alist (cdr al)))
	      ((null al) nil)
	      (cond ((equal val (caar al)) (return (car al)))))))

; sassoc and sassq, silly relatives from lisp 1.5 of assoc
;

(defun sassoc(x y z)
  (or (assoc x y)
      (apply z nil)))
(defun sassq(x y z)
  (or (assq x y)
      (apply z nil)))

;--- bigp - x : lispval
;	returns t if x is a bignum
;
(def bigp (lambda (arg) (equal (type arg) 'bignum)))

;--- comment - any
; 	ignores the rest of the things in the list
(def comment
  (nlambda (x) 'comment))

;--- concatl - l : list of atoms
;	returns the list of atoms concatentated
;
(def concatl
 (lambda (x) (apply 'concat x)))



;--- copy - l : list (will work if atom but will have no effect)
;	makes a copy of the list.
;
(def copy 
  (lambda (l)
	  (cond ((atom l) l)
		(t (cons (copy (car l)) (copy (cdr l)))))))


;--- cvttomaclisp - converts the readtable to a maclisp character syntax
;
(def cvttomaclisp
  (lambda nil
	  (setsyntax '\| 138.)		; double quoting char
	  (setsyntax '\/ 143.)		; escape
	  (setsyntax '\\ 2)		; normal char
	  (setsyntax '\" 2)		; normal char
	  (setsyntax '\[ 2)		; normal char
	  (setsyntax '\] 2)		; normal char
	  (sstatus uctolc t)))


;--- defun - standard maclisp function definition form.
;
(def defun 
       (macro (l)
	     (prog (name type arglist body)
		   (setq name (cadr l) l (cddr l))
		   (cond ((null (car l)) (setq type 'lambda))
			 ((eq 'fexpr (car l)) (setq type 'nlambda l (cdr l)))
			 ((eq 'expr (car l))  (setq type 'lambda l (cdr l)))
			 ((eq 'macro (car l)) (setq type 'macro l (cdr l)))
			 ((atom (car l)) (setq type 'lexpr 
					       l `((,(car l)) ,@(cdr l))))
			 (t (setq type 'lambda)))
		   (return `(def ,name 
				 (,type ,@l))))))


;--- defprop - like putprop except args are not evaled
;
(def defprop 
    (nlambda (argl)
	(putprop (car argl) (cadr argl) (caddr argl) )))

;--- delete - val - s-expression
;	    - list - list to delete fromm
;	    -[n] optional count , if not specified, it is infinity
; delete removes every thing in the top level of list which equals val
; the list structure is modified
;
(def delete
  (lexpr (nargs)
	 ((lambda (val list n)
		  (cond ((or (atom list) (zerop n)) list)
			((equal val (car list)) 
			 (delete val (cdr list) (sub1 n)))
			(t (rplacd list (delete val (cdr list) n)))))
	  (arg 1) 
	  (arg 2) 
	  (cond ((equal nargs 3) (arg 3))
		(t 99999999)))))


;--- delq   - val - s-expression
;	    - list - list to delete fromm
;	    -[n] optional count , if not specified, it is infinity
; delq removes every thing in the top level of list which eq's val
; the list structure is modified
;
(def delq
  (lexpr (nargs)
	 ((lambda (val list n)
		  (cond ((or (atom list) (zerop n)) list)
			((eq val (car list)) 
			 (delq val (cdr list) (sub1 n)))
			(t (rplacd list (delq val (cdr list) n)))))
	  (arg 1) 
	  (arg 2) 
	  (cond ((equal nargs 3) (arg 3))
		(t -1)))))

;--- evenp : num   -  return 
;
(def evenp
  (lambda (n)
	  (cond ((not (zerop (boole 4 1 n))) t))))

;--- ex [name] : unevaluated name of file to edit.
;	the ex editor is forked to edit the given file, if no
;	name is given the previous name is used
;
(def ex 
  (nlambda (x) 
	   (prog (handy handyport bigname)
		 (cond ((null x) (setq x (list edit_file)))
		       (t (setq edit_file (car x))))		 
		 (setq bigname (concat (car x) '".l"))
		 (cond ((setq handyport (car (errset (infile bigname) nil)))
			(close handyport)
			(setq handy bigname))
		       (t (setq handy (car x))))
		 (setq handy (concat '"ex " handy))
		 (setq handy (list 'process handy))
		 (eval handy))))

;--- exec - arg1 [arg2 [arg3 ...] ] unevaluated atoms
;	A string of all the args concatenated together seperated by 
;	blanks is forked as a process.
;
(def exec
 (nlambda ($list)
   (prog ($handy)
         (setq $handy (quote ""))
    loop (cond ((null $list)
                (return (eval (list (quote process) $handy))))
               (t (setq $handy
                        (concat (concat $handy (car $list))
                                (quote " ")))
                  (setq $list (cdr $list))
                  (go loop))))))


;--- exl - [name] : unevaluated name of file to edit and load.
;	If name is not given the last file edited will be used.
;	After the file is edited it will be `load'ed into lisp.
;
(def exl (nlambda (fil) (cond (fil (setq edit_file (car fil))))
			(eval (list 'ex edit_file)) 
			(load edit_file)))

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
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '"(" (explode (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '")" )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(" " "." " " ,@(explode ll) 
						     ,@(ncons '")")))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '" " (explode (car ll))))
		       (rplacd xx sofar))))))

;--- explodec - arg : lispval
; returns the list of character which would be use to print arg assuming that
; patom were used to print all atoms.
; that is, no slashification would be used.
;
(def explodec
  (lambda (arg)
	  (cond ((atom arg) (aexplodec arg))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '"(" (explodec (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '")" )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(" " "." " " ,@(explodec ll) 
						     ,@(ncons '")")))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '" " (explodec (car ll))))
		       (rplacd xx sofar))))))

;--- exploden - arg : lispval
;	returns a list just like explodec, except we return fixnums instead
; of characters.
;
(def exploden
  (lambda (arg)
	  (cond ((atom arg) (aexploden arg))
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
(defun expt(x y)
  (cond ((or (floatp y) (lessp y 0))
	 (exp(times y (log x)))) ; bomb out for (-3)^4 or (-3)^4.0 or 0^y.
	(t ; y is integer, y>= 0
	   (prog (res)
		 (setq res 1)
	    loop
		 (cond ((equal y 0) (return res))
		       ((oddp y)(setq res (times  res x) y (sub1 y)))
		       (t (setq x (times x x) y (quotient y 2))))
		 (go loop)))))


;--- expt
; old
'(defun expt(x y)
  (prog (res)
	(setq res 1)
 loop	(cond ((equal y 0) (return res))
	      (t (setq res (times x res)
		       y (sub1 y))))
	(go loop)))

;--- fixp - l : lispval
;	returns t if l is a fixnum or bignum
;
(defun fixp (x) (or (equal (type x) 'fixnum)
		    (equal (type x) 'bignum)))


;--- floatp - l : lispval
;	returns t if l is a flonum
;
(defun floatp (x) (equal 'flonum (type x)))


;--- getchar,getcharn   - x : atom
;	     		- n : fixnum
; returns the n'th character of x's pname (the first corresponds to n=1)
; if n is out of bounds, nil is return
(def getchar
  (lambda (x n)
	  (cond ((lessp n 1) nil)
		(t (do ((i n (sub1 i))
			(lis (aexplodec x) (cdr lis)))
		       ((cond ((null lis) (return nil))
			      ((equal i 1) (return (car lis))))))))))

(def getcharn
  (lambda (x n)
	  (cond ((lessp n 1) nil)
		(t (do ((i n (sub1 i))
			(lis (aexploden x) (cdr lis)))
		       ((cond ((null lis) (return nil))
			      ((equal i 1) (return (car lis))))))))))


(def getl 
  (lambda (atm lis)
	  (do ((ll (cond ((atom atm) (plist atm))
			 (t (cdr atm)))
		   (cddr ll)))
	      ((null ll) nil)
	      (cond ((member (car ll) lis) (return ll))))))

;--- last - l : list
;	returns the last cons cell of the list, NOT the last element
;
(def last 
  (lambda (a)
	  (do ((ll a (cdr ll)))
	      ((null (cdr ll))  ll))))

;--- include - read in the file name given
;
(def include (nlambda (l) (load (car l))))

;--- length - l : list
;	returns the number of elements in the list.
;
(def length 
  (lambda ($l$)
	  (cond ((atom $l$) 0))
	  (do ((ll $l$ (cdr ll))
	       (i 0 (add1 i)))
	      ((null ll) i))))


;--- let - vb - binding forms
;	 - bd - body
; 	this macro allow one to express lambda binding for certain
;	variables and keep the information together.
;	the binding forms have this form
;	  (vrbl (vrbl2 val2) )
;	here vrbl will be bound to nil, and vrbl2 will be bound to the
;	result of evaluating val2
;	the general form using let is
;	(let (vrbl1 (vrbl2 val2))  
;	     .. body ..
;	)
;
(def let
  (macro (l)
	 `((lambda ,(mapcar '(lambda (x) (cond ((atom x) x)
					       (t (car x))))
			    (cadr l))
		   ,@(cddr l))
	   ,@(mapcar '(lambda (x) (cond ((atom x) nil)
				       (t (cadr x))))
		    (cadr l)))))

		   
;--- listify : n  - integer
;	returns a list of the first n args to the enclosing lexpr if
; n is positive, else returns the last -n args to the lexpr if n is
; negative.
;
(def listify 
  (macro (lis)
	 `(let ((n ,(cadr lis)))
	       (cond ((minusp n)
		      (do ((i (arg nil)  (1- i))
			   (result nil (cons (arg i) result)))
			  ((< i (+ (arg nil) n  1)) result) ))
		     (t (do ((i n  (1- i))
			     (result nil (cons (arg i) result)))
			    ((< i 1) result) ))))))
	 
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
				    (t (car def))))
		   (cond ((memq disc '(lambda lexpr nil))
			  (cons nam (mapcar 'macroexpand (cdr form))))
			 ((eq disc 'nlambda) form)
			 ((eq disc 'macro)
			  (setq form 
				(apply (cond ((bcdp def)
					      (mfunction (getentry def)
							 'nlambda))
					     (t (cons 'nlambda
						      (cdr def))))
				       form))
			  (go top))))))
	    (t (return (cons (macroexpand (car form))
			     (mapcar 'macroexpand (cdr form)))))))))


;--- max - arg1 arg2 ... : sequence of numbe
;	returns the maximum
;
(def max
  (lexpr (nargs)
	 (do ((i nargs (sub1 i))
	      (max (arg 1)))
	     ((lessp i 2) max)
	     (cond ((greaterp (arg i) max) (setq max (arg i)))))))




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
(def memq
  (lambda ($a$ $l$)
	  (do ((ll $l$ (cdr ll)))
	      ((null ll) nil)
	      (cond ((eq $a$ (car ll)) (return ll))))))

;--- min - arg1 ... numbers 
;
; 	returns minimum of n numbers. 
;

(def min
  (lexpr (nargs)
	 (do ((i nargs (sub1 i))
	      (min (arg 1)))
	     ((lessp i 2) min)
	     (cond ((lessp (arg i) min) (setq min (arg i)))))))

;--- nconc - x1 x2 ...: lists
;	The cdr of the last cons cell of xi is set to xi+1.  This is the
;	structure modification version of append
;
(def nconc 
  (lexpr (nargs) 
	 (cond ((zerop nargs) nil)
	       (t (do ((i 1 nxt)
		       (nxt 2 (add1 nxt))
		       (res (cons nil (arg 1)))) 
		      ((equal i nargs) (cdr res))
		      (cond ((arg i) (rplacd (last (arg i)) (arg nxt)))
			    (t (rplacd (last res) (arg nxt)))))))))


;--- nreverse - l : list
;	reverse the list in place
;
(defun nreverse (x)
    (cond ((null x) nil)
	  (t (n$reverse1 x nil))))

(defun n$reverse1 (x y)
    (cond ((null (cdr x)) (rplacd x y))
    (t (n$reverse1 (cdr x) (rplacd x y)))))

(def oddp
  (lambda (n)
	  (cond ((not (zerop (boole 1 1 n))) t))))

;--- plusp : x - number
; returns t iff x is greater than zero

(def plusp
  (lambda (x)
	  (greaterp x 0)))

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
(def shell (lambda nil (process csh)))



;--- signp - test - unevaluated atom
;	   - value - evaluated value
; test can be l, le, e, n, ge or g   with the obvious meaning
; we return t if value compares to 0 by test
(def signp
  (macro (l)
	 `(signphelpfcn ',(cadr l) ,(caddr l))))

;-- signphelpfcn
(def signphelpfcn
  (lambda (tst val)
	  (cond ((eq 'l tst) (minusp val 0))
		((eq 'le tst) (or (zerop val) (minusp val)))
		((eq 'e tst) (zerop val))
		((eq 'n tst) (not (zerop val)))
		((eq 'ge tst) (not (minusp val)))
		((eq 'g tst) (greaterp val 0)))))


;--- sload : fn - file name (must include the .l)
;	loads in the file printing each result as it is seen
;
(def sload
  (lambda (fn)
	  (prog (por)
		(cond ((setq por (infile fn)))
		      (t (patom '"bad file name")(terpr)(return nil)))
		(do ((x (read por) (read por)))
		    ((eq 'eof x))
		    (print x)
		    (eval x)))))

(defun sort(a fun)
  (prog (n)
	(cond	((null a) (return nil)) ;no elements
		(t
		 (setq n (length a))
		 (do i 1  (add1 i) (greaterp i n)(sorthelp a fun))
		 (return a) ))))

(defun sorthelp (a fun)
  (cond ((null (cdr a)) a)
        ((funcall fun (cadr a) (car a))  
	 (exchange2 a)
	 (sorthelp (cdr a) fun))
	(t (sorthelp (cdr a) fun))))

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
;	     patrn - sexp
; substitutes in patrn all occurances eq to old with new and returns the
; result
; MUST be put in the manual
(def subst
  (lambda (new old patrn)
	  (cond ((eq old patrn) new)
		((atom patrn) patrn)
		(t (cons (subst new old (car patrn))
			 (subst new old (cdr patrn)))))))

;--- xcons : a - sexp
;	     b - sexp
; returns (b . a)   that is, it is an exchanged cons
;
(def xcons  (lambda (a b) (cons b a)))

;---------------------------------------
; ARRAY functions .
;
(def array
  (macro ($lis$)
	 `(*array ',(cadr $lis$) ',(caddr $lis$) ,@(cdddr $lis$))))



; array access function 

(def arracfun
  (lambda (actlst ardisc)
	   (prog (diml ind val)

		 (setq actlst (mapcar 'eval actlst)
		       diml   (getaux ardisc))

		 (cond ((null (equal (length actlst)
				     (length (cdr diml))))
			(break '"Wrong number of indexes to array ref"))

		       (t (setq ind (arrcomputeind (cdr actlst)
						   (cddr diml)
						   (car actlst))
				val (arrayref ardisc ind))
			  (cond ((equal (car diml) t)
				 (setq val (eval val))))
			  (return val))))))




(def *array
  (lexpr (nargs)
	   (prog (name type rtype dims size tname)

		 (setq name  (arg 1)
		       type  (arg 2)
		       rtype (cond ((or (null type)
					(equal type t))
				    (setq type t)	; nil is equiv to t
				    'value)
				   (t type))
		       dims  (do ((i 3 (add1 i))
				  (res nil (cons (arg i) res)))
				 ((greaterp i nargs) (nreverse res)))
		       size  (apply 'times dims))

		 (setq tname (marray (segment rtype size)
				     (getd 'arracfun)
				     (cons type dims)
				     size
				     (sizeof rtype)))
		 (cond (name (set name tname)
			     (putd name tname)))
		 (return tname))))

(def arraycall
  (nlambda ($$lis$$)
	   ; form (arraycall type name sub1 sub2 ... subn)
	   ((lambda (ardisc)
		    (cond ((not (equal (car (getaux ardisc))) (car $$lis$$))
			   (patom '" Type given arraycall:")
			   (patom (car $$lis$$))
			   (patom '" doesnt match array type:")
			   (patom (car (getaux ardisc)))
			   (break nil)))
		    (apply (getaccess ardisc) 
			   (list (cddr $$lis$$) ardisc)))
	    (eval (cadr $$lis$$)))))
				
		       
			

; function to compute the raw array index

(def arrcomputeind
  (lambda (indl diml res)
	  (cond ((null diml) res)
		(t (arrcomputeind (cdr indl)
				  (cdr diml)
				  (plus (times res (car diml))
					(car indl)))))))

; store  
;  we make store a macro to insure that all parts are evaluated at the
; right time even if it is compiled.
;  (store (foo 34 i) (plus r f))
; gets translated to
;  (storeintern foo (plus r f) (list 34 i))
; and storeintern is a lambda, so when foo is evaluated it will pass the
;	array descriptor to storeintern, so storeintern can look at the
;	aux part to determine the type of array.
;
(defmacro store ( (arrname . indexes) value)
  (cond ((eq 'funcall arrname) 
	 (setq arrname `(eval ,(car indexes))
	       indexes (cdr indexes))))
  `(storeintern ,arrname ,value (list ,@indexes)))

(def storeintern
  (lambda (arrnam vl actlst)
	   (prog (loc)
		 (cond ((equal t (car (getaux arrnam)))
			(setq loc (arracfcnsimp actlst arrnam))
			(set loc vl))

		       (t (replace (apply arrnam actlst) vl)))
		 (return vl))))


(def arracfcnsimp
  (lambda (indexes adisc)
	  (prog (dims)
		(setq dims (cdr (getaux adisc)))
		(cond ((null (equal (length indexes)
				    (length dims)))
		       (break '"wrong number of indexes to array"))
		      (t (setq dims (arrcomputeind (cdr indexes)
						   (cdr dims)
						   (car indexes)))))
		(return (arrayref adisc dims)))))

(def arraydims (lambda (arg) (cond ((atom arg) (getaux (eval arg)))
				   ((arrayp arg) (getaux arg))
				   (t (break '"non array arg to arraydims")))))

; fill array from list or array

(def fillarray
  (lambda (arr lis)
	  (prog (maxv typept)
		(cond ((atom arr) (setq arr (eval arr))))

		(cond ((atom lis)
		       (setq lis (eval lis))
		       (return (fillarrayarray arr lis)))

		      ((arrayp lis) (return (fillarrayarray arr lis))))

		(setq maxv (sub1 (getlength arr))
		      typept (cond ((equal t (car (getaux arr)))
				    t)
				   (t nil)))
		(do ((ls lis)
		     (i 0 (add1 i)))
		    ((greaterp i maxv))

		    (cond (typept (set (arrayref arr i) (car ls)))
			  (t (replace (arrayref arr i) (car ls))))

		    (cond ((cdr ls) (setq ls (cdr ls))))))))

(def fillarrayarray
  (lambda (arrto arrfrom)
	  (prog (maxv)
		(setq maxv (sub1 (min (getlength arrto)
				      (getlength arrfrom))))
		(do ((i 0 (add1 i)))
		    ((greaterp i maxv))
		    (replace (arrayref arrto i) (arrayref arrfrom i))))))

;----------------------
; equivalences 

(putd 'abs (getd 'absval))
(putd 'add (getd 'sum))
(putd 'chrct (getd 'charcnt))
(putd 'diff (getd 'difference))
(putd 'numbp  (getd 'numberp))
(putd 'princ (getd 'patom))
(putd 'remainder (getd 'mod))
(putd 'terpri (getd 'terpr))
(putd 'typep (getd 'type))
(putd 'symeval (getd 'eval))
(putd '< (getd 'lessp))
(putd '= (getd 'equal))
(putd '> (getd 'greaterp))
(putd '- (getd 'difference))
(putd '"=" (getd 'equal))
(putd '"/" (getd 'quotient))
(putd '"+" (getd 'add))
(putd '"-" (getd 'difference))
(putd '*dif (getd 'difference))
(putd '\\ (getd 'mod)) 
(putd '"1+" (getd 'add1))
(putd '"1-" (getd 'sub1))
(putd '* (getd 'times))
(putd '*$ (getd 'times))
(putd '/$ (getd 'quotient))
(putd '+$ (getd 'add))
(putd '-$ (getd 'difference))
