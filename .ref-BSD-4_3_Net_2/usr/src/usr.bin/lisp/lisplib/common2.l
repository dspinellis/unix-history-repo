(setq rcs-common2-
   "$Header: common2.l,v 1.10 84/02/29 19:32:00 jkf Exp $")

;;
;; common2.l				-[Fri Feb  3 07:42:40 1984 by jkf]-
;;
;; lesser used functions
;;


(declare (macros t))

;--- process functions
; these functions permit the user to start up processes and either
; to either wait for their completion or to continue processing,
; communicating with them through a pipe.
;
; the main function, *process, is written in C.  These functions
; handle the common cases
;
;--- *process-send  :: start a process and return port to write to
;
(defun *process-send (command)
   (cadr (*process command nil t)))

;--- *process-receive :: start a process and return port to read from
;
(defun *process-receive (command)
   (car (*process command t)))

;--- process :: the old nlambda version of process
;  this function is kept around for compatibility
; use: (process command [frompipe [topipe]])
;  if the from and to pipes aren't given, run it and wait
;
(defun process fexpr (args)
   (declare (*args 1 3))
   (let ((command (car args))
	 (fromport (cadr args))
	 (toport (caddr args)))
      (cond ((null (cdr args)) (*process command))  ; call and wait
	    (t (let ((res (*process command fromport toport)))
		  (cond (fromport (set fromport (cadr res))))
		  (cond (toport (set toport (car res))))
		  ; return pid
		  (caddr res))))))


;--- msg : print a message consisting of strings and values
; arguments are:
;   N	    - print a newline
;   (N foo) - print foo newlines (foo is evaluated)
;   B       - print a blank
;   (B foo) - print foo blanks (foo is evaluated)
;   (P foo) - print following args to port foo (foo is evaluated)
;   (C foo) - go to column foo (foo is evaluated)
;   (T n)   - print n tabs
;   D	    - drain
;   other   - evaluate a princ the result (remember strings eval to themselves)

(defmacro msg (&rest msglist)
  (do ((ll msglist (cdr ll))
       (result)
       (cur nil nil)
       (curport nil)
       (current))
      ((null ll) `(progn ,@(nreverse result)))
      (setq current (car ll))
      (If (dtpr current)
	  then (If (eq (car current) 'N)
		   then (setq cur `(msg-tyo-char 10 ,(cadr current)))
		elseif (eq (car current) 'B)
		  then (setq cur `(msg-tyo-char 32 ,(cadr current)))
		elseif (eq (car current) 'T)
		  then (setq cur `(msg-tyo-char #\tab ,(cadr current)))
		elseif (eq (car current) 'P)
		   then (setq curport (cadr current))
		elseif (eq (car current) 'C)
		   then (setq cur `(tab (1- ,(cadr current))))
		else (setq cur `(msg-print ,current)))
       elseif (eq current 'N)
	  then (setq cur (list 'terpr))		; (can't use backquote
       elseif (eq current 'B)			; since must have new
      	  then (setq cur (list 'tyo 32))	; dtpr cell at end)
       elseif (eq current 'D)
	  then (setq cur '(drain))
       else (setq cur `(msg-print ,current)))
      (If cur 
	  then (setq result (cons (If curport then (nconc cur (ncons curport))
			                      else cur)
				  result)))))




(defun msg-tyo-char (ch n &optional (port nil))
  (do ((i n (1- i)))
      ((< i 1))
      (cond ((eq ch 10) (terpr port))
	    (t (tyo ch port)))))

(defun msg-print (item &optional (port nil))
   (patom item port))

;--- printblanks :: print out a stream of blanks to the given port
; (printblanks 'x_numberofblanks 'p_port)
;
(def printblanks
   (lambda (n prt)
      (let ((easy (memq n '( 0  ""
			     1  " "
			     2  "  "
			     3  "   "
			     4  "    "
			     5  "     "
			     6  "      "
			     7  "       "
			     8  "        "))))
	 (cond (easy (patom (cadr easy) prt))
	       (t (do ((i n (1- i)))
		      ((<& i 1))
		      (patom " " prt)))))))





; --- linelength [numb]
;
; sets the linelength (actually just varib linel) to the
; number given: numb
; if numb is not given, the current line length is returned
; 

(declare (special linel))

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
     (lambda (port) (- linel (nwritn port))))

;--- nthcdr :: do n cdrs of the list and return the result
;
; 
(defun nthcdr (index list)
   (cond ((fixp index)
	  (cond ((<& index 0)
		 (cons nil list))
		((=& index 0)
		 list)
		(t (nthcdr (1- index) (cdr list)))))
	 (t (error "Non fixnum first argument to nthcdr " index))))

;--- nthcdr (cmacro) :: version of nthcdr for use by the compiler
;
(defcmacro nthcdr (index list)
   (if (and (fixp index) (=& index 0))
      then list	; (nthcdr 0 expr) => expr
      else (let ((val (assq index '((1  . cdr)
				    (2  . cddr)
				    (3  . cdddr)
				    (4  . cddddr)
				    (5  . cdddddr)
				    (6  . cddddddr)))))
	      (cond (val `(,(cdr val) ,list))	; (nthcdr 1-6 list)
		    (t `(nthcdr ,index ,list)))))) ; other cases


;--- nth :: return nth element of the list
; cdr index times and then car to get the element.
; thus the first element is 0
;
(defun nth (index list)
   (car (nthcdr index list)))

;--- nth (cmacro) :: compiler macro to do the same thing
;
(defcmacro nth (index list)
   `(car (nthcdr ,index ,list)))

   


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


;this looks like funcall, so we will just use it
'(def apply* 
  (nlambda ($x$)
	(eval (cons (eval (car $x$)) (cdr $x$)))))

(putd 'apply* (getd 'funcall))

(defun remq (item list &optional (cnt -1))	;no tail recursion sucks.
   (let ((head nil)
	 (tail nil))
      (do ((l list (cdr l))
	   (newcell))
	  ((null l) head)
	  (cond ((or (not (eq (car l) item))
		     (=& 0 cnt))
		 (setq newcell (list (car l)))
		 (cond ((null head) (setq head newcell))
		       (t (rplacd tail newcell)))
		 (setq tail newcell))
		(t (setq cnt (1- cnt)))))))

(defun tab n
   (prog (nn prt over)
      (setq nn (arg 1))
      (cond ((>& n 1) (setq prt (arg 2))))
      (cond ((>& (setq over (nwritn prt)) nn)
	     (terpri prt)
	     (setq over 0)))
      (printblanks (- nn over) prt)))

;--- charcnt :: returns the number of characters left on the current line
; 	p - port
;(local function)
(def charcnt
     (lambda (port) (- linel (nwritn port))))

;(local function)
;
(declare (special $outport$))
(def $patom1 (lambda (x) (patom x $outport$)))

;;; --- cmu  functions ---
(def attach
   (lambda (x y)
	   (cond ((dtpr y) (rplacd y (cons (car y) (cdr y))) (rplaca y x))
		 (t (error "An atom can't be attached to " y)))))
(def Cnth
   (lambda (x n)
	   (cond ((> 1 n) (cons nil x))
		 (t
		    (prog nil
		     lp   (cond ((or (atom x) (eq n 1)) (return x)))
			  (setq x (cdr x))
			  (setq n (1- n))
			  (go lp))))))




(def dsubst
   (lambda (x y z)
	   (prog (b)
		 (cond ((eq y (setq b z)) (return (copy x))))
		 lp
		 (cond ((atom z) (return b))
		       ((cond ((symbolp y) (eq y (car z))) (t (equal y (car z))))
			(rplaca z (copy x)))
		       (t (dsubst x y (car z))))
		 (cond ((and y (eq y (cdr z))) (rplacd z (copy x)) (return b)))
		 (setq z (cdr z))
		 (go lp))))

(putd 'eqstr (getd 'equal))

(defun insert (x l comparefn nodups)
      (cond ((null l) (list x))
            ((atom l) (error "an atom, can't be inserted into" l))
            ((and nodups (member x l)) l)
	    (t (cond
                ((null comparefn) (setq comparefn (function alphalessp))))
               (prog (l1 n n1 y)
                     (setq l1 l)
                     (setq n (length l))
                a    (setq n1 (/ (add1 n) 2))
                     (setq y (Cnth l1 n1))
                     (cond ((< n 3)
                            (cond ((funcall comparefn x (car y))
                                   (cond
                                    ((not (equal x (car y)))
                                     (rplacd y (cons (car y) (cdr y)))
                                     (rplaca y x))))
                                  ((eq n 1) (rplacd y (cons x (cdr y))))
                                  ((funcall comparefn x (cadr y))
                                   (cond
                                    ((not (equal x (cadr y)))
                                     (rplacd (cdr y)
                                             (cons (cadr y) (cddr y)))
                                     (rplaca (cdr y) x))))
                                  (t (rplacd (cdr y) (cons x (cddr y))))))
                           ((funcall comparefn x (car y))
                            (cond
                             ((not (equal x (car y)))
                              (setq n (sub1 n1))
                              (go a))))
                           (t (setq l1 (cdr y)) (setq n (- n n1)) (go a))))
               l)))




(def kwote (lambda (x) (list 'quote x)))

(def lconc
     (lambda 
      (ptr x)
      (prog (xx)
            (return
             (cond ((atom x) ptr)
                   (t (setq xx (last x))
                      (cond ((atom ptr) (cons x xx))
                            ((dtpr (cdr ptr))
                             (rplacd (cdr ptr) x)
                             (rplacd ptr xx))
                            (t (rplaca (rplacd ptr xx) x)))))))))
(def ldiff
     (lambda (x y)
      (cond ((eq x y) nil)
            ((null y) x)
            (t
             (prog (v z)
                   (setq z (setq v (ncons (car x))))
              loop (setq x (cdr x))
                   (cond ((eq x y) (return z))
                         ((null x) (error "not a tail - ldiff")))
                   (setq v (cdr (rplacd v (ncons (car x)))))
                   (go loop))))))

(def lsubst
     (lambda (x y z)
      (cond ((null z) nil)
            ((atom z) (cond ((eq y z) x) (t z)))
            ((equal y (car z)) (nconc (copy x) (lsubst x y (cdr z))))
            (t (cons (lsubst x y (car z)) (lsubst x y (cdr z)))))))

(def merge
   (lambda (a b %%cfn)
      (declare (special %%cfn))
      (cond ((null %%cfn) (setq %%cfn (function alphalessp))))
      (merge1 a b)))

(def merge1
   (lambda (a b)
      (declare (special %%cfn))
      (cond ((null a) b)
	    ((null b) a)
	    (t
	       (prog (val end)
		  (setq val
			(setq end
			      (cond ((funcall %%cfn (car a) (car b))
				     (prog1 a (setq a (cdr a))))
				    (t (prog1 b (setq b (cdr b)))))))
		  loop (cond ((null a) (rplacd end b) (return val))
			     ((null b) (rplacd end a) (return val))
			     ((funcall %%cfn (car a) (car b))
			      (rplacd end a)
			      (setq a (cdr a)))
			     (t (rplacd end b) (setq b (cdr b))))
		  (setq end (cdr end))
		  (go loop))))))

(defmacro neq (a b) `(not (eq ,a ,b)))

(putd 'nthchar (getd 'getchar))
;(def nthchar
;     (lambda (x n)
;      (cond ((plusp n) (car (Cnth (explodec x) n)))
;            ((minusp n) (car (Cnth (reverse (explodec x)) (minus n))))
;            ((zerop n) nil))))

(defmacro quote! (&rest a) (quote!-expr-mac a))

(eval-when (compile eval load)
   
(defun quote!-expr-mac (form)
   (cond ((null form) nil)
	 ((atom form) `',form)
	 ((eq (car form) '!)
	  `(cons ,(cadr form) ,(quote!-expr-mac (cddr form))))
	 ((eq (car form) '!!)
	  (cond ((cddr form) `(append ,(cadr form)
				       ,(quote!-expr-mac (cddr form))))
		(t (cadr form))))
	 (t `(cons ,(quote!-expr-mac (car form))
		    ,(quote!-expr-mac (cdr form))))))

)

(defun remove (item list &optional (cnt -1))
  (let ((head '())
	(tail nil))
    (do ((l list (cdr l))
	 (newcell))
	((null l) head)
      (cond ((or (not (equal (car l) item))
		 (zerop cnt))
	     (setq newcell (list (car l)))
	     (cond ((null head) (setq head newcell))
		   (t (rplacd tail newcell)))
	     (setq tail newcell))
	    (t (setq cnt (1- cnt)))))))

(def subpair
     (lambda (old new expr)
      (cond (old (subpr expr old (or new '(nil)))) (t expr))))

(def subpr
   (lambda (expr l1 l2)
	   (prog (d a)
		 (cond ((atom expr) (go lp))
		       ((setq d (cdr expr)) (setq d (subpr d l1 l2))))
		 (setq a (subpr (car expr) l1 l2))
		 (return
		    (cond ((or (neq a (car expr))
			       (neq d (cdr expr))) (cons a d))
			  (t expr)))
		 lp   (cond ((null l1) (return expr))
			    (l2 (cond ((eq expr (car l1))
				       (return (car l2)))))
			    (t (cond ((eq expr (caar l1))
				      (return (cdar l1))))))
		 (setq l1 (cdr l1))
		 (and l2 (setq l2 (or (cdr l2) '(nil))))
		 (go lp))))
(def tailp
   (lambda (x y)
	   (and x
		(prog nil
		      lp   (cond ((atom y) (return nil)) ((eq x y) (return x)))
                 (setq y (cdr y))
                 (go lp)))))

(def tconc
     (lambda (p x)
      (cond ((atom p) (cons (setq x (ncons x)) x))
            ((dtpr (cdr p)) (rplacd p (cdr (rplacd (cdr p) (ncons x)))))
            (t (rplaca p (cdr (rplacd p (ncons x))))))))

;--- int:vector-range-error
; this is called from compiled code if a vector reference is made
; which is out of bounds.  it should print an error message and
; never return
(defun int:vector-range-error (vec index)
   (error "vector index out of range detected in compiled code "
	  (list vec index)))

;--- int:wrong-number-of-args-error :: pass wna error message to user
; this is called from compiled code (through wnaerr in the C interpreter)
; when it has been detected that the wrong number of arguments have
; been passed.  The state of the arguments are:
;	args 1 to (- n 3) are the acutal arguments
;	arg (- n 2) is the name of the function called
;	arg (- n 1) is the minimum number of arguments allowed
; 	arg n is the maximum number of arguments allowed
;		(or -1 if there is no maximum)
(defun int:wrong-number-of-args-error n
   (let ((max (arg n))
	 (min (arg (1- n)))
	 (name (arg (- n 2))))
      (do ((i (- n 3) (1- i))
	   (x)
	   (args))
	  ((<& i 1)
	   ; cases
	   ;  exact number
	   ;  min and max
	   ;  only a min
	   (if (=& min max)
	      then (setq x
		    (format nil
		     "`~a' expects ~r argument~p but was given ~@d:"
		     name min min (length args)))
	    elseif (=& max -1)
	      then (setq x
		    (format nil
		     "`~a' expects at least ~r argument~p but was given ~@d:"
		      name min min (length args)))
	      else (setq x
		    (format nil
		     "`~a' expects between ~r and ~r arguments but was given ~@d:"
		     name min max (length args))))
		   
	   (error x args))
	  (push (arg i) args))))   
;--- functions to retrieve parts of the vector returned by
;    filestat
;
(eval-when (compile eval)
   (defmacro filestat-chk (name index)
	     `(defun ,name (arg)
		      (cond ((vectorp arg)
			     (vref arg ,index))
			    (t (error (concat ',name '|: bad arg |) arg))))))
(filestat-chk filestat:mode	0)
(filestat-chk filestat:type	1)
(filestat-chk filestat:nlink	2)
(filestat-chk filestat:uid 	3)
(filestat-chk filestat:gid	4)
(filestat-chk filestat:size	5)
(filestat-chk filestat:atime	6)
(filestat-chk filestat:mtime	7)
(filestat-chk filestat:ctime	8)
(filestat-chk filestat:dev	9)
(filestat-chk filestat:rdev	10)
(filestat-chk filestat:ino	11)

;; lisp coded showstack and baktrace.
;;

(declare (special showstack-prinlevel showstack-prinlength
		  showstack-printer prinlevel prinlength))

(or (boundp 'showstack-prinlevel) (setq showstack-prinlevel 3))
(or (boundp 'showstack-prinlength) (setq showstack-prinlength 4))
(or (boundp 'showstack-printer)	(setq showstack-printer 'print))
(or (getd 'old-showstack) (putd 'old-showstack (getd  'showstack)))
(or (getd 'old-baktrace) (putd 'old-baktrace (getd  'baktrace)))

;--- showstack :: do a stack backtrace.
; arguments (unevaluated) are
;	t  - print trace expressions too (normally they are not printed)
;	N  - for some fixnum N, only print N levels.
;	len N - set prinlength to N
;	lev N - set prinlevel to N
;
(defun showstack fexpr (args)
   (showstack-baktrace args t))

(defun baktrace fexpr (args)
   (showstack-baktrace args nil))

(defun showstack-baktrace (args showstackp)
   (let ((print-trace nil)
	 (levels-to-print -1)
	 (prinlevel showstack-prinlevel)
	 (prinlength showstack-prinlength)
	 (res nil)
	 (newres nil)
	 (oldval nil)
	 (stk nil))
      ;; scan arguments
      (do ((xx args (cdr xx)))
	  ((null xx))
	  (cond ((eq t (car xx)) (setq print-trace t))
		((fixp (car xx)) (setq levels-to-print (car xx)))
		((eq 'lev (car xx))
		 (setq xx (cdr xx) prinlevel (car xx)))
		((eq 'len (car xx))
		 (setq xx (cdr xx) prinlength (car xx)))))
      ;; print the levels
      (do ((levs levels-to-print)
	   (firsttime t nil))
	  ((or (equal 0 stk)
	       (zerop levs))
	   (terpr))
	  (setq res (int:showstack stk))
	  (cond ((null res) (terpr) (return nil)))
	  (setq stk (cdr res)
		res (car res))
	  (cond ((or print-trace (not (trace-funp res)))
		 (cond ((and oldval showstackp)
			(setq newres (subst-eq '<**> oldval res)))
		       (t (setq newres res)))
		 (cond (showstackp (funcall showstack-printer newres) (terpr))
		       (t (baktraceprint newres firsttime)))
		 (setq levs (1- levs))
		 (setq oldval res))))))

(defun baktraceprint (form firsttime)
   (cond ((not firsttime) (patom " -- ")))
   (cond ((> (nwritn) 65) (terpr)))
   (cond ((atom form) (print form))
	 (t (let ((prinlevel 1)
		  (prinlength 2))
	       (cond ((dtpr form) (print (car form)))
		     (t (print form)))))))


;--- trace-funp  :: see if this is a trace function call
; return t if this call is a result of tracing a function, or of calling
; showstack
;
(defun trace-funp (expr)
   (or (and (symbolp expr)
	    (memq expr '(T-eval  T-apply T-setq
				 eval int:showstack showstack-baktrace)))
       (and (dtpr expr)
	    (cond ((symbolp (car expr))
		   (memq (car expr) '(trace-break T-cond T-eval T-setq
						  T-apply)))
		  ((dtpr (car expr))
		   (and (eq 'lambda (caar expr))
			(eq 'T-arglst (caadar expr))))))))

;--- subst-eq  :: replace parts eq to new with old
; make new list structure
;
(defun subst-eq (new old list)
   (cond ((eq old list)
	  new)
	 ((and (dtpr list)
	       (subst-eqp old list))
	  (cond ((eq old (car list))
		 (cons new (subst-eq new old (cdr list))))
		((dtpr (car list))
		 (cons (subst-eq new old (car list))
		       (subst-eq new old (cdr list))))
		(t (cons (car list)
			 (subst-eq new old (cdr list))))))
	 (t list)))

(defun subst-eqp (old list)
   (cond ((eq old list) t)
	 ((dtpr list)
	  (or (subst-eqp old (car list))
	      (subst-eqp old (cdr list))))
	 (t nil)))



;;; environment macros

(defmacro environment (&rest args)
   (do ((xx args (cddr xx))
	(when)(action)(res))
       ((null xx)
	`(progn 'compile
		,@(nreverse res)))
       (setq when (car xx)
	     action (cadr xx))
       (if (atom when)
	  then (setq when (ncons when)))
       (if (and (dtpr action)
		(symbolp (car action)))
	  then (setq action (cons (concat "environment-" (car action))
				  (cdr action))))
       (push `(eval-when ,when ,action) res)))
       

(defun environment-files fexpr (names)
   (mapc '(lambda (filename)
	     (if (not (get filename 'version)) then (load filename)))
	 names))

(defun environment-syntax fexpr (names)
   (mapc '(lambda (class)
	     (caseq class
		 (maclisp (cvttomaclisp))
		 (intlisp (cvttointlisp))
		 (ucilisp (cvttoucilisp))
		 ((franz franzlisp) (cvttofranzlisp))
		 (t (error "unknown syntax conversion type " class))))
	 names))

;--- standard environments
(defmacro environment-maclisp (&rest args)
   `(environment (compile load eval) (files machacks)
		 (compile eval) (syntax maclisp)
		 ,@args))


(defmacro environment-lmlisp (&rest args)
   `(environment (compile load eval) (files machacks lmhacks)
		 (compile eval) (syntax maclisp)
		 ,@args))

;;;--- i/o functions redefined.
; The common I/O functions are redefined here to do tilde expansion
; if the tilde-expansion symbol is non nil
(declare (special tilde-expansion))
   
;First, define the current <name> as int:<name>
;
(cond ((null (getd 'int:infile))
       (putd 'int:infile (getd 'infile))
       (putd 'int:outfile (getd 'outfile))
       (putd 'int:fileopen (getd 'fileopen))
       (putd 'int:cfasl (getd 'cfasl))
       (putd 'int:fasl (getd 'fasl))))

;Second, define the new functions:

(defun infile (filename)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "infile: non symbol or string filename " filename)))
   (cond (tilde-expansion (setq filename (tilde-expand filename))))
   (int:infile filename))

(defun outfile (filename &optional args)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "outfile: non symbol or string filename " filename)))
   (cond (tilde-expansion (setq filename (tilde-expand filename))))
   (int:outfile filename args))

;--- fileopen :: open a file with a non-standard stdio file
;  [this should probably be flushed because it depends on stdio,
;   which we may not use in the future]
(defun fileopen (filename mode)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "fileopen: non symbol or string filename " filename)))
   (cond (tilde-expansion (setq filename (tilde-expand filename))))
   (int:fileopen filename mode))

(defun fasl (filename &rest args)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "fasl: non symbol or string filename " filename)))
   (cond (tilde-expansion (setq filename (tilde-expand filename))))
   (lexpr-funcall 'int:fasl filename args))

(defun cfasl (filename &rest args)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "cfasl: non symbol or string filename " filename)))
   (cond (tilde-expansion (setq filename (tilde-expand filename))))
   (lexpr-funcall 'int:cfasl filename args))


;--- probef :: test if a file exists
;
(defun probef (filename)
   (cond ((not (or (symbolp filename) (stringp filename)))
	  (error "probef: non symbol or string filename " filename)))
   (sys:access filename 0))



(declare (special user-name-to-dir-cache))
(or (boundp 'user-name-to-dir-cache) (setq user-name-to-dir-cache nil))

;--- username-to-dir
; given a user name, return the home directory name
;
(defun username-to-dir (name)
   (cond ((symbolp name) (setq name (get_pname name)))
	 ((stringp name))
	 (t (error "username-to-dir: Illegal name " name)))
   (let ((val (assoc name user-name-to-dir-cache)))
      (cond ((null val)
	     (setq val (sys:getpwnam name))
	     (cond (val (push (cons name val) user-name-to-dir-cache))))
	    (t (setq val (cdr val))))
      (cond (val (sys:getpwnam-dir val)))))
		    
;--- username-to-dir-flush-cache :: clear all memory of where users are
; it is important to call this function upon startup to clear all
; knowledge of pathnames since this object file could have been copied
; from another machine
;
(defun username-to-dir-flush-cache ()
   (setq user-name-to-dir-cache nil))

;--- lisp interface to int:franz-call
;
(eval-when (compile eval)
   (setq fc_getpwnam 1   fc_access 2  fc_chdir 3  fc_unlink 4
	 fc_time   5     fc_chmod  6  fc_getpid 7 fc_stat  8
	 fc_gethostname 9 fc_link 10  fc_sleep 11 fc_nice 12))

;--- sys:getpwnam
; (sys:getpwnam 'st_username)
; rets vector: (t_name x_uid x_gid t_dir)
;
(defun sys:getpwnam (name)
   (cond ((or (symbolp name) (stringp name))
	  (int:franz-call #.fc_getpwnam name))
	 (t (error "sys:getpwnam : illegal name " name))))

; return dir portion
;
(defun sys:getpwnam-dir (vec) (vref vec 3))

(defun sys:access (name class)
   (cond ((and (or (symbolp name) (stringp name))
	       (fixp class))
	  (cond (tilde-expansion (setq name (tilde-expand name))))
	  (zerop (int:franz-call #.fc_access name class)))
	 (t (error "sys:access : illegal name or class " name class))))

(defun chdir (dir)
   (cond ((or (symbolp dir) (stringp dir))
	  (cond (tilde-expansion (setq dir (tilde-expand dir))))
	  (cond ((zerop (int:franz-call #.fc_chdir dir)))
		(t (error "cd: can't chdir to " dir))))
	 (t (error "chdir: illegal argument " dir))))

;--- sys:unlink :: unlink (remove) a file
;
(defun sys:unlink (name)
   (cond ((or (symbolp name) (stringp name))
	  (cond (tilde-expansion (setq name (tilde-expand name))))
	  (cond ((zerop (int:franz-call #.fc_unlink name)))
		(t (error "sys:unlink : unlink failed of " name))))
	 (t (error "sys:unlink : illegal argument " name))))

;--- sys:link :: make (hard) link to file
;
(defun sys:link (oldname newname)
   (cond ((or (symbolp oldname) (stringp oldname))
	  (cond (tilde-expansion (setq oldname (tilde-expand oldname))))
	  (cond ((or (symbolp newname) (stringp newname))
		 (cond (tilde-expansion (setq newname 
						(tilde-expand newname))))
		 (cond ((zerop (int:franz-call #.fc_link oldname newname)))
		       (t (error "sys:link : unlink failed of "
				 oldname newname))))
		(t (error "sys:unlink : illegal argument " newname))))
	 (t (error "sys:unlink : illegal argument " oldname))))

;--- sys:time :: return 'absolute' time in seconds
;
(defun sys:time ()
   (int:franz-call #.fc_time))

;--- sys:chmod :: change mode of file
; return t iff it succeeded.
;
(defun sys:chmod (name mode)
   (cond ((and (or (stringp name) (symbolp name))
	       (fixp mode))
	  (cond (tilde-expansion (setq name (tilde-expand name))))
	  (cond ((zerop (int:franz-call #.fc_chmod name mode)))
		(t (error "sys:chmod : chmod failed of " name))))
	 (t (error "sys:chmod : illegal argument(s): " name mode))))
   
(defun sys:getpid ()
   (int:franz-call #.fc_getpid))

(defun filestat (name)
   (let (ret)
      (cond ((or (symbolp name) (stringp name))
	     (cond (tilde-expansion (setq name (tilde-expand name))))
	     (cond ((null (setq ret (int:franz-call #.fc_stat name)))
		    (error "filestat : file doesn't exist " name))
		   (t ret)))
	    (t (error "filestat : illegal argument " name)))))

;--- sys:gethostname :: retrieve the current host name as a string
;
(defun sys:gethostname ()
   (int:franz-call #.fc_gethostname))

(defun sleep (seconds)
   ;; (sleep 'x_seconds)
   ;; pause for the given number of seconds
   (cond ((fixp seconds) (int:franz-call #.fc_sleep seconds))
	 (t (error "sleep: non-fixnum argument " seconds))))

(defun sys:nice (delta-priority)
   ;; modify the priority by the given amount
   (cond ((fixp delta-priority) (int:franz-call #.fc_nice delta-priority))
	 (t (error "sys:nice: non-fixnum argument " delta-priority))))
