;;
;; common2.l				-[Tue May 25 06:09:54 1982 by jkf]-
;;
;; lesser used functions
;;

(setq SCCS-common2 "@(#)common2.l	1.7	6/22/82")

(declare (macros t))

;--- msg - arg1 ...  arguments of the form described below
;	B - print out a blank
;	N - print out a newline (terpr)
;	(B n) - print out n blanks
;	(P p) - henceforth print on port p
;	string - patom this exactly (no evaluation)
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
		     ((and (eq 'B thisform) (setq thisform " ") nil))
		     ((eq 'N thisform) (cons `(terpr ,@outport)
					      (msgmake (cdr forms) outport)))
		     ((and (dtpr thisform)
			   (eq 'P (car thisform)))
		      (msgmake (cdr forms) `(,@(cdr thisform))))

		     ((and (dtpr thisform)
			   (eq 'B (car thisform)))
		      (cons `(printblanks ,@(cdr thisform) ,outport)
			     (msgmake (cdr forms) outport)))
		     (t (cons `(patom ,thisform ,@outport)
			       (msgmake (cdr forms) outport)))))
	    (car forms)))))

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




; ==============================================
;
;	(linelength [numb])
;
; sets the linelength (actually just varib linel) to the
; number given: numb
; if numb is not given, the current line length is returned
; =================================================

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
(def nthcdr
 (lambda (n x)
  (cond ((zerop n) x)
        ((lessp n 0) (cons nil x))
        (t (nthcdr (1- n) (cdr x) )))))


;--- macro versions of nthcdr and nth.  Theses can't be used until
; all in compiled code to nthcdr are flushed.
; We will just store them here until we can figure out how to do this.
;
(defmacro nthcdr-macro (n x)
  (cond ((numberp n)
	 (nthcdr-macro-internal n x))
	(t `(nthcdr* ,n ,x))))

(defun nthcdr-macro-internal (n x)
  (do ((x x `(cdr ,x))
       (i 0 (1+ i)))
      ((= i n) x)))

(defun nthcdr* (n list)
  (do ((i n (1- i))
       (l list (cdr l)))
      ((zerop i) l)))

(defmacro nth-macro (n list)
  (cond ((eq (typep n) 'fixnum)
	 `(car ,(nthcdr-macro-internal n list)))
	(t `(car (nthcdr* ,n ,list)))))


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

(def insert
     (lambda (x l comparefn nodups)
      (cond ((null l) (list x))
            ((atom l)
             (error "an atom, can't be inserted into" l))
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
                                    ((not
                                      (and nodups (equal x (car y))))
                                     (rplacd y (cons (car y) (cdr y)))
                                     (rplaca y x))))
                                  ((eq n 1) (rplacd y (cons x (cdr y))))
                                  ((funcall comparefn x (cadr y))
                                   (cond
                                    ((not
                                      (and nodups (equal x (cadr y))))
                                     (rplacd (cdr y)
                                             (cons (cadr y) (cddr y)))
                                     (rplaca (cdr y) x))))
                                  (t (rplacd (cdr y) (cons x (cddr y))))))
                           ((funcall comparefn x (car y))
                            (cond
                             ((not (and nodups (equal x (car y))))
                              (setq n (sub1 n1))
                              (go a))))
                           (t (setq l1 (cdr y)) (setq n (- n n1)) (go a))))
               l))))

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
     (lambda 
      (a b %%cfn)
      (cond ((null %%cfn) (setq %%cfn (function alphalessp))))
      (merge1 a b)))

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

(def remove
     (lambda (elt list)
      (cond ((atom list) list)
            ((equal (car list) elt) (remove elt (cdr list)))
            ((cons (car list) (remove elt (cdr list)))))))

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

;;; --- setf macro
;
;(setf (cadr x) 3) --> (rplaca (cdr x) 3)

(defmacro setf (expr val &rest rest)
	  (cond ((atom expr)
		 (or (symbolp expr)
		     (error '|-- setf can't handle this.| expr))
		 `(setq ,expr ,val))
		(t
		 (do ((y)) (())
		     (or (symbolp (car expr))
			 (error '|-- setf can't handle this.| expr))
		     (and (setq y (get (car expr) 'setf-expand))
			  (return (apply y `(,expr ,val ,@rest))))
		     (or (and
			   (setq y (getd (car expr)))
			   (or (and (dtpr y)
				    (eq (car y) 'macro))
			       (and (bcdp y)
				    (eq (getdisc y) 'macro))))
			 (error '|-- setf can't handle this.| expr))
		     (setq expr (apply y expr))))))

(defmacro defsetf (name vars &rest body)
	  `(eval-when 
	    (compile load eval)
	    (defun (,name setf-expand) ,vars . ,body)))

(defsetf car (e v) `(rplaca ,(cadr e) ,v))
(defsetf caar (e v) `(rplaca (car ,(cadr e)) ,v))
(defsetf cadr (e v) `(rplaca (cdr ,(cadr e)) ,v))
(defsetf caaar (e v) `(rplaca (caar ,(cadr e)) ,v))
(defsetf cadar (e v) `(rplaca (cdar ,(cadr e)) ,v))
(defsetf caadr (e v) `(rplaca (cadr ,(cadr e)) ,v))
(defsetf caddr (e v) `(rplaca (cddr ,(cadr e)) ,v))
(defsetf caaaar (e v) `(rplaca (caaar ,(cadr e)) ,v))
(defsetf cadaar (e v) `(rplaca (cdaar ,(cadr e)) ,v))
(defsetf caadar (e v) `(rplaca (cadar ,(cadr e)) ,v))
(defsetf caddar (e v) `(rplaca (cddar ,(cadr e)) ,v))
(defsetf caaadr (e v) `(rplaca (caadr ,(cadr e)) ,v))
(defsetf cadadr (e v) `(rplaca (cdadr ,(cadr e)) ,v))
(defsetf caaddr (e v) `(rplaca (caddr ,(cadr e)) ,v))
(defsetf cadddr (e v) `(rplaca (cdddr ,(cadr e)) ,v))
(defsetf cdr (e v) `(rplacd ,(cadr e) ,v))
(defsetf cdar (e v) `(rplacd (car ,(cadr e)) ,v))
(defsetf cddr (e v) `(rplacd (cdr ,(cadr e)) ,v))
(defsetf cdaar (e v) `(rplacd (caar ,(cadr e)) ,v))
(defsetf cddar (e v) `(rplacd (cdar ,(cadr e)) ,v))
(defsetf cdadr (e v) `(rplacd (cadr ,(cadr e)) ,v))
(defsetf cdddr (e v) `(rplacd (cddr ,(cadr e)) ,v))
(defsetf cdaaar (e v) `(rplacd (caaar ,(cadr e)) ,v))
(defsetf cddaar (e v) `(rplacd (cdaar ,(cadr e)) ,v))
(defsetf cdadar (e v) `(rplacd (cadar ,(cadr e)) ,v))
(defsetf cdddar (e v) `(rplacd (cddar ,(cadr e)) ,v))
(defsetf cdaadr (e v) `(rplacd (caadr ,(cadr e)) ,v))
(defsetf cddadr (e v) `(rplacd (cdadr ,(cadr e)) ,v))
(defsetf cdaddr (e v) `(rplacd (caddr ,(cadr e)) ,v))
(defsetf cddddr (e v) `(rplacd (cdddr ,(cadr e)) ,v))
(defsetf cxr (e v) `(rplacx ,(cadr e) ,(caddr e) ,v))

(defsetf nth (e v) `(rplaca (nthcdr ,(cadr e) ,(caddr e)) ,v))

(defsetf arraycall (e v) `(store ,e ,v))
(defsetf get (e v) `(putprop ,(cadr e) ,v ,(caddr e)))

(defsetf plist (e v) `(setplist ,(cadr e) ,v))

(defsetf symeval (e v) `(set ,(cadr e) ,v))

(defsetf arg (e v) `(setarg ,(cadr e) ,v))

(defsetf args (e v) `(args ,(cadr e) ,v))


;--- functions to retrieve parts of the list returned by
;    filestat
;
(eval-when (compile eval)
   (defmacro filestat-chk (name accessfcn)
	     `(defun ,name (arg)
		      (cond ((and arg (dtpr arg))
			     (,accessfcn arg))
			    (t (error (concat ',name '|: bad arg |) arg))))))
(filestat-chk filestat:mode	car)
(filestat-chk filestat:type	cadr)
(filestat-chk filestat:nlink	caddr)
(filestat-chk filestat:uid 	cadddr)
(filestat-chk filestat:gid	caddddr)
(filestat-chk filestat:size	cadddddr)
(filestat-chk filestat:atime	caddddddr)
(filestat-chk filestat:mtime	cadddddddr)
(filestat-chk filestat:ctime	caddddddddr)
(filestat-chk filestat:dev	cadddddddddr)
(filestat-chk filestat:rdev	caddddddddddr)
(filestat-chk filestat:ino	cadddddddddddr)

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

