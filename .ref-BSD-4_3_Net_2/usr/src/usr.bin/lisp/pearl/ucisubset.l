;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ucisubset.l ;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for a subset of UCI Lisp that are either used by PEARL
;     or were needed by PEARL users at Berkeley.
; This was purposely designed to interfere as little as necessary
;     with Franz Lisp, so things like the standard UCI do macro
;     and the Charniak (et al) let macro are not provided.
; Includes what used to be sprint.l (at the end).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

(eval-when (compile)
  (declare (special defmacro-for-compiling *savedefs*))
  (setq defmacro-for-compiling t)
  (setq *savedefs* nil))

(declare (macros t))

(defvar poport)
(defvar pparm1 50)
(defvar pparm2 100)
(defvar lpar)
(defvar rpar)
(defvar form)
(defvar linel)
(defvar *outport* nil)
(defvar *fileopen*)
(defvar prettyprops '((comment . pp-comment)
		      (function . pp-function)
		      (value . pp-value)))

(declare (localf *patom1))

(defvar *file* nil)
(defvar *oldfunctiondefinition*)
(defvar *savedefs* t)

(defmacro funl (&rest rest)
  `(function (lambda .,rest)))

;
; ucilisp (de df dm) declare function macros.
;
; (DE name args body) -> declare exprs and lexprs.
;   If *savedefs* is t and function has previous definition,
;   save it under the property OLDDEF, and return '(name Redefined).
;   Otherwise, just do a defun and return name (as with defun).
;
(defun de macro (l)
  (cond (*savedefs*
	 `(progn 'compile
		 (setq *oldfunctiondefinition* (getd ',(cadr l)))
		 (defun .,(cdr l))
		 (and *file*
		      (putprop ',(cadr l) *file* 'sourcefile))
		 (cond (*oldfunctiondefinition*
			(putprop ',(cadr l) *oldfunctiondefinition* 'olddef)
			(list ',(cadr l) 'Redefined))
		       ( t ',(cadr l)))))
	( t `(defun .,(cdr l)))))
  
;
; (df name args body) -> declare fexprs.
;
(defun df macro (l) 
  (cond (*savedefs*
	 `(progn 'compile
		 (setq *oldfunctiondefinition* (getd ',(cadr l)))
		 (defun ,(cadr l) fexpr .,(cddr l))
		 (and *file*
		      (putprop ',(cadr l) *file* 'sourcefile))
		 (cond (*oldfunctiondefinition*
			(putprop ',(cadr l) *oldfunctiondefinition* 'olddef)
			(list ',(cadr l) 'Redefined))
		       ( t ',(cadr l)))))
	( t `(defun ,(cadr l) fexpr .,(cddr l)))))

;
; macro's are not compiled except under the same
;	conditions as in franz lisp.
;	(usually just do (declare (macros t))
;		to have macros also compiled).
;
;
; (dm name args body) -> declare macros. same as (defun name 'macro body)
;
(defun dm macro (l) 
  (cond (*savedefs*
	 `(progn 'compile
		 (setq *oldfunctiondefinition* (getd ',(cadr l)))
		 (defun ,(cadr l) macro .,(cddr l))
		 (and *file*
		      (putprop ',(cadr l) *file* 'sourcefile))
		 (cond (*oldfunctiondefinition*
			(putprop ',(cadr l) *oldfunctiondefinition* 'olddef)
			(list ',(cadr l) 'Redefined))
		       ( t ',(cadr l)))))
	( t `(defun ,(cadr l) macro .,(cddr l)))))

; UCI Lisp character macros are non-separating when occurring in 
;    the middle of atoms.
(eval-when (compile load eval)
  (add-syntax-class 'vucisplicemacro
		    '(csplicing-macro escape-when-first))
  (add-syntax-class 'vucireadmacro
		    '(cmacro escape-when-first)))

;
; ucilisp functions which declare character macros.
;
;
; dsm - declare splicing read macro.
;
(defun dsm macro (l) 
  (cond (*savedefs*
	 `(progn 'compile
		 (setq *oldfunctiondefinition*
		       (and (memq (getsyntax ',(cadr l))
				  '(vucireadmacro vucisplicemacro
						  vsplicing-macro vmacro))
			    (get ',(cadr l) readtable)))
		 (eval-when (compile load eval)
			    (setsyntax ',(cadr l) 'vucisplicemacro ',(caddr l)))
		 
		 (and *file*
		      (putprop ',(cadr l) *file* 'sourcefile))
		 (cond (*oldfunctiondefinition*
			(putprop ',(cadr l) *oldfunctiondefinition* 'oldmacro)
			(list ',(cadr l) 'Redefined))
		       ( t ',(cadr l)))))
	( t `(eval-when (compile load eval)
			(setsyntax ',(cadr l) 'vucisplicemacro ',(caddr l))))))

;
; drm - declare read macro.
;
(defun drm macro (l) 
  (cond (*savedefs*
	 `(progn 'compile
		 (setq *oldfunctiondefinition*
		       (and (memq (getsyntax ',(cadr l))
				  '(vucireadmacro vucisplicemacro
						  vsplicing-macro vmacro))
			    (get ',(cadr l) readtable)))
		 (eval-when (compile load eval)
			    (setsyntax ',(cadr l) 'vucireadmacro ',(caddr l)))
		 
		 (and *file*
		      (putprop ',(cadr l) *file* 'sourcefile))
		 (cond (*oldfunctiondefinition*
			(putprop ',(cadr l) *oldfunctiondefinition* 'oldmacro)
			(list ',(cadr l) 'Redefined))
		       ( t ',(cadr l)))))
	( t `(eval-when (compile load eval)
			(setsyntax ',(cadr l) 'vucireadmacro ',(caddr l))))))

;
; ucilisp selectq function. (written by jkf)
;
(defun selectq* macro (form)
  ((lambda (x)
	   `((lambda (,x)
		     (cond 
		      ,@(maplist 
			 (function
			  (lambda (ff)
				  (cond ((null (cdr ff))
					 `( t  ,(car ff)))
					((atom (caar ff))
					 `((eq ,x ',(caar ff))
					   . ,(cdar ff)))
					(t
					 `((memq ,x ',(caar ff))
					   . ,(cdar ff))))))
			 (cddr form))))
	     ,(cadr form)))
   (gensym 'z)))

(defun some macro (l)
  `((lambda (f a)
	    (prog ()
		  loop
		  (cond ((null a) (return nil))
			((funcall f (car a))
			 (return a))
			( t (setq a (cdr a))
			    (go loop)))))
    ,(cadr l)
    ,(caddr l)))

(defmacro subset (fun lis)
  `(mapcan (function (lambda (ele)
			     (cond ((funcall ,fun ele) (ncons ele)))))
	   ,lis))
  
(defun length (l)
  (prog (n)
	(setq n 0)
	loop
	(and (atom l) 
	     (return n))
	(setq l (cdr l))
	(setq n (1+ n))
	(go loop)))

(defmacro apply* (fcn args)
  `(prog (fcndef)
	 (return
	  (cond ((atom ,fcn)
		 (or (and (eq 'binary (type ,fcn))
			  (setq fcndef ,fcn))
		     (setq fcndef (getd ,fcn)))
		 (cond ((or (and (eq 'binary (type fcndef))
				 (eq 'macro (getdisc fcndef)))
			    (and (dtpr fcndef)
				 (eq 'macro (car fcndef))))
			(funcall ,fcn (cons ,fcn ,args)))
		       ( t (apply ,fcn ,args))))
		( t (apply ,fcn ,args))))))

(defmacro every (fcn args)
  `(prog (kkkk)
	 (setq kkkk ,args)
	 loop
	 (cond ((null kkkk)
		(return t))
	       ((apply* ,fcn (list (pop kkkk)))
		(go loop)))
	 (return nil)))

(defun timer fexpr (request)
  (let ((timein (ptime)) timeout result cpu garbage)
       (prog ()
	     loop
	     (setq result (eval (car request)))
	     (and (setq request (cdr request))
		  (go loop)))
       (setq timeout (ptime))
       (setq cpu (quotient (fix (times 1000
				       (quotient (difference (car timeout)
							     (car timein))
						 60.0)))
			   1000.0))
       (setq garbage (quotient (fix (times 1000
					   (quotient (difference (cadr timeout)
								 (cadr timein))
						     60.0)))
			       1000.0))
       (print (cons cpu garbage))
       (terpri)
       result))

(putd 'consp (getd 'dtpr))

(putd 'msgprintfn (getd 'patom))
  
;
; ucilisp msg function. (written by jkf)
;
(defmacro msg ( &rest body)
  `(progn ,@(mapcar 
	     (function
	      (lambda (form)
		      (cond ((eq form t) '(line-feed 1))
			    ((numberp form)
			     (cond ((>& form 0) 
				    `(msg-space ,form))
				   ( t `(line-feed ,(minus form)))))
			    ((atom form) `(msgprintfn ,form))
			    ((eq (car form) t) '(msgprintfn '\	))
			    ((eq (car form) 'e) 
			     `(msgprintfn ,(cadr form)))
			    ( t `(msgprintfn ,form)))))
	     body)
	  nil)) ; return nil!
  
;
; this NEED NOT be fixed to not use do.
;
(defmacro msg-space (n)
  (cond ((eq 1 n) '(patom '" "))
	( t `(do i ,n (1- i) (<& i 1) (patom '\ ))))) 

(defmacro line-feed (n)
  (cond ((eq 1 n) '(terpr))
	( t `(do i ,n (1- i) (<& i 1) (terpr)))))

; compatability functions: functions required by uci lisp but not
;	present in franz
;
; union uses the franz do loop (not the ucilisp one).

(defvar membfn 'member)

(defun union n
  (and (> n 0)
       (do ((res (ncons nil))
	    (i 1 (1+ i)))
	   ((eq i (1+ n)) (car res))
	   (mapc (function
		    (lambda (arg)
		       (or (apply* membfn (list arg (car res)))
			   (tconc res arg))))
		 (arg i)))))

(defun enter (v l)
  (cond ((apply* membfn (list v l)) l)
	( t (cons v l))))

(defun append2 (a b &aux (c (ncons nil)))
      (do ((a a (cdr a)))
	  ((null a))
	  (tconc c (car a)))
      (rplacd (cdr c) b)
      (car c))

(putd 'noduples (getd 'union))
(putd 'append* (getd 'append))
(putd '*append (getd 'append))
(putd '*dif (getd 'diff))
(putd '*eval (getd 'eval))
(putd '*great (getd 'greaterp))
(putd '*less (getd 'lessp))
(putd '*max (getd 'max))
(putd '*nconc (getd 'nconc))
(putd '*plus (getd 'plus))
(putd '*times (getd 'times))
(putd 'expandmacro (getd 'macroexpand))
(putd 'mapcl (getd 'mapcar))
(putd 'memb (getd 'member))

(dm clrbfi () 
 '(drain piport))

(defun save fexpr (l)
  (let ((fcnname (car l)))
       (putprop fcnname (getd fcnname) 'olddef)))

(defun unsave fexpr (l) 
  (let* ((name (car l))
	 (old (get name 'olddef)))
	(and old
	     (putprop name (getd name) 'olddef)
	     (putd name old))
	old))

(putd 'atcat (getd 'concat))

(putd 'gt (getd '>))
(putd 'lt (getd '<))

(defun le macro (x)
  `(not (> .,(cdr x))))

(defun ge macro (x)
  `(not (< .,(cdr x))))

(defun litatom macro (x)
  `(and (atom .,(cdr x))
	(not (numberp .,(cdr x)))))

(putd 'peekc (getd 'tyipeek))

;
;	unbound - (setq x (unbound)) will unbind x.
; "this [code] is sick" - jkf.
;
(defun unbound macro (l)
  `(fake -4))

(or (getd 'franzboundp)
  (putd 'franzboundp (getd 'boundp)))

(defun boundp (item)
  (cond ((arrayp item))
	((franzboundp item))))

(defvar *dskin* t)
(defvar piport)

;(eval-when (load eval compile)
;  (or (boundp '*dskin*)
;      (setq *dskin* t)))

(eval-when (load eval)
  (or (getd 'dskprintfn)
      (putd 'dskprintfn (getd 'patom))))

(defun dskin fexpr (l)
  (mapc 'dskin1 l)
  (terpri) t )

(defun dskin1 (*file*)
  (prog (port)
	(terpri)
	(patom '|>>>|)
	(cond ((null (setq port (car (errset (infile *file*) nil))))
	             (patom '|couldn't open file |)
		     (patom *file*))
	      ( t (patom *file*)
		  (patom '| |)
		  (dskin2 port)
		  (close port)))))

(defun dskin2 (port)
  (prog (expr value)
    loop
      (cond ((null (setq expr (read port))) nil)
	    ( t (cond ((memq (car expr) '(de df defmacro dm drm
					     dsm setq def defun))
		       (cond ((memq *dskin* '(name both))
			      (patom (cadr expr))
			      (patom '|: |))))
		      ((eq (car expr) 'create)
		       (cond ((memq *dskin* '(name both))
			      (patom (caddr expr))
			      (patom '|: |)))))
		(setq value (eval expr))
		(and (memq *dskin* '(t both))
		     (or (eq value '*invisible*)
			 (progn (dskprintfn value)
				(patom '| |))))
		(go loop)))))

(defun nequal (arg1 arg2)
  (not (equal arg1 arg2)))

(defun readl fexpr (l)
  (cond ((null l) (readl1 nil))
	( t (readl1 (eval (car l))))))

(putd 'lineread (getd 'readl))

(defun readl1 (flag)
  (cond ((not (and flag
		   (eq (tyipeek) 10)
		   (tyi)))
	 (prog (input)
	       (setq input (ncons nil))  ; initialize for tconc.
	       loop
	       (cond ((not (eq (tyipeek) 10))
		      (tconc input (read))
		      (go loop))
		     ( t ; the actual list is in the CAR.
			 (tyi)
			 (return (car input))))))))

(defun defv fexpr (l)
  (set (car l) (cadr l)))

(defun remprops (item proplist)
  (mapc (funl (prop)
	      (remprop item prop))
	proplist)
  nil)

(defun addprop (id value prop)
  (putprop id (enter value (get id prop)) prop))

(defun nconc1 (l elmt)
  (rplacd (last l) (cons elmt nil)))

(defun dremove (elmt l)
  (let (newl)
       (cond ((dtpr l)
	      (cond ((eq elmt (car l))
		     (setq newl (delq elmt l))
		     (rplaca l (car newl))
		     (rplacd l (cdr newl)))
		    ( t (delq elmt l))))
	     ( t l))))

(defun intersection (set1 set2)
  (prog (inter)
	(mapc (funl (elt) (putprop elt t '*inter*)) set1)
	(mapc (funl (elt) (and (get elt '*inter*)
			       (setq inter (cons elt inter))))
	      set2)
	(mapc (funl (elt) (remprop elt '*inter*)) set1)
	(return inter)))

(defun initsym1 expr (l)
  (prog (num)
	(cond ((dtpr l)
	       (setq num (cadr l))
	       (setq l (car l)))
	      ( t (setq num 0)))
	(putprop l num 'symctr)
	(return (concat l num))))

(defun initsym fexpr (l)
  (mapcar (function initsym1) l))

(defun newsym fexpr (l)
  (let ((name (car l)))
       (concat name
	       (putprop name
			(1+ (or (get name 'symctr)
				-1))
			'symctr))))

(defun oldsym fexpr (l)
  (let ((sym (car l)))
       (concat sym (get sym 'symctr))))

(defun allsym fexpr (l)
  (prog (num symctr syms)
	(cond ((dtpr (car l))
	       (setq num (cadar l))
	       (setq l (caar l)))
	      ( t (setq num 0)
		  (setq l (car l))))
	(or (setq symctr (get l 'symctr))
	    (return))
	loop
	(and (>& num symctr)
	     (return syms))
	(setq syms (cons (concat l symctr) syms))
	(setq symctr (1- symctr))
	(go loop)))

(defun remsym1 expr (l)
  (prog1 (funcall (function oldsym)
		  (cond ((dtpr (car l)) (car l))
			( t  l)))
	 (mapc (function remob) (apply (function allsym) l))
	 (cond ((dtpr (car l)) (putprop (caar l) (1- (cadar l)) 'symctr))
	       ( t (remprop (car l) 'symctr)))))

(defun remsym fexpr (l)
  (maplist (function remsym1) l))

(defun symstat fexpr (l)
  (mapcar (funl (k)
		(list k (get k 'symctr)))
	  l))

(defun suflist (itemlist num)
  (cond ((dtpr itemlist) (nth (1+ num) itemlist))))

;;;;;;;;;;;;;;;;;;;;;;; (formerly sprint.l) ;;;;;;;;;;;;;;;;;;;;;;;;
;  A few additions to the library file ucbpp.l, mostly to add
;  a UCI Lisp-like "sprint" including some modifications for
;     more flexible printmacros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Moved to front and converted to defvar.
; (declare (special poport pparm1 pparm2 lpar rpar form linel))
; (declare (localf *patom1))
; (declare (special *outport* *fileopen* prettyprops))

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
; THEN, hacked to use for PEARL.

; moved to front.
;(setq prettyprops '((comment . pp-comment)
;		    (function . pp-function)
;		    (value . pp-value)))

; printret is like print yet it returns the value printed, this is used
; by pp		
(def printret
  (macro (*l*)
	 `(progn (print ,@(cdr *l*)) ,(cadr *l*))))

(def pp
  (nlambda (*xlist*)
	(prog (*outport* *cur* *fileopen* *prl* *atm*)

	      (setq *outport* poport)			; default port
	      ; check if more to do, if not close output file if it is
	      ; open and leave


   toploop    (cond ((null (setq *cur* (car *xlist*)))
		     (condclosefile)
		     (terpr)
		     (return t)))

	      (cond ((dtpr *cur*)
		     (cond ((equal 'P (car *cur*))	; specifying a port
			    (condclosefile)		; close file if open
			    (setq *outport* (eval (cadr *cur*))))

			   ((equal 'F (car *cur*))	; specifying a file
			    (condclosefile)		; close file if open
			    (setq *outport* (outfile (cadr *cur*))
				  *fileopen* t))

						
			   ((equal 'E (car *cur*))
			    (eval (cadr *cur*)))

			   ( t (terpri *outport*)
			       (*prpr *cur*)))	;-DNC inserted
		     (go botloop)))


      (mapc (function
	       (lambda (prop)
		       (prog (printer)
			     (cond ((dtpr prop)
				    (setq printer (cdr prop))
				    (setq prop (car prop)))
				   ( t (setq printer 'pp-prop)))
			     (cond ((eq 'value prop)
				    (cond ((boundp *cur*)
					   (apply printer
						  (list *cur*
							(eval *cur*)
							'value)))))
				   ((eq 'function prop)
				    (cond ((and (getd *cur*)
						(not (bcdp (getd *cur*))))
					   (apply printer
						  (list *cur*
							(getd *cur*)
							'function)))))
				   ((get *cur* prop)
				    (apply printer
					   (list *cur*
						 (get *cur* prop)
						 prop)))))))
	    prettyprops)


 botloop      (setq *xlist* (cdr *xlist*))

	      (go toploop))))

; moved to front.
;(setq pparm1 50 pparm2 100)

;   -DNC These "prettyprinter parameters" are used to decide when we should
;	quit printing down the right margin and move back to the left -
;	Do it when the leftmargin > pparm1 and there are more than pparm2
;	more chars to print in the expression

; cmu prefers dv instead of setq

#+cmu
(def pp-value (lambda (i v p)
		      (terpri *outport*) (*prpr (list 'dv i v))))

#-cmu
(def pp-value (lambda (i v p)
		      (terpr *outport*) (*prpr `(setq ,i ',v))))
(def pp-function (lambda (i v p)
			 (terpri *outport*) (*prpr (list 'def i v))))
(def pp-prop (lambda (i v p)
		     (terpri *outport*) (*prpr (list 'defprop i v p))))

(def condclosefile 
  (lambda nil
	  (cond (*fileopen*
		 (terpr *outport*)
		 (close *outport*)
		 (setq *fileopen* nil)))))

;
; these routines are meant to be used by pp but since
; some people insist on using them we will set *outport* to nil
; as the default (moved to front).
;(setq *outport* nil)


(def *prpr 
  (lambda (x)
	  (cond ((not (boundp '*outport*)) (setq *outport* poport)))
	  (terpr *outport*)
	  (*prdf x 0 0)))

; This is the principle addition for PEARL.
; SPRINT simply calls *prdf after filling in any missing parameters.
(defun sprint (value &optional (lmar 0) (rmar 0))
  (cond ((not (boundp '*outport*)) (setq *outport* poport)))
  (*prdf value lmar rmar))

(defvar rmar)	; -DNC this used to be m - I've tried to
		; to fix up the pretty printer a bit.  It
		; used to mess up regularly on (a b .c) types
		; of lists.  Also printmacros have been added.



; Used to be $prdf but added a bit and changed to * to avoid
;   PEARL's history read macro $.
(def *prdf
  (lambda (l lmar rmar)
    (prog (pmac)
;
;			- DNC - Here we try to fix the tendency to print a
;			  thin column down the right margin by allowing it
;			  to move back to the left if necessary.
;
	  (cond ((and (>& lmar pparm1) (>& (flatc l (1+ pparm2)) pparm2))
		 (terpri *outport*)
		 (princ '"; <<<<< start back on the left <<<<<" *outport*)
		 (*prdf l 5 0)
		 (terpri *outport*)
		 (princ '"; >>>>> continue on the right >>>>>" *outport*)
		 (terpri *outport*)
		 (return nil)))
          (tab lmar *outport*)
     a    (cond ((and (dtpr l)
                      (atom (car l))
                      (setq pmac (get (car l) 'printmacro))
		      (cond ((stringp pmac)
			     ; Added for PEARL (and UCI Lisp compatibility).
			     ; a string printmacro means print this
			     ;   string and then the cadr of l if
			     ;   it's not nil, and only if l is
			     ;   a one- or two-element list.
			     (cond ((cddr l) ; more than two elements.
				    nil)
				   ((null (cdr l)) ; only one element.
				    (patom pmac)
				    t)
				   ( t (patom pmac)  ; two elements.
				       (patom (cadr l))
				       t)))
			    ( t (apply pmac (list l lmar rmar)))))
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
                 (return (printret l *outport*)))
                ((<& (+ rmar (flatc l (chrct *outport*)))
                        (chrct *outport*))
;
;	This is just a heuristic - if print can fit it in then figure that
;	the printmacros won't hurt.  Note that despite the pretentions there
;	is no guarantee that everything will fit in before rmar - for example
;	atoms (and now even hunks) are just blindly printed.	- DNC
;
                 (printaccross l lmar rmar))
                ((and (*patom1 lpar)
                      (atom (car l))
                      (not (atom (cdr l)))
                      (not (atom (cddr l))))
                 (prog (c)
                       (printret (car l) *outport*)
                       (*patom1 '" ")
                       (setq c (nwritn *outport*))
                  a    (*prd1 (cdr l) c)
                       (cond
                        ((not (atom (cdr (setq l (cdr l)))))
                         (terpr *outport*)
                         (go a)))))
                (t
                 (prog (c)
                       (setq c (nwritn *outport*))
                  a    (*prd1 l c)
                       (cond
                        ((not (atom (setq l (cdr l))))
                         (terpr *outport*)
                         (go a))))))
     b    (*patom1 rpar))))

(def *prd1
  (lambda (l n)
    (prog nil
          (*prdf (car l)
                 n
                 (cond ((null (setq l (cdr l))) (|1+| rmar))
                       ((atom l) (setq n nil) (+ 4 rmar (pntlen l)))
                       ( t rmar)))
          (cond
           ((null n) (*patom1 '" . ") (return (printret l *outport*))))
;         (*** setting n is pretty disgusting)
;         (*** the last arg to *prdf is the space needed for the suffix)
;		;Note that this is still not really right - if the prefix
;		takes several lines one would like to use the old rmar 
;(		until the last line where the " . mumble)" goes.
	)))

; -DNC here's the printmacro for progs - it replaces some hackery that
; used to be in the guts of *prdf.

(def printprog
  (lambda (l lmar rmar)
    (prog (col)
          (cond ((cdr (last l)) (return nil)))
          (setq col (1+ lmar))
          (princ '|(| *outport*)
          (princ (car l) *outport*)
          (princ '| | *outport*)
          (print (cadr l) *outport*)
          (mapc '(lambda (x)
			 (cond ((atom x)
				(tab col *outport*)
				(print x *outport*))
                          ( t (*prdf x (+ lmar 6) rmar))))
		(cddr l))
          (princ '|)| *outport*)
          (return t))))

(putprop 'prog 'printprog 'printmacro)

; Here's the printmacro for def.  The original *prdf had some special code
; for lambda and nlambda.

(def printdef
  (lambda (l lmar rmar)
    (cond ((and (\=& 0 lmar)		; only if we're really printing a defn
                (\=& 0 rmar)
                (cadr l)
                (atom (cadr l))
                (caddr l)
                (null (cdddr l))
                (memq (caaddr l) '(lambda nlambda macro lexpr))
                (null (cdr (last (caddr l)))))
           (princ '|(| *outport*)
           (princ 'def *outport*)
           (princ '| | *outport*)
           (princ (cadr l) *outport*)
           (terpri *outport*)
           (princ '|  (| *outport*)
           (princ (caaddr l) *outport*)
           (princ '| | *outport*)
           (princ (cadaddr l) *outport*)
           (terpri *outport*)
           (mapc  '(lambda (x) (*prdf x 4 0)) (cddaddr l))
           (princ '|))| *outport*)
           t))))

(putprop 'def 'printdef 'printmacro)

; There's a version of this hacked into the printer (where it don't belong!)
; Note that it must NOT apply to things like (quote a b).

(def printquote
  (lambda (l lmar rmar)
    (cond ((or (null (cdr l)) (cddr l)) nil)
          ( t (princ '|'| *outport*) 
	      (*prdf (cadr l) (1+ lmar) rmar)
	      t))))

(putprop 'quote 'printquote 'printmacro)




(def printaccross
  (lambda (l lmar rmar)
    (prog nil
;         (*** this is needed to make sure the printmacros are executed)
          (princ '|(| *outport*)  ;)
     l:   (cond ((null l))
                ((atom l) (princ '|. | *outport*) (princ l *outport*))
                ( t (*prdf (car l) (nwritn *outport*) rmar)
		    (setq l (cdr l))
		    (cond (l (princ '| | *outport*)))
		    (go l:))))))



(def tab (lexpr (n)
  (prog (nn prt) (setq nn (arg 1))
		(cond ((>& n 1) (setq prt (arg 2))))
		(cond ((>& (nwritn prt) nn) (terpri prt)))
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

(def *patom1 (lambda (x) (patom x *outport*)))

; vi: set lisp:
