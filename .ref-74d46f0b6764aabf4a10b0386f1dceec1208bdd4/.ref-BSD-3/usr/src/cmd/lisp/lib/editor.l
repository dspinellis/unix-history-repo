(setsyntax '";" 'splicing 'zapline)
; editor from bbn-lisp c. 1968 
; (transcribed by R. Fateman for UNIX LISP, Oct., 1977)
; (modified and enhanced by P. Pifer, May, 1978)
; (corrected again by R. Fateman for VAX Unix Lisp, Dec., 1978)
; (cleaned up, commented and compiled by J. Foderaro, Aug., 1979)
; ( ... fixed bug in ^ command)
;
(declare (special edok em pf pl l))


 (setq printflag t)
; print on by default

 (setq printlevel 3)

 (setq maxlevel 100)

 (setq findflag nil)

(setq supereditflg t)(setq printflag t)(setq edrptcnt nil)


;--- remedit - removes all traces of the editor from the oblist.
;	       Note that if the editor is compiled, the code space
;	       will not be reclaimed
;
(def remedit
  (lambda nil
	  (prog nil
		(mapc (function (lambda (x) (set x nil)))
		      '(editmacros findflag supereditflg edrptcnt
			printflag printlevel maxlevel))
		(mapc (function (lambda (x) (putd x nil)))
		      '(editf editv tconc eprint eprint1 printlevel dsubst 
			editcoms edit1f edit2f edit2af edit4e 
			editqf edit4e edit4f edit4f1 editnth bpnt 
			bpnt0 subpair subpr ri ro li lo bi bo 
			ldiff nthcdr attach edite editcom editdefault
			remedit))
		(return 'gone))))

;--- subst - a - newval
;	   - b : oldvall
;	   - c : string
;	substitute a for b in c
;
(def subst
      (lambda (a b c)
	      (cond ((equal b c) a)
		    ((atom c) c)
		    (t (cons (subst a b (car c)) (subst a b (cdr c)))))))

(def tconc
      (lambda (x p)
	      (cond ((null (car p))
		     (rplacd p (car (rplaca p (list x)))))
		    (t (rplacd p (cdr (rplacd (cdr p) (list x))))))))

;--- printlevel - x : new value
;	set the printlevel to x and return the old value
;	[change this to prog1 ]
;
(def printlevel
  (lambda (x)
	  (prog (a)
		(setq a printlevel)
		(setq printlevel x)
		(return a))))

;--- editf - funcname : name of function to edit
;	   - [cmds]   : commands to apply right away
;	This is the starting point in the editor.  You specify the
;	file you wish to edit and perhaps some initial commands to 
;	the editor. If the function is not machine coded you 
;	enter the editor.
;
(def editf
  (nlambda (x)
	   (prog (a c)
		 (setq a (getd (car x)))
		 (cond ((or (null a) (bcdp a))
			(return '(not editable))))
		 (putd (car x) (car (edite a (cdr x) nil)))
		 (return (car x)))))

'(def dsubst
  (lambda (x y z)
	  (prog nil
		(cond ((null z) (return z))
		      ((equal y (car z)) (rplaca z x) (go l)))
		(cond ((null (atom (car z))) (dsubst x y (car z))))
	   l    (dsubst x y (cdr z))
		(return z))))

;--- dsubst - x : oldval
;	    - y : newval
;	    - z : form
;	directly substitutes all occurances of x in form z with y.
;	It uses rplaca and does not copy the structure.
;
(def dsubst
  (lambda (x y z)
	  (cond ((dptr z) 
		 (cond ((equal y (car z))
			(rplaca (car z) x))
		       (t (dsubst x y (car z)))))
		(t z))
	  (dsubst x y (cdr z))
	  z))


(def editcoms (lambda (c) (mapc (function editcom) c)))

(def edit1f
  (lambda (c l)
	  (cond ((equal c 0)
		 (cond ((null (cdr l)) (err nil))
		       (t (cdr l))))
		((greaterp c 0)
		 (cond ((greaterp c (length (car l))) (err nil))
		       (t (cons (car (nthcdr (sub1 c) (car l) )) l))))
		((greaterp (times c -1) (length (car l)))
		 (err nil))
		(t (cons (car (nthcdr (plus (length (car l)) c) (car l) ))
			 l)))))

(def edit2f
  (lambda (c)
	  (cond ((greaterp (car c) 0)
		 (cond ((greaterp (car c) (length (car l)))
			(err nil))
		       (t (rplaca l (edit2af (sub1 (car c))
					     (car l)
					     (cdr c)
					     nil)))))
		((or (equal (car c) 0)
		     (null (cdr c))
		     (greaterp (times -1 (car c)) (length (car l))))
		 (err nil))
		(t (rplaca l (edit2af (sub1 (times -1 (car c)))
				      (car l)
				      (cdr c)
				      t))))))

 (def edit2af
      (lambda (n x r d)
	      (prog nil
		    (cond ((null (equal n 0))
			   (rplacd (nthcdr  (sub1 n) x)
				   (nconc r
					  (cond (d (nthcdr  n x))
						(t (nthcdr (add1 n) x ))))))
			  (d (attach (car r) x)
			     (rplacd x (nconc (cdr r) (cdr x))))
			  (r (rplaca x (car r))
			     (rplacd x (nconc (cdr r) (cdr x))))
			  (t (print (list 'aha x)) 
			     (rplaca x (cadr x)) 
			     (rplacd x (cddr x))))
		    (return x))))

(def edit4e
      (lambda (x y)
	      (cond ((equal x y) t)
		    ((atom x) (eq x '&))
		    ((atom y) nil)
		    ((edit4e (car x) (car y))
		     (or (eq (cadr x) '-)
			 (edit4e (cdr x) (cdr y)))))))

(def editqf
  (lambda (s)
	  (prog (q1)
		(return (cond ((setq q1 (member s (cdar l)))
			       (setq l (cons q1 l)))
			      (t (edit4f s 'n)
				 (cond ((not (atom s))
					(setq l (cons (caar l) l))))))))))

(def edit4f
  (lambda (s n)
	  (prog (ff ll x)
		(setq ll (cond ((eq n 'n) (cons (caar l) l))
			       (t l)))
		(setq x (car ll))
		(setq ff (cons nil nil))
		(cond ((and n (not (numberp n))) (setq n 1)))
	lp      (cond ((edit4f1 s x maxlevel)
		       (setq l (nconc (car ff) ll))
		       (return (car l)))
		      ((null n) (err nil)))
	lp1     (setq x (car ll))
		(cond ((null (setq ll (cdr ll))) (err nil))
		      ((and (setq x (member x (car ll)))
			    (null (atom (setq x (cdr x)))))
		       (go lp)))
		(go lp1))))

(def edit4f1
  (lambda (s a lvl)
	  (prog nil
		(cond ((null (greaterp lvl 0)) (return nil)))
	lp      (cond ((atom a) (return nil))
		      ((and (edit4e s (car a))
			    (or (null n)
				(equal 0 (setq n (sub1 n)))))
		       (return (tconc a ff)))
		      ((and s
			    (equal s (cdr a))
			    (or (null n)
				(equal 0 (setq n (sub1 n)))))
		       (return (tconc a ff)))
		      ((and n
			    (edit4f1 s (car a) (sub1 lvl))
			    (equal 0 n))
		       (return (tconc (car a) ff))))
		(setq a (cdr a))
		(go lp))))

(def editnth
  (lambda (x n)
	  (cond ((null (setq n (cond ((or (null (lessp n 0))
					  (greaterp (setq n
							  (plus (length x)
								n
								1))
						    0))
				      (nthcdr  (sub1 n) x)))))
		 (err nil))
		(t n))))

(def bpnt
  (lambda (x)
	  (prog (y n)
		(cond ((equal 0 (car x)) (setq y (car l)))
		      (t (setq y (car (editnth (car l) (car x))))))
		(cond ((null (cdr x)) (setq n 3))
		      ((null (numberp (cadr x))) (go b1))
		      ((lessp (cadr x) 0)
		       (setq n (plus (cadr x) 2)))
		      (t (setq n (cadr x))))
		(return (bpnt0 y 1 n))
	b1      (err nil))))

(def bpnt0
  (lambda (l n d)
	  (prog (oldl)
		(setq oldl (printlevel (difference d n)))
		(cond ((atom (errset (eprint l) t))
		       (terpri)
		       (terpri)))
		(printlevel oldl)
		(return nil))))


(def ro
  (lambda (n x)
	  (prog (a)
		(setq a (editnth x n))
		(cond ((or (null a) (atom (car a))) (err nil)))
		(rplacd (last (car a)) (cdr a))
		(rplacd a nil))))

(def ri
  (lambda (m n x)
	  (prog (a b)
		(setq a (editnth x m))
		(setq b (editnth (car a) n))
		(cond ((or (null a) (null b)) (err nil)))
		(rplacd a (nconc (cdr b) (cdr a)))
		(rplacd b nil))))

(def li
  (lambda (n x)
	  (prog (a)
		(setq a (editnth x n))
		(cond ((null a) (err nil)))
		(rplaca a (cons (car a) (cdr a)))
		(rplacd a nil))))

(def lo
  (lambda (n x)
	  (prog (a)
		(setq a (editnth x n))
		(cond ((or (null a) (atom (car a))) (err nil)))
		(rplacd a (cdar a))
		(rplaca a (caar a)))))

(def bi
  (lambda (m n x)
	  (prog (a b)
		(setq b (cdr (setq a (editnth x n))))
		(setq x (editnth x m))
		(cond ((and a (null (greaterp (length a) (length x))))
		       (rplacd a nil)
		       (rplaca x (cons (car x) (cdr x)))
		       (rplacd x b))
		      (t (err nil))))))

(def bo
  (lambda (n x)
	  (prog nil
		(setq x (editnth x n))
		(cond ((atom (car x)) (err nil)))
		(rplacd x (nconc (cdar x) (cdr x)))
		(return (rplaca x (caar x))))))

(def subpair
  (lambda (x y z fl)
	  (cond (fl (subpr x y (copy z)))
		((subpr x y z)))))

 (def subpr
      (lambda (x y z)
	      (prog (c d)
		    (setq c x)
		    (setq d y)
	    loop    (cond ((or (null c) (null d)) (return z))
			  (t (dsubst (car d) (car c) z)
			     (setq c (cdr c))
			     (setq d (cdr d))
			     (go loop))))))

(def ldiff
  (lambda (x y)
	  (prog (a b)
		(setq a x)
		(setq b nil)
	loop    (cond ((equal a y) (return (reverse b)))
		      ((null a) (return (err nil)))
		      (t (setq b (cons (car a) b))
			 (setq a (cdr a))
			 (go loop))))))

(def editv
  (nlambda (editvx)
	   (prog nil
		 (set (car editvx)
		      (car (edite (eval (car editvx))
				  (cdr editvx)
				  nil)))
		 (return (car editvx)))))

(def nthcdr
  (lambda (n x)
	  (cond ((equal n 0) x)
		((lessp n 0) (cons nil x))
		(t (nthcdr  (sub1 n)(cdr x))))))

(def attach
  (lambda (x y)
	  (prog (a)
		(setq a (cons (car y) (cdr y)))
		(rplaca y x)
		(rplacd y a)
		(return y))))

 (def eprint (lambda (x) (print (eprint1 x printlevel))))

(def edite
  (lambda (x ops l)
	  (prog (c m em edok copied pf pl)
		(cond ((null l) (setq l (list x))))
		(setq em editmacros)
		(setq pf printflag)
		(setq pl 3)
		(cond (ops (cond ((dtpr (errset (mapc 
					     (function 
					        (lambda (x)
							(editcom (setq c x))))
					     ops)
					  t))
				  (return (car (last l))))
				 (t (go b)))))
		(print 'edit)
		(cond (pf (terpri) (editcom 'p)))
		(setq pf printflag)
	ct      (setq findflag nil)
	a       (cond (edok (return (cdr edok))))
		(terpri)
		(patom '*)
		(drain)
		(cond ((atom (errset (setq c (read)) t)) (go ct)))
		(cond ((dtpr (errset (editcom c) t))
		       (cond (pf (editcom 'p)))
		       (setq pf printflag)
		       (go a)))
	b       (terpri)
		(print c)
		(patom '?)
		(terpri)
		(go ct))))

(def editdefault
      (lambda (x) (editcom (list 'f x 't))))

(def editcom
  (lambda (c)
	  (prog (cc c2 c3 cl)
	a       (cond (findflag (setq findflag nil) (editqf c))
		      ((numberp c) (setq l (edit1f c l)))
		      ((atom c)
		       (cond ((eq c 'ok)
			      (setq ersetflg t)
			      (setq edok (cons t (last l)))
			      (return (setq pf nil)))
			     ((eq c 'e)
			      (setq ersetflg t)
			      (print (eval (read)))
			      (terpri))
			     ((eq c 'p)
			      (setq pf nil)
			      (bpnt0 (car l) 1 pl))
			     ((eq c 'pp)
			      (setq pf nil)
			      (terpri)
			      (errset ($prpr (car l)) t)
			      (terpri))
			     ((eq c 'mark)
			      (setq m (cons l m)))
			     ((eq c '^)
			      (setq l (list (last l))))
			     ((eq c 'copy) (setq copied (copy l)))
			     ((eq c 'restore) (setq l copied))
			     ((eq c '<)
			      (cond (m (setq l (car m)))
				    (t (err '"no marks"))))
			     ((eq c '<<)
			      (cond (m (setq l (car m))
				       (setq m (cdr m)))
				    (t (err '"no marks"))))
			     ((eq c 'poff)
			      (setq pf nil)
			      (setq printflag nil))
			     ((eq c 'pon)
			      (setq pf t)
			      (setq printflag t))
			     (t (cond ((and (setq cc
						  (cond ((null
							  (setq cc 
								(assoc c em)))
							 nil)
							((cdr cc))))
					    (null (car cc)))
				       (editcoms (copy cc)))
				      (t (return (editdefault c)))))))
		      ((numberp (setq cc (car c))) (edit2f c))
		      (t (setq c2 (cadr c))
			 (setq c3
			       (cond ((null (cddr c)) nil)
				     ((car (cddr c)))))
			 (setq cl (car l))
			 (cond ((eq cc 's)
				(set c2
				     (car (cond ((null (setq c c3)) l)
						((equal c 0) l)
						(t (editnth cl c))))))
			       ((eq cc 'r)
				(dsubst c3 c2 cl))
			       ((eq cc 'e)
				(setq cc (eval c2))
				(cond ((null (cddr c))
				       (print cc)
				       (terpri)))
				(return cc))
			       ((eq cc 'i)
				(setq c
				      (cons (cond ((atom c2) c2)
						  (t (eval c2)))
					    (mapcar (function eval)
						    (cddr c))))
				(go a))
			       ((eq cc 'n)
				(nconc cl (cdr c)))
			       ((eq cc 'p)
				(bpnt (cdr c))
				(setq pf nil))
			       ((eq cc 'f)
				(edit4f c2 c3))
			       ((eq cc 'nth)
				(setq l (cons (editnth cl c2) l)))
			       ((member cc
					 '(ri ro li lo bi bo))
				(apply1 cc (append (cdr c) (list cl))))
			       ((member cc '(m d))
				(setq cc (cond ((atom (setq cc c2))
						(cons cc
						      (cons nil
							    (cddr c))))
					       (t (cons (car cc) (cddr c)))))
				(setq em (cons cc em))
				(cond ((eq (car c) 'm) 
				       (setq editmacros
					     (cons cc editmacros)))))
			       ((eq cc 'pl)
				(cond ((lessp c2 1) (err nil))
				      (t (setq pl (add 1 c2)))))
			       (t (cond ((or (null
					      (setq cc
						    (cond ((null
							    (setq cc
								  (assoc cc em)))
							   nil)
							  (t (cdr cc)))))
					     (null (cond ((null cc) nil)
							 (t (car cc)))))
					 (return (editdefault c)))
					((atom (car cc))
					 (editcoms
					  (subst (cond ((null c) nil)
						       ((cdr c)))
						 (car cc)
						 (cdr cc))))
					(t (editcoms
					    (subpair (car cc)
						     (cdr c)
						     (cdr cc)
						     t))))))))
		(return (car l)))))

(def eprint1
  (lambda (x lev)
	  (cond ((atom x) x)
		((equal 0 lev) '&)
		((and (atom (cdr x)) (cdr x)) x)
		(t (mapcar (function (lambda (y) (eprint1 y (sub1 lev))))
			   x)))))

(def assoc
  (lambda (e l)
	  (cond ((null l) nil)
		((equal e (caar l)) (car l))
		(t (assoc e (cdr l))))))

 (def apply1
      (lambda (f l)
	      (eval (cons f (mapcar '(lambda (z) (list 'quote z))
				    l)))))




(def editp
  (nlambda (x)
	   (prog (a b)
		 (setq a (car x))
		 (edite (caar x))
		 (return a))))

(def makefile
  (nlambda (x)
	   (prog (poport n f ff l df)
		 (setq l (cons nil (cadr x)))
		 (setq ff (eval (car x)))
		 (setq poport
		       (outfile (setq n (concatp 'mkfl))))
	l1       (cond ((null (setq l (cdr l))) (go e1)))
		 (setq f (car l))
		 (cond ((null f) (go l1))
		       ((null (setq df (getd f))) (go l1))
		       (t (setq df (list 'def f df))
			  ($prpr df)
			  (terpri)
			  (go l1)))
	e1       (close poport)
		 (null (eval (cons 'exec (cons '/bin/mv (cons n (cons ff nil)))))))))

(def appfile
  (nlambda (x)
	   (prog (i poport n f ff l df)
		 (setq l (cons nil (cadr x)))
		 (setq ff (eval (car x)))
		 (setq i (infile ff))
		 (setq poport
		       (outfile (setq n (concatp 'apfl))))
	l1       (cond ((eq (setq f (read i poport)) 'eof)
			(go l2))
		       (t ($prpr f) (terpri)))
		 (go l1)
	l2       (cond ((null (setq l (cdr l))) (go e1)))
		 (setq f (car l))
		 (cond ((null f) (go l2))
		       ((null (setq df (getd f))) (go l2))
		       (t (setq df (list 'def f df))
			  ($prpr df)
			  (terpri)
			  (go l2)))
	e1       (close poport)
		 (null (eval (cons 'exec (cons '/bin/mv (cons n (cons ff nil)))))))))

(def exec
  (nlambda ($list)
	   (prog ($handy)
		 (setq $handy '"")
	 loop    (cond ((null $list)
			(return (eval (list 'process $handy))))
		       (t (setq $handy
				(concat (concat $handy (car $list))
					'" "))
			  (setq $list (cdr $list))
			  (go loop))))))

(setq editmacros nil)
