(setq SCCS-ucido "@(#)ucido.l      1.3     6/29/81")
;
; ucilisp do loop,  this is a seperate file due to conflicts with
;	the franz lisp do function.  To use this, one needs
;	to load this file in at run time.  (And have calls to
;	do be close compiled in compiled code).
;
;	NOTE: do is a macro and must be declared before calls to it
;		in code to be compiled!
;
;	to compile this file: liszt ucido.l
;
(declare (macros t))

(eval-when (compile)
  (load 'ucifnc))

(defun do macro (l)
  ((lambda (dotype alist)
	   (cond ((eq dotype 'while)
		  (dowhile (car alist) (cdr alist)))
		 ((eq dotype 'until)
		  (dowhile (list 'not (car alist))
			   (cdr alist)))
		 ((eq dotype 'for)
		  (dofor (car alist) 
			 (cadr alist)
			 (caddr alist)
			 (cdddr alist)))
		 (t `((lambda ()
			      ,@alist)))))
   (cadr l)
   (cddr l)))

(defun dowhile (expr alist)
  `(prog (returnvar)
	 loop
	 (cond (,expr
		(setq returnvar ((lambda ()
					 ,@alist)))
		(go loop))
	       (t (return returnvar)))))

(defun dofor (var fortype varlist stmlist)
  (selectq fortype 
	   (in `(prog (returnvar l1 l2)
		      (setq l2 ',varlist)
		      loop
		      (setq l1 (car l2))
		      (setq l2 (cdr l2))
		      (cond ((null l1) 
			     (return returnvar)))
		      (setq returnvar
			    ((lambda (,var)
				     ,@stmlist)
			     (l1)))
		      (go loop)))
	   (on `(prog (returnvar l1 l2)
		      (setq l2 ',varlist)
		      loop
		      (cond ((null l2) 
			     (return returnvar)))
		      (setq returnvar
			    ((lambda (,var)
				     ,@stmlist)
			     (l2)))
		      (setq l2 (cdr l2))
		      (go loop)))
	   (rpt `(prog (returnvar ,var)
		       (setq ,var 1)
		       loop
		       (cond ((not (> ,var ,varlist))
			      (setq returnvar ((lambda ()
						       ,@stmlist)))
			      (setq ,var (1+ ,var))
			      (go loop))
			     (t (return returnvar)))))
	   nil))
