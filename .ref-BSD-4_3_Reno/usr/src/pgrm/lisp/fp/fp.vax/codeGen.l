;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-codeGen.l "@(#)codeGen.l	5.1 (Berkeley) 5/31/85")

; Main Routine to do code generation

(include specials.l)
(declare 
  (localf build_constr mName condit$fp alpha$fp insert$fp ti$fp while$fp)
  )

(defmacro getFform (xx)
  `(implode (nreverse `(p f ,@(cdr (nreverse (explodec (cxr 0 ,xx))))))))

(defun mName (name)
  (cond ((atom name) `',name)
	(t `',(getFform name))))

(defun mNameI (name)
  (cond ((atom name) name)
	(t (getFform name))))

(defun codeGen (ptree)
  (cond ((atom ptree) `',ptree)		; primitive or
							; user defined
	
	((eq (cxr 0 ptree) 'alpha$$)			; apply to all
	 (alpha$fp (cxr 1 ptree)))
	
	((eq (cxr 0 ptree) 'insert$$)			; insert
	 (insert$fp (cxr 1 ptree)))
	
	((eq (cxr 0 ptree) 'ti$$)			; tree insert
	 (ti$fp (cxr 1 ptree)))
	
	((eq (cxr 0 ptree) 'select$$)			; selector
	 (let ((sel (cxr 1 ptree)))
	      
	      (If (zerop sel) 		; No stats for errors
		  then `#'(lambda (x) (bottom))
		  
		  else
		  
		  `#'(lambda (x)
			     (cond ((not (listp x)) (bottom)))
			     (cond (DynTraceFlg (measSel ,sel x)))
			     ,(cond ((plusp sel)
				     `(If (greaterp ,sel (length x))
					  then (bottom)
					  else (nthelem ,sel x)))
				    
				    
				    ((minusp sel)
				     `(let  ((len (length x)))
					    (If (greaterp ,(absval sel) len) 
						then (bottom)
						else (nthelem (plus len ,(1+ sel)) x)))))))))


	
	((eq (cxr 0 ptree) 'constant$$)			; constant
	 (let ((const (cxr 1 ptree)))
	      (If (eq const '?)
		  then `#'(lambda (x) (bottom))
		  
		  else
		  
		  `#'(lambda (x) 
			     (cond (DynTraceFlg (measCons ,const x)))
			     ,const))))



	((eq (cxr 0 ptree) 'condit$$)			; conditional
	 (condit$fp (cxr 1 ptree) (cxr 2 ptree) (cxr 3 ptree)))
	
	((eq (cxr 0 ptree) 'while$$)			; while
	 (while$fp (cxr 1 ptree) (cxr 2 ptree)))
	
	
	((eq (cxr 0 ptree) 'compos$$)			; composition
	 (let ((cm1 (cxr 1 ptree))
	       (cm2 (cxr 2 ptree)))
	      `#'(lambda (x) 
			 (cond (DynTraceFlg
				(measComp ,(mName cm1) ,(mName cm2) x)))
			 (funcall ,(codeGen cm1)
				  (funcall ,(codeGen cm2)
					   x)))))

	
	((eq (cxr 0 ptree) 'constr$$)
	 (build_constr ptree))				; construction
	
	(t 'error)))					; error, sb '?


; build up the list of arguments for a construction

(defun build_constr (pt)
  (cond ((and (eq 2 (hunksize pt)) (null (cxr 1 pt)))
	 `#'(lambda (x) (cond (DynTraceFlg (measCons nil x)))  nil))
	(t
	 (do ((i 2 (1+ i))
	      (stat (list `,(mNameI (cxr 1 pt))))
	      (con (list (codeGen (cxr 1 pt)))))
	     ((greaterp i (1- (hunksize pt))) 
	      (return
	       (funcall 'constr$fp con stat)))
	     (setq stat (append stat (list `,(mNameI (cxr i pt)))))
	     (setq con (append con (list (codeGen (cxr i pt)))))))))


; generate a lisp function definition from an FP parse tree

(defun put_fn (fn_name p_tree)
  (untraceDel (extName fn_name))
  (putd fn_name 
	`(lambda (x) 
		 (cond (DynTraceFlg (IncrUDF ',fn_name x)))
		 (funcall ,(codeGen p_tree) x))))


; The Functional forms
;


; fp conditional

(def condit$fp
  (lambda (Pptree Tptree Fptree)
	  (let ((test (codeGen Pptree))
		(true (codeGen Tptree))
		(false (codeGen Fptree)))
	       
	       (let ((q
		      `(lambda (x)
				(cond (DynTraceFlg
				       (measCond 
					,(mName Pptree)
					,(mName Tptree)
					,(mName Fptree) x)))

				(let ((z (funcall ,test x)))
				     (cond 
				      ((eq 'T z) (funcall ,true x))
				      ((eq 'F z) (funcall ,false x))
				      (t (bottom)))))))
		    `(function ,q)))))



; construction 

(def constr$fp 
  (lexpr (v)
	 (let* ((vl (listify v))
		(q
		 `(lambda (x)
			  (cond (DynTraceFlg
				 (measConstr ',(cadr vl) x)))
			  (let* ((savelevel level)
				 (h 
				  (list  
				   ,@(mapcar 
				      #'(lambda 
					 (y)
					 `(let ((r ,`(funcall ,y x)))
					       (setq level savelevel)
					       r))
				      (car vl)))))
				(setq level savelevel)
				h
				))))
	       `(function ,q))))




; apply to all

(def alpha$fp
  (lambda (ptree)
	  (let* ((fn (codeGen ptree))
		 (q
		  `(lambda (x)
			   (cond (DynTraceFlg
				  (measAlph ,(mName ptree) x)))
			   (cond ((null x) nil)
				 ((not (listp x)) (bottom))
				 (t 
				  (let* ((savelevel level)
					 (h
					  (mapcar 
					   '(lambda (y)
						    (setq level savelevel)
						    (funcall ,fn y))
					   x)))
					
					(setq level savelevel)
					h))))))
		`(function ,q))))


; insert

(def insert$fp
  (lambda (ptree)
	  (let* ((fn (codeGen ptree))
		 (q
		  `(lambda (x)
			   (cond (DynTraceFlg (measIns ,(mName ptree) x)))
			   (cond ((not (listp x)) (bottom))
				 ((null x) 
				  (let ((ufn (get 'u-fnc ,fn)))
				       (cond 
					(ufn (funcall ufn))
					(t (bottom)))))
				 (t (let ((v (reverse x)) (z nil))
					 (setq z (car v))
					 (setq v (cdr v))
					 (mapc '(lambda (y) (setq z (funcall ,fn (list y z)))) v)
					 z))))))
		`(function ,q))))




(defun while$fp (pFn fFn)
  (let* ((fn_p (codeGen pFn))
	 (fn_f (codeGen fFn))
	 (q
	  `(lambda (x)
		   (cond (DynTraceFlg
			  (measWhile ,(mName pFn) ,(mName fFn) x)))
		   (do
		    ((z (funcall ,fn_p x) (funcall ,fn_p rslt))
		     (rslt x))
		    ((eq 'F z) rslt)
		    (cond ((undefp z) (bottom)))
		    (setq rslt (funcall ,fn_f rslt))))))
	`(function ,q)))




; Tree insert

(def ti$fp
  (lambda (ptree)
	  (let* ((fn (codeGen ptree))
		 (q
		  `(lambda (x)
			   (cond (DynTraceFlg (measAi ,(mName ptree) x)))
			   (treeIns$fp ,fn x))))
		`(function ,q))))
