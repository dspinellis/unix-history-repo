;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-primFp.l "@(#)primFp.l	5.1 (Berkeley) %G%")

;  FP interpreter/compiler
(include specials.l)
(declare (special y_l z_l)
  (localf ok_pair ok_eqpair rpair$ lpair$ trnspz allNulls
	  allLists emptyHeader treeInsWithLen))

; fp addition

(defun plus$fp (x)
  (cond (DynTraceFlg (IncrTimes 'plus$fp)))
  (cond ((ok_pair x 'numberp) (plus (car x) (cadr x)))
	(t (bottom))))

; unit function

(defun (u-fnc plus$fp) nil
  0)

; fp subtraction

(defun sub$fp (x)
  (cond (DynTraceFlg (IncrTimes 'sub$fp)))
  (cond ((ok_pair x 'numberp) (diff (car x) (cadr x)))
	(t (bottom))))


; unit function

(defun (u-fnc sub$fp) nil
  0)

; fp multiplication

(defun times$fp (x)
  (cond (DynTraceFlg (IncrTimes 'times$fp)))
  (cond ((ok_pair x 'numberp) (product (car x) (cadr x)))
	(t (bottom))))

; unit function

(defun (u-fnc times$fp) nil
  1)


; fp division 

(defun div$fp (x)
  (cond (DynTraceFlg (IncrTimes 'div$fp)))
  (cond ((ok_pair x 'numberp) 
	 (cond ((not (zerop (cadr x)))
		(quotient (car x) (cadr x)))
	       (t (bottom))))
	(t (bottom))))

; unit function

(defun (u-fnc div$fp) nil
  1)



; logical functions, and or xor not

(defun and$fp (x)
  (cond (DynTraceFlg (IncrTimes 'and$fp)))
  (cond ((ok_pair x 'boolp) 
	 (cond
	  ((eq 'F (car x)) 'F)
	  (t (cadr x))))
	(t (bottom))))

; unit function

(defun (u-fnc and$fp) nil
  'T)


(defun or$fp (x)
  (cond (DynTraceFlg (IncrTimes 'or$fp)))
  (cond ((ok_pair x 'boolp) 
	 (cond
	  ((eq 'T (car x)) 'T)
	  (t (cadr x))))
	(t (bottom))))

; unit function

(defun (u-fnc or$fp) nil
  'F)


(defun xor$fp (x)
  (cond (DynTraceFlg (IncrTimes 'xor$fp)))
  (cond ((ok_pair x 'boolp)
	 (let ((p (car x))
	       (q (cadr x)))
	      (cond ((or (and (eq p 'T) (eq q 'T))
			 (and (eq p 'F) (eq q 'F)))
		     'F)
		    (t 'T))))
	(t (bottom))))

; unit function

(defun (u-fnc xor$fp) nil
  'F)


(defun not$fp (x)
  (cond (DynTraceFlg (IncrTimes 'not$fp)))
  (cond ((not (atom x)) (bottom))
	((boolp x) (cond ((eq x 'T) 'F) (t 'T)))
	(t (bottom))))


; relational operators,  <  <=  =  >=  >  ~=

(defun lt$fp (x)
  (cond (DynTraceFlg (IncrTimes 'lt$fp)))
  (cond ((ok_pair x 'numberp) 
	 (cond ((lessp (car x) (cadr x)) 'T)
	       (t 'F)))
	(t (bottom))))

(defun le$fp (x)
  (cond (DynTraceFlg (IncrTimes 'le$fp)))
  (cond ((ok_pair x 'numberp) 
	 (cond ((not (greaterp (car x) (cadr x))) 'T)
	       (t 'F)))
	(t (bottom))))

(defun eq$fp (x)
  (cond (DynTraceFlg (IncrTimes 'eq$fp)))
  (cond ((ok_eqpair x ) 
	 (cond ((equal  (car x) (cadr x)) 'T)
	       (t 'F)))
	(t (bottom))))

(defun ge$fp (x)
  (cond (DynTraceFlg (IncrTimes 'ge$fp)))
  (cond ((ok_pair x 'numberp) 
	 (cond ((not (lessp (car x) (cadr x))) 'T)
	       (t 'F)))
	(t (bottom))))

(defun gt$fp (x)
  (cond (DynTraceFlg (IncrTimes 'gt$fp)))
  (cond ((ok_pair x 'numberp) 
	 (cond ((greaterp (car x) (cadr x)) 'T)
	       (t 'F)))
	(t (bottom))))

(defun ne$fp (x)
  (cond (DynTraceFlg (IncrTimes 'ne$fp)))
  (cond ((ok_eqpair x) 
	 (cond ((not (equal  (car x) (cadr x))) 'T)
	       (t 'F)))
	(t (bottom))))



; check arguments for eq and ne

(defun ok_eqpair (x)
  (cond ((not (atom x))
	 (cond ((eq (length x) 2) t)))))

; check arguments for binary arithmetics/logicals

(defun ok_pair (x typ)
  (cond ((not (atom x))
	 (cond ((eq (length x) 2)
		(cond 
		 ((and (atom (car x)) (atom (cadr x)))
		  (cond ((and (funcall typ (car x))
			      (funcall typ (cadr x))) t)))))))))

; check if a variable is boolean, 'T' or 'F'

(defun boolp (x)
  (memq x '(T F)))


(defun undefp (x)
  (eq x '?))

(defun tl$fp (x)
  (cond (DynTraceFlg (IncrSize 'tl$fp (size x)) (IncrTimes 'tl$fp)))
  (cond ((atom x) (bottom))
	(t (cdr x))))


(defun tlr$fp (x)
  (cond (DynTraceFlg (IncrSize 'tlr$fp (size x)) (IncrTimes 'tlr$fp)))
  (cond ((listp x) (cond
		    ((onep (length x)) nil)
		    (t (reverse (cdr (reverse x))))))
	(t (bottom))))

; this function is just like id$fp execept it also prints its
; argument on the stdout. It is meant to be used only for debuging.

(defun out$fp (x)
  (fpPP x)
  (terpri)
  x)

(defun id$fp (x)
  (cond (DynTraceFlg (IncrSize 'id$fp (size x)) (IncrTimes 'id$fp)))
  x)

(defun atom$fp (x)
  (cond (DynTraceFlg (IncrSize 'atom$fp (size x)) (IncrTimes 'atom$fp)))
  (cond ((atom x) 'T)
	(t 'F)))

(defun null$fp (x)
  (cond (DynTraceFlg (IncrSize 'null$fp (size x)) (IncrTimes 'null$fp)))
  (cond ((null x) 'T)
	(t  'F)))

(defun reverse$fp (x)
  (cond (DynTraceFlg (IncrSize 'reverse$fp (size x)) (IncrTimes 'reverse$fp)))
  (cond  ((null x) x)
	 ((listp x) (reverse x))
	 (t (bottom))))

(defun lpair$ (x)
  (cond ((or (undefp x) (not (listp x))) nil)
	(t
	 (setq y_l (car x))
	 (setq z_l (cdr x))
	 (cond ((null z_l)  t)
	       (t (cond ((or (not (listp z_l)) (not (onep (length z_l)))) nil)
			(t (listp (setq z_l (car z_l))))))))))

(defun rpair$ (x)
  (cond ((or (undefp x) (not (listp x))) nil)
	(t
	 (setq y_l (car x))
	 (setq z_l (cdr x))
	 (cond ((null y_l)  t)
	       (t (cond ((not (listp y_l)) nil)
			(t (setq z_l (car z_l)) t)))))))


(defun distl$fp (x)
  (let ((y_l nil) (z_l nil))
       (cond ((lpair$ x) 
	      (cond (DynTraceFlg
		     (IncrSize 'distl$fp (size z_l)) (IncrTimes 'distl$fp)))
	      (mapcar '(lambda (u) (list y_l u)) z_l))
	     (t (bottom)))))

(defun distr$fp (x)
  (let ((y_l nil) (z_l nil))
       (cond ((rpair$ x)
	      (cond (DynTraceFlg
		     (IncrSize 'distr$fp (size y_l)) (IncrTimes 'distr$fp)))
	      (mapcar '(lambda (u) (list u z_l)) y_l))
	     (t (bottom)))))


(defun length$fp (x)
  (cond (DynTraceFlg (IncrSize 'length$fp (size x)) (IncrTimes 'length$fp)))
  (cond ((listp x) (length x))
	(t (bottom))))

(defun apndl$fp (x)
  (cond ((and (dtpr x) (eq 2 (length x)) (listp (cadr x)))
	 (cond (DynTraceFlg
		(IncrSize 'apndl$fp (size (cadr x))) (IncrTimes 'apndl$fp)))
	 (cons (car x) (cadr x)))
	(t (bottom))))


(defun apndr$fp (x)
  (cond ((and (dtpr x) (eq 2 (length x)) (listp (car x)))
	 (cond (DynTraceFlg
		(IncrSize 'apndr$fp (size (car x))) (IncrTimes 'apndr$fp)))
	 (append (car x) (cdr x)))
	(t (bottom))))


(defun rotl$fp (x)
  (cond (DynTraceFlg (IncrSize 'rotl$fp (size x)) (IncrTimes 'rotl$fp)))
  (cond ((null x) x)
	((listp x) (cond ((onep (length x)) x)
			 (t (append (cdr x) (list (car x))))))
	(t (bottom))))

(defun rotr$fp (x)
  (cond (DynTraceFlg (IncrSize 'rotr$fp (size x)) (IncrTimes 'rotr$fp)))
  (cond ((null x) x)
	((listp x) (cond ((onep (length x)) x)
			 (t (reverse (rotl$fp (reverse x))))))
	(t (bottom))))


(defun trans$fp (x)
  (If (and (listp x) (allLists x))
      then (If (allNulls x)
	       then
	       (cond (DynTraceFlg
		      (IncrSize 'trans$fp (size x))
		      (IncrTimes 'trans$fp)))
	       nil
	       
	       else
	       (cond (DynTraceFlg
		      (IncrSize 'trans$fp 
				(+ (size (car x))
				   (size (cadr x)))) (IncrTimes 'trans$fp)))
	       
	       (do ((a x (cdr a))
		    (f (length (car x))))
		   ((null a) (trnspz x))
		   (If (or (not (listp (car a))) (not (eq f (length (car a)))))
		       then (bottom))))
      else
      
      (bottom)))

(defun allNulls (x)
  (do ((a x (cdr a)))
      ((null a) t)
      (If (car a) then (return nil))))

(defun allLists (x)
  (do ((a x (cdr a)))
      ((null a) t)
      (If (not (dtpr (car a))) then (return nil))))


(defun trnspz (l)
  (do
   ((h (emptyHeader (length (car l))))
    (v l (cdr v)))
   ((null v) (mapcar 'car h))
   (mapcar #'(lambda (x y) (tconc x y)) h (car v))))


(defun emptyHeader (n)
  (do
   ((r nil)
    (c n (1- c)))
   ((= c 0) r)
   (setq r (cons (ncons nil) r))))


(defun iota$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'iota$fp)))
  (cond ((undefp x) x)
	((listp x) (bottom))
	((not (fixp x)) (bottom))
	((lessp x 0) (bottom))
	((zerop x) nil)
	(t
	 (do ((z x (1- z))
	      (rslt nil))
	     ((zerop z) rslt)
	     (setq rslt (cons z rslt))))))

; this is the stuff that was added by dorab patel to make this have
; the same functions as David Lahti's interpreter


;; Modified by SBB to accept nil as a valid input

(defun last$fp (x)
  (cond (DynTraceFlg (IncrSize 'last$fp (size x)) (IncrTimes 'last$fp)))
    (cond ((null x) nil)
	  ((listp x) (car (last x)))
	  (t (bottom))))

;; Added by SBB

(defun first$fp (x)
  (If DynTraceFlg then (IncrSize 'first$fp (size x)) (IncrTimes 'first$fp))
  (If (not (listp x)) then (bottom)
      else (car x)))

(defun front$fp (x)
  (cond (DynTraceFlg (IncrSize 'front$fp (size x)) (IncrTimes 'front$fp)))
    (cond ((null x) (bottom))
	  ((listp x) (reverse (cdr (reverse x))))
	  (t (bottom))))

(defun pick$fp (sAndX)
  (let ((s (car sAndX))
	(x (cadr sAndX)))
       (If (or (not (fixp s)) (zerop s) (cddr sAndX)) then  (bottom)
	   else
	   
	   (progn
	    (cond (DynTraceFlg
		   (IncrTimes 'select$fp)
		   (IncrSize 'select$fp (size x))))
	    
	    (cond ((not (listp x)) (bottom))
		  ((plusp s) 
		   (If (greaterp s (length x)) then (bottom)
		       else (nthelem s x)))
		  ((minusp s)
		   (let  ((len (length x)))
			 (If (greaterp (absval s) len) then (bottom)
			     else (nthelem (plus len 1 s) x)))))))))


(defun concat$fp (x)
  (cond (DynTraceFlg (IncrSize 'concat$fp (size x)) (IncrTimes 'concat$fp)))
  
  (If (listp x)
      then
      (do ((a x  (cdr a))
	   (y (copy x) (cdr y))
	   (rslt (ncons nil)))
	  ((null a) (car rslt))
	  (If (not (listp (car a))) then (bottom))
	  
	  (lconc rslt (car y)))
      
      else (bottom)))


(defun pair$fp (x)
  (cond (DynTraceFlg (IncrSize 'pair$fp (size x)) (IncrTimes 'pair$fp)))
  (cond ((not (listp x)) (bottom))
	((null x) (bottom))
	(t (do ((count 0 (add count 2)) ; set local vars
		(max (length x))
		(ret (ncons nil)))
	       ((not (lessp count max)) (car ret)) ; return car of tconc struc
	       (cond ((equal (diff max count) 1) ; if only one element left
		      (tconc ret (list (car x))))
		     (t (tconc ret (list (car x) (cadr x)))
			(setq x (cddr x))))))))
    

(defun split$fp (x)
  (cond (DynTraceFlg (IncrSize 'split$fp (size x)) (IncrTimes 'split$fp)))
  (cond ((not (listp x)) (bottom))
	((null x) (bottom))
	((eq (length x) 1) (list x nil))
	(t
	 (do ((count 1 (add1 count))
	      (mid (fix (plus 0.5 (quotient (length x) 2.0))))
	      (ret nil))
	     ((greaterp count mid) (cons (nreverse ret) (list x)))
	     (setq ret (cons (car x) ret))
	     (setq x (cdr x))))))


; Library functions: sin, asin, cos, acos, log, exp, mod

(defun sin$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'sin$fp)))
  (cond ((numberp x) (sin x))
	(t (bottom))))

(defun asin$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'asin$fp)))
  (cond ((and (numberp x) (not (greaterp (abs x) 1.0))) (asin x))
	(t (bottom))))

(defun cos$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'cos$fp)))
  (cond ((numberp x) (cos x))
	(t (bottom))))

(defun acos$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'acos$fp)))
  (cond ((and (numberp x) (not (greaterp (abs x) 1.0))) (acos x))
	(t (bottom))))

(defun log$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'log$fp)))
  (cond ((and (numberp x) (not (minusp x))) (log x))
	(t (bottom))))

(defun exp$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'exp$fp)))
  (cond ((numberp x) (exp x))
	(t (bottom))))

(defun mod$fp (x)
  (cond (DynTraceFlg  (IncrTimes 'mod$fp)))
  (cond ((ok_pair x 'numberp) (mod (car x) (cadr x)))
	(t (bottom))))


;; Tree insert function


(defun treeIns$fp (fn x)
  (If (not (listp x)) then  (bottom)
      else
      (If (null x) then  (unitTreeInsert fn)
	  else 
	  (let ((len (length x)))
	       (If (onep len) then (car x)
		   else
		   (If (twop len) then (funcall fn x )
		       else (treeInsWithLen fn x len)))))))


(defun treeInsWithLen (fn x len)
  (let* ((r1 (copy x))
	 (nLen (fix (plus 0.5 (quotient len 2.0))))
	 (p (Cnth r1 nLen))
	 (r2 (cdr p)))
	(rplacd p nil)
	(let ((saveLevel level))
	     (setq level (1+ level))
	     (let ((R1 (treeIns fn r1 nLen)))
		  (setq level (1+ saveLevel))
		  (let ((R2 (treeIns fn r2 (diff len nLen))))
		       (setq level saveLevel)
		       (funcall fn `(,R1 ,R2)))))))
