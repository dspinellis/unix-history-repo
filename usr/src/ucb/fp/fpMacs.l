;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-fpMacs.l "@(#)fpMacs.l	5.1 (Berkeley) 5/31/85")

(declare
  (macros t)
  (special ptport infile))


(eval-when (compile eval load)

  (setq whiteSpace ''(9 10 32))
  (setq blankOrTab ''(9 32))
  (setq CR 10)
  (setq BLANK 32)
  (setq lAngle '|<|)
  (setq rAngle '|>|)
  
  (setq funcForms
	''(alpha$fp
	   insert$fp
	   constant$fp
	   condit$fp
	   constr$fp
	   compos$fp
	   while$fp
	   ti$fp))
  
  (setq multiAdicFns
	''(select$fp
	   tl$fp
	   tlr$fp
	   id$fp
	   atom$fp
	   null$fp
	   reverse$fp
	   distl$fp
	   distr$fp
	   length$fp
	   apndl$fp
	   apndr$fp
	   rotl$fp
	   rotr$fp
	   trans$fp
	   first$fp
	   last$fp
	   front$fp
	   pick$fp
	   concat$fp
	   pair$fp
	   split$fp))
  
  (setq dyadFns
	''(plus$fp
	   sub$fp
	   times$fp
	   div$fp
	   and$fp
	   or$fp
	   xor$fp
	   not$fp
	   lt$fp
	   le$fp
	   eq$fp
	   ge$fp
	   gt$fp
	   ne$fp))
  
  
  (setq libFns
	''(sin$fp
	   asin$fp
	   cos$fp
	   acos$fp
	   log$fp
	   exp$fp
	   mod$fp))
  
  (setq miscFns
	''(iota$fp))
  )


(defmacro Tyi nil
  `(let ((z (tyi)))
	(cond ((and (null infile) ptport) (tyo z ptport))
	      (t z))))

(defmacro peekc nil
       `(tyipeek infile))

(defmacro Getc nil
  `(let ((piport infile))
	(prog (c)
	      (cond ((eq 'eof$$ (setq c (readc piport 'eof$$)))
		     (*throw 'parse$err 'eof$$))
		    (t (setq c (car (exploden c)))
		       (cond
			((not (and (null in_buf) (memq c #.whiteSpace)))
			 (setq in_buf (cons c in_buf))))))
	      (cond ((and (null infile) ptport)
		     (cond
		      ((not (and (null in_buf) (memq c #.whiteSpace)))
		       (tyo c ptport)))))
	      (return c))))

(defmacro Read nil
  `(let ((z (read)))
	(prog nil
	      (cond ((and (null infile) ptport (not (listp z))) (patom z ptport)))
	      (cond ((and (null infile) ptport (not (listp z)))
		     (do
		      ((c (tyipeek) (tyipeek)))
		      ((or (and (eq c #.CR) (Tyi) t)
			   (null (memq c #.blankOrTab))))
		      (Tyi))))
	      
	      (return z))))

(defmacro find (flg lst)
  `(cond ((atom ,lst) (eq ,flg ,lst))
	 ((not (listp ,lst)) nil)
	 (t (memq ,flg ,lst))))


; we want top-level size, not total number of arguments

(defmacro size (x)
  `(cond ((atom ,x) 1)
	 (t (length ,x))))

(defmacro twop (x)
  `(eq 2 ,x))


;; Special macros to help out tree insert

(defmacro treeIns (fn input Len)
  `(cond ((zerop ,Len) (unitTreeInsert ,fn))
	 ((onep ,Len) (car ,input))
	 ((twop ,Len) (funcall ,fn  ,input))
	 (t (treeInsWithLen ,fn ,input ,Len))))


(defmacro unitTreeInsert (fn)
  `(let ((ufn (get 'u-fnc ,fn)))
	(cond (ufn  (funcall ufn))
	      (t (bottom)))))


(putprop 'fpMacs t 'loaded)

