;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-fpPP.l "@(#)fpPP.l	5.1 (Berkeley) 5/31/85")

;; pretty printer for fp -- snarfed from FRANZ LISP

(include specials.l)

(declare (special fpPParm1 fpPParm2 lAngle rAngle))

; printRet is like print yet it returns the value printed,
; this is used by fpPP.

(def printRet
  (macro ($l$)
	 `(progn 
	   (let ((z ,@(cdr $l$)))
		(cond ((null z) (patom "<>"))
		      (t
		       (print ,@(cdr $l$))))))))


(def fpPP
  (lambda (x)
	  (terpri)
	  (prDF x 0 0)
	  (terpri)))


(setq fpPParm1 50 fpPParm2 100)

;   -DNC These "prettyprinter parameters" are used to decide when we should
;	quit printing down the right margin and move back to the left -
;	Do it when the leftmargin > fpPParm1 and there are more than fpPParm2
;	more chars to print in the expression



(declare (special rmar))

(def prDF
  (lambda (l lmar rmar)
    (prog nil
;
;			- DNC - Here we try to fix the tendency to print a
;			  thin column down the right margin by allowing it
;			  to move back to the left if necessary.
;
	  (cond ((and (>& lmar fpPParm1) (>& (flatc l (1+ fpPParm2)) fpPParm2))
		 (terpri)
		 (patom "; <<<<< start back on the left <<<<<")
		 (prDF l 5 0)
		 (terpri)
		 (patom "; >>>>> continue on the right >>>>>")
		 (terpri)
		 (return nil)))
          (tab lmar)
     a    (cond 
                ((or (not (dtpr l))
;                    (*** at the moment we just punt hunks etc)
                     ;(and (atom (car l)) (atom (cdr l)))
		     )
                 (return (printRet l)))
                ((<& (+ rmar (flatc l (charcnt poport)))
		    (charcnt poport))
		 ;
		 ;	This is just a heuristic - if print can fit it in then figure that
;	the printmacros won't hurt.  Note that despite the pretentions there
;	is no guarantee that everything will fit in before rmar - for example
;	atoms (and now even hunks) are just blindly printed.	- DNC
;
                 (printAccross l lmar rmar))
                ((and ($patom1 lAngle)
                      (atom (car l))
                      (not (atom (cdr l)))
                      (not (atom (cddr l))))
                 (prog (c)
                       (printRet (car l))
                       ($patom1 '" ")
                       (setq c (nwritn))
                  a    (prD1 (cdr l) c)
                       (cond
                        ((not (atom (cdr (setq l (cdr l)))))
                         (terpri)
                         (go a)))))
                (t
                 (prog (c)
                       (setq c (nwritn))
                  a    (prD1 l c)
                       (cond
                        ((not (atom (setq l (cdr l))))
                         (terpri)
                         (go a))))))
     b    ($patom1 rAngle))))


(def prD1
  (lambda (l n)
    (prog nil
          (prDF (car l)
                 n
                 (cond ((null (setq l (cdr l))) (|1+| rmar))
                       ((atom l) (setq n nil) (plus 4 rmar (pntlen l)))
                       (t rmar)))

;         The last arg to prDF is the space needed for the suffix
;	  Note that this is still not really right - if the prefix
;		takes several lines one would like to use the old rmar 
;		until the last line where the " . mumble" goes.
	)))


(def printAccross
  (lambda (l lmar rmar)
    (prog nil
;     this is needed to make sure the printmacros are executed
          (princ '|<|)
     l:   (cond ((null l))
                (t (prDF (car l) (nwritn) rmar)
                   (setq l (cdr l))
                   (cond (l (princ '| |)))
                   (go l:))))))
