
;; This file contains macros for the compiler to use to expand CMULisp 
;; functions.  In particular, it should be included in order to compile
;; functions which use:
;; ***, selectq, quote!
;;								- DNC
;; LWE 1/11/80 Added prin1 to this nasty motherfucker because the CMU
;; stuff needs it, but it ain't around anywhere.

(def prin1 (lambda (x) (print x))) ; doesn't take into account 
				    ; possible file arg, but that's the 
				    ; breaks.

(declare (special selectq) (macros t))

(def *** (macro (l) nil))

(def selectq 
 (macro(l)
  (prog (firstcl restcl rslt)
	(setq rslt (ncons (quote cond)))
	(cond ((atom (car (setq l (cdr l)))) (setq firstcl (setq restcl (car l))))
	      ((eq (caar l) (quote setq)) (setq firstcl (car l)) (setq restcl (cadar l)))
	      (t (setq firstcl (list (quote setq) (setq restcl (quote selectq)) (car l)))))
   lp   (cond
	 ((cdr (setq l (cdr l))) (nconc rslt
					(ncons
					 (cons (list (cond ((atom (caar l)) (quote eq)) (t (quote memq)))
 						     firstcl
						     (list (quote quote) (caar l)))
					       (cdar l))))
				 (setq firstcl restcl)
				 (go lp)))
	(nconc rslt (ncons (cons t l)))
	(return rslt))) 
]


(def quote!x
 (lambda(x)
  (prog	(y z)
	(return
	 (cond ((atom x) x)
	       ((and (eq (car x) (quote !)) (cdr x))
		(setq y (quote!x (cddr x)))
		(cond ((cddr x) (list (quote cons) (cadr x) (cond ((and y (eq y (cddr x))) (kwote y)) (t y))))
		      (t (list (quote ncons) (cadr x)))))
	       ((and (eq (car x) (quote !!)) (cdr x))
		(setq y (quote!x (cddr x)))
		(cond ((cddr x)
		       (list (quote append) (cadr x) (cond ((and y (eq y (cddr x))) (kwote y)) (t y))))
		      (t (cadr x))))
	       (t (setq y (quote!x (car x)))
		  (setq z (quote!x (cdr x)))
		  (cond	((eq y (car x))
			 (cond ((eq z (cdr x)) x)
			       (y (list (quote cons) (kwote y) z))
			       (t (list (quote cons) y z))))
			((and z (eq z (cdr x))) (list (quote cons) y (kwote z)))
			(t (list (quote cons) y z))))))))
)

(def quote!
 (macro (l) (prog (x) (setq x (quote!x (cdr l))) (return (cond ((eq x (cdr l)) (kwote x)) (t x)))))
)
