(setq SCCS-mystify "@(#)mystify.l	1.1	10/2/80")
(setsyntax '\; 'splicing 'zapline)
;
; listify takes a bignum and turns it into  a list of fixnums
; mystify is its inverse function

(def mystify
 (lambda (x) (cond (x (scons (car x) (mystify (cdr x))))
		   (t nil]

(def listify
 (lambda (x) (cond (x (cons (car x) (listify (cdr x))))
		   (t nil]
