(def mystify
 (lambda (x) (cond (x (scons (car x) (mystify (cdr x))))
		   (t nil]

(def listify
 (lambda (x) (cond (x (cons (car x) (listify (cdr x))))
		   (t nil]
