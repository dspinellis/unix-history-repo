
(declare (special args c))	; LWE This grossness is necessary for the macro
				; exists to work properly. (Maybe.)
(declare (special piport))
(def dc-dskin			; LWE Hacking to compile OK
  (nlambda (args)
    (prog (tmp tmp1 tmp2)
	  [setq tmp
		(prog (c cc)
			(setq cc (get (car args) 'comment))
		loop	(cond ((not cc)(return nil)))
			(setq c (car cc))
			(cond ((eq (car c)(cadr args))
			       (return nil)))
			(setq cc (cdr cc))
			(go loop]
	  (setq tmp2 piport)
	  (setq tmp1 (get-comment 27 tmp2))
	  (cond (tmp  (disgusting tmp
				 (cons (cadr args)
				       (cons (caddr args) tmp1))))
;                 (rplaca tmp
;                         (cons (cadr args)
;                               (cons (caddr args) tmp1 ))))
                (t
                 (putprop (car args)
                          (cons (cons (cadr args)
                                      (cons (caddr args) tmp1))
                                (get (car args) 'comment))
                          'comment)))
          (mark!changed (car args))
          (return nil]

(def disgusting (lambda (a b) ; (rplaca a b)))
			b))

(def get-comment
  (lambda (stopper piport)
    (prog (ans line)
          (cond ((eq 10 (tyipeek piport)) (tyi piport)))
     l:   (setq line nil)
;          (until (member (car line) (list 10 stopper))
;                 (setq line (cons (tyi piport) line)))
	   (prog nil loop
		(cond ((member (car line)(list 10 stopper))
		       (return nil)))
	        (setq line (cons (tyi piport) line))
		(go loop))
          (setq ans (cons (implode (dreverse (cdr line))) ans))
          (cond ((eq (car line) 10) (go l:)) (t (return (dreverse ans)))))))
