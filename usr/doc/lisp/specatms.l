; program to determine the special atoms
(setq specs (mapcan '(lambda (x) (cond ((and (not (getd x))
					     (boundp x))
					(ncons x))
				       (t nil)))
		    (oblist)))
