(defun build-sysp nil
  (do ((temp (oblist) (cdr temp))(sysfuncs))
      ((null temp)(setq system-functions\ sysfuncs));atom has ^G at end
      (cond ((getd (car temp))
	     (setq sysfuncs (cons (car temp) sysfuncs))))))

(defun sysp (x) (cond ((memq x system-functions\)t)))
(or (boundp 'system-functions\) (build-sysp))

(defun fretry (pdlpnt frame)
  (freturn pdlpnt
	   (cond ((eq (car frame) 'eval) (eval (caddr frame) (cadddr frame)))
		 ((eq (car frame) 'apply)
		  (eval `(apply ',(caaddr frame) ',(cadaddr frame)) 
			(cadddr frame))))))

