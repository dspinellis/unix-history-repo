;;; This is a bootstrap file to allow the true defns to be read in.

(comment dv dvfns (dv %changes mark!changed))

(def dv (nlambda (l) (set (car l) (cadr l)) (mark!changed (car l))))

(setq %changes nil)

(def mark!changed
     (lambda 
      (x)
      (cond ((not (memq x %changes)) (setq %changes (cons x %changes))))
      x))

