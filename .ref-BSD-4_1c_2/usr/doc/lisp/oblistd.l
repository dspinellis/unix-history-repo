; oblist dumping program

(def dumpit
  (lambda (x)
	  (setq prt (outfile x))	
	  (do ((i (oblist) (cdr i)))
	      ((null i))
	      (setq nm (car i))
	      (cond ((getd nm) (print nm prt)(terpr prt))))))
