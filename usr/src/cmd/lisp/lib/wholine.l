;; programs to send information to the who line of a h19 terminal
;
; call is (whofilewrite '(list of stuff to print out))
;
(setq whofilename nil wholineexists nil)
(defun whofilewrite (stuff)
  (cond ((null wholineexists) 
	 (cond ((equal "19" (substring (getenv 'TERM) 2 2))
		(setq wholineexists t))
	       (t (setq wholineexists 'nope)))))
  (cond ((eq wholineexists t)
	 (cond ((null whofilename) 
		(setq whofilename (concat (getenv 'HOME) "/.who"))))
	 (cond ((errset (progn (setq whooutport (outfile whofilename))
			       (do ((xx stuff (cdr xx)))
				   ((null xx) (close whooutport))
				   (patom (car xx) whooutport)
				   (cond ((cdr xx) (patom " " whooutport)))))))))))

(putprop 'wholine t 'version)
