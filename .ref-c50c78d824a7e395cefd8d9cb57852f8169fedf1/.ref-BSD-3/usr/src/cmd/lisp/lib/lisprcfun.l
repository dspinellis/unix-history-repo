; this file is read in just before dumplisping if you want .lisprc
; from your home directory read in before the lisp begins.
(def lisprc-top-level
  (lambda nil
	  ((lambda (hom prt)
		   (cond (hom
			  (cond ((and 
				  (errset 
				   (progn
				    (setq prt (infile (concat hom '"/.lisprc")))
				    (close prt))
				   nil)
				  (null (errset
					 (load (concat hom '"/.lisprc")))))
				  (patom '"Error in .lisprc file detected")
				  (terpr)))))
		   (putd 'top-level original-top-level))
	   (getenv 'HOME) nil)))

(setq original-top-level (getd 'top-level))
(putd  'top-level (getd 'lisprc-top-level))
