;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-handlers.l "@(#)handlers.l	5.1 (Berkeley) %G%")

;; Handlers snarfed from FRANZ

; special atoms:
(declare (special debug-level-count break-level-count
		  errlist tpl-errlist user-top-level
		  franz-not-virgin piport ER%tpl ER%all
		  $ldprint ptport infile
		  top-level-eof * ** *** + ++ +++ ^w)
	 (macros t))

(eval-when (compile eval load)
  (or (get 'fpMacs 'loaded) (load 'fpMacs)))


; this is the break handler, it should be tied to 
; ER%tpl always.
; it is entered if there is an error which no one wants to handle.
; We loop forever, printing out our error level until someone
; types a ^D which goes to the next break level above us (or the 
; top-level if there are no break levels above us.
; a (return n) will return that value to the error message
; which called us, if that is possible (that is if the error is
; continuable)
;
(def break-err-handler 
  (lexpr (n)
	 ((lambda (message break-level-count retval rettype ^w)
		  (setq piport nil)
		  (cond ((greaterp n 0) 
			 (cond ((eq (cadddr (arg 1)) '|NAMESTACK OVERFLOW|)
				
				(msg N "non-terminating" (N 2) '? N)
				(cond 
				 (ptport
				  (let ((scriptName (truename ptport)))
				       (resetio)
				       (setq ptport (outfile scriptName 'append))
				       (cond ((null ptport)
					      (msg "can't reopen script-file "
						   scriptName
						   N))))))
				(and (null ptport) (resetio))
				(reset)))
			 (print 'Error:)
			 (mapc '(lambda (a) (patom " ") (patom a) ) 
			       (cdddr (arg 1)))
			 (terpr)
			 (cond ((caddr (arg 1)) (setq rettype 'contuab))
			       (t (setq rettype nil))))
			(t (setq rettype 'localcall)))
		  
		  (do nil (nil)
		      (cond ((dtpr 
			      (setq
			       retval
			       (*catch
				'break-catch 
				(do ((form)) (nil)
				    (patom "<")
				    (patom break-level-count)
				    (patom ">: ")
				    (cond ((eq top-level-eof
					       (setq form (read nil top-level-eof)))
					   (cond ((null (status isatty))
						  (exit)))
					   (eval 1)		; force interrupt check
					   (return (sub1 break-level-count)))
					  ((and (dtpr form) (eq 'return (car form)))
					   (cond ((or (eq rettype 'contuab) 
						      (eq rettype 'localcall))
						  (return (ncons (eval (cadr form)))))
						 (t (patom "Can't continue from this error")
						    (terpr))))
					  ((and (dtpr form) (eq 'retbrk (car form)))
					   (cond ((numberp (setq form (eval (cadr form))))
						  (return form))
						 (t (return (sub1 break-level-count)))))
					  (t (print (eval form))
					     (terpr)))))))
			     (return (cond ((eq rettype 'localcall) 
					    (car retval))
					   (t retval))))
			    ((lessp retval break-level-count)
			     (setq tpl-errlist errlist)
			     (*throw 'break-catch retval))
			    (t (terpr)))))
	  nil
	  (add1 break-level-count)
	  nil
	  nil
	  nil)))



; this reset function is designed to work with the franz-top-level.
; When franz-top-level begins, it makes franz-reset be reset. 
; when a reset occurs now, we set the global variable tpl-errlist to 
; the current value of errlist and throw to top level.  At top level,
; then tpl-errlist will be evaluated.
;
(def franz-reset
  (lambda nil
	  (setq tpl-errlist errlist)
	  (errset (*throw 'top-level-catch '?)
		  nil)
	  (old-reset-function)))



;---- autoloader functions


(def undef-func-handler
  (lambda (args)
	  (prog (funcnam file n)
		(setq funcnam (caddddr args))
		(setq n (nreverse (explode (setq funcnam (caddddr args)))))
		(cond ((and (not (greaterp 4 (length n)))
			    (eq 'pf_ (implode `(,(car n) ,(cadr n) ,(caddr n)))))
		       (cond ((and ptport (null infile)) (terpri ptport)))
		       (msg N (implode (nreverse (cdddr n))) " not defined"
			    N)
		       (bottom))
		      (t
		       (cond ((symbolp funcnam) 
			      (cond ((setq file (get funcnam 'autoload))
				     (cond ($ldprint
					    (patom "[autoload ") (patom file)
					    (patom "]")(terpr)))
				     (load file))
				    (t (return nil)))
			      (cond ((getd funcnam) (return (ncons funcnam)))
				    (t (patom "Autoload file does not contain func ")
				       (return nil))))))))))



(defun break-resp (x)		; reset on a break (handled like inf recursion)
  (msg (N 2) "       [break]" (N 2) '? N)
  (cond 
   (ptport
    (let ((scriptName (truename ptport)))
	 (resetio)
	 (setq ptport (outfile scriptName 'append))
	 (cond ((null ptport)
		(msg "can't reopen script-file " scriptName N))))))
  (and (null ptport) (resetio))
  (reset))

