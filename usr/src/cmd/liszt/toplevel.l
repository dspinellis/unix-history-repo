
; special atoms:
(declare (special debug-level-count break-level-count
		  errlist tpl-errlist user-top-level
		  top-level-eof * - ^w)
	 (macros t))

(setq top-level-eof (gensym 'Q)
      tpl-errlist nil
      errlist nil
      user-top-level nil )

;------------------------------------------------------
;  Top level function for franz			jkf, march 1980
;
; The following function contains the top-level read, eval, print 
; loop.  With the help of the error handling functions, 
; break-err-handler and  debug-err-handler,  franz-top-level provides
; a reasonable enviroment for working with franz lisp.  
; 

(def franz-top-level
  (lambda nil
      (cond ((or (not (boundp 'franz-not-virgin))
		 (null franz-not-virgin))
	     (patom (status version))
	     (setq franz-not-virgin t)
	     (setq ER%tpl 'break-err-handler)
	     (putd 'reset (getd 'franz-reset))
	     (terpr)
	     (read-in-lisprc-file)))
     
     ; loop forever
     (do nil (nil)
	 (setq retval
	  (*catch 
	  '(top-level-catch break-catch)
	   ; begin or return to top level
	   (progn
             (setq debug-level-count 0   break-level-count 0)
             (cond (tpl-errlist (mapc 'eval tpl-errlist)))
	     (do ((^w nil nil))
		 (nil)
		 (cond (user-top-level (funcall user-top-level))
		       (t (patom "-> ")
			  (cond ((eq top-level-eof
				     (setq - 
					   (car (errset (read nil 
							      top-level-eof)))))
				 (cond ((not (status isatty))
					(exit)))
				 (cond ((null (status ignoreeof))
					(terpr)
					(print 'Goodbye)
					(terpr)
					(exit))
				       (t (terpr)
					  (setq - ''EOF)))))
			  (setq * (eval -))
			  (print *)
			  (terpr)))))))
	 (terpr)
	 (patom "[Return to top level]")
	 (terpr)
	 (cond ((eq 'reset retval) (old-reset-function))))))


; debug-err-handler is the clb of ER%all when we are doing debugging
; and we want to catch all errors.
; It is just a read eval print loop with errset.
; the only way to leave is: 
;   (reset) just back to top level
;   (return x) return the value, if possible
;   ^D	continue as if this handler wasn't called.
; the form of errmsgs is:
;  (error_type unique_id continuable message_string other_args ...)
;
(def debug-err-handler 
  (lexpr (n)
	  ((lambda (message debug-level-count retval ^w)
	       (cond ((greaterp n 0)
		      (print 'Error:)
		      (mapc '(lambda (a) (patom " ") (patom a) ) 
			    (cdddr (arg 1)))
		      (terpr)))
	       (setq ER%all 'debug-err-handler)
	       (do nil (nil)
		   (cond ((setq retval 
			   (dtpr 
			    (errset 
			     (do ((form)) (nil)
				(patom "D<")
				(patom debug-level-count)
				(patom ">: ")
				(cond ((eq top-level-eof
					   (setq form (read nil top-level-eof)))
				       (cond ((null (status isatty))
					      (exit)))
				       (return nil))
				      ((and (dtpr form)
					    (eq 'return (car form)))
				       (cond ((caddr errmsgs)
					      (return (ncons (eval (cadr form)))))
					     (t (patom "Can't continue from this error"))))
				      (t (print (eval form))
					 (terpr)))))))
				(return (car retval))))))
	   nil
	   (add1 debug-level-count)
	   nil
	   nil)))

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
		      (print 'Error:)
		      (mapc '(lambda (a) (patom " ") (patom a) ) 
				    (cdddr (arg 1)))
		      (terpr)
		      (cond ((caddr (arg 1)) (setq rettyp 'contuab))
			    (t (setq rettyp nil))))
		     (t (setq rettyp 'localcall)))

	       (do nil (nil)
		   (cond ((dtpr 
			   (setq retval
			    (*catch 'break-catch 
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
				       (cond ((or (eq rettyp 'contuab) 
						  (eq rettyp 'localcall))
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

(def debugging 
  (lambda (val)
	  (cond (val (setq ER%all 'debug-err-handler))
		(t (setq ER%all nil)))))


; the problem with this definition for break is that we are
; forced to put an errset around the break-err-handler. This means
; that we will never get break errors, since all errors will be
; caught by our errset (better ours than one higher up though).
; perhaps the solution is to automatically turn debugmode on.
;
(defmacro break (message &optional (pred t))
  `(*break ,pred ',message))

(def *break
  (lambda (pred message)
     (let ((^w nil))
	  (cond ((not (boundp 'break-level-count)) (setq break-level-count 1)))
	  (cond (pred (terpr)
		      (patom "Break ")
		      (patom message)
		      (terpr)
		      (do ((form))
			  (nil)
			  (cond ((dtpr (setq form (errset (break-err-handler))))
				 (return (car form))))))))))


; this reset function is designed to work with the franz-top-level.
; When franz-top-level begins, it makes franz-reset be reset. 
; when a reset occurs now, we set the global variable tpl-errlist to 
; the current value of errlist and throw to top level.  At top level,
; then tpl-errlist will be evaluated.
;
(def franz-reset
  (lambda nil
	  (setq tpl-errlist errlist)
	  (errset (*throw 'top-level-catch 'reset)
		  nil)
	  (old-reset-function)))


; this definition will have to do until we have the ability to
; cause and error on any channel in franz
(def error
  (lexpr (n)
	 (cond ((greaterp n 0)
		(patom (arg 1))
		
		(cond  ((greaterp n 1)
			(patom " ")
			(patom (arg 2))))
		(terpr)))
	 (err)))


; this file is read in just before dumplisping if you want .lisprc
; from your home directory read in before the lisp begins.
(def read-in-lisprc-file
  (lambda nil
	  ((lambda (hom prt)
		   (setq break-level-count 0	; do this in case break
			 debug-level-count 0)   ; occurs during readin
		   (*catch '(break-catch top-level-catch)
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
				      (terpr)))))))
	   (getenv 'HOME) nil)))

(putd 'top-level (getd 'franz-top-level))

; if this is the first time this file has been read in, then 
; make franz-reset be the reset function, but remember the original
; reset function as old-reset-function.  We need the old reset function
; if we are going to allow the user to change top-levels, for in 
; order to do that we really have to jump all the way up to the top.
(cond ((null (getd 'old-reset-function))
       (putd 'old-reset-function (getd 'reset))))
