(setq rcs-toplevel-
   "$Header: toplevel.l,v 1.6 83/11/18 08:47:24 jkf Exp $")

;;
;; toplevel.l				-[Sun Oct 30 08:14:49 1983 by jkf]-
;;
;;  toplevel read eval print loop
;;


; special atoms:
(declare (special debug-level-count break-level-count
		  errlist tpl-errlist user-top-level
		  franz-not-virgin piport ER%tpl ER%all
		  $ldprint evalhook funcallhook
		  franz-minor-version-number
		  top-level-init
		  top-level-prompt top-level-read
		  top-level-eval top-level-print
		  top-level-eof * ** *** + ++ +++ ^w)
         (localf autorunlisp cvtsearchpathtolist)
	 (macros t))

(setq top-level-eof (gensym 'Q)
      tpl-errlist nil
      errlist nil
      user-top-level nil
      top-level-init nil
      top-level-prompt nil
      top-level-read  nil
      top-level-eval nil
      top-level-print nil)

;--- initialization, prompt, read, eval, and print functions are
; user-selectable by just assigning another value to top-level-init,
; top-level-prompt, top-level-read, top-level-eval, and top-level-print.
;
(defmacro top-init nil
   '(cond ((and top-level-init
		(getd top-level-init))
	   (funcall top-level-init))
	  (t (cond ((not (autorunlisp))
		    (patom (status version))
		    ; franz-minor-version-number defined in version.l
		    (cond ((boundp 'franz-minor-version-number)
			   (patom franz-minor-version-number)))
		    (terpr)
		    (read-in-lisprc-file))))))
     
(defmacro top-prompt nil
   `(cond ((and top-level-prompt
		(getd top-level-prompt))
	   (funcall top-level-prompt))
	  (t (patom "-> "))))

(defmacro top-read (&rest args)
   `(cond ((and top-level-read
		(getd top-level-read))
	   (funcall top-level-read ,@args))
	  (t (read ,@args))))

(defmacro top-eval (&rest args)
   `(cond ((and top-level-eval
		(getd top-level-eval))
	   (funcall top-level-eval ,@args))
	  (t (eval ,@args))))

(defmacro top-print (&rest args)
   `(cond ((and top-level-print
		(getd top-level-print))
	   (funcall top-level-print ,@args))
	  (t (print ,@args))))

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
     (putd 'reset (getd 'franz-reset))
     (username-to-dir-flush-cache)	 ; clear tilde expansion knowledge
      (cond ((or (not (boundp 'franz-not-virgin))
		 (null franz-not-virgin))
	     (setq franz-not-virgin t
		   + nil ++ nil +++ nil
		   * nil ** nil *** nil)
	     (setq ER%tpl 'break-err-handler)
	     (top-init)))
     
     ; loop forever
     (do ((+*) (-) (retval))
	 (nil)
	 (setq retval
	  (*catch 
	  '(top-level-catch break-catch)
	   ; begin or return to top level
	   (progn
             (setq debug-level-count 0   break-level-count 0
		   evalhook nil	  funcallhook nil)
             (cond (tpl-errlist (mapc 'eval tpl-errlist)))
	     (do ((^w nil nil))
		 (nil)
		 (cond (user-top-level (funcall user-top-level))
		       (t (top-prompt)
			  (cond ((eq top-level-eof
				     (setq - 
					   (car (errset (top-read nil 
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
			  (setq +* (top-eval -))
			  ; update list of old forms
			  (let ((val -))
			       (let ((o+ +) (o++ ++))
				    (setq +   val
					  ++  o+
					  +++ o++)))
			  ; update list of old values
			  (let ((val +*))
			       (let ((o* *) (o** **))
				    (setq *   val
					  **  o*
					  *** o**)))
			  (top-print +*)
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
;   (return x) return the value to the error checker. 
;		if nil is returned then we will continue as if the error
;		didn't occur. Otherwise if the returned value is a list,
;		then if the error is continuable, the car of that list
;		will be returned to recontinue computation.
;   ^D	continue as if this handler wasn't called.
; the form of errmsgs is:
;  (error_type unique_id continuable message_string other_args ...)
;
(def debug-err-handler
   (lexpr (n)
	  ((lambda (message debug-level-count retval ^w piport)
	      (cond ((greaterp n 0)
		     (print 'Error:)
		     (mapc '(lambda (a) (patom " ") (patom a) )
			   (cdddr (arg 1)))
		     (terpr)))
	      (setq ER%all 'debug-err-handler)
	      (do ((retval)) (nil)
		  (cond ((dtpr
			    (setq retval
				  (errset
				     (do ((form)) (nil)
					 (patom "D<")
					 (patom debug-level-count)
					 (patom ">: ")
					 (cond ((eq top-level-eof
						    (setq form
							  (top-read nil
								top-level-eof)))
						(cond ((null (status isatty))
						       (exit)))
						(return nil))
					       ((and (dtpr form)
						     (eq 'return
							 (car form)))
						(return (eval (cadr form))))
					       (t (setq form (top-eval form))
						  (top-print form)
						  (terpr)))))))
			 (return (car retval))))))
	   nil
	   (add1 debug-level-count)
	   nil
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
     ((lambda (message break-level-count retval rettype ^w piport)
	 (cond ((greaterp n 0)
		(print 'Error:)
		(mapc '(lambda (a) (patom " ") (patom a) )
		      (cdddr (arg 1)))
		(terpr)
		(cond ((caddr (arg 1)) (setq rettype 'contuab))
		      (t (setq rettype nil))))
	       (t (setq rettype 'localcall)))

	 (do nil (nil)
	     (cond ((dtpr
		       (setq retval
			     (*catch 'break-catch
				 (do ((form)) (nil)
				     (patom "<")
				     (patom break-level-count)
				     (patom ">: ")
				     (cond ((eq top-level-eof
						(setq form
						      (top-read
							 nil
							 top-level-eof)))
					    (cond ((null (status isatty))
						   (exit)))
					    (eval 1)	; force interrupt check
					    (return (sub1 break-level-count)))
					   ((and (dtpr form)
						 (eq 'return (car form)))
					    (cond ((or (eq rettype 'contuab)
						       (eq rettype 'localcall))
						   (return (ncons (top-eval (cadr form)))))
						  (t (patom "Can't continue from this error")
						     (terpr))))
					   ((and (dtpr form) (eq 'retbrk (car form)))
					    (cond ((numberp (setq form (top-eval (cadr form))))
						   (return form))
						  (t (return (sub1 break-level-count)))))
					   (t (setq form (top-eval form))
					      (top-print form)
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
      nil
      nil)))

(defvar debug-error-handler 'debug-err-handler) ; name of function to get
						; control on ER%all error
(def debugging 
  (lambda (val)
	  (cond (val (setq ER%all debug-error-handler)
		     (sstatus translink nil)
		     (*rset t))
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


(declare (special $ldprint))

;--- read-in-lisprc-file
; search for a lisp init file.  Look first in . then in $HOME
; look first for .o , then .l and then "",
; look for file bodies .lisprc and then lisprc
; 
(def read-in-lisprc-file
   (lambda nil
      (setq break-level-count 0	; do this in case break
	    debug-level-count 0)   ; occurs during readin
      (*catch '(break-catch top-level-catch)
	      (do ((dirs `("." ,(getenv 'HOME)) (cdr dirs))
		   ($ldprint nil $ldprint))	; prevent messages
		  ((null dirs))
		  (cond ((do ((name '(".lisprc" "lisprc") (cdr name)))
			     ((null name))
			     (cond ((do ((ext '(".o" ".l" "") (cdr ext))
					 (file))
					((null ext))
					(cond ((probef
						  (setq file
							(concat (car dirs)
								"/"
								(car name)
								(car ext))))
					       (cond ((atom (errset (load file)))
						      (patom
							 "Error loading lisp init file ")
						      (print file)
						      (terpr)
						      (return 'error)))
					       (return t))))
				    (return t))))
			 (return t)))))))

(putd 'top-level (getd 'franz-top-level))

; if this is the first time this file has been read in, then 
; make franz-reset be the reset function, but remember the original
; reset function as old-reset-function.  We need the old reset function
; if we are going to allow the user to change top-levels, for in 
; order to do that we really have to jump all the way up to the top.
(cond ((null (getd 'old-reset-function))
       (putd 'old-reset-function (getd 'reset))))


;---- autoloader functions

(def undef-func-handler
  (lambda (args)
    (prog (funcnam file)
	  (setq funcnam (caddddr args))
	  (cond ((symbolp funcnam) 
		 (cond ((setq file (or (get funcnam 'autoload)
				       (get funcnam 'macro-autoload)))
			(cond ($ldprint
			       (patom "[autoload ") (patom file)
			       (patom "]")(terpr)))
			(load file))
		       (t (return nil)))
		 (cond ((getd funcnam) (return (ncons funcnam)))
		       (t (patom "Autoload file " ) (print file)
			  (patom " does not contain function ")
			  (print funcnam)
			  (terpr)
			  (return nil))))))))

(setq ER%undef 'undef-func-handler)

(declare (special $ldprint))
;--- autorunlisp :: check if this lisp is supposed to run a program right
; away.
;
(defun autorunlisp nil
  (cond ((and (> (argv -1) 2) (equal (argv 1) '-f))
	 (let ((progname (argv 2))
	       ($ldprint nil)
	       (searchlist nil))	; don't give fasl messages
	      (setq searchlist (cvtsearchpathtolist (getenv 'PATH)))
	      ; give two args to load to insure that a fasl is done.
	      (cond ((null 
		      (errset (load-autorunobject progname searchlist)))
		     (exit 0))
		    (t t))))))


(defun cvtsearchpathtolist (path)
  (do ((x (explodec path) (cdr x))
       (names nil)
       (cur nil))
      ((null x) 
       (nreverse names))
      (cond ((or (eq ': (car x)) 
		 (and (null (cdr x)) (setq cur (cons (car x) cur))))
	     (cond (cur (setq names (cons (implode (nreverse cur))
					  names))
			(setq cur nil))
		   (t (setq names (cons '|.| names)))))
	    (t (setq cur (cons (car x) cur))))))

(defun load-autorunobject (name search)
  (cond ((memq (getchar name 1) '(/ |.|))
	 (cond ((probef name) (fasl name))
	       (t (error "From lisp autorun: can't find file to load"))))
	(t (do ((xx search (cdr xx))
		(fullname))
	       ((null xx) (error "Can't find file to execute "))
	       (cond ((probef (setq fullname (concat (car xx) "/" name)))
		      (return (fasl-a-file fullname nil nil))))))))

;--- command-line-args :: return a list of the command line arguments
; The list does not include the name of the program being executed (argv 0).
; It also doesn't include the autorun flag and arg.
;
(defun command-line-args ()
   (do ((res nil (cons (argv i) res))
	(i (1- (argv -1)) (1- i)))
       ((<& i 1)
	(if (and (eq '-f (car res))
		 (cdr res))
	   then (cddr res)
	   else res))))

(defun debug fexpr (args)
  (load 'fix)	; load in fix package
  (eval (cons 'debug args)))	; enter debug through eval

;-- default autoloader properties

(putprop 'trace (concat lisp-library-directory "/trace") 'autoload)
(putprop 'untrace (concat lisp-library-directory "/trace") 'autoload)

(putprop 'step (concat lisp-library-directory "/step") 'autoload)
(putprop 'editf (concat lisp-library-directory "/cmuedit") 'autoload)
(putprop 'editv (concat lisp-library-directory "/cmuedit") 'autoload)
(putprop 'editp (concat lisp-library-directory "/cmuedit") 'autoload)
(putprop 'edite (concat lisp-library-directory "/cmuedit") 'autoload)

(putprop 'defstruct (concat lisp-library-directory "/struct") 'macro-autoload)
(putprop 'defstruct-expand-ref-macro
	 (concat lisp-library-directory "/struct") 'autoload)
(putprop 'defstruct-expand-cons-macro
	 (concat lisp-library-directory "/struct") 'autoload)
(putprop 'defstruct-expand-alter-macro
         (concat lisp-library-directory "/struct") 'autoload)

(putprop 'loop      (concat lisp-library-directory "/loop")   'macro-autoload)
(putprop 'defflavor
	 (concat lisp-library-directory "/flavors") 'macro-autoload)
(putprop 'defflavor1
	 (concat lisp-library-directory "/flavors") 'autoload)

(putprop 'format (concat lisp-library-directory "/format") 'autoload)
(putprop 'ferror (concat lisp-library-directory "/format") 'autoload)

(putprop 'make-hash-table
	 (concat lisp-library-directory "/hash") 'autoload)
(putprop 'make-equal-hash-table
	 (concat lisp-library-directory "/hash") 'autoload)

(putprop 'describe (concat lisp-library-directory "/describe") 'autoload)

(putprop 'cgol (concat lisp-library-directory "/cgol/cgoll")   'autoload)
(putprop 'cgolprint (concat lisp-library-directory "/cgol/cgp")   'autoload)

; probably should be in franz so we don't have to autoload
(putprop 'displace  (concat lisp-library-directory "/machacks")   'autoload)

(putprop 'defrecord (concat lisp-library-directory "/record") 'macro-autoload)
(putprop 'record-pkg-construct
   (concat lisp-library-directory "/record") 'autoload)
(putprop 'record-pkg-access
   (concat lisp-library-directory "/record") 'autoload)
(putprop 'record-pkg-illegal-access
   (concat lisp-library-directory "/record") 'autoload)
