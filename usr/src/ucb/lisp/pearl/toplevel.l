;;;;;;;;;;;;;;;;;;;;;;;;;;;;; toplevel.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Franz and UCI Lisp top level functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering and John Foderaro.

;-------------------------------------------------------------------------
;  Top level functions for PEARL		Joe Faletti, December 1981
;      modified from
;  Top level function for franz			jkf, march 1980
;
; The following function contains the top-level read, eval, print 
; loop.  With the help of the usual error handling functions, 
; pearl-break-err-handler and  debug-err-handler,  pearl-top-level provides
; a reasonable environment for working with PEARL.  
; 

(defvar \$ldprint)

; Handle ^C with fixit.
(de pearl:int-serv (x)
  (fixit nil))

; Before Opus 38.31:
; (setq pearl-title (concat " plus PEARL " (status ctime)))
; Moved to franz.l:
; (setq pearl-title (concat " plus PEARL " (time-string)))

(de read-in-initprl-file ()
  (setq break-level-count 0    ; do this in case break
	debug-level-count 0)   ; occurs during readin
  (*catch '(break-catch top-level-catch)
	  (do ((dirs `("." ,(getenv 'HOME)) (cdr dirs))
	       ; prevent warnings (from setdbsize in particular).
	       (*warn* nil *warn*)
	       (\$ldprint nil \$ldprint))	; prevent messages
	      ((null dirs))
	      (cond ((do ((name '(".init.prl" "init.prl") (cdr name)))
			 ((null name))
			 (cond ((do ((ext '(".o" ".l" "") (cdr ext))
				     (file))
				    ((null ext))
				    (cond ((probef
					    (setq file (concat (car dirs)
							       "/"
							       (car name)
							       (car ext))))
					   (cond ((atom (errset (load file)))
						  (patom
						   "Error loading init.prl file ")
						  (print file)
						  (terpr)
						  (return 'error)))
					   (return t))))
				(return t))))
		     (return t))))))

(de read-in-startprl-file ()
  (setq break-level-count 0    ; do this in case break
	debug-level-count 0)   ; occurs during readin
  (*catch '(break-catch top-level-catch)
	  (do ((dirs `("." ,(getenv 'HOME)) (cdr dirs))
	       (\$ldprint nil \$ldprint))	; prevent messages
	      ((null dirs))
	      (cond ((do ((name '(".start.prl" "start.prl") (cdr name)))
			 ((null name))
			 (cond ((do ((ext '(".o" ".l" "") (cdr ext))
				     (file))
				    ((null ext))
				    (cond ((probef
					    (setq file (concat (car dirs)
							       "/"
							       (car name)
							       (car ext))))
					   (cond ((atom (errset (load file)))
						  (patom
						   "Error loading start.prl file ")
						  (print file)
						  (terpr)
						  (return 'error)))
					   (return t))))
				(return t))))
		     (return t))))))

; For the implementor who wishes to dump a PEARL.
(df savepearl (name)
  (sstatus ignoreeof nil)     ; to undo ~/.lisprc
  (setq franz-not-virgin nil)
  (aliasdef 'top-level 'pearl-top-level-init)
  (setq \$gcprint nil)
  (gc)			; garbage collect before dumping lisp
  (cond (name (eval (list 'dumplisp (car name))))
	( t (dumplisp pearl)))
  t)

; For the user who wishes to dump a PEARL that starts with .init.prl.
(de savefresh n
  (prog (name)
	;   (INITFN 'STARTUPPEARL)
	(setq franz-not-virgin nil)
	(aliasdef 'top-level 'pearl-top-level-init)
	(setq \$gcprint nil)
	(gc)			; garbage collect before dumping lisp
	(cond ((\=& n 1) (setq name (arg 1)))
	      ((\=& n 2) (setq name (concat (arg 1) '|/| (arg 2))))
	      ( t (setq name 'pearl)))
	(eval (list 'dumplisp name))
	(return t)))

; For the user who wishes to dump a PEARL that continues with the
;     read-eval-print loop.
(de savecontinue n
  (prog (name)
	;   (INITFN 'PEARL-REP-LOOP)
	(aliasdef 'top-level 'pearl-top-level)
	(setq \$gcprint nil)
	(gc)			; garbage collect before dumping lisp
	(cond ((\=& n 1) (setq name (arg 1)))
	      ((\=& n 2) (setq name (concat (arg 1) '|/| (arg 2))))
	      ( t (setq name 'pearl)))
	(eval (list 'dumplisp name))
	(return t)))

(de pearlreploop ()
  (prog (*pval*)
	*pearlloop*
	(terpri)
	(and *printhistorynumber*
	     (patom (1+ *historynumber*)))
	(patom *pearlprompt*)
	(setq *readlinechanged* nil)
	(cond ((eq (unbound)
		   (setq *pval*
			 (car (errset (eval (addhistory (read)))))))
	       (rplacx (\\ *historynumber* *historysize*)
		       *histval*
		       (unbound))
	       (prin 'unbound))
	      ( t (rplacx (\\ *historynumber* *historysize*)
			  *histval*
			  *pval*)
		  (pearlprintfn  *pval*)))
	(go *pearlloop*)))
 
(de pearl ()
  (read-in-initprl-file)
  (cond ((not (boundp '*db1size*))
	 (setdbsize 7.)))
  (cond ((not (boundp '*db*))
	 (builddb *maindb*)
	 (setq *db* *maindb*)))
  (cond ((not (boundp '*pearlprompt*))
	 (setq *pearlprompt* '|pearl> |))
	((null *pearlprompt*)
	 (setq *pearlprompt* '|-> |)))
  (cond ((not (boundp '*historysize*))
	 (setq *historysize* 64.)))
  (setq *historynumber* -1.)
  (setq *history* (makhunk *historysize*))
  (setq *histval* (makhunk *historysize*))
  (read-in-startprl-file)
  (terpri)
  (pearlreploop))
 
(de initpearl ()
  (cond ((not (boundp '*db1size*))
	 (setdbsize 7.)))
  (cond ((not (boundp '*db*))
	 (builddb *maindb*)
	 (setq *db* *maindb*))))

(de pearl-top-level-init ()
  (aliasdef 'reset 'franz-reset)
  (aliasdef 'top-level 'pearl-top-level)
  (signal 2 'pearl:int-serv)
  (*catch '(top-level-catch break-catch)
	  (cond ((or (not (boundp 'franz-not-virgin))
		     (null franz-not-virgin))
		 (setq franz-not-virgin t
		       + nil ++ nil +++ nil
		       * nil ** nil *** nil)
		 ; This is changed because fixit is included now.
		 ;	   (setq ER%tpl 'pearl-break-err-handler)
		 (setq ER%tpl 'fixit)
		 (setq ER%brk 'fixit)
		 (setq ER%err 'fixit)
		 
		 ; The rest of the code should be within this
		 ;     cond if autorunlisp existed
		 ;          (cond ((not (autorunlisp))))
		 ;
		 (patom (status version))
		 (cond ((boundp 'franz-minor-version-number)
			(patom franz-minor-version-number)))
		 (patom pearl-title)
		 (terpr)
		 (cond (*firststartup* (setq *firststartup* nil)
				       (read-in-initprl-file)))
		 (or *pearlprompt*
		     (setq *pearlprompt* '|-> |))
		 (and (not (\=& 64 *historysize*))
		      (setq *history* (makhunk *historysize*))
		      (setq *histval* (makhunk *historysize*)))
		 (read-in-startprl-file))))
  (reset))

(de pearl-top-level ()
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
			( t ; Print prompt.
			    (and *printhistorynumber*
				 (patom (1+ *historynumber*)))
			    (patom *pearlprompt*)
			    (setq *readlinechanged* nil)
			    
			    (cond ((eq top-level-eof
				       ; read and add to history.
				       (setq - 
					     (car (errset
						   (addhistory
						    (read nil
							  top-level-eof))))))
				   (cond ((not (status isatty))
					  (exit)))
				   (cond ((null (status ignoreeof))
					  (terpr)
					  (print 'Goodbye)
					  (terpr)
					  (exit))
					 ( t (terpr)
					     (setq - ''EOF)))))
			    ; Eval and story result in history.
			    (setq +* (eval -))
			    (rplacx (\\ *historynumber* *historysize*)
				    *histval*
				    +*)
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
			    ; Don't print *invisible*.
			    (and (neq '*invisible* +*)
				 (pearlprintfn +*))
			    (terpr))))
	      (terpr)
	      (patom "[Return to top level]")
	      (terpr)
	      (cond ((eq 'reset retval) (old-reset-function))))))))

; this is the break handler, it should be tied to 
; ER%tpl always.
; it is entered if there is an error which no one wants to handle.
; We loop forever, printing out our error level until someone
; types a ^D which goes to the next break level above us (or the 
; top-level if there are no break levels above us.)
; a (return n) will return that value to the error message
; which called us, if that is possible (that is if the error is
; continuable)
;
(def pearl-break-err-handler 
  (lexpr
   (n)
   ((lambda
     (message break-level-count retval rettype ^w piport)
     (cond ((>& n 0) 
	    (print 'error:)
	    (mapc '(lambda (a) (patom " ") (patom a) ) 
		  (cdddr (arg 1)))
	    (terpr)
	    (cond ((caddr (arg 1)) (setq rettype 'contuab))
		  ( t (setq rettype nil))))
	   ( t (setq rettype 'localcall)))
     
     (do nil (nil)
	 (cond ((dtpr 
		 (setq retval
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
				   (eval 1)	; force interrupt check
				   (return (1- break-level-count)))
				  ((and (dtpr form)
					(eq 'return (car form)))
				   (cond ((or (eq rettype 'contuab) 
					      (eq rettype 'localcall))
					  (return (ncons (eval (cadr form)))))
					 ( t (patom
					      "Can't continue from this error")
					     (terpr))))
				  ((and (dtpr form) (eq 'retbrk (car form)))
				   (cond ((numberp (setq form
							 (eval (cadr form))))
					  (return form))
					 ( t (return (1- break-level-count)))))
				  ( t (pearlbreakprintfn (eval form))
				      (terpr)))))))
		(return (cond ((eq rettype 'localcall) 
			       (car retval))
			      ( t retval))))
	       ((<& retval break-level-count)
		(setq tpl-errlist errlist)
		(*throw 'break-catch retval))
	       ( t (terpr)))))
    nil
    (1+ break-level-count)
    nil
    nil
    nil
    nil)))

(aliasdef 'break-err-handler 'pearl-break-err-handler)

; vi: set lisp:
