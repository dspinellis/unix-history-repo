;;;;;;;;;;;;;;;;;;;;;;;;;;;;; history.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for adding a command to the command history, printing
;    the command history, processing aliased atoms and handling
;    the history-invoking splice macros ! and $.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Given two lists of atoms, determine if the first is a prefix of the second.
(de prefix (item1 item2)
  (prog ()
	prefixloop
	(cond ((null item1) (return t))      ; item1 ran out first: succeed.
	      ((null item2) (return nil))    ; item2 ran out first: fail.
	      ((neq (car item1) (car item2)) ; no match: fail.
	       (return nil))
	      ; Otherwise, try next atoms.
	      ( t (setq item1 (cdr item1))
		  (setq item2 (cdr item2))
		  (go prefixloop)))))

; Add the LINE to the *history* hunk in the *historynumber* spot,
;   after possibly replacing it with its alias (for atoms) saved
;   on the property list under the ALIAS property.
(de addhistory (line)
  (let (alias)
       ; Replace with alias if there is one.
       (and *usealiases*
	    (litatom line)
	    (setq alias (get line 'alias))
	    (setq line alias))
       
       ; Store in the command history table.
       (setq *historynumber* (1+ *historynumber*))
       (rplacx (\\ *historynumber* *historysize*)
	       *history*
	       (copy line))	; To eliminate macroexpansions.
       
       ; If it has been changed by read macros, print it out again.
       (cond (*readlinechanged*
	      (pearlprintfn line)
	      (terpri)))
       line))
 
; Print the command history.   Optional argument determines how
;   many commands get printed, otherwise, the whole history.
(de history narg
  (cond ((\=& 0 narg)
	 (cond ((ge *historynumber* *historysize*)
	        (for command (1+ (- *historynumber* *historysize*))
		     *historynumber*
		     (msg t command  ": "
			  (cxr (\\ command *historysize*)
			       *history*))))
	       ( t (for command 0 *historynumber*
			(msg t command ": " (cxr command *history*))))))
	( t 
	 (cond ((ge *historynumber* (arg 1))
	        (for command (1+ (- *historynumber* (arg 1)))
		     *historynumber*
		     (msg t command  ": "
			  (cxr (\\ command *historysize*)
			       *history*))))
	       ( t (for command 0 *historynumber*
			(msg t command ": " (cxr command *history*)))))))
  '*invisible*)
 
; Look for a command with the next atom as a prefix and return the command.
(de prefixcommandhistory ()
  (let* ((wanted (read))
	 (wanthead (explode wanted))
	 (commandnum *historynumber*)
	 (stoppingcommand (cond ((ge *historynumber* *historysize*)
				 (- *historynumber* *historysize*))
				( t -1.)))
	 commandhead)
	(setq *readlinechanged* t)
	(while (and (>& commandnum stoppingcommand)
		    (not (prefix wanthead
				 (prog2 (setq commandhead
					      (cxr (\\ commandnum
							      *historysize*)
						   *history*))
					(setq commandhead
					      (explode
					       (cond ((atom commandhead)
						      commandhead)
						     ( t (car commandhead)))))
					))))
	       (setq commandnum (1- commandnum)))
	
	(cond ((>& commandnum stoppingcommand)
	       (ncons (cxr (\\ commandnum *historysize*)
			   *history*)))
	      ( t (ncons (concat '\! wanted))))))
 
; History command invoker.
(dsm \!
  (lambda ()
	  (let
	   (num whole)
	   (selectq (tyipeek)
		    (33. (readc)				; !!
			 (setq *readlinechanged* t)
			 (ncons (cxr (\\ *historynumber* *historysize*)
				     *history*)))
		    (58. (readc) (setq num (read))		; !:
			 (setq *readlinechanged* t)
			 (setq whole (cxr (\\ *historynumber* *historysize*)
					  *history*))
			 (cond ((atom whole) (ncons whole))
			       (  t	  (ncons (nth num whole)))))
		    (94. (readc)				; !^
			 (setq *readlinechanged* t)
			 (setq whole (cxr (\\ *historynumber* *historysize*)
					  *history*))
			 (cond ((atom whole) (ncons whole))
			       (  t	  (ncons (cadr whole)))))
		    (42. (readc)				; !*
			 (setq *readlinechanged* t)
			 (setq whole (cxr (\\ *historynumber* *historysize*)
					  *history*))
			 (cond ((atom whole) (ncons whole))
			       (  t	  (cdr whole))))
		    (36. (readc)				; !$
			 (setq *readlinechanged* t)
			 (setq whole (cxr (\\ *historynumber* *historysize*)
					  *history*))
			 (cond ((atom whole) (ncons whole))
			       (  t	  (ncons (last whole)))))
		    (9.  (ncons '\!))				; !Tab
		    (10. (ncons '\!))				; !LF
		    (13. (ncons '\!))				; !CR
		    (32. (ncons '\!))				; !Blank
		    (41. (ncons '\!))				; !rpar
		    ((48. 49. 50. 51. 52. 53. 54. 55. 56. 57.)	; !Number
		     (setq *readlinechanged* t)
		     (setq num (read))
		     (ncons (cxr (\\ num *historysize*)
				 *history*)))
		    (otherwise (prefixcommandhistory)))		; !Prefix
	   )))
 
; Look for a command with the next atom as a prefix and return its value.
(de prefixcommandvalue ()
  (let* ((wanted (read))
	 (wanthead (explode wanted))
	 (commandnum *historynumber*)
	 (stoppingcommand (cond ((ge *historynumber* *historysize*)
				 (- *historynumber* *historysize*))
				( t -1.)))
	 commandhead)
	(setq *readlinechanged* t)
	(while (and (>& commandnum stoppingcommand)
		    (not (prefix wanthead
				 (prog2 (setq commandhead
					      (cxr (\\ commandnum
							      *historysize*)
						   *histval*))
					(setq commandhead
					      (explode
					       (cond ((atom commandhead)
						      commandhead)
						     ( t (car commandhead)))))
					))))
	       (setq commandnum (1- commandnum)))
	
	(cond ((>& commandnum stoppingcommand)
	       (cxr (\\ commandnum *historysize*)
		    *histval*))
	      ( t (concat '\$ wanted)))))
 
; History command result invoker.
(dsm \$
  (lambda ()
	  (let
	   (num whole)
	   (ncons
	    (selectq (tyipeek)
		     (36. (readc)				; $$
			  (setq *readlinechanged* t)
			  (list 'quote
				(cxr (\\ *historynumber* *historysize*)
				     *histval*)))
		     (9.  '\$)					; $Tab
		     (10. '\$)					; $LF
		     (13. '\$)					; $CR
		     (32. '\$)					; $Blank
		     (41. '\$)					; !rpar
		     ((48. 49. 50. 51. 52. 53. 54. 55. 56. 57.)	; $Number
		      (setq *readlinechanged* t)
		      (setq num (read))
		      (list 'quote (cxr (\\ num *historysize*)
					*histval*)))
		     (otherwise ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; $Prefix
				(list 'quote (prefixcommandvalue))))))))

; vi: set lisp:
