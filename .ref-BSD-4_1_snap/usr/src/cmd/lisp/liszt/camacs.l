
;----------- macros for the compiler -------------


(setq sectioncamacsid "@(#)camacs.l	5.4	5/13/81")  ; id for SCCS

; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  
; author: j. foderaro

(declare (macros t))			; compile and save macros

;--- comp-err
;    comp-warn
;    comp-note
;    comp-gerr
; these are the compiler message producing macros.  The form is
; (comp-xxxx val1 val2 val3 ... valn) , all values are printed according
;  to this scheme. If vali is an atom, it is patomed, if vali is a
;  list, it is evaluated and printed. If vali is N a newline is printed
; 
; furthermore
;    the name of the current function is printed first
;    after comp-err prints the message, it does a throw to Comp-err .
;    errors are preceeded by Error: 
;	warnings by %Warning: and
;	notes by %Note:
;     The message is sent to the message file
;
(def comp-err
  (macro (l)
	 `(progn (comp-msg "?Error: " v-ifile ": " g-fname ": " 
			   ,@(cdr l) )
		 (setq er-fatal (1+ er-fatal))
		 (throw nil Comp-error))))

(def comp-warn
  (macro (l)
	 `(progn (cond (fl-warn 
			(comp-msg "%Warning: " v-ifile ": "  g-fname ": " 
					   ,@(cdr l)))))))

(def comp-note
  (macro (l)
	 `(progn (cond (fl-verb
			(comp-msg "%Note: " v-ifile ": "  ,@(cdr l)))))))

(def comp-gerr
  (macro (l)
	 `(progn (comp-msg
			"?Error: " v-ifile ": " g-fname ": ",@(cdr l))
		 (setq er-fatal (1+ er-fatal)))))

;--- comp-msg - port
;	      - lst
;  prints the lst to the given port.  The lst is printed in the manner
; described above, that is atoms are patomed, and lists are evaluated
; and printed, and N prints a newline.   The output is always drained.
;
(def comp-msg
  (macro (lis)
	 (do ((xx (cdr lis) (cdr xx))
	      (res nil))
	     ((null xx) 
	      `(progn ,@(nreverse (cons '(terpri) res))))
	     (setq res 
		   (cons (cond ((atom (car xx))
				(cond ((eq (car xx) 'N) '(terpr))
				      ((stringp (car xx)) `(patom ,(car xx)))
				      (t `(niceprint ,(car xx)))))
			       (t `(niceprint ,(car xx))))
			 res)))))
(def niceprint
  (macro (l)
	 `((lambda (float-format) (patom ,(cadr l))) "%.2f")))

;--- super if macro
(defun If macro  (lis) 
       (prog (majlis minlis revl)
	     (do ((revl (reverse lis) (cdr revl)))
		 ((null revl))
		 (cond ((eq (car revl) 'else)
			(setq majlis `((t ,@minlis) ,@majlis)
			      minlis nil))
		       ((or (eq (car revl) 'then) (eq (car revl) 'thenret))
			(setq revl (cdr revl)
			      majlis `((,(car revl) ,@minlis) ,@majlis)
			      minlis nil))
		       ((eq (car revl) 'elseif))
		       ((eq (car revl) 'If)
			(setq majlis `(cond ,@majlis)))
		       (t (setq minlis `( ,(car revl) ,@minlis)))))
	     ; we displace the previous macro, that is we actually replace
	     ; the if list structure with the corresponding cond, meaning
	     ; that the expansion is done only once
	     (rplaca  lis (car majlis))
	     (rplacd lis (cdr majlis))
	     (return majlis)))

;--- standard push macro
; (Push stackname valuetoadd)

(defmacro Push (atm val)
  `(setq ,atm (cons ,val ,atm)))

;--- pop macro

(defmacro Pop (val)
  `(prog1 (car ,val) (setq ,val (cdr ,val))))

;--- unpush macro - like pop except top value is thrown away
(defmacro unpush (atm)
  `(setq ,atm (cdr ,atm)))

;--- and an increment macro

(defmacro incr (atm)
  `(setq ,atm (1+ ,atm)))

(defmacro decr (atm)
  `(setq ,atm (1- ,atm)))
;--- add a comment

(defmacro makecomment (arg)
  `(cond (fl-comments (setq g-comments (cons ,arg g-comments)))))

;--- add a comment irregardless of the fl-comments flag
(defmacro forcecomment (arg)
  `(setq g-comments (cons ,arg g-comments)))

;--- write to the .s file

(defmacro sfilewrite (arg)
  `(patom ,arg vp-sfile))


(putprop 'camacs t 'version)	; flag that this file has been loaded
