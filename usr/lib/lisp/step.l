(setq rcs-step-
   "$Header: /usr/lib/lisp/step.l,v 1.1 83/01/29 18:39:46 jkf Exp $")

; vi: set lisp :

;;;                     LISP Stepping Package
;;;
;;; Adapted by Mitch Marcus for Franz Lisp from Chuck Rich's MACLISP
;;; package.
;;;
;;;
;;; Adapted 2/80 from the Maclisp version of 11/03/76
;;; Further modified 5/80 by Don Cohen (DNC)
;;;
;;; modified by jkf 6/81 to handle funcallhook.
;;;
;;; User Interface Function
;;;
;;;             Valid Forms:                            
;;; (step) or (step nil)	:: turn off stepping
;;; (step t) 			:: turn on stepping right away.
;;; (step e)			:: turn on stepping of eval only
;;; (step foo1 foo2 ...)  	:: turn on stepping when one of fooi is
;;;				:: called
;;;
;------ implementation:
; evalhook* is nil meaning no stepping, or t meaning always step
;    or is a list of forms which will start continuous stepping.
;
; The hook functions are evalhook* and funcallhook*.
;

(declare (special 
	  evalhook-switch piport 	
	  hookautolfcount funcallhook
	  evalhook evalhook* |evalhook#| prinlevel prinlength
  	  fcn-evalhook fcn-funcallhook
	  Standard-Input)
         (macros nil))

;; First Some Macros

(defun 7bit macro (s)
       ;; (7BIT n c) tests if n is ascii for c
       (list '= (list 'boole 1 127. (cadr s)) (caddr s)))

;--- print* 
; indent based on current evalhook recursion level then print the
; arg in form
;
(defun print* macro (s)
          ;; print with indentation
         '(do ((i 1 (1+ i))
               (indent (* 2 |evalhook#|))
               (prinlevel 3)
               (prinlength 5))
              ((> i indent) 
	       (cond ((eq type 'funcall) (patom "f:")))
	       (print form))
              (tyo 32.)))

(defun step fexpr (arg)
  (cond ((or (null arg) (car arg))
	 (setq evalhook-switch t) ; for fixit package
	 (setq |evalhook#| 0.)                  ;initialize depth count
	  (setq hookautolfcount 0)		; count if auto lfs at break
	 (setq evalhook nil)                  ;for safety
	 (setq funcallhook nil)
	 ; (step e) means just step eval things, else step eval and funcal
	 (cond ((eq (car arg) 'e) 
		(setq fcn-evalhook 'evalhook* fcn-funcallhook nil))
	       (t (setq fcn-evalhook 'evalhook* fcn-funcallhook 'funcallhook*)))
	 (setq evalhook*
	       (cond ((null arg) nil)
		     ((or (eq (car arg) t) (eq (car arg) 'e)))
		     (arg)))
	 (setq evalhook fcn-evalhook)      ;turn system hook to my function
	 (setq funcallhook fcn-funcallhook)
	 (sstatus translink nil)
	 (*rset t)                            ;must be on for hook to work
	 (sstatus evalhook t))           ;arm it
	(t (setq evalhook* nil)
	   (setq evalhook nil)
	   (setq hookautolfcount 0)		; count if auto lfs at break
	   (setq evalhook-switch nil)
	   (sstatus evalhook nil))))


;---- funcall-evalhook*
;
; common function to handle evalhook's and funcallhook's.
; the form to be evaluated is given as form and the type (eval or funcall)
; is given as type.
;

(defun funcall-evalhook* (form type)
  (cond (evalhook*
	 ;; see if selective feature kicks in here
	 (and (not (atom form))
	      (not (eq evalhook* t))
	      (memq (car form) evalhook*)
	      (setq evalhook* t))	; yes, begin stepping always

	 (cond ((eq evalhook* t)
		;; print out form before evaluation
		(print*)

		(cond ((atom form)
		       ;; since form is atom, we just eval it and print
		       ;; out its value, no need to ask user what to do
		       (cond ((not (or (numberp form)(null form)(eq form t)))
			      (princ '" = ")
			      ((lambda (prinlevel prinlength)
				       (setq form (evalhook form nil nil))
				       (print form))	
			       3 5)))
		       (terpri))
		      (t ; s-expression
			 (prog (cmd ehookfn fhookfcn)

			   cmdlp  (cond ((greaterp hookautolfcount 0)
					 (setq hookautolfcount (sub1 hookautolfcount))
					 (terpr)
					 (setq cmd #\lf))
					(t (setq cmd (let ((piport 
							    Standard-Input))
							  (drain piport)
							  (tyi piport)))))

			       ;; uppercase alphabetics
			       ;; dispatch on command character
			       (cond ((eq cmd #\lf)    
				      ; \n so continue
				      (setq ehookfn fcn-evalhook
					    fhookfcn fcn-funcallhook))

				     ((memq cmd '(#/p #/P))
				      ; "P" print in full
				      (print form)
				      (go cmdlp))

				     ; "G"
				     ((memq cmd '(#/g #/G))
				      (setq evalhook* nil  ;stop everything
					    ehookfn nil
					    fhookfcn nil))

				     ((memq cmd '(#/c #/C))
				      ;"C" no deeper
				      (setq ehookfn nil
					    fhookfcn nil))

				     ((memq cmd '(#/d #/D))
				      ;"D" call debug
				      (setq evalhook-switch nil)
				      (sstatus evalhook nil)
				      (debug)
				      (setq evalhook-switch t)
				      (sstatus evalhook t)
				      (go cmdlp))


				     ((memq cmd '(#/b #/B))
				      ; "B" give breakpoint
				      (break step)
				      (print*)
				      (go cmdlp))

				     ((memq cmd '(#/q #/Q))
				      ; "Q" stop stepping
				      (step nil)
				      (reset))

				     ((memq cmd '(#/n #/N))
				      (setq hookautolfcount 
					    (let ((piport Standard-Input))
						 (read)))
				      (cond ((not (numberp hookautolfcount))
					     (patom "arg to n should be number")
					     (terpr)
					     (setq hookautolfcount 0))))

				     ; "s" eval form
				     ((memq cmd '(#/s #/S))
				      (let ((piport Standard-Input)
					    (fcns nil))
					   (setq fcns (read))
					   (cond ((dtpr fcns) 
						  (setq evalhook* fcns))
						 ((symbolp fcns)
						  (setq evalhook* (list fcns))))))

				     ; "e" step eval only
				     ((memq cmd '(#/e #/E))
				      (setq fcn-funcallhook nil))

				     ; "?", "H" show the options
				     ((memq cmd '(72 104 63.))
				      #+cmu (ty /usr/lisp/doc/step\.ref)
				      #-cmu(stephelpform)
				      (terpri)
				      (go cmdlp))
				     ((eq cmd #\eof)
				      (patom "EOF typed")
				      (terpr))

				     (t (princ '"Try one of ?BCDGMPQ or <cr>")
					(go cmdlp)))

			       ;; evaluate form
			       (clear-input-buffer)
			       ((lambda (|evalhook#|)
					(setq form (continue-evaluation 
						    form 
						    type 
						    ehookfn 
						    fhookfcn)))
				(1+ |evalhook#|))

			       ;; print out evaluated form
			       (cond ((and evalhook* 
					   (or (eq type 'funcall)
					       (not (zerop |evalhook#|))))
				      (let ((type nil))
					   (print*))
				      (terpri)
				      )))))
		;;return evaluated form
		form)
	       (t ;  why was this here? (clear-input-buffer)
		  (continue-evaluation form type fcn-evalhook fcn-funcallhook))))   
	(t ;  why was this here? (clear-input-buffer)
	   (continue-evaluation form type fcn-evalhook fcn-funcallhook))))

;--- stephelpform 
;
; print a summary of the functions of step
;
(defun stephelpform nil
  (patom "<cr> - single step;  n <number> - step <number> times")(terpr)
  (patom "b - break;  q - quit stepping;  d - call debug;") (terpri)
  (patom "c - turn off step for deeper levels; e - stop at eval forms only")
  (terpri)
  (patom "h,? - print this") (terpr))

;--- funcallhook* 
;
; automatically called when a funcall is done and funcallhook*'s 
; value is the name of this function (funcallhook*).  When this is
; called, a function with n-1 args is being funcalled, the args
; to the function are (arg 1) through (arg (sub1 n)), the name of
; the function is (arg n)
;
(defun funcallhook* n
  (let ((name (arg n))
	(args (listify (sub1 n))))
       (funcall-evalhook* (cons name args) 'funcall)))

;--- evalhook* 
;
; called whenever an eval is done and evalhook*'s value is the 
; name of this function (evalhook*).  arg is the thing being
; evaluated.
;
(defun evalhook* (arg)
  (funcall-evalhook* arg 'eval))

(defun continue-evaluation (form type evalhookfcn funcallhookfcn)
  (cond ((eq type 'eval) (evalhook form evalhookfcn funcallhookfcn))
	(t (funcallhook form funcallhookfcn evalhookfcn))))


(or (boundp 'prinlength) (setq prinlength nil))

(or (boundp 'prinlevel) (setq prinlevel nil))

; Standard-Input is a variable bound to the initial stdin port. It is 
; bound in the auxfns0 package, but older lisps may not have that new
; package, so in case they don't we approximate Standard-Input with nil
; which works in many cases, but drain's do not work.
(or (boundp 'Standard-Input) (setq Standard-Input nil))
(defun clear-input-buffer nil (drain Standard-Input))
