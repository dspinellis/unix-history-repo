;;;                     LISP Stepping Package
;;;
;;; Adapted by Mitch Marcus for Franz Lisp from Chuck Rich's MACLISP
;;; package.
;;;
;;;
;;; Adapted 2/80 from the Maclisp version of 11/03/76
;;; Further modified 5/80 by Don Cohen (DNC)
;;;
;;; User Interface Function
;;;
;;;             Valid Forms:                            set EVALHOOK*
;;;                             (STEP)                  NIL
;;;                             (STEP T)                T
;;;                             (STEP NIL)              NIL
;;;                             (STEP FOO1 FOO2 ...)    (FOO1 FOO2)
;;;

(declare (special 
	  evalhook-switch piport 	; LWE 1/11/81 So the compiler 
					; doesn't have to.
	  evalhook evalhook* |evalhook#| prinlevel prinlength)
         (macros nil))

;; First Some Macros

(defun 7bit macro (s)
       ;; (7BIT n c) tests if n is ascii for c
       (list '= (list 'boole 1 127. (cadr s)) (caddr s)))

(defun print* macro (s)
          ;; print with indentation
         '(do ((i 1 (1+ i))
               (indent (* 2 |evalhook#|))
               (prinlevel 3)
               (prinlength 5))
              ((> i indent) ($prin1 form))
              (tyo 32.)))

(defun step fexpr (arg)
  (cond ((or (null arg) (car arg))
	 (*rset t)                            ;must be on for hook to work
	 (setq evalhook-switch t) ; for fixit package
	 (setq |evalhook#| 0.)                  ;initialize depth count
	 (setq evalhook nil)                  ;for safety
	 (setq evalhook*
	       (cond ((null arg) nil)
		     ((eq (car arg) t))
		     (arg)))
	 (setq evalhook 'evalhook*)      ;turn system hook to my function
	 (sstatus evalhook t))           ;arm it
	(t (setq evalhook* nil)
	   (setq evalhook nil)
	   (setq evalhook-switch nil)
	   (sstatus evalhook nil))))

;;;
;;; LISP evaluator comes here whenever EVALHOOK is Non-NIL and points here
;;; It expects me to do the evaluation and return the value.
;;;

(defun evalhook* (form)
       ;; returns evaluation of form
       (cond (evalhook*
              ;; see if selective feature kicks in here
              (and (not (atom form))
                   (not (eq evalhook* t))
                   (memq (car form) evalhook*)
                   (setq evalhook* t))
              (cond ((eq evalhook* t)
		     (and (= |evalhook#| 0) (drain piport))
                     ;; print out form before evaluation
                     (print*)
                     (cond ((atom form)
                            (cond ((not (or (numberp form)(null form)(eq form t)))
                                   (princ '" = ")
                                   ((lambda (prinlevel prinlength)
                                            (setq form (evalhook form nil))
					    ($prin1 form))	
                                    3 5))
				  (t ))
			    (terpri))
                           (t ; s-expression
                            (prog (cmd hookfn)
                        cmdlp  (setq cmd (tyi piport))
;; uppercase alphabetics
;; deleted  (-DNC)             (cond (((lambda (n)(and (> n 64.)(< n 91.)))
;; this is ridiculous                  (boole 2. 32. (boole 1 127. cmd)))
;;                                    (setq cmd (boole 2 32. cmd))))
;; dispatch on command character
                               (cond ((eq cmd 10.)    
; \n so continue
                                      (cond (nil  ;MUST ADAPT OR ELIMINATE
					     (and (not (atom form))
                                                  (eq (car (getl (car form)
'(expr fexpr lexpr subr fsubr lsubr macro)))
                                                 'macro))
                                             ;; do macro expansion
                                             (setq form (funcall (get (car form) 'macro)
                                                                 form))
                                             (print*)
                                             (go cmdlp))
                                            (t (setq hookfn 'evalhook*))))
                                     ((memq cmd '(80. 112.))
; "P" print in full
				      (drain piport)
				      (print form)
                                      (go cmdlp))
; "G"
                                     ((memq cmd '(71. 103.)) 
				      (drain piport)
                                      (setq evalhook* nil  ;stop everything
                                            hookfn nil))
                                     ((memq cmd '(67. 99.))
;"C" no deeper
				      (drain piport)
                                      (setq hookfn nil))
				     ((memq cmd '(68. 100.))
;"D" call debug
				      (setq evalhook-switch nil)
				      (sstatus evalhook nil)
				      (debug)
				      (setq evalhook-switch t)
				      (sstatus evalhook t)
				      (go cmdlp))
                                     ((memq cmd '(77. 109.))
; "M" continue including macro expansion
				      (drain piport)
                                      (setq hookfn 'evalhook*))
                                     ((memq cmd '(66. 98.))
; "B" give breakpoint
				      (drain piport)
                                      (break step)
                                      (print*)
                                      (go cmdlp))
				     ((memq cmd '(81. 113.))
; "Q" stop stepping
				      (step nil)
				      (reset))
; "?", "H" show the options
				     ((memq cmd '(72 104 63.))
				      (ty /usr/lisp/doc/step\.ref)
				      (drain piport)
				      (go cmdlp))
                                     (t (princ '"Try one of ?BCDGMPQ or <cr>")
					(drain piport)
					(go cmdlp)))
                               ;; evaluate form
                               ((lambda (|evalhook#|)
                                        (setq form (evalhook form hookfn)))
                                (1+ |evalhook#|))
                               ;; print out evaluated form
                               (cond ((and evalhook* (not (zerop |evalhook#|)))
				      (print*) (terpri)
				      )))))
                     ;;return evaluated form
                     form)
                    (t (evalhook form 'evalhook*))))   ; keep looking
             (t (evalhook form 'evalhook*))))          ; skip out quick

(defun $prin1 (arg) (print (trim arg (cond (prinlevel) (t -1)) 
				     (cond (prinlength) (t -1)))))
;;new $prin1 - DNC (trim replaces $prinutil)

(defun trim (arg depth length)
    (cond ((not (dtpr arg)) arg)	; should worry about hunks
	  ((zerop depth) '&)
	  ((zerop length) '(<etc>))
	  (t (cons (trim (car arg) (sub1 depth) length)
		   (trim (cdr arg) depth (sub1 length))))))

;(defun $prinutil (arg pdepth)
;  (cond ((not (dtpr arg))	; there really ought to be code for hunks
;	 (princ arg))
;	((eq (car arg) 'quote)
;	 (princ '"'")
;	 ($prinutil (cadr arg) pdepth))
;	(t (princ '"(")
;	   (do ((rest arg (cdr rest)) (plngth 0 (1+ plngth)))
;	       ((null rest) (princ '|)|))
;	       (cond ((= plngth prinlength) (princ '|...)|) (return))
;					;;; code for (a b .c) added - DNC
;		     ((and (cdr rest) (atom (cdr rest))) 
;		      ($prinutil (car rest) (1+ pdepth))
;		      (princ '| . |) (princ (cdr rest))	(return))
;		     ((= pdepth prinlevel) (princ '&))
;		     (t ($prinutil (car rest) (1+ pdepth))))
;	       (cond ((cdr rest) (princ '| |)))))))

(or (boundp 'prinlength) (setq prinlength nil))

;;; - DNC prin1 is reserved for other things in CMULisp
;;;   we just use $prin1 instead
;;;(or (boundp 'prin1) (setq prin1 nil))

(or (boundp 'prinlevel) (setq prinlevel nil))
