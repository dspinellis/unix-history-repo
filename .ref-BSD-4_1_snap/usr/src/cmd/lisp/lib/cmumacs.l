;; file of common cmu functions which should be macros 
;; I hope that by just loading in the file an environment will be
;; created which will permit the cmu files to be compiled.
;;; This is a bootstrap file to allow the true defns to be read in.

(declare (macros t))


;--- dv :: set variable to value and remember it was changed
; (dv name value)   name is setq'ed to value (no evaluation) and
;		 the fact that it was done is remembered
;
(defmacro dv (name value)
  `(progn 'compile
	  (setq ,name ',value)
	  (mark!changed ',name)))

(defmacro mark!changed (name)
  `(let ((atomname ,name))
        (and (boundp '%changes) (setq %changes (cons form %changes)) name)
	atomname))

;--- *** :: comment macro
;
(defmacro *** (&rest x) nil)

;--- the following are macroizations from cmu3.l

;(jkf)- ucb list* macro.
;
(defmacro list* (&rest forms)
	  (cond ((null forms) nil)
		((null (cdr forms)) (car forms))
		(t (construct-list* forms))))

(defun construct-list* (forms)
       (setq forms (reverse forms))
       (do ((forms (cddr forms) (cdr forms))
	    (return-form `(cons ,(cadr forms) ,(car forms))
			 `(cons ,(car forms) ,return-form)))
	   ((null forms) return-form))) 

(defmacro neq (a b) `(not (eq ,a ,b)))


(defmacro push (value stack) `(setq ,stack (cons ,value ,stack)))


(defmacro pop (stack &optional (storeit nil storeit-p))
  (cond ((storeit-p `(setq ,storeit (prog1 (car ,stack)
					   (setq ,stack (cdr ,stack))))))
	(t `(prog1 (car ,stack) (setq ,stack (cdr ,stack))))))



;(jkf) this is actually maknum is the maclisp terminology
(putd 'munknam (getd 'maknum))

; added for CMULisp compatibilty (used by editor etc)
(putd 'prin1 (getd 'print))

;--- selectq :: case statement type construct
;
;   (selectq <form>
;	     (<tag1> <expr1> ...)
;	     (<tag2> <expr2> ...)
;		 ...
;	     (<tagn> <exprn> ...)
;	      (<exprfinal> ...))
; <form> is evaluated and then compared with the tagi, if it matches
; the expri are evaluated.  If it doesn't match, then <exprfinal> are
; evaluated.
;
(def selectq
  (macro (form)
	 ((lambda (x)
		  `((lambda (,x)
			    (cond 
			     ,@(maplist 
				'(lambda (ff)
					 (cond ((null (cdr ff))
						`(t  ,(car ff)))
					       ((atom (caar ff))
						`((eq ,x ',(caar ff))
						  . ,(cdar ff)))
					       (t
						`((memq ,x ',(caar ff))
						  . ,(cdar ff)))))
				       (cddr form))))
		    ,(cadr form)))
	  (gensym 'Z))))

(defmacro lineread (&optional (x nil)) 
  `(%lineread ,x))

(putprop 'cmumacs t 'version)
