;; file of common cmu functions which should be macros 
;; I hope that by just loading in the file an environment will be
;; created which will permit the cmu files to be compiled.

(declare (macros t))

(eval-when (compile eval load)
   (or (boundp 'CMU-fcn-def) (setq CMU-fcn-def (getd 'def))))

;-- contents
;	dv	mark!changed 	***	list* [construct-list* lambda]
;	neq	push	pop	mukname (equivlance)
;	prin1 (equiv to print)	selectq	lineread
;

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
        (and (boundp '%changes) (setq %changes (cons atomname %changes)))
	atomname))

;--- *** :: comment macro
;
(defmacro *** (&rest x) nil)

;; this must be rewritten as a macro		****
;(def quote! (nlambda (a) (quote!-expr a)))

; this will be thrown away if the code below it works
(def quote!-expr
     (lambda 
      (x)
      (cond ((atom x) x)
            ((eq (car x) '!)
             (cons (eval (cadr x)) (quote!-expr (cddr x))))
            ((eq (car x) '!!)
             (cond ((cddr x)
                    (append (eval (cadr x)) (quote!-expr (cddr x))))
                   (t (eval (cadr x)))))
            (t
             (prog (u v)
                   (setq u (quote!-expr (car x)))
                   (setq v (quote!-expr (cdr x)))
                   (cond ((and (eq u (car x)) (eq v (cdr x))) (return x)))
                   (return (cons u v)))))))
;; this is probably what the above forms do. (jkf)
(defmacro quote! (&rest a) (quote!-expr-mac a))
(eval-when (compile eval load)
   
(defun quote!-expr-mac (form)
   (cond ((null form) nil)
	 ((atom form) `',form)
	 ((eq (car form) '!)
	  `(cons ,(cadr form) ,(quote!-expr-mac (cddr form))))
	 ((eq (car form) '!!)
	  (cond ((cddr form) `(append ,(cadr form)
				       ,(quote!-expr-mac (cddr form))))
		(t (cadr form))))
	 (t `(cons ,(quote!-expr-mac (car form))
		    ,(quote!-expr-mac (cdr form))))))

); end eval-when
		 
	 
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
  (cond (storeit-p `(setq ,storeit (prog1 (car ,stack)
					   (setq ,stack (cdr ,stack)))))
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



(defmacro de (name &rest body)
   (cond ((status feature complr) `(def ,name (lambda ,@body)))
	 (t `(progn (putd ,name '(lambda ,@body))
		    (mark!changed ',name)))))
(defmacro dn (name &rest body)
   (cond ((status feature complr) `(def ,name (nlambda ,@body)))
	 (t `(progn (putd ,name '(nlambda ,@body))
		    (mark!changed ',name)))))
(defmacro dm (name &rest body)
   (cond ((status feature complr) `(def ,name (macro ,@body)))
	 (t `(progn (putd ,name '(macro ,@body))
		    (mark!changed ',name)))))

(eval-when (compile eval load)
   (or (boundp 'OLD-fcn-def) (setq OLD-fcn-def (getd 'def))))

(defmacro def (&rest form)
    (cond ((status feature complr)
	   `(progn 'compile
		    (eval-when (compile) (putd 'def OLD-fcn-def))
		    (def ,@form)
		    (eval-when (compile) (putd 'def CMU-fcn-def))))
	  (t `(progn (putd ',(car form) ',(cadr form))
		    (mark!changed ',(car form))))))

(eval-when (compile eval load)
   (or (boundp 'CMU-fcn-def) (setq CMU-fcn-def (getd 'def))))

;--iteration macros

(def Cdo (macro (l) (expand-do l)))

(def exists (macro (l) (expand-ex 'some l)))

(declare (special var))

(eval-when (compile eval load)
   
(def expand-ex
     (lambda 
      (fn form)
      (quote! !
              fn
              !
              (caddr form)
              (function
               (lambda 
                !
                (cond ((atom (cadr form)) (ncons (cadr form)))
                      (t (cadr form)))
                !
                (car (setq form (cdddr form)))))
              !
              (cond ((cdr form) (list 'function (cadr form)))))))
) ; end eval-when

(def expand-do
     (lambda 
      (l)
      (prog (label var init incr limit part)
            (cond
             ((setq part (memq 'for l))
              (setq var (cadr part))
              (setq l (append (ldiff l part) (cddr part)))))
            (cond
             ((setq part (exists w l (memq w '(gets = _ :=))))
              (setq init (cadr part))
              (setq l (append (ldiff l part) (cddr part)))))
            (cond
             ((setq part (exists w l (memq w '(step by))))
              (setq incr (cadr part))
              (setq l (append (ldiff l part) (cddr part)))))
            (cond
             ((setq part (memq 'to l))
              (setq limit (cadr part))
              (setq l (append (ldiff l part) (cddr part)))))
            (return
             (quote! prog
                     !
                     (cond (var (ncons var)))
                     !!
                     (cond
                      (var
                       (ncons
                        (list 'setq var (cond (init) (t 1))))))
                     !
                     (setq label (gensym))
                     !!
                     (mapcan (function
                              (lambda 
                               (exp)
                               (cond ((eq part 'while)
                                      (setq part nil)
                                      (quote!
                                       (cond
                                        ((not ! exp) (return nil)))))
                                     ((eq part 'until)
                                      (setq part nil)
                                      (quote!
                                       (cond (! exp (return nil)))))
                                     ((memq (setq part exp)
                                            '(while until do Cdo))
                                      nil)
                                     (t (ncons exp)))))
                             l)
                     !!
                     (cond
                      (var
                       (quote!
                        (setq ! var (+ ! var ! (cond (incr) (t 1)))))))
                     !!
                     (cond
                      ((and var limit)
                       (quote! (cond ((> ! var ! limit) (return nil))))))
                     (go ! label))))))


(def expand-fe
     (lambda 
      (form)
      (prog (vars body)
            (return
             (cons (cond ((memq (cadr form)
                                (quote
                                 (map mapc
                                      mapcan
                                      mapcar
                                      mapcon
                                      mapconc
                                      maplist)))
                          (setq form (cdr form))
                          (car form))
                         (t 'mapc))
                   (progn (setq vars (cadr form))
                          (cond ((atom vars) (setq vars (list vars))))
                          (cons (cons 'function
                                      (ncons
                                       (cons 'lambda
                                             (cons vars
                                                   (setq body
                                                         (Cnth (cdddr
                                                                form)
                                                               (length
                                                                vars)))))))
                                (ldiff (cddr form) body))))))))
(def expand-set-of
     (lambda 
      (form)
      (prog (vars body)
            (setq vars (cadr form))
            (cond ((atom vars) (setq vars (list vars))))
            (setq form (cddr form))
            (return
             (quote! mapcan
                     (function
                      (lambda 
                       !
                       vars
                       (cond
                        (! (car
                            (setq body (Cnth (cdr form) (length vars))))
                           (list ! (car vars))))))
                     !!
                     (ldiff form body))))))

(dv filelst nil)

(def for (macro (l) (expand-do l)))

(def for-each (macro (l) (expand-fe l)))

(def forall (macro (l) (expand-ex 'every l)))

(def set-of (macro (l) (expand-set-of l)))

(def ty (macro (f) (append '(exec cat) (cdr f))))

(def until (macro (l) (expand-do l)))

(def while (macro (l) (expand-do l)))

(putprop 'cmumacs t 'version)
