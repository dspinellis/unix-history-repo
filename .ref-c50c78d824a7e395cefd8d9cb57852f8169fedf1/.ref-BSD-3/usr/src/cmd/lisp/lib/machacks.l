; this file will be loaded whenever the -m switch is set for compilation.
; NOTE this file is loaded after the maclisp syntax has been set up!!
(sstatus dumpcore t)
(sstatus feature unix)
(setsyntax '/ 2)

(def macsyma-env		; put at the beginning of each macsyma file
  (macro (l) `(include |//usr//staff//jkf//mac//libmax//prelud.l|)))

(def franzify
  (macro (l) `(eval-when (compile eval)
			 (sstatus feature franz)
			 (sstatus feature unix)
			 (sstatus nofeature maclisp)
			 (sstatus nofeature its))))
(def error
  (lexpr (a)
	 (terpr)
	 (patom '|Error: |)
	 (do ((ll a (sub1 ll)))
	     ((zerop ll)(terpr))
	     (patom (arg ll)))))

(def fasload 
  (nlambda (argl)
	   (fasl (concat '|//usr//staff//jkf//mac//| 
			 (cadddr argl)		; fourth arg
			 '|//| 
			 (car argl)		; first arg
			 '|.|
			 (cadr argl)))))	; second arg

(def coutput
  (lambda (msg)
	  (print msg)	; should go to unfasl port
	  (terpr)))

(opval 'pagelimit 5000.)

(defmacro let (binding-forms &rest body)
	  `((lambda ,(mapcar '(lambda (x) (cond ((atom x) x) (t (car x))))
			     binding-forms)
		    ,@body)
	    ,@(mapcar '(lambda (x) (cond ((atom x) nil)
					 ((null (cdr x)) nil)
					 (t (cadr x))))
		      binding-forms)))

(defmacro let* (binding-forms &rest body)
    (construct-let* (reverse binding-forms) body))

(defun construct-let* (binding-forms body)
  (cond ((null binding-forms)
	 (cond ((= (length body) 1) (car body))
	       (t `(progn . ,body))))
	(t (construct-let*
	    (cdr binding-forms)
	    (cond
	     ;;(let* (a b) x) --> ((lambda (a) ((lambda (b) x) nil)) nil)
	     ((atom (car binding-forms))
	      `(((lambda (,(car binding-forms)) . ,body) nil)))
	     ;;(let* (((a . b) v) x)) -->
	     ;;  ((lambda (let*val) 
	     ;;	    ((lambda (a) (setq let*val (cdr let*val))
	     ;;		         ((lambda (b) x)
	     ;;			  let*val))
	     ;;	     (car let*val)))
	     ;;   v)
	     ((null (atom (caar binding-forms)))
	      `(((lambda (let*val) ,(constr-let*-hack (caar binding-forms)
						      body))
		 ,(cadar binding-forms))))
	     
	     ;;(let* ((a) (b)) x) --> ((lambda (a) ((lambda (b) x) nil)) nil)
	     ((null (cdar binding-forms))
	      `(((lambda (,(caar binding-forms)) . ,body) nil)))
	     ;;(let* ((a 3) (b 4)) x) --> ((lambda (a) ((lambda (b) x) 4)) 3)
	     (t `(((lambda (,(caar binding-forms)) . ,body)
		   ,(cadar binding-forms)))))))))

(defun constr-let*-hack (lst body)
  (cond ((atom lst) `((lambda (,lst) ,@body) let*val))
	((null (cdr lst))
	   `((lambda (,(car lst)) ,@body) (car let*val)))
	(t `((lambda (,(car lst)) (setq let*val (cdr let*val))
				  ,(constr-let*-hack (cdr lst)
						     body))
	     (car let*val)))))

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

(defun displace (old-form new-form)
       (cond ((atom old-form)
	      (error '|Not able to displace this form| old-form))
	     ((atom new-form)
	      (rplaca old-form 'progn)
	      (rplacd old-form (list new-form)))
	     (t (rplaca old-form (car new-form))
		(rplacd old-form (cdr new-form)))))

(def caseq
  (macro (form)
	   ((lambda (x)
		    `((lambda (,x)
			      (cond 
			       ,@(mapcar '(lambda (ff)
						  (cond ((eq (car ff) 't)
							 `(t ,(cadr ff)))
							(t `((eq ,x ',(car ff))
							     ,(cadr ff)))))
					 (cddr form))))
		      ,(cadr form)))
	    (gensym 'Z))))

