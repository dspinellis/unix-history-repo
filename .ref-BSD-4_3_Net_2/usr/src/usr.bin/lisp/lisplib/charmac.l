;;
;; charmac.l				-[Sat Jan 29 18:13:40 1983 by jkf]-
;;
;; character macros
;; this contains the definition of the backquote and sharpsign
;; character macros.  [the backquote macro also defines the comma macro]
;;

(setq rcs-charmac-
   "$Header: /usr/lib/lisp/charmac.l,v 1.1 83/01/29 18:33:29 jkf Exp $")


(declare (macros t))

(setq **backquote** 1)

(declare (special **backquote**  Backquote-comma Backquote-comma-at
		  Backquote-comma-dot))

(setq Backquote-comma (gensym)
   Backquote-comma-at (gensym)
   Backquote-comma-dot (gensym))

(def back-quote-ch-macro 
  (lambda nil 
	  (back=quotify  ((lambda (**backquote**) (read)) 
			  (1+ **backquote**)))))

(def back-quote-comma-macro
 (lambda nil
  ((lambda (**backquote**)
	   (cond ((zerop **backquote**)
		  (error "comma not inside a backquote."))
		 ((eq (tyipeek) 64)
		  (tyi)
		  (cons Backquote-comma-at (read)))
		 ((eq (tyipeek) 46)
		  (tyi)
		  (cons Backquote-comma-dot (read)))
		 (t (cons Backquote-comma (read)))))
   (1- **backquote**))))

(def back=quotify 
  (lambda (x)
	  ((lambda (a d aa ad dqp)
		   (cond ((atom x) (list 'quote x))
			 ((eq (car x) Backquote-comma) (cdr x))
			 ((or (atom (car x))
			      (not (or (eq (caar x) Backquote-comma-at)
				       (eq (caar x) Backquote-comma-dot))))
			  (setq a (back=quotify (car x)) d (back=quotify (cdr x))
				ad (atom d) aa (atom a)
				dqp (and (not ad) (eq (car d) 'quote)))
			  (cond ((and dqp (not (atom a)) (eq (car a) 'quote))
				 (list 'quote (cons (cadr a) (cadr d))))
				((and dqp (null (cadr d)))
				 (list 'list a))
				((and (not ad) (eq (car d) 'list))
				 (cons 'list (cons a (cdr d))))
				(t (list 'cons a d))))
			 ((eq (caar x) Backquote-comma-at)
			  (list 'append (cdar x) (back=quotify (cdr x))))
			 ((eq (caar x) Backquote-comma-dot)
			  (list 'nconc (cdar x)(back=quotify (cdr x))))
			 ))
	   nil nil nil nil nil)))


(setsyntax '\` 'macro 'back-quote-ch-macro)
(setsyntax '\, 'macro 'back-quote-comma-macro)


;------- sharpsign macro, used for conditional assembly

;#O <SEXP> or #o <SEXP> reads sexp with ibase bound to 8.
;#+<FEATURE> <SEXP> makes <SEXP> exist if (STATUS FEATURE <FEATURE>) is T
;#-<FEATURE> <SEXP> makes <SEXP> exist if (STATUS FEATURE <FEATURE>) is NIL
;#+(OR F1 F2 ...) <SEXP> makes <SEXP> exist of any one of F1,F2,... are in
;			 the (STATUS FEATURES) list.
;#+(AND F1 F2 ...) works similarly except all must be present in the list.
;#+(NOT <FEATURE>) is the same as #-<FEATURE>.
;#/CHAR returns the numerical character code of CHAR.
;#\SYMBOL gets the numerical character code of non-printing characters.
;#' is to FUNCTION as ' is to QUOTE.
;#.<SEXP> evaluates <SEXP> at read time and leaves the result.
;#,<SEXP> evaluates <SEXP> at load time.  Here it is the same as "#.".
;#t returns t, this means something in NIL, I am not sure what.


(declare (special sharpm-function-names franz-symbolic-character-names))
(setq sharpm-function-names nil)

(def new-sharp-sign-macro
   (lambda ()
      ((lambda (char entry)
	  (cond ((setq entry (assq char sharpm-function-names))
		 (funcall (cdr entry) char))
		(t (error "Unknown character after #:" (ascii char)))))
       (tyi) nil)))

(setsyntax '\# 'splicing 'new-sharp-sign-macro)

;--- defsharp  ::  define a sharp sign handler
; form is (defsharp key arglist body ...)
; where key is a number or a list of numbers (fixnum equivalents of chars)
; arglist is a list of one argument, which will be bound to the fixnum
; representation of the character typed.
; body is the function to be executed when #key is seen.  it should return
; either nil or (list x) where x is what will be spliced in.
;
(def defsharp
   (macro (arg)    ; arg is (defsharp number-or-list arglist function-body)
	  (prog (name)
	     (setq name (concat "Sharpm" (cond ((dtpr (cadr arg)) (caadr arg))
					       (t (cadr arg)))
				(gensym)))
	     (cond ((dtpr (cadr arg))
		    (return `(progn 'compile
				    ,@(mapcar
					 '(lambda (x)
					     (defsharp-expand x name))
					 (cadr arg))
				    (defun ,name ,(caddr arg) ,@(cdddr arg)))))
		   (t (return `(progn 'compile
				      ,(defsharp-expand (cadr arg) name)
				      (defun ,name ,(caddr arg) ,@(cdddr arg)))))))))

(eval-when (compile load eval)
   (defun defsharp-expand (code name)
	  (cond ((symbolp code) (setq code (car (aexploden code)))))
	  `((lambda (current)
	       (cond ((setq current (assq ,code sharpm-function-names))
		      (rplacd current ',name))
		     (t (setq sharpm-function-names
			      (cons '(,code . ,name)
				    sharpm-function-names)))))
	    nil)))


;; standard sharp sign functions:
(declare (special ibase))

(defsharp (o O) (x) ((lambda (ibase) (list (read))) 8.))  ;#o #O
(defsharp (x X) (x) (do ((res 0)			  ;#x #X (hex)
			 (this (tyi) (tyi))
			 (firstch t nil)
			 (factor 1))
			(nil)
			(cond ((not (or (> this 57.)	; #/0 <= this <= #/9
					(< this 48.)))
			       (setq res (+ (* res 16.) (- this 48.))))
			      ((not (or (> this 102.)   ; #/a <= this <= #/f
					(< this  97.)))
			       (setq res (+ (* res 16.) (- this (- 97 10)))))
			      ((not (or (> this 70.)
					(< this 65.)))
			       (setq res (+ (* res 16.) (- this (- 65 10)))))
			      ((and firstch (eq this 43.)))		; #/+
			      ((and firstch (eq this 45.))		; #/-
			       (setq factor (* -1 factor)))
			      (t (untyi this)
				 (return (list (* factor res)))))))
			    
			      

(defsharp + (x) ((lambda (frob)				; #+
		      (cond ((not (feature-present frob)) (read)))
		      nil)
		   (read)))
(defsharp - (x) ((lambda (frob)				; #-
		      (cond ((feature-present frob) (read)))
		      nil)
		   (read)))
(defsharp / (x) (list (tyi)))				;#/  fixum equiv
(defsharp ^ (x) (list (boole 1 31. (tyi))))		;#^  cntrl next char
(defsharp \' (x) (list (list 'function (read))))	;#'  function
(defsharp (\, \.) (x) (list (eval (read))))		;#, or #.
(defsharp \\ (x) ((lambda (frob char)			;#\
		      (setq char
			    (cdr (assq frob franz-symbolic-character-names)))
		      (or char (error '|Illegal character name in #\\| frob))
		      (list char))
		   (read) nil))
(defsharp (t T) (x) (list t))			;#t (for NIL)
(defsharp (M m  Q q  F f) (char)  ;M m Q q F f
   (cond ((not (feature-present
		  (cadr (assoc char '((77. maclisp) (109. maclisp)
				      (81. lispm) (113. lispm)
				      (70. franz) (102. franz))))))
	  (read)))
   nil)
		      

(defun feature-present (feature)
       (cond ((atom feature)
	      (memq feature (status features)))	;damn fsubrs
	     ((eq (car feature) 'not)
	      (not (feature-present (cadr feature))))
	     ((eq (car feature) 'and)
	      (do ((list (cdr feature) (cdr list)))
		  ((null list) t)
		  (cond ((not (feature-present (car list)))
			 (return nil)))))
	     ((eq (car feature) 'or)
	      (do ((list (cdr feature) (cdr list)))
		  ((null list) nil)
		  (cond ((feature-present (car list))
			 (return t)))))
	     (t (error '|Unknown form after #+ or #-| feature))))

(setq franz-symbolic-character-names
      '((eof . -1)  (backspace . 8.)(bs . 8.)
        (tab . 9.) (lf . 10.) (linefeed . 10.)
	(ff . 12.) (form . 12.) (return . 13.) (cr . 13.)
	(newline . 10.) (vt . 11.)
        (esc . 27.) (alt . 27.) 
	(space . 32.) (sp . 32.)
	(dq . 34.)     ; "
	(lpar . 40.) (rpar . 41.)
	(vert . 124.)  ; |
	(rubout . 127.)
	))

