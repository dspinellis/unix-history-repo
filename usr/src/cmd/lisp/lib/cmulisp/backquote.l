(setq SCCS-backquote "@(#)backquote.l	1.2	10/22/80")

(setsyntax '\; 'splicing 'zapline)	

;---- This file contains the definitions of the backquote and sharpsign
; character macros. [The backquote macro actually uses two characters,
; ` and ,]

(setq **backquote** 1)

(eval-when (eval) (cond ((null (getd 'declare)) 
			 (def declare (nlambda (l) nil)))))

(declare (special **backquote**))

(def back-quote-ch-macro 
  (lambda nil 
	  (back=quotify  ((lambda (**backquote**) (read)) 
			  (1+ **backquote**)))))

(def back-quote-comma-macro
 (lambda nil
  ((lambda (**backquote**)
	   (cond ((zerop **backquote**)
		  (break "comma not inside a backquote."))
		 ((eq (tyipeek) 64)
		  (tyi)
		  (cons '\,@ (read)))
		 ((eq (tyipeek) 46)
		  (tyi)
		  (cons '\,\. (read)))
		 (t (cons '\, (read)))))
   (1- **backquote**))))

(def back=quotify 
  (lambda (x)
	  ((lambda (a d aa ad dqp)
		   (cond ((atom x) (list 'quote x))
			 ((eq (car x) '\,) (cdr x))
			 ((or (atom (car x))
			      (not (memq (caar x) '( \,@ \,\.))))
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
			 ((eq (caar x) '\,@)
			  (list 'append (cdar x) (back=quotify (cdr x))))
			 ((eq (caar x) '\,\.)
			  (list 'nconc (cdar x)(back=quotify (cdr x))))
			 ))
	   nil nil nil nil nil)))

(putd '1+ (getd 'add1))
(putd '1- (getd 'sub1))

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

(declare (special franz-symbolic-character-names))

; (setsyntax '\# 'splicing 'sharp-sign-macro)

(def sharp-sign-macro 
  (lambda ()
	  ((lambda (char frob)
		   (setq char (tyi))
		   (cond ((or (eq char 79.)			;O
			      (eq char 105.))			;o
			  ((lambda (ibase) (list (read))) 8.))
			 
			 ((eq char 43.)			;+
			  (setq frob (read))
			  (cond ((not (feature-present frob)) (read)))
			  nil)
			 ((eq char 45.)			;-
			  (setq frob (read))
			  (cond ((feature-present frob) (read)))
			  nil)
			 
			 ((eq char 47.)			;/
			  (list (tyi)))			;return numeric value of CHAR
			 ;list because splicing
			 
			 ((eq char 94.)			; ^ returns following char
			  (list (boole 1 31. (tyi))))	; made into a control char
			 
			 ((eq char 39.)			;'
			  (list (list 'function (read))))
			 
			 ((or (eq char 44.) (eq char 46.))	;, or .
			  (list (eval (read))))
			 
			 ((eq char 92.)			;\
			  (setq frob (read))		;get symbolic name of character
			  (setq char
				(cdr (assq frob franz-symbolic-character-names)))
			  (or char (error '|Illegal character name in #\\| frob))
			  (list char))
			 
			 (t (error '|Bad character after #| (ascii char)))))
	   nil nil)))

(def feature-present (lambda(feature)
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
	     (t (error '|Unknown form after #+ or #-| feature)))))

(setq franz-symbolic-character-names
      '((eof . -1) (bs . 8.) (backspace . 8.)
        (tab . 9.) (lf . 10.) (linefeed . 10.)
	(ff . 12.) (form . 12.) (return . 13.) (cr . 13.)
	(newline . 10.) (vt . 9.)
	(alt . 27.) (esc . 27.)
	(sp . 32.) (space . 32.)
	(rubout . 127.)))

