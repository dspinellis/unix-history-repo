(setq |SCCS-machacks| "@(#)machacks.l	1.4	2/27/81")

;; machacks - maclisp compatibility package.
;; When this file is fasl'ed into a lisp, it will change the syntax to
;; maclisp's syntax and will define functions know to the standard maclisp.
;
; this file will be fasled whenever the -m switch is set for compilation.
;
;; Vaxima installation note:
;  * each site which runs vaxima should set the following symbol to 
;    the location of the Vaxima main directory
(setq vaxima-main-dir "//vb//mac//")

(declare (macros t))

;-- macsyma-env 
; This really isn't part of the maclisp compatibility package but we put
; it here to allow us to bootstrap the macsyma macro packages.
;
(def macsyma-env		; put at the beginning of each macsyma file
  (macro (l) `(include |libmax//prelud.l|)))

(def coutput
  (lambda (msg)
	  (print msg)	; should go to unfasl port
	  (terpr)))

;--- displace 
; This is useful after a macro has been expanded and you want to save the
; interpreter the trouble of expanding the macro again.  
; [This is really only useful for interpretation]
(defun displace (old-form new-form)
       (cond ((atom old-form)
	      (error '|Not able to displace this form| old-form))
	     ((atom new-form)
	      (rplaca old-form 'progn)
	      (rplacd old-form (list new-form)))
	     (t (rplaca old-form (car new-form))
		(rplacd old-form (cdr new-form)))))

;--- caseq
; use is 
;    (caseq expr
;	    (match1 do1)
;	    (match2 do2)
;	    (t  doifallelsefails))
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



;A winning macro to store things anywhere:  (stolen from AI:ALAN;LSPENV)
;(SETF (CADR X) 3) --> (RPLACA (CDR X) 3)

(DEFMACRO SETF (EXPR VAL)
	  (COND ((ATOM EXPR)
		 (OR (SYMBOLP EXPR) (ERROR '|-- SETF can't handle this.| EXPR))
		 `(SETQ ,EXPR ,VAL))
		(T
		 (DO ((Y)) (())
		     (OR (SYMBOLP (CAR EXPR))
			 (ERROR '|-- SETF can't handle this.| EXPR))
		     (AND (SETQ Y (GET (CAR EXPR) 'SETF-EXPAND))
			  (RETURN (FUNCALL Y EXPR VAL)))
		     (OR (and
			   (SETQ Y (GETd (CAR EXPR)))
			   (or (and (dtpr y)
				    (eq (car y) 'macro))
			       (and (bcdp y)
				    (eq (getdisc y) 'macro))))
			 (ERROR '|-- SETF can't handle this.| EXPR))
		     (SETQ EXPR (apply Y EXPR))))))

(DEFMACRO DEFSETF (NAME VARS &REST BODY)
	  `(eval-when 
	    (compile load eval)
	    (DEFPROP ,NAME (LAMBDA ,VARS . ,BODY) SETF-EXPAND)))

(DEFSETF CAR (E V) `(RPLACA ,(CADR E) ,V))
(DEFSETF CAAR (E V) `(RPLACA (CAR ,(CADR E)) ,V))
(DEFSETF CADR (E V) `(RPLACA (CDR ,(CADR E)) ,V))
(DEFSETF CAAAR (E V) `(RPLACA (CAAR ,(CADR E)) ,V))
(DEFSETF CADAR (E V) `(RPLACA (CDAR ,(CADR E)) ,V))
(DEFSETF CAADR (E V) `(RPLACA (CADR ,(CADR E)) ,V))
(DEFSETF CADDR (E V) `(RPLACA (CDDR ,(CADR E)) ,V))
(DEFSETF CAAAAR (E V) `(RPLACA (CAAAR ,(CADR E)) ,V))
(DEFSETF CADAAR (E V) `(RPLACA (CDAAR ,(CADR E)) ,V))
(DEFSETF CAADAR (E V) `(RPLACA (CADAR ,(CADR E)) ,V))
(DEFSETF CADDAR (E V) `(RPLACA (CDDAR ,(CADR E)) ,V))
(DEFSETF CAAADR (E V) `(RPLACA (CAADR ,(CADR E)) ,V))
(DEFSETF CADADR (E V) `(RPLACA (CDADR ,(CADR E)) ,V))
(DEFSETF CAADDR (E V) `(RPLACA (CADDR ,(CADR E)) ,V))
(DEFSETF CADDDR (E V) `(RPLACA (CDDDR ,(CADR E)) ,V))
(DEFSETF CDR (E V) `(RPLACD ,(CADR E) ,V))
(DEFSETF CDAR (E V) `(RPLACD (CAR ,(CADR E)) ,V))
(DEFSETF CDDR (E V) `(RPLACD (CDR ,(CADR E)) ,V))
(DEFSETF CDAAR (E V) `(RPLACD (CAAR ,(CADR E)) ,V))
(DEFSETF CDDAR (E V) `(RPLACD (CDAR ,(CADR E)) ,V))
(DEFSETF CDADR (E V) `(RPLACD (CADR ,(CADR E)) ,V))
(DEFSETF CDDDR (E V) `(RPLACD (CDDR ,(CADR E)) ,V))
(DEFSETF CDAAAR (E V) `(RPLACD (CAAAR ,(CADR E)) ,V))
(DEFSETF CDDAAR (E V) `(RPLACD (CDAAR ,(CADR E)) ,V))
(DEFSETF CDADAR (E V) `(RPLACD (CADAR ,(CADR E)) ,V))
(DEFSETF CDDDAR (E V) `(RPLACD (CDDAR ,(CADR E)) ,V))
(DEFSETF CDAADR (E V) `(RPLACD (CAADR ,(CADR E)) ,V))
(DEFSETF CDDADR (E V) `(RPLACD (CDADR ,(CADR E)) ,V))
(DEFSETF CDADDR (E V) `(RPLACD (CADDR ,(CADR E)) ,V))
(DEFSETF CDDDDR (E V) `(RPLACD (CDDDR ,(CADR E)) ,V))

(DEFSETF CXR (E V) `(RPLACX ,(CADR E) ,(CADDR E) ,V))

(DEFSETF NTH (E V) `(RPLACA (NTHCDR ,(CADR E) ,(CADDR E)) ,V))

(DEFSETF ARRAYCALL (E V) `(STORE ,E ,V))

(DEFSETF GET (E V) `(PUTPROP ,(CADR E) ,V ,(CADDR E)))

(DEFSETF PLIST (E V) `(SETPLIST ,(CADR E) ,V))

(DEFSETF SYMEVAL (E V) `(SET ,(CADR E) ,V))

(DEFSETF ARG (E V) `(SETARG ,(CADR E) ,V))

(DEFSETF ARGS (E V) `(ARGS ,(CADR E) ,V))

(DEFSETF SFA-GET (E V) `(SFA-STORE ,(CADR E) ,(CADDR E) ,V))

(DEFSETF EXAMINE (E V) `(DEPOSIT ,(CADR E) ,V))


(defmacro list* (&rest forms)
	  (cond ((null forms) nil)
		((null (cdr forms)) (car forms))
		(t (construct-list* forms))))
(defmacro ttf (&rest l) `(list* . , l))


(defun construct-list* (forms)
       (setq forms (reverse forms))
       (do ((forms (cddr forms) (cdr forms))
	    (return-form `(cons ,(cadr forms) ,(car forms))
			 `(cons ,(car forms) ,return-form)))
	   ((null forms) return-form))) 

;; lexpr-funcall is a cross between apply and funcall.  The last arguments
;; is a list of the rest of the arguments
(defmacro lexpr-funcall (func &rest args)
  `(apply ,func (list* ,@args)))

; contents of the file libmax;macros  all of these functions are
; (by default) in maclisp
;; (IF X P Q1 Q2 ...) --> (COND (X P) (T Q1 Q2 ...))
;; It is important that (IF NIL <FORM>) returns NIL as Macsyma code depends
;; upon this in places.  See also IFN in LIBMAX;MAXMAC.

(DEFMACRO IF (PREDICATE THEN &REST ELSE)
	  (COND ((NULL ELSE) `(COND (,PREDICATE ,THEN)))
		(T `(COND (,PREDICATE ,THEN) (T . ,ELSE)))))

;; LET, LET*, LIST* are now a part of Multics Lisp.  Nobody should miss
;; the code commented out below.
;; (LET ((A 3) (B) C) STUFF) --> ((LAMBDA (A B C) STUFF) 3 NIL NIL)
;; (LET* ((A 3) (B 4)) STUFF) --> ((LAMBDA (A) ((LAMBDA (B) STUFF) 4)) 3)

;; (PUSH X S) --> (SETQ S (CONS X S))

(DEFMACRO PUSH (OBJECT LIST) `(SETF ,LIST (CONS ,OBJECT ,LIST)))

;; (POP S) -->   (PROG1 (CAR S) (SETF S (CDR S)))
;; (POP S V) --> (PROG1 (SETF V (CAR S)) (SETF S (CDR S)))
;; This relies on the fact that SETF returns the value stored.

(DEFMACRO POP (LIST &OPTIONAL (INTO NIL INTO-P))
  (COND (INTO-P `(PROG1 (SETF ,INTO (CAR ,LIST))
                        (SETF ,LIST (CDR ,LIST))))
        (T `(PROG1 (CAR ,LIST)
                   (SETF ,LIST (CDR ,LIST))))))

;; (FOR I m n . BODY) will evaluate BODY with I bound to m,m+1,...,n-1
;; sequentially.  (FOR I 0 n . BODY) --> (DOTIMES (I n) . BODY)

(DEFMACRO FOR (VAR START STOP . BODY)
          `(DO ,VAR ,START (1+ ,VAR) (= ,VAR ,STOP) ,@BODY))

(DEFMACRO EVENP (X) `(NOT (ODDP ,X)))

; these were grabbed from lspsrc;umlmac.5
(DEFMACRO WHEN (P . C) `(COND (,P . ,C)))
(DEFMACRO UNLESS (P . C) `(COND ((NOT ,P) . ,C)))
(defmacro DOLIST ((var form index) &rest body &aux (dummy (gensym)) decls)
   (setq decls (cond ((and body 
			   (not (atom (car body)))
			   (eq (caar body) 'DECLARE))
		      (prog2 () (cdar body) (pop body)))))
   (cond (index (setq index (ncons `(,INDEX 0 (1+ ,INDEX)) ))
		(push `(FIXNUM ,INDEX) decls)))
   (and decls (setq decls  (ncons `(DECLARE ,.decls))))
   `(DO ((,DUMMY ,FORM (CDR ,DUMMY)) (,VAR) ,.index )
	((NULL ,DUMMY))
      ,@decls
      (SETQ ,VAR (CAR ,DUMMY))  ,.BODY))

;Repeat a number of times.  <count> evaluates to the number of times,
;and <body> is executed with <var> bound to 0, 1, ...
;Don't generate dummy variable if <count> is an integer.  We could also do this
;if <count> were a symbol, but the symbol may get clobbered inside the body,
;so the behavior of the macro would change.

(DEFMACRO DOTIMES (SPEC &REST BODY)
  (LET (VAR COUNT DUMMY DECLS)
       (SETQ DECLS `(DECLARE 
		     (FIXNUM ,var )	;LOOP VARIABLE TO BE FILLED IN HERE
		     ,.(cond ((and body 
				   (not (atom (car body)))
				   (eq (caar body) 'DECLARE))
			      (prog2 () (cdar body) (pop body))))))
       (COND ((ATOM SPEC) (SETQ VAR (GENSYM) COUNT SPEC)) 
	     ('T (DESETQ (VAR COUNT) SPEC)
		 (COND ((NULL VAR) (SETQ VAR (GENSYM))))
		 (COND ((NOT (FIXP COUNT))
			(SETQ DUMMY `((,(gensym) ,count))
			      COUNT (CAAR DUMMY))))))
       (SETF (CADADR DECLS) VAR)
       `(DO ((,var 0 (1+ ,var)) ,.dummy)
	    ((NOT (< ,var ,count)))
	  ,decls
	  ,.body)))


;; The following is NOT courtesy AI: LISPM2; LMMAC 118
;; Theirs is buggy!
;; PSETQ looks like SETQ but does its work in parallel.
(DEFMACRO PSETQ (&REST REST)
     (COND ((CDDR REST)
	    `(SETQ ,(CAR REST)
		   (PROG1 ,(CADR REST) (PSETQ . ,(CDDR REST)))))
           ;; The last pair.  Keep it simple;  no superfluous
	   ;; (PROG1 (SETQ...) (PSETQ)).
	   ((CDR REST) `(SETQ . ,REST))
	   (T (error '|Odd number of args to PSETQ| rest 'wrng-no-args))))


(defmacro if-for-maclisp-else-lispm (&rest ll) (car ll))

(PROGN 'COMPILE
	       (DEFMACRO LOGAND (&REST FORMS) `(BOOLE 1 . ,FORMS))
	       (DEFMACRO LOGIOR (&REST FORMS) `(BOOLE 7 . ,FORMS))
	       (DEFMACRO LOGXOR (&REST FORMS) `(BOOLE 6 . ,FORMS))
	       )

(DEFMACRO DEFVAR (VARIABLE &OPTIONAL (INITIAL-VALUE NIL IV-P) DOCUMENTATION)
  DOCUMENTATION ;; Ignored for now.
  (IF IV-P `(PROGN 'COMPILE
		   (DECLARE (SPECIAL ,VARIABLE))
		   (OR (BOUNDP ',VARIABLE) (SETQ ,VARIABLE ,INITIAL-VALUE)))
      `(DECLARE (SPECIAL ,VARIABLE))))

(DEFMACRO PSETQ (VAR VALUE . REST)
	  (COND (REST `(SETQ ,VAR (PROG1 ,VALUE (PSETQ . ,REST))))
		(T `(SETQ ,VAR ,VALUE))))

 
;; (DOTIMES (I N) BODY) evaluates BODY N times, with I bound to 0, 1, ..., N-1.
;; (DOLIST (X L) BODY) successively binds X to the elements of L, and evaluates
;; BODY each time.

;; Things to beware of:
;; [1] This won't work for COUNT being a bignum.
;; [2] If COUNT is a symbol, somebody could clobber its value inside the body.
;; [3] Somebody inside of BODY could reference **COUNT**.

(DEFMACRO DOTIMES ((VAR COUNT) . BODY)
  (IF (OR (FIXP COUNT) (SYMBOLP COUNT))
      `(DO ((,VAR 0 (1+ ,VAR)))
	   ((>= ,VAR ,COUNT))
	   (DECLARE (FIXNUM ,VAR))
	   . ,BODY)
      `(DO ((,VAR 0 (1+ ,VAR))
	    (**COUNT** ,COUNT))
	   ((>= ,VAR **COUNT**))
	   (DECLARE (FIXNUM ,VAR **COUNT**))
	   . ,BODY)))

(DEFMACRO DOLIST ((VAR LIST) . BODY)
  `(DO ((**LIST** ,LIST (CDR **LIST**))
	(,VAR))
       ((NULL **LIST**))
       (SETQ ,VAR (CAR **LIST**))
       . ,BODY))

;; CASE is apparently missing from ITS MacLisp.
;; (DEFMACRO SELECT (KEY . FORMS)
;; 	  (SETQ FORMS
;; 		(MAPCAR #'(LAMBDA (FORM) (IF (EQ (CAR FORM) 'OTHERWISE)
;; 					     (CONS T (CDR FORM)) FORM))
;; 			FORMS))
;; 	  `(CASE ,KEY . ,FORMS))

(DEFMACRO SELECTQ (KEY . FORMS)
	  (SETQ FORMS
		(MAPCAR '(LAMBDA (FORM) (IF (EQ (CAR FORM) 'OTHERWISE)
					     (CONS T (CDR FORM)) FORM))
			FORMS))
	  `(CASEQ ,KEY . ,FORMS))

