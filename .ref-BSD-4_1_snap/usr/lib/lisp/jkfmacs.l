(setq SCCS-jkfmacs "@(#)jkfmacs.l	1.3	7/9/81")

;------ jkfmacs :: common and useful macros
;
(declare (macros t))

; contents: 
;	If macro
;


;--- super if macro
; This macro allow the following forms:
;	(If a then b)   ==>  (cond (a b))
;	(If a thenret)  ==>  (cond (a))
;	(If a then b else c) ==> (cond (a b) (t c))
;	(If a then b b2 	     ==> (cond (a b b2) (c d d2) (t e))
;	 elseif c then d d2
;	 else e)
;
;
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

;--- msg : print a message consisting of strings and values
; arguments are:
;   N	    - print a newline
;   (N foo) - print foo newlines (foo is evaluated)
;   B       - print a blank
;   (B foo) - print foo blanks (foo is evaluated)
;   (P foo) - print following args to port foo (foo is evaluated)
;   D	    - drain
;   other   - evaluate a princ the result (remember strings eval to themselves)

(defmacro msg (&rest msglist)
  (do ((ll msglist (cdr ll))
       (result)
       (cur nil nil)
       (curport nil)
       (current))
      ((null ll) `(progn ,@(nreverse result)))
      (setq current (car ll))
      (If (dtpr current)
	  then (If (eq (car current) 'N)
		   then (setq cur `(msg-tyo-char 10 ,(cadr current)))
		elseif (eq (car current) 'B)
		   then (setq cur `(msg-tyo-char 32 ,(cadr current)))
		elseif (eq (car current) 'P)
		   then (setq curport (cadr current))
		else (setq cur `(princ ,current)))
       elseif (eq current 'N)
	  then (setq cur (list 'tyo 10))	; (can't use backquote
       elseif (eq current 'B)			; since must have new
      	  then (setq cur (list 'tyo 32))	; dtpr cell at end)
       elseif (eq current 'D)
	  then (setq cur '(drain))
       else (setq cur `(princ ,current)))
      (If cur 
	  then (setq result (cons (If curport then (nconc cur (ncons curport))
			                      else cur)
				  result)))))

(defun msg-tyo-char (ch n)
  (do ((i n (1- n)))
      ((< n 1))
      (tyo ch)))



;--- standard push, unpush and pop macros
;
(defmacro push (stack value)
  `(setq ,stack (cons ,value ,stack)))

(defmacro unpush (stack)
  `(setq ,stack (cdr ,stack)))

(defmacro pop (stack)
  `(prog1 (car stack) (setq ,stack (cdr ,stack))))



(putprop 'jkfmacs 1 'version)

