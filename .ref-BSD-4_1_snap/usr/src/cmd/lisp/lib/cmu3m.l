;;(jkf) Some of these should be written as macros to make them compilable.
;;
(declare (macros t))

; VIII. Additions from CMU (-DNC)
;	***		list*		neq
;	nreconc		nth		push		pop
;			munkam

;(def *** (nlambda (arg) nil))		; the CMU comment function
(defmacro *** (&rest junk) nil)		; macroized by jkf

;(jkf)- CMU version of list*
;(def list*
;  (lexpr (n)
;        (cond ((zerop n) (err '"no args to list*")))
;	(do ((left (sub1 n) (sub1 left))
;	     (tail (arg n) (cons (arg left) tail)))
;	    ((< left 1) tail)
;	    nil)))		; -DNC (written by LWE) - 
				; (list* 'a 'b 'c) = (a b .c)

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

;(jkf)- cmu neq
;(def neq			; -DNC inserted (cause CMU lisp uses it)
; (lambda (x y)
;    (not (eq x y))))
(defmacro neq (a b) `(not (eq ,a ,b)))

;(jkf) nreconc in auxfns0.l now
;(def nreconc
;   (lambda (x y) (nconc (nreverse x) y)))
;			; -DNC (LWE) added to correspond to maclisp


;----nth[N,L] returns the Nth element of list L, with 0 being the first
;             element
;----LWE 1/29/80
;
(defun nth (N L)
	(cond ((null L)nil)
	      (t(do ((LCDR L (cdr LCDR))
		     (COUNT N (1- COUNT)))
		    ((or (and (atom LCDR) LCDR
   		              (err '"non-proper list passed to nth"))
		         (or (lessp COUNT 0)(zerop COUNT)))
		     (car LCDR))
		    nil))))

;---push[X,L] is like (SETQ L (CONS X L)). Use where L is like a stack.
;
;---LWE 1/29/80
;--- macroized by jkf, 25jun81

;(def push 
;     (nlambda (nargs)
;	(set (cadr nargs)
;	     (cons (eval (car nargs))
;		   (eval (cadr nargs))))))

(defmacro push (value stack) `(setq ,stack (cons ,value ,stack)))

;---pop[L,X] returns CAR of L, sets L to CDR of L, and optionally assigns
;            the popped value to X.
;
;---LWE 1/29/80
;--- macroized by jkf, 

;(def pop 
;     (nlambda (nargs)
;	((lambda (top-of-stack)
;		(cond ((cadr nargs)
;		       (set (cadr nargs) top-of-stack))
;		      (top-of-stack)))
; 	 (prog2 nil 
;		(car (eval (car nargs)))
;		(set (car nargs)(cdr (eval (car nargs))))))))

(defmacro pop (stack &optional (storeit nil storeit-p))
  (cond ((storeit-p) `(setq ,storeit (prog1 (car ,stack)
					    (setq ,stack (cdr ,stack)))))
	(t (prog1 (car ,stack) (setq ,stack (cdr ,stack))))))

; the following has been added for Maclisp compatibility 
; LWE 1/29/80


;(jkf) this is actually maknum is the maclisp terminology
(putd 'munknam (getd 'maknum))

; added for CMULisp compatibilty (used by editor etc)
(putd 'prin1 (getd 'print))

