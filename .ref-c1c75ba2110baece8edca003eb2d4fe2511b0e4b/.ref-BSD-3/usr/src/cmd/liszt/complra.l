;--- file: complra.l
(include "compmacs.l")

(def put 
  (macro (x)
	 ((lambda (atm prp arg)
		  `(progn (putprop ,atm ,arg ,prp) ,atm))
	  (cadr x) (caddr x) (cadddr x))))



; register allocation and important addresses for compiled code
;
(setq np-reg     'r6		;points one beyond top stack value
      lbot-reg   'r7		;current value of lbot
      ln-reg     'r8		;address of linker
      olbot-reg  'r10		;base of args to this fcn
      bnp-reg	 'r11		;bind np
      bnp-val    '"*-32(r8)"	;value of global var bnp
      i-mov      'movl		;stacking instruction for namestack
      i-clr	 'clrl		;clear namestack
      qfuncl	 '"*-28(r8)"	;addr of qfuncl
      )

; these are the short cut places to call when you want to call
; a non system function with 4 or less arguments

(setplist 'qfs '(0 "*-8(r8)"   1 "*-12(r8)"   2 "*-16(r8)"
		 3 "*-20(r8)"  4 "*-24(r8)"))

(setq faslflag nil)

(declare (special w-vars w-labs w-ret w-name w-bv w-atmt cm-alv v-cnt))




(cond ((lessp (opval 'pagelimit) 2000) (opval 'pagelimit 2000)))



(def Gensym (lambda (x)
	(prog (e)
		(setq e (gensym (cond (x) (t 'L))))
		(setq twa-list (cons e twa-list))
		(return e))))

(def cvt (lambda (a)
	(prog (l)
		(setq l (quotient a 2704))
		(setq a (difference a (times l 2704)))
		(setq l (list l (quotient a 52) (mod a 52)))
		(return (mapcar '(lambda (x) (nthelem
			(add1 x)
			'(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
			  a b c d e f g h i j k l m n o p q r s t u v w x y z))) l)))))

(def nth
  (lambda (x n)
	  (cond ((equal n 0) x)
		((lessp n 0)
		 (prog (m lst)
		       (setq m (difference 0 n))
		       (setq x (reverse x))
		    lp (cond ((zerop m) (return lst)))
		       (setq lst (cons (car x) lst))
		       (setq x (cdr x))
		       (setq m (sub1 m))
		       (go lp)))
		(t (nth (cdr x) (sub1 n))))))

(def cleanup (lambda nil
		     (mapc 'rematom twa-list)
		     (setq twa-list nil)))

(def mylogor (lambda (x y)
		   (boole 7 x y)))

(def leftshift 
  (lambda (x cnt)
	  (prog ()
	loop   (cond ((zerop cnt) (return x))
		     ((lessp cnt 0) 
		      (setq x (quotient x 2))
		      (setq cnt (add1 cnt)))
		     (t (setq x (times x 2)) (setq cnt (sub1 cnt))))
		(go loop))))

(def flag 
  (lambda (atm flg)
	(cond ((put atm flg t) atm))))

(def ifflag 
  (lambda (atm flg)
	  (cond ((and (and (atom atm) (not (numberp atm))) 
		      (get atm flg)) 
		 t))))

(def unflag 
  (lambda (atm flg)
	(put atm flg nil)))



;--- chain - a : an atom
;	returns a if a has the form cxr where x is an elt of {a d}
;	else returns nil.
;
(def chain 
  (lambda (a)
	  (prog (expl)
		(cond ((lessp (flatsize a) 3) (return nil)))
		(setq expl (explode a))
		(cond ((not (eq (car expl) 'c)) (return nil)))
	loop    (setq expl (cdr expl))
		(cond ((eq (car expl) 'a) (go loop))
		      ((eq (car expl) 'd) (go loop))
		      ((and (eq (car expl) 'r) (null (cdr expl))) (return a))
		      (t (return nil))))))
		
;--- ismacro - a : atom name found in the functional position
;	returns the body of the macro if a is the name of a macro, else
;	return nil.
;
(def ismacro 
  (lambda (a)
	  (prog (x)
		(cond ((not (symbolp a)) (return nil))
		      ((setq x (assoc a k-macros)) (return (cadr x))))
		(setq x (getd a))
		(cond ((and (bcdp x) (eq (getdisc x) 'macro)) (return x))
		      ((and (dtpr x) (eq (car x) 'macro)) (return x))))))

;--- isnlam - a : atom found in the functional position
;	return the body of the nlambda if a names an nlambda,
;	else return nil
;
(def isnlam 
  (lambda (a)
	  (prog (x)
		(cond ((not (symbolp a)) (return nil)))
		(cond ((setq x (assoc a k-nlams)) (return (cadr x))))
		(setq x (getd a))
		(cond ((and (dtpr x) (eq (car x) 'nlambda)) (return x))
		      ((and (bcdp x) (eq (getdisc x) 'nlambda)) (return x))))))

(def ucar 
  (lambda (arg)
	  (cond ((dtpr arg) (car arg))
		((numberp arg) arg)
		((getd arg) arg)
		(t (get arg '*car)))))

;--- defsysf - funname : lisp function name
;	     - inname  : internal system name
;	We declare that funname is a system type function with
;	the address of the c-code for it at inname.  Thus we
;	can call this function directly without going through
;	the oblist.  This type of optimization can be turned off
;	by disabling this routine (if debuggin is desired)
;
(def defsysf
  (lambda (funname inname)
	  (putprop funname inname 'x-sysf)))	; indicate of prop list

(def $pr$ 
  (macro (x)
	(list 'patom (cadr x) 'vp-sfile)))

(def emit1 
  (lambda (a)
	(aprint a)
	($terpri)))

(def emit2 
  (lambda (a b)
	(aprint a)
	($pr$ '" ")
	(aprint b)
	($terpri)))

(def emit3 
  (lambda (a b c)
	(aprint a)
	($pr$ '" ")
	(aprint b)
	($pr$ '\,)
	(aprint c)
	($terpri)))

(def emit4 
  (lambda (a b c d)
	  (aprint a)
	  ($pr$ '" ")
	  (aprint b)
	  ($pr$ '\,)
	  (aprint c)
	  ($pr$ '\,)
	  (aprint d)
	  ($terpri)))

(def aprint 
  (lambda (foo)
	  (prog nil
		loop (cond ((null foo) (return))
			   ((atom foo) ($pr$ foo) (return))
			   (t ($pr$ (car foo))
			      (setq foo (cdr foo))))
		(go loop))))
		
(def $terpri (lambda () (terpr vp-sfile)))

