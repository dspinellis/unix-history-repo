;;
;;  common0.l				-[Thu Apr  1 10:23:14 1982 by jkf]-
;;
;;   Functions which are required to execute the low level lisp macros
;; and functions.
;;
;;   This is the first file of functions read in when building a lisp.
;; If this lisp is to run interpretedly, then we must not use anything
;; which hasn't be defined in the C lisp kernel, except ';' which is
;; defined as the comment character before reading this file.
;; We cannot use defmacro, the backquote or the # macro.
;;
;;   This file should be as short as possible since it must be written in
;; a rather primitive way.
;;

(setq SCCS-common0 "@(#)common0.l	1.3	4/1/82")

;--- declare : ignore whatever is given, this info is for the compiler
;
(def declare (nlambda (x) nil))

(declare (macros t))

;--- defun - standard maclisp function definition form.
;
(def defun 
       (macro (l)
	     (prog (name type arglist body specind specnam)
		   (setq name (cadr l) l (cddr l))
		   (cond ((dtpr name) 
			  (cond ((memq (cadr name) '(macro expr fexpr lexpr))
				 (setq l (cons (cadr name) l)
				       name (car name)))
				(t (setq specnam (car name)
					 specind (cadr name)
					 name (concat (gensym) "::" specnam))))))
		   (cond ((null (car l)) (setq type 'lambda))
			 ((eq 'fexpr (car l)) (setq type 'nlambda l (cdr l)))
			 ((eq 'expr (car l))  (setq type 'lambda l (cdr l)))
			 ((eq 'macro (car l)) (setq type 'macro l (cdr l)))
			 ((atom (car l))
			  (setq type 'lexpr
				l (nconc (list (list (car l)))
					 (cdr l))))
			 (t (setq type 'lambda)))
		   (cond ((and (eq 'lambda type) (or (memq '&aux (car l))
						     (memq '&optional (car l))
						     (memq '&rest (car l))
						     (memq '&body (car l))))
			  (setq l (lambdacvt l)
				type (car l)
				l (cdr l))))
		   (setq body (list 'def name (cons type l)))
		   (cond (specnam 
			  (return (list 'progn ''compile
					  body
					  (list 'putprop
						(list 'quote specnam)
						(list 'getd
						      (list 'quote name))
						(list 'quote specind)))))
			 (t (return body))))))


;--- error : print error message and cause an error
;  call is usually (error "string" value)
;
(def error
  (lexpr (n)
	 (cond ((greaterp n 0)
		(patom (arg 1))
		(cond  ((greaterp n 1)
			(patom " ")
			(patom (arg 2))))
		(terpr)))
	 (err)))



;--- append : append two or more lists
; the result will be a copy of all but the last list
;
(declare (localf append2args))		

(def append
  (lexpr (nargs)
	 (cond ((eq nargs 2) (append2args (arg 1) (arg 2)))
	       ((zerop nargs) nil)
	       (t (do ((i (1- nargs) (1- i))
		       (res (arg nargs)))
		      ((zerop i) res)
		      (setq res (append2args (arg i) res)))))))

;--- append2args : append just two args
; a version of append which only works on 2 arguments
;
(def append2args 
  (lambda (x y)
	  (prog (l l*)
		(cond ((null x) (return y))
		      ((atom x) (error "Non-list to append:" x)))
		(setq l* (setq l (cons (car x) nil)))
	loop	(cond ((atom x) (error "Non-list to append:" x))
		      ((setq x (cdr x))
		       (setq l* (cdr (rplacd l* (cons (car x) nil))))
		       (go loop)))
		(rplacd l* y)
		(return l))))

;--- append1 : add object to end of list
; adds element y to then end of a copy of list x
;
(def append1 (lambda (x y) (append x (list y))))

;--- assoc - x : lispval
;	   - l : list
;	l is a list of lists. The list is examined and the first
;	sublist whose car equals x is returned.
;
(def assoc
  (lambda (val alist)
	  (do ((al alist (cdr al)))
	      ((null al) nil)
	      (cond ((null (car al)))
		    ((not (dtpr (car al)))
		     (error "bad arg to assoc" al))
		    ((equal val (caar al)) (return (car al)))))))

;--- rassq : like assq but look at the cdr instead of the car
;
(def rassq
   (lambda (form list)
      (cond ((null list) nil)
	    ((not (dtpr list))
	     (error "rassq: illegal second argument: " list))
	    (t (do ((ll list (cdr ll)))
		   ((null ll) nil)
		   (cond ((eq form (cdar ll)) (return (car ll)))))))))
;--- concatl - l : list of atoms
;	returns the list of atoms concatentated
;
(def concatl
 (lambda (x) (apply 'concat x)))


;--- memq - arg : (probably a symbol)
;	  - lis : list
; returns part of lis beginning with arg if arg is in lis
;	
(def memq
  (lambda ($a$ $l$)
	  (do ((ll $l$ (cdr ll)))
	      ((null ll) nil)
	      (cond ((eq $a$ (car ll)) (return ll))))))

;--- nconc - x1 x2 ...: lists
;	The cdr of the last cons cell of xi is set to xi+1.  This is the
;	structure modification version of append
;

(def nconc 
  (lexpr (nargs) 
	 (cond ((eq nargs '2) 
		(cond ((null (arg 1)) (arg 2))
		      (t (do ((tmp (arg 1) (cdr tmp)))
			     ((null (cdr tmp)) 
			      (rplacd tmp (arg 2))
			      (arg 1))))))
	       ((zerop nargs) nil)
	       (t (do ((i 1 nxt)
		       (nxt 2 (1+ nxt))
		       (res (cons nil (arg 1)))) 
		      ((eq i nargs) (cdr res))
		      (cond ((arg i) (rplacd (last (arg i)) (arg nxt)))
			    (t (rplacd (last res) (arg nxt)))))))))



(declare (localf nreverse1))	; quick fcn shared by nreverse and nreconc

;--- nreconc :: nreverse and nconc
; (nreconc list elemt) is equiv to (nconc (nreverse list) element)
;
(defun nreconc (list element)
  (cond ((null list) element)
	(t (nreverse1 list element))))

;--- nreverse - l : list
;	reverse the list in place
;

(defun nreverse (x)
  (cond ((null x) x)
	(t (nreverse1 x nil))))


;--- nreverse1
;  common local function to nreconc and nreverse.  [This can just be
; nreconc when I get local global functions allow in the compiler -jkf]
;
(defun nreverse1 (x ele)
  (prog (nxt)
  loop
	(setq nxt (cdr x))
	(rplacd x ele)
	(setq ele x)
	(cond (nxt (setq x nxt) (go loop)))
	(return x)))

