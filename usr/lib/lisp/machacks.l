(setq rcs-machacks-
   "$Header: machacks.l 1.5 83/07/05 00:04:09 jkf Exp $")

;; (c) copywrite 1982, University of California, Berkeley
;; (c) copywrite 1982, Massachusetts Insititute of Technology

;; This file was originally written at the University of California,
;; Berkeley.  Some portions were modified and additions made were made at
;; MIT.

;; machacks - maclisp compatibility package.
;; when this file is fasl'ed into a lisp, it will change the syntax to
;; maclisp's syntax and will define functions know to the standard maclisp.
;; it is also used to bootstrap vaxima compilation.
;
; this file will be fasled whenever the -m switch is set for compilation.
;

(declare (macros t))

(def coutput
  (lambda (msg)
	  (print msg)	; should go to unfasl port
	  (terpr)))

;--- displace 
; this is useful after a macro has been expanded and you want to save the
; interpreter the trouble of expanding the macro again.  
; [this is really only useful for interpretation]
(defun displace (old-form new-form)
       (cond ((atom old-form)
	      (error '|not able to displace this form| old-form))
	     ((atom new-form)
	      (rplaca old-form 'progn)
	      (rplacd old-form (list new-form)))
	     (t (rplaca old-form (car new-form))
		(rplacd old-form (cdr new-form)))))



;--- fboundp  :: check if a symbol has a function binding
;
(defmacro fboundp (form &protect (form)) `(and (symbolp ,form) (getd ,form)))




(defmacro list* (&rest forms)
	  (cond ((null forms) nil)
		((null (cdr forms)) (car forms))
		(t (construct-list* forms))))

(eval-when (load compile eval)
   (defun construct-list* (forms)
	  (setq forms (reverse forms))
	  (do ((forms (cddr forms) (cdr forms))
	       (return-form `(cons ,(cadr forms) ,(car forms))
			     `(cons ,(car forms) ,return-form)))
	      ((null forms) return-form))))

(defmacro ttf (&rest l) `(list* . , l))


;; lexpr-funcall is a cross between apply and funcall.  the last arguments
;; is a list of the rest of the arguments
;; this is now in Franz Opus 38.35
;; (defmacro lexpr-funcall (func &rest args)
;;    `(apply ,func (list* ,@args)))

; contents of the file libmax;macros  all of these functions are
; (by default) in maclisp
;; (if x p q1 q2 ...) --> (cond (x p) (t q1 q2 ...))
;; it is important that (if nil <form>) returns nil as macsyma code depends
;; upon this in places.  see also ifn in libmax;maxmac.
; in Franz Lisp, opus 38.36 and on
;(defmacro if (predicate then &rest else)
;	  (cond ((null else) `(cond (,predicate ,then)))
;		(t `(cond (,predicate ,then) (t . ,else)))))

;; let, let*, list* are now a part of multics lisp.  nobody should miss
;; the code commented out below.
;; (let ((a 3) (b) c) stuff) --> ((lambda (a b c) stuff) 3 nil nil)
;; (let* ((a 3) (b 4)) stuff) --> ((lambda (a) ((lambda (b) stuff) 4)) 3)

;; (push x s) --> (setq s (cons x s))
; in franz
;(defmacro push (object list) `(setf ,list (cons ,object ,list)))

;; (pop s) -->   (prog1 (car s) (setf s (cdr s)))
;; (pop s v) --> (prog1 (setf v (car s)) (setf s (cdr s)))
;; this relies on the fact that setf returns the value stored.

;(defmacro pop (list &optional (into nil into-p))
;  (cond (into-p `(prog1 (setf ,into (car ,list))
;                        (setf ,list (cdr ,list))))
;        (t `(prog1 (car ,list)
;                   (setf ,list (cdr ,list))))))

;; (for i m n . body) will evaluate body with i bound to m,m+1,...,n-1
;; sequentially.  (for i 0 n . body) --> (dotimes (i n) . body)

(defmacro for (var start stop . body)
          `(do ,var ,start (1+ ,var) (= ,var ,stop) ,@body))

; these were grabbed from lspsrc;umlmac.5
(defmacro when (p . c) `(cond (,p . ,c)))
(defmacro unless (p . c) `(cond ((not ,p) . ,c)))


(defmacro if-for-maclisp-else-lispm (&rest ll) (car ll))

(defmacro logand (&rest forms) `(boole 1 . ,forms))
(defmacro logior (&rest forms) `(boole 7 . ,forms))
(defmacro logxor (&rest forms) `(boole 6 . ,forms))
(defmacro lognot (n) `(boole 10. ,n -1))
(defmacro bit-test (&rest forms) `(not (zerop (boole 1 . ,forms))))
(defmacro bit-set (x y) `(boole 7 ,x ,y))
(defmacro bit-clear (x y) `(boole 2 ,x ,y))

;; (<= a b) --> (not (> a b))
;; (<= a b c) --> (not (or (> a b) (> b c)))
;; funny arglist to check for correct number of arguments.

(defmacro <= (arg1 arg2 &rest rest &aux result)
  (setq rest (list* arg1 arg2 rest))
  (do l rest (cdr l) (null (cdr l))
      (push `(> ,(car l) ,(cadr l)) result))
  (cond ((null (cdr result)) `(not ,(car result)))
	(t `(not (or . ,(nreverse result))))))

;; (>= a b) --> (not (< a b))
;; (>= a b c) --> (not (or (< a b) (< b c)))
;; funny arglist to check for correct number of arguments.

(defmacro >= (arg1 arg2 &rest rest &aux result)
  (setq rest (list* arg1 arg2 rest))
  (do l rest (cdr l) (null (cdr l))
      (push `(< ,(car l) ,(cadr l)) result))
  (cond ((null (cdr result)) `(not ,(car result)))
	(t `(not (or . ,(nreverse result))))))



(defmacro psetq (var value . rest)
  (cond (rest `(setq ,var (prog1 ,value (psetq . ,rest))))
	(t `(setq ,var ,value))))

 
;; (dotimes (i n) body) evaluates body n times, with i bound to 0, 1, ..., n-1.
;; (dolist (x l) body) successively binds x to the elements of l, and evaluates
;; body each time.

;; things to beware of:
;; [1] this won't work for count being a bignum.
;; [2] if count is a symbol, somebody could clobber its value inside the body.
;; [3] somebody inside of body could reference **count**.

(defmacro dotimes ((var count) . body)
  (if (or (fixp count) (symbolp count))
      `(do ((,var 0 (1+ ,var)))
	   ((>= ,var ,count))
	   (declare (fixnum ,var))
	   . ,body)
      `(do ((,var 0 (1+ ,var))
	    (**count** ,count))
	   ((>= ,var **count**))
	   (declare (fixnum ,var **count**))
	   . ,body)))

(defmacro dolist ((var list) . body)
  `(do ((**list** ,list (cdr **list**))
	(,var))
       ((null **list**))
       (setq ,var (car **list**))
       . ,body))


;; symbolconc is the same as concat in franz
;
(defmacro symbolconc (&rest args) `(concat ,@args))


;-- these functions are from /usr/lib/lisp/lmhacks on the mit-vax

;;  This file contains miscellaneous functions and macros that 
;;  ZetaLisp users often find useful

(declare (macros t))

(defmacro macro (name argl &body body)
  `(def ,name (macro ,argl ,@body)))

(defun gcd (a b)
  (or (plusp a)
      (setq a (minus a)))
  (or (plusp b)
      (setq b (minus b)))
  (do ((a a b)
       (b b (remainder a b)))
      ((zerop b)
       a)))

(defmacro first (a) `(car ,a))
(defmacro second (a) `(cadr ,a))
(defmacro third (a) `(caddr ,a))
(defmacro fourth (a) `(cadddr ,a))
(defmacro fifth (a) `(car (cddddr ,a)))
(defmacro sixth (a) `(cadr (cddddr ,a)))
(defmacro seventh (a) `(caddr (cddddr ,a)))

(defmacro rest1 (list) `(cdr ,list))
(defmacro rest2 (list) `(cddr ,list))
(defmacro rest3 (list) `(cdddr ,list))
(defmacro rest4 (list) `(cddddr ,list))

(defmacro copylist (list) `(append ,list nil))
(defmacro copytree (list) `(subst nil nil ,list))

(defun circular-list (&rest elements)
  (setq elements (copylist elements))
  (rplacd (last elements) elements)
  elements)

(defun butlast (x)
  (cond ((null (cdr x)) nil)
	(t (cons (car x) (butlast (cdr x))))))

(defun find-position-in-list (item list)
  (do ((i 0 (1+ i)))
      ((null list) nil)
    (if (eq (car list) item)
	(return i)
	(setq list (cdr list)))))

(defun find-postion-in-list-equal (item list)
  (do ((i 0 (1+ i)))
      ((null list) nil)
    (if (equal (car list) item)
	(return i)
	(setq list (cdr list)))))

(defun mem (pred item list)
  (do ()
      ((null list) nil)
      (if (funcall pred item (car list))
	  (return list))
      (setq list (cdr list))))



;--- remq is in common2.l



(defun rem (pred item list &optional (cnt -1))
  (let ((head '())
	(tail nil))
    (do ((l list (cdr l))
	 (newcell))
	((null l) head)
      (cond ((or (funcall pred (car l) item)
		 (zerop cnt))
	     (setq newcell (list (car l)))
	     (cond ((null head) (setq head newcell))
		   (t (rplacd tail newcell)))
	     (setq tail newcell))
	    (t (setq cnt (1- cnt)))))))

(defun rem-if (pred list)
  (let ((head '())
	(tail nil))
    (do ((l list (cdr l))
	 (newcell))
	((null l) head)
      (cond ((not (funcall pred (car l)))
	     (setq newcell (list (car l)))
	     (cond ((null head) (setq head newcell))
		   (t (rplacd tail newcell)))
	     (setq tail newcell))))))

(defun rem-if-not (pred list)
  (let ((head '())
	(tail nil))
    (do ((l list (cdr l))
	 (newcell))
	((null l) head)
      (cond ((funcall pred (car l))
	     (setq newcell (list (car l)))
	     (cond ((null head) (setq head newcell))
		   (t (rplacd tail newcell)))
	     (setq tail newcell))))))

(make-equivalent subset rem-if-not)
(make-equivalent subset-not rem-if)

(defun del (pred item list &optional (cnt -1))
  (let ((ret (cons nil list)))
    (do ((list ret))
	((null (cdr list))
	 (cdr ret))
      (cond ((and (funcall pred item (second list))
		 (not (zerop cnt)))
	     (setq cnt (1- cnt))
	     (rplacd list (cddr list)))
	    (t (setq list (cdr list)))))))

(defun del-if (pred list)
  (let ((ret (cons nil list)))
    (do ((list ret))
	((null (cdr list))
	 (cdr ret))
	(if (funcall pred (second list))
	    (rplacd list (cddr list))
	    (setq list (cdr list))))))

(defun del-if-not (pred list)
  (let ((ret (cons nil list)))
    (do ((list ret))
	((null (cdr list))
	 (cdr ret))
	(if (not (funcall pred (second list)))
	    (rplacd list (cddr list))
	    (setq list (cdr list))))))

(defun some (forms pred &optional step-function)
  (and (not (null forms))
       (if (funcall pred (car forms))
	   forms
	   (some (if (null step-function)
		     (cdr forms)
		     (funcall step-function forms))
		 pred
		 step-function))))

(defun every (forms pred &optional step-function)
  (or (null forms)
      (and (funcall pred (car forms))
	   (every (if (null step-function)
		      (cdr forms)
		      (funcall step-function forms))
		  pred
		  step-function))))

(defmacro pairp (x) `(dtpr ,x))

(defun tailp (subset set)
  (do ((s set (cdr s)))
      ((null s) nil)
      (cond ((eq s subset) (return t)))))

; defunp
; like defun except it's an implicit prog
; expands
;  (defunp fn (args) form1 form2 ... formn)
; into
;  (defun fn (args) (prog () form1 form2 ... (return (formn))))
; and hence allows returns in the middle of "defun"'s
; If original defun body is just one form (eg, let, cond, etc.)
; return is wrapped around the whole thing.
;

(defmacro defunp (fn arglist . body)
  `(defun ,fn ,arglist
     (prog ()
       ,@(let ((bodyrev (reverse body)))
	      (nreconc (cdr bodyrev) 
		       (cond ((eq 'return (caar bodyrev))
			      ; last form is already a return
			      `(,(car bodyrev)))
			     (t `((return ,(car bodyrev))))))))))

(defmacro let-globally (vars-values . body)
  (let ((temp-vars (mapcar #'(lambda (q) (gensym)) vars-values)))
    `(progn ((lambda ,temp-vars ,@(mapcar #'(lambda (var-value value)
					      `(setq ,(car var-value) ,value))
					  vars-values temp-vars))
	      ,@(mapcar #'cadr vars-values))
	    ,@body)))

(defmacro local-declare (dcls . body)
    `(progn 'compile ,@(mapcar #'(lambda (x) `(declare ,x)) dcls) ,@body))

(defmacro defconst (variable &optional (initial-value nil iv-p) documentation)
  documentation ;; ignored for now.
  (if iv-p `(progn 'compile
		   (eval-when (eval compile load)
			      (declare (special ,variable)))
		   (setq ,variable ,initial-value))
      `(eval-when (eval compile load)
		  (declare (special ,variable)))))

(defmacro check-arg (var-name predicate description)
  `(if (not ,(if (atom predicate)
		 `(,predicate ,var-name)
		 predicate))
       (ferror t "The argument ~S was ~S, which is not ~A.~%"
	       ',var-name ,var-name ,description)))

(defmacro check-arg-type (var-name type-name &optional description)
  `(if (not (typep ,var-name ,type-name)
       (ferror t "The argument ~s was ~S, which is not ~A~A.~%"
	       ',var-name ,var-name
	       ,(if (null description) " a" "")
	       ,(if (null description) type-name description)))))

;;; Defsubst

(defmacro defsubst (function-spec lambda-list &body body)
  `(progn 'compile
     (defun ,function-spec ,lambda-list ,@body)
     (defcmacro ,function-spec ,lambda-list
       (sublis (list ,@(do ((v lambda-list (cdr v))
			    (r nil (cons `(cons ',(car v) ,(car v)) r)))
			 ((null v) (nreverse r))))
	       ',(if (null (cdr body)) (car body)
		    `(progn . ,body))))
     ',function-spec))

;--- ^ :: fixnum expt
(defun ^ (x y)
  (expt x y))

(putprop 'machacks t 'version)
