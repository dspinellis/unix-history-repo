(setq rcs-vector-
   "$Header: vector.l 1.5 83/07/30 15:35:51 layer Exp $")

;; vector handling functions	-[Sun Jun 19 15:09:14 1983 by jkf]-
;; [also contains closure functions]
;;
;; preliminary.  this is subject to change at any moment.
;; Don't use the functions in this file!!	--jkf
;;
;; contains functions:
;;  vector{,i-byte,i-word,i-long}   : create and initialize
;;  vref{,i-byte,i-word,i-long}	 : reference
;;  vset{,i-byte,i-word,i-long}	 : set
;;  vsize	-- must write
;;  vsize-word
;;  vsize-byte 
;;
;; references external functions
;;  new-vector{,i-byte,i-word,i-long
;;
;; references internal functions:
;;  int:vref 'vect 'index 'class
;;  int:vset 'vect 'index 'value 'class
;;  int:vsize 'vect

;--- vector
;  call is (vector elmt0 elmt1 ... elmtn)
; creates an n-1 size vector and initializes
;
(defmacro vector-macro (create class)
   `(let ((vec (,create n)))
      (do ((from n to)
	   (to (1- n) (1- to)))
	  ((< to 0))
	  (int:vset vec to (arg from) ,class))
      vec))

(defun vector n (vector-macro new-vector 3))
(defun vectori-byte n (vector-macro new-vectori-byte 0))
(defun vectori-word n (vector-macro new-vectori-word 1))
(defun vectori-long n (vector-macro new-vectori-long 2))

;--- vref
; refernces an element of a vector
;   (vref 'vect 'index)
;
(defmacro vref-macro (vector index predicate limit class)
   `(cond ((not (,predicate ,vector))
	  ,(cond ((eq predicate 'vector)
		  `(error "vref: non vector argument " ,vector))
		 (t `(error "vref: non vectori argument " ,vector))))
	 ((not (fixp ,index))
	  (error "vref: non fixnum index " ,index))
	 ((or (< ,index 0) (not (< ,index ,limit)))
	  (error "vref: index out of range " ,index ,vector))
	 (t (int:vref ,vector ,index ,class))))

(defun vref (vect ind)
   (vref-macro vect ind vectorp (vsize vect) 3))

(defun vrefi-byte (vect ind)
   (vref-macro vect ind vectorip (vsize-byte vect) 0))
(defun vrefi-word (vect ind)
   (vref-macro vect ind vectorip (vsize-word vect) 1))
(defun vrefi-long (vect ind)
   (vref-macro vect ind vectorip (vsize vect) 2))


;--- vset
; use:
;	(vset 'vector 'index 'value)
;
(defmacro vset-macro (vector index value predicate limit class)
   `(cond ((not (,predicate ,vector))
	  ,(cond ((eq predicate 'vector)
		  `(error "vset: non vector argument " ,vector))
		 (t `(error "vset: non vectori argument " ,vector))))
	 ((not (fixp ,index))
	  (error "vset: non fixnum index " ,index))
	 ((or (<& ,index 0) (not (<& ,index ,limit)))
	  (error "vset: index out of range " ,index ,vector))
	 (t (int:vset ,vector ,index ,value ,class))))

(defun vset (vect ind val)
   (vset-macro vect ind val vectorp (vsize vect) 3))

(defun vseti-byte (vect ind val)
   (vset-macro vect ind val vectorip (vsize-byte vect) 0))

(defun vseti-word (vect ind val)
   (vset-macro vect ind val vectorip (vsize-word vect) 1))

(defun vseti-long (vect ind val)
   (vset-macro vect ind val vectorip  (vsize vect) 2))


;;; vector sizes

;--- vsize :: size of vector viewed as vector of longwords
;
(defun vsize (vector)
   (if (or (vectorp vector) (vectorip vector))
      then (int:vsize vector 2)
      else (error "vsize: non vector argument " vector)))

(defun vsize-word (vectori)
   (if (vectorip vectori)
      then (int:vsize vectori 1)
      else (error "vsize-word: non vectori argument " vectori)))

(defun vsize-byte (vectori)
   (if (vectorip vectori)
      then (int:vsize vectori 0)
      else (error "vsize-byte: non vectori argument " vectori)))

;; vector property list functions
;;
(defun vget (vector ind)
   (let ((x (vprop vector)))
      (if (dtpr x)
	 then (get x ind))))

;--- vputprop :: store value, indicator pair on property list
; if a non-dtpr is already there,  make it the car of the list
;
(defun vputprop (vector value ind)
   (let ((x (vprop vector)))
      (if (not (dtpr x))	
	 then (setq x (ncons x))
	      (vsetprop vector x))
      (putprop x value ind)))

	     
;; closures
;
;- closures are implemented in terms of vectors so we'll store the
; code here for now
;  a closure is a vector with leader field eq to 'closure'
; the 0th element of a closure vector is the functional form
; to funcall
; then the elements go in triplets
;			1 is the symbol name
;			    either
;	2 is nil 			2 is a pointer to a vector
;	3 is the saved value		3 is a fixnum index into the vector
;	 ^				   ^
;	 |---- the simple case		   |-- when we are sharing a value
;					       slot, this points to the
;					       value slot
;
; the size of the vector tells the number of variables.
;

;--- closure :: make a closure
; form (closure 'l_vars 'g_fcn)
; l_vars is a list of symbols
; g_fcn is a functional form, either a symbol or a lambda expression
; alist is a list of what has been already stored so far.
;   it will always be non nil, so we can nconc to it to return values.
;
(defun make-fclosure-with-alist (vars fcn alist)
   (cond ((not (or (null vars) (dtpr vars)))
	  (error "fclosure: vars list has a bad form " vars)))
   
   (let ((vect (new-vector (1+ (length vars)) nil 'fclosure)))
      (do ((xx vars (cdr xx))
	   (val)
	   (sym)
	   (i 1 (1+ i)))
	  ((null xx)
	   (setf (vref vect 0) fcn)	; store the function to call
	   vect)
	  (setq sym (car xx))
	  (cond ((not (symbolp sym))
		 (error "fclosure: non symbol in var list " sym)))

	  ; don't allow the variable nil to be closed over
	  (cond ((null sym)
		 (error "fclosure: you can't close over nil " vars)))

	  ; if the fclosure variable has already been given slot, use
	  ; it, else make a new one
	  (cond ((null (setq val (assq sym alist)))
		   ; if the variable is bound use it's current value,
		   ; else use nil
		   (cond ((setq val (boundp sym))
			  (setq val (cdr val))))
		   ; generate a new closure variable object
		   (setq val (cons sym (cons val (copyint* 0))))
		   ; remember this value for later fclosures
		   (nconc alist (list val))))
	  (setf (vref vect i) val))))
   


;--- fclosure :: generate a simple fclosure
; 
(defun fclosure (vars func)
   (make-fclosure-with-alist vars func (list nil)))

(defun fclosure-list n
   (cond ((not (evenp n))
	  (error "fclosure-alist: not given an even number of arguments: "
		 (listify n))))
   (do ((i 1 (+ i 2))
	(alist (list nil))
	(res))
       ((> i n) (nreverse res))
       (push (make-fclosure-with-alist (arg i) (arg (1+ i)) alist) res)))

(defun fclosurep (fclosure)
   (and (vectorp fclosure)
	(eq 'fclosure (vprop fclosure))))
(defun fclosure-alist (fclosure)
   (cond ((fclosurep fclosure)
	  (do ((xx 1 (1+ xx))
	       (lim  (vsize fclosure))
	       (val)
	       (res))
	      ((not (< xx lim))
	       res)
	      (setq val (vref fclosure xx))
	      (push (cons (car val) (cadr val)) res)))
	 (t (error "fclosure-alist: non fclosure argument: " fclosure))))



(defun fclosure-function (fclosure)
   (and (fclosurep fclosure)
	(vref fclosure 0)))

(defun vector-dump (vect)
   (let (size)
      (msg "size = " (setq size (vsize vect)) ", prop= " (vprop vect) N)
      (do ((ii 0 (1+ ii)))
	  ((not (< ii size)))
	  (msg ii ": " (vref vect ii) N ))))
   
	
;--- symeval-in-fclosure :: determine the value of a symbol
;   with respect to an fclosure.
;
(defun symeval-in-fclosure (fclosure symbol)
  (cond ((not (fclosurep fclosure))
	 (error "set-in-fclosure: non fclosure first argument: " fclosure))
	(t (do ((xx 1 (1+ xx))
		(val)
		(lim (vsize fclosure)))
	       ((not (< xx lim))
		(error "symeval-in-fclosure: variable not found" symbol))
	       (setq val (vref fclosure xx))
	       (cond ((eq symbol (car val))
		      (return (int:fclosure-stack-stuff val))))))))

;--- set-in-fclosure :: set the value of a symbol in an fclosure
;
(defun set-in-fclosure (fclosure symbol value)
  (cond ((not (fclosurep fclosure))
	 (error "set-in-fclosure: non fclosure first argument: " fclosure))
	(t (do ((xx 1 (1+ xx))
		(val)
		(lim (vsize fclosure)))
	       ((not (< xx lim))
		(error "set-in-fclosure: variable not found" symbol))
	       (setq val (vref fclosure xx))
	       (cond ((eq symbol (car val))
		      (return (int:fclosure-stack-stuff val value))))))))

(defmacro let-fclosed (vars function)
  `(let ,vars (fclosure ',(mapcar #'(lambda (x) (if (atom x) x (car x))) vars)
			,function)))
		

