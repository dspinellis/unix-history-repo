;; vector handling functions	-[Sun Apr 25 10:25:11 1982 by jkf]-
;;
;; preliminary.  this is subject to change at any moment.
;; Don't use the functions in this file!!	--jkf
;;
;; contains functions:
;;  vector{,-byte,-word,-long}   : create and initialize
;;  vref{,-byte,-word,-long}	 : reference
;;  vset{,-byte,-word,-long}	 : set
;;
;; references external functions
;;  new-vector{,-byte,-word,-long
;;  vsize
;;
;; references internal functions:
;;  int:vref 'vect 'index 'class
;;  int:vset 'value 'vect 'index 'class
;;

;--- vector
;  call is (vector elmt0 elmt1 ... elmtn)
; creates an n-1 size vector and initializes
;
(defmacro vector-macro (create class)
   `(let ((vec (,create n)))
      (do ((from n to)
	   (to (1- n) (1- to)))
	  ((< to 0))
	  (int:vset (arg from) vec to ,class))
      vec))

(defun vector n (vector-macro new-vector 3))
(defun vector-byte n (vector-macro new-vector-byte 0))
(defun vector-word n (vector-macro new-vector-word 1))
(defun vector-long n (vector-macro new-vector-long 2))

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

(defun vref-byte (vect ind)
   (vref-macro vect ind vectorip (* (vsize vect) 4) 0))
(defun vref-word (vect ind)
   (vref-macro vect ind vectorip (* (vsize vect) 2) 1))
(defun vref-long (vect ind)
   (vref-macro vect ind vectorip (vsize vect) 2))


;--- vset
; use:
;	(vset 'vector 'index 'value)
;
(defmacro vset-macro (value vector index predicate limit class)
   `(cond ((not (,predicate ,vector))
	  ,(cond ((eq predicate 'vector)
		  `(error "vset: non vector argument " ,vector))
		 (t `(error "vset: non vectori argument " ,vector))))
	 ((not (fixp ,index))
	  (error "vset: non fixnum index " ,index))
	 ((or (< ,index 0) (not (< ,index ,limit)))
	  (error "vset: index out of range " ,index ,vector))
	 (t (int:vset ,value ,vector ,index ,class))))

(defun vset (vect ind val)
   (vset-macro val vect ind vectorp (vsize vect) 3))

(defun vset-byte (vect ind val)
   (vset-macro val vect ind vectorip (* (vsize vect) 4) 0))

(defun vset-word (vect ind val)
   (vset-macro val vect ind vectorip (* (vsize vect) 2) 1))

(defun vset-long (vect ind val)
   (vset-macro val vect ind vectorip  (vsize vect) 2))
