(setq rcs-hash-
   "$Header: hash.l,v 1.2 85/03/24 11:36:16 sklower Exp $")

;                Aug 5, 1982
; (c) copyright 1982, Massachusetts Institute of Technology
;
;   Hash tables are basically just fast property lists. There are much the
; same access functions: puthash, gethash, and remhash. The syntax is 
; different though. For small lists property lists are probably what you 
; want but when the lists start to become large hash tables become
; infinitely better than property lists.

;; Current bugs:  hash-table-rehash and the equal version need to be 
;;		  rewritten.  There is no reason to write the array twice.

;   Note very carefully that the syntax is <puthash key value hash-table>,
; <gethash key hash-table>, and <remhash key hash-table>.

;   Before hash tables are used they have to be made i.e. you first do
; (setq myhash (make-hash-table)) then (puthash 'name 'joe myhash).
; Make-hash-table takes several alternating keywords and arguments
; the only one of which you will probably use is :size. So 
; (setq otherhash (make-hash-table ':size 20)) will make otherhash a
; hash table of length 20. If you know what the length of the hash table
; will be and it is greater than about 20 it is a good idea to specify
; the length so that hash-table-rehash will not need to be called.
; This will speed up puthashing considerably especially when the hash
; table is very large.
;   Keys must be eq, equal will not work.

#+Franz (environment-maclisp)

(defstruct (hash-table (:constructor make-hash-table-internal)
		       :named)
  (real-hash-table (new-vector 17)) ;where entries are stored
  (hash-table-fullness 0)      ; how many entries in table
  (rehash-after-n-misses 4)    ; when puthashing you rehash the table
                               ; if you miss this many times
  (hash-table-size 17)         ; how big the vector is
  (hash-table-rehash-size 1.5) ; factor to multiply by current size 
                               ; to the get new size of the vector
  (hash-table-rehash-function 'hash-table-rehash))

;   Make-hash-table makes a hash table. The vector that all the information
; is stored in is made nmiss larger than the apparent size of the hash
; table so that if you hash to a number close to the size of the table
; you do not miss right off the table. So that for example if you
; hash to the last element of the table and miss you are not aff the table.

(defun make-hash-table (&rest options &aux (size 8) (rhf 'hash-table-rehash)
			                   (rhs 1.5) (nmisses 4))
  (loop for (key option) on options by #'cddr
	do (selectq key
		    (:size (setq size option))
		    (:rehash-function (setq rhf option))
		    (:rehash-size (setq rhs option))
		    (otherwise
		     (ferror () "~S is not a valid hash table option"
			     key))))
      (setq size (hash-table-good-size (* size 2)))
      (make-hash-table-internal
         real-hash-table (new-vector (+ size nmisses))
	 hash-table-size size
	 rehash-after-n-misses nmisses
	 hash-table-rehash-size rhs
	 hash-table-rehash-function rhf))

(defun hash-table-good-size (size)
  (setq size (max (fix size) 17))        ;minimum size is 17
  (or (oddp size) (setq size (1+ size))) ; make it odd
  (do ()
      ((and (not (zerop (\ size 3)))     ; make it a semi-prime number
	    (not (zerop (\ size 5)))
	    (not (zerop (\ size 7))))
       size)
      (setq size (+ size 2))))

;; Using conses instead of putting increasing the size of the data table
;; by a factor of two, decreases the amount of storage required for a 
;; partially full hash table but can adversely affect the paging and 
;; caching behavior of the hash table.  Sometime, should meter this 
;; difference.  (A compactifying garbage collector could help.)

(defmacro make-hash-element (key value)	; creates a hash element
  `(cons ,key ,value))

(defmacro hash-key (element)	; the key given a hash element
  `(car ,element)) 

(defmacro hash-value (element)	; the value of a hash element
  `(cdr ,element))

(defmacro si:hash-code (hash-table key)	;hash code for key
  `(\ (maknum ,key) (hash-table-size ,hash-table)))

;   Gethash either returns the value associated with that key in that 
; hash table or nil if there is none.

(defun gethash (key hash-table &aux position-value)
  (do ((try-position (si:hash-code hash-table key) (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n)) 
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n) nil) ;it is not there so just return nil
    (cond ((eq key
	       (hash-key (setq position-value
			       (vref real-hash-table try-position))))
	     (return (hash-value position-value))))))

(eval-when (compile load eval)
  (defsetf gethash (e v) `(puthash ,(cadr e) ,v ,(caddr e))))

;   Puthash inserts a hash-element for the given key and value in the
; hash table that is passed to it. If the key already exists in the hash 
; table the value of that key is replaced by the new value. If it finds an
; empty space it adds a hash-element for that key and value into that 
; space and increments hash-table-fullness by one. If it cannot find
; the key or an empty space in four tries then it calls rehash on the
; hash table and tries again. 

(declare (localf puthash-internal))

(defun puthash (key value hash-table)
  (puthash-internal key value hash-table nil))

(defun swaphash (key value hash-table)
  (puthash-internal key value hash-table t))

(defun puthash-internal (key value hash-table swap?)
  (do ((try-position (si:hash-code hash-table key) (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n))
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n)  ;if cannot find a place in n tries then rehash     
       (funcall (hash-table-rehash-function hash-table)
		hash-table (hash-table-rehash-size hash-table))
       (puthash key value hash-table))
    (cond ((or (eq (hash-key (vref real-hash-table try-position))
		   key)
	       (and (null (vref real-hash-table try-position))
		    (setf (hash-table-fullness hash-table)
			    (1+ (hash-table-fullness hash-table)))))
	   (return
	    (prog1 (if swap? (hash-value (vref real-hash-table try-position))
		       value)
		   (setf (vref real-hash-table try-position)
			 (make-hash-element key value))))))))

;   Remhash removes the hash-element associated with the given key from
; the hash table that is passed to it. If it finds the element and removes
; it then it returns the key. If it cannot find the element then it returns
; nil.

(defun remhash (key hash-table)
  (do ((try-position (si:hash-code hash-table key) (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n))
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n) nil)   ;not in the hash table return nil
    (cond ((eq (hash-key (vref real-hash-table try-position)) key)
	   (setf (vref real-hash-table try-position) nil)
	   (return key))))) ;return the key if found and removed

;    Hash-table-rehash first saves the contents of the current hash table
; in a temporary vector then puthashes the elements of this temporary vector
; into the original hash-table after making it larger by a factor of
; the variable grow.

(defun hash-table-rehash (hash-table grow)
  (let* ((real-hash-table  (real-hash-table hash-table))
	 (nmisses (rehash-after-n-misses hash-table))
	 (new-size (+ nmisses
		      (hash-table-good-size (times grow
					      (hash-table-size hash-table)))))
	 (j 0)
	 (temp-array (new-vector new-size)))
    (do ((current-position 0 (1+ current-position))
	 (old-size (+ (hash-table-size hash-table) nmisses)))
	((>= current-position old-size))
	(let ((current-hash-element (vref real-hash-table current-position)))
	  (cond ((null current-hash-element))
		(t (setf (vref temp-array j) current-hash-element)
		   (setq j (1+ j))))))
    (cond ((not (= grow 1)) ;if the hash table has grown
           (setf (real-hash-table hash-table) (new-vector new-size))
	   (setf (hash-table-fullness hash-table) 0)
	   (setf (hash-table-size hash-table) (- new-size nmisses))))
    (do ((position 0 (1+ position))) ;add old values to new table
	((= position j))
	(puthash (hash-key (vref temp-array position))
		 (hash-value (vref temp-array position))
		 hash-table))))

(defun si:lookhash (hash-table)
  (let ((real-hash-table (real-hash-table hash-table)))
    (loop for num from 0 to (1- (vsize real-hash-table))
	  collect (vref real-hash-table num))))

(defun maphash (func hash-table)
  (let ((real-hash-table (real-hash-table hash-table)))
    (loop for num from 0 to (1- (vsize real-hash-table))
	  with keyword and value
	  do (setq keyword (vref real-hash-table num))
	  unless (null keyword)
	  do (progn (setq value (cdr keyword)
			  keyword (car keyword))
		    (funcall func keyword value)))))

;; SXHASH
;;  Sigh, this also comes from the LISP machine

(defun sxhash (x)
  (cond ((symbolp x)
	 (sxhash-string (get_pname x)))
	((stringp x)
	 (sxhash-string x))
	((eq (typep x) 'fixnum)
	 (if (minusp x)
	     (logxor x #o-1777777777)
	     x))
	((dtpr x)
	 (do ((rot 4)
	      (hash 0)
	      (y))
	     ((atom x)
	      (if (not (null x))
		  (setq hash (logxor (rot (sxhash x) (- rot 4)) hash)))
	      (if (minusp hash)
		  (logxor hash #o-1777777777)
		  hash))
	   (setq y (pop x))
	   (if (>= (setq rot (+ rot 7)) 24)
	       (setq rot (- rot 24)))
	   (setq hash (logxor (rot (cond ((symbolp y)
					  (sxhash-string (get_pname y)))
					 ((stringp y)
					  (sxhash-string y))
					 ((eq (typep y) 'fixnum)
					  y)
					 (t (sxhash y)))
				   rot)
			      hash))))
	((bigp x)
	 (sxhash (bignum-to-list x)))
	((floatp x)
	 (fix x))
	(t 0)))

(defun sxhash-string (string)
  (do ((i 1 (1+ i))
       (n (flatc string))
       (hash 0))
    ((> i n)
     (if (minusp hash)
	 (logxor hash #o-1777777777)
	 hash))
    (setq hash (rot (logxor (getcharn string i) #o177) 7))))

;; Equal hash tables

;; Notice the slots are exactly the same as in hash-table so we use the same
;; macros.

(defstruct (equal-hash-table (:constructor make-equal-hash-table-internal)
			     :named)
  (real-hash-table (new-vector 17)) ;where entries are stored
  (hash-table-fullness 0)      ; how many entries in table
  (rehash-after-n-misses 4)    ; when puthashing you rehash the table
                               ; if you miss this many times
  (hash-table-size 17)         ; how big the vector is
  (hash-table-rehash-size 1.5) ; factor to multiply by current size 
                               ; to the get new size of the vector
  (hash-table-rehash-function 'equal-hash-table-rehash))

;   Make-hash-table makes a hash table. The vector that all the information
; is stored in is made nmiss larger than the apparent size of the hash
; table so that if you hash to a number close to the size of the table
; you do not miss right off the table. So that for example if you
; hash to the last element of the table and miss you are not aff the table.

(defun make-equal-hash-table (&rest options &aux (size 8)
				           (rhf 'hash-table-rehash)
			                   (rhs 1.5) (nmisses 4))
  (loop for (key option) on options by #'cddr
	do (selectq key
		    (:size (setq size option))
		    (:rehash-function (setq rhf option))
		    (:rehash-size (setq rhs option))
		    (otherwise
		     (ferror () "~S is not a valid hash table option"
			     key))))
      (setq size (hash-table-good-size (* size 2)))
      (make-equal-hash-table-internal
         real-hash-table (new-vector (+ size nmisses))
	 hash-table-size size
	 rehash-after-n-misses nmisses
	 hash-table-rehash-size rhs
	 hash-table-rehash-function rhf))

;   Gethash-equal either returns the value associated with that key in that 
; hash table or nil if there is none.

(defun gethash-equal (key hash-table &aux position-value)
  (do ((try-position (remainder (sxhash key) (hash-table-size  hash-table))
		     (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n)) 
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n) nil) ;it is not there so just return nil
      (cond ((equal key
		    (hash-key (setq position-value
				    (vref real-hash-table try-position))))
	     (return (hash-value position-value))))))

(eval-when (eval compile load)
  (defsetf gethash-equal (e v) `(puthash-equal ,(cadr e) v ,(caddr e))))

;   Puthash inserts a hash-element for the given key and value in the
; hash table that is passed to it. If the key already exists in the hash 
; table the value of that key is replaced by the new value. If it finds an
; empty space it adds a hash-element for that key and value into that 
; space and increments hash-table-fullness by one. If it cannot find
; the key or an empty space in four tries then it calls rehash on the
; hash table and tries again. 

(declare (localf puthash-equal-internal))

(defun puthash-equal (key value hash-table)
  (puthash-equal-internal key value hash-table nil))

(defun swaphash-equal (key value hash-table)
  (puthash-equal-internal key value hash-table t))

(defun puthash-equal-internal (key value hash-table swap?)
  (do ((try-position (remainder (sxhash key) (hash-table-size hash-table))
		     (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n))
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n)  ;if cannot find a place in n tries then rehash     
       (funcall (hash-table-rehash-function hash-table)
		hash-table (hash-table-rehash-size hash-table))
       (puthash-equal key value hash-table))

      (cond ((or (equal (hash-key (vref real-hash-table try-position))
		     key)
		 (and (null (vref real-hash-table try-position))
		      (setf (hash-table-fullness hash-table)
			    (1+ (hash-table-fullness hash-table)))))
	     (return
	      (prog1 (if swap? (hash-value
				(vref real-hash-table try-position))
			 value)
		     (setf (vref real-hash-table try-position)
			   (make-hash-element key value))))))))


;   Remhash removes the hash-element associated with the given key from
; the hash table that is passed to it. If it finds the element and removes
; it then it returns the key. If it cannot find the element then it returns
; nil.

(defun remhash-equal (key hash-table)
  (do ((try-position (remainder (sxhash key) (hash-table-size hash-table))
		     (1+ try-position))
       (n (rehash-after-n-misses hash-table) (1- n))
       (real-hash-table (real-hash-table hash-table)))
      ((zerop n) nil)   ;not in the hash table return nil
      (cond ((equal (hash-key (vref real-hash-table try-position)) key)
	     (setf (vref real-hash-table try-position) nil)
	     (return key))))) ;return the key if found and removed


;    Hash-table-rehash first saves the contents of the current hash table
; in a temporary vector then puthashes the elements of this temporary vector
; into the original hash-table after making it larger by a factor of
; the variable grow.

(defun equal-hash-table-rehash (hash-table grow)
  (let* ((real-hash-table  (real-hash-table hash-table))
	 (nmisses (rehash-after-n-misses hash-table))
	 (new-size (+ nmisses
		      (hash-table-good-size (times grow
					      (hash-table-size hash-table)))))
	 (j 0)
	 (temp-array (new-vector new-size)))
    (do ((current-position 0 (1+ current-position))
	 (old-size (+ (hash-table-size hash-table) nmisses)))
	((>= current-position old-size))
	(let ((current-hash-element (vref real-hash-table current-position)))
	  (cond ((null current-hash-element))
		(t (setf (vref temp-array j) current-hash-element)
		   (setq j (1+ j))))))
    (cond ((not (= grow 1)) ;if the hash table has grown
           (setf (real-hash-table hash-table) (new-vector new-size))
	   (setf (hash-table-fullness hash-table) 0)
	   (setf (hash-table-size hash-table) (- new-size nmisses))))
    (do ((position 0 (1+ position))) ;add old values to new table
	((= position j))
	(puthash (hash-key (vref temp-array position))
		 (hash-value (vref temp-array position))
		 hash-table))))

(defun maphash-equal (func hash-table)
  (let ((real-hash-table (real-hash-table hash-table)))
    (loop for num from 0 to (1- (vsize real-hash-table))
	  with keyword and value
	  do (setq keyword (vref real-hash-table num))
	  unless (null keyword)
	  do (progn (setq value (cdr keyword)
			  keyword (car keyword))
		    (funcall func keyword value)))))

(sstatus feature hash-tables)
