;;
;; array.l				-[Tue Jul  5 23:51:48 1983 by layer]-
;;
;; maclisp compatible array package.  This implements maclisp
;; compatible arrays.
;;
;; features of the new package:
;; Most array will be notype arrays.  This is because they are the most 
;; efficient in Franz.  What used to be fixnum and flonums arrays are
;; now fixnum-block and flonum-block arrays.
;; The array access functions are more specialized and much faster now.
;; The array access functions have different semantics.  Now they are
;; responsible for both accessing and storing in an array.
;; When an access function is asked to access a value, it will be given
;; the subscripts already evaluated and the array object.  These will
;; be stacked, so the array access function should be a lexpr to read them.
;; When an access function is asked to store a value that value will be
;; the first argument, the subscripts will follow and finally there will
;; be the array object.  
;; It is up to the access function to determine if it is being asked to
;; store or retrieve a value, and this determination will probably
;; be made by looking at the number of arguments.


(setq rcs-array-
   "$Header: array.l 1.5 83/07/05 23:51:58 layer Exp $")

(declare (special gcdisable)
   (macros t))

(def array
  (macro ($lis$)
	 `(*array ',(cadr $lis$) ',(caddr $lis$) ,@(cdddr $lis$))))

(def *array
  (lexpr (nargs)
	   (prog (name type rtype dims size tname numdims)

		 (cond ((lessp (setq numdims (- nargs 2)) 1)
			(error "no bounds to array declaration ")))

		 (setq name  (arg 1)
		       type  (arg 2)
		       rtype (cond ((memq type '(t nil fixnum flonum))
				    'value)
				   ((eq type 'fixnum-block)
				    'fixnum)
				   ((eq type 'flonum-block)
				    'flonum)
				   (t (error "array: bad type: " type)))
		       dims  (do ((i nargs (1- i))
				  (res nil (cons (arg i) res)))
				 ((eq i 2) res))

		       size  (apply 'times dims))

		 (cond ((null type) (setq type 'unmarked_array)))

		 ; we disable gc during the next calculation since
		 ; the data returned from small-segment is unprotected
		 ; and a gc would cause its data to be put on the 
		 ; free list.
		 (let ((gcdisable t))
		      (setq tname
			    (marray (small-segment rtype size)
				    (cond ((eq rtype 'value)
					   (cond ((eq numdims 1) 
						  (getd 'arrac-oneD))
						 ((eq numdims 2) 
						  (getd 'arrac-twoD))
						 (t (getd 'arrac-nD))))
					  (t (getd 'arrac-nD)))
				    (cons type dims)
				    size
				    (sizeof rtype))))
		 ; if type is fixnum or flonum
		 ; we must intialize to 0 or 0.0
		 (cond ((or (and (eq 'fixnum type)
				 (setq rtype 0))
			    (and (eq 'flonum type)
				 (setq rtype 0.0))
			    (and (or (status feature 68k)
				     (status feature for-68k))
				 (progn (setq rtype nil) t)))
			(do ((i size))
			    ((zerop i))
			    (set (arrayref tname (setq i (1- i))) rtype))))

		 (cond (name (putd name tname)))
		 (return tname))))

(defmacro arraycall (type array &rest indexes)
  `(funcall ,array ,@indexes))

;--- array-type :: return type of array
;
(defun array-type (arr)
   (cond ((not (arrayp arr)) (error "array-type: non array passed " arr))
	 (t (car (getaux arr)))))

; this is used by the old array scheme.  Keep this around until
; everything is recompiled

(defun ev-arraycall (type array indexes)
  (apply array indexes))


;;;---- array access functions.

; we first define a macro to evaluate a value cell.  In compiled code cdr
; is the fastest way to do this, in interpreted code the type checker 
; would not let us use cdr so we have to use eval.
(eval-when (compile)
  (defmacro value-eval (x) `(cdr ,x))  ; one level of indirection
  (defmacro simple-arrayref (arr ind) `(offset-cxr ,ind (getdata ,arr))))

(eval-when (eval)
  (defun value-eval (x) (eval x))
  (defun simple-arrayref (arr ind) (arrayref arr ind)))

;- one dimensional
(defun arrac-oneD n
  (cond ((eq n 2) (value-eval (simple-arrayref (arg 2) (arg 1))))
	((eq n 3) (set (simple-arrayref (arg 3) (arg 2)) (arg 1)))
	(t (error " wrong number of subscripts to array: " (arg n)))))

(defun arrac-twoD n
  (let ((aux (getaux (arg n))))
       (cond ((eq n 3)
	      (value-eval (simple-arrayref
			     (arg n)
			     (+ (* (arg 1) (caddr aux)) (arg 2)))))
	     ((eq n 4)
	      (set (simple-arrayref (arg n)
				    (+ (* (arg 2) (caddr aux)) (arg 3))) 
		   (arg 1)))
	     (t (error " wrong number of subscripts to array: " (arg n))))))

;-- n dimensional array access function.  
(defun arrac-nD n
  (let ((aux (getaux (arg n)))
	firstsub subs
	store
	(index 0))

       (setq subs (length (cdr aux)))
       (cond ((eq n (1+ subs))
	      (setq firstsub 1))
	     ((eq n (+ 2 subs))
	      (setq firstsub 2 store t))
	     (t (error "wrong number of subscripts to array: " (arg n))))

       (setq index (arg firstsub))
       (do ((bounds (cddr aux) (cdr bounds))
	    (i firstsub))
	   ((null bounds))
	   (setq index (+ (* index (car bounds)) (arg (setq i (1+ i))))))

       (setq subs (arrayref (arg n) index))	; get cell requested
       (cond ((memq (car aux) '(fixnum-block flonum-block))
	      (cond (store (replace subs (arg 1)))
		    (t (cpy1 subs))))
	     (t (cond (store (set subs (arg 1)))
		      (t (value-eval subs)))))))


(defmacro store ( (arrname . indexes) value)
 (do ((fnd))
     (nil)
   (cond ((eq 'funcall arrname)
	  (return `(funcall ,(car indexes) ,value . ,(cdr indexes))))
	 ((eq 'apply arrname)
	  (return `(apply ,(car indexes) (cons ,value ,@(cdr indexes)))))
	 ((eq 'arraycall arrname)
	  (return `(funcall ,(cadr indexes) ,value ,@(cddr indexes))))
	 ((arrayp arrname)
	  (return `(funcall ',arrname ,value ,@indexes))))
   (setq fnd (getd arrname))
   (cond ((or (and (dtpr fnd) (eq 'macro (car fnd)))
	      (and (bcdp fnd) (eq 'macro (getdisc fnd))))
	  (setq fnd (apply arrname (cons arrname indexes)))
	  (setq arrname (car fnd)
		indexes (cdr fnd)))
	 (t (return `(,arrname ,value . ,indexes))))))

;-- storeintern  -  there may be residual calls to storeintern from 
; old code, we handle it here.  this routine can be eliminated when
; code is recompiled

(defun storeintern (arrnam value indexes)
  (apply arrnam (cons value indexes)))

;--- small segment storage allocators.

; this function allocates segments of storage and attempt to use the whole
; block instead of throwing away what isnt used
;

(declare (special gcdisable))

(defun small-segment (type n)
  (prog (lastseg retv elementsize itemsperpage-1 gcdisable tmp)
	(setq gcdisable t) 	; its not a good idea to gc while we are
				; handling pointers to things segment returns.
	(desetq (elementsize . itemsperpage-1) (get 'segment-sizes type))
	(cond ((null elementsize) (error "small-segment: bad type " type)))
	(setq lastseg (get 'segment-types type))
	(cond ((and lastseg (not (lessp (car lastseg) n))))
	      (t ; must allocate a block of storage, want to the least number of
		 ; pages which includes n elements
		 ; there are elementsize elements per page, and 
		 ; itemsperpage-1 is the number of elements on a page minus 1 
		 (setq retv (boole 4 
				   (+ n itemsperpage-1) 
				   itemsperpage-1))  ; 4 is x & ~y
		 (setq lastseg (cons retv (maknum (segment type retv))))))
	(setq retv (cdr lastseg))
	(rplaca lastseg (- (car lastseg) n))
	(rplacd lastseg (+ (cdr lastseg) (* elementsize n)))
	(cond ((greaterp (car lastseg) 0)
	       (putprop 'segment-types lastseg type)
	       (cond ((null (setq tmp (get 'segment-arrays type)))
		      (putprop 'segment-arrays 
			       (setq tmp (marray nil nil nil nil nil))
			       type)))
	       (putdata tmp (fake (cdr lastseg)))
	       (putlength tmp (car lastseg))
	       (putdelta tmp elementsize))
	      (t  ; remove all counters since we no longer have any space
		  ; left and we can't have a zero length array
		  (remprop 'segment-types type)
		  (remprop 'segment-arrays type)))
	(return (fake retv))))

; data base for small-segment
(putprop 'segment-sizes '( 4 . 127) 'value)
(putprop 'segment-sizes '( 4 . 127) 'fixnum)
(putprop 'segment-sizes '( 8 . 63)  'flonum)


(def arraydims (lambda (arg) (cond ((symbolp arg) (getaux (getd arg)))
				   ((arrayp arg) (getaux arg))
				   (t (break '"non array arg to arraydims")))))

; fill array from list or array

(def fillarray
  (lambda (arr lis)
	  (prog (maxv typept)
		(cond ((symbolp arr) (setq arr (getd arr))))

		(cond ((symbolp lis)
		       (setq lis (getd lis))
		       (return (fillarrayarray arr lis)))

		      ((arrayp lis) (return (fillarrayarray arr lis))))

		(setq maxv (1- (getlength arr))
		      typept (cond ((memq (car (getaux arr))
					  '(t fixnum flonum unmarked_array))
				    t)
				   (t nil)))
		(do ((ls lis)
		     (i 0 (1+ i)))
		    ((>& i maxv))

		    (cond (typept (set (arrayref arr i) (car ls)))
			  (t (replace (arrayref arr i) (car ls))))

		    (cond ((cdr ls) (setq ls (cdr ls))))))))

(def fillarrayarray
  (lambda (arrto arrfrom)
	  (prog (maxv)
		(setq maxv (1- (min (getlength arrto)
				      (getlength arrfrom))))
		(do ((i 0 (1+ i)))
		    ((>& i maxv))
		    (replace (arrayref arrto i) (arrayref arrfrom i))))))

(def listarray
  (lexpr (n)
	 (prog (arr size typ ret newv)
	       (setq arr (arg 1))
	       (cond ((arrayp arr))
		     ((and (symbolp arr) (arrayp (setq arr (getd arr)))))
		     (t (error "Non array to listarray " arr)))
	       (setq size (cond ((eq n 2) (arg 2))
				(t (apply '* (cdr (arraydims arr))))))
	       (setq typ (car (getaux arr)))
	       (cond ((memq typ '(t fixnum flonum unmarked_array))
		      (setq typ t))
		     (t (setq typ nil)))
	       (do ((i (1- size) (1- i)))
		   ((lessp i 0))
		   (setq newv (arrayref arr i))
		   (setq ret (cons (cond (typ (eval newv))
					 (t (cpy1 newv)))
				   ret)))
	       (return ret))))
