(include-if (null (get 'chead 'version)) "chead.l")
(Liszt-file array
   "$Header: /na/franz/liszt/RCS/array.l,v 1.2 83/03/03 12:14:13 jkf Exp $")

;;; ----	a r r a y			array referencing
;;;


;--- d-handlearrayref :: general array handler
; this function is called from d-exp when the car is an array (declare macarray)
; In the current array scheme, stores look like array references with one
; extra argument. Thus we must determine if we are accessing or storing in
; the array.
; Note that we must turn g-loc to reg and g-cc to nil since, even though
; d-supercxr handles g-loc and g-cc, d-superrplacx does not and we cannot
; know ahead of time which one we will use.  If this seems important,
; we can beef up d-superrplacx
;
;.. d-exp
(defun d-handlearrayref nil
  (let ((spec (get (car v-form) g-arrayspecs))
	expr
	(g-loc 'reg)  g-cc)

       (makecomment '(array ref))
       (If (eq (1+ (length (cdr spec))) (length (cdr v-form)))
	   then (d-dostore spec (cadr v-form) (cddr v-form))
	   else (setq expr (d-arrayindexcomp (cdr v-form) (cdr spec)))

	        (let ((v-form `(cxr ,expr (getdata (getd ',(car v-form))))))
		     (d-supercxr (car spec) nil)))))


;--- d-dostore :: store value in array.
;	spec - array descriptor from declare, e.g. (foo t 12 3 4)
;	value - expression to calculate value to be stored.
;	indexes - list of expressions which are the actual indicies.
;
;.. d-handlearrayref
(defun d-dostore (spec value indexes)
  (let (expr gen)
       (makecomment '(doing store))
       ; create an expression for doing index calculation.
       (setq expr (d-arrayindexcomp indexes (cdr spec))
	     gen  (gensym))

       ; calculate value to store and stack it.
       (d-pushargs (ncons value))
       (rplaca g-locs gen)	; name just stacked varib

       ; do the store operation.
       (let ((v-form `(rplacx ,expr (getdata (getd ',(car v-form)))
			      ,gen)))
	    (d-superrplacx (car spec)))

       ; move the value we stored into r0
       (d-move 'unstack 'reg)
       (setq g-locs (cdr g-locs))
       (decr g-loccnt)))




;.. d-dostore, d-handlearrayref
(defun d-arrayindexcomp (actual formal)
  (If (null (cdr actual))
      then (car actual)	; always allow one arg
   elseif  (eq (length actual) (length formal))
      then (do ((ac actual (cdr ac))
		(fo formal (cdr fo))
		(res))
	       ((null ac) (cons '+ res))
	       (setq res (cons (If (null (cdr fo)) then (car ac)
				   else `(* ,(car ac) ,(apply 'times (cdr fo))))
			       res)))
   else (comp-err "Wrong number of subscripts to array " actual)))



;;;  vector handling functions

(defun cc-vref ()
   (d-vref 'lisp))

(defun cc-vrefi-byte ()
   (d-vref 'byte))

(defun cc-vrefi-word ()
   (d-vref 'word))

(defun cc-vrefi-long ()
   (d-vref 'long))

;--- d-vref :: handle all types of vref's
; type is  long,word,byte or lisp
;
(defun d-vref (type)
   (let ((vect (cadr v-form))
	 (index (caddr v-form))
	 (vect-addr) (index-addr) (temp) (size)
	 (needlowcheck t))  ; t if must check lower index bounds
      ; if the index is simple then we can just compute the vector
      ; location into r0
      (makecomment `(doing vec ref type ,type))
      (if (fixp index)
	 then (if (<& index 0)
		 then (comp-err "vector index less than 0 " v-form))
	      (setq needlowcheck nil))
      
      (if (setq index-addr (d-simple index))
	 then (let ((g-loc 'reg) g-cc g-ret)
		 (d-exp vect))
	      (setq vect-addr 'r0)	; the vector op is in r0
	      ; we really want the cdr of index (the actual number).
	      ; if we can do that simply, great.  otherwise we
	      ; bring the index into r5 and then do the cdr ourselves
	      (if (setq temp (d-simple `(cdr ,index)))
		 then (d-move temp 'r5)
		 else (d-move index-addr 'r5)
		      (e-write3 'movl '"(r5)" 'r5))
	      (setq index-addr 'r5)
	 else ; the index isn't computable simply, so we must
	      ; stack the vector location to keep it safe
	      (let ((g-loc 'stack) g-cc g-ret)
		 (d-exp vect))
	      (push nil g-locs)
	      (incr g-loccnt)
	      ; compute index's value into r5
	      (d-fixnumexp index)
	      ; now put vector address into r0
	      (d-move 'unstack 'r0)
	      (decr g-loccnt)
	      (pop g-locs)
	      (setq vect-addr 'r0  index-addr 'r5)
	      ; must be sure that the cc's reflect the value of r5
	      (e-write2 'tstl 'r5))
      ;; at this point, vect-addr (always r0) contains the location of
      ;; the start of the vector,  index-addr (always r5) contains
      ;; the index value.  the condition codes reflect the value of
      ;; the index
      ;; First we insure that the index is non negative
      ;; test must use a jmp in case the object file is large
      (if needlowcheck
	 then (e-write2 'jgeq "1f")
	      (e-write2 'jmp 'vecindexerr)
	      (e-label 1))
      ;; now, we compare against the size of the vector
      ;; the size of the vector is in bytes, we may want to shift this
      ;; to reflect the size in words or longwords, depending on the
      ;; type of reference
      (if (eq type 'byte)
	 then ; can compare right away
	      (e-write3 'cmpl index-addr (concat "-8(" vect-addr ")"))
	 else ; shift size into r4
	      (setq size (if (eq type 'word) then 1 else 2))
	      (e-write4 'ashl
			(concat '$- size)
			(concat "-8(" vect-addr ")")
			'r4)
	      (d-clearreg 'r4)
	      (e-write3 'cmpl index-addr 'r4))
      ;; size is the number of objects, the index is 0 based so
      ;; it must be less than the vector size
      (e-write2 'jlss "1f")
      (e-write2 'jmp 'vecindexerr)
      (e-label 1)
      ;; if we get here then the access is in bounds
      (if g-loc
	 then ; we care about the value.
	      ; if the value is one of the fixnum types, then we
	      ; move the value to r5 so it can be fixnum converted
	      (if (eq type 'lisp)
		 then (e-write3 'movl
				(concat "(" vect-addr ")"
					"[" index-addr "]")
				(e-cvt g-loc))
		      (if g-cc then (d-handlecc))
		 else (setq temp (cadr (assq type '((byte cvtbl)
						    (word cvtwl)
						    (long movl)))))
		      (e-write3 temp
				(concat "(" vect-addr ")"
					"[" index-addr "]")
			        'r5)
		      (if (eq type 'byte)
			 then ; all bytes values are within the fixnum
			      ; range, we convert them to immediate
			      ; fixum with ease.
			      (e-write4 'ashl '$2 'r5 'r5)
			      (e-write3 'movab "5120(r5)" (e-cvt g-loc))
			 else ; must convert the hard way
			      (e-write2 'jsb "_qnewint")
			      (d-clearreg)
			      (if (not (eq g-loc 'reg))
				 then (d-move 'reg g-loc)))
		      ; result is always non nil
		      (if (car g-cc) then (e-goto (car g-cc))))
       elseif g-cc
	    ; we dont care about the value, just whether it nil
	    then (if (eq type 'lisp)
		    then (e-write2 'tstl
				   (concat "(" vect-addr ")"
					   "[" index-addr "]"))
			 (d-handlecc)
		    else ; if fixnum, then it is always true
			 (if (car g-cc) then (e-goto (car g-cc)))))
      (d-vectorindexcode)))


;--- d-vectorindexcode :: put out code to call the vector of range error
; at this point the vector is in r0, the index an immediate fixnum in r5
; we call the function int:vector-range-error with two arguments, the
; vector and the index.
;
(defun d-vectorindexcode ()
   (if (null g-didvectorcode)
      then (let ((afterlab (d-genlab)))
	      (e-goto afterlab)
	      (e-label 'vecindexerr)
	      (d-move 'reg 'stack)
	      (e-write2 'jsb '_qnewint)
	      (d-move 'reg 'stack)
	      (d-calltran 'int:vector-range-error 2)
	      ; never returns
	      (e-label afterlab))
	   (setq g-didvectorcode t)))
   
	    
			      
			  
      
		    
