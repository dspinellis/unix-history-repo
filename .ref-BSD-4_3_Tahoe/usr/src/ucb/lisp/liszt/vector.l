(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file vector
   "$Header: vector.l,v 1.12 87/12/15 17:10:04 sklower Exp $")

;;; ----	v e c t o r			vector referencing
;;;
;;;				-[Fri Nov 11 22:35:50 1983 by jkf]-


(defun cc-vset ()
   ;;  Set a vector created via 'vector'.
   (d-vset 'lisp))

(defun cc-vref ()
   ;;  Reference a vector created via 'vector'.
   (d-vref 'lisp))

(defun cc-vseti-byte ()
   ;;  Set a vector created via 'vectori-byte'.
   (d-vset 'byte))

(defun cc-vrefi-byte ()
   ;;  Reference a vector created via 'vectori-byte'.
   (d-vref 'byte))

(defun cc-vseti-word ()
   ;;  Set a vector created via 'vectori-word'.
   (d-vset 'word))

(defun cc-vrefi-word ()
   ;;  Reference a vector created via 'vectori-word'.
   (d-vref 'word))

(defun cc-vseti-long ()
   ;;  Set a vector created via 'vectori-long'.
   (d-vset 'long))

(defun cc-vrefi-long ()
   ;;  Reference a vector created via 'vectori-long'.
   (d-vref 'long))

;--- d-vset :: handle all types of vset's
(defun d-vset (type)
   ;;  Generic vector store.  Type is either 'lisp', 'byte', 'word',
   ;; or 'long'.
   (let ((vect (cadr v-form))
	 (index (caddr v-form))
	 (val (cadddr v-form))
	 (vect-addr) (index-addr)
	 (vect-val) (fetchval)
	 (temp) (size)
	 (vec-reg #+(or for-vax for-tahoe) 'r0 #+for-68k 'a0)
	 (val-reg #+(or for-vax for-tahoe) 'r1 #+for-68k 'd1)
	 (index-reg '#.fixnum-reg)
	 (temp-reg #+(or for-vax for-tahoe) 'r4 #+for-68k 'd0)
	 (temp-areg #+(or for-vax for-tahoe) 'bogus! #+for-68k 'a1)
	 (oklab (d-genlab))
	 (needlowcheck t))		; t if must check lower index bounds

       #+for-68k (d-regused '#.fixnum-reg)
       (makecomment `(doing vec set type ,type))
       (if (fixp index)
	   then (if (<& index 0)
		    then (comp-err "vector index less than 0 " v-form))
		(setq needlowcheck nil))

       ; Compute the value to be stored...
       ;
       ; If we are doing an immediate vector, then get the value
       ; instead of the boxed fixnum (in the case of byte), or
       ; word/long.
       (if (null (eq 'lisp type)) then (setq val `(cdr ,val)))

       (if (null (setq vect-val (d-simple val)))
	   then (let ((g-loc val-reg) g-cc g-ret)
		    (d-exp val))
		(setq vect-val val-reg)
	   else (setq vect-val (e-cvt vect-val)))

       ; make sure that we are not going to clobber val-reg...
       (if (not (and (d-simple vect) (d-simple index)))
	   then ; val-reg could be clobbered when we do the
		; fetching of the vector or index values
		(setq fetchval t)
		(e-move vect-val (e-cvt 'stack)))

       ; Compute the index...
       ;
       (if (setq index-addr (d-simple index))
	   then (let ((g-loc vec-reg) g-cc g-ret)
		    (d-exp vect))
		(setq vect-addr vec-reg)	; the vector op is in vec-reg
		; we really want the cdr of index (the actual number).
		; if we can do that simply, great.  otherwise we
		; bring the index into index-reg and then do the cdr ourselves
		(if (setq temp (d-simple `(cdr ,index)))
		    then (d-move temp index-reg)
		    else (d-move index-addr index-reg)
			 #+(or for-vax for-tahoe)
			 (e-move `(0 ,index-reg) index-reg)
			 #+for-68k
			 (progn
			     (e-move index-reg 'a5)
			     (e-move '(0 a5) index-reg)))
		(setq index-addr index-reg)
	   else ; the index isn't computable simply, so we must
		; stack the vector location to keep it safe
		(let ((g-loc 'stack) g-cc g-ret)
		    (d-exp vect))
		(push nil g-locs)
		(incr g-loccnt)
		; compute index's value into index-reg
		(d-fixnumexp index)
		; now put vector address into vec-reg
		(d-move 'unstack vec-reg)
		(decr g-loccnt)
		(pop g-locs)
		(setq vect-addr vec-reg
		      index-addr index-reg)
		; must be sure that the cc's reflect the value of index-reg
		(e-tst index-reg))

       ;   At this point, vect-addr (always vec-reg) contains the location of
       ; the start of the vector,  index-addr (always index-reg) contains
       ; the index value.
       ;   The condition codes reflect the value of the index.
       ; First we insure that the index is non negative
       ; test must use a jmp in case the object file is large
       ;
       (if needlowcheck
	   then (e-write2 #+(or for-vax for-tahoe) 'jgeq #+for-68k 'jpl oklab)
		(e-write2 'jmp 'vecindexerr)
		(e-label oklab)
		(setq oklab (d-genlab)))
       ;; now, we compare against the size of the vector
       ;; the size of the vector is in bytes, we may want to shift this
       ;; to reflect the size in words or longwords, depending on the
       ;; type of reference
       (if (eq type 'byte)
	   then ; can compare right away
		(e-cmp index-addr `(-8 ,vect-addr))
	   else ; shift size into temp-reg
		(setq size (if (eq type 'word) then 1 else 2))
		#+for-vax
		    (e-write4 'ashl (concat '$- size)
			      `(-8 ,vect-addr) temp-reg)
		#+for-tahoe
		    (e-write4 'shar (concat '$ size)
			      `(-8 ,vect-addr) temp-reg)
		#+for-68k
		(progn
		    (e-move `(-8 ,vect-addr) temp-reg)
		    (e-write3 'asrl `($ ,size) temp-reg))
		(e-cmp index-addr temp-reg)
		(d-clearreg temp-reg))
       ;; size is the number of objects, the index is 0 based so
       ;; it must be less than the vector size
       (e-write2 #+(or for-vax for-tahoe) 'jlss #+for-68k 'jmi oklab)
       (e-write2 'jmp 'vecindexerr)
       (e-label oklab)

       (if fetchval
	   then ; unstack the value to store...
		(e-move (e-cvt 'unstack) val-reg)
		(setq vect-val val-reg))

       ;; if we get here then the access is in bounds
       (if (eq type 'lisp)
	   then #+(or for-vax for-tahoe)
		(e-move vect-val `(0 ,vect-addr ,index-addr))
		#+for-68k
		(progn
		    (e-move index-addr temp-reg)
		    (e-write3 'asll '($ 2) temp-reg)
		    (e-add vect-addr temp-reg)
		    (e-move temp-reg temp-areg)
		    (e-move vect-val `(0 ,temp-areg)))
		(if g-loc (e-move vect-val (e-cvt g-loc)))
		(if g-cc then (d-handlecc))
	   else (setq temp (cadr (assq type '((byte movb)
					      (word movw)
					      (long movl)))))
		#+(or for-vax for-tahoe)
		(e-write3 temp vect-val `(0 ,vect-addr ,index-addr))
		#+for-68k
		(progn
		    (e-move index-addr temp-reg)
		    (caseq type
			(word (e-write3 'asll '($ 1) temp-reg))
			(long (e-write3 'asll '($ 2) temp-reg)))
		    (e-write3 'lea `(% 0 ,vec-reg ,temp-reg) temp-areg)
		    (if (eq type 'long)
			then (e-write3 temp vect-val `(0 ,temp-areg))
			else (e-move vect-val 'd1)
			     (e-write3 temp 'd1 `(0 ,temp-areg))))
		(if g-loc
		    then (if (eq type 'byte)
			     then ; all bytes values are within the fixnum
				  ; range, we convert them to immediate
				  ; fixum with ease.
				  #+for-vax
				  (progn
				      (e-write4 'ashl '($ 2)
						index-reg index-reg)
				      (e-write3 'movab
						`(5120 ,index-reg)
						(e-cvt g-loc)))
				  #+for-tahoe
				  (progn
				      (e-write4 'shal '($ 2)
						index-reg index-reg)
				      (e-write3 'movab
						`(5120 ,index-reg)
						(e-cvt g-loc)))
				  #+for-68k
				  (progn
				      (e-move index-reg temp-reg)
				      (e-write3 'asll '($ 2) temp-reg)
				      (e-move temp-reg temp-areg)
				      (e-move
						(e-cvt '(fixnum 0))
						temp-reg)
				      (e-write3 'lea
						`(% 0 ,temp-areg ,temp-reg)
						temp-areg)
				      (e-move
						temp-areg
						(e-cvt g-loc)))
			     else ; must convert the hard way
				  (e-call-qnewint)
				  (d-clearreg)
				  (if (not (eq g-loc 'reg))
				      then (d-move 'reg g-loc)))
			 ; result is always non nil
			 (if (car g-cc) then (e-goto (car g-cc)))
		 elseif (car g-cc) then (e-goto (car g-cc))))
       (d-vectorindexcode)))

;--- d-vref :: handle all types of vref's
(defun d-vref (type)
   ;;  Generic vector reference.  Type is either 'lisp', 'byte', 'word',
   ;; or 'long'.
   (let ((vect (cadr v-form))
	 (index (caddr v-form))
	 (vect-addr) (index-addr) (temp) (size)
	 (vec-reg #+(or for-vax for-tahoe) 'r0 #+for-68k 'a0)
	 (index-reg '#.fixnum-reg)
	 (temp-reg #+(or for-vax for-tahoe) 'r4 #+for-68k 'd0)
	 (temp-areg #+(or for-vax for-tahoe) 'rX #+for-68k 'a1)
	 (oklab (d-genlab))
	 (needlowcheck t))  ; t if must check lower index bounds

       #+for-68k (d-regused '#.fixnum-reg)
       (makecomment `(doing vec ref type ,type))
       (if (fixp index)
	   then (if (<& index 0)
		    then (comp-err "vector index less than 0 " v-form))
		(setq needlowcheck nil))

       (if (setq index-addr (d-simple index))
	   then (let ((g-loc vec-reg) g-cc g-ret)
		    (d-exp vect))
		(setq vect-addr vec-reg)	; the vector op is in vec-reg
		; we really want the cdr of index (the actual number).
		; if we can do that simply, great.  otherwise we
		; bring the index into index-reg and then do the cdr ourselves
		(if (setq temp (d-simple `(cdr ,index)))
		    then (d-move temp index-reg)
		    else (d-move index-addr index-reg)
			 #+(or for-vax for-tahoe)
			 (e-move `(0 ,index-reg) index-reg)
			 #+for-68k
			 (progn
			     (e-move index-reg 'a5)
			     (e-move '(0 a5) index-reg)))
		(setq index-addr index-reg)
	   else ; the index isn't computable simply, so we must
		; stack the vector location to keep it safe
		(let ((g-loc 'stack) g-cc g-ret)
		    (d-exp vect))
		(push nil g-locs)
		(incr g-loccnt)
		; compute index's value into index-reg
		(d-fixnumexp index)
		; now put vector address into vec-reg
		(d-move 'unstack vec-reg)
		(decr g-loccnt)
		(pop g-locs)
		(setq vect-addr vec-reg
		      index-addr index-reg)
		; must be sure that the cc's reflect the value of index-reg
		(e-tst index-reg))
       
       ; at this point, vect-addr (always vec-reg) contains the location of
       ; the start of the vector,  index-addr (always index-reg) contains
       ; the index value.  the condition codes reflect the value of
       ; the index
       ; First we insure that the index is non negative
       ; test must use a jmp in case the object file is large
       (if needlowcheck
	   then (e-write2 #+(or for-vax for-tahoe) 'jgeq #+for-68k 'jpl oklab)
		(e-write2 'jmp 'vecindexerr)
		(e-label oklab)
		(setq oklab (d-genlab)))

       ; now, we compare against the size of the vector
       ; the size of the vector is in bytes, we may want to shift this
       ; to reflect the size in words or longwords, depending on the
       ; type of reference
       (if (eq type 'byte)
	   then ; can compare right away
		(e-cmp index-addr `(-8 ,vect-addr))
	   else ; shift size into temp-reg
		(setq size (if (eq type 'word) then 1 else 2))
		#+for-vax
		(e-write4 'ashl (concat '$- size) `(-8 ,vect-addr) temp-reg)
		#+for-tahoe
		(e-write4 'shar (concat '$ size) `(-8 ,vect-addr) temp-reg)
		#+for-68k
		(progn
		    (e-move `(-8 ,vect-addr) temp-reg)
		    (e-write3 'asrl `($ ,size) temp-reg))
		(e-cmp index-addr temp-reg)
		(d-clearreg temp-reg))
       ; size is the number of objects, the index is 0 based so
       ; it must be less than the vector size
       (e-write2 #+(or for-vax for-tahoe) 'jlss #+for-68k 'jmi oklab)
       (e-write2 'jmp 'vecindexerr)
       (e-label oklab)

       ;; if we get here then the access is in bounds
       (if g-loc
	   then ; we care about the value.
		; if the value is one of the fixnum types, then we
		; move the value to index-reg so it can be fixnum converted
		(if (eq type 'lisp)
		    then #+(or for-vax for-tahoe)
			 (e-move `(0 ,vect-addr ,index-addr)
				   (e-cvt g-loc))
			 #+for-68k
			 (progn
			     (e-move index-addr temp-reg)
			     (e-write3 'asll '($ 2) temp-reg)
			     (e-add vect-addr temp-reg)
			     (e-move temp-reg temp-areg)
			     (e-move `(0 ,temp-areg) (e-cvt g-loc)))
			 (if g-cc then (d-handlecc))
		    else #+(or for-vax for-tahoe)
			 (progn
			     (setq temp (cadr (assq type '((byte cvtbl)
							   (word cvtwl)
							   (long movl)))))
			     (e-write3 temp
				       `(0 ,vect-addr ,index-addr)
				       index-reg))
			 #+for-68k
			 (progn
			     (setq temp
				   (cadr (assq type '((byte movb)
						      (word movw)
						      (long movl)))))
			     (caseq type
				    (word (e-write3 'asll '($ 1) index-reg))
				    (long (e-write3 'asll '($ 2) index-reg)))
			     (e-write3 'lea `(% 0 ,vec-reg ,index-reg)
				       temp-areg)
			     (if (memq type '(byte word))
				 then (e-write2 'clrl index-reg))
			     (e-write3 temp `(0 ,temp-areg) index-reg))
			 (if (eq type 'byte)
			     then ; all bytes values are within the fixnum
				  ; range, we convert them to immediate
				  ; fixum with ease.
				  #+for-vax
				  (progn
				      (e-write4 'ashl '($ 2)
						index-reg index-reg)
				      (e-write3 'movab
						`(5120 ,index-reg)
						(e-cvt g-loc)))
				  #+for-tahoe
				  (progn
				      (e-write4 'shal '($ 2)
						index-reg index-reg)
				      (e-write3 'movab
						`(5120 ,index-reg)
						(e-cvt g-loc)))
				  #+for-68k
				  (progn
				      (e-write3 'asll '($ 2) index-reg)
				      (e-move index-reg temp-areg)
				      (e-move
						'($ _nilatom+0x1400)
						temp-reg)
				      (e-write3 'lea
						`(% 0 ,temp-areg ,temp-reg)
						temp-areg)
				      (e-move
						temp-areg
						(e-cvt g-loc)))
			     else ; must convert the hard way
				  (e-call-qnewint)
				  (d-clearreg)
				  (if (not (eq g-loc 'reg))
				      then (d-move 'reg g-loc)))
			 ; result is always non nil
			 (if (car g-cc) then (e-goto (car g-cc))))
	elseif g-cc
	     ; we dont care about the value, just whether it nil
	   then (if (eq type 'lisp)
		    then #+(or for-vax for-tahoe)
			 (e-tst `(0 ,vect-addr ,index-addr))
			 #+for-68k
			 (progn
			     (e-move index-addr temp-reg)
			     (e-write3 'asll '($ 2) temp-reg)
			     (e-add vect-addr temp-reg)
			     (e-move temp-reg temp-areg)
			     (e-cmpnil `(0 ,temp-areg)))
			 (d-handlecc)
		    else ; if fixnum, then it is always true
			 (if (car g-cc) then (e-goto (car g-cc)))))
       (d-vectorindexcode)))

;--- d-vectorindexcode :: put out code to call the vector range error.
; At this point the vector is in r0, the index an immediate fixnum in r5
; we call the function int:vector-range-error with two arguments, the
; vector and the index.
;
(defun d-vectorindexcode ()
   (if (null g-didvectorcode)
      then (let ((afterlab (d-genlab)))
	      (e-goto afterlab)
	      (e-label 'vecindexerr)
	      (d-move #+(or for-vax for-tahoe) 'r0 #+for-68k 'a0 'stack)
	      (e-call-qnewint)
	      (d-move 'reg 'stack)
	      (d-calltran 'int:vector-range-error 2)
	      ; never returns
	      (e-label afterlab))
	   (setq g-didvectorcode t)))


;------------------------ vector access functions

;--- cc-vectorp :: check for vectorness
;
(defun cc-vectorp nil
  (d-typesimp (cadr v-form) #.(immed-const 18)))

;--- cc-vectorip :: check for vectoriness
;
(defun cc-vectorip nil
  (d-typesimp (cadr v-form) #.(immed-const 19)))

;--- c-vsize :: extract vsize
;
(defun c-vsize nil
   (d-vectorsize (cadr v-form) '2))

(defun c-vsize-byte nil
   (d-vectorsize (cadr v-form) '0))

(defun c-vsize-word nil
   (d-vectorsize (cadr v-form) '1))

(defun d-vectorsize (form shift)
   (let ((g-loc #+(or for-vax for-tahoe) 'reg #+for-68k 'a0)
	 g-cc
	 g-ret)
       (d-exp form))
   ; get size into `fixnum-reg' for fixnum boxing
   (if (zerop shift)
       then (e-move '(-8 #+(or for-vax for-tahoe) r0 #+for-68k a0) '#.fixnum-reg)
       else #+for-vax
	    (e-write4 'ashl (concat '$- shift) '(-8 r0) '#.fixnum-reg)
	    #+for-tahoe
	    (e-write4 'shar (concat '$ shift) '(-8 r0) '#.fixnum-reg)
	    #+for-68k
	    (progn
		(e-move '(-8 a0) '#.fixnum-reg)
		(e-write3 'asrl `($ ,shift) '#.fixnum-reg)))
   (e-call-qnewint))
