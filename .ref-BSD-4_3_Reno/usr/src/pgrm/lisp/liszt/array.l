(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file array
   "$Header: array.l,v 1.7 83/08/28 17:12:39 layer Exp $")

;;; ----	a r r a y			array referencing
;;;
;;;				-[Sat Aug  6 23:59:45 1983 by layer]-


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
(defun d-handlearrayref nil
  (let ((spec (get (car v-form) g-arrayspecs))
	expr
	(g-loc 'reg)  g-cc)

       (makecomment '(array ref))
       (if (eq (1+ (length (cdr spec))) (length (cdr v-form)))
	   then (d-dostore spec (cadr v-form) (cddr v-form))
	   else (setq expr (d-arrayindexcomp (cdr v-form) (cdr spec)))

	        (let ((v-form `(cxr ,expr (getdata (getd ',(car v-form))))))
		     (d-supercxr (car spec) nil)))))


;--- d-dostore :: store value in array.
;	spec - array descriptor from declare, e.g. (foo t 12 3 4)
;	value - expression to calculate value to be stored.
;	indexes - list of expressions which are the actual indicies.
;
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




(defun d-arrayindexcomp (actual formal)
  (if (null (cdr actual))
      then (car actual)	; always allow one arg
   elseif  (eq (length actual) (length formal))
      then (do ((ac actual (cdr ac))
		(fo formal (cdr fo))
		(res))
	       ((null ac) (cons '+ res))
	       (setq res (cons (if (null (cdr fo)) then (car ac)
				   else `(* ,(car ac) ,(apply 'times (cdr fo))))
			       res)))
   else (comp-err "Wrong number of subscripts to array " actual)))
