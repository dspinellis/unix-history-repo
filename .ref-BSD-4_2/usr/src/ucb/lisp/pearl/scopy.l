;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scopy.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for copying structures in various ways.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Internal slot processor of SCOPY.
(dm scopyslot (none)
  '(progn
    (setq slotvalue (getslotvalue slotnum oldvalblock))
    (selectq (setq valuetype (getslotvaluetype slotnum oldvalblock))
	     (CONSTANT (setq slotvalue (insidescopy slotvalue)))
	     (LOCAL (and (equivclassp (cdr slotvalue))
			 (progn
			  (setq oldvarcell (cdr slotvalue))
			  (setq slotvalue (cons (car slotvalue) (punbound)))))
		    (cond ((eq *any*conscell* slotvalue) nil)
			  ; Bound variable.
			  ((neq (cdr slotvalue) (punbound))
			   (setq valuetype 'CONSTANT)
			   (setq slotvalue (insidescopy (cdr slotvalue))))
			  ; Test for previously seen unbound variable.
			  ((setq newvarcell
				 (assq (car slotvalue)
				       (getalist *currenttopcopy*)))
			   (setq slotvalue newvarcell))
			  ; Otherwise it is a new unbound variable.
			  ( t (setq slotvalue
				    (addalist (car slotvalue)
					      *currenttopcopy*))
			      (and (equivclassp oldvarcell)
				   (progn
				    (rplacd slotvalue oldvarcell)
				    (rplacd oldvarcell
					    (cons slotvalue
						  (cdr oldvarcell))))))))
	     (ADJUNCT (setq oldvarcell (cdr slotvalue))
		      (setq slotvalue (insidescopy (car slotvalue)))
		      (cond ((eq *any*conscell* oldvarcell)
			     (setq slotvalue (cons slotvalue *any*conscell*)))
			    ((atom oldvarcell)
			     (setq slotvalue (cons slotvalue oldvarcell)))
			    ; Used to throw away bound adjunct variables.
			    ;((neq (cdr oldvarcell) (punbound))
			      ; (setq valuetype 'CONSTANT)
			      ; (setq slotvalue (insidescopy (car slotvalue)))
			      ; )
			    ; Test for previously seen variable.
			    ((setq newvarcell
				   (assq (car oldvarcell)
					 (getalist *currenttopcopy*)))
			     (setq slotvalue (cons slotvalue newvarcell)))
			    ; Otherwise it is a new variable.
			    ( t (setq newvarcell
				      (addalist (car oldvarcell)
						*currenttopcopy*))
				(setq slotvalue (cons slotvalue newvarcell)))))
	     (GLOBAL nil))
    (putslotvaluetype slotnum valuetype valblock)
    (putslotvalue     slotnum slotvalue valblock)
    (putpred      slotnum (copy (getpred      slotnum oldvalblock)) valblock)
    (putslothooks slotnum (copy (getslothooks slotnum oldvalblock)) valblock)))


; Internal item processor of SCOPY.
(de insidescopy (item)
  (let
   (defblock valblock length slotvalue valuetype oldvalblock
	     oldvarcell newvarcell abbrev)
   (cond ((null item) nil)
	 ((numberp item) item)                    ; Integer
	 ((dtpr item)                             ; Lisp or Setof
	  (mapcar (function insidescopy) item))
	 ((psymbolp item) item)                   ; Symbol
	 ((atom item) item)                       ; Lisp Atom
	 ; Otherwise, an instance of a structure
	 ((structurep item)
	  (cond ((setq valblock (cdr (assq item *scopieditems*))) valblock)
		( t (setq oldvalblock item)
		    (setq defblock (getdefinition oldvalblock))
		    (setq valblock
			  (allocval (setq length (getstructlength defblock))))
		    (puttypetag '*pearlinst* valblock)
		    (push (cons item valblock) *scopieditems*)
		    (cond (*toplevelp*
			   (setq *currenttopcopy* valblock)
			   (setq *currentpearlstructure* valblock)
			   (initbothalists valblock)
			   (setq *currenttopalists* (getbothalists valblock))
			   ; Include the current environment in 
			   ;    the variable assoc-list.
			   (and *blockstack*
				(putalist (cdar *blockstack*) valblock))
			   (setq *toplevelp* nil))
			  ( t (putbothalists *currenttopalists* valblock)))
		    
		    (putdef defblock valblock)
		    (and (setq abbrev (getabbrev oldvalblock))
			 ; Make new abbrev and store struct in abbrev.
			 (setq abbrev (eval `(newsym ,abbrev)))
			 (set abbrev valblock)
			 ; and abbrev in struct.
			 (putabbrev abbrev valblock))
		    (for slotnum 1 length
			 (scopyslot))
		    valblock))))))

; Copy a structure.  Bound variables are replaced by their values.
;   Unbound variables are installed as new local variables in the
;   copy, subject to overruling by the current open blocks.
(de scopy (item)
  (setq *scopieditems* nil)
  (setq *toplevelp* t)
  (insidescopy item))



; Internal slot processor of PATTERNIZE.
(dm patternizeslot (none)
  '(progn
    (setq slotvalue (getslotvalue slotnum oldvalblock))
    (selectq (setq valuetype (getslotvaluetype slotnum oldvalblock))
	     (CONSTANT (setq slotvalue (insidepatternize slotvalue)))
	     (LOCAL (cond ((eq *any*conscell* slotvalue) nil)
			  ; Bound variable.
			  ((and (neq (cdr slotvalue) (punbound))
				(not (equivclassp (cdr slotvalue))))
			   (setq valuetype 'CONSTANT)
			   (setq slotvalue (insidepatternize (cdr slotvalue))))
			  ; Otherwise it is an unbound variable to
			  ;   be replaced by ?*any*.
			  ( t (setq slotvalue *any*conscell*))))
	     (ADJUNCT (setq slotvalue (insidepatternize (car slotvalue)))
		      (setq valuetype 'CONSTANT))
	     (GLOBAL nil))
    (putslotvaluetype slotnum valuetype valblock)
    (putslotvalue     slotnum slotvalue valblock)
    (putpred      slotnum (copy (getpred      slotnum oldvalblock)) valblock)
    (putslothooks slotnum (copy (getslothooks slotnum oldvalblock)) valblock)))

; Internal item processor of PATTERNIZE.
(de insidepatternize (item)
  (let
   (defblock valblock length slotvalue valuetype oldvalblock abbrev)
   (cond ((null item) nil)
	 ((numberp item) item)                    ; Integer
	 ((dtpr item)                             ; Setof
	  (mapcar (function insidepatternize) item))
	 ((psymbolp item) item)                   ; Symbol
	 ((atom item) item)                       ; Lisp Atom
	 ; Otherwise, an instance of a structure
	 ((structurep item)
	  (cond ((setq valblock (cdr (assq item *scopieditems*))) valblock)
		( t (setq oldvalblock item)
		    (setq defblock (getdefinition oldvalblock))
		    (setq valblock
			  (allocval (setq length (getstructlength defblock))))
		    (puttypetag '*pearlinst* valblock)
		    (push (cons item valblock) *scopieditems*)
		    (cond (*toplevelp*
			   (setq *currenttopcopy* valblock)
			   (setq *currentpearlstructure* valblock)
			   (initbothalists valblock)
			   (setq *currenttopalists* (getbothalists valblock))
			   ; Include the current environment in 
			   ;    the variable assoc-list.
			   (and *blockstack*
				(putalist (cdar *blockstack*) valblock))
			   (setq *toplevelp* nil))
			  ( t (putbothalists *currenttopalists* valblock)))
		    
		    (putdef defblock valblock)
		    (and (setq abbrev (getabbrev oldvalblock))
			 ; Make new abbrev and store struct in abbrev.
			 (setq abbrev (eval `(newsym ,abbrev)))
			 (set abbrev valblock)
			 ; and abbrev in struct.
			 (putabbrev abbrev valblock))
		    (for slotnum 1 length
			 (patternizeslot))
		    valblock))))))

; Do an scopy but replace all local variables with ?*any*.
(de patternize (item)
  (setq *scopieditems* nil)
  (setq *toplevelp* t)
  (insidepatternize item))

; Internal environment Scopy.
; Do an scopy of <item> as if it were a recursive call within
;    an scopy of <outer>.
(de intscopy (item outer)
  (let
   (defblock valblock length slotvalue valuetype oldvalblock
	     newvarcell oldvarcell abbrev)
   (setq *scopieditems* nil)
   (cond ((null item) nil)
	 ((numberp item) item)                    ; Integer
	 ((dtpr item)                             ; Lisp or Setof
	  (mapcar (function insidescopy) item))
	 ((psymbolp item) item)                   ; Symbol
	 ((atom item) item)                       ; Lisp Atom
	 ; Otherwise, an instance of a structure
	 ((structurep item)
	  (setq oldvalblock item)
	  (setq defblock (getdefinition oldvalblock))
	  (setq valblock (allocval (setq length (getstructlength defblock))))
	  (puttypetag '*pearlinst* valblock)
	  (push (cons item valblock) *scopieditems*)
	  (initbothalists valblock)
	  (setq *currenttopcopy* outer)
	  (setq *currentpearlstructure* outer)
	  (setq *toplevelp* nil)
	  (putdef defblock valblock)
	  (and (setq abbrev (getabbrev oldvalblock))
	       ; Make new abbrev and store struct in abbrev.
	       (setq abbrev (eval `(newsym ,abbrev)))
	       (set abbrev valblock)
	       ; and abbrev in struct.
	       (putabbrev abbrev valblock))
	  (for slotnum 1 length
	       (scopyslot))
	  valblock))))

; Internal slot processor of VARREPLACE
(dm varreplaceslot (none)
  '(progn
    (setq slotvalue (getslotvalue slotnum item))
    (selectq (setq valuetype (getslotvaluetype slotnum item))
	     (CONSTANT (insidevarreplace slotvalue))
	     (LOCAL (cond ((eq *any*conscell* slotvalue) nil)
			  ; Bound variable, so replace with value.
			  ((and (neq (cdr slotvalue) (punbound))
				(not (equivclassp (cdr slotvalue))))
			   (putslotvaluetype slotnum 'CONSTANT item)
			   ; Should the value be varreplaced like this?
			   (putslotvalue slotnum
					 (insidevarreplace (cdr slotvalue))
					 item))
			  ; Otherwise an unbound variable.
			  ( t nil)))
	     (ADJUNCT (insidevarreplace (car slotvalue)))
	     (GLOBAL  (and (neq (setq slotvalue (eval slotvalue)) (punbound))
			   (not (equivclassp slotvalue))
			   (progn (putslotvaluetype slotnum 'CONSTANT item)
				  (putslotvalue slotnum
						(insidevarreplace slotvalue)
						item)))))))

; Internal item processor of VARREPLACE
(de insidevarreplace (item)
  (let
   (length slotvalue valuetype)
   (cond ((null item) nil)
	 ((numberp item) item)                    ; Integer
	 ((dtpr item)                             ; Lisp or Setof
	  (mapcar (function insidevarreplace) item))
	 ((psymbolp item) item)                   ; Symbol
	 ((atom item) item)                       ; Lisp Atom
	 ; Otherwise, an instance of a structure
	 ((structurep item)
	  (cond ((memq item *scopieditems*) item)
		( t (setq length (getstructlength (getdefinition item)))
		    (cond (*toplevelp*
			   (setq *currentpearlstructure* item)
			   (setq *toplevelp* nil)))
		    (push item *scopieditems*)
		    (for slotnum 1 length
			 (varreplaceslot))
		    item))))))

; Go through a structure replacing bound variables by their values.
(de varreplace (item)
  (setq *scopieditems* nil)
  (setq *toplevelp* t)
  (insidevarreplace item))


; Merge ITEM2 into ITEM1 by copying all bound slots of ITEM2 into
;    any unfrozen slots of ITEM1.
(de smerge (item1 item2)
  (let ((defblock1 (getdefinition item1))
	(defblock2 (getdefinition item2)))
       (and (neq defblock1 defblock2)
	    (not (memq defblock1 (getexpansionlist defblock2)))
	    (progn (msg t "SMERGE: Values not mergeable: " item2
			t "   and " item1)
		   (pearlbreak)))
       (prog (length oldvalue potential result newitem1 newitem2)
	     ; unbind all non-frozen vars first.
	     (mapc (funl (cell) (rplacd cell (punbound))) (getalist item1))
	     (setq length (getstructlength defblock2))
	     (setq result (punbound))
	     (dobasehooks2< '<smerge *runsmergehooks*)
	     (for slotnum 1 length
		  (setq potential (getvalue slotnum item2))
		  (setq oldvalue (getvalue slotnum item1))
		  (and (pboundp potential)
		       (not (pboundp oldvalue))
		       (progn (putslotvalue slotnum potential item1)
			      (putslotvaluetype slotnum 'CONSTANT item1))))
	     (setq result item1)
	     (dobasehooks2> '>smerge *runsmergehooks*)
	     (return result))))


; vi: set lisp:
