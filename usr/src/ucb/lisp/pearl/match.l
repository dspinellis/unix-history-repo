;;;;;;;;;;;;;;;;;;;;;;;;;;;;; match.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for matching, comparing, and testing structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.
; Unification added by David Chin.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions which accomplish unification of two variables.

; Turns on unification (irrevocably).
(de useunification ()
  (setq *unifyunbounds* t)
  'UsingUnification)

; sets all variables in the var list of the equiv class (first arg) which are
; still bound to the equiv class to the new value (second arg).
(defmacro setequivclass (equiv value)
  `(mapc (funl (var)
	       (cond ((dtpr var)  ; a local var cell
		      ; If bound to equiv class, then save the old value
		      ;   and set the var to value.
		      (and (eq (cdr var) ,equiv)
			   (push (cons var (cdr var)) *equivsavestack*)
			   (rplacd var ,value)))
		     ( t ; otherwise a global var.
			 (and (eq (eval var) ,equiv)
			      (push (cons var (eval var)) *equivsavestack*)
			      (set var ,value)))))
	 (cdr ,equiv)))

; unifies two unbound variables (0, one or both may already be equiv classes).
(dm unifytwovars (none)
  '(progn (setq xval (cond ((dtpr xvar) (cdr xvar))
			   ( t (eval xvar))))
	  (setq yval (cond ((dtpr yvar) (cdr yvar))
			   ( t (eval yvar))))
	  (cond ((eq xvar yvar)
		 ; Same variable, so leave xvar and yvar alone.
		 (setq newval nil))
		; Both values are unbound so create a new equiv class.
		((and (eq xval (punbound))
		      (eq yval (punbound)))
		 (setq newval (cons (equivclass) (list xvar yvar))))
		; Same equiv class (not "unbound"), so leave xvar & yvar alone.
		((eq xval yval)
		 (setq newval nil))
		; Both are equiv classes, so merge into a new equiv class.
		((and (pboundp xval)
		      (pboundp yval))
		 (setq newval
		       (cons (equivclass)
			     (cond ((<& (length (cdr xval))
					(length (cdr yval)))
				    (append (cdr xval) (cdr yval)))
				   ( t (append (cdr yval) (cdr xval))))))
		 ; And change the equiv class for the other vars in the list.
		 (setequivclass xval newval)
		 (setequivclass yval newval))
		((punboundatomp xval) ; xvar is not an equiv class.
		 (cond ((memq xvar (cdr yval)) ; but used to be in yvar's.
			(setq newval yval))
		       ( t ; else build a new equiv class with yvar added.
			   (setq newval (cons (equivclass)
					      (cons xvar (cdr yval))))
			   (setequivclass yval newval))))
		( t ; otherwise yvar is not an equiv class.
		    (cond ((memq yvar (cdr xval)) ; but used to be in xvar's.
			   (setq newval xval))
			  ( t ; else build a new equiv class with xvar added.
			      (setq newval (cons (equivclass)
						 (cons yvar (cdr xval))))
			      (setequivclass xval newval)))))
	  ; Set the variables to a new equiv class created above.
	  (and newval
	       (progn
		; Save the old values in case match fails
		(push (cons xvar xval) *equivsavestack*)
		(push (cons yvar yval) *equivsavestack*)
		; And set variables (either local or global).
		(cond ((dtpr xvar) (rplacd xvar newval))
		      ( t (set xvar newval)))
		(cond ((dtpr yvar) (rplacd yvar newval))
		      ( t (set yvar newval)))))
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Low level macros for matching.

; Fast macro for minimum of two lengths.
(defmacro min& (n1 n2)
  `(let ((min ,n1)
	 (other ,n2))
	(and (>& min other)
	     (setq min other))
	min))

; Unbind all vars on the item's assoc list
(defmacro unbindvars (item)
  `(mapc (funl (cell) (rplacd cell (punbound))) (getalist ,item)))

; Set the GLOBAL or VAR variable to the value.
(defmacro varset (var val)
  `(let ((localvar ,var)
	 (localval ,val)
	 savevarval)
	(cond ((dtpr localvar)
	       (setq savevarval (cdr localvar))
	       (rplacd localvar localval))
	      ( t (push localvar *globalsavestack*)
		  (setq savevarval (eval localvar))
		  (set localvar localval)))
	(and *unifyunbounds*
	     (equivclassp savevarval)
	     (setequivclass savevarval localval))))

; Set the GLOBAL or VAR adjunct variable to the value.
(defmacro adjvarset (var val)
  `(let ((localvar ,var)
	 (localval ,val)
	 savevarval)
	(and localvar
	     (progn (cond ((dtpr localvar)
			   (setq savevarval (cdr localvar))
			   (rplacd localvar localval))
			  ( t (push localvar *globalsavestack*)
			      (setq savevarval (eval localvar))
			      (set localvar localval)))
		    (and *unifyunbounds*
			 (equivclassp savevarval)
			 (setequivclass savevarval localval))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros for matching individual values.

; Check whether VAL is consistent with the predicates in PREDLIST.
(defmacro consistentvalue (val predlist type item defblock)
  `(prog (restriction)
	 loop
	 (cond ((null ,predlist) (return t)) ; all predicates were true.
	       ; Otherwise, execute the next one.
	       ((cond ((reallitatom (setq restriction (pop ,predlist)))
		       ; The name of a function to be applied.
		       (apply* restriction (ncons ,val)))
		      ; An s-expression predicate -- fill in and execute.
		      ((dtpr restriction)
		       (eval (fillin1 restriction ,val ,item ,defblock)))
		      ; Otherwise, a value.
		      ( t
		       (selectq ,type
				(0 (or (let ((def (getdefinition ,val)))
					    (eq restriction def))
				       (disguisedas ,val restriction)))
				(1 (disguisedas ,val restriction))
				(2 (\=& restriction ,val))
				(3 (eq restriction ,val))
				(otherwise
				 ; A better way needed ?? Never done????
				 (eq restriction (car ,val))))))
		(go loop))
	       ; Otherwise this predicate failed, so we fail.
	       ( t (return nil)))))

; Check two values for "equality".
(defmacro equalvalue (xval yval type)
  `(selectq ,type
	    (0 (basicmatch ,xval ,yval))
	    (1 (eq ,xval ,yval))
	    (2 (\=& ,xval ,yval))
	    (3 (equal ,xval ,yval))
	    (otherwise
	     ; A better way needed!!!!!!!!!!!!!!!!!!!  something like:
	     ; (apply (function and)
	     ;        (mapcar (function equalvalue) ,xval ,yval (strip ,type)))
	     t)))

; Check to see if two slots whose number is passed are matchable,
; binding any variables and running any predicates.
; Assumes slotnum, item1, item2, def1, def2 already set and others declared
;    in main PROG.  The local PROG is necessary for slothooks processing.
(dm compatible (none)
  '(prog ()
	 ; *val and *var are both set by these calls.
	 ; *var are set to nil if no local, global, or adjunct variable.
	 (setq xval (getvarandvalue slotnum item1 'xvar))
	 (setq yval (getvarandvalue slotnum item2 'yvar))
	 ;
	 ; *ANY* => always match
	 (and (or (eq xvar *any*conscell*)
		  (eq yvar *any*conscell*))
	      (return t))
	 ;
	 ; If both are unbound, return *matchunboundsresult* (initially nil).
	 (setq xvalunbound (punboundatomp xval))
	 (setq yvalunbound (punboundatomp yval))
	 (setq bothunbound (and xvalunbound yvalunbound))
	 (and bothunbound
	      (or *unifyunbounds*
		  (return *matchunboundsresult*)))
	 ;
	 ; Get the slots' common type and individual predicates.
	 (setq slottype (getslottype slotnum def1))
	 (setq xpredlist (getpred slotnum item1))
	 (setq ypredlist (getpred slotnum item2))
	 (doslothooks2< '<match *runmatchhooks*)
	 ;
	 ; Otherwise we check to see if one of the slots can be
	 ;     bound to the other.
	 (cond (bothunbound ; Two unbound variables to be unified.
			    (unifytwovars)
			    (setq result t))
	       (xvalunbound ; Match x's variable against y's value.
		(and (setq result
			   (consistentvalue yval xpredlist slottype item2 def2))
		     (varset xvar yval)))
	       (yvalunbound ; Match y's variable against x's value.
		(and (setq result
			   (consistentvalue xval ypredlist slottype item1 def1))
		     (varset yvar xval)))
	       ( t  ; both are bound values -- check "equality".
		    (and (setq result (equalvalue xval yval slottype))
			 ; and set the adjunct variables (if any)
			 (progn (adjvarset xvar yval)
				(adjvarset yvar xval)))))
	 (doslothooks2> '>match *runmatchhooks*)
	 (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Principle match functions.

; Match two structures slot by slot, WITHOUT unbinding variables first,
; but binding along the way.
(de basicmatch (item1 item2)
  (prog (newitem1 newitem2 result slottype xvar yvar xval yval def1 def2
		  xvalunbound yvalunbound length
		  newxval newyval xpredlist ypredlist xhooks yhooks
		  newval bothunbound)
	(setq def1 (getdefinition item1))
	(setq def2 (getdefinition item2))
	(setq length (getstructlength def1))
	(dobasehooks2< '<match *runmatchhooks*)
	(cond ((eq item1 item2) (setq result t)) ; Same structure -> t.
	      ; Not even related -> nil.
	      ((not (eq def1 def2)) (setq result nil))
	      ; No slots -> t.
	      ((\=& 0 length) (setq result t))
	      ; Otherwise, compare slot by slot.
	      ( t (setq result
			(for slotnum 1 length
			     (or (compatible)
				 (return nil))))))
	(dobasehooks2> '>match *runmatchhooks*)
	(return result)))

; Match two structures slot by slot, unbinding variables first.
(de standardmatch (item1 item2)
  (prog (newitem1 newitem2 result slottype xvar yvar xval yval def1 def2
		  xvalunbound yvalunbound length *globalsavestack*
		  newxval newyval xpredlist ypredlist xhooks yhooks
		  newval bothunbound *equivsavestack*)
	(unbindvars item1)
	(unbindvars item2)
	(setq def1 (getdefinition item1))
	(setq def2 (getdefinition item2))
	(setq length (getstructlength def1))
	(dobasehooks2< '<match *runmatchhooks*)
	(cond ((eq item1 item2) (setq result t)) ; Same structure -> t.
	      ; Not even related -> nil.
	      ((not (eq def1 def2)) (setq result nil))
	      ; No slots -> t.
	      ((\=& 0 length) (setq result t))
	      ; Otherwise, compare slot by slot.
	      ( t (setq result
			(for slotnum 1 length
			     (or (compatible)
				 (return nil))))))
	(dobasehooks2> '>match *runmatchhooks*)
	(or result 
	    ; Clean up the variables because of the failure.
	    (progn (unbindvars item1)
		   (unbindvars item2)
		   (and *globalsavestack*
			(mapc (funl (var)
				    (set var (punbound)))
			      *globalsavestack*))
		   ; *equivsavestack* is only non-nil when *unifyunbounds* is t.
		   (and *equivsavestack*
			(mapc (funl (pair)
				    (cond ((dtpr (car pair))
					   (rplacd (car pair) (cdr pair)))
					  ( t (set (car pair) (cdr pair)))))
			      *equivsavestack*))))
	(return result)))

(aliasdef 'match 'standardmatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions similar to above but for expanded structures.

; Check to see either defblock is an expansion of the other.
(defmacro relatedhier (defblock1 defblock2)
  `(or (eq ,defblock1 ,defblock2)
       (memq ,defblock2 (getexpansionlist ,defblock1))
       (memq ,defblock1 (getexpansionlist ,defblock2))))

; Check whether VAL is consistent with the predicates in PREDLIST.
(defmacro expconsistentvalue (val predlist type item defblock)
  `(prog (restriction)
	 loop
	 (cond ((null ,predlist) (return t)) ; all predicates were true.
	       ; Otherwise, execute the next one.
	       ((cond ((reallitatom (setq restriction (pop ,predlist)))
		       ; The name of a function to be applied.
		       (apply* restriction (ncons ,val)))
		      ; An s-expression predicate -- fill in and execute.
		      ((dtpr restriction)
		       (eval (fillin1 restriction ,val ,item ,defblock)))
		      ; Otherwise, a value.
		      ( t
		       (selectq ,type
				(0 (or (let ((def (getdefinition ,val)))
					    (relatedhier restriction def))
				       (disguisedas ,val restriction)))
				(1 (disguisedas ,val restriction))
				(2 (\=& restriction ,val))
				(3 (eq restriction ,val))
				(otherwise
				 ; A better way needed ?? Never done????
				 (eq restriction (car ,val))))))
		(go loop))
	       ; Otherwise this predicate failed, so we fail.
	       ( t (return nil)))))

; Check two values for "equality".
(defmacro expequalvalue (xval yval type)
  `(selectq ,type
	    (0 (basicexpandedmatch ,xval ,yval))
	    (1 (eq ,xval ,yval))
	    (2 (\=& ,xval ,yval))
	    (3 (equal ,xval ,yval))
	    (otherwise
	     ; A better way needed!!!!!!!!!!!!!!!!!!!  something like:
	     ; (apply (function and)
	     ;    (mapcar (function expequalvalue) ,xval ,yval (strip ,type)))
	     t)))

; Check to see if two slots whose number is passed are matchable,
; binding any variables and running any predicates.
; Assumes slotnum, item1, item2, def1, def2 already set and others declared
;    in main PROG.  The local PROG is necessary for slothooks processing.
(dm expcompatible (none)
  '(prog ()
	 ; *val and *var are both set by these calls.
	 ; *var are set to nil if no local, global, or adjunct variable.
	 (setq xval (getvarandvalue slotnum item1 'xvar))
	 (setq yval (getvarandvalue slotnum item2 'yvar))
	 ;
	 ; *ANY* => always match
	 (and (or (eq xvar *any*conscell*)
		  (eq yvar *any*conscell*))
	      (return t))
	 ;
	 ; If both are unbound, return *matchunboundsresult* (initially nil).
	 (setq xvalunbound (punboundatomp xval))
	 (setq yvalunbound (punboundatomp yval))
	 (setq bothunbound (and xvalunbound yvalunbound))
	 (and bothunbound
	      (or *unifyunbounds*
		  (return *matchunboundsresult*)))
	 ;
	 ; Get the slots' common type and individual predicates.
	 (setq slottype (getslottype slotnum def1))
	 (setq xpredlist (getpred slotnum item1))
	 (setq ypredlist (getpred slotnum item2))
	 (doslothooks2< '<match *runmatchhooks*)
	 ;
	 ; Otherwise we check to see if one of the slots can be
	 ;     bound to the other.
	 (cond (bothunbound ; Two unbound variables to be unified.
			    (unifytwovars)
			    (setq result t))
	       (xvalunbound ; Match x's variable against y's value.
		(and (setq result
			   (expconsistentvalue yval xpredlist slottype
					       item2 def2))
		     (varset xvar yval)))
	       (yvalunbound ; Match y's variable against x's value.
		(and (setq result
			   (expconsistentvalue xval ypredlist slottype
					       item1 def1))
		     (varset yvar xval)))
	       ( t  ; both are bound values -- check "equality".
		    (and (setq result (expequalvalue xval yval slottype))
			 ; and set the adjunct variables (if any)
			 (progn (adjvarset xvar yval)
				(adjvarset yvar xval)))))
	 (doslothooks2> '>match *runmatchhooks*)
	 (return result)))

; Match two structures slot by slot, WITHOUT unbinding variables first,
; but binding along the way.
(de basicexpandedmatch (item1 item2)
  (prog (newitem1 newitem2 result slottype xvar yvar xval yval def1 def2
		  xvalunbound yvalunbound length
		  newxval newyval xpredlist ypredlist xhooks yhooks
		  newval bothunbound)
	(setq def1 (getdefinition item1))
	(setq def2 (getdefinition item2))
	(setq length (min& (getstructlength def1)
			   (getstructlength def2)))
	(dobasehooks2< '<match *runmatchhooks*)
	(cond ((eq item1 item2) (setq result t)) ; Same structure -> t.
	      ; Not even related hierarchically -> nil.
	      ((not (relatedhier def1 def2)) (setq result nil))
	      ; No slots -> t.
	      ((\=& 0 length) (setq result t))
	      ; Otherwise, compare slot by slot.
	      ( t (setq result
			(for slotnum 1 length
			     (or (expcompatible)
				 (return nil))))))
	(dobasehooks2> '>match *runmatchhooks*)
	(return result)))

; Match two structures slot by slot, unbinding variables first.
(de standardexpandedmatch (item1 item2)
  (prog (newitem1 newitem2 result slottype xvar yvar xval yval def1 def2
		  xvalunbound yvalunbound length *globalsavestack*
		  newxval newyval xpredlist ypredlist xhooks yhooks
		  newval bothunbound *equivsavestack*)
	(unbindvars item1)
	(unbindvars item2)
	(setq def1 (getdefinition item1))
	(setq def2 (getdefinition item2))
	(setq length (min& (getstructlength def1)
			   (getstructlength def2)))
	(dobasehooks2< '<match *runmatchhooks*)
	(cond ((eq item1 item2) (setq result t)) ; Same structure -> t.
	      ; Not even related hierarchically -> nil.
	      ((not (relatedhier def1 def2)) (setq result nil))
	      ; No slots -> t.
	      ((\=& 0 length) (setq result t))
	      ; Otherwise, compare slot by slot.
	      ( t (setq result
			(for slotnum 1 length
			     (or (expcompatible)
				 (return nil))))))
	(dobasehooks2> '>match *runmatchhooks*)
	(or result 
	    ; Clean up the variables because of the failure.
	    (progn (unbindvars item1)
		   (unbindvars item2)
		   (and *globalsavestack*
			(mapc (funl (var)
				    (set var (punbound)))
			      *globalsavestack*))
		   ; *equivsavestack is only non-nil when *unifyunbounds* is t.
		   (and *equivsavestack*
			(mapc (funl (var)
				    (cond ((dtpr (car var))
					   (rplacd (car var) (cdr var)))
					  ( t (set (car var) (cdr var)))))
			      *equivsavestack*))
		   ))
	(return result)))

(aliasdef 'expandedmatch 'standardexpandedmatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for testing for equality and other comparisons.

; Check to see if two slots passed (with a type number) are EQUAL,
; NOT binding any variables OR checking any predicates.
(dm slotequal (none)
  '(prog ()
	 ; *val and *var are both set by these calls.
	 ; *var are set to nil if no local, global, or adjunct variable.
	 (setq xval (getvarandvalue slotnum item1 'xvar))
	 (setq yval (getvarandvalue slotnum item2 'yvar))
	 ;
	 ; If the slot of the first ITEM is unbound, fail
	 (and (punboundatomp xval)
	      (progn (msg t "Unbound variables not allowed in STREQUAL" t)
		     (pearlbreak)))
	 ; If the slot of the second ITEM is unbound, fail
	 (and (punboundatomp yval)
	      (progn (msg t "Unbound variables not allowed in STREQUAL" t)
		     (pearlbreak)))
	 ;
	 ; Get the slots' common type.
	 (setq slottype (getslottype slotnum def1))
	 (doslothooks2< '<strequal *runstrequalhooks*)
	 (setq result
	       (selectq slottype
			(0 (strequal xval yval))
			(1 (eq xval yval))
			(2 (\=& xval yval))
			(3 (equal xval yval))
			(otherwise
			 ; A better way needed!!!!!!!!!!!!!!!!!!!
			 (equal xval yval))))
	 (doslothooks2> '>strequal *runstrequalhooks*)
	 (return result)))

; Test two structures for "EQUAL"ity slot by slot, without unbinding
; variables first, and NOT binding along the way.
(de strequal (item1 item2)
  (prog (newitem1 newitem2 result slottype xvar yvar xval yval
		  def1 def2 length newxval newyval xhooks yhooks)
	(setq def1 (getdefinition item1))
	(setq def2 (getdefinition item2))
	(setq length (getstructlength def1))
	(dobasehooks2< '<strequal *runmatchhooks*)
	(cond ((eq item1 item2) (setq result t)) ; Same structure -> t.
	      ; Not even same type -> nil.
	      ((neq def1 def2) (setq result nil))
	      ; No slots -> t.
	      ((\=& 0 length) (setq result t))
	      ; Otherwise, compare slot by slot.
	      ( t (setq result
			(for slotnum 1 length
			     (or (slotequal)
				 (return nil))))))
	(dobasehooks2> '>strequal *runmatchhooks*)
	(return result)))

; Check to see if ITEM1 is an expansion of ITEM2.
(de isanexpanded (item1 item2)
  (let ((defblock1 (getdefinition item1))
	(defblock2 (getdefinition item2)))
       (or (eq defblock1 defblock2)
	   (memq defblock1 (getexpansionlist defblock2)))))

; Check to see if ITEM1 is (an expansion of) the base with name NAME.
(de isa (item1 name)
  (let ((defblock (getdefinition item1))
	(typedef (eval (defatom name))))
       (or (eq defblock typedef)
	   (memq defblock (getexpansionlist typedef)))))

; Test item to see if it's a nilstruct.
(de nullstruct (item)
  (eq (getdefinition item)
      (eval (defatom 'nilstruct))))

; Test item to see if it's a nilsym.
(de nullsym (item)
  (eq item
      (eval (symatom 'nilsym))))

(de memmatch (item list)
  (cond ((null list) nil)
	((not (dtpr list)) nil)
	((match item (car list)) list)
	( t (memmatch item (cdr list)))))

(de memstrequal (item list)
  (cond ((null list) nil)
	((not (dtpr list)) nil)
	((strequal item (car list)) list)
	( t (memstrequal item (cdr list)))))

; vi: set lisp:
