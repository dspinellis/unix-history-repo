;;;;;;;;;;;;;;;;;;;;;;;;;;;;; path.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for accessing and changing information associated with
;    slots of structures via a path.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; The PATH functions provide methods for adding and accessing information
;     in a structure.  The PATH macro takes as a first argument the function
;     to be performed and simply expands to the function.  The functions
;     available are:
;     1. PUTPATH -- replaces the value in the slot with one provided.
;     2. CLEARPATH -- replaces the value of the slot with the default.
;     3. ADDSETPATH -- adds the value provided to a SETOF slot (only one
;			    level of adding is currently available).
;     4. DELSETPATH -- deletes the value provided from a SETOF slot (note
;			    that this requires one to know the actual
;			    value to delete).
;     5. ADDPREDPATH -- adds a predicate (function, STRUCT, or hook) to
;			    the PREDLIST.
;     6. DELPREDPATH -- deletes a predicate from the PREDLIST.
;     7. GETPATH -- returns a pointer to the value in the slot.
;     8. GETPREDPATH -- returns the list of function and STRUCT
;				    predicates for the slot.
;     9. GETHOOKPATH -- returns the list of (dotted pair) hook
;				    functions for the slot.
;    10. APPLYPATH -- returns the result of APPLYing the function
;			      provided to the value for the slot.
;
; During a PATH operation, the global variable *PATHTOP* contains the
;    top level item which is being accessed and *PATHLOCAL* is the most
;    local item being accessed.  These are most handy for use by hooks
;    and predicates.

(defmacro path (fcn item pathlist &optional val)
  (selectq fcn
	   (put          `(putpath      ,item ,pathlist ,val))
	   (clear        `(clearpath    ,item ,pathlist))
	   (addset       `(addsetpath   ,item ,pathlist ,val))
	   (delset       `(delsetpath   ,item ,pathlist ,val))
	   (addpred      `(addpredpath  ,item ,pathlist ,val))
	   (delpred      `(delpredpath  ,item ,pathlist ,val))
	   (get          `(getpath      ,item ,pathlist))
	   (getpred      `(getpredpath  ,item ,pathlist))
	   (gethook      `(gethookpath  ,item ,pathlist))
	   (apply        `(applypath    ,item ,pathlist ,val))
	   (otherwise (msg t "PATH: Illegal function selector: " fcn
			   ". Rest of call was: "  item " " pathlist " " val t)
		      (pearlbreak))))

(de putpath (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(checkrunhandleslothooks1 '<put *runputpathhooks*)
	(selectq (getslotvaluetype slotnum item)
		 (CONSTANT (putslotvalue slotnum value item))
		 (ADJUNCT
		  (putslotvalue slotnum
				(cons value (cdr (getslotvalue slotnum item)))
				item))
		 ((LOCAL GLOBAL)
		  (putslotvaluetype slotnum 'CONSTANT item)
		  (putslotvalue slotnum value item)))
	(checkrunhandleslothooks1 '>put *runputpathhooks*)
	(return value)))
 
(de clearpath (item path)
  (prog (numitempair slotnum value result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(setq value (defaultfortype (getslottype slotnum (getdefinition item))))
	(checkrunhandleslothooks1 '<clear *runclearpathhooks*)
	(putslotvaluetype slotnum 'CONSTANT item)
	(putslotvalue slotnum value item)
	(checkrunhandleslothooks1 '>clear *runclearpathhooks*)
	(return value)))
 
(de addsetpath (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(checkrunhandleslothooks1 '<addset *runaddsetpathhooks*)
	(putslotvaluetype slotnum 'CONSTANT item)
	(putslotvalue slotnum (cons value (getvalue slotnum item)) item)
	(checkrunhandleslothooks1 '>addset *runaddsetpathhooks*)
	(return value)))
 
(de delsetpath (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(checkrunhandleslothooks1 '<delset *rundelsetpathhooks*)
	(putslotvaluetype slotnum 'CONSTANT item)
	(putslotvalue slotnum (delq value (getvalue slotnum item)) item)
	(checkrunhandleslothooks1 '>delset *rundelsetpathhooks*)
	(return value)))
 
(de addpredpath (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(checkrunhandleslothooks1 '<addpred *runaddpredpathhooks*)
	(putpred slotnum (cons value (getpred slotnum item)) item)
	(checkrunhandleslothooks1 '>addpred *runaddpredpathhooks*)
	(return value)))
 
(de delpredpath  (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(checkrunhandleslothooks1 '<delpred *rundelpredpathhooks*)
	(putpred slotnum (delete value (getpred slotnum item)) item)
	(checkrunhandleslothooks1 '>delpred *rundelpredpathhooks*)
	(return value)))
 
(de getpath (item path)
  (prog (numitempair slotnum value result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(setq value (punbound))
	(checkrunhandleslothooks1 '<get *rungetpathhooks*)
	(or (neq value (punbound))
	    (setq value (getvalue slotnum item)))
	(checkrunhandleslothooks1 '>get *rungetpathhooks*)
	(return value)))
 
(de getpredpath (item path)
  (prog (numitempair slotnum value result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cadr numitempair)))
	(setq value (punbound))
	(checkrunhandleslothooks1 '<getpred *rungetpredpathhooks*)
	(or (neq value (punbound))
	    (setq value (getpred slotnum item)))
	(checkrunhandleslothooks1 '>getpred *rungetpredpathhooks*)
	(return value)))
 
(de gethookpath (item path value)
  (prog (numitempair slotnum result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cadr numitempair)))
	(setq value (punbound))
	(checkrunhandleslothooks1 '<gethook *rungethookpathhooks*)
	(or (neq value (punbound))
	    (setq value (getslothooks slotnum item)))
	(checkrunhandleslothooks1 '>gethook *rungethookpathhooks*)
	(return value)))
 
(de applypath (fcn item path)
  (prog (numitempair slotnum value result)
	(setq *pathtop* item)
	(setq *currentpearlstructure* item)
	(and (null (setq numitempair (followpath item path)))
	     (return nil))
	(setq slotnum (car numitempair))
	(setq *pathlocal* (setq item (cdr numitempair)))
	(setq value (getvalue slotnum item))
	(checkrunhandleslothooks1 '<apply *runapplypathhooks*)
	(executehook1 fcn value item (getdefinition item))
	(checkrunhandleslothooks1 '>apply *runapplypathhooks*)
	(return value)))
 
; This does indirection.  If the path is longer and we come to a
;    symbol, we try to find something of the type with the name
;    that is next on the path and with the symbol in its first slot.
;    Unfortunately, this always uses the data base in *db*.
(defmacro findstructsymbolpair (defblock symbol)
  `(progn (and (setq bucket (gethash2 (getuniquenum ,defblock)
				      (getuniquenum ,symbol)
				      ; **** FIX to use different dbs (how?)
				      (getdb2 *db*)
				      ))
	       (while (and (setq potential (pop bucket))
			   (not (and (eq (getdefinition potential) ,defblock)
				     (eq (getvalue 1 potential)
					 ,symbol))))
		      potential))
	  potential))
 
; Follow the path down through the structures starting at item.
(de followpath (item path)
  (or (structurep item)
      (progn (msg t "PATH: only works on structures, not on " item
		  ". Requested path was: " path t)
	     (pearlbreak)))
  (let (slotnum type slotname bucket potential slotlocation)
       (and (atom path)
	    (setq path (ncons path)))
       (while (setq slotname (pop path))
	      (and (\=& 0
			(setq slotnum
			      (slotnametonumber slotname
						(getdefinition item))))
		   (progn (msg t "PATH: illegal slotname " slotname "requested "
			       "from " item ". Remaining path is: " path t)
			  (pearlbreak)))
	      (and (null path)
		   (return (cons slotnum item)))
	      ; If a symbol slot (and more path), do indirection.
	      (cond ((\=& 1
			  (setq type (getslottype slotnum
						  (getdefinition item))))
		     (and (null (setq item
				      (findstructsymbolpair
				       (eval (defatom (pop path)))
				       (getvalue slotnum item))))
			  (return nil)))
		    ((\=& 0 type) (setq item (getvalue slotnum item)))
		    (  t  (msg t "PATH: Unable to follow path.  "
			       "Bad slotname is " slotname t)
			  (pearlbreak))))))


; vi: set lisp:
