;;;;;;;;;;;;;;;;;;;;;;;;;;;;; print.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for converting from internal form to a printable form.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Convert a predicate, which might be a structure, to printable form.
(de convertpreds (pred)
  (cond ((or (litatom pred)
	     (dtpr pred)
	     (numberp pred))
	 pred)
	((structurep pred) (allform pred))
	((definitionp pred) (getpname pred))
	( t pred)))
 
; Reverse assoc through a list of cons-cells -- look at the CDRs
;   for value and return the first cons-cell that matches.
(de revassq (value alist)
  (while alist  ; is not NIL
	 (and (eq value (cdar alist))
	      (return (car alist)))
	 (setq alist (cdr alist))))

; Convert an ordinal to printable form.
(defmacro ppsetform (slotval ppset)
  `(cond ((eq 'int ,ppset) ,slotval)
	 ( t (let ((assqlist (eval (ordatom ,ppset)))
		   assqresult)
		  (cond ((setq assqresult (revassq ,slotval assqlist))
			 (car assqresult))
			((\=& 0 ,slotval) '*zero-ordinal-value*)
			( t  (list ,ppset ,slotval)))))))

; Convert a stream to printable form.
(defmacro streamform (item)
  `(cond ((eq t (cadr ,item)) (list '*function-stream:*
				    (structureform (cddr ,item))))
	 ((or *fullprint*
	      (not *streamprintlength*))
	  (list '*stream:*
		(structureform (cadr ,item))
		(mapcan (funl (struct)
			      (cond ((eq '*db* struct) nil)
				    ( t (ncons (structureform struct)))))
			(cddr ,item))))
	 ( t
	  (list
	   '*stream:*
	   (structureform (cadr ,item))
	   (let
	    ((rest (cddr ,item))
	     (result (ncons nil))
	     next)
	    (cond ((dtpr (car rest))
		   ; stream built by expandedfetch.
		   (let ((itemnum 1)
			 bucket)
			(while (setq bucket (pop rest))
			       (mapc
				(funl (next)
				      (or (eq '*db* next)
					  (progn
					   (and (>& itemnum *streamprintlength*)
						(progn
						 (tconc result '|...|)
						 (return (car result))))
					   (tconc result (structureform next))
					   (setq itemnum (1+ itemnum))
					   )))
				bucket)
			       (or rest
				   (return (car result))))))
		  ( t (for itemnum 1 *streamprintlength*
			   (while (and (setq next (pop rest))
				       (eq '*db* next))
				  ) ; do nothing
			   (or next
			       (return (car result)))
			   (tconc result (structureform next)))))
	    (and rest
		 (tconc result '|...|))
	    (car result))))))
 
; Convert a symbol to printable form.
(defmacro symbolform (item)
  `(getsymbolpname ,item))

; Convert an equivalence class list to printable form.
(defmacro equivclassform (equiv)
  `(let ((equivclass ,equiv))
	(mapcan (funl (var)
		      (cond ((dtpr var) ; a local var
			     ; filter out variables which are no longer
			     ; members of the equivalence class
			     (and (eq (cdr var) equivclass)
				  (ncons (list '*var* (car var)))))
			    ( t ; otherwise a global var
				(and (eq (eval var) equivclass)
				     (ncons (list '*global* var))))))
		(cdr equivclass))))

; Convert a definition to printable form.
(defmacro defform (item)
  `(cons 'definition-of:
	 (structureform (getdefaultinst ,item))))

; Convert the constant portion of a slot
(defmacro slotconstform (item typenum ppset)
  `(selectq ,typenum
	    (0 (or (and *abbrevprint*
			(getabbrev ,item))
		   (structureform ,item)))
	    (1 (symbolform ,item))
	    (2 (ppsetform ,item ,ppset))
	    (3 (allform ,item))
	    (otherwise
	     (let ((newtypenum (- ,typenum 4.)))
		  (cond ((dtpr ,item)
			 (mapcar
			  (funl (singleitem)
				(listitemform singleitem newtypenum ,ppset))
			  ,item))
			; otherwise, in case value is somehow not a list,
			;    do your best.
			(t (allform ,item)))))))

; Makes a function out of slotconstform for mapping on a setof slot.
(de listitemform (item typenum ppset)
  (slotconstform item typenum ppset))

; Macro version of slotconstform for normal use on a slot's value.
(defmacro slotitemform (printval)
  `(let ((item ,printval)
	 (typenum (getslottype slotnum defblock))
	 (ppset (getppset slotnum defblock)))
	(slotconstform item typenum ppset)))

; Convert a slot from internal form to a list form.
(dm slotform (none)    ; but assumes SLOTNUM, ITEM, PRINTVAL and PRINTVAR.
  '(progn
    (setq printval (getslotvalue slotnum item))
    (selectq (getslotvaluetype slotnum item)
	     (CONSTANT  (slotitemform printval))
	     (LOCAL     (cond ((eq (punbound) (cdr printval))
			       (list '*var* (car printval)))
			      ((equivclassp (cdr printval))
			       (list (list '*var* (car printval))
				     ; Unfortunate kludge to get rid of \'s.
				     (ncons 'pearlequals)
				     (equivclassform (cdr printval))))
			      ( t (list (list '*var* (car printval))
					; Unfortunate kludge to get rid of \'s.
					(ncons 'pearlequals)
					(slotitemform (cdr printval))))))
	     (ADJUNCT   (list (slotitemform (car printval))
			      (ncons 'pearlequals)
			      (let ((var (cdr printval)))
				   (cond ((dtpr var)
					  (list '*var* (car var)))
					 ( t (list '*global* var))))))
	     (GLOBAL    (cond ((eq (punbound) (eval printval))
			       (list '*global* printval))
			      ((equivclassp (eval printval))
			       (list (list '*global* printval)
				     ; Unfortunate kludge to get rid of \'s.
				     (ncons 'pearlequals)
				     (equivclassform (eval printval))))
			      ( t (list (list '*global* printval)
					; Unfortunate kludge to get rid of \'s.
					(ncons 'pearlequals)
					(slotitemform (eval printval)))))))))

(de structureform (item)
  (let* ((curlist (ncons nil))
	 (defblock (getdefinition item))
	 (basehooks (getbasehooks defblock))
	 ppset
	 printvar
	 printval)
	(cond ((and *uniqueprint*
		    ; if there then return it.
		    (cdr (assq item *uniqueprintlist*))))
	      ( t (tconc curlist (getpname defblock))
		  (and *fullprint*
		       basehooks
		       (tconc curlist (cons 'if basehooks)))
		  (and *uniqueprint*
		       (push (cons item (car curlist))
			     *uniqueprintlist*))
		  (for slotnum 1 (getstructlength defblock)
		       (tconc curlist
			      (nconc (ncons (car
					     (getslotname slotnum defblock)))
				     (ncons (slotform))
				     (and *fullprint*
					  (mapcar (function convertpreds)
						  (getpred slotnum item)))
				     (and *fullprint*
					  (getslothooks slotnum item)))))
		  (car curlist)))))

; Convert any combination of PEARL and Lisp items (possibly from internal
; form) to a printable list structure.
(de allform (item)
  (cond ((hunkp item)
	 (selectq (gettypetag item)
		  (*pearlinst* (structureform item))
		  (*pearlsymbol* (symbolform item))
		  (*pearldef* (defform item))
		  (*pearldb*  (list 'database: (getdbname item)))
		  (*pearlinactivedb*  (list 'Inactive 'Database))
		  (otherwise item))) ; arbitrary hunk?.
	((streamp item) (streamform item))
	((equivclassp item) (equivclassform item))
	((atom item) item)
	((dtpr item) (cons (allform (car item))
			   (allform (cdr item))))
	; Else return item (arbitrary pieces of core?).
	( t item)))

; Convert a PEARL item in full detail and SPRINT the result.
(de fullform (item)
  (let ((*fullprint* t)
	(*abbrevprint* nil)
	(*uniqueprintlist* nil))
       (allform item)))

; Convert a PEARL item using abbreviations and SPRINT the result.
(de abbrevform (item)
  (let ((*abbrevprint* t)
	(*fullprint* nil)
	(*uniqueprintlist* nil))
       (allform item)))

; Normal function to convert a PEARL item and SPRINT the result.
(de valform (item)
  (let ((*fullprint* nil)
	(*abbrevprint* nil)
	(*uniqueprintlist* nil))
       (allform item)))

; Convert any PEARL item using whatever the current settings of 
;   *abbrevprint*, *fullprint* and *uniqueprint* are,
;   and SPRINT the result.
; BUT, don't bother if *quiet* is non-nil.
(de allprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (allform item) lmar rmar))
  '*invisible*)
 
(de structureprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (structureform item) lmar rmar))
  '*invisible*)
 
(de symbolprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (symbolform item) lmar rmar))
  '*invisible*)
 
(de streamprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (streamform item) lmar rmar))
  '*invisible*)
 
(de fullprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (fullform item) lmar rmar))
  '*invisible*)
 
(de valprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (valform item) lmar rmar))
  '*invisible*)
 
(de abbrevprint (item &optional (lmar 0) (rmar 0))
  (or *quiet*
      (sprint (abbrevform item) lmar rmar))
  '*invisible*)
 
; Run some commands but silence any printing it normally does.
(df quiet (command)
  (let ((*quiet* t))
       (eval `(progn ,@command))))

; Print out a data base, printing only buckets that have something in them.
(de printdb (&optional (db *db*))
  (let ((db1 (getdb1 db))
	(db2 (getdb2 db))
	bucket)
       (or (databasep db)
	   (progn (msg t "PRINTDB: Argument is not a database." t)
		  (pearlbreak)))
       (msg t "DB-Name: " (getdbname db))
       (msg t "Active: " (getdbactive db))
       (msg t "Children: " (mapcar (function pname) (getdbchildren db)))
       (msg t "Parent: " (pname (getdbparent db)))
       (msg t "DB1:")
       (and db1
	    (for slotnum 0 (1- *db1size*)
		 (and (setq bucket (remq '*db* (cxr slotnum db1)))
		      (progn (msg t "    " slotnum ": ")
			     (pearlprintfn bucket)))))
       
       (msg t "DB2:")
       (and db2
	    (for slotnum 0 (1- *db2size*)
		 (and (setq bucket (remq '*db* (cxr slotnum db2)))
		      (progn (msg t "    " slotnum ": ")
			     (pearlprintfn bucket)))))
       '*invisible*))

; Print complete information on the internal values stored in a structure
;    and its definition (or a definition and its default instance).
(de debugprint (item)
  (let (def name)
       (cond ((definitionp item)
	      (setq def item)
	      (setq item (getdefaultinst def)))
	     ( t  (setq def (getdefinition item))))
       (and (setq name (getabbrev item))
	    (msg t "******** " name " ********"))
       (msg t "Definition:")
       (msg t "   Unique\#: " (getuniquenum def))
       (msg "  Length: " (getstructlength def))
       (msg "  DefaultInst: " (getdefaultinst def))
       (msg t "   Isa: " (getisa def))
       (msg t "   Pname: " (getpname def))
       (msg "  HashAlias: " (gethashalias def))
       (msg "  ExpansionList: " (getexpansionlist def))
       (msg t "   BaseIfs: " (getbasehooks def))
       (msg t "Individual:")
       (msg "  Abbrev: " (getabbrev item))
       (msg t "   AList: " (getalist item))
       (msg "  AListcp: " (getalistcp item))
       (for slotnum 1 (getstructlength def)
	    (msg t t "***Slotnum " slotnum
		 " : " (getslotname slotnum def))
	    (msg t "Formatinfo: " (getformatinfo slotnum def))
	    (msg "  HashInfo: " (gethashinfo slotnum def))
	    (msg "  Enforce: " (getenforce slotnum def))
	    (msg "  Type: " (getslottype slotnum def))
	    (msg "  PPSet: " (getppset slotnum def))
	    (msg t "ValueType: " (getslotvaluetype slotnum item))
	    (msg "  Internal Value: " (getslotvalue slotnum item))
	    (msg t "Value: " (getvalue slotnum item))
	    (msg "  Preds: " (getpred slotnum item))
	    (msg "  SlotIfs: " (getslothooks slotnum item)))
       '*invisible*))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the print functions for use with the top level, msg, and the
;    trace, break, etc. packages.

; standard trace print should use allform after turning off tracing.
(de pearltraceprintfn (*traceval*)
  ; Set the $tracemute flag to t so that tracing won't be done
  ; inside allform.
  (let ((\$tracemute t))
       (print (allform *traceval*))))

; standard showstack print should use allform.
(de pearlshowstackprintfn (*showstackval*)
  (print (allform *showstackval*)))

; standard break print should use allform.
(de pearlbreakprintfn (*breakval*)
  (print (allform *breakval*)))

; standard fix print should use allform.
(de pearlfixprintfn (*fixval*)
  (print (allform *fixval*)))

; msg should allform, unless *invisible*.
(de msgprintfn (*msgval*)
  (or (eq '*invisible* *msgval*)
      (patom (allform *msgval*))))

; printing in a trace-break should allprint.
(de pearltracebreakprintfn (*printval*)
  (allprint *printval* 3))

; standard print should allprint.
(de pearlprintfn (*printval*)
  (allprint *printval* 3))

; standard dskin print should use allform unless an atom.
(de dskprintfn (*dskval*)
  (cond ((atom *dskval*) (patom *dskval*))
	( t (print (allform  *dskval*)))))
 

; vi: set lisp:
