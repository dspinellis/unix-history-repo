;;;;;;;;;;;;;;;;;;;;;;;;;;;;; create.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for creating, copying, and merging structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Create a new structure of one of the five types:
;     BASE:        build a new structure with all new slots.
;     EXPANDED:    build new slots in addition to slots inherited from
;                     a BASE or EXPANDED structure.
;     INDIVIDUAL:  create an instance of a BASE or EXPANDED structure,
;                     filling in slots or inheriting defaults from above.
;     PATTERN:     create an instance of a BASE or EXPANDED structure but
;                     fill in unspecified slots with ?*ANY*.
;     FUNCTION:    build a new structure with slots describing the
;                     arguments to the function.
 
; Generalized syntax for this function is:
;
; (CREATE <StructureType> <ExpandedorBaseName> <NewItemName>
;         [{HashingInfo} <OldSlotName> {{{:=} <SlotValue>} | ^}
;                        { : <Variable> }
;                        {<ListOfRestrictionStructureOrSlotIfNames>} ]
;    . . . . . . .
;         [{HashingInfo} <NewSlotName> <Type> {{{:=} <SlotValue>} | ^}
;                        { : <Variable> }
;                        {<ListOfRestrictionStructureOrSlotIfNames>} ] )
 
; BASE structures have no <ExpandedorBaseName> and only new slots.
; EXPANDED structures should have at least one new slot and inherit
;       default values from the <ExpandedorBaseName>.
; INDIVIDUAL structures have only old slots and inherit default values from
;       the <ExpandedorBaseName>;  if the <NewItemName> occurs, the atom
;       <NewItemName> is set to point to the internal form which is also
;       returned as the value of CREATE.
; PATTERN structures have only old slots and all unspecified slots are
;       set to ?*ANY* rather than inheriting a default.
; FUNCTION structures have no <ExpandedOrBaseName> and only new slots.
;       They are interpreted as functions to be run rather than structures
;       to be accessed when they are MATCHed, FETCHed or PATHed.
;
; The structure created is always stored in the SPECIAL variable *LASTCREATED*
;       in addition to the <NewItemName> if specified and the atom formed
;       by prepending a 'd:' to the front of the <ExpandedOrBaseName>.
;
; If the SlotValue for a slot in a BASE or EXPANDED structure is preceded
;       by a :=, then the slot is filled with this value but it is not
;       used as the default for this slot in future INDIVIDUALS and
;       EXPANDEDS.
; If the SlotValue is not preceded by a :=, then the value represents a
;       default to be inherited by INDIVIDUALs and new EXPANDEDS.
 
; This just sets the two atoms *toplevelp* and *currentcreatetype*
;   and calls the real workhorse "insidecreate".
(df create (l)
  (setq *toplevelp* t)
  (setq *currentcreatetype* (car l))
  (apply (function insidecreate) l))
 
; Pick apart the atoms before the slots, handle them and pass the
;    rest on to the appropriate version of create XXX.
(df insidecreate (l)
  (let ((type (car l))
	(abbrev (cond (*toplevelp* '*buildabbrev*)
		      ( t nil)))
	(name1 (cadr l))
	(name2 (caddr l))
	(name3 (cadddr l))
	slots)
       (cond ((reallitatom name2)
	      (setq abbrev name2)
	      (setq slots (cdddr l))
	      (cond ((reallitatom name3)
		     (setq abbrev name3)
		     (setq slots (cddddr l)))
		    ( t  (setq name3 name2))))
	     ( t (setq name3 (setq name2 name1))
		 (setq slots (cddr l))))
       (and (memq type '(ind individual pat pattern))
	    (eq name1 name3)
	    (setq name3 '*lastcreated*))
       (set name3
	    (setq *lastcreated*
		  (selectq type
			   ((individual ind)
			    (createindividual name1 abbrev slots))
			   (base
			    (createbase name1 abbrev slots))
			   ((expanded exp)
			    (createexpanded name1 name2 abbrev slots))
			   ((pattern pat)
			    (createpattern name1 abbrev slots))
			   ((function fn)
			    (createfunction name1 abbrev slots))
			   (otherwise (msg t "CREATE: Illegal selector: " type
					   " in created structure: " l t)
				      (pearlbreak)))))))
 
; Create a new structure and insert it in the database.
(defmacro dbcreate (&rest rest)
  `(insertdb (create .,rest)))

(defmacro cb (&rest rest)
  `(create base .,rest))

(defmacro ci (&rest rest)
  `(create individual .,rest))

(defmacro ce (&rest rest)
  `(create expanded .,rest))

(defmacro cp (&rest rest)
  `(create pattern .,rest))

(defmacro cf (&rest rest)
  `(create function .,rest))
 
(defmacro base (&rest rest)
  `(create base .,rest))

(defmacro ind (&rest rest)
  `(create individual .,rest))

(defmacro individual (&rest rest)
  `(create individual .,rest))

(defmacro pexp (&rest rest)
  `(create expanded .,rest))

(defmacro expanded (&rest rest)
  `(create expanded .,rest))

(defmacro pat (&rest rest)
  `(create pattern .,rest))

(defmacro pattern (&rest rest)
  `(create pattern .,rest))

(defmacro fn (&rest rest)
  `(create function .,rest))
 
(defmacro pfunction (&rest rest)
  `(create function .,rest))
 
;  Put a *VAR* variable in the structure's assoc-list and return the cons-cell.
(defmacro installvar (varname)
  `(cond ((eq '*any* ,varname) *any*conscell*)
	 ; else, if there, return it.
	 ((assq ,varname (getalist *currenttopcreated*)))
	 ; else, add it (which also returns the special conscell).
	 (  t   (addalist ,varname *currenttopcreated*))))
 
; Install an adjunct variable in the slot.
(defmacro installadjunct (adjunctvar)
  `(let (var)
	(cond ((dtpr ,adjunctvar)
	       (setq var (cadr ,adjunctvar))
	       (selectq (car ,adjunctvar)
			(*var* (installvar var))
			(*global* var)
			(otherwise
			 (msg t "CREATE: no adjunct variable given after colon "
			      "-- rest of slot is: " ,adjunctvar slot t)
			 (pearlbreak))))
	      (  t  (msg t "CREATE: no adjunct variable given after colon. "
			 "Rest of slot is: " ,adjunctvar slot t)
		    (pearlbreak)))))

(dm handlepossibleadjunctvar (none) ; but assumes SLOT, SLOTVALUE, & VALUETYPE.
  '(let ((adjunctvar (car slot)))
	(and (eq adjunctvar ':)
	     (cond ((neq valuetype 'CONSTANT)
		    (msg t "CREATE: Adjunct variables not allowed in "
			 "slots whose values are also variables." t)
		    (pearlbreak))
		   ( t (setq slot (cdr slot)) ; throw away ":".
		       (setq adjunctvar (pop slot))
		       (setq valuetype 'ADJUNCT)
		       (setq slotvalue (cons slotvalue
					     (installadjunct adjunctvar))))))))

; Ensure that value is of type TYPENUM (used after ! or on value in atom
;    where setof value expected).  Value returned (t / never) is used
;    only in evaluating atom.  If error, doesn't return.
(de enforcetype (value typenum)
  (or (selectq typenum
	       (0 (structurep value))
	       (1 (psymbolp value))
	       (2 (numberp value))
	       (3 (not (reallitatom value)))
	       (otherwise
		(apply (function and)
		       (mapcar (funl (singlevalue)
				     (enforcetype singlevalue
						  (- typenum 4)))
			       value))))
      (progn (msg t "CREATE: Value after ! or bound to atom in SETOF "
		  "slot is of wrong type.  Value is: " value t)
	     (pearlbreak))))

; Get the value for a slot.
; If preceded by an ! then it is already in internal form but verify this.
; If is preceded by a $ then it should be evaluated before continuing
;    processing (on its value).
(dm constructvalue (none)
  '(let ((typenum (getslottype slotnum defblock))
	 (ppset   (getppset    slotnum defblock)))
	(selectq (car slot)
		 (\!  (setq slot (cdr slot))
		      (setq newvalue (eval (pop slot)))
		      (enforcetype newvalue typenum)
		      (setq valuetype 'CONSTANT)
		      (setq slotvalue newvalue))
		 (\$  (setq slot (cdr slot))
		      (setq newvalue (eval (pop slot)))
		      (setq valuetype 'CONSTANT)
		      (setq slotvalue (buildvalue newvalue typenum ppset)))
		 (otherwise
		  (cond ((and (dtpr (car slot))
			      (eq (caar slot) '*var*))
			 (setq valuetype 'LOCAL)
			 (setq newvalue (cadr (pop slot)))
			 (setq slotvalue (installvar newvalue)))
			((and (dtpr (car slot))
			      (eq (caar slot) '*global*))
			 (setq valuetype 'GLOBAL)
			 (setq slotvalue (cadr (pop slot))))
			(  t  (setq valuetype 'CONSTANT)
			      (setq slotvalue
				    (buildvalue (pop slot) typenum ppset))))))))
 
; Generate the default value for slots of the given type.
(defmacro defaultfortype (typenum)
  `(selectq ,typenum
	    (0 (eval (instatom 'nilstruct)))
	    (1 (eval (symatom 'nilsym)))
	    (2 0)
	    (3 nil)))
 
; Look at the ISA to find the default value, or the else use
;    the default default for that type.
(defmacro inheritvalue (structdef)
  `(let ((isa ,structdef))
	(cond ((or (null isa)
		   (not (getenforce slotnum isa)))
	       (setq slotvalue (defaultfortype (getslottype slotnum defblock)))
	       (setq valuetype 'CONSTANT))
	      ( t (let ((default (getdefaultinst isa)))
		       (setq slotvalue (getslotvalue slotnum default))
		       (setq valuetype (getslotvaluetype slotnum default)))))))
 
; Look for predicates and hooks.  Use tconc to keep in order.
(dm handlepredicatesandhooks (none)
  '(progn
    (setq predlist (ncons nil))
    (setq slothooklist (ncons nil))
    (while (setq fcn (pop slot))
	   (cond ((atom fcn)
		  (cond ((eq fcn 'instead)
			 ; Don't inherit hooks.
			 (putpred slotnum nil valblock))
			((memq fcn '(if hook))
			 ; A hook follows.
			 (tconc slothooklist (cons (pop slot) (pop slot))))
			; Structure predicate.
			((structurenamep fcn)
			 (tconc predlist (eval (defatom fcn))))
			; Otherwise, a predicate name.
			( t (tconc predlist fcn))))
		 ; Otherwise an s-expression predicate.
		 ( t (tconc predlist fcn))))
    (putpred      slotnum
		  (nconc (car predlist) (getpred slotnum valblock))
		  valblock)
    (putslothooks slotnum
		  (nconc (car slothooklist) (getslothooks slotnum valblock))
		  valblock)))

; Build a new slot in the current structure.
(dm buildslot (none)
  '(progn
    (setq slotname (pop slot))
    (clearhashandformat slotnum defblock)
    ; To gather hashing and enforce information before installing in defblock.
    (setq hashcollect 0)
    (setq reqstruct nil)

    ; Check for hashing marks first.
    (while (selectq slotname
		    ; First starred slot used for > hashing if no & present.
		    (*   (and (\=& 0 first*edslot)
			      (setq first*edslot (minus slotnum)))
			 (addhash*   hashcollect))
		    (**  (addhash**  hashcollect))
		    (*** (addhash*** hashcollect))
		    (&   (cond ((not (\=& 0 hashalias))
				(msg t "CREATE: Only 1 hash alias (&) or "
				     "selected slot (^) allowed in: "
				     newname t)
				t)
			       ( t (setq hashalias slotnum))))
		    (^   (cond ((not (\=& 0 hashalias))
				(msg t "CREATE: Only 1 hash alias (&) or "
				     "selected slot (^) allowed in: "
				     newname t)
				t)
			       ( t (setq hashalias (minus slotnum)))))
		    (&&  (cond ((not (\=& 0 hashfocus))
				(msg t "CREATE: Only 1 hash focus (&&) "
				     "allowed in: " newname t)
				t)
			       ( t (setq reqstruct t)
				   (setq hashfocus slotnum))))
		    (:   (addhash:   hashcollect))
		    (::  (addhash::  hashcollect))
		    (>   (addhash>   hashcollect))
		    (<   (addhash<   hashcollect)))
	   (setq slotname (pop slot)))
    (and (\=& 0 (length slot))
	 (progn (msg t "CREATE: Missing slot name and/or type in slot number "
		     slotnum " of structure: " newname t)
		(pearlbreak)))

    ;  Slotname now holds the slotname.  Should be checked for duplicates!!
    (putslotname slotnum (ncons slotname) defblock)

    ; Now look for the type.
    (setq typenum 0)
    (setq slottype (pop slot))
    (while (selectq slottype
		    (struct (setq reqstruct nil)
			    nil)     ; i. e., add 0 to TYPENUM.
		    (symbol (setq typenum (1+ typenum))  nil)
		    (int    (setq typenum (+ 2 typenum))  nil)
		    (lisp   (cond ((not (\=& 0 typenum))
				   (msg t "CREATE: <setof lisp> not allowed. "
					"Type changed to <lisp> in slot "
					slotname " of " newname t)
				   (setq typenum 3) nil)
				  ((not (\=& 0 hashcollect))
				   (setq hashcollect 0)
				   (msg t "CREATE: No hashing allowed on "
					"<lisp> slots in slot " slotname
					" of " newname t)))
			    (setq typenum 3) nil)
		    (setof (setq typenum (+ 4 typenum))    t)
		    (otherwise
		     ; Either an ordinal type ==> integer,
		     ;   or a structure name ==> struct, or an error.
		     (cond ((memq slottype *ordinalnames*)
			    (setq typenum (+ 2 typenum)) nil)
			   ((structurenamep slottype)
			    (setq reqstruct nil)
			    nil)     ; i. e., add 0 to TYPENUM.
			   (  t  (msg t "CREATE: Illegal type: " slottype
				      " in slot: " slotname " of " newname t)
				 nil))))
	   (setq slottype (pop slot)))
    (and reqstruct
	 (progn (msg t "CREATE: && hashing only allowed on STRUCT slots."
		     t " Bad slot is called " slotname
		     " and is of type " slottype t)
		(pearlbreak)))
    ; Save the last word of the type which is possibly a structure or
    ; ordinal type name for future use.
    (putppset slotnum slottype defblock)
    (putslottype slotnum typenum defblock)

    ; Next, look for a value, or ^ to inherit from above;
    ;     these may be preceded by := or == to determine future
    ;     "enforcing" (should be less strong term) of this default.
    (setq slotvalue nil)
    (setq valuetype nil)
    (setq enforce (pop slot))
    (selectq enforce
	     (:\=  (cond ((eq (car slot) '^)
			  (setq slot (cdr slot))
			  (inheritvalue nil))
			 ( t (constructvalue))))
	     (\=\= (addenforce hashcollect)
		   (cond ((eq (car slot) '^)
			  (setq slot (cdr slot))
			  (inheritvalue nil))
			 ( t (constructvalue))))
	     ((^ nil)
	      (addenforce hashcollect)
	      (inheritvalue nil))
	     (otherwise (push enforce slot)
			(addenforce hashcollect)
			(constructvalue)))

    (handlepossibleadjunctvar)

    ; Hash, enforce, slotvalue and valuetype can now be installed.
    (puthashandenforce slotnum hashcollect defblock)
    (putslotvaluetype  slotnum valuetype   valblock)
    (putslotvalue      slotnum slotvalue   valblock)

    (handlepredicatesandhooks)))
 
; Create a new structure of type BASE:  a structure with ALL NEW slots.
(de createbase (newname abbrev slots)
  (and (eq newname 'nilstruct)
       (boundp (defatom 'nilstruct))
       (progn (msg t "CREATE BASE: Cannot redefine nilstruct." t)
	      (pearlbreak)))
  (and (structurenamep newname)
       *warn*
       (msg t "CREATE BASE: Warning: Creating a new definition"
	    " of an existing structure: " newname t))
  (prog (defblock slotname slottype enforce fcn ppset slot length isa
		  typenum valblock predlist slothooklist
		  first*edslot basehooks basehookbefore newvalue reqstruct
		  hashalias hashfocus hashcollect slotvalue valuetype)

	; Process base hooks if the first "slot" is named "if" or "hook".
	(cond ((memq (caar slots) '(if hook))
	       (setq basehookbefore (cdr (pop slots)))
	       (setq basehooks (ncons nil))

	       ; Use tconc to preserve order.
	       (while basehookbefore ; is not NIL
		      (tconc basehooks (cons (pop basehookbefore)
					     (pop basehookbefore))))
	       (setq basehooks (car basehooks)))
	      ( t (setq basehooks nil)))

	; Allocate hunks for definition and default instance (valblock)
	;     based on number of slots.
	(setq defblock (allocdef (setq length (length slots))))
	(setq valblock (allocval length))
	(puttypetag '*pearldef* defblock)
	(puttypetag '*pearlinst* valblock)
	(cond (*toplevelp* (setq *currenttopcreated* valblock)
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
	(putdefaultinst valblock defblock)
	(set (instatom newname) valblock)
	(set (defatom newname) defblock)
	(and abbrev
	     (cond ((eq abbrev '*buildabbrev*)
		    (putabbrev (instatom newname) valblock))
		   ( t (putabbrev abbrev valblock))))
	(putuniquenum (newnum) defblock)
	(putstructlength length defblock)
	(putisa nil defblock)
	(putexpansionlist nil defblock)
	(putbasehooks basehooks defblock)
	(putpname newname defblock)

	(setq hashalias 0)
	(setq hashfocus 0)
	(setq first*edslot 0)
	(for slotnum 1 length
	     (setq slot (pop slots))
	     (buildslot))

	(cond ((\=& 0 hashalias) (puthashalias first*edslot defblock))
	      ( t  (puthashalias hashalias defblock)))
	(puthashfocus hashfocus defblock)

	(return valblock)))
 
; Create a new individual just for this slot.
(defmacro buildstructvalue (struct)
  `(cond ((and (atom ,struct)                      ; if an atom
	       (boundp ,struct)                    ; and bound
	       (structurep (eval ,struct)))        ; to a structure,
	  (eval ,struct))                          ; evaluate it.
	 ; Otherwise, recursively call create.
	 ( t (selectq (car ,struct)
		      ; New create type in slot.
		      ((ind individual pat pattern fn function
			    base exp expanded)
		       (let ((*currentcreatetype* (car ,struct)))
			    (apply (function insidecreate) ,struct)))
		      (otherwise
		       ; Otherwise, use current create type.
		       (apply (function insidecreate)
			      (cons *currentcreatetype* ,struct)))))))
 
; Get a pointer to the symbol.
(defmacro buildsymbolvalue (sym)
  `(cond ((symbolnamep ,sym)   (eval (symatom ,sym)))
	 ; If not a symbol name, then ...
         ((and (atom ,sym)                      ; if an atom
	       (boundp ,sym)                    ; and bound
	       (psymbolp (eval ,sym)))          ; to a symbol,
	  (eval ,sym))                          ; evaluate it.
	 ; Otherwise, error.
	 ( t  (msg t "CREATE: " ,sym " is used in a slot of type SYMBOL but "
		   "neither is the name of a symbol nor evaluates to one." t)
	      (pearlbreak))))
 
; Get an integer using PPSET if not an integer.
(defmacro buildintvalue (intval bppset)
  `(let (assocval)
	(cond ((numberp ,intval) ,intval)
	      ; Ordinal type given for ppset.
	      ((and ,bppset    ; is not NIL.
		    (setq assocval (assq ,intval (eval (ordatom ,bppset)))))
	       (cdr assocval))
	      ; Some other atom which is bound to an integer.
	      ((and (atom ,intval)
		    (boundp ,intval)
		    (numberp (eval ,intval)))
	       (eval ,intval))
	      ; Otherwise, error.
	      ( t (msg t "CREATE: Unbound atom or non-integer value: "
		       ,intval " in integer slot." t) 
		  (pearlbreak)))))
 
; Construct a new value of the specified type using the pplist if necessary
(de buildvalue (value typenum ppset)
  (selectq typenum
	   (0 (buildstructvalue value))
	   (1 (buildsymbolvalue value))
	   (2 (buildintvalue    value ppset))
	   (3 value) ; i.e., could be anything they want.
	   (otherwise
	    (cond ((and (atom value)
			(boundp value)
			(enforcetype (eval value) typenum))
		   (eval value))
		  ( t (mapcar (funl (singlevalue)
				    (buildvalue singlevalue
						(- typenum 4) ppset))
			      value))))))
 
; Return the position number of SLOTNAME in structure DEFBLOCK.
(defmacro slotnametonumber (slotname defblock)
  `(progn
    (setq slotlocation 0)
    (for slotnum 1 (getstructlength ,defblock)
	 (and (memq ,slotname (getslotname slotnum ,defblock))
	      (setq slotlocation slotnum)))
    slotlocation))
 
; Find the slotname in SLOT, put it in SLOTNAME, and find its SLOTNUM.
(dm findslotnum (none)
  '(progn
    (setq slotname slot)
    (while (memq (car slotname) '(* ** *** & ^ && : :: < > +))
	   (setq slotname (cdr slotname)))
    (cond ((and (dtpr (cadr slotname))
		(eq '*slot* (car (cadr slotname))))
	   (setq slotname (cadr (cadr slotname)))
	   (minus (slotnametonumber slotname olddefblock)))
	  ( t (setq slotname (car slotname))
	      (slotnametonumber slotname olddefblock)))))
 
; Look up through ISA links and add name to Expansion Lists.
;    Assumes PROG vars NEWNAME and OLDDEFBLOCK.
(dm addtoexpansionlists (none)
  '(progn
    (setq isa olddefblock)
    (while isa         ; is not null
	   (putexpansionlist (cons defblock (getexpansionlist isa)) isa)
	   (setq isa (getisa isa)))))
 
; Copy definition for one slot.
(dm copyslice (none)
  '(progn
    (putslottype      slotnum (getslottype      slotnum olddefblock) defblock)
    (putslotname      slotnum (getslotname      slotnum olddefblock) defblock)
    (putppset         slotnum (getppset         slotnum olddefblock) defblock)
    (puthashandformat slotnum (gethashandformat slotnum olddefblock) defblock)))
 
; Copy default values, predicates, and hooks for one slot.
(dm copyslot (none)
  '(progn
    (putslotvaluetype slotnum (getslotvaluetype slotnum oldvalblock) valblock)
    (putslotvalue     slotnum (getslotvalue     slotnum oldvalblock) valblock)
    (putpred          slotnum (getpred          slotnum oldvalblock) valblock)
    (putslothooks     slotnum (getslothooks     slotnum oldvalblock) valblock)))
 
; Copy an old slot from an ISA into the current structure.
(dm fillbaseslot (none)
  '(progn
    (cond ((<& slotnum 0)
	   (setq slotnum (minus slotnum))
	   (setq newslotnamep t))
	  ( t (setq newslotnamep nil)))

    ; First check for changed hashing.
    (setq slotname (pop slot))
    (clearhashandformat slotnum defblock)
    (setq hashcollect 0)
    (while (selectq slotname
		    (*   (and (\=& 0 first*edslot)
			      (setq first*edslot (minus slotnum)))
			 (addhash*   hashcollect) t)
		    (**  (addhash**  hashcollect) t)
		    (*** (addhash*** hashcollect) t)
		    (&   (cond ((not (\=& 0 hashalias))
				(msg t "CREATE EXPANDED: Only 1 hash alias "
				     "(&) or selected slot (^) allowed in: "
				     newname t)
				t)
			       ( t (setq hashalias slotnum))))
		    (^   (cond ((not (\=& 0 hashalias))
				(msg t "CREATE EXPANDED: Only 1 hash alias "
				     "(&) or selected slot (^) allowed in: "
				     newname t)
				t)
			       ( t (setq hashalias (minus slotnum)))))
		    (&&  (cond ((not (\=& 0 hashfocus))
				(msg t "CREATE EXPANDED: Only 1 hash focus "
				     "(&&) allowed in: " newname t)
				t)
			       ( t (setq hashfocus slotnum))))
		    (:   (addhash:   hashcollect) t)
		    (::  (addhash::  hashcollect) t)
		    (>   (addhash>   hashcollect) t)
		    (<   (addhash<   hashcollect) t)
		    (+   (setq hashcollect (gethashinfo slotnum olddefblock))
			 t))
	   (setq slotname (pop slot)))

    (and (\=& 0 (length slot))
	 (progn (msg t "CREATE EXPANDED: Missing slot name and/or value in: "
		     newname t)
		(pearlbreak)))

    (and newslotnamep
	 (pop slot)
	 (addslotname slotnum slotname defblock))

    ; Next, check for value or ^, possibly preceded by := or ==.
    (setq enforce (pop slot))
    (selectq enforce
	     (:\= (cond ((eq (car slot) '^)  ; a waste.
			 (setq slot (cdr slot))
			 (inheritvalue (getisa defblock)))
			( t (constructvalue))))
	     (\=\= (addenforce hashcollect)
		   (cond ((eq (car slot) '^)
			  (setq slot (cdr slot))
			  (inheritvalue (getisa defblock)))
			 ( t (constructvalue))))
	     ((^ nil)  (addenforce hashcollect)
		       (inheritvalue (getisa defblock)))
	     (otherwise (push enforce slot)
			(addenforce hashcollect)
			(constructvalue)))
    
    (handlepossibleadjunctvar)

    ; Hash, enforce, slotvalue and valuetype can now be installed.
    (puthashandenforce slotnum hashcollect defblock)
    (putslotvaluetype  slotnum valuetype   valblock)
    (putslotvalue      slotnum slotvalue   valblock)

    (handlepredicatesandhooks)))
 
; Create a new structure of type EXPANDED:  build new slots in
;    addition to slots inherited from a BASE or EXPANDED structure.
(de createexpanded (basename newname abbrev slots)
  (and (eq newname 'nilstruct)
       (progn (msg t "CREATE EXPANDED: Cannot redefine nilstruct." t)
	      (pearlbreak)))
  (and (structurenamep newname)
       *warn*
       (msg t "CREATE EXPANDED: Warning: Creating a new definition of "
	    "an existing structure: " newname t))
  (or (structurenamep basename)
      (progn (msg t "CREATE EXPANDED: " basename
		  " is not the name of a previously declared structure." t
		  "     New name is " newname ". Slots are: " slots t)
	     (pearlbreak)))
  (prog (defblock valblock oldvalblock olddefblock slotname
		  slottype enforce slot oldlength length slotnum
		  typenum slotnumlist beginslots predlist slothooklist
		  fcn ppset isa first*edslot slotlocation basehooks
		  basehookbefore newvalue result item reqstruct
		  newslotnamep hashalias hashfocus hashcollect
		  slotvalue valuetype)
	(setq olddefblock (eval (defatom basename)))
	(setq oldlength (getstructlength olddefblock))
	
	; Handle base hooks, if first "slot" is called "if" or "hook".
	(cond ((memq (caar slots) '(if hook))
	       (setq basehookbefore (cdr (pop slots)))
	       (setq basehooks (ncons nil))
	       (while basehookbefore ; is not NIL
		      (tconc basehooks (cons (pop basehookbefore)
					     (pop basehookbefore))))
	       (setq basehooks (nconc (car basehooks)
				      (getbasehooks olddefblock))))
	      ( t (setq basehooks (getbasehooks olddefblock))))
	
	; Create a list of slotnumbers for the slotnames in SLOTS,
	; meanwhile also determining the LENGTH.
	(setq beginslots slots)   ; save to process again.
	(setq slotnumlist (ncons nil))
	(setq length oldlength)
	(while (setq slot (pop slots))
	       (cond ((not (\=& 0 (setq slotnum (findslotnum))))
		      ; Old slot name or new name for old slot (negative).
		      (tconc slotnumlist slotnum))
		     ; Otherwise, new slot name: increase length.
		     (  t   (setq length (1+ length))
			    (tconc slotnumlist length))))
	(setq slotnumlist (car slotnumlist))
	(setq slots beginslots)
	
	; Allocate new hunks.
	(setq defblock (allocdef length))
	(setq valblock (allocval length))
	(puttypetag '*pearldef* defblock)
	(puttypetag '*pearlinst* valblock)
	(cond (*toplevelp* (setq *currenttopcreated* valblock)
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
	(putdefaultinst valblock defblock)
	(set (instatom newname) valblock)
	(set (defatom newname) defblock)
	(and abbrev
	     (cond ((eq abbrev '*buildabbrev*)
		    (putabbrev (instatom newname) valblock))
		   ( t (putabbrev abbrev valblock))))
	(putuniquenum (newnum) defblock)
	(putstructlength length defblock)

	; Set up the hierarchy of ISAs.
	(putisa olddefblock defblock)
	(putexpansionlist nil defblock)
	(addtoexpansionlists)

	(putbasehooks basehooks defblock)
	(putpname newname defblock)
	(setq oldvalblock (getdefaultinst olddefblock))

	; (puthashalias 0   defblock)
	(setq hashalias 0)
	(setq hashfocus 0)
	(or (<& (setq first*edslot (gethashalias olddefblock)) 0)
	    (setq first*edslot 0))
	; Copy old slots in first.
	(for slotnum 1 oldlength
	     (copyslice)
	     (copyslot olddefblock))
	; Run base hooks attached to the base we are expanding.
	(and (getbasehooks olddefblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '<expanded *runexpandedhooks*)
	     (setq valblock item))

	; For each slot, if it's new, build a new slot;
	;                if it's old, fill it in differently.
	(while (setq slot (pop slots))
	       (setq slotnum (pop slotnumlist))
	       (cond ((>& slotnum oldlength)
		      (buildslot))
		     (  t  (fillbaseslot))))
	(cond ((\=& 0 hashalias) (puthashalias first*edslot defblock))
	      ( t (puthashalias hashalias defblock)))
	(cond ((\=& 0 hashfocus) (puthashfocus (gethashfocus olddefblock)
					       defblock))
	      ( t (puthashfocus hashfocus defblock)))

	; Run base hooks attached to the base we are expanding.
	(and (getbasehooks olddefblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '>expanded *runexpandedhooks*)
	     (setq valblock item))
	(return valblock)))
 
; Fill in an individual slot with the value specified.  If the value is
;   prefaced by the character "^" then the value should be inherited from
;   above but there are still predicates and/or IFs to process.
(dm fillindivslot (none)
  '(progn
    (setq slotname (pop slot))
    ; Find slot number.
    (and (\=& 0 (setq slotnum (slotnametonumber slotname defblock)))
	 (progn (msg t "CREATE: Undefined slot: " slotname
		     ", in individual or pattern: " basename)
		(pearlbreak)))
    (cond ((\=& 0 (length slot))
	   (msg t "Missing value in: CREATE INDIVIDUAL (or PATTERN) "
		basename ".  Remaining slots are: " slots t)
	   (pearlbreak))
	  ; If ^, inherit.
	  ((eq (car slot) '^)
	   (setq slot (cdr slot))
	   (inheritvalue defblock))
	  ; Otherwise, construct a new value and insert.
	  ( t (constructvalue)))
    
    (handlepossibleadjunctvar)

    ; Store type and value.
    (putslotvaluetype  slotnum valuetype   valblock)
    (putslotvalue      slotnum slotvalue   valblock)

    (handlepredicatesandhooks)))
 
; Create a new structure of type INDIVIDUAL:  an instance of a
;    BASE or EXPANDED structure.  Slots are either filled explicitly
;    or they inherit defaults from above.
(de createindividual (basename abbrev slots)
  (or (structurenamep basename)
      (progn (msg t "CREATE INDIVIDUAL: " basename
		  " is not the name of a previously declared structure."
		  t "     Slots are: " slots t)
	     (pearlbreak)))
  (prog (defblock valblock slotname length slotnum oldvalblock
		  isa typenum ppset slot predlist slothooklist fcn
		  slotlocation newvalue result item
		  slotvalue valuetype)

	; Find definition and allocate hunk for individual.
	(setq defblock (eval (defatom basename)))
	(setq valblock (allocval (setq length (getstructlength defblock))))
	(puttypetag '*pearlinst* valblock)
	(cond (*toplevelp* (setq *currenttopcreated* valblock)
			   (setq *currentpearlstructure* valblock)
			   (initbothalists valblock)
			   (setq *currenttopalists* (getbothalists valblock))
			   ; Include the current environment in 
			   ;    the variable assoc-list.
			   (and *blockstack*
				(putalist (cdar *blockstack*) valblock))
			   (setq *toplevelp* nil))
	      ( t (putbothalists *currenttopalists* valblock)))

	(and abbrev
	     (cond ((eq abbrev '*buildabbrev*)
		    (putabbrev (eval `(newsym ,(getpname defblock))) valblock))
		   ( t (putabbrev abbrev valblock))))
	(putdef defblock valblock)
	(setq oldvalblock (getdefaultinst defblock))

	; Copy slots from old structure first, then run base hooks.
	(for slotnum 1 length
	     (copyslot defblock))
	(and (getbasehooks defblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '<individual *runindividualhooks*)
	     (setq valblock item))

	; Replace copied values for slots that are actually listed
	;    then run base hooks.
	(while (setq slot (pop slots))
	       (fillindivslot))
	(and (getbasehooks defblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '>individual *runindividualhooks*)
	     (setq valblock item))
	(return valblock)))
 
; Copy default values, predicates, and hooks for one slot.
(dm copypatternslot (none)
  '(progn
    (putslotvaluetype slotnum 'LOCAL                             valblock)
    (putslotvalue     slotnum *any*conscell*                     valblock)
    (putpred          slotnum (getpred      slotnum oldvalblock) valblock)
    (putslothooks     slotnum (getslothooks slotnum oldvalblock) valblock)))
 
; Create a new structure of type PATTERN:  an instance of a BASE or
;    EXPANDED structure.  Unspecified slots are filled with ?*ANY*.
(de createpattern (basename abbrev slots)
  (prog (defblock valblock oldvalblock slotname length slotnum isa
		  slotlocation slot predlist slothooklist fcn typenum
		  ppset newvalue result item slotvalue valuetype)
	(or (structurenamep basename)
	    (progn (msg t "CREATE PATTERN: " basename
			" is not the name of a previously declared structure."
			t)
		   (pearlbreak)))

	; Get definition and allocate hunk for pattern.
	(setq defblock (eval (defatom basename)))
	(setq valblock (allocval (setq length (getstructlength defblock))))
	(setq oldvalblock (getdefaultinst defblock))
	(puttypetag '*pearlinst* valblock)
	(cond (*toplevelp* (setq *currenttopcreated* valblock)
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
	(and abbrev
	     (cond ((eq abbrev '*buildabbrev*)
		    (putabbrev (eval `(newsym ,(getpname defblock))) valblock))
		   ( t (putabbrev abbrev valblock))))

	; Copy slot values from base and run base hooks on base structure.
	(for slotnum 1 length
	     (copypatternslot))
	(and (getbasehooks defblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '<pattern *runpatternhooks*)
	     (setq valblock item))

	; Fill in new values for any slots listed and run base hooks.
	(while (setq slot (pop slots))
	       (fillindivslot))
	(and (getbasehooks defblock)
	     (setq item valblock)
	     (checkrunhandlebasehooks1 '>pattern *runpatternhooks*)
	     (setq valblock item))
	(return valblock)))
 
; Create a new structure of type FUNCTION: a structure with slots
;    describing the arguments to the function of the same name.
(de createfunction (fcnname abbrev slots)
  ; Function must already be defined and be a lambda (expr).
  (cond ((islambda fcnname)
	 (putprop fcnname t 'functionstruct)
	 (createbase fcnname abbrev slots))
	( t  (msg t "CREATE FUNCTION: Only lambdas (exprs) allowed as "
		  "function structures: " fcnname slots t)
	     (pearlbreak))))
 
; vi: set lisp:
