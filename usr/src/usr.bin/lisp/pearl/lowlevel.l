;;;;;;;;;;;;;;;;;;;;;;;;;;;; lowlevel.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros (mostly) for accessing structures, symbols and definitions.
;    See the file "template" for a picture of how structures and
;    symbols and data bases are arranged to explain the simplest
;    of the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.
 
; Throughout the code for PEARL:
;    defblock:  will contain a definition of a structure,
;    valblock:  will contain an instance of a structure,
;    slotnum:   will contain a slot number to index into a structure.
; An attempt has been made throughout the rest to similarly name
;    things to be obvious.

; These macros are designed so that PEARL can be moved to a new Lisp
;     simply by implementing the functions "makhunk", "cxr", and
;     "rplacx" to behave as they do in Franz Lisp.

(defmacro getdefaultinst (defblock)
  `(cxr 3 ,defblock))
 
(defmacro getdefinition (valblock)
  `(cxr 0 ,valblock))
 
(defmacro allocdef (numofslots)
  `(makhunk (+ 10 (* 4 ,numofslots))))
 
(defmacro allocval (numofslots)
  `(makhunk (+ 4 (* 4 ,numofslots))))
 
(defmacro puttypetag (tag hunk)
  `(rplacx 1 ,hunk ,tag))
 
(defmacro gettypetag (hunk)
  `(cxr 1 ,hunk))
 
(defmacro putstructlength (size defblock)
  `(rplacx 2 ,defblock ,size))
 
(defmacro getstructlength (defblock)
  `(cxr 2 ,defblock))
 
(defmacro putuniquenum (num defblockorsym)
  `(rplacx 0 ,defblockorsym ,num))
 
(defmacro getuniquenum (defblockorsym)
  `(cxr 0 ,defblockorsym))
 
; Generate a new unique number.
(dm newnum (none)
  '(setq *lastsymbolnum* (1+ *lastsymbolnum*)))
 
; Special atom for each structure's definition.
(de defatom (symbol)
  (concat 'd: symbol))
 
; Special atom for each structure's default instance.
(de instatom (symbol)
  (concat 'i: symbol))
 
; Special atom for each symbol.
(de symatom (symbol)
  (concat 's: symbol))
 
; Special atom for each block.
(de blockatom (symbol)
  (concat 'b: symbol))
 
; Special atom for each ordinal type.
(de ordatom (symbol)
  (concat 'o: symbol))
 
(defmacro putsymbolpname (name block)
  `(rplacx 2 ,block ,name))
 
(defmacro getsymbolpname (symbolitem)
  `(cxr 2 ,symbolitem))
 
(defmacro putpname (name blk)
  `(rplacx 5 ,blk ,name))
 
(defmacro getpname (blk)
  `(cxr 5 ,blk))
 
(defmacro putdef (defblock valblock)
  `(rplacx 0 ,valblock ,defblock))
 
(defmacro putisa (isa valblock)
  `(rplacx 4 ,valblock ,isa))
 
(defmacro getisa (valblock)
  `(cxr 4 ,valblock))
 
(defmacro putdefaultinst (valblock defblock)
  `(rplacx 3 ,defblock ,valblock))
 
(defmacro puthashalias (hashnum blk)
  `(rplacx 6 ,blk ,hashnum))
 
(defmacro gethashalias (blk)
  `(cxr 6 ,blk))
 
(defmacro puthashfocus (hashnum blk)
  `(rplacx 7 ,blk ,hashnum))
 
(defmacro gethashfocus (blk)
  `(cxr 7 ,blk))
 
(defmacro putexpansionlist (explist blk)
  `(rplacx 8 ,blk ,explist))
 
(defmacro getexpansionlist (blk)
  `(cxr 8 ,blk))
 
(defmacro putbasehooks (hooklist defblk)
  `(rplacx 9 ,defblk ,hooklist))
 
(defmacro getbasehooks (defblk)
  `(cxr 9 ,defblk))
 
(de addbasehook (conscell item)
  (let* ((itemdef (getdefinition item))
	 (oldhooks (getbasehooks itemdef)))
	(cond (oldhooks (nconc1 oldhooks conscell))
	      ( t (putbasehooks itemdef (ncons conscell))))))
 
(defmacro getslotname (slotnum blk)
  `(cxr (+ 8 (* 4 ,slotnum)) ,blk))
 
(defmacro putslotname (slotnum slotname blk)
  `(rplacx (+ 8 (* 4 ,slotnum)) ,blk ,slotname))

(defmacro addslotname (slotnum slotname blk)
  `(rplacx (+ 8 (* 4 ,slotnum)) ,blk
	   (cons ,slotname (cxr (+ 8 (* 4 ,slotnum)) ,blk))))
 
(defmacro putslottype (slotnum typenum blk)
  `(rplacx (+ 7 (* 4 ,slotnum)) ,blk ,typenum))
 
(defmacro getslottype (slotnum blk)
  `(cxr (+ 7 (* 4 ,slotnum)) ,blk))
 
(defmacro putppset (slotnum setname blk)
  `(rplacx (+ 9 (* 4 ,slotnum)) ,blk ,setname))
 
(defmacro getppset (slotnum blk)
  `(cxr (+ 9 (* 4 ,slotnum)) ,blk))
 
(defmacro initbothalists (inst)
  `(rplacx 2 ,inst (ncons nil)))

(defmacro putbothalists (alist inst)
  `(rplacx 2 ,inst ,alist))

(defmacro getbothalists (inst)
  `(cxr 2 ,inst))

(defmacro getalist (inst)
  `(cdr (cxr 2 ,inst)))

(defmacro putalist (alist inst)
  `(rplacd (cxr 2 ,inst) ,alist))
 
; This must return the new special conscell.
(defmacro addalist (var inst)
  `(let ((specialcell (cons ,var (punbound))))
	(putalist (cons specialcell (getalist ,inst)) ,inst)
	specialcell))
 
; The frozen variables are kept here instead of the regular assoc-list.
(defmacro getalistcp (inst)
  `(car (cxr 2 ,inst)))
 
(defmacro putalistcp (alist inst)
  `(rplaca (cxr 2 ,inst) ,alist))
 
(defmacro getabbrev (inst)
  `(cxr 3 ,inst))
 
(defmacro putabbrev (abbrev inst)
  `(rplacx 3 ,inst ,abbrev))
 
; Put zero as the (initial) hash and format info.
(defmacro clearhashandformat (slotnum defblock)
  `(rplacx (+ 6 (* 4 ,slotnum)) ,defblock 0))
 
(defmacro puthashandformat (slotnum hashnum defblock)
  `(rplacx (+ 6 (* 4 ,slotnum)) ,defblock ,hashnum))
 
(defmacro gethashandformat (slotnum defblock)
  `(cxr (+ 6 (* 4 ,slotnum)) ,defblock))
 
(defmacro puthashandenforce (slotnum hashnum blk)
  `(rplacx (+ 6 (* 4 ,slotnum)) ,blk
           (boole 7 (boole 1 (boole 10. 127. 0)
                           (cxr (+ 6 (* 4 ,slotnum)) ,blk))
                    (boole 1 127. ,hashnum))))
 
(defmacro puthashinfo (slotnum hashnum blk)
  `(rplacx (+ 6 (* 4 ,slotnum)) ,blk
           (boole 7 (boole 1 (boole 10. 63. 0)
                           (cxr (+ 6 (* 4 ,slotnum)) ,blk))
                    (boole 1 63. ,hashnum))))
 
(defmacro addhash* (hashnum)
  `(setq ,hashnum (boole 7 1 ,hashnum)))
 
(defmacro addhash** (hashnum)
  `(setq ,hashnum (boole 7 2 ,hashnum)))
 
(defmacro addhash: (hashnum)
  `(setq ,hashnum (boole 7 4 ,hashnum)))
 
(defmacro addhash:: (hashnum)
  `(setq ,hashnum (boole 7 8. ,hashnum)))
 
(defmacro addhash> (hashnum)
  `(setq ,hashnum (boole 7 16. ,hashnum)))
 
(defmacro addhash< (hashnum)
  `(setq ,hashnum (boole 7 32. ,hashnum)))
 
(defmacro addhash*** (hashnum)
  `(setq ,hashnum (boole 7 64. ,hashnum)))
 
(defmacro addenforce (hashnum)
  `(setq ,hashnum (boole 7 128. ,hashnum)))
 
(defmacro gethashinfo (slotnum blk)
  `(boole 1 63.
	  (cxr (+ 6 (* 4 ,slotnum)) ,blk)))

(defmacro gethash* (hashnum)
  `(\=& 1 (boole 1 1 ,hashnum)))
 
(defmacro gethash** (hashnum)
  `(\=& 2 (boole 1 2 ,hashnum)))
 
(defmacro gethash: (hashnum)
  `(\=& 4 (boole 1 4 ,hashnum)))
 
(defmacro gethash:: (hashnum)
  `(\=& 8. (boole 1 8. ,hashnum)))
 
(defmacro gethash> (hashnum)
  `(\=& 16. (boole 1 16. ,hashnum)))
 
(defmacro gethash< (hashnum)
  `(\=& 32. (boole 1 32. ,hashnum)))
 
(defmacro gethash*** (hashnum)
  `(\=& 64. (boole 1 64. ,hashnum)))
 
(defmacro getenforce (slotnum defblock)
  `(\=& 128. (boole 1 128. (cxr (+ 6 (* 4 ,slotnum)) ,defblock))))
 
; The format information is eventually intended for custom tailoring of
;    printing of structures but we've never gotten around to adding it.
;    The main idea is whether to print it if it contains the default
;    value, or whether to print to a limited depth, or whether to print
;    at all, etc.
(defmacro putformatinfo (slotnum hashnum blk)
  `(rplacx (+ 6 (* 4 ,slotnum)) ,blk
	   (boole 7
		  (boole 1 (boole 10. 192. 0)
			 (cxr (+ 6 (* 4 ,slotnum)) ,blk))
		  (boole 1 192. (lsh ,hashnum 6)))))
 
(defmacro getformatinfo (slotnum blk)
  `(lsh (boole 1
	       (boole 10. 192. 0)
	       (cxr (+ 6 (* 4 ,slotnum)) ,blk)) -6))
 
(defmacro putpred (slotnum value inst)
  `(rplacx (+ 2 (* 4 ,slotnum)) ,inst ,value))
 
(defmacro getpred (slotnum inst)
  `(cxr (+ 2 (* 4 ,slotnum)) ,inst))
 
(defmacro putslothooks (slotnum slothooklist inst)
  `(rplacx (+ 3 (* 4 ,slotnum)) ,inst ,slothooklist))
 
(defmacro getslothooks (slotnum inst)
  `(cxr (+ 3 (* 4 ,slotnum)) ,inst))
 
; Values of slots in PEARL structures are of one of four types.
; The type is stored as an atom in the "slotvaluetype"
;    and describes what type of value will be found in the "slotvalue".
; The possible types and what is put in "slotvalue" are:
;    CONSTANT A constant value   -- the value.
;    LOCAL    A local variable   -- the variable's alist conscell
;					(name . value).
;    ADJUNCT  A constant value plus an adjunct variable
;     	                          -- a conscell with CAR = the constant value
;     	                             and CDR = the adjvar's conscell
;					(name . value).
;    GLOBAL   A global variable  -- the (atom) name of the global variable.
;

(defmacro putslotvaluetype (slotnum type inst)
  `(rplacx (* 4 ,slotnum) ,inst ,type))

(defmacro getslotvaluetype (slotnum inst)
  `(cxr (* 4 ,slotnum) ,inst))

(defmacro putslotvalue (slotnum value inst)
  `(rplacx (1+ (* 4 ,slotnum)) ,inst ,value))

(defmacro getslotvalue (slotnum inst)
  `(cxr (1+ (* 4 ,slotnum)) ,inst))

(dm equivclass (none)
  ''*equivclass*)

(de equivclassp (potequivclass)
  (and (dtpr potequivclass)
       (eq (equivclass) (car potequivclass))))

; returns (punbound) for unified variables instead of the equiv cons cell.
(defmacro getvalofequivorvar (equivorvar)
  `(let ((val ,equivorvar))
        (cond ((equivclassp val) (punbound))
	      ( t val))))

(defmacro getvalue (slotnum inst)
  `(let ((value (getslotvalue ,slotnum ,inst)))
	(selectq (getslotvaluetype ,slotnum ,inst)
		 (CONSTANT  value)        ; A constant value.
		 (LOCAL     (getvalofequivorvar (cdr value))) ; A local var.
		 (ADJUNCT   (car value))  ; A constant plus adjvar.
		 (GLOBAL    (getvalofequivorvar (eval value))) ; A global var.
		 (otherwise (punbound)))))

; Same as getvalue, except that if the slot has an variable in it
;    the atom in "var" gets set to that value.
(defmacro getvarandvalue (slotnum inst var)
  `(let ((value (getslotvalue ,slotnum ,inst)))
	(selectq (getslotvaluetype ,slotnum ,inst)
		 (CONSTANT  (set ,var nil)
			    value)          ; A constant value.
		 (LOCAL     (set ,var value)
			    (getvalofequivorvar (cdr value))) ; A local var.
		 (ADJUNCT   (set ,var (cdr value))
			    (car value))    ; A constant plus adjvar.
		 (GLOBAL    (set ,var value)
			    (getvalofequivorvar (eval value))) ; A global var.
		 (otherwise (punbound)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  The next bunch of functions are for hashing and building data bases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For each data base, there are three parts (each a hunk):
;    the header which contains the name,
;                              whether it is active
;                              its parent and children and ...
;    the two parts of the actual data base:
;        DB1 for items hashed under one value.
;        DB2 for items hashed under two or more values.
; DB1 and DB2 each contain pointers to conscells whose cars are the
;    atom *db* and whose cdrs are the list of items in that bucket.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FIRST, the functions to access and add to a hash bucket:

; Items hashed under only one integer are in DB1.
(defmacro gethash1 (num1 db1)
  `(cxr (\\ ,num1 *db1size*) ,db1))
 
; Add the item to the front of the appropriate hash bucket (AFTER the
;    special *db* conscell).
(defmacro puthash1 (num1 db1 item)
  `(let ((bucket (gethash1 ,num1 ,db1)))
	; Avoid exact duplicates.
	(or (memq ,item bucket)
	    (rplacd bucket (cons ,item (cdr bucket))))
	bucket))
 
; Items hashed under either two or more integers are in DB2.
(defmacro gethash2 (num1 num2 db2)
  `(cxr (\\ (+ ,num1 (* ,num2 1024.)) *db2size*)
	,db2))
 
; Add the item to the front of the appropriate hash bucket (AFTER the
;    special *db* conscell).
(defmacro puthash2 (num1 num2 db2 item)
  `(let ((bucket (gethash2 ,num1 ,num2 ,db2)))
	; Avoid exact duplicates.
	(or (memq ,item bucket)
	    (rplacd bucket (cons ,item (cdr bucket))))
	bucket))
 
(defmacro gethash3 (num1 num2 num3 db2)
  `(cxr (\\ (+ ,num1
	       (* ,num2 1024.)
	       (* ,num3 1048576.))  ; = 1024 * 1024
	    *db2size*)
	,db2))
 
; Add the item to the front of the appropriate hash bucket (AFTER the
;    special *db* conscell).
(defmacro puthash3 (num1 num2 num3 db2 item)
  `(let ((bucket (gethash3 ,num1 ,num2 ,num3 ,db2)))
	; Avoid exact duplicates.
	(or (memq ,item bucket)
	    (rplacd bucket (cons ,item (cdr bucket))))
	bucket))

(defmacro gethashmulti (num1 others db2)
  `(cxr (\\ (+ ,num1 
	       (apply (function +) 
		      (mapcar (function *)
			      ,others *multiproducts*)))
	    *db2size*)
	,db2))
 
; Add the item to the front of the appropriate hash bucket (AFTER the
;    special *db* conscell).
(defmacro puthashmulti (num1 others db2 item)
  `(let ((bucket (gethashmulti ,num1 ,others ,db2)))
	; Avoid exact duplicates.
	(or (memq ,item bucket)
	    (rplacd bucket (cons ,item (cdr bucket))))
	bucket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now the header info.

(defmacro putdbname (name db)
  `(rplacx 0 ,db ,name))
 
(defmacro putdbchildren (childlist db)
  `(rplacx 2 ,db ,childlist))
 
(defmacro setdbactive (db)
  `(rplacx 3 ,db t))
 
(defmacro cleardbactive (db)
  `(rplacx 3 ,db nil))
 
(defmacro putdbparent (parent db)
  `(rplacx 4 ,db ,parent))
 
(defmacro putdb1 (db1 db)
  `(rplacx 5 ,db ,db1))
 
(defmacro putdb2 (db2 db)
  `(rplacx 6 ,db ,db2))
 
(defmacro getdbname (db)
  `(cxr 0 ,db))
 
(defmacro getdbchildren (db)
  `(cxr 2 ,db))
 
(defmacro getdbactive (db)
  `(cxr 3 ,db))
 
(defmacro getdbparent (db)
  `(cxr 4 ,db))
 
(defmacro getdb1 (db)
  `(cxr 5 ,db))
 
(defmacro getdb2 (db)
  `(cxr 6 ,db))
 
; The following predicates do the best we can to check for the type of
;    object by checking what we hope are reasonably unique arrangements
;    of values.  In the case of definitions, instances, databases and
;    symbols, a tag is put in the hunk saying what it is.  This is
;    assumed to be enough.

(de streamp (potstream)
  (and (dtpr potstream)
       (eq '*stream* (car potstream))))

(de databasep (potdb)
  (and (hunkp potdb)
       (let ((tag (gettypetag potdb)))
	    (or (eq tag '*pearldb*)
		(eq tag '*pearlinactivedb*)))))

(de blockp (potblock)
  (let* ((name (car potblock))
	 (blockname (blockatom name)))
	(and (boundp blockname)
	     (eq name
		 (car (eval blockname)))
	     (eq potblock
		 (eval blockname)))))

(de definitionp (potdef)
  (and (hunkp potdef)
       (eq '*pearldef* (gettypetag potdef))))

(de psymbolp (potsymbol)
  (and (hunkp potsymbol)
       (eq '*pearlsymbol* (gettypetag potsymbol))))

(de structurep (potstruct)
  (and (hunkp potstruct)
       (eq '*pearlinst* (gettypetag potstruct))))

(de symbolnamep (potname)
  (let ((symname (symatom potname)))
       (and (boundp symname)
	    (psymbolp (eval symname)))))

(de structurenamep (potname)
  (let ((defname (defatom potname)))
       (and (boundp defname)
	    (definitionp (eval defname)))))

; Determine the print name of an arbitrary object.
(de pname (item)
  (cond ((definitionp item)   (getpname item))
	((structurep item)    (getpname (getdefinition item)))
	((psymbolp item)      (getsymbolpname item))
	((databasep item)     (getdbname item))
	((atom item)          item)
	((streamp item)       (msg t "PNAME: streams do not have pnames: "
				   item t))
	( t (msg t "PNAME: " item " does not have a printname"))))
 
; For loop patterned after (do for ...) in UCI Lisp, except that an
;     initial value is required instead of RPT (and there is no DO). 
(defmacro for (val init final &rest body)
  `((lambda (,val pforlim)
	    (prog (pforval)
		  pforlab
		  (and (>& ,val pforlim)
		       (return pforval))
		  (setq pforval (progn .,body))
		  (setq ,val (1+ ,val))
		  (go pforlab)))
    ,init
    ,final))
 
; While loop patterned after (do while ...) in UCI Lisp.
(defmacro while (val &rest body)
  `(prog (pwhval)
	 pwhlab
	 (and (not ,val)
	      (return pwhval))
	 (setq pwhval (progn .,body))
	 (go pwhlab)))

; vi: set lisp:
