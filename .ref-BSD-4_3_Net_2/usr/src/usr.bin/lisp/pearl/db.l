;;;;;;;;;;;;;;;;;;;;;;;;;;;;; db.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for building and releasing a forest of data bases.
;    See the file "template" plus the discussion in the "lowlevel.l" file for
;        a picture and an idea of how data bases are arranged internally.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Clear out the *db* conscells in the two parts of the data base,
;    thus releasing the old buckets for GC, IF they aren't pointed
;    to from elsewhere.
(de cleardb (&optional (db *db*))
  (let ((parent (getdbparent db))
	(db1 (getdb1 db))
	(db2 (getdb2 db)))
       (cond (parent (connectdb db parent))
	     (  t  (for slotnum 0 (1- *db1size*)
			(rplacd (cxr slotnum db1) nil))
		   (for slotnum 0 (1- *db2size*)
			(rplacd (cxr slotnum db2) nil))))
       t))
 
; Used by builddb to connect the sibling's buckets with its parent's.
; Also used by cleardb on a sibling.
(de connectdb (newdb olddb)
  (let ((newdb1 (getdb1 newdb))
	(newdb2 (getdb2 newdb))
	(olddb1 (getdb1 olddb))
	(olddb2 (getdb2 olddb)))
       (for slotnum 0 (1- *db1size*)
	    (rplacd (cxr slotnum newdb1) (cxr slotnum olddb1)))
       (for slotnum 0 (1- *db2size*)
	    (rplacd (cxr slotnum newdb2) (cxr slotnum olddb2)))
       t))
 
; Set the size for data bases to 2 to the "poweroftwo" -- actually
;    the next smaller prime number.
;    *Availablesizes* is in inits.l and is designed to
;        make the data bases a factor of 4 apart
;        EXCEPT in Franz, where the largest are equal-sized.
(de setdbsize (poweroftwo)
  (let (pair rebuilddb)
       (and *activedbnames*
	    (progn (and *warn*
			(msg t "SETDBSIZE: Warning: Size change "
			     "is causing the release of all databases."
			     t "     You must rebuild all "
			     "but the default yourself." t))
		   (mapcar (funl (dbname) (releasedb (eval dbname)))
			   (copy *activedbnames*))
		   (setq rebuilddb t)
		   ))
       (and (or (<& poweroftwo 2.)
		(>& poweroftwo 13.))
	    (progn (msg t "SETDBSIZE: Database size is a power to raise 2 to"
			t "    and must be greater than 1 and less than 14."
			t "    It cannot be " poweroftwo "." t)
		   (pearlbreak)))
       (or (setq pair (assq poweroftwo *availablesizes*))
	   (progn (msg t "SETDBSIZE: "
		       "Database sizes are integer powers to raise 2 to." t)
		  (pearlbreak)))
       (setq *db2size* (cdr pair))
       ; The sizes of the two parts of the data base are 
       ;   in a 1 to 4 ratio.
       (setq pair (assq (- poweroftwo 2.) *availablesizes*))
       (setq *db1size* (cdr pair))
       (and rebuilddb
	    (setq *db* (builddb *maindb*)))
       t))
 
; (BUILDDB NEWDB OLDDB)  Build an extension to OLDDB called NEWDB.  If OLDDB
;     is NIL then build at the bottom level, else add as a leaf of the tree.
; The new data base is stored under the atom which is its name,
;     unlike the rest of PEARL objects (i.e., no special-prefix atom).
; Each new leaf has each of its hash buckets tied into the buckets of the
;     parent so that nextitem need not know how many data bases it is
;     dealing with.
(df builddb (l)
  (let ((newdbname (car l))
	(olddbname (cadr l)))
       (and (memq newdbname *activedbnames*)
	    (progn (msg t "BUILDDB: " newdbname
			" is already an active database name." t)
		   (pearlbreak)))
       (and olddbname
	    ; Two db's given but old one bad.
	    (not (memq olddbname *activedbnames*))
	    (progn (msg t "BUILDDB: " olddbname
			" is not an active database name." t)
		   (pearlbreak)))
       (let ((newdb (makhunk 7))
	     (olddb (and olddbname
			 (eval olddbname)))
	     (db1 (makhunk *db1size*))
	     (db2 (makhunk *db2size*)))
	    (push newdbname *activedbnames*)
	    (putdbname newdbname newdb)
	    (set newdbname newdb)
	    (puttypetag '*pearldb* newdb)
	    (putdbchildren nil newdb)
	    (setdbactive newdb)
	    (putdbparent olddb newdb)
	    (putdb1 db1 newdb)
	    (putdb2 db2 newdb)
	    ; add the *db* conscells.
	    (for slotnum 0 (1- *db1size*)
		 (rplacx slotnum db1 (cons '*db* nil)))
	    (for slotnum 0 (1- *db2size*)
		 (rplacx slotnum db2 (cons '*db* nil)))
	    (and olddb ; Two db's.
		 ; add to parent's children.
		 (putdbchildren (cons newdb (getdbchildren olddb))
				olddb)
		 ; Connectdb does the extra work for adding to the tree.
		 (connectdb newdb olddb))
	    newdb)))
 
; Release a data base.   If its children are also released, then
;    it can be garbage collected.  If not, do not mark it inactive
;    until they are.
(de releasedb (db)
  (and (not (databasep db))
       (progn (msg t "RELEASEDB: Argument is not a database." t)
	      (pearlbreak)))
  (let ((dbname (getdbname db))
	(parent (getdbparent db)))
       (and (not (memq dbname *activedbnames*))
	    (progn (msg t "RELEASEDB: Trying to release an inactive database: "
			db t)
		   (pearlbreak)))
       (cond ((null (getdbchildren db))    ; No children.
	      (setq *activedbnames* (delq (getdbname db) *activedbnames*))
	      (and (equal *activedbnames* '(nil))
		   (setq *activedbnames* nil))
	      (set dbname (unbound))
	      (putdbname nil db)
	      (and parent
		   (putdbchildren (delq db (getdbchildren parent)) parent))
	      (cleardbactive db)
	      (putdbparent nil db)
	      (while (and parent                        ; There's a parent --
			  (null (getdbchildren parent)) ; with 0 children --
			  (not (getdbactive parent)))   ; that's inactive.
		     (cleardb parent)
		     (putdb1 nil parent)
		     (putdb2 nil parent)
		     ; Save next parent with prog1 and then remove self from
		     ; parent's child list and clear out own parent pointer
		     (setq parent
			   (prog1
			    (getdbparent parent)  ; To be the new parent
			    (and (getdbparent parent)
				 (putdbchildren
				  (delq parent
					(getdbchildren (getdbparent parent)))
				  (getdbparent parent))
				 )
			    (putdbparent nil parent))))
	      (cleardb db)
	      (puttypetag '*pearlinactivedb* db)
	      (putdb1 nil db)
	      (putdb2 nil db))
	     ( t (setq *activedbnames* (delq dbname *activedbnames*))
		 (and (equal *activedbnames* '(nil))
		      (setq *activedbnames* nil))
		 (set dbname (unbound))
		 (putdbname nil db)
		 (cleardbactive db)
		 (puttypetag '*pearlinactivedb* db)
		 (putdb1 nil db)
		 (putdb2 nil db)))
       t))
 

; vi: set lisp:
