;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hash.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for hashing, inserting, and fetching items into the
;    data bases, plus operating on streams.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Find the next item on the CDDR list of the stream that matches the CADR of
; the stream and return it, also updating the stream.
(de nextitem (stream)
  (or (streamp stream)
      (progn (msg t "NEXTITEM: Not a stream: " stream t)
	     (pearlbreak)))
  (setq stream (cdr stream))	          ; Throw away the *STREAM*.
  (cond ((eq t (car stream))	          ; This means function structure.
	 (prog1 (evalfcn (cdr stream))
		(rplacd (rplaca stream nil) nil)))
	((null (cadr stream)) nil)        ; Test for empty stream
	; Stream built by standardfetch.
	; To debug or modify this, you must draw a picture of what
	;   standardfetch built because of the way it is written.
	((not (dtpr (cadr stream)))
	 (prog (item result)
	       (setq item (car stream))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<nextitem *runnextitemhooks*)
	       (while (and (cdr stream)
			   (or (eq (cadr stream) '*db*)
			       (not (match item (cadr stream)))))
		      (rplacd stream (cddr stream)))
	       (setq item (cadr stream))
	       (rplacd stream (cddr stream))
	       (checkrunhandlebasehooks1 '>nextitem *runnextitemhooks*)
	       (return item)))
	; Stream built by expandedfetch (or fetcheverywhere).
	; To debug or modify this, you must draw a picture of what
	;   expandedfetch built because of the way it is written.
	((not (dtpr (caadr stream)))
	 (prog (item result)
	       (setq item (car stream))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<nextitem *runnextitemhooks*)
	       (while (and (cdr stream)
			   (or (eq (caadr stream) '*db*)
			       (not (expandedmatch item (caadr stream)))))
		      (or (car (rplaca (cdr stream) (cdadr stream)))
			  (rplacd stream (cddr stream))))
	       (setq item (caadr stream))
	       (or (not (cdr stream))
		   (car (rplaca (cdr stream) (cdadr stream)))
		   (rplacd stream (cddr stream)))
	       (checkrunhandlebasehooks1 '>nextitem *runnextitemhooks*)
	       (return item)))))
 
(defmacro hashinfo (slotnum)
  `(cxr ,slotnum *hashingmarks*))

(defmacro sethashinfo (slotnum value)
  `(rplacx ,slotnum *hashingmarks* ,value))

(defmacro slotval (slotnum)
  `(cxr ,slotnum *slotvalues*))

(defmacro storeslot (slotnum value)
  `(rplacx ,slotnum *slotvalues* ,value))

; If there is anything to hash this slot on, say so and put it in HASHV.
(defmacro hashablevalue (slotnum item defblock hashinfo)
  `(not (memq (setq hashv (gethashvalue ,slotnum ,item ,defblock ,hashinfo))
	      *unhashablevalues*)))
 
; If this slot is to take part in a hashing combination, (and it is the
;    second one in :: or ** hashing), then add it to the right hash bucket.
(dm hashslot (none)
  '(cond ((\=& 0 hashinfo) nil) ; No hashing to be done
	 ((hashablevalue slotnum item defblock hashinfo) ; Sets HASHV
	  (and (gethash*  hashinfo)
	       (puthash2  unique hashv db2 item))
;	  (and (gethash:  hashinfo)
;	       (puthash1  hashv db1 item))
	  (and (gethash** hashinfo)
	       (cond ((null mark**)
		      ; First one found.
		      (setq mark** hashv))
		     ; Second one found
		     ((neq t mark**)
		      (puthash3 unique mark** hashv db2 item)
		      (setq mark** t))
		     ; Third or greater found.
		     (  t  (msg t "HASH: More than two **'s in: "
				(getpname defblock) t))))
;	  (and (gethash:: hashinfo)
;	       (cond ((null mark::)
;		      ; First one found.
;		      (setq mark:: hashv))
;		     ; Second one found
;		     ((neq t mark::)
;		      (puthash2 mark:: hashv db2 item)
;		      (setq mark:: t))
;		     ; Third or greater found.
;		     (  t  (msg t "HASH: More than two ::'s in: "
;				(getpname defblock) t))))
	  (and (gethash*** hashinfo)
	       (cond ((null mark***)
		      ; First one found.
		      (setq mark*** (ncons hashv)))
		     ; Later ones found.
		     (  t  (tconc mark*** hashv))))
)))
 
; For each of the four ways of hashing, or else just based on the type,
;    check to see if the pattern can be hashed that way and if so,
;    RETURN the right hashbucket.  If the previous one can't be done,
;    try the next one but stop with the first that can be done.
;    The order is ***, **, ::, &&, *, and :.
(dm insidestandardfetch (none)
  '(cond ((prog2
	   (for slotnum 1 length
		(and (gethash*** (hashinfo slotnum))
		     (cond ((eq (punbound)
				(setq hashv (slotval slotnum)))
			    (setq mark nil)
			    (return nil))
			   ((null mark)
			    (setq mark (ncons nil))
			    (tconc mark hashv)
			    nil)
			   ( t  (tconc mark hashv)))))
	   mark)
	  (gethashmulti unique (car mark) db2))
	 ((for slotnum 1 length
	       (and (gethash** (hashinfo slotnum))
		    (cond ((eq (punbound)
			       (setq hashv (slotval slotnum)))
			   (return nil))
			  ((null mark) (setq mark hashv) nil)
			  ( t (return (gethash3 unique mark hashv db2)))))))
;	 ((for slotnum 1 length
;	       (and (gethash:: (hashinfo slotnum))
;		    (cond ((eq (punbound)
;			       (setq hashv (slotval slotnum)))
;			   (return nil))
;			  ((null mark) (setq mark hashv) nil)
;			  ( t (return (gethash2 mark hashv db2)))))))
	 ((and (not (\=& 0 focus))
	       (pboundp (setq hashv (slotval focus))))
	  (recursetoinsidestandardfetch (getslotvalue focus item) db1 db2))
	 ((for slotnum 1 length
	       (and (gethash* (hashinfo slotnum))
		    (and (pboundp (setq hashv
					(slotval slotnum)))
			 (return (gethash2 unique hashv db2))))))
;	 ((for slotnum 1 length
;	       (and (gethash: (hashinfo slotnum))
;		    (and (pboundp (setq hashv
;					(slotval slotnum)))
;			 (return (gethash1 hashv db1))))))
	 ( t (gethash1 unique db1))))
 
(de recursetoinsidestandardfetch (item db1 db2)
  (let* ((defblock (getdefinition item))
	 (length (getstructlength defblock))
	 (*slotvalues* (makhunk (1+ length)))
	 (*hashingmarks* (makhunk (1+ length)))
	 (unique (getuniquenum defblock))
	 mark hashv focus hashinfo)
	(setq focus (gethashfocus defblock))
	(for slotnum 1 length
	     (setq hashinfo (gethashinfo slotnum defblock))
	     (sethashinfo slotnum hashinfo)
	     (or (and (\=& 0 hashinfo)
		      (not (\=& focus slotnum)))
		 (storeslot slotnum
			    (gethashvalue slotnum item defblock hashinfo))))
	(insidestandardfetch)))

; Return a pair consisting of the ITEM and a hash-bucket-list that should
; have what we are looking for in it.
(de standardfetch (item &optional (db *db*))
  (cond ((get (pname item) 'functionstruct)
	 (cons '*stream* (cons t item)))
	( t (prog (mark defblock bucket db1 db2 hashv result focus
			length hashinfo unique)
		  (setq defblock (getdefinition item))
	          (setq *currentpearlstructure* item)
		  (checkrunhandlebasehooks1 '<fetch *runfetchhooks*)
		  (setq db1 (getdb1 db))
		  (setq db2 (getdb2 db))
		  (setq length (getstructlength defblock))
		  (setq focus (gethashfocus defblock))
		  (for slotnum 1 length
		       (setq hashinfo (gethashinfo slotnum defblock))
		       (sethashinfo slotnum hashinfo)
		       (or (and (\=& 0 hashinfo)
				(not (\=& focus slotnum)))
			   (storeslot slotnum
				      (gethashvalue slotnum item
						    defblock hashinfo))))
		  (setq unique (getuniquenum defblock))
		  (setq bucket (insidestandardfetch))
		  (checkrunhandlebasehooks1 '>fetch *runfetchhooks*)
		  (return (cons '*stream* (cons item bucket)))))))
 
(aliasdef 'fetch 'standardfetch)

(de expandedfetch (item &optional (db *db*))
  (cond ((get (pname item) 'functionstruct)
	 (cons '*stream* (cons t item)))
	( t (prog (mark defblock defblocklist buckets db1 db2 hashv result
			focus length hashinfo)
		  (setq defblock (getdefinition item))
	          (setq *currentpearlstructure* item)
		  (checkrunhandlebasehooks1 '<fetch *runfetchhooks*)
		  (setq db1 (getdb1 db))
		  (setq db2 (getdb2 db))
		  (setq length (getstructlength defblock))
		  (setq focus (gethashfocus defblock))
		  (for slotnum 1 length
		       (setq hashinfo (gethashinfo slotnum defblock))
		       (sethashinfo slotnum hashinfo)
		       (or (and (\=& 0 hashinfo)
				(not (\=& focus slotnum)))
			   (storeslot slotnum
				      (gethashvalue slotnum item
						    defblock hashinfo))))
		  (setq defblocklist (cons defblock
					   (getexpansionlist defblock)))
		  ; Note that instead of being one list, buckets is a
		  ;    list of lists.
		  (setq buckets
			(mapcar
			 (funl (expandeddefblock)
			       (let ((unique (getuniquenum expandeddefblock)))
				    (insidestandardfetch)))
			 defblocklist))
		  (dremove nil buckets)
		  (checkrunhandlebasehooks1 '>fetch *runfetchhooks*)
		  (return (cons '*stream* (cons item buckets)))))))
 
; Find the object EVERYWHERE it might be: ; (Well, only 1 for each hash method).
; For each of the four ways of hashing, plus just based on the type,
;    check to see if the pattern can be hashed that way and if so,
;    return the right hash bucket. A list of these lists is made.
;    NIL's are removed in the main function.
;    The order is ***, **, ::, &&, *, and :.
(dm insidefetcheverywhere (none)
  '(let ((bucketlist (ncons nil)))
	(for slotnum 1 length
	     (and (gethash*** (hashinfo slotnum))
		  (cond ((eq (punbound)
			     (setq hashv (slotval slotnum)))
			 (setq mark nil)
			 (return nil))
			((null mark) (setq mark (ncons hashv)) nil)
			( t  (tconc mark hashv)))))
	(and mark
	     (tconc bucketlist
		    (gethashmulti unique (car mark) db2))
	     (setq mark nil))
	(for slotnum 1 length
	     (and (gethash** (hashinfo slotnum))
		  (cond ((eq (punbound)
			     (setq hashv (slotval slotnum)))
			 (return nil))
			((null mark) (setq mark hashv) nil)
			( t (tconc bucketlist
				   (gethash3 unique mark hashv db2))
			    (setq mark nil)
			    (return nil)))))
	(and (not (\=& 0 focus))
	     (pboundp (setq hashv (slotval focus)))
	     (tconc bucketlist
		    (recursetoinsidestandardfetch (getslotvalue focus item)
						  db1 db2)))
	(for slotnum 1 length
	     (and (gethash* (hashinfo slotnum))
		  (and (pboundp (setq hashv
				      (slotval slotnum)))
		       (tconc bucketlist
			      (gethash2 unique hashv db2)))))
	(tconc bucketlist
	       (gethash1 unique db1))
	(car bucketlist)))

; Return a list consisting of the ITEM and a list of hash-bucket-list
;   that must have what we are looking for in it if it's there.
(de fetcheverywhere (item &optional (db *db*))
  (cond ((get (pname item) 'functionstruct)
	 (cons '*stream* (cons t item)))
	( t (prog (mark defblock buckets db1 db2 hashv result focus
			length hashinfo unique)
		  (setq defblock (getdefinition item))
		  (setq length (getstructlength defblock))
		  (setq focus (gethashfocus defblock))
		  (for slotnum 1 length
		       (setq hashinfo (gethashinfo slotnum defblock))
		       (sethashinfo slotnum hashinfo)
		       (or (and (\=& 0 hashinfo)
				(not (\=& focus slotnum)))
			   (storeslot slotnum
				      (gethashvalue slotnum item
						    defblock hashinfo))))
	          (setq *currentpearlstructure* item)
		  (checkrunhandlebasehooks1 '<fetch *runfetchhooks*)
		  (setq db1 (getdb1 db))
		  (setq db2 (getdb2 db))
		  (setq unique (getuniquenum defblock))
		  (setq buckets (insidefetcheverywhere))
		  (dremove nil buckets)
		  (checkrunhandlebasehooks1 '>fetch *runfetchhooks*)
		  (return (cons '*stream* (cons item buckets)))))))
 
; Discover if a hash alias is to be used.
(dm noalias (none)
  '(cond ((>& alias 0)
	  (cond ((gethash< hashinfo)
		 (cond ((gethash> hashinfo) nil) ; < > cancels
		       ( t t)))
		( t nil)))
	 ( t (cond ((gethash< hashinfo) t)
		   ( t (cond ((gethash> hashinfo) nil) ; < > cancels
			     ( t t)))))))
 
; Get the value that should be hashed for the given slot of ITEM
;     else return unbound.
(de gethashvalue (slotnum item defblock hashinfo)
  (let
   ((potential (getvalue slotnum item))
    alias)
   (cond ((null potential) nil)
	 ((pboundp potential)
	  (let ((potdef (getdefinition potential)))
	       (selectq (getslottype slotnum defblock)
			(0 (setq alias (gethashalias potdef))
			   (cond ((or (noalias)
				      (\=& 0 alias))
				  (getuniquenum potdef))
				 ( t
				  (setq alias (abs alias))
				  (gethashvalue alias potential potdef
						(gethashinfo alias potdef)))))
			(1  (getuniquenum potential)) ; Symbol.
			(2  potential)                ; Integer.
			(3  (punbound))               ; Lisp not hashed.
			(otherwise nil))))            ; SetOf not hashed (YET).
	 ( t (punbound)))))

; Fetch the first item matching the pattern.
(defmacro firstfetch (pattern)
  `(nextitem (fetch ,pattern)))
 
(defmacro fetchcreate (&rest rest)
  `(fetch (create .,rest)))
 
(defmacro inlinefetchcreate (&rest rest)
  `(fetch (quote ,(create rest))))
 
(defmacro inlinecreate (&rest rest)	   
  `(quote ,(create rest)))
 
; Build a value to pass to the function for the parameter for this slot.
(dm fcnslot (none)
  '(let ((slotv (getvalue slotnum item))
	 (type (getslottype slotnum defblock)))
	(cond ((eq slotv (punbound))       (punbound))
	      ((and (<& type 4)
		    (or (not (\=& 0 type))
			(not (get (getpname (getdefinition slotv))
				  'functionstruct))))        slotv)
	      ((\=& 0 type)
	       (evalfcn slotv))
	      ((\=& 0 (boole 1 3 type))
	       (mapcar (function evalfcn) slotv))
	      ( t slotv))))
 
; Evaluate a function structure.
(de evalfcn (item)
  (cond ((dtpr item) (mapcar (function evalfcn) item))
	((not (get (getpname (getdefinition item)) 'functionstruct)) item)
	( t (let* ((defblock (getdefinition item))
		   (length (getstructlength defblock))
		   (fcncall (ncons nil))
		   slotv)
		  (tconc fcncall (getpname defblock))
		  (for slotnum 1 length
		       (tconc fcncall (fcnslot)))
		  (apply* (caar fcncall) (cdar fcncall))))))
 
; A kludge to be removed (with disguisedas) when we implement VIEWS.
(defmacro getstructorsymnum (strsym) 
  `(cond ((psymbolp ,strsym) (getuniquenum ,strsym))
	 (  t  (getuniquenum (getdefinition ,strsym)))))
 
; (DISGUISEDAS Filler Struct DB) means "Is filler a struct?
; if there is an item in the data base DB of the form
;	   (STRUCT (<first slot> FILLER) ... )
; then return it.   If not, return NIL.
(de disguisedas (filler struct &optional (db *db*))
  (prog (fillernum bucket db2 item value)
	(setq db2 (getdb2 db))
	(setq fillernum (getstructorsymnum filler))
	(setq bucket (remq '*db*
			   (gethash2 (getuniquenum struct) fillernum db2)))
	loop
	(cond ((null bucket) (return nil))
	      ((and (eq struct (getdefinition (setq item (pop bucket))))
		    (neq (punbound) (setq value (getvalue 1 item)))
		    (eq (getstructorsymnum value) fillernum))
	       (return item))
	      ( t (go loop)))))

(de insertbyfocus (focus item db1 db2)
  (prog (unique mark** mark:: mark*** defblock
		value hashinfo hashv focusslotnum)
	(setq defblock (getdefinition focus))
	(setq unique (getuniquenum defblock))
	(puthash1 unique db1 item)
	(and (not (\=& 0 (setq focusslotnum (gethashfocus defblock))))
	     (pboundp (setq value (getvalue focusslotnum focus)))
	     (insertbyfocus value item db1 db2))
	(for slotnum 1 (getstructlength defblock)
	     (setq hashinfo (gethashinfo slotnum defblock))
	     (cond ((\=& 0 hashinfo) nil)
		   ((hashablevalue slotnum focus defblock hashinfo) ; Sets HASHV
		    (and (gethash*  hashinfo)
			 (puthash2  unique hashv db2 item))
;	            (and (gethash:  hashinfo)
;	                 (puthash1  hashv db1 item))
		    (and (gethash** hashinfo)
			 (cond ((null mark**)
				; First one found.
				(setq mark** hashv))
			       ; Second one found
			       ((neq t mark**)
				(puthash3 unique mark** hashv db2 item)
				(setq mark** t))
			       ; Third or greater found.
			       (  t  (msg t "HASH: More than two **'s in: "
					  (getpname defblock) t))))
;	            (and (gethash:: hashinfo)
;	                 (cond ((null mark::)
;		                ; First one found.
;		                (setq mark:: hashv))
;		               ; Second one found
;		               ((neq t mark::)
;		                (puthash2 mark:: hashv db2 item)
;		                (setq mark:: t))
;		               ; Third or greater found.
;		               (  t  (msg t "HASH: More than two ::'s in: "
;				          (getpname defblock) t))))
		    (and (gethash*** hashinfo)
			 (cond ((null mark***)
				; First one found.
				(setq mark*** (ncons hashv)))
			       ; Later ones found.
			       (  t  (tconc mark*** hashv))))
		    )))
	(and mark***
	     (puthashmulti unique (car mark***) db2 item))))

; We must put this struct into the data base somewhere,
; perhaps in several places.
(de insertdb (item &optional (db *db*))
  (or item
      (progn (msg t "Trying to INSERTDB a nil item: " item t)
	     (pearlbreak)))
  (and (dtpr item)
       (progn (msg t "Trying to INSERTDB a cons-cell: " item t)
	      (pearlbreak)))
  (cond ((get (getpname (getdefinition item)) 'functionstruct)
	 (evalfcn item))
	(  t
	 (prog (unique mark** mark:: mark*** defblock db1 db2
		       value hashinfo hashv result focus)
	       (setq defblock (getdefinition item))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<insertdb *runinsertdbhooks*)
	       (setq unique (getuniquenum defblock))
	       (setq db1 (getdb1 db))
	       (setq db2 (getdb2 db))
	       (puthash1 unique db1 item)
	       (and (not (\=& 0 (setq focus (gethashfocus defblock))))
		    (pboundp (setq value (getvalue focus item)))
		    (insertbyfocus value item db1 db2))
	       
	       (for slotnum 1 (getstructlength defblock)
		    (setq hashinfo (gethashinfo slotnum defblock))
		    (hashslot))
	       (and mark***
		    (puthashmulti unique (car mark***) db2 item))
	       (checkrunhandlebasehooks1 '>insertdb *runinsertdbhooks*)
	       (return item)))))
 
; For each way that this slot can be hashed, destructively remove the
;     item from the correct bucket.  Expects SLOTNUM, DEFBLOCK, ITEM,
;     MARK**, MARK::, MARK***, HASHV, UNIQUE, DB1, DB2.
(dm removeslot (none)
  '(cond ((\=& 0 hashinfo) nil) ; No hashing to be done
	 ((hashablevalue slotnum item defblock hashinfo) ; Sets HASHV
	  (and (gethash*  hashinfo)
	       (delq item (gethash2 unique hashv db2)))
;	  (and (gethash:  hashinfo)
;	       (delq item (gethash1 hashv db1)))
	  (and (gethash** hashinfo)
	       (cond ((null mark**)
		      (setq mark** hashv))
		     ((neq t mark**)
		      (delq item (gethash3 unique mark** hashv db2))
		      (setq mark** t))
		     (  t  (msg t "More than two **'s in: "
				(getpname defblock) t))))
;	  (and (gethash:: hashinfo)
;	       (cond ((null mark::)
;		      (setq mark:: hashv))
;		     ((neq t mark::)
;		      (delq item (gethash2 mark:: hashv db2))
;		      (setq mark:: t))
;		     (  t  (msg t "More than two ::'s in: "
;				(getpname defblock) t))))
          (and (gethash*** hashinfo)
	       (cond ((null mark***)
		      ; First one found.
		      (setq mark*** (ncons hashv)))
		     ; Later ones found.
		     (  t  (tconc mark*** hashv))))
)))
 
(de removebyfocus (focus item db1 db2)
  (prog (unique mark** mark:: mark*** defblock hashinfo hashv focusslotnum)
	(setq defblock (getdefinition focus))
	(setq unique (getuniquenum defblock))
 	(dremove item (gethash1 unique db1))
	(and (not (\=& 0 (setq focusslotnum (gethashfocus defblock))))
 	     (removebyfocus (getvalue focusslotnum focus) item db1 db2))
	(for slotnum 1 (getstructlength defblock)
	     (setq hashinfo (gethashinfo slotnum defblock))
	     (cond ((\=& 0 hashinfo) nil)
		   ((hashablevalue slotnum focus defblock hashinfo) ; Sets HASHV
		    (and (gethash*  hashinfo)
			 (delq item (gethash2 unique hashv db2)))
;	            (and (gethash:  hashinfo)
;	                 (delq item (gethash1 hashv db1)))
	            (and (gethash** hashinfo)
			 (cond ((null mark**)
				(setq mark** hashv))
			       ((neq t mark**)
				(delq item (gethash3 unique mark** hashv db2))
				(setq mark** t))
			       (  t  (msg t "More than two **'s in: "
					  (getpname defblock) t))))
;	            (and (gethash:: hashinfo)
;	                 (cond ((null mark::)
;		                (setq mark:: hashv))
;		               ((neq t mark::)
;		                (delq item (gethash2 mark:: hashv db2))
;		                (setq mark:: t))
;		               (  t  (msg t "More than two ::'s in: "
;			          	(getpname defblock) t))))
		    (and (gethash*** hashinfo)
			 (cond ((null mark***)
				; First one found.
				(setq mark*** (ncons hashv)))
			       ; Later ones found.
			       (  t  (tconc mark*** hashv))))
		    )))
	(and mark***
	     (delq item (gethashmulti unique mark*** db2)))
	))

; We may have to remove this struct from several places so look
;   every place it might have been hashed.
(de removedb (item &optional (db *db*))
  (or item
      (progn (msg t "Trying to REMOVEDB a nil item: " item t)
	     (pearlbreak)))
  (and (dtpr item)
       (progn (msg t "Trying to REMOVEDB a cons-cell: " item t)
	      (pearlbreak)))
  (or (structurep item)
      (progn (msg t "Trying to REMOVEDB a non-structure: " item t)
	     (pearlbreak)))
  (cond ((get (getpname (getdefinition item)) 'functionstruct) nil)
	(  t
	 (prog (unique mark** mark:: mark*** defblock db1 db2
		       hashinfo hashv result focus)
	       (setq defblock (getdefinition item))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<removedb *runremovedbhooks*)
	       (setq unique (getuniquenum defblock))
	       (or db
		   (setq db *db*))
	       (setq db1 (getdb1 db))
	       (setq db2 (getdb2 db))
	       (delq item (gethash1 unique db1))
	       (and (not (\=& 0 (setq focus (gethashfocus defblock))))
		    (removebyfocus (getvalue focus item) item db1 db2))
	       (for slotnum 1 (getstructlength defblock)
		    (setq hashinfo (gethashinfo slotnum defblock))
		    (removeslot))
	       (and mark***
		    (delq item (gethashmulti unique mark*** db2)))
	       (checkrunhandlebasehooks1 '>removedb *runremovedbhooks*)
	       (return item)))))
 
; Find the next item on the CDDR list of the stream that is STREQUAL to
; the CADR of the stream and return it, also updating the stream.
(de nextequal (stream)
  (or (streamp stream)
      (progn (msg t "NEXTEQUAL:  not a stream: " stream t)
	     (pearlbreak)))
  (setq stream (cdr stream))	 ; Throw away the *STREAM*.
  (cond ((eq t (car stream))	 ; This means function structure.
	 (prog1 (evalfcn (cdr stream))
		(rplacd (rplaca stream nil) nil)))
	((null (cadr stream)) nil)   ; Test for empty stream
	; Stream built by standardfetch.
	; To debug or modify this, you must draw a picture of what
	;   standardfetch built because of the way it is written.
	((not (dtpr (cadr stream)))
	 (prog (item result)
	       (setq item (car stream))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<nextequal *runnextequalhooks*)
	       (while (and (cdr stream)
			   (or (eq (cadr stream) '*db*)
			       (not (strequal item (cadr stream)))))
		      (rplacd stream (cddr stream)))
	       (cond ((cadr stream)
		      (setq item (cadr stream)))
		     ( t (setq item nil)))
	       (rplacd stream (cddr stream))
	       (checkrunhandlebasehooks1 '>nextequal *runnextequalhooks*)
	       (return item)))
	; Stream built by expandedfetch (or fetcheverywhere).
	; To debug or modify this, you must draw a picture of what
	;   expandedfetch built because of the way it is written.
	((not (dtpr (caadr stream)))
	 (prog (item result)
	       (setq item (car stream))
	       (setq *currentpearlstructure* item)
	       (checkrunhandlebasehooks1 '<nextequal *runnextequalhooks*)
	       (while (and (cdr stream)
			   (or (eq (caadr stream) '*db*)
			       (not (strequal item (caadr stream)))))
		      (or (car (rplaca (cdr stream) (cdadr stream)))
			  (rplacd stream (cddr stream))))
	       (cond ((cadr stream)
		      (setq item (caadr stream)))
		     ( t (setq item nil)))
	       (or (not (cdr stream))
		   (car (rplaca (cdr stream) (cdadr stream)))
		   (rplacd stream (cddr stream)))
	       (checkrunhandlebasehooks1 '>nextequal *runnextequalhooks*)
	       (return item)))))
 
; Find out if an EQUAL ITEM is in the DB by using FETCH and then
;  applying NEXTEQUAL.
(de indb (item &optional (db *db*))
  (prog (result newitem answer)
	(setq *currentpearlstructure* item)
	(checkrunhandlebasehooks1 '<indb *runindbhooks*)
	(setq newitem nil)
	(and (setq answer (nextequal (fetch item db)))
	     (setq newitem (setq item answer)))
	(checkrunhandlebasehooks1 '>indb *runindbhooks*)
	(and newitem
	     (neq item newitem)
	     (setq answer item))
	(return answer)))

; (FOREACH STREAM FCN) applies FCN to each element returned by
;    NEXTITEM from STREAM.
(df foreach (l)
  (let ((stream (eval (car l)))
	(fcn (cadr l))
	item)
       (while (setq item (nextitem stream))
	      (apply* fcn (ncons item)))))
 
; Convert a stream to a list of actual matchers.
(de streamtolist (stream)
  (let ((result (ncons nil))
	item)
       (while (setq item (nextitem stream))
	      (tconc result item))
       (car result)))
 

; vi: set lisp:
