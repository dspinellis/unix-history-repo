;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hook.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for filling in, running and processing the results of
;    both slot and base hooks.  Also, hidden and visible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Convert an equal sign followed by an atom into (*SLOT* atom) 
;    for use in both predicates and hooks.
(drm \=
  (lambda ()
	  (let ((nextchar (tyipeek)))
	       (cond ((\=&  9. nextchar) '\=)
		     ((\=& 10. nextchar) '\=)
		     ((\=& 13. nextchar) '\=)
		     ((\=& 32. nextchar) '\=)
		     ((\=& 41. nextchar) '\=)
		     ((eqstr (ascii nextchar) '\=)
		      (readc)
		      '\=\=)
		     ( t  (list '*slot* (read)))))))
 
; Convert a slotname into a slot number for a particular type of structure.
(defmacro numberofslot (slotname defblock)
  `(for slotnum 1 (getstructlength ,defblock)
	(and (memq ,slotname (getslotname slotnum ,defblock))
	     (return slotnum)))
  )
 
; Fill a predicate or hook (FCN) in with the right things, using
;         VALUE  for   *  or  >*,
;	  ITEM   for   ** or  >** and to find variables and slotvalues,
;     and DEFBLOCK to find slotnumbers.
(de fillin1 (fcn value item defblock)
  (cond ((null fcn) nil)
	((atom fcn) (cond ((eq '** fcn) (list 'quote item))
			  ((eq '* fcn) (list 'quote value))
			  ((eq '>** fcn) (list 'quote item))
			  ((eq '>* fcn) (list 'quote value))
			  ( t fcn)))
	((dtpr fcn)
	 (cond ((eq '*slot* (car fcn))
		(list 'quote
		      (getvalue (numberofslot (cadr fcn) defblock)
				item)))
	       ((eq '*var* (car fcn))
		(list 'quote
		      (valueof (cadr fcn) item)))
	       ((eq '*global* (car fcn))
		(cadr fcn))
	       ( t (mapcar (funl (x) (fillin1 x value item defblock))
			   fcn))))
	(  t  fcn)))

; Fill a two-item predicate or hook (FCN) in with the right things, using
;         VAL1   for   *
;         VAL2   for   >*
;	  ITEM1  for   ** and to find variables and slotvalues,
;	  ITEM2  for   >**
;         RESULT for   ?
;     and DEFBLOCK to find slotnumbers.
; Must be made into a LEXPR in UCI Lisp because of number of arguments.
(de fillin2 (fcn val1 val2 item1 item2 defblock result)
  (cond ((null fcn) nil)
	((atom fcn) (cond ((eq '** fcn)  (list 'quote item1))
			  ((eq '>** fcn) (list 'quote item2))
			  ((eq '* fcn)   (list 'quote val1))
			  ((eq '>* fcn)  (list 'quote val2))
			  ((eq '\? fcn)  (list 'quote result))
			  ( t fcn)))
	((dtpr fcn)
	 (cond ((eq '*slot* (car fcn))
		(list 'quote
		      (getvalue (numberofslot (cadr fcn) defblock)
				item1)))
	       ((eq '*var* (car fcn))
		(list 'quote
		      (valueof (cadr fcn) item1)))
	       ((eq '*global* (car fcn))
		(cadr fcn))
	       ( t (mapcar (funl (x) (fillin2 x val1 val2
					      item1 item2
					      defblock result))
			   fcn))))
	( t   fcn)))
 
; If an atom, apply it, else fill it in and evaluate it.
(defmacro executehook1 (fcn value item defblock)
  `(cond ((atom ,fcn) (apply* ,fcn (ncons ,value)))
	 (  t  (eval (fillin1 ,fcn ,value ,item ,defblock)))))
 
; If an atom, apply it, else fill it in and evaluate it.
(defmacro executehook2 (fcn val1 val2 item1 item2 defblock result)
  `(cond ((atom ,fcn) (apply* ,fcn (list ,val1 ,val2)))
	 (  t  (eval (fillin2 ,fcn ,val1 ,val2
			      ,item1 ,item2 ,defblock ,result)))))

; If slothooks are supposed to be run, run them and check for *done*,
;    *fail* or *use*, doing the appropriate thing.  Can almost be
;    used alone but assumes SLOTNUM, ITEM, RESULT, and VALUE.
(defmacro checkrunhandleslothooks1 (fcn runhooksatom)
  `(and *runallslothooks*
	,runhooksatom
	(setq result
	      (let ((defblock (getdefinition item))
		    (alist (getslothooks slotnum item))
		    (retvalue nil)
		    pair)
		   (while (and (not retvalue)
			       (setq pair (pop alist)))
			  (and (eq (car pair) ,fcn)
			       (setq retvalue
				     (executehook1 (cdr pair) value
						   item defblock))
			       (or (and (dtpr retvalue)
					(memq (car retvalue)
					      '(*fail* *done* *use*)))
				   (setq retvalue nil))))
		    retvalue))
	(dtpr result)
	(selectq (car result)
		 (*done* (and (cdr result)
			      (return (cadr result)))
			 (return value))
		 (*fail* (and (cdr result)
			      (return (cadr result)))
			 (return '*fail*))
		 (*use* (setq value (cadr result))))))

; *done* and *fail* cause an immediate return.  *use* changes the
;     value that was going to be used.
(defmacro handlehookresult (oldval newval)
  `(and (dtpr ,newval)
	(selectq (car ,newval)
		 (*done* (and (cdr ,newval)
			      (return (cadr ,newval)))
			 (return ,oldval))
		 (*fail* (and (cdr ,newval)
			      (return (cadr ,newval)))
			 (return '*fail*))
		 (*use* (setq ,oldval (cadr ,newval))))))

; If slothooks are supposed to be run, run them and check for *done*,
;    *fail* or *use*, doing the appropriate thing.  Can almost be
;    used alone but assumes RESULT and ITEM.
(defmacro checkrunhandlebasehooks1 (fcn runhooksatom)
  `(and *runallbasehooks*
	,runhooksatom
	(setq result
	      (let ((retvalue nil)
		    alist
		    pair
		    defblock)
		   (and item
			(setq defblock (getdefinition item))
			(setq alist (getbasehooks defblock)))
		   (while (and (not retvalue)
			       (setq pair (pop alist)))
			  (and (eq (car pair) ,fcn)
			       (setq retvalue
				     (executehook1 (cdr pair) item
						   item defblock))
			       (or (and (dtpr retvalue)
					(memq (car retvalue)
					      '(*fail* *done* *use*)))
				   (setq retvalue nil))))
		   retvalue))
	(dtpr result)
	(selectq (car result)
		 (*done* (and (cdr result)
			      (return (cadr result)))
			 (return item))
		 (*fail* (and (cdr result)
			      (return (cadr result)))
			 (return '*fail*))
		 (*use* (setq item (cadr result))))))

; If slothooks are supposed to be run, run them.  Assumes SLOTNUM,
;    ITEM, and VALUE.  This is not a standalone function, since it
;    does not handle RESULT but rather returns it.
(defmacro checkandrunslothooks2 (fcn hooks val1 val2 item1 item2)
  `(let ((defblock (getdefinition ,item1))
	 (retvalue nil)
	 pair)
	(while (and (not retvalue)
		    (setq pair (pop ,hooks)))
	       (and (eq (car pair) ,fcn)
		    (setq retvalue
			  (executehook2 (cdr pair) ,val1 ,val2
					,item1 ,item2 defblock result))
		    (or (and (dtpr retvalue)
			     (memq (car retvalue)
				   '(*fail* *done* *use*)))
			(setq retvalue nil))))
	 retvalue))

; Assumes XVAL or YVAL is where you want changes.
(defmacro doslothooks2< (fcn runhookatom)
  `(cond ((and *runallslothooks*
	       ,runhookatom)
	  (setq newxval nil)
	  (setq newyval nil)
	  (and (setq xhooks (getslothooks slotnum item1))
	       (setq newxval
		     (checkandrunslothooks2 ,fcn xhooks xval yval
					    item1 item2)))
	  (and (setq yhooks (getslothooks slotnum item2))
	       (setq newyval
		     (checkandrunslothooks2 ,fcn yhooks yval xval
					    item2 item1)))
	  (handlehookresult xval newxval)
	  (handlehookresult yval newyval))))

; Assumes RESULT is where you want changes.
(defmacro doslothooks2> (fcn runhookatom)
  `(cond ((and *runallslothooks*
	       ,runhookatom)
	  (setq newxval nil)
	  (setq newyval nil)
	  (and (setq xhooks (getslothooks slotnum item1))
	       (setq newxval
		     (checkandrunslothooks2 ,fcn xhooks xval yval
					    item1 item2)))
	  (and (setq yhooks (getslothooks slotnum item2))
	       (setq newyval
		     (checkandrunslothooks2 ,fcn yhooks yval xval
					    item2 item1)))
	  (handlehookresult result newxval)
	  (handlehookresult result newyval))))

(defmacro checkandrunbasehooks2 (fcn item1 item2)
  `(let* ((retvalue nil)
	  (defblock (getdefinition ,item1))
	  (alist (getbasehooks defblock))
	  pair)
	 (while (and (not retvalue)
		     (setq pair (pop alist)))
		(and (eq (car pair) ,fcn)
		     (setq retvalue
			   (executehook2 (cdr pair) ,item1 ,item2
					 ,item1 ,item2 defblock result))
		     (or (and (dtpr retvalue)
			      (memq (car retvalue)
				    '(*fail* *done* *use*)))
			 (setq retvalue nil))))
	 retvalue))

; Assumes ITEM1 and ITEM2 are where you want changes.
(defmacro dobasehooks2< (fcn runhookatom)
  `(cond ((and *runallbasehooks*
	       ,runhookatom)
	  (setq newitem1 (checkandrunbasehooks2 ,fcn item1 item2))
	  (setq newitem2 (checkandrunbasehooks2 ,fcn item2 item1))
	  (handlehookresult item1 newitem1)
	  (handlehookresult item2 newitem2))))

; Assumes RESULT is where you want changes.
(defmacro dobasehooks2> (fcn runhookatom)
  `(cond ((and *runallbasehooks*
	       ,runhookatom)
	  (setq newitem1 (checkandrunbasehooks2 ,fcn item1 item2))
	  (setq newitem2 (checkandrunbasehooks2 ,fcn item2 item1))
	  (handlehookresult result newitem1)
	  (handlehookresult result newitem2))))

; Runbasehooks for single items for the user.
(de runbasehooks1 (fcn item)
  (and (null item)
       (progn (msg t "RUNBASEIFS1: Null item given to run hooks on." t)
	      (pearlbreak)))
  (let* ((retvalue nil)
	 (defblock (getdefinition item))
	 (alist (getbasehooks defblock))
	 pair)
	(while (and (not retvalue)
		    (setq pair (pop alist)))
	       (and (eq (car pair) fcn)
		    (setq retvalue (executehook1 (cdr pair) item item defblock))
		    (or (and (dtpr retvalue)
			     (memq (car retvalue) '(*fail* *done* *use*)))
			(setq retvalue nil))))
	retvalue))

; Runbasehooks for two items for the user.
(de runbasehooks2 (fcn item1 item2 result)
  (and (null item1)
       (progn (msg t "RUNBASEIFS2: Null first item given to run hooks on." t)
	      (pearlbreak)))
  (and (null item2)
       (progn (msg t "RUNBASEIFS2: Null second item given to run hooks on." t)
	      (pearlbreak)))
  (let* ((retvalue nil)
	 (defblock (getdefinition item1))
	 (alist (getbasehooks defblock))
	 pair)
	(while (and (not retvalue)
		    (setq pair (pop alist)))
	       (and (eq (car pair) fcn)
		    (setq retvalue
			  (executehook2 (cdr pair) item1 item2
					item1 item2 defblock result))
		    (or (and (dtpr retvalue)
			     (memq (car retvalue) '(*fail* *done* *use*)))
			(setq retvalue nil))))
	retvalue))

; Run slot hooks for the slot named SLOTNAME for one item for the user.
(de runslothooks1 (fcn item slotname value)
  (and (null item)
       (progn (msg t "RUNSLOTIFS1: Null item given to run hooks on." t)
	      (pearlbreak)))
  (let* ((retvalue nil)
	 (defblock (getdefinition item))
	 (slotnum (numberofslot slotname defblock))
	 (alist (getslothooks slotnum item))
	 pair)
	(while (and (not retvalue)
		    (setq pair (pop alist)))
	       (and (eq (car pair) fcn)
		    (setq retvalue
			  (executehook1 (cdr pair) value item defblock))
		    (or (and (dtpr retvalue)
			     (memq (car retvalue) '(*fail* *done* *use*)))
			(setq retvalue nil))))
	retvalue))

; Run slot hooks for the slot named SLOTNAME for two items for the user.
; Must be made into a LEXPR in UCI Lisp because of number of arguments.
(de runslothooks2 (fcn item1 item2 slotname val1 val2 result)
  (and (null item1)
       (progn (msg t "RUNSLOTIFS1: Null first item given to run hooks on." t)
	      (pearlbreak)))
  (and (null item2)
       (progn (msg t "RUNSLOTIFS1: Null second item given to run hooks on." t)
	      (pearlbreak)))
  (let* ((retvalue1 nil)
	 (retvalue2 nil)
	 (defblock (getdefinition item1))
	 (slotnum (numberofslot slotname defblock))
	 (alist (getslothooks slotnum item1))
	 pair)
	(while (and (not retvalue1)
		    (setq pair (pop alist)))
	       (and (eq (car pair) fcn)
		    (setq retvalue1
			  (executehook2 (cdr pair) val1 val2
					item1 item2 defblock result))
		    (or (and (dtpr retvalue1)
			     (memq (car retvalue1) '(*fail* *done* *use*)))
			(setq retvalue1 nil))))
	(setq defblock (getdefinition item2))
	(setq slotnum (numberofslot slotname defblock))
	(setq alist (getslothooks slotnum item2))
	(while (and (not retvalue2)
		    (setq pair (pop alist)))
	       (and (eq (car pair) fcn)
		    (setq retvalue2
			  (executehook2 (cdr pair) val2 val1
					item2 item1 defblock result))
		    (or (and (dtpr retvalue2)
			     (memq (car retvalue2) '(*fail* *done* *use*)))
			(setq retvalue2 nil))))
	(cons retvalue1 retvalue2)))

; Run command with its associated *run...hooks* atom set to nil
;    temporarily with a let so that its hooks WON'T be run.
(defmacro hidden (command)
  (let ((name (concat '*run (car command) 'hooks*)))
       `(let ((,name nil))
	     ,command)))

; Run command with its associated *run...hooks* atom set to t
;    temporarily with a let so that its hooks WILL be run.
(defmacro visible (command)
  (let ((name (concat '*run (car command) 'hooks*)))
       `(let ((,name t))
	     ,command)))

; vi: set lisp:
