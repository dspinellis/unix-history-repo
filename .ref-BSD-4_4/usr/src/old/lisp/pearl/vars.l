;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vars.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for declaring and creating pattern-matching variables
;    and blocks and for freezing and thawing them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Convert a question mark variable ?var to either (*global* var) if "var"
;    is in *globallist* or else make it local (*var* var).
(drm \?
  (lambda ()
	  (let ((nextchar (tyipeek))
		var)
	       (cond ((\=&  9. nextchar) '\?)
		     ((\=& 10. nextchar) '\?)
		     ((\=& 13. nextchar) '\?)
		     ((\=& 32. nextchar) '\?)
		     ((\=& 41. nextchar) '\?)
		     ( t (setq var (read))
			 (cond ((memq var *globallist*)
				(list '*global* var))
			       (  t  (list '*var* var))))))))
 
; VALUEOF and VARVALUE are EXPR and FEXPR versions of a function to
;     get the value of the variable VAR in the structure STRUCT.
(de valueof (var struct)
  (getvalofequivorvar
   (cdr (or (assq var (getalist struct))
	    (assq var (getalistcp struct))
	    (progn (msg t "VALUEOF: Variable " var
			" does not occur in structure:" struct t)
		   (pearlbreak))))))
 
;    This is a FEXPR version of valueof (above).
(df varvalue (l)     ; (VAR STRUCT)
  (let ((var (car l))
	(struct (eval (cadr l))))
       (getvalofequivorvar
	(cdr (or (assq var (getalist struct))
		 (assq var (getalistcp struct))
		 (progn (msg t "VARVALUE: Variable " var
			     " does not occur in structure:" struct t)
			(pearlbreak)))))))

; Set the given variable, in the given environment (if present) to
;    the value given.  If no environment given, look first at
;    *currentstructure*, then at *currentpearlstructure*, then at
;    *blockstack*, else complain.
(df setv (l)    ; (var 'val 'environment)
  (let*
   ((var (car l))
    (type (car var))
    (name (cadr var))
    (val (eval (cadr l)))
    (environment (eval (caddr l)))
    varcell
    oldvarval)
   (cond ((eq '*global* type)   ; global variable.
	  (setq oldvarval (eval name))
	  (set name val))
	 ((eq '*var* type)      ; local or block variable.
	  (cond (environment
		 ; optional 3rd argument given for environment.
		 (cond ((structurep environment)
			(setq varcell
			      (or (assq name (getalist environment))
				  (assq name (getalistcp environment))
				  (progn (msg t "SETV: No variable named: " name
					      " in structure: " t environment t)
					 (pearlbreak)))))
		       ((blockp environment)
			(setq varcell
			      (or (assq name environment)
				  (progn (msg t "SETV: No variable named: " name
					      " in block: " t environment t)
					 (pearlbreak)))))
		       ( t (msg t "SETV: Given environment is neither "
				"a block nor a structure: " t environment)
			   (pearlbreak))))
		; otherwise, try to find in standard environment.
		((setq varcell
		       (or (and (structurep *currentstructure*)
				(or (assq name (getalist *currentstructure*))
				    (assq name (getalistcp *currentstructure*))
				    ))
			   (and (structurep *currentpearlstructure*)
				(or (assq name
					  (getalist *currentpearlstructure*))
				    (assq name
					  (getalistcp *currentpearlstructure*))
				    ))
			   (and *blockstack*
				(assq name (cdar *blockstack*))))))
		( t ; Else if not there either, blow up.
		    (msg t "SETV: No variable in the current"
			 " environment named: " name t)
		    (pearlbreak)))
	  ; Successfully found the variable.
	  (and varcell
	       (setq oldvarval (cdr varcell))
	       (rplacd varcell val)))
	 ( t (msg t "SETV: " var " is not a variable." t)
	     (pearlbreak)))
   (and (equivclassp oldvarval)
	(mapc (funl (newvar) (cond ((dtpr newvar)	    ; a local var cell.
				    (and (eq (cdr newvar) oldvarval)
					 (rplacd newvar val)))
				   ( t ; otherwise a global var's name.
				       (and (eq (eval newvar) oldvarval)
					    (set newvar val)))))
	      (cdr oldvarval)))
   val))

; Get the value of a local variable.   Look in the same places as
;    SETV above but return nil if not found.
(df *var* (l)
  (let ((var (car l)))
       (getvalofequivorvar
	  (cdr (or (and (structurep *currentstructure*)
			(or (assq var (getalist *currentstructure*))
			    (assq var (getalistcp *currentstructure*))))
		   (and (structurep *currentpearlstructure*)
			(or (assq var (getalist *currentpearlstructure*))
			    (assq var
				  (getalistcp *currentpearlstructure*))))
		   (and *blockstack*
			(assq var (cdar *blockstack*))))))))

; Get the value of a global variable.
(df *global* (l)
  (getvalofequivorvar
     (eval (car l))))

; Declare a variable to be GLOBAL by entering it on the *GLOBALLIST*
;    and PEARL-unbinding it.
(df global (l)
  (let ((variable (car l)))
       (set variable (punbound))
       (push variable *globallist*)
       variable))
 
; PEARL-unbind a global variable. ("unbindvars" does the local variables
;    in an entire structure (see match.l)).
(df unbind (l)
  (let ((var (car l)))
       (cond ((memq var *globallist*)
	      (set var (punbound)))
	     ( t (set var (punbound))
		 (and *warn*
		      (msg t "UNBIND: Warning: " var
			   " is not a global variable but unbound it anyway."
			   t))))))

; Determine if the variable is GLOBAL, i.e., on the *GLOBALLIST*
(de globalp (variable)
  (memq variable *globallist*))
 
; (BLOCK <name> (<LIST OF VARIABLES>)) starts a (possibly embedded)
;    set of variables accessible to all structure CREATEd within
;    the block.   Terminated by a call to (ENDBLOCK <name>).
; The name is optional.  If used, then the block may be reaccessed
;    with b:<name>.
 
(df block (l)
  (let ((name (car l))
	varlist
	alist)
       (cond ((reallitatom name) (setq varlist (cadr l)))
	     ( t  (setq varlist name)
		  (setq name 'unnamedblock)))
       (setq alist
	     (nconc (ncons (cons nil (punbound)))  ; Cell for Frozen vars.
		    (mapcar (funl (varname) (cons varname (punbound)))
			    varlist)
		    (cond (*blockstack* (cdar *blockstack*))
			  ( t nil))))
       (and name
	    (set name alist))
       ; Create a special cons cell, point b:<name> at it and push it.
       (push (set (blockatom name)
		  (cons name alist))
	     *blockstack*)
       name))
 
; (ENDBLOCK <name>) ends the block with name <name>.
;    If <name> is * then close one block, regardless of name.
;    If <name> is nil then close one unnamed block only.
(df endblock (l)
  (let ((name (car l)))
       (and (null name)
	    (setq name 'unnamedblock))
       (cond ((not *blockstack*)
	      (msg t "ENDBLOCK: No blocks to end")
	      (msg ", not even named: " name t)
	      (pearlbreak))
	     ((or (eq name '*)
		  (eq name (caar *blockstack*)))
	      (prog1 (caar *blockstack*)
		     (setq *blockstack* (cdr *blockstack*))))
	     ( t (msg t "ENDBLOCK: Block to be ended, "
		      name " doesn't match innermost block, named: "
		      (caar *blockstack*) t)
		 (pearlbreak)))))

; (ENDANYBLOCKS <name>) ends all blocks back through the block
;    with name <name>.
; If <name> is * then end all blocks.
; If <name> is nil then end all blocks back through the
;    last unnamed block.
(df endanyblocks (l)
  (let ((name (car l))
	(block *blockstack*))
       (cond ((not *blockstack*)    nil)
	     ((eq name '*)       (setq *blockstack* nil))
	     ((null (while (and block
				(neq (caar block) name))
			   (setq block (cdr block))))
	      (msg t "ENDANYBLOCKS: No currently open block named "
		   name " to end blocks back to." t)
	      (pearlbreak))
	     ( t (setq *blockstack* (pop block))
		 (caar *blockstack*)))
       t))

; (ENDALLBLOCKS <name>) ends any open blocks, regardless of name.
(de endallblocks ()
  (setq *blockstack* nil)
  t)

; (SETBLOCK <blockname>) changes the current scope to that of
;      <blockname>, BUT doesn't allow ending former blocks!
(df setblock (l)
  (let ((blockname (car l)))
       (cond ((and (boundp (blockatom blockname))
		   (blockp (eval (blockatom blockname))))
	      (setq *blockstack* (eval (blockatom blockname))))
	     ( t (msg t "SETBLOCK: There is no block named: " blockname t)
		 (pearlbreak)))))
 
; Take all the bound variables off the STRUCT'S ALIST, and put them on
; the ALISTCP, preserving unique alist pairs.  Also take care of all the
; BLOCK alists.  WARNING: This code is tough so be careful with it!
(de freezebindings (struct)
  (let ((oldalist (getalist struct))     ; to be frozen.
	(unboundalist (ncons nil))       ; to still unbound variables.
	(boundalist (getalistcp struct)) ; already frozen.
	rest
	currentblock)
       ; While there are more variables to process, and we haven't reached
       ;     a block, add either to "unboundalist" or "boundalist".
       (while (and oldalist
		   (reallitatom (caar oldalist)))
	      (setq rest (cdr oldalist))
	      (cond ((eq (cdar oldalist) (punbound))
		     (tconc unboundalist (car oldalist)))
		    ( t (setq boundalist (rplacd oldalist boundalist))))
	      (setq oldalist rest))
       (and oldalist
	    (rplaca unboundalist
		    (nconc (car unboundalist)
			   oldalist))) ; pointer to the enclosing blocks.
       ; Store new lists.
       (putalist (car unboundalist) struct)
       (putalistcp boundalist struct)
       ; Process blocks one at a time.
       (while oldalist
	      (setq currentblock oldalist)
	      (setq oldalist (cdr oldalist))
	      (setq unboundalist (ncons nil))
	      (setq boundalist (caar currentblock))
	      (while (and oldalist
			  (reallitatom (caar oldalist)))
		     (setq rest (cdr oldalist))
		     (cond ((eq (cdar oldalist) (punbound))
			    (tconc unboundalist (car oldalist)))
			   ( t (setq boundalist (rplacd oldalist boundalist))))
		     (setq oldalist rest))
	      (and oldalist
		   (rplaca unboundalist
			   (nconc (car unboundalist)
				  oldalist))) ; pointer to the enclosing blocks.
	      ; store frozen vars.
	      (rplaca (car currentblock) boundalist)
	      (rplacd currentblock (car unboundalist)))
       t))
 
; Take all the bound variables off the STRUCT's ALIST, and put them on
;   the ALISTCP, preserving unique alist pairs.
(de freezestruct (struct)
  (let ((oldalist (getalist struct))
	(unboundalist (ncons nil))
	(boundalist (getalistcp struct))
	rest)
       (while (and oldalist                       ; is not NIL, and
		   (reallitatom (caar oldalist))) ; have not reached block
	      (setq rest (cdr oldalist))
	      (cond ((eq (cdar oldalist) (punbound))
		     (tconc unboundalist (car oldalist)))
		    ( t (setq boundalist (rplacd oldalist boundalist))))
	      (setq oldalist rest))
       (and oldalist
	    (rplaca unboundalist
		    (nconc (car unboundalist)
			   oldalist))) ; pointer to the enclosing blocks.
       (putalist (car unboundalist) struct)
       (putalistcp boundalist struct)
       t))
 
(df freezeblock (blockname)
  (let (block
	oldalist
	unboundalist
	boundalist
	rest)
       (cond ((and (boundp (blockatom (car blockname)))
		   (setq block (eval (blockatom (car blockname))))
		   (blockp block)))
	     ( t (msg t "FREEZEBLOCK: " blockname
		      " is not the name of a block." t)
		 (pearlbreak)))
       (setq oldalist (cddr block))
       (setq unboundalist (ncons nil))
       (setq boundalist (caadr block))
       (while (and oldalist
		   (reallitatom (caar oldalist)))
	      (setq rest (cdr oldalist))
	      (cond ((eq (cdar oldalist) (punbound))
		     (tconc unboundalist (car oldalist)))
		    ( t (setq boundalist (rplacd oldalist boundalist))))
	      (setq oldalist rest))
       (and oldalist
	    (rplaca unboundalist
		    (nconc (car unboundalist)
			   oldalist))) ; pointer to the enclosing blocks.
       (rplaca (cadr block) boundalist) ; store frozen vars.
       (rplacd (cdr block) (car unboundalist))
       t))
 
(dm findnextblockstart (none) ; But expects ALIST
  '(while (and alist
	       (reallitatom (caar alist)))
	  (setq alist (cdr alist))))
 
; This is for JUST THE STRUCT.
(de thawstruct (struct)
  (let ((alist (getalist struct)))
       (putalist (nconc (getalistcp struct) alist) struct)
       (putalistcp nil struct)
       t))
 
; Restore the Alist to include all values. (Undo FREEZEBINDINGS)
; This is done for ALL BLOCKs that STRUCT is a member of.
(de thawbindings (struct)
  (let ((alist (getalist struct)))
       (putalist (nconc (getalistcp struct) alist) struct)
       (putalistcp nil struct)
       (while (findnextblockstart)
	      (rplacd alist (nconc (caar alist) (cdr alist)))
	      (rplaca (car alist) nil))
       t))
 
; This is for JUST ONE BLOCK.
(df thawblock (blockname)
  (let (alist
	block)
       (cond ((and (boundp (blockatom (car blockname)))
		   (setq block (eval (blockatom (car blockname))))
		   (blockp block))
	      block)
	     ( t (msg t "THAWBLOCK: " blockname
		      " is not the name of a block." t)
		 (pearlbreak)))
       (setq alist (cddr block))
       (rplacd (cdr block) (nconc (caadr block) alist))
       (rplaca (cadr block) nil)
       t))


; vi: set lisp:
