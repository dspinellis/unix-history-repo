;;;;;;;;;;;;;;;;;;;;;;;;;;;;; symord.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions for defining symbols and ordinal types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; Define one SYMBOL in a hunk for easy identification.
;   This will not work independently (for some reason).
(dm onesymbol (none)
  '(funl (symname)
	 (or (and (not (litatom symname))
		  (not (msg t "SYMBOL: Symbols can only be simple names, not:"
			    symname t)))
	     (and (eq symname 'nilsym)
		  (boundp (symatom 'nilsym))
		  (not (msg t "SYMBOL: Cannot redefine nilsym." t)))
	     (and (null symname)
		  (not (msg t "SYMBOL: Cannot name a symbol nil." t)))
	     (and (symbolnamep symname)
		  ; but okay to do.
		  (and *warn*
		       (msg t "SYMBOL: Warning: Redefining symbol: "
			    symname t)))
	     (let ((block (set (symatom symname) (makhunk 3))))
		  (putuniquenum (newnum) block)
		  (puttypetag '*pearlsymbol* block)
		  (putsymbolpname symname block)
		  block))))
 
; Define a bunch of SYMBOLS.
(df symbol (l)
  (mapcar (onesymbol) l))

; An EXPR which allows the defining of one SYMBOL.
(de symbole (symname)
  (cond ((not (litatom symname))
	 (msg t "SYMBOLE: symbols can only be simple names, not: "
	      symname t)
	 (pearlbreak))
	(  t  (apply* (onesymbol) (ncons symname)) symname)))
 
(de getsymbol (symname)
  (cond ((symbolnamep symname)
	 (eval (symatom symname)))
	(  t  (msg t "GETSYMBOL: " symname " is not the name of a symbol." t)
	      (pearlbreak))))
 
; (ordinal name (x y z)) or  (ordinal name (x 1 y 3 z 8)).
; Define a set of integer constants for readability in input and output.
; Also define o:name, name:max and name:min, and name:x, name:y and name:z.
(df ordinal (l)
  (let ((ordinalname (car l))
	(ordinalelements (cadr l))
	(alist (ncons nil))
	(count 0)
	(min 0)
	max
	name
	value)
       (push ordinalname *ordinalnames*)
       (set (ordatom ordinalname)
	    (cond ((not (numberp (cadr ordinalelements)))
		   ; generate numbers.
		   (while ordinalelements
			  (setq count (1+ count))
			  (tconc alist (cons (setq name (pop ordinalelements))
					     count))
			  (set (concat ordinalname ":" name) count))
		   (or (\=& 0 count)
		       (setq min 1))
		   (setq max count)
		   (car alist))
		  ; use numbers provided by user.
		  ( t (setq min (setq max (cadr ordinalelements)))
		      (while ordinalelements
			     (tconc alist
				    (cons (setq name (pop ordinalelements))
					  (setq value (pop ordinalelements))))
			     (set (concat ordinalname ":" name) value)
			     (and (<& value min)
				  (setq min value))
			     (and (>& value max)
				  (setq max value)))
		      (car alist))))
       (set (concat ordinalname ":min") min)
       (set (concat ordinalname ":max") max)
       (cons ordinalname (car alist))))
 

; vi: set lisp:
