; -*- Package:SYSTEM-INTERNALS; Mode:LISP; Base:8 -*-
; MACHINE MISCELLANEOUS FUNCTIONS NOT WORTHY OF BEING IN QFCTNS
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
(setq rcs-describe-
   "$Header: describe.l,v 1.3 85/03/24 11:23:34 sklower Exp $")

(setq SCCS-describe "@(#) describe.l	1.1	83/01/27 @(#)")

;Describe anything

(environment-lmlisp (compile eval) (files struct flavorm))


(declare (special indent))

(defun describe (anything &optional no-complaints &aux (indent 0))
  (describe-2 anything no-complaints))

(defun describe-2 (anything no-complaints &aux type)
  (cond ((named-structure-p anything)
	 (describe-defstruct anything))
	((and (instancep anything)
	      (get-handler-for anything ':describe))
	 (send anything ':describe))
	((:typep anything 'flavor)
	 (describe-flavor anything))
	((arrayp anything)
	 (describe-array anything))
	((symbolp anything)
	 (describe-symbol anything))
	((listp anything)
	 (describe-list anything))
	((floatp anything)
	 (describe-flonum anything))
	((bigp anything)
	 (describe-bignum anything))
	((fixp anything)
	 (format t "~%~vX~R is ~[even~;odd~]"
		 indent anything (if (evenp anything) 0 1)))
	((not no-complaints)
	 (format t "~%I don't know how to describe ~S" anything)))
  (terpri)
  anything)

(defun describe-1 (thing)	;an internal subroutine
  (cond ((or (null thing) ;Don't recursively describe relatively boring things
	     (numberp thing) (symbolp thing) (stringp thing))
	 nil)
	(t (let ((indent (+ indent 4)))
		(describe-2 thing t))
	   (terpri))))

(defun describe-symbol (sym)
  (cond ((boundp sym)
	 (let ((prinlevel 2) (prinlength 3))
	   (format t  "~%~vXThe value of ~S is ~S" indent sym (symeval sym)))
	 (describe-1 (symeval sym))))
  (cond ((fboundp sym)
	 (let ((prinlevel 2) (prinlength 3))
	   (format t "~%~vX~S is the function ~S: ~S"
		   indent sym (getd sym) '(???)))
	 (describe-1 (getd sym))))
  (do ((pl (plist sym) (cddr pl))
       (prinlevel 2)
       (prinlength 3))
      ((null pl))
;   (format t "~%~~vXS has property ~S: ~S"	; SMH@MIT-EMS
    (format t "~%~vX~S has property ~S: ~S"
	    indent sym (car pl) (cadr pl))
    (describe-1 (cadr pl)))
  nil)

(defun describe-list (l)
  (format t "~%~vX~S is a list" indent l))

;Fixed indent botch: this is not necessarily called from describe!  SMH@EMS
(defun describe-defstruct
       (x &optional defstruct-type
	  &aux description
	       (indent (cond ((and (boundp 'indent) (fixp indent)) indent)
			     (t 0))))
  (setq description (get (or defstruct-type (named-structure-symbol x))
			 'defstruct-description))
; (format t "~%~vX~S is a ~S~%" indent x (defstruct-description-name)) SMH@EMS
  (format t "~%~vX~S is a ~S~%" indent x
	  (defstruct-description-name description))
  (do l (defstruct-description-slot-alist) (cdr l) (null l)
      (format t "~vX   ~30A~S~%"
	      indent
	      (concat (caar l) ":")
	      (eval `(,(defstruct-slot-description-ref-macro-name (cdar l))
		      ',x)))))

(defun describe-fclosure (cl)
  (format t "~vX~%~S is an fclosure of ~S:~%" cl (fclosure-function cl))
  (loop for pair in (fclosure-alist cl)
	do (format t "~vX   Value cell of ~S:        ~32,7S~%"
		   indent
		   (car pair) (cadr pair))))

(defun describe-flonum (x)
  (format t "~%~vX~S is a flonum.~%  " indent x)
;;  (format T "Excess-2000 exponent ~O, 32-bit mantissa ~O~4,48O~4,48O (including sign)")
  )

(defun describe-bignum (x)
  (let ((len (haulong x))
	(barf nil))
    (format t "~&~S is a bignum.~&It is ~R word~:P long."
	    x len)
    (terpri))
  x)

(defun describe-array (array &aux arraydims ndims)
 (cond ((arrayp array)
	(format t "~vX~%This is a ~S type array."
		indent (car (getaux array)))
	(setq arraydims (cdr (arraydims array)))
	(setq ndims (length arraydims))
	(cond ((> ndims 1)
	       (format t  "~vX~%It is ~D-dimensional, with dimensions "
		       indent ndims)
	       (do l arraydims (cdr l) (null l)
		   (format t "~s " (car l))))
	      (t (format t "~%It is ~S long." (car arraydims)))))
       (t (ferror nil "~S is not an array" array))))

(declare (macros t))

(defmacro mapatoms (fcnt) `(mapc ,fcnt (oblist)))

(declare (special apropos-substring return-list))

(defun apropos (apropos-substring &rest rest
		&aux return-list)
  rest 
  (mapatoms #'apropos-1 pkg)
  return-list)

(defun apropos-1 (symbol)
  (cond ((within-string apropos-substring (get_pname symbol))
	 (push symbol return-list)
	 (format t "~%~s" symbol)
	 (and (fboundp symbol)
	      (format t " - Function"))
	 (and (boundp symbol)
	      (cond ((fboundp symbol) (princ ", Bound"))
		    (t (princ " - Bound")))))))

(defun within-string (s1 s2 &aux (len (flatc s1)))
  (loop for i from 1 to (flatc s2)
	with fc = (getcharn s1 1)
	when (and (= (getcharn s2 i) fc)
		  (eqstr (substring s2 i len) s1))
	return t))
	
