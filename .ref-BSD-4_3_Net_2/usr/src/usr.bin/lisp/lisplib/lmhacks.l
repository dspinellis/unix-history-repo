(setq rcs-lmhacks-
   "$Header: lmhacks.l,v 1.2 83/08/15 22:32:31 jkf Exp $")

;;  This file contains miscellaneous functions and macros that 
;;  ZetaLisp users often find useful


;;;  (c) Copyright 1982 Massachusetts Institute of Technology 

;; This is a simple multiple value scheme based on the one implemented
;; in MACLISP.  It doesn't clean up after its self properly, so if
;; you ask for multiple values, you will get them regardless of whether
;; they are returned.

(environment-maclisp (compile eval) (files struct flavorm))

(declare (macros t))

(defvar si:argn () "Number of arguments returned by last values")
(defvar si:arg2 () "Second return value")
(defvar si:arg3 () "Third return value")
(defvar si:arg4 () "Fourth return value")
(defvar si:arg5 () "Fifth return value")
(defvar si:arg6 () "Sixth return value")
(defvar si:arg7 () "Seventh return value")
(defvar si:arg8 () "Eigth return value")
(defvar si:arglist () "Additional return values after the eigth")

(defvar si:return-registers
  '(si:arg2 si:arg3 si:arg4 si:arg5 si:arg6 si:arg7 si:arg8))

(defmacro values (&rest values)
  `(prog2 (setq si:argn ,(length values))
	  ,(first values)
	  ,@(do ((vals (cdr values) (cdr vals))
		 (regs si:return-registers (cdr regs))
		 (forms))
		(nil)
	      (cond ((null vals)
		     (return (reverse forms)))
		    ((null regs)
		     (return
		      `(,@(reverse forms)
			(setq si:arglist (list ,@vals)))))
		    (t (push `(setq ,(car regs) ,(car vals))
			     forms))))))

(defun values-list (list)
  (setq si:argn (length list))
  (do ((vals (cdr list) (cdr vals))
       (regs si:return-registers (cdr regs)))
      ((null regs)
       (if (not (null vals))
	   (setq si:arglist vals))
       (car list))
    (set (car regs) (car vals))))

(defmacro multiple-value (vars form)
  `(progn
     ,@(if (not (null (car vars)))
	  `((setq ,(car vars) ,form)
	    (if (< si:argn 1) (setq ,(car vars) nil)))
	  `(,form))
     ,@(do ((vs (cdr vars) (cdr vs))
	    (regs si:return-registers (cdr regs))
	    (i 2 (1+ i))
	    (forms))
	   (nil)
	 (cond ((null vars)
		(return (reverse forms)))
	       ((null regs)
		(return
		 (do ((vs vs (cdr vs)))
		     ((null vs) (nreverse forms))
		   (and (not (null (car vs)))
			(push
			 `(setq ,(car vs)
				(prog1
				 (if (not (> ,i si:argn))
				     (car si:arglist))
				 (setq si:arglist (cdr si:arglist))))
			 forms)))))
	       ((not (null (car vs)))
		(push `(setq ,(car vs) (if (not (> ,i si:argn)) ,(car regs))
			     ,(car regs) nil)
		      forms))))))

(defmacro multiple-value-bind (vars form &rest body)
  `(let ,vars
	(multiple-value ,vars ,form)
	,@body))

(defmacro multiple-value-list (form)
  `(multiple-value-list-1 ,form))

(defun multiple-value-list-1 (si:arg1)
  (cond ((= 0 si:argn) ())
	((= 1 si:argn)
	 (list si:arg1))
	((= 2 si:argn)
	 (list si:arg1 si:arg2))
	((= 3 si:argn)
	 (list si:arg1 si:arg2 si:arg3))
	((= 4 si:argn)
	 (list si:arg1 si:arg2 si:arg3 si:arg4))
	((= 5 si:argn)
	 (list si:arg1 si:arg2 si:arg3 si:arg4 si:arg5))
	((= 6 si:argn)
	 (list si:arg1 si:arg2 si:arg3 si:arg4 si:arg5 si:arg6))
	((= 7 si:argn)
	 (list si:arg1 si:arg2 si:arg3 si:arg4 si:arg5 si:arg6
	       si:arg7))
	((= 8 si:argn)
	 (list si:arg1 si:arg2 si:arg3 si:arg4 si:arg5 si:arg6
	       si:arg7 si:arg8))
	((> si:argn 8)
	 (rplacd (nthcdr (- si:argn 9) si:arglist) nil)
	 (list* si:arg1 si:arg2 si:arg3 si:arg4 si:arg5 si:arg6
		si:arg7 si:arg8 si:arglist))
	(t (ferror () "Internal error, si:argn = ~D" si:argn))))

(defun union (set &rest others)
  (loop for s in others
	do (loop for elt in s
		 unless (memq elt set)
		 do (push elt set))
	finally (return set)))

(defun make-list (length &rest options &aux (iv))
  (loop for (key val) on options by #'cddr
	do (selectq key
	     (:initial-value
		(setq iv val))
	     (:area)
	     (otherwise
	      (error "Illegal parameter to make-list" key))))
  (loop for i from 1 to length collect iv))

;; si:printing-random-object
;; A macro for aiding in the printing of random objects.
;; This macro generates a form which: (by default) includes the virtual 
;; address in the printed representation.
;; Options are	:NO-POINTER to suppress the pointer
;;		:TYPEP princs the typep of the object first.

;; Example:
;; (DEFSELECT ((:PROPERTY HACKER :NAMED-STRUCTURE-INVOKE))
;;   (:PRINT-SELF (HACKER STREAM IGNORE IGNORE)
;;     (SI:PRINTING-RANDOM-OBJECT (HACKER STREAM :TYPEP)
;;       (PRIN1 (HACKER-NAME HACKER) STREAM))))
;; ==> #<HACKER /"MMcM/" 6172536765>

(defmacro si:printing-random-object ((object stream . options) &body body)
  (let ((%pointer t)
	(typep nil))
    (do ((l options (cdr l)))
	((null l))
      (selectq (car l)
	(:no-pointer (setq %pointer nil))
	(:typep (setq typep t))
	(:fastp (setq l (cdr l)))		; for compatibility sake
	(otherwise
	 (ferror nil "~S is an unknown keyword in si:printing-random-object"
		 (car l)))))
    `(progn
       (patom "#<" ,stream)
       ,@(and typep
	      `((patom (:typep ,object) ,stream)))
       ,@(and typep body
	      `((patom " " ,stream)))
       ,@body
       ,@(and %pointer
	      `((patom " " ,stream)
		(patom (maknum ,object) ,stream)))
       (patom ">" ,stream)
       ,object)))

(defun named-structure-p (x &aux symbol)
  (cond ((or (and (hunkp x) (atom (setq symbol (cxr 0 x))))
	     (and (vectorp x)
		  (setq symbol (or (and (atom (vprop x)) (vprop x))
				   (and (dtpr (vprop x))
					(atom (car (vprop x)))
					(car (vprop x)))))))
				  
	 (if (get symbol 'defstruct-description)
	     symbol))))

(defun named-structure-symbol (x)
  (or (named-structure-p x)
      (ferror () "~S was supposed to have been a named structure."
	      x)))

(declare (localf named-structure-invoke-internal))

(defun named-structure-invoke (operation struct &rest args)
  (named-structure-invoke-internal operation struct args t))

(defun named-structure-invoke-carefully (operation struct &rest args)
  (named-structure-invoke-internal operation struct args nil))

(defun named-structure-invoke-internal (operation struct args error-p)
   (let (symbol fun)
      (setq symbol (named-structure-symbol struct))
      (if (setq fun (get symbol ':named-structure-invoke))
	 then (lexpr-funcall fun operation struct args)
	 else (and error-p
		   (ferror ()
			   "No named structure invoke function for ~S"
			   struct)))))

(defmacro defselect ((function-spec default-handler no-which-operations)
		     &rest args)
  (let ((name (intern (gensym)))
	fun-name)
    `(progn 'compile
       (defun ,(if (eq (car function-spec) ':property)
		   (cdr function-spec)
		   (ferror () "Can't interpret ~S defselect function spec"
				  function-spec))
	      (operation &rest args &aux temp)
	 (if (setq temp (gethash operation (get ',name 'select-table)))
	     (lexpr-funcall temp args)
	     ,(if default-handler
		  `(lexpr-funcall ,default-handler operation args)
		  `(ferror () "No handler for the ~S method of ~S"
			   operation ',function-spec))))
       (setf (get ',name 'select-table) (make-hash-table))
       ,@(do ((args args (cdr args))
	     (form)
	     (forms nil))
  	    ((null args) (nreverse forms))
	  (setq form (car args))
	  (cond ((atom (cdr form))
		 (setq fun-name (cdr form)))
		(t (setq fun-name
			 (intern (concat name (if (atom (car form)) (car form)
						  (caar form)))))
		   (push `(defun ,fun-name ,@(cdr form)) forms)))
	  (if (atom (car form))
	      (push `(puthash ',(car form) ',fun-name
			      (get ',name 'select-table))
		    forms)
	      (mapc #'(lambda (q)
			(push `(puthash ',q ',fun-name
					(get ',name 'select-table))
			      forms))
		    (car form))))
       ,@(and (not no-which-operations)
	      `((defun ,(setq fun-name (intern
					(concat name '-which-operations)))
		       (&rest args)
		  '(:which-operations ,@(loop for form in args
					      appending (if (atom (car form))
							    (list (car form))
							    (car form)))))
		(puthash ':which-operations ',fun-name
			 (get ',name 'select-table))))
       ',function-spec)))

(defun :typep (ob &optional (type nil) &aux temp)
  (cond ((instancep ob)
	 (instance-typep ob type))
	((setq temp (named-structure-p ob))
	 (if (null type) temp
	     (if (eq type temp) t
		 (memq type (nth 11. (get temp 'defstruct-description))))))
	((hunkp ob)
	 (if (null type) 'hunk (eq type 'hunk)))
	((null type)
	 (funcall 'typep ob))
	(t (eq type (funcall 'typep ob)))))

(defun send-internal (object message &rest args)
  (declare (special .own-flavor. self))
  (lexpr-funcall (if (eq self object)
		     (or (gethash message
				  (flavor-method-hash-table .own-flavor.))
			 (flavor-default-handler .own-flavor.))
		     object)
		 message args))

;; New printer

(declare (special poport prinlevel prinlength top-level-print))

(defun zprint (x &optional (stream poport))
       (zprin1 x stream)
       't)

(defun zprinc (x &optional (stream poport))
       (zprin1a x stream () (or prinlevel -1)))

(defun zprin1 (x &optional (stream poport))
       (zprin1a x stream 't (or prinlevel -1)))

(defun zprin1a (ob stream slashifyp level &aux temp)
  (cond ((null ob) (patom "()" stream))
	((setq temp (named-structure-p ob))
	 (or (named-structure-invoke-carefully ':print-self ob stream
						level slashifyp)
	     (si:printing-random-object (ob stream :typep))))
	((instancep ob)
	 (if (get-handler-for ob ':print-self)
	     (send ob ':print-self stream)
	     (si:printing-random-object (ob stream :typep))))
        ((atom ob)
	 (if slashifyp (xxprint ob stream)
	     (patom ob stream)))
	((dtpr ob) (zprint-list ob stream slashifyp (1- level)))
	((hunkp ob) (zprint-hunk ob stream slashifyp (1- level)))
	((= level 0)
	 (patom "&" stream))
	(t
	 (if slashifyp (xxprint ob stream)
	     (patom ob stream))))
  't)

(defun zprint-list (l stream slashifyp level)
       (tyo #/( stream)
       (do ((l l (cdr l))
	    (i (or prinlength -1) (1- i))
	    (first t nil))
	   ((not (dtpr l))
	    (cond ((not (null l))
		   (patom " . " stream)
		   (zprin1a l stream slashifyp level)))
	    't)
           (cond ((= i 0)
		  (patom " ..." stream)
		  (return 't)))
	   (if (not first)
	       (tyo #/  stream))
	   (zprin1a (car l) stream slashifyp level))
       (tyo #/) stream))

(defun zprint-hunk (l stream slashifyp level)
       (tyo #/{ stream)
       (do ((i 0 (1+ i))
	    (lim (hunksize l))
	    (first t nil))
	   ((= i lim)
	    't)
           (cond ((and (not (null prinlength)) (not (< i prinlength)))
		  (patom " ..." stream)
		  (return 't)))
	   (if (not first)
	       (tyo #/  stream))
	   (zprin1a (cxr i l) stream slashifyp level))
       (tyo #/} stream))

(eval-when (load eval)
   (putd 'xxprint (getd 'print))
   (putd 'xxprinc (getd 'princ)))

(defun new-printer ()
  (setq top-level-print 'zprint)
  (putd 'print (getd 'zprint))
  (putd 'prin1 (getd 'zprin1))
  't)

(defun old-printer ()
  (setq top-level-print 'xxprint)
  (putd 'print (getd 'xxprint))
  (putd 'princ (getd 'xxprinc))
  't)




(putprop 'lmhacks t 'version)
