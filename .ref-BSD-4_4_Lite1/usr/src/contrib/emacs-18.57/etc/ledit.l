;;; -*- Mode: lisp -*-

; load in the c functions

(removeaddress '_signal)
(removeaddress '_switch_to_proc)
(removeaddress '_set_proc_str)

(cfasl "/src/mdc/ledit/leditcfns.o" '_switch_to_proc 'emacs)

(getaddress '_set_proc_str 'set_proc_str)

(declare (special *ledit-infile*               ; emacs->lisp tempfile
		  *ledit-outfile*              ; lisp->emacs tempfile
		  *ledit-ppfile*               ; pp->emacs tempfile
                  *ledit-lisztfile*            ; compiler input
                  *ledit-objfile*              ; compiler output
		  *ledit-initialized*)         ; flag
	 )

(setq *ledit-initialized* nil)

;;; INIT-LEDIT

(defun init-ledit ()
  (let ((user (getenv '|USER|)))		;USER must be uppercase
       (setq
	 *ledit-outfile* (concat "/tmp/" user ".l2") ; lisp -> emacs
	 *ledit-infile*  (concat "/tmp/" user ".l1") ; emacs -> lisp
	 *ledit-ppfile*  (concat "/tmp/" user ".l3") ; pp output to emacs.
	 *ledit-lisztfile*  (concat "/tmp/" user ".l4")
	 *ledit-objfile*  (concat "/tmp/" user ".o")
	 *ledit-initialized* t)))

;;; LEDIT
; if 1 arg, arg is taken as a tag name to pass to emacs.
; if 2 args, second arg is a keyword.  If 2nd arg is pp,
; pp is applied to first arg, and result is sent to emacs
; to put in a buffer called LEDIT (which is first erased.)

(defun ledit fexpr (args)
    (apply #'ledit* args))

;;; LEDIT*

(defun ledit* n
    (if (not *ledit-initialized*) (init-ledit))
    (ledit-output (listify n))
    (syscall 10. *ledit-infile*)        ; syscall 10 is "delete"
    (syscall 10. *ledit-lisztfile*)
    (emacs)
    (ledit-input)
    (syscall 10. *ledit-outfile*)
    (syscall 10. *ledit-ppfile*)
    t)

;;; LEDIT-OUTPUT
;;; Egad, what a mess!  Doesn't work for XEMACS yet.
;;; Here's an example from Moclisp:
;;; -> (defun bar (nothing) (bar nothing))
;;; bar
;;; -> (ledit bar)
;;; should produce...
;;; (progn) (progn tag (setq tag "bar") (&goto-tag))
;;; and
;;; -> (ledit bar pp)
;;; should stuff this to emacs...
;;; (progn) (switch-to-buffer "LEDIT") (erase-buffer)
;;; (insert-file "/tmp/walter.l3") (lisp-mode)
;;; and this...
;;; (def bar
;;;   (lambda (x)
;;;    (bar nothing)))
;;; into *LEDIT*

(defun ledit-output (args)
  (if args
      (let ((ofile (outfile *ledit-outfile*)))
	   (format ofile "(progn)")             ; this is necessary.

	   (cond ((null (cdr args)) ; no keyword -> arg is a tag.
		  (format ofile "(progn tag (setq tag \"~A\"~
			         (&goto-tag))"
			         (car args)))
		 ((eq (cadr args) 'pp)       ; pp-> pp first arg to emacs
		      (apply 'pp `((|F| ,*ledit-ppfile*) ,(car args)))
		      (format ofile "(switch-to-buffer \"LEDIT\")~
				     (erase-buffer)")
		      (format ofile "(insert-file \"~A\")"
			             *ledit-ppfile*)
		      (format ofile "(lisp-mode)"))
	   
		 (t (format t "~&~A -- unknown option~%" (cdr args))))
	   (close ofile))))

;;; LISZT*
;;; Need this guy to do compile-input.
;;; Liszt returns 0 if all was well.
;;; Note that in ordinary use the user will have to get used to looking
;;; at "%Warning: ... Compiler declared *foo* special" messages, since
;;; you don't usually want to hunt around in your file, zap in the the
;;; declarations, then go back to what you were doing.
;;; Fortunately this doesn't cause the compiler to bomb.
;;; Some sleepless night I will think of a way to get around this.

(defun liszt* (&rest args)
   (apply #'liszt args))

;;; LEDIT-INPUT
;;; Although there are two cases here, in practice
;;; it is never the case that there is both input to be
;;; interpreted and input to be compiled.

(defun ledit-input ()
  (if (probef *ledit-lisztfile*)
      (cond ((getd #'liszt)
	     (format t ";Compiling LEDIT:")
	     (and (zerop (liszt* *ledit-lisztfile* '-o *ledit-objfile*))
		  (load *ledit-objfile*)))
	    (t (format t ";Can't compile LEDIT: No liszt.~%;Reading instead:")
	       (let ((ifile (infile *ledit-lisztfile*)))
		 (ledit-load ifile)
		 (close ifile)))))

  (if (probef *ledit-infile*)
      (let ((ifile (infile *ledit-infile*)))
	(format t ";Reading from LEDIT:~%")
	(ledit-load ifile)
	(close ifile))))

;;; LEDIT-LOAD
;;; A generally useful form of load

(defun ledit-load (ifile)
  (let ((eof-form (list 'eof-form)))
    (do ((form (read ifile eof-form) (read ifile eof-form)))
      ((eq form eof-form))
      (format t ";  ~A~%" (eval form)))))

(setsyntax #/ 'macro 'ledit)                  ; make ^E = (ledit)<return>

;; more robust version of the c function set_proc_str. Does argument checking.
;; set_proc_str sets the string that is stuffed to the tty after franz pauses
;; and the csh wakes up. It is usually "%emacs" or "%vemacs" or "%?emacs"
(defun set-proc-str (arg)
  (if (stringp arg)
    (set_proc_str arg)
    (if (symbolp arg)
      (set_proc_str (get-pname arg))
      (error arg " is illegal argument to set-proc-str"))))
