;; Common-Lisp extensions for GNU Emacs Lisp.
;; Copyright (C) 1987, 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;
;;;; These are extensions to Emacs Lisp that provide some form of
;;;; Common Lisp compatibility, beyond what is already built-in
;;;; in Emacs Lisp.
;;;;
;;;; When developing them, I had the code spread among several files.
;;;; This file 'cl.el' is a concatenation of those original files,
;;;; minus some declarations that became redundant.  The marks between
;;;; the original files can be found easily, as they are lines that
;;;; begin with four semicolons (as this does).  The names of the
;;;; original parts follow the four semicolons in uppercase, those
;;;; names are GLOBAL, SYMBOLS, LISTS, SEQUENCES, CONDITIONALS,
;;;; ITERATIONS, MULTIPLE VALUES, ARITH, SETF and DEFSTRUCT.  If you
;;;; add functions to this file, you might want to put them in a place
;;;; that is compatible with the division above (or invent your own
;;;; categories).
;;;;
;;;; To compile this file, make sure you load it first.  This is
;;;; because many things are implemented as macros and now that all
;;;; the files are concatenated together one cannot ensure that
;;;; declaration always precedes use.
;;;;
;;;; Bug reports, suggestions and comments,
;;;; to quiroz@cs.rochester.edu

(provide 'cl)


;;;; GLOBAL
;;;;    This file provides utilities and declarations that are global
;;;;    to Common Lisp and so might be used by more than one of the
;;;;    other libraries.  Especially, I intend to keep here some
;;;;    utilities that help parsing/destructuring some difficult calls. 
;;;;
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)

(defmacro psetq (&rest pairs)
  "(psetq {VARIABLE VALUE}...): In parallel, set each VARIABLE to its VALUE.
All the VALUEs are evaluated, and then all the VARIABLEs are set.
Aside from order of evaluation, this is the same as `setq'."
  (let ((nforms (length pairs))		;count of args
	;; next are used to destructure the call
	symbols				;even numbered args
	forms				;odd numbered args
	;; these are used to generate code
	bindings			;for the let
	newsyms				;list of gensyms
	assignments			;for the setq
	;; auxiliary indices
	i)
    ;; check there is a reasonable number of forms
    (if (/= (% nforms 2) 0)
	(error "Odd number of arguments to `psetq'"))

    ;; destructure the args
    (let ((ptr pairs)			;traverses the args
	  var				;visits each symbol position
	  )
      (while ptr
	(setq var (car ptr))		;next variable
	(if (not (symbolp var))
	    (error "`psetq' expected a symbol, found '%s'."
		   (prin1-to-string var)))
	(setq symbols (cons var symbols))
	(setq forms   (cons (car (cdr ptr)) forms))
	(setq ptr (cdr (cdr ptr)))))

    ;; assign new symbols to the bindings
    (let ((ptr forms)			;traverses the forms
	  form				;each form goes here
	  newsym			;gensym for current value of form
	  )
      (while ptr
	(setq form (car ptr))
	(setq newsym (gensym))
	(setq bindings (cons (list newsym form) bindings))
	(setq newsyms (cons newsym newsyms))
	(setq ptr (cdr ptr))))
    (setq newsyms (nreverse newsyms))	;to sync with symbols
    
    ;; pair symbols with newsyms for assignment
    (let ((ptr1 symbols)		;traverses original names
	  (ptr2 newsyms)		;traverses new symbols
	  )
      (while ptr1
	(setq assignments
	      (cons (car ptr1) (cons (car ptr2) assignments)))
	(setq ptr1 (cdr ptr1))
	(setq ptr2 (cdr ptr2))))
    
    ;; generate code
    (list 'let
	  bindings
	  (cons 'setq assignments)
	  nil)))

;;; utilities
;;;
;;; pair-with-newsyms takes a list and returns a list of lists of the
;;; form (newsym form), such that a let* can then bind the evaluation
;;; of the forms to the newsyms.  The idea is to guarantee correct
;;; order of evaluation of the subforms of a setf.  It also returns a
;;; list of the newsyms generated, in the corresponding order.

(defun pair-with-newsyms (oldforms)
  "PAIR-WITH-NEWSYMS OLDFORMS
The top-level components of the list oldforms are paired with fresh
symbols, the pairings list and the newsyms list are returned."
  (do ((ptr oldforms (cdr ptr))
       (bindings '())
       (newsyms  '()))
      ((endp ptr) (values (nreverse bindings) (nreverse newsyms)))
    (let ((newsym (gentemp)))
      (setq bindings (cons (list newsym (car ptr)) bindings))
      (setq newsyms  (cons newsym newsyms)))))

(defun zip-lists (evens odds)
  "Merge two lists EVENS and ODDS, taking elts from each list alternatingly.
EVENS and ODDS are two lists.  ZIP-LISTS constructs a new list, whose
even numbered elements (0,2,...) come from EVENS and whose odd
numbered elements (1,3,...) come from ODDS. 
The construction stops when the shorter list is exhausted."
  (do* ((p0   evens    (cdr p0))
        (p1   odds     (cdr p1))
        (even (car p0) (car p0))
        (odd  (car p1) (car p1))
        (result '()))
      ((or (endp p0) (endp p1))
       (nreverse result))
    (setq result
          (cons odd (cons even result)))))

(defun unzip-list (list)
  "Extract even and odd elements of LIST into two separate lists.
The argument LIST is separated in two strands, the even and the odd
numbered elements.  Numbering starts with 0, so the first element
belongs in EVENS. No check is made that there is an even number of
elements to start with."
  (do* ((ptr   list       (cddr ptr))
        (this  (car ptr)  (car ptr))
        (next  (cadr ptr) (cadr ptr))
        (evens '())
        (odds  '()))
      ((endp ptr)
       (values (nreverse evens) (nreverse odds)))
    (setq evens (cons this evens))
    (setq odds  (cons next odds))))

(defun reassemble-argslists (argslists)
  "(reassemble-argslists ARGSLISTS).
ARGSLISTS is a list of sequences.  Return a list of lists, the first
sublist being all the entries coming from ELT 0 of the original
sublists, the next those coming from ELT 1 and so on, until the
shortest list is exhausted."
  (let* ((minlen   (apply 'min (mapcar 'length argslists)))
         (result   '()))
    (dotimes (i minlen (nreverse result))
      ;; capture all the elements at index i
      (setq result
            (cons (mapcar
                   (function (lambda (sublist) (elt sublist i)))
                   argslists)
                  result)))))

;;; to help parsing keyword arguments

(defun build-klist (argslist acceptable)
  "Decode a keyword argument list ARGSLIST for keywords in ACCEPTABLE.
ARGSLIST is a list, presumably the &rest argument of a call, whose
even numbered elements must be keywords.
ACCEPTABLE is a list of keywords, the only ones that are truly acceptable.
The result is an alist containing the arguments named by the keywords
in ACCEPTABLE, or nil if something failed."

  ;; check legality of the arguments, then destructure them
  (unless (and (listp argslist)
               (evenp (length argslist)))
    (error "Odd number of keyword-args"))
  (unless (and (listp acceptable)
               (every 'keywordp acceptable))
    (error "Second arg should be a list of keywords"))
  (multiple-value-bind
      (keywords forms)
      (unzip-list argslist)
    (unless (every 'keywordp keywords)
      (error "Expected keywords, found `%s'"
             (prin1-to-string keywords)))
    (do*                                ;pick up the pieces
        ((auxlist                       ;auxiliary a-list, may
          (pairlis keywords forms))     ;contain repetitions and junk
         (ptr    acceptable  (cdr ptr)) ;pointer in acceptable
         (this  (car ptr)  (car ptr))   ;current acceptable keyword
         (auxval nil)                   ;used to move values around
         (alist  '()))                  ;used to build the result
        ((endp ptr) alist)
      ;; if THIS appears in auxlist, use its value
      (when (setq auxval (assoc this auxlist))
        (setq alist (cons auxval alist))))))


;;; Checking that a list of symbols contains no duplicates is a common
;;; task when checking the legality of some macros.  The check for 'eq
;;; pairs can be too expensive, as it is quadratic on the length of
;;; the list.  I use a 4-pass, linear, counting approach.  It surely
;;; loses on small lists (less than 5 elements?), but should win for
;;; larger lists.  The fourth pass could be eliminated.
;;; 10 dec 1986.  Emacs Lisp has no REMPROP, so I just eliminated the
;;; 4th pass.
(defun duplicate-symbols-p (list)
  "Find all symbols appearing more than once in LIST.
Return a list of all such duplicates; `nil' if there are no duplicates."
  (let  ((duplicates '())               ;result built here
         (propname   (gensym))          ;we use a fresh property
         )
    ;; check validity
    (unless (and (listp list)
                 (every 'symbolp list))
      (error "A list of symbols is needed"))
    ;; pass 1: mark
    (dolist (x list)
      (put x propname 0))
    ;; pass 2: count
    (dolist (x list)
      (put x propname (1+ (get x propname))))
    ;; pass 3: collect
    (dolist (x list)
      (if (> (get x propname) 1)
          (setq duplicates (cons x duplicates))))
    ;; pass 4: unmark.  eliminated.
    ;; (dolist (x list) (remprop x propname))
    ;; return result
    duplicates))

;;;; end of cl-global.el

;;;; SYMBOLS
;;;;    This file provides the gentemp function, which generates fresh
;;;;    symbols, plus some other minor Common Lisp symbol tools.
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)

;;; Keywords.  There are no packages in Emacs Lisp, so this is only a
;;; kludge around to let things be "as if" a keyword package was around.

(defmacro defkeyword (x &optional docstring)
  "Make symbol X a keyword (symbol whose value is itself).
Optional second argument is a documentation string for it."
  (cond
   ((symbolp x)
    (list 'defconst x (list 'quote x)))
   (t
    (error "`%s' is not a symbol" (prin1-to-string x)))))

(defun keywordp (sym)
  "Return `t' if SYM is a keyword."
  (cond
   ((and (symbolp sym)
         (char-equal (aref (symbol-name sym) 0) ?\:))
    ;; looks like one, make sure value is right
    (set sym sym))
   (t
    nil)))

(defun keyword-of (sym)
  "Return a keyword that is naturally associated with symbol SYM.
If SYM is keyword, the value is SYM.
Otherwise it is a keyword whose name is `:' followed by SYM's name."
  (cond
   ((keywordp sym)
    sym)
   ((symbolp sym)
    (let ((newsym (intern (concat ":" (symbol-name sym)))))
      (set newsym newsym)))
   (t
    (error "Expected a symbol, not `%s'" (prin1-to-string sym)))))

;;; Temporary symbols.  
;;; 

(defvar *gentemp-index* 0
  "Integer used by gentemp to produce new names.")

(defvar *gentemp-prefix* "T$$_"
  "Names generated by gentemp begin with this string by default.")

(defun gentemp (&optional prefix oblist)
  "Generate a fresh interned symbol.
There are 2 optional arguments, PREFIX and OBLIST.  PREFIX is the
string that begins the new name, OBLIST is the obarray used to search for
old names.  The defaults are just right, YOU SHOULD NEVER NEED THESE
ARGUMENTS IN YOUR OWN CODE."
  (if (null prefix)
      (setq prefix *gentemp-prefix*))
  (if (null oblist)
      (setq oblist obarray))            ;default for the intern functions
  (let ((newsymbol nil)
        (newname))
    (while (not newsymbol)
      (setq newname (concat prefix *gentemp-index*))
      (setq *gentemp-index* (+ *gentemp-index* 1))
      (if (not (intern-soft newname oblist))
          (setq newsymbol (intern newname oblist))))
    newsymbol))

(defvar *gensym-index* 0
  "Integer used by gensym to produce new names.")

(defvar *gensym-prefix* "G$$_"
  "Names generated by gensym begin with this string by default.")

(defun gensym (&optional prefix)
  "Generate a fresh uninterned symbol.
There is an  optional argument, PREFIX.  PREFIX is the
string that begins the new name. Most people take just the default,
except when debugging needs suggest otherwise."
  (if (null prefix)
      (setq prefix *gensym-prefix*))
  (let ((newsymbol nil)
        (newname   ""))
    (while (not newsymbol)
      (setq newname (concat prefix *gensym-index*))
      (setq *gensym-index* (+ *gensym-index* 1))
      (if (not (intern-soft newname))
          (setq newsymbol (make-symbol newname))))
    newsymbol))

;;;; end of cl-symbols.el

;;;; CONDITIONALS
;;;;    This file provides some of the conditional constructs of
;;;;    Common Lisp.  Total compatibility is again impossible, as the
;;;;    'if' form is different in both languages, so only a good
;;;;    approximation is desired.
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)

;;; indentation info
(put 'case      'lisp-indent-hook 1)
(put 'ecase     'lisp-indent-hook 1)
(put 'when      'lisp-indent-hook 1)
(put 'unless    'lisp-indent-hook 1)

;;; WHEN and UNLESS
;;; These two forms are simplified ifs, with a single branch.

(defmacro when (condition &rest body)
  "(when CONDITION . BODY) => evaluate BODY if CONDITION is true."
  (list* 'if (list 'not condition) '() body))

(defmacro unless (condition &rest body)
  "(unless CONDITION . BODY) => evaluate BODY if CONDITION is false."
  (list* 'if condition '() body))

;;; CASE and ECASE
;;; CASE selects among several clauses, based on the value (evaluated)
;;; of a expression and a list of (unevaluated) key values.  ECASE is
;;; the same, but signals an error if no clause is activated.

(defmacro case (expr &rest cases)
  "(case EXPR . CASES) => evals EXPR, chooses from CASES on that value.
EXPR   -> any form
CASES  -> list of clauses, non empty
CLAUSE -> HEAD . BODY
HEAD   -> t             = catch all, must be last clause
       -> otherwise     = same as t
       -> nil           = illegal
       -> atom          = activated if (eql  EXPR HEAD)
       -> list of atoms = activated if (member EXPR HEAD)
BODY   -> list of forms, implicit PROGN is built around it.
EXPR is evaluated only once."
  (let* ((newsym (gentemp))
         (clauses (case-clausify cases newsym)))
    ;; convert case into a cond inside a let
    (list 'let
         (list (list newsym expr))
         (list* 'cond (nreverse clauses)))))

(defmacro ecase (expr &rest cases)
  "(ecase EXPR . CASES) => like `case', but error if no case fits.
`t'-clauses are not allowed."
  (let* ((newsym (gentemp))
         (clauses (case-clausify cases newsym)))
    ;; check that no 't clause is present.
    ;; case-clausify would put one such at the beginning of clauses
    (if (eq (caar clauses) t)
        (error "No clause-head should be `t' or `otherwise' for `ecase'"))
    ;; insert error-catching clause
    (setq clauses
          (cons
           (list 't (list 'error
                          "ecase on %s = %s failed to take any branch."
                          (list 'quote expr)
                          (list 'prin1-to-string newsym)))
           clauses))
    ;; generate code as usual
    (list 'let
          (list (list newsym expr))
          (list* 'cond (nreverse clauses)))))


(defun case-clausify (cases newsym)
  "CASE-CLAUSIFY CASES NEWSYM => clauses for a 'cond'
Converts the CASES of a [e]case macro into cond clauses to be
evaluated inside a let that binds NEWSYM.  Returns the clauses in
reverse order."
  (do* ((currentpos cases        (cdr currentpos))
        (nextpos    (cdr cases)  (cdr nextpos))
        (curclause  (car cases)  (car currentpos))
        (result     '()))
      ((endp currentpos) result)
    (let ((head (car curclause))
          (body (cdr curclause)))
      ;; construct a cond-clause according to the head
      (cond
       ((null head)
        (error "Case clauses cannot have null heads: `%s'"
               (prin1-to-string curclause)))
       ((or (eq head 't)
            (eq head 'otherwise))
        ;; check it is the last clause
        (if (not (endp nextpos))
            (error "Clause with `t' or `otherwise' head must be last"))
        ;; accept this clause as a 't' for cond
        (setq result (cons (cons 't body) result)))
       ((atom head)
        (setq result
              (cons (cons (list 'eql newsym (list 'quote head)) body)
                    result)))
       ((listp head)
        (setq result
              (cons (cons (list 'member newsym (list 'quote head)) body)
                    result)))
       (t
        ;; catch-all for this parser
        (error "Don't know how to parse case clause `%s'."
               (prin1-to-string head)))))))

;;;; end of cl-conditionals.el

;;;; ITERATIONS
;;;;    This file provides simple iterative macros (a la Common Lisp)
;;;;    constructed on the basis of let, let* and while, which are the
;;;;    primitive binding/iteration constructs of Emacs Lisp
;;;;
;;;;    The Common Lisp iterations use to have a block named nil
;;;;    wrapped around them, and allow declarations at the beginning
;;;;    of their bodies and you can return a value using (return ...).
;;;;    Nothing of the sort exists in Emacs Lisp, so I haven't tried
;;;;    to imitate these behaviors.
;;;;
;;;;    Other than the above, the semantics of Common Lisp are
;;;;    correctly reproduced to the extent this was reasonable.
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)

;;; some lisp-indentation information
(put 'do                'lisp-indent-hook 2)
(put 'do*               'lisp-indent-hook 2)
(put 'dolist            'lisp-indent-hook 1)
(put 'dotimes           'lisp-indent-hook 1)
(put 'do-symbols        'lisp-indent-hook 1)
(put 'do-all-symbols    'lisp-indent-hook 1)


(defmacro do (stepforms endforms &rest body)
  "(do STEPFORMS ENDFORMS . BODY): Iterate BODY, stepping some local variables.
STEPFORMS must be a list of symbols or lists.  In the second case, the
lists must start with a symbol and contain up to two more forms. In
the STEPFORMS, a symbol is the same as a (symbol).  The other 2 forms
are the initial value (def. NIL) and the form to step (def. itself).
The values used by initialization and stepping are computed in parallel.
The ENDFORMS are a list (CONDITION . ENDBODY).  If the CONDITION
evaluates to true in any iteration, ENDBODY is evaluated and the last
form in it is returned.
The BODY (which may be empty) is evaluated at every iteration, with
the symbols of the STEPFORMS bound to the initial or stepped values."
  ;; check the syntax of the macro
  (and (check-do-stepforms stepforms)
       (check-do-endforms endforms))
  ;; construct emacs-lisp equivalent
  (let ((initlist (extract-do-inits stepforms))
        (steplist (extract-do-steps stepforms))
        (endcond  (car endforms))
        (endbody  (cdr endforms)))
    (cons 'let (cons initlist
                     (cons (cons 'while (cons (list 'not endcond) 
                                              (append body steplist)))
                           (append endbody))))))


(defmacro do* (stepforms endforms &rest body)
  "`do*' is to `do' as `let*' is to `let'.
STEPFORMS must be a list of symbols or lists.  In the second case, the
lists must start with a symbol and contain up to two more forms. In
the STEPFORMS, a symbol is the same as a (symbol).  The other 2 forms
are the initial value (def. NIL) and the form to step (def. itself).
Initializations and steppings are done in the sequence they are written.
The ENDFORMS are a list (CONDITION . ENDBODY).  If the CONDITION
evaluates to true in any iteration, ENDBODY is evaluated and the last
form in it is returned.
The BODY (which may be empty) is evaluated at every iteration, with
the symbols of the STEPFORMS bound to the initial or stepped values."
  ;; check the syntax of the macro
  (and (check-do-stepforms stepforms)
       (check-do-endforms endforms))
  ;; construct emacs-lisp equivalent
  (let ((initlist (extract-do-inits stepforms))
        (steplist (extract-do*-steps stepforms))
        (endcond  (car endforms))
        (endbody  (cdr endforms)))
    (cons 'let* (cons initlist
                     (cons (cons 'while (cons (list 'not endcond) 
                                              (append body steplist)))
                           (append endbody))))))


;;; DO and DO* share the syntax checking functions that follow.

(defun check-do-stepforms (forms)
  "True if FORMS is a valid stepforms for the do[*] macro (q.v.)"
  (cond
   ((nlistp forms)
    (error "Init/Step form for do[*] should be a list, not `%s'"
           (prin1-to-string forms)))
   (t                                   ;valid list
    ;; each entry must be a symbol, or a list whose car is a symbol
    ;; and whose length is no more than three
    (mapcar
     (function
      (lambda (entry)
        (cond
         ((or (symbolp entry)
              (and (listp entry)
                   (symbolp (car entry))
                   (< (length entry) 4)))
          t)
         (t
          (error
           "Init/Step must be symbol or (symbol [init [step]]), not `%s'"
           (prin1-to-string entry))))))
     forms))))

(defun check-do-endforms (forms)
  "True if FORMS is a valid endforms for the do[*] macro (q.v.)"
  (cond
   ((listp forms)
    t)
   (t
    (error "Termination form for do macro should be a list, not `%s'"
           (prin1-to-string forms)))))

(defun extract-do-inits (forms)
  "Returns a list of the initializations (for do) in FORMS
-a stepforms, see the do macro-. Forms is assumed syntactically valid."
  (mapcar
   (function
    (lambda (entry)
      (cond
       ((symbolp entry)
        (list entry nil))
       ((listp entry)
        (list (car entry) (cadr entry))))))
   forms))

;;; There used to be a reason to deal with DO differently than with
;;; DO*.  The writing of PSETQ has made it largely unnecessary.

(defun extract-do-steps (forms)
  "EXTRACT-DO-STEPS FORMS => an s-expr
FORMS is the stepforms part of a DO macro (q.v.).  This function
constructs an s-expression that does the stepping at the end of an
iteration."
  (list (cons 'psetq (select-stepping-forms forms))))

(defun extract-do*-steps (forms)
  "EXTRACT-DO*-STEPS FORMS => an s-expr
FORMS is the stepforms part of a DO* macro (q.v.).  This function
constructs an s-expression that does the stepping at the end of an
iteration."
  (list (cons 'setq (select-stepping-forms forms))))

(defun select-stepping-forms (forms)
  "Separate only the forms that cause stepping."
  (let ((result '())			;ends up being (... var form ...)
	(ptr forms)			;to traverse the forms
	entry				;to explore each form in turn
	)
    (while ptr				;(not (endp entry)) might be safer
      (setq entry (car ptr))
      (cond
       ((and (listp entry)
	     (= (length entry) 3))
	(setq result (append		;append in reverse order!
		      (list (caddr entry) (car entry))
		      result))))
      (setq ptr (cdr ptr)))		;step in the list of forms
    ;;put things back in the
    ;;correct order before return
    (nreverse result)))

;;; Other iterative constructs

(defmacro dolist  (stepform &rest body)
  "(dolist (VAR LIST [RESULTFORM]) . BODY): do BODY for each elt of LIST.
The RESULTFORM defaults to nil.  The VAR is bound to successive
elements of the value of LIST and remains bound (to the nil value) when the
RESULTFORM is evaluated."
  ;; check sanity
  (cond
   ((nlistp stepform)
    (error "Stepform for `dolist' should be (VAR LIST [RESULT]), not `%s'"
           (prin1-to-string stepform)))
   ((not (symbolp (car stepform)))
    (error "First component of stepform should be a symbol, not `%s'"
           (prin1-to-string (car stepform))))
   ((> (length stepform) 3)
    (error "Too many components in stepform `%s'"
           (prin1-to-string stepform))))
  ;; generate code
  (let* ((var (car stepform))
         (listform (cadr stepform))
         (resultform (caddr stepform)))
    (list 'progn
          (list 'mapcar
                (list 'function
                      (cons 'lambda (cons (list var) body)))
                listform)
          (list 'let
                (list (list var nil))
                resultform))))

(defmacro dotimes (stepform &rest body)
  "(dotimes (VAR COUNTFORM [RESULTFORM]) .  BODY): Repeat BODY, counting in VAR.
The COUNTFORM should return a positive integer.  The VAR is bound to
successive integers from 0 to COUNTFORM-1 and the BODY is repeated for
each of them.  At the end, the RESULTFORM is evaluated and its value
returned. During this last evaluation, the VAR is still bound, and its
value is the number of times the iteration occurred. An omitted RESULTFORM
defaults to nil."
  ;; check sanity 
  (cond
   ((nlistp stepform)
    (error "Stepform for `dotimes' should be (VAR COUNT [RESULT]), not `%s'"
           (prin1-to-string stepform)))
   ((not (symbolp (car stepform)))
    (error "First component of stepform should be a symbol, not `%s'"
           (prin1-to-string (car stepform))))
   ((> (length stepform) 3)
    (error "Too many components in stepform `%s'"
           (prin1-to-string stepform))))
  ;; generate code
  (let* ((var (car stepform))
         (countform (cadr stepform))
         (resultform (caddr stepform))
         (newsym (gentemp)))
    (list
     'let* (list (list newsym countform))
     (list*
      'do*
      (list (list var 0 (list '+ var 1)))
      (list (list '>= var newsym) resultform)
      body))))

(defmacro do-symbols (stepform &rest body)
  "(do_symbols (VAR [OBARRAY [RESULTFORM]]) . BODY)
The VAR is bound to each of the symbols in OBARRAY (def. obarray) and
the BODY is repeatedly performed for each of those bindings. At the
end, RESULTFORM (def. nil) is evaluated and its value returned.
During this last evaluation, the VAR is still bound and its value is nil.
See also the function `mapatoms'."
  ;; check sanity
  (cond
   ((nlistp stepform)
    (error "Stepform for `do-symbols' should be (VAR OBARRAY [RESULT]), not `%s'"
           (prin1-to-string stepform)))
   ((not (symbolp (car stepform)))
    (error "First component of stepform should be a symbol, not `%s'"
           (prin1-to-string (car stepform))))
   ((> (length stepform) 3)
    (error "Too many components in stepform `%s'"
           (prin1-to-string stepform))))
  ;; generate code
  (let* ((var (car stepform))
         (oblist (cadr stepform))
         (resultform (caddr stepform)))
    (list 'progn
          (list 'mapatoms
                (list 'function
                      (cons 'lambda (cons (list var) body)))
                oblist)
          (list 'let
                (list (list var nil))
                resultform))))


(defmacro do-all-symbols (stepform &rest body)
  "(do-all-symbols (VAR [RESULTFORM]) . BODY)
Is the same as (do-symbols (VAR obarray RESULTFORM) . BODY)."
  (list*
   'do-symbols
   (list (car stepform) 'obarray (cadr stepform))
   body))

(defmacro loop (&rest body)
  "(loop . BODY) repeats BODY indefinitely and does not return.
Normally BODY uses `throw' or `signal' to cause an exit.
The forms in BODY should be lists, as non-lists are reserved for new features."
  ;; check that the body doesn't have atomic forms
  (if (nlistp body)
      (error "Body of `loop' should be a list of lists or nil")
    ;; ok, it is a list, check for atomic components
    (mapcar
     (function (lambda (component)
                 (if (nlistp component)
                     (error "Components of `loop' should be lists"))))
     body)
    ;; build the infinite loop
    (cons 'while (cons 't body))))

;;;; end of cl-iterations.el

;;;; LISTS
;;;;    This file provides some of the lists machinery of Common-Lisp
;;;;    in a way compatible with Emacs Lisp.  Especially, see the the
;;;;    typical c[ad]*r functions.
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)



;;; Synonyms for list functions
(defun first (x)
  "Synonym for `car'"
  (car x))

(defun second (x)
  "Return the second element of the list LIST."
  (nth 1 x))

(defun third (x)
  "Return the third element of the list LIST."
  (nth 2 x))

(defun fourth (x)
  "Return the fourth element of the list LIST."
  (nth 3 x))

(defun fifth (x)
  "Return the fifth element of the list LIST."
  (nth 4 x))

(defun sixth (x)
  "Return the sixth element of the list LIST."
  (nth 5 x))

(defun seventh (x)
  "Return the seventh element of the list LIST."
  (nth 6 x))

(defun eighth (x)
  "Return the eighth element of the list LIST."
  (nth 7 x))

(defun ninth (x)
  "Return the ninth element of the list LIST."
  (nth 8 x))

(defun tenth (x)
  "Return the tenth element of the list LIST."
  (nth 9 x))

(defun rest (x)
  "Synonym for `cdr'"
  (cdr x))

(defun endp (x)
  "t if X is nil, nil if X is a cons; error otherwise."
  (if (listp x)
      (null x)
    (error "endp received a non-cons, non-null argument `%s'"
	   (prin1-to-string x))))

(defun last (x)
  "Returns the last link in the list LIST."
  (if (nlistp x)
      (error "Arg to `last' must be a list"))
  (do ((current-cons    x       (cdr current-cons))
       (next-cons    (cdr x)    (cdr next-cons)))
      ((endp next-cons) current-cons)))

(defun list-length (x)                  ;taken from CLtL sect. 15.2
  "Returns the length of a non-circular list, or `nil' for a circular one."
  (do ((n 0)                            ;counter
       (fast x (cddr fast))             ;fast pointer, leaps by 2
       (slow x (cdr slow))              ;slow pointer, leaps by 1
       (ready nil))                     ;indicates termination
      (ready n)
    (cond
     ((endp fast)
      (setq ready t))                   ;return n
     ((endp (cdr fast))
      (setq n (+ n 1))
      (setq ready t))                   ;return n+1
     ((and (eq fast slow) (> n 0))
      (setq n nil)
      (setq ready t))                   ;return nil
     (t
      (setq n (+ n 2))))))              ;just advance counter

(defun member (item list)
  "Look for ITEM in LIST; return first link in LIST whose car is `eql' to ITEM."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((eql item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun butlast (list &optional n)
  "Return a new list like LIST but sans the last N elements.
N defaults to 1.  If the list doesn't have N elements, nil is returned."
  (if (null n) (setq n 1))
  (reverse (nthcdr n (reverse list))))

(defun list* (arg &rest others)
  "Return a new list containing the first arguments consed onto the last arg.
Thus, (list* 1 2 3 '(a b)) returns (1 2 3 a b)."
  (if (null others)
      arg
    (let* ((allargs (cons arg others))
           (front   (butlast allargs))
           (back    (last allargs)))
      (rplacd (last front) (car back))
      front)))

(defun adjoin (item list)
  "Return a list which contains ITEM but is otherwise like LIST.
If ITEM occurs in LIST, the value is LIST.  Otherwise it is (cons ITEM LIST).
When comparing ITEM against elements, `eql' is used."
  (cond
   ((member item list)
    list)
   (t
    (cons item list))))

(defun ldiff (list sublist)
  "Return a new list like LIST but sans SUBLIST.
SUBLIST must be one of the links in LIST; otherwise the value is LIST itself."
  (do ((result '())
       (curcons list (cdr curcons)))
      ((or (endp curcons) (eq curcons sublist))
       (reverse result))
    (setq result (cons (car curcons) result))))

;;; The popular c[ad]*r functions.

(defun caar (X)
  "Return the car of the car of X."
  (car (car X)))

(defun cadr (X)
  "Return the car of the cdr of X."
  (car (cdr X)))

(defun cdar (X)
  "Return the cdr of the car of X."
  (cdr (car X)))

(defun cddr (X)
  "Return the cdr of the cdr of X."
  (cdr (cdr X)))

(defun caaar (X)
  "Return the car of the car of the car of X."
  (car (car (car X))))

(defun caadr (X)
  "Return the car of the car of the cdr of X."
  (car (car (cdr X))))

(defun cadar (X)
  "Return the car of the cdr of the car of X."
  (car (cdr (car X))))

(defun cdaar (X)
  "Return the cdr of the car of the car of X."
  (cdr (car (car X))))

(defun caddr (X)
  "Return the car of the cdr of the cdr of X."
  (car (cdr (cdr X))))

(defun cdadr (X)
  "Return the cdr of the car of the cdr of X."
  (cdr (car (cdr X))))

(defun cddar (X)
  "Return the cdr of the cdr of the car of X."
  (cdr (cdr (car X))))

(defun cdddr (X)
  "Return the cdr of the cdr of the cdr of X."
  (cdr (cdr (cdr X))))

(defun caaaar (X)
  "Return the car of the car of the car of the car of X."
  (car (car (car (car X)))))

(defun caaadr (X)
  "Return the car of the car of the car of the cdr of X."
  (car (car (car (cdr X)))))

(defun caadar (X)
  "Return the car of the car of the cdr of the car of X."
  (car (car (cdr (car X)))))

(defun cadaar (X)
  "Return the car of the cdr of the car of the car of X."
  (car (cdr (car (car X)))))

(defun cdaaar (X)
  "Return the cdr of the car of the car of the car of X."
  (cdr (car (car (car X)))))

(defun caaddr (X)
  "Return the car of the car of the cdr of the cdr of X."
  (car (car (cdr (cdr X)))))

(defun cadadr (X)
  "Return the car of the cdr of the car of the cdr of X."
  (car (cdr (car (cdr X)))))

(defun cdaadr (X)
  "Return the cdr of the car of the car of the cdr of X."
  (cdr (car (car (cdr X)))))

(defun caddar (X)
  "Return the car of the cdr of the cdr of the car of X."
  (car (cdr (cdr (car X)))))

(defun cdadar (X)
  "Return the cdr of the car of the cdr of the car of X."
  (cdr (car (cdr (car X)))))

(defun cddaar (X)
  "Return the cdr of the cdr of the car of the car of X."
  (cdr (cdr (car (car X)))))

(defun cadddr (X)
  "Return the car of the cdr of the cdr of the cdr of X."
  (car (cdr (cdr (cdr X)))))

(defun cddadr (X)
  "Return the cdr of the cdr of the car of the cdr of X."
  (cdr (cdr (car (cdr X)))))

(defun cdaddr (X)
  "Return the cdr of the car of the cdr of the cdr of X."
  (cdr (car (cdr (cdr X)))))

(defun cdddar (X)
  "Return the cdr of the cdr of the cdr of the car of X."
  (cdr (cdr (cdr (car X)))))

(defun cddddr (X)
  "Return the cdr of the cdr of the cdr of the cdr of X."
  (cdr (cdr (cdr (cdr X)))))

;;; some inverses of the accessors are needed for setf purposes

(defun setnth (n list newval)
  "Set (nth N LIST) to NEWVAL.  Returns NEWVAL."
  (rplaca (nthcdr n list) newval))

(defun setnthcdr (n list newval)
  "SETNTHCDR N LIST NEWVAL => NEWVAL
As a side effect, sets the Nth cdr of LIST to NEWVAL."
  (cond
   ((< n 0)
    (error "N must be 0 or greater, not %d" n))
   ((= n 0)
    (rplaca list (car newval))
    (rplacd list (cdr newval))
    newval)
   (t
    (rplacd (nthcdr (- n 1) list) newval))))

;;; A-lists machinery

(defun acons (key item alist)
  "Return a new alist with KEY paired with ITEM; otherwise like ALIST.
Does not copy ALIST."
  (cons (cons key item) alist))

(defun pairlis (keys data &optional alist)
  "Return a new alist with each elt of KEYS paired with an elt of DATA;
optional 3rd arg ALIST is nconc'd at the end.  KEYS and DATA must
have the same length."
  (unless (= (length keys) (length data))
    (error "Keys and data should be the same length"))
  (do* ;;collect keys and data in front of alist
      ((kptr keys (cdr kptr))           ;traverses the keys
       (dptr data (cdr dptr))           ;traverses the data
       (key (car kptr) (car kptr))      ;current key
       (item (car dptr) (car dptr))     ;current data item
       (result alist))
      ((endp kptr) result)
    (setq result (acons key item result))))

;;;; end of cl-lists.el

;;;; SEQUENCES
;;;; Emacs Lisp provides many of the 'sequences' functionality of
;;;; Common Lisp.  This file provides a few things that were left out.
;;;; 


(defkeyword :test      "Used to designate positive (selection) tests.")
(defkeyword :test-not  "Used to designate negative (rejection) tests.")
(defkeyword :key       "Used to designate component extractions.")
(defkeyword :predicate "Used to define matching of sequence components.")
(defkeyword :start     "Inclusive low index in sequence")
(defkeyword :end       "Exclusive high index in sequence")
(defkeyword :start1    "Inclusive low index in first of two sequences.")
(defkeyword :start2    "Inclusive low index in second of two sequences.")
(defkeyword :end1      "Exclusive high index in first of two sequences.")
(defkeyword :end2      "Exclusive high index in second of two sequences.")
(defkeyword :count     "Number of elements to affect.")
(defkeyword :from-end  "T when counting backwards.")

(defun some     (pred seq &rest moreseqs)
  "Test PREDICATE on each element of SEQUENCE; is it ever non-nil?
Extra args are additional sequences; PREDICATE gets one arg from each
sequence and we advance down all the sequences together in lock-step.
A sequence means either a list or a vector."
  (let ((args  (reassemble-argslists (list* seq moreseqs))))
    (do* ((ready nil)                   ;flag: return when t
          (result nil)                  ;resulting value
          (applyval nil)                ;result of applying pred once
          (remaining args
                     (cdr remaining))   ;remaining argument sets
          (current (car remaining)      ;current argument set
                   (car remaining)))
        ((or ready (endp remaining)) result)
      (setq applyval (apply pred current))
      (when applyval
        (setq ready t)
        (setq result applyval)))))

(defun every    (pred seq &rest moreseqs)
  "Test PREDICATE on each element of SEQUENCE; is it always non-nil?
Extra args are additional sequences; PREDICATE gets one arg from each
sequence and we advance down all the sequences together in lock-step.
A sequence means either a list or a vector."
  (let ((args  (reassemble-argslists (list* seq moreseqs))))
    (do* ((ready nil)                   ;flag: return when t
          (result t)                    ;resulting value
          (applyval nil)                ;result of applying pred once
          (remaining args
                     (cdr remaining))   ;remaining argument sets
          (current (car remaining)      ;current argument set
                   (car remaining)))
        ((or ready (endp remaining)) result)
      (setq applyval (apply pred current))
      (unless applyval
        (setq ready t)
        (setq result nil)))))

(defun notany   (pred seq &rest moreseqs)
  "Test PREDICATE on each element of SEQUENCE; is it always nil?
Extra args are additional sequences; PREDICATE gets one arg from each
sequence and we advance down all the sequences together in lock-step.
A sequence means either a list or a vector."
  (let ((args  (reassemble-argslists (list* seq moreseqs))))
    (do* ((ready nil)                   ;flag: return when t
          (result t)                    ;resulting value
          (applyval nil)                ;result of applying pred once
          (remaining args
                     (cdr remaining))   ;remaining argument sets
          (current (car remaining)      ;current argument set
                   (car remaining)))
        ((or ready (endp remaining)) result)
      (setq applyval (apply pred current))
      (when applyval
        (setq ready t)
        (setq result nil)))))

(defun notevery (pred seq &rest moreseqs)
  "Test PREDICATE on each element of SEQUENCE; is it sometimes nil?
Extra args are additional sequences; PREDICATE gets one arg from each
sequence and we advance down all the sequences together in lock-step.
A sequence means either a list or a vector."
  (let ((args  (reassemble-argslists (list* seq moreseqs))))
    (do* ((ready nil)                   ;flag: return when t
          (result nil)                  ;resulting value
          (applyval nil)                ;result of applying pred once
          (remaining args
                     (cdr remaining))   ;remaining argument sets
          (current (car remaining)      ;current argument set
                   (car remaining)))
        ((or ready (endp remaining)) result)
      (setq applyval (apply pred current))
      (unless applyval
        (setq ready t)
        (setq result t)))))



;;; an inverse of elt is needed for setf purposes

(defun setelt (seq n newval)
  "In SEQUENCE, set the Nth element to NEWVAL.  Returns NEWVAL.
A sequence means either a list or a vector."
  (let ((l (length seq)))
    (cond
     ((or (< n 0)
          (>= n l))
      (error "N(%d) should be between 0 and %d" n l))
     (t
      ;; only two cases need be considered
      (cond
       ((listp seq)
        (setnth n seq newval))
       ((arrayp seq)
        (aset seq n newval))
       (t
        (error "SEQ should be a sequence, not `%s'"
               (prin1-to-string seq))))))))

;;; Testing with keyword arguments.
;;;
;;; Many of the sequence functions use keywords to denote some stylized
;;; form of selecting entries in a sequence.  The involved arguments
;;; are collected with a &rest marker (as Emacs Lisp doesn't have a &key
;;; marker), then they are passed to build-klist, who
;;; constructs an association list.  That association list is used to
;;; test for satisfaction and matching.

(defun extract-from-klist (key klist &optional default)
  "EXTRACT-FROM-KLIST KEY KLIST [DEFAULT] => value of KEY or DEFAULT
Extract value associated with KEY in KLIST (return DEFAULT if nil)."
  (let ((retrieved (cdr (assoc key klist))))
    (or retrieved default)))

(defun add-to-klist (key item klist)
  "ADD-TO-KLIST KEY ITEM KLIST => new KLIST
Add association (KEY . ITEM) to KLIST."
  (setq klist (acons key item klist)))

(defun elt-satisfies-test-p (item elt klist)
  "ELT-SATISFIES-TEST-P ITEM ELT KLIST => t or nil
KLIST encodes a keyword-arguments test, as in CH. 14 of CLtL.
True if the given ITEM and ELT satisfy the test."
  (let ((test     (extract-from-klist :test klist))
        (test-not (extract-from-klist :test-not klist))
        (keyfn    (extract-from-klist :key klist 'identity)))
    (cond
     (test
      (funcall test item (funcall keyfn elt)))
     (test-not
      (not (funcall test-not item (funcall keyfn elt))))
     (t                                 ;should never happen
      (error "Neither :test nor :test-not in `%s'"
             (prin1-to-string klist))))))

(defun elt-satisfies-if-p   (item klist)
  "ELT-SATISFIES-IF-P ITEM KLIST => t or nil
True if an -if style function was called and ITEM satisfies the
predicate under :predicate in KLIST."
  (let ((predicate (extract-from-klist :predicate klist))
        (keyfn     (extract-from-klist :key 'identity)))
    (funcall predicate item (funcall keyfn elt))))

(defun elt-satisfies-if-not-p (item klist)
  "ELT-SATISFIES-IF-NOT-P ITEM KLIST => t or nil
KLIST encodes a keyword-arguments test, as in CH. 14 of CLtL.
True if an -if-not style function was called and ITEM does not satisfy
the predicate under :predicate in KLIST."
  (let ((predicate (extract-from-klist :predicate klist))
        (keyfn     (extract-from-klist :key 'identity)))
    (not (funcall predicate item (funcall keyfn elt)))))

(defun elts-match-under-klist-p (e1 e2 klist)
  "ELTS-MATCH-UNDER-KLIST-P E1 E2 KLIST => t or nil
KLIST encodes a keyword-arguments test, as in CH. 14 of CLtL.
True if elements E1 and E2 match under the tests encoded in KLIST."
  (let ((test     (extract-from-klist :test klist))
        (test-not (extract-from-klist :test-not klist))
        (keyfn    (extract-from-klist :key klist 'identity)))
    (cond
     (test
      (funcall test (funcall keyfn e1) (funcall keyfn e2)))
     (test-not
      (not (funcall test-not (funcall keyfn e1) (funcall keyfn e2))))
     (t                                 ;should never happen
      (error "Neither :test nor :test-not in `%s'"
             (prin1-to-string klist))))))

;;;; end of cl-sequences.el

;;;; MULTIPLE VALUES
;;;;    This package approximates the behavior of the multiple-values
;;;;    forms of Common Lisp.  
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)



;;; Lisp indentation information
(put 'multiple-value-bind  'lisp-indent-hook 2)
(put 'multiple-value-setq  'lisp-indent-hook 2)
(put 'multiple-value-list  'lisp-indent-hook nil)
(put 'multiple-value-call  'lisp-indent-hook 1)
(put 'multiple-value-prog1 'lisp-indent-hook 1)


;;; Global state of the package is kept here
(defvar *mvalues-values* nil
  "Most recently returned multiple-values")
(defvar *mvalues-count*  nil
  "Count of multiple-values returned, or nil if the mechanism was not used")

;;; values is the standard multiple-value-return form.  Must be the
;;; last thing evaluated inside a function.  If the caller is not
;;; expecting multiple values, only the first one is passed.  (values)
;;; is the same as no-values returned (unaware callers see nil). The
;;; alternative (values-list <list>) is just a convenient shorthand
;;; and complements multiple-value-list.

(defun values (&rest val-forms)
  "Produce multiple values (zero or more).  Each arg is one value.
See also `multiple-value-bind', which is one way to examine the
multiple values produced by a form.  If the containing form or caller
does not check specially to see multiple values, it will see only
the first value."
  (setq *mvalues-values* val-forms)
  (setq *mvalues-count*  (length *mvalues-values*))
  (car *mvalues-values*))


(defun values-list (&optional val-forms)
  "Produce multiple values (zero or mode).  Each element of LIST is one value.
This is equivalent to (apply 'values LIST)."
  (cond ((nlistp val-forms)
         (error "Argument to values-list must be a list, not `%s'"
                (prin1-to-string val-forms))))
  (setq *mvalues-values* val-forms)
  (setq *mvalues-count* (length *mvalues-values*))
  (car *mvalues-values*))


;;; Callers that want to see the multiple values use these macros.

(defmacro multiple-value-list (form)
  "Execute FORM and return a list of all the (multiple) values FORM produces.
See `values' and `multiple-value-bind'."
  (list 'progn
        (list 'setq '*mvalues-count* nil)
        (list 'let (list (list 'it '(gensym)))
              (list 'set 'it form)
              (list 'if '*mvalues-count*
                    (list 'copy-sequence '*mvalues-values*)
                    (list 'progn
                          (list 'setq '*mvalues-count* 1)
                          (list 'setq '*mvalues-values*
                                (list 'list (list 'symbol-value 'it)))
                          (list 'copy-sequence '*mvalues-values*))))))

(defmacro multiple-value-call (function &rest args)
  "Call FUNCTION on all the values produced by the remaining arguments.
(multiple-value-call '+ (values 1 2) (values 3 4)) is 10."
  (let* ((result (gentemp))
         (arg    (gentemp)))
    (list 'apply (list 'function (eval function))
          (list 'let* (list (list result '()))
                (list 'dolist (list arg (list 'quote args) result)
                      (list 'setq result
                            (list 'append
                                  result
                                  (list 'multiple-value-list
                                        (list 'eval arg)))))))))

(defmacro multiple-value-bind (vars form &rest body)
  "Bind VARS to the (multiple) values produced by FORM, then do BODY.
VARS is a list of variables; each is bound to one of FORM's values.
If FORM doesn't make enough values, the extra variables are bound to nil.
(Ordinary forms produce only one value; to produce more, use `values'.)
Extra values are ignored.
BODY (zero or more forms) is executed with the variables bound,
then the bindings are unwound."
  (let* ((vals   (gentemp))             ;name for intermediate values
         (clauses (mv-bind-clausify     ;convert into clauses usable
                   vars vals)))         ; in a let form
    (list* 'let*
           (cons (list vals (list 'multiple-value-list form))
                 clauses)
           body)))

(defmacro multiple-value-setq (vars form)
  "Set VARS to the (multiple) values produced by FORM.
VARS is a list of variables; each is set to one of FORM's values.
If FORM doesn't make enough values, the extra variables are set to nil.
(Ordinary forms produce only one value; to produce more, use `values'.)
Extra values are ignored."
  (let* ((vals (gentemp))               ;name for intermediate values
         (clauses (mv-bind-clausify     ;convert into clauses usable
                   vars vals)))         ; in a setq (after append).
    (list 'let*
          (list (list vals (list 'multiple-value-list form)))
          (cons 'setq (apply (function append) clauses)))))

(defmacro multiple-value-prog1 (form &rest body)
  "Evaluate FORM, then BODY, then produce the same values FORM produced.
Thus, (multiple-value-prog1 (values 1 2) (foobar)) produces values 1 and 2.
This is like `prog1' except that `prog1' would produce only one value,
which would be the first of FORM's values."
  (let* ((heldvalues (gentemp)))
    (cons 'let*
          (cons (list (list heldvalues (list 'multiple-value-list form)))
                (append body (list (list 'values-list heldvalues)))))))

;;; utility functions
;;;
;;; mv-bind-clausify makes the pairs needed to have the variables in
;;; the variable list correspond with the values returned by the form.
;;; vals is a fresh symbol that intervenes in all the bindings.

(defun mv-bind-clausify (vars vals)
  "MV-BIND-CLAUSIFY VARS VALS => Auxiliary list
Forms a list of pairs `(,(nth i vars) (nth i vals)) for i from 0 to
the length of VARS (a list of symbols).  VALS is just a fresh symbol."
  (if (or (nlistp vars)
          (notevery 'symbolp vars))
      (error "Expected a list of symbols, not `%s'"
             (prin1-to-string vars)))
  (let* ((nvars    (length vars))
         (clauses '()))
    (dotimes (n nvars clauses)
      (setq clauses (cons (list (nth n vars)
                                (list 'nth n vals)) clauses)))))

;;;; end of cl-multiple-values.el

;;;; ARITH
;;;;    This file provides integer arithmetic extensions.  Although
;;;;    Emacs Lisp doesn't really support anything but integers, that
;;;;    has still to be made to look more or less standard.
;;;;
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)


(defun plusp (number)
  "True if NUMBER is strictly greater than zero."
  (> number 0))

(defun minusp (number)
  "True if NUMBER is strictly less than zero."
  (< number 0))

(defun oddp (number)
  "True if INTEGER is not divisible by 2."
  (/= (% number 2) 0))

(defun evenp (number)
  "True if INTEGER is divisible by 2."
  (= (% number 2) 0))

(defun abs (number)
  "Return the absolute value of NUMBER."
  (cond
   ((< number 0)
    (- 0 number))
   (t                                   ;number is >= 0
    number)))

(defun signum (number)
  "Return -1, 0 or 1 according to the sign of NUMBER."
  (cond
   ((< number 0)
    -1)
   ((> number 0)
    1)
   (t                                   ;exactly zero
    0)))

(defun gcd (&rest integers)
  "Return the greatest common divisor of all the arguments.
The arguments must be integers.  With no arguments, value is zero."
  (let ((howmany (length integers)))
    (cond
     ((= howmany 0)
      0)
     ((= howmany 1)
      (abs (car integers)))
     ((> howmany 2)
      (apply (function gcd)
       (cons (gcd (nth 0 integers) (nth 1 integers))
             (nthcdr 2 integers))))
     (t                                 ;howmany=2
      ;; essentially the euclidean algorithm
      (when (zerop (* (nth 0 integers) (nth 1 integers)))
        (error "A zero argument is invalid for `gcd'"))
      (do* ((absa (abs (nth 0 integers))) ; better to operate only
            (absb (abs (nth 1 integers))) ;on positives.
            (dd (max absa absb))        ; setup correct order for the
            (ds (min absa absb))        ;succesive divisions.
            ;; intermediate results
            (q 0)
            (r 0)
            ;; final results
            (done nil)                  ; flag: end of iterations
            (result 0))                 ; final value
          (done result)
        (setq q (/ dd ds))
        (setq r (% dd ds))
        (cond 
         ((zerop r) (setq done t) (setq result ds))
         ( t        (setq dd ds)  (setq ds r))))))))

(defun lcm (integer &rest more)
  "Return the least common multiple of all the arguments.
The arguments must be integers and there must be at least one of them."
  (let ((howmany (length more))
        (a       integer)
        (b       (nth 0 more))
        prod                            ; intermediate product
        (yetmore (nthcdr 1 more)))
    (cond
     ((zerop howmany)
      (abs a))
     ((> howmany 1)                     ; recursive case
      (apply (function lcm)
             (cons (lcm a b) yetmore)))
     (t                                 ; base case, just 2 args
      (setq prod (* a b))
      (cond
       ((zerop prod)
        0)
       (t
        (/ (abs prod) (gcd a b))))))))

(defun isqrt (number)
  "Return the integer square root of NUMBER.
NUMBER must not be negative.  Result is largest integer less than or
equal to the real square root of the argument."
  (cond
   ((minusp number)
    (error "Argument to `isqrt' must not be negative"))
   ((zerop number)
    0)
   ((<= number 3)
    1)
   (t
    ;; This is some sort of newtonian iteration, trying not to get in
    ;; an infinite loop.  That's why I catch 0, 1, 2 and 3 as special
    ;; cases, so then rounding won't make this iteration loop.
    (do* ((approx (/ number 2) iter)
          (done nil)
          (iter   0))
        (done (if (> (* approx approx) number)
                  (- approx 1)          ;reached from above
                  approx))
      (setq iter
            (/ (+ approx
                  (/ number approx)
                  (if (>= (% number approx) (/ approx 2))
                      1 0))
               2))
      (setq done (eql approx iter))))))

(defun floor (number &optional divisor)
  "Divide DIVIDEND by DIVISOR, rounding toward minus infinity.
DIVISOR defaults to 1.  The remainder is produced as a second value."
  (cond
   ((and (null divisor)                 ; trivial case
         (numberp number))
    (values number 0))
   (t                                   ; do the division
    (multiple-value-bind
        (q r s)
        (safe-idiv number divisor)
      (cond
       ((zerop s)
        (values 0 0))
       ((plusp s)
        (values q r))
       (t
        (unless (zerop r)
          (setq q (- 0 (+ q 1)))
          (setq r (- number (* q divisor))))
        (values q r)))))))

(defun ceiling (number &optional divisor)
  "Divide DIVIDEND by DIVISOR, rounding toward plus infinity.
DIVISOR defaults to 1.  The remainder is produced as a second value."
  (cond
   ((and (null divisor)                 ; trivial case
         (numberp number))
    (values number 0))
   (t                                   ; do the division
    (multiple-value-bind
        (q r s)
        (safe-idiv number divisor)
      (cond
       ((zerop s)
        (values 0 0))
       ((minusp s)
        (values q r))
       (t
        (unless (zerop r)
          (setq q (+ q 1))
          (setq r (- number (* q divisor))))
        (values q r)))))))

(defun truncate (number &optional divisor)
  "Divide DIVIDEND by DIVISOR, rounding toward zero.
DIVISOR defaults to 1.  The remainder is produced as a second value."
  (cond
   ((and (null divisor)                 ; trivial case
         (numberp number))
    (values number 0))
   (t                                   ; do the division
    (multiple-value-bind
        (q r s)
        (safe-idiv number divisor)
      (cond
       ((zerop s)
        (values 0 0))
       ((plusp s)
        (values q r))
       (t
        (unless (zerop r)
          (setq q (- 0 q))
          (setq r (- number (* q divisor))))
        (values q r)))))))

(defun round (number &optional divisor)
  "Divide DIVIDEND by DIVISOR, rounding to nearest integer.
DIVISOR defaults to 1.  The remainder is produced as a second value."
  (cond
   ((and (null divisor)                 ; trivial case
         (numberp number))
    (values number 0))    
   (t                                   ; do the division
    (multiple-value-bind
        (q r s)
        (safe-idiv number divisor)
      (setq r (abs r))
      ;; adjust magnitudes first, and then signs
      (let ((other-r (- (abs divisor) r)))
        (cond
         ((> r other-r)
          (setq q (+ q 1)))
         ((and (= r other-r)
               (oddp q))
          ;; round to even is mandatory
          (setq q (+ q 1))))
        (setq q (* s q))
        (setq r (- number (* q divisor)))
        (values q r))))))

(defun mod (number divisor)
  "Return remainder of X by Y (rounding quotient toward minus infinity).
That is, the remainder goes with the quotient produced by `floor'."
  (multiple-value-bind (q r) (floor number divisor)
    r))

(defun rem (number divisor)
  "Return remainder of X by Y (rounding quotient toward zero).
That is, the remainder goes with the quotient produced by `truncate'."
  (multiple-value-bind (q r) (truncate number divisor)
    r))

;;; internal utilities
;;;
;;; safe-idiv performs an integer division with positive numbers only.
;;; It is known that some machines/compilers implement weird remainder
;;; computations when working with negatives, so the idea here is to
;;; make sure we know what is coming back to the caller in all cases.

(defun safe-idiv (a b)
  "SAFE-IDIV A B => Q R S
Q=|A|/|B|, R is the rest, S is the sign of A/B."
  (unless (and (numberp a) (numberp b))
    (error "Arguments to `safe-idiv' must be numbers"))
  (when (zerop b)
    (error "Cannot divide %d by zero" a))
  (let* ((absa (abs a))
         (absb (abs b))
         (q    (/ absa absb))
         (s    (* (signum a) (signum b)))
         (r    (- a (* (* s q) b))))
    (values q r s)))

;;;; end of cl-arith.el

;;;; SETF
;;;;    This file provides the setf macro and friends. The purpose has
;;;;    been modest, only the simplest defsetf forms are accepted.
;;;;    Use it and enjoy.
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)


(defkeyword :setf-update-fn
  "Property, its value is the function setf must invoke to update a
generalized variable whose access form is a function call of the
symbol that has this property.")

(defkeyword :setf-update-doc
  "Property of symbols that have a `defsetf' update function on them,
installed by the `defsetf' from its optional third argument.")

(defmacro setf (&rest pairs)
  "Generalized `setq' that can set things other than variable values.
A use of `setf' looks like (setf {PLACE VALUE}...).
The behavior of (setf PLACE VALUE) is to access the generalized variable
at PLACE and store VALUE there.  It returns VALUE.  If there is more
than one PLACE and VALUE, each PLACE is set from its VALUE before
the next PLACE is evaluated."
  (let ((nforms (length pairs)))
    ;; check the number of subforms
    (cond
     ((/= (% nforms 2) 0)
      (error "Odd number of arguments to `setf'"))
     ((= nforms 0)
      nil)
     ((> nforms 2)
      ;; this is the recursive case
      (cons 'progn
            (do*                        ;collect the place-value pairs
                ((args pairs (cddr args))
                 (place (car args) (car args))
                 (value (cadr args) (cadr args))
                 (result '()))
                ((endp args) (nreverse result))
              (setq result
                    (cons (list 'setf place value)
                          result)))))
     (t                                 ;i.e., nforms=2
      ;; this is the base case (SETF PLACE VALUE)
      (let* ((place (car pairs))
             (value (cadr pairs))
             (head  nil)
             (updatefn nil))
        ;; dispatch on the type of the PLACE
        (cond
         ((symbolp place)
          (list 'setq place value))
         ((and (listp place)
               (setq head (car place))
               (symbolp head)
               (setq updatefn (get head :setf-update-fn)))
	  (if (or (and (consp updatefn) (eq (car updatefn) 'lambda))
		  (and (symbolp updatefn)
		       (fboundp updatefn)
		       (let ((defn (symbol-function updatefn)))
			 (or (subrp defn)
			     (and (consp defn) (eq (car defn) 'lambda))))))
	      (cons updatefn (append (cdr place) (list value)))
	    (multiple-value-bind
		(bindings newsyms)
		(pair-with-newsyms (append (cdr place) (list value)))
	      ;; this let* gets new symbols to ensure adequate order of
	      ;; evaluation of the subforms.
	      (list 'let
		    bindings              
		    (cons updatefn newsyms)))))
         (t
          (error "No `setf' update-function for `%s'"
                 (prin1-to-string place)))))))))

(defmacro defsetf (accessfn updatefn &optional docstring)
  "Define how `setf' works on a certain kind of generalized variable.
A use of `defsetf' looks like (defsetf ACCESSFN UPDATEFN [DOCSTRING]).
ACCESSFN is a symbol.  UPDATEFN is a function or macro which takes
one more argument than ACCESSFN does.  DEFSETF defines the translation
of (SETF (ACCESFN . ARGS) NEWVAL) to be a form like (UPDATEFN ARGS... NEWVAL).
The function UPDATEFN must return its last arg, after performing the
updating called for."
  ;; reject ill-formed requests.  too bad one can't test for functionp
  ;; or macrop.
  (when (not (symbolp accessfn))
    (error "First argument of `defsetf' must be a symbol, not `%s'"
           (prin1-to-string accessfn)))
  ;; update properties
  (put accessfn :setf-update-fn updatefn)
  (put accessfn :setf-update-doc docstring))

;;; This section provides the "default" setfs for Common-Emacs-Lisp
;;; The user will not normally add anything to this, although
;;; defstruct will introduce new ones as a matter of fact.
;;;
;;; Apply is a special case.   The Common Lisp
;;; standard makes the case of apply be useful when the user writes
;;; something like (apply #'name ...), Emacs Lisp doesn't have the #
;;; stuff, but it has (function ...).  Notice that V18 includes a new
;;; apply: this file is compatible with V18 and pre-V18 Emacses.

;;; INCOMPATIBILITY: the SETF macro evaluates its arguments in the
;;; (correct) left to right sequence *before* checking for apply
;;; methods (which should really be an special case inside setf).  Due
;;; to this, the lambda expression defsetf'd to apply will succeed in
;;; applying the right function even if the name was not quoted, but
;;; computed!  That extension is not Common Lisp (nor is particularly
;;; useful, I think).

(defsetf apply
  (lambda (&rest args)
    ;; dissasemble the calling form
    ;; "(((quote fn) x1 x2 ... xn) val)" (function instead of quote, too)
    (let* ((fnform (car args))          ;functional form
           (applyargs (append           ;arguments "to apply fnform"
                       (apply 'list* (butlast (cdr args)))
                       (last args)))
           (newupdater nil))            ; its update-fn, if any
      (cond
       ((and (symbolp fnform)
             (setq newupdater (get fnform :setf-update-fn)))
        ;; just do it
        (apply  newupdater applyargs))
       (t
        (error "Can't `setf' to `%s'"
               (prin1-to-string fnform))))))
  "`apply' is a special case for `setf'")


(defsetf aref
  aset
  "`setf' inversion for `aref'")

(defsetf nth
  setnth
  "`setf' inversion for `nth'")

(defsetf nthcdr
  setnthcdr
  "`setf' inversion for `nthcdr'")

(defsetf elt
  setelt
  "`setf' inversion for `elt'")

(defsetf first
  (lambda (list val) (setnth 0 list val))
  "`setf' inversion for `first'")

(defsetf second
  (lambda (list val) (setnth 1 list val))
  "`setf' inversion for `second'")

(defsetf third
  (lambda (list val) (setnth 2 list val))
  "`setf' inversion for `third'")

(defsetf fourth
  (lambda (list val) (setnth 3 list val))
  "`setf' inversion for `fourth'")

(defsetf fifth
  (lambda (list val) (setnth 4 list val))
  "`setf' inversion for `fifth'")

(defsetf sixth
  (lambda (list val) (setnth 5 list val))
  "`setf' inversion for `sixth'")

(defsetf seventh
  (lambda (list val) (setnth 6 list val))
  "`setf' inversion for `seventh'")

(defsetf eighth
  (lambda (list val) (setnth 7 list val))
  "`setf' inversion for `eighth'")

(defsetf ninth
  (lambda (list val) (setnth 8 list val))
  "`setf' inversion for `ninth'")

(defsetf tenth
  (lambda (list val) (setnth 9 list val))
  "`setf' inversion for `tenth'")

(defsetf rest
  (lambda (list val) (setcdr list val))
  "`setf' inversion for `rest'")

(defsetf car setcar "Replace the car of a cons")

(defsetf cdr setcdr "Replace the cdr of a cons")

(defsetf caar
  (lambda (list val) (setcar (nth 0 list) val))
  "`setf' inversion for `caar'")

(defsetf cadr
  (lambda (list val) (setcar (cdr list) val))
  "`setf' inversion for `cadr'")

(defsetf cdar
  (lambda (list val) (setcdr (car list) val))
  "`setf' inversion for `cdar'")

(defsetf cddr
  (lambda (list val) (setcdr (cdr list) val))
  "`setf' inversion for `cddr'")

(defsetf caaar
  (lambda (list val) (setcar (caar list) val))
  "`setf' inversion for `caaar'")

(defsetf caadr
  (lambda (list val) (setcar (cadr list) val))
  "`setf' inversion for `caadr'")

(defsetf cadar
  (lambda (list val) (setcar (cdar list) val))
  "`setf' inversion for `cadar'")

(defsetf cdaar
  (lambda (list val) (setcdr (caar list) val))
  "`setf' inversion for `cdaar'")

(defsetf caddr
  (lambda (list val) (setcar (cddr list) val))
  "`setf' inversion for `caddr'")

(defsetf cdadr
  (lambda (list val) (setcdr (cadr list) val))
  "`setf' inversion for `cdadr'")

(defsetf cddar
  (lambda (list val) (setcdr (cdar list) val))
  "`setf' inversion for `cddar'")

(defsetf cdddr
  (lambda (list val) (setcdr (cddr list) val))
  "`setf' inversion for `cdddr'")

(defsetf caaaar
  (lambda (list val) (setcar (caaar list) val))
  "`setf' inversion for `caaaar'")

(defsetf caaadr
  (lambda (list val) (setcar (caadr list) val))
  "`setf' inversion for `caaadr'")

(defsetf caadar
  (lambda (list val) (setcar (cadar list) val))
  "`setf' inversion for `caadar'")

(defsetf cadaar
  (lambda (list val) (setcar (cdaar list) val))
  "`setf' inversion for `cadaar'")

(defsetf cdaaar
  (lambda (list val) (setcdr (caar list) val))
  "`setf' inversion for `cdaaar'")

(defsetf caaddr
  (lambda (list val) (setcar (caddr list) val))
  "`setf' inversion for `caaddr'")

(defsetf cadadr
  (lambda (list val) (setcar (cdadr list) val))
  "`setf' inversion for `cadadr'")

(defsetf cdaadr
  (lambda (list val) (setcdr (caadr list) val))
  "`setf' inversion for `cdaadr'")

(defsetf caddar
  (lambda (list val) (setcar (cddar list) val))
  "`setf' inversion for `caddar'")

(defsetf cdadar
  (lambda (list val) (setcdr (cadar list) val))
  "`setf' inversion for `cdadar'")

(defsetf cddaar
  (lambda (list val) (setcdr (cdaar list) val))
  "`setf' inversion for `cddaar'")

(defsetf cadddr
  (lambda (list val) (setcar (cdddr list) val))
  "`setf' inversion for `cadddr'")

(defsetf cddadr
  (lambda (list val) (setcdr (cdadr list) val))
  "`setf' inversion for `cddadr'")

(defsetf cdaddr
  (lambda (list val) (setcdr (caddr list) val))
  "`setf' inversion for `cdaddr'")

(defsetf cdddar
  (lambda (list val) (setcdr (cddar list) val))
  "`setf' inversion for `cdddar'")

(defsetf cddddr
  (lambda (list val) (setcdr (cddr list) val))
  "`setf' inversion for `cddddr'")


(defsetf get
  put
  "`setf' inversion for `get' is `put'")

(defsetf symbol-function
  fset
  "`setf' inversion for `symbol-function' is `fset'")

(defsetf symbol-plist
  setplist
  "`setf' inversion for `symbol-plist' is `setplist'")

(defsetf symbol-value
  set
  "`setf' inversion for `symbol-value' is `set'")

;;; Modify macros
;;;
;;; It could be nice to implement define-modify-macro, but I don't
;;; think it really pays.

(defmacro incf (ref &optional delta)
  "(incf REF [DELTA]) -> increment the g.v. REF by DELTA (default 1)"
  (if (null delta)
      (setq delta 1))
  (list 'setf ref (list '+ ref delta)))

(defmacro decf (ref &optional delta)
  "(decf REF [DELTA]) -> decrement the g.v. REF by DELTA (default 1)"
  (if (null delta)
      (setq delta 1))
  (list 'setf ref (list '- ref delta)))

(defmacro push (item ref)
  "(push ITEM REF) -> cons ITEM at the head of the g.v. REF (a list)"
  (list 'setf ref (list 'cons item ref)))

(defmacro pushnew (item ref)
  "(pushnew ITEM REF): adjoin ITEM at the head of the g.v. REF (a list)"
  (list 'setf ref (list 'adjoin item ref)))

(defmacro pop (ref)
  "(pop REF) -> (prog1 (car REF) (setf REF (cdr REF)))"
  (let ((listname (gensym)))
    (list 'let (list (list listname ref))
          (list 'prog1
                (list 'car listname)
                (list 'setf ref (list 'cdr listname))))))

;;; PSETF
;;;
;;; Psetf is the generalized variable equivalent of psetq.  The right
;;; hand sides are evaluated and assigned (via setf) to the left hand
;;; sides. The evaluations are done in an environment where they
;;; appear to occur in parallel.

(defmacro psetf (&rest pairs)
  "(psetf {PLACE VALUE}...): Set several generalized variables in parallel.
All the VALUEs are computed, and then all the PLACEs are stored as in `setf'.
See also `psetq', `shiftf' and `rotatef'."
  (unless (evenp (length pairs))
    (error "Odd number of arguments to `psetf'"))
  (multiple-value-bind
      (places forms)
      (unzip-list pairs)
    ;; obtain fresh symbols to simulate the parallelism
    (multiple-value-bind
        (bindings newsyms)
        (pair-with-newsyms forms)
      (list 'let
            bindings
            (cons 'setf (zip-lists places newsyms))
            nil))))

;;; SHIFTF and ROTATEF 
;;;

(defmacro shiftf (&rest forms)
  "(shiftf PLACE1 PLACE2... NEWVALUE): set PLACE1 to PLACE2, PLACE2 to PLACE3...
Each PLACE is set to the old value of the following PLACE,
and the last PLACE is set to the value NEWVALUE."
  (unless (> (length forms) 1)
    (error "`shiftf' needs more than one argument"))
  (let ((places (butlast forms))
	(newvalue (car (last forms))))
    ;; the places are accessed to fresh symbols
    (multiple-value-bind
	(bindings newsyms)
	(pair-with-newsyms places)
      (list 'let bindings
	    (cons 'setf
		  (zip-lists places
			     (append (cdr newsyms) (list newvalue))))
	    (car newsyms)))))

(defmacro rotatef (&rest places)
  "(rotatef PLACE...) sets each PLACE to the old value of the following PLACE.
The last PLACE is set to the old value of the first PLACE.
Thus, the values rotate through the PLACEs."
  (cond
   ((null places)
    nil)
   (t
    (multiple-value-bind
	(bindings newsyms)
	(pair-with-newsyms places)
      (list
       'let bindings
       (cons 'setf
	     (zip-lists places
			(append (cdr newsyms) (list (car newsyms)))))
       nil)))))

;;;; STRUCTS
;;;;    This file provides the structures mechanism.  See the
;;;;    documentation for Common-Lisp's defstruct.  Mine doesn't
;;;;    implement all the functionality of the standard, although some
;;;;    more could be grafted if so desired.  More details along with
;;;;    the code.
;;;;
;;;;
;;;;    Cesar Quiroz @ UofR DofCSc - Dec. 1986
;;;;       (quiroz@cs.rochester.edu)


(defkeyword :include             "Syntax of `defstruct'")
(defkeyword :named               "Syntax of `defstruct'")
(defkeyword :conc-name           "Syntax of `defstruct'")
(defkeyword :copier              "Syntax of `defstruct'")
(defkeyword :predicate           "Syntax of `defstruct'")
(defkeyword :print-function      "Syntax of `defstruct'")
(defkeyword :type                "Syntax of `defstruct'")
(defkeyword :initial-offset      "Syntax of `defstruct'")

(defkeyword :structure-doc       "Documentation string for a structure.")
(defkeyword :structure-slotsn    "Number of slots in structure")
(defkeyword :structure-slots     "List of the slot's names")
(defkeyword :structure-indices   "List of (KEYWORD-NAME . INDEX)")
(defkeyword :structure-initforms "List of (KEYWORD-NAME . INITFORM)")


(defmacro defstruct (&rest args)
  "(defstruct NAME [DOC-STRING] . SLOTS)  define NAME as structure type.
NAME must be a symbol, the name of the new structure.  It could also
be a list (NAME . OPTIONS), but not all options are supported currently.
As of Dec. 1986, this is supporting :conc-name, :copier and :predicate
completely, :include arguably completely and :constructor only to
change the name of the default constructor.  No BOA constructors allowed.
The DOC-STRING is established as the 'structure-doc' property of NAME.
The SLOTS are one or more of the following:
SYMBOL -- meaning the SYMBOL is the name of a SLOT of NAME
list of SYMBOL and VALUE -- meaning that VALUE is the initial value of
the slot.
`defstruct' defines functions `make-NAME', `NAME-p', `copy-NAME' for the
structure, and functions with the same name as the slots to access
them.  `setf' of the accessors sets their values."
  (multiple-value-bind
      (name options docstring slotsn slots initlist)
      (parse$defstruct$args args)
    ;; Names for the member functions come from the options.  The
    ;; slots* stuff collects info about the slots declared explicitly. 
    (multiple-value-bind
        (conc-name constructor copier predicate moreslotsn moreslots moreinits)
        (parse$defstruct$options name options slots)
      ;; The moreslots* stuff refers to slots gained as a consequence
      ;; of (:include clauses).
      (when (and (numberp moreslotsn)
                 (> moreslotsn 0))
        (setf slotsn (+ slotsn moreslotsn))
        (setf slots (append moreslots slots))
        (setf initlist (append moreinits initlist)))
      (unless (> slotsn 0)
        (error "%s needs at least one slot"
               (prin1-to-string name)))
      (let ((dups (duplicate-symbols-p slots)))
        (when dups
          (error "`%s' are duplicates"
                 (prin1-to-string dups))))
      (setq initlist (simplify$inits slots initlist))
      (let (properties functions keywords accessors alterators returned)
        ;; compute properties of NAME
        (setq properties
              (list
               (list 'put (list 'quote name) :structure-doc
                     docstring)
               (list 'put (list 'quote name) :structure-slotsn
                     slotsn)
               (list 'put (list 'quote name) :structure-slots
                     (list 'quote slots))
               (list 'put (list 'quote name) :structure-initforms
                     (list 'quote initlist))
               (list 'put (list 'quote name) :structure-indices
                     (list 'quote (extract$indices initlist)))))

        ;; Compute functions associated with NAME.  This is not
	;; handling BOA constructors yet, but here would be the place.
        (setq functions
              (list
               (list 'fset (list 'quote constructor)
                     (list 'function
                           (list 'lambda (list '&rest 'args)
                                 (list 'make$structure$instance
                                       (list 'quote name)
                                       'args))))
               (list 'fset (list 'quote copier)
                     (list 'function
                           (list 'lambda (list 'struct)
                                 (list 'copy-vector 'struct))))
               (list 'fset (list 'quote predicate)
                     (list 'function
                           (list 'lambda (list 'thing)
                                 (list 'and
                                       (list 'vectorp 'thing)
                                       (list 'eq
                                             (list 'elt 'thing 0)
                                             (list 'quote name))
                                       (list '=
                                             (list 'length 'thing)
                                             (1+ slotsn))))))))
        ;; compute accessors for NAME's slots
        (multiple-value-setq
            (accessors alterators keywords)
            (build$accessors$for name conc-name predicate slots slotsn))
        ;; generate returned value -- not defined by the standard
        (setq returned
              (list
               (cons 'vector
                     (mapcar
                      '(lambda (x) (list 'quote x))
                      (cons name slots)))))
        ;; generate code
        (cons 'progn
              (nconc properties functions keywords
                     accessors alterators returned))))))

(defun parse$defstruct$args (args)
  "PARSE$DEFSTRUCT$ARGS ARGS => NAME OPTIONS DOCSTRING SLOTSN SLOTS INITLIST
NAME=symbol, OPTIONS=list of, DOCSTRING=string, SLOTSN=count of slots,
SLOTS=list of their names, INITLIST=alist (keyword . initform)."
  (let (name                            ;args=(symbol...) or ((symbol...)...)
        options                         ;args=((symbol . options) ...)
        (docstring "")                  ;args=(head docstring . slotargs)
        slotargs                        ;second or third cdr of args
        (slotsn 0)                      ;number of slots 
        (slots '())                     ;list of slot names
        (initlist '()))                 ;list of (slot keyword . initform)
    ;; extract name and options
    (cond
     ((symbolp (car args))              ;simple name
      (setq name    (car args)
            options '()))
     ((and (listp   (car args))         ;(name . options)
           (symbolp (caar args)))
      (setq name    (caar args)
            options (cdar args)))
     (t
      (error "First arg to `defstruct' must be symbol or (symbol ...)")))
    (setq slotargs (cdr args))
    ;; is there a docstring?
    (when (stringp (car slotargs))
      (setq docstring (car slotargs)
            slotargs  (cdr slotargs)))
    ;; now for the slots
    (multiple-value-bind
        (slotsn slots initlist)
        (process$slots slotargs)
      (values name options docstring slotsn slots initlist))))

(defun process$slots (slots)
  "PROCESS$SLOTS SLOTS => SLOTSN SLOTSLIST INITLIST
Converts a list of symbols or lists of symbol and form into the last 3
values returned by PARSE$DEFSTRUCT$ARGS."
  (let ((slotsn (length slots))         ;number of slots
        slotslist                       ;(slot1 slot2 ...)
        initlist)                       ;((:slot1 . init1) ...)
    (do*
        ((ptr  slots     (cdr ptr))
         (this (car ptr) (car ptr)))
        ((endp ptr))
      (cond
       ((symbolp this)
        (setq slotslist (cons this slotslist))
        (setq initlist (acons (keyword-of this) nil initlist)))
       ((and (listp this)
             (symbolp (car this)))
        (let ((name (car this))
              (form (cadr this)))
          ;; this silently ignores any slot options.  bad...
          (setq slotslist (cons name slotslist))
          (setq initlist  (acons (keyword-of name) form initlist))))
       (t
        (error "Slot should be symbol or (symbol ...), not `%s'"
               (prin1-to-string this)))))
    (values slotsn (nreverse slotslist) (nreverse initlist))))

(defun parse$defstruct$options (name options slots)
  "PARSE$DEFSTRUCT$OPTIONS NAME OPTIONS SLOTS => CONC-NAME CONST COPIER PRED
Returns at least those 4 values (a string and 3 symbols, to name the necessary
functions),  might return also things discovered by actually
inspecting the options, namely MORESLOTSN MORESLOTS MOREINITS, as can
be created by :include, and perhaps a list of BOACONSTRUCTORS."
  (let* ((namestring (symbol-name name))
         ;; to build the return values
         (conc-name  (concat namestring "-"))
         (const (intern (concat "make-" namestring)))
         (copier (intern (concat "copy-" namestring)))
         (pred (intern (concat namestring "-p")))
         (moreslotsn 0)
         (moreslots '())
         (moreinits '())
         ;; auxiliaries
         option-head                    ;When an option is not a plain
         option-second                  ; keyword, it must be a list of
         option-rest                    ; the form (head second . rest)
         these-slotsn                   ;When :include is found, the
         these-slots                    ; info about the included
         these-inits                    ; structure is added here.
         )
    ;; Values above are the defaults.  Now we read the options themselves
    (dolist (option options)
      ;; 2 cases arise, as options must be a keyword or a list
      (cond
       ((keywordp option)
        (case option
          (:named
           )                            ;ignore silently
          (t
           (error "Can't recognize option `%s'"
                  (prin1-to-string option)))))
       ((and (listp option)
             (keywordp (setq option-head (car option))))
        (setq option-second (second option))
        (setq option-rest   (nthcdr 2 option))
        (case option-head
          (:conc-name
           (setq conc-name
                 (cond
                  ((stringp option-second)
                   option-second)
                  ((null option-second)
                   "")
                  (t
                   (error "`%s' is invalid as `conc-name'"
                          (prin1-to-string option-second))))))
          (:copier
           (setq copier
                 (cond
                  ((and (symbolp option-second)
                        (null option-rest))
                   option-second)
                  (t
                   (error "Can't recognize option `%s'"
                          (prin1-to-string option))))))

          (:constructor                 ;no BOA-constructors allowed
           (setq const
                 (cond
                  ((and (symbolp option-second)
                        (null option-rest))
                   option-second)
                  (t
                   (error "Can't recognize option `%s'"
                          (prin1-to-string option))))))
          (:predicate
           (setq pred
                 (cond
                  ((and (symbolp option-second)
                        (null option-rest))
                   option-second)
                  (t
                   (error "Can't recognize option `%s'"
                          (prin1-to-string option))))))
          (:include
           (unless (symbolp option-second)
             (error "Arg to `:include' should be a symbol, not `%s'"
                    (prin1-to-string option-second)))
           (setq these-slotsn (get option-second :structure-slotsn)
                 these-slots  (get option-second :structure-slots)
                 these-inits  (get option-second :structure-initforms))
           (unless (and (numberp these-slotsn)
                        (> these-slotsn 0))
             (error "`%s' is not a valid structure"
                    (prin1-to-string option-second)))
           (multiple-value-bind
               (xtra-slotsn xtra-slots xtra-inits)
               (process$slots option-rest)
             (when (> xtra-slotsn 0)
               (dolist (xslot xtra-slots)
                 (unless (memq xslot these-slots)
                   (error "`%s' is not a slot of `%s'"
                          (prin1-to-string xslot)
                          (prin1-to-string option-second))))
               (setq these-inits (append xtra-inits these-inits)))
             (setq moreslotsn (+ moreslotsn these-slotsn))
             (setq moreslots  (append these-slots moreslots))
             (setq moreinits  (append these-inits moreinits))))
          ((:print-function :type :initial-offset)
           )                            ;ignore silently
          (t
           (error "Can't recognize option `%s'"
                  (prin1-to-string option)))))
       (t
        (error "Can't recognize option `%s'"
               (prin1-to-string option)))))
    ;; Return values found
    (values conc-name const copier pred
            moreslotsn moreslots moreinits)))

(defun simplify$inits (slots initlist)
  "SIMPLIFY$INITS SLOTS INITLIST => new INITLIST
Removes from INITLIST - an ALIST - any shadowed bindings."
  (let ((result '())                    ;built here
        key                             ;from the slot 
        )
    (dolist (slot slots)
      (setq key (keyword-of slot))
      (setq result (acons key (cdr (assoc key initlist)) result)))
    (nreverse result)))

(defun extract$indices (initlist)
  "EXTRACT$INDICES INITLIST => indices list
Kludge.  From a list of pairs (keyword . form) build a list of pairs
of the form (keyword . position in list from 0).  Useful to precompute
some of the work of MAKE$STRUCTURE$INSTANCE."
  (let ((result '())
        (index   0))
    (dolist (entry initlist (nreverse result))
      (setq result (acons (car entry) index result)
            index  (+ index 1)))))

(defun build$accessors$for (name conc-name predicate slots slotsn)
  "BUILD$ACCESSORS$FOR NAME PREDICATE SLOTS SLOTSN  => FSETS DEFSETFS KWDS
Generate the code for accesors and defsetfs of a structure called
NAME, whose slots are SLOTS.  Also, establishes the keywords for the
slots names."
  (do ((i 0 (1+ i))
       (accessors '())
       (alterators '())
       (keywords '())
       (canonic  ""))                   ;slot name with conc-name prepended
      ((>= i slotsn)
       (values
        (nreverse accessors) (nreverse alterators) (nreverse keywords)))
    (setq canonic (intern (concat conc-name (symbol-name (nth i slots)))))
    (setq accessors
          (cons
           (list 'fset (list 'quote canonic)
                 (list 'function
                       (list 'lambda (list 'object)
                             (list 'cond
                                   (list (list predicate 'object)
                                         (list 'aref 'object (1+ i)))
                                   (list 't
                                         (list 'error
                                               "`%s' not a %s."
                                               (list 'prin1-to-string
                                                     'object)
                                               (list 'prin1-to-string
                                                     (list 'quote
                                                           name))))))))
           accessors))
    (setq alterators
           (cons
            (list 'defsetf canonic
                  (list 'lambda (list 'object 'newval)
                        (list 'cond
                              (list (list predicate 'object)
                                    (list 'aset 'object (1+ i) 'newval))
                              (list 't
                                    (list 'error
                                          "`%s' not a `%s'"
                                          (list 'prin1-to-string
                                                'object)
                                          (list 'prin1-to-string
                                                (list 'quote
                                                      name)))))))
            alterators))
    (setq keywords
          (cons (list 'defkeyword (keyword-of (nth i slots)))
                keywords))))

(defun make$structure$instance (name args)
  "MAKE$STRUCTURE$INSTANCE NAME ARGS => new struct NAME
A struct of type NAME is created, some slots might be initialized
according to ARGS (the &rest argument of MAKE-name)."
  (unless (symbolp name)
    (error "`%s' is not a possible name for a structure"
           (prin1-to-string name)))
  (let ((initforms (get name :structure-initforms))
        (slotsn    (get name :structure-slotsn))
        (indices   (get name :structure-indices))
        initalist                       ;pairlis'd on initforms
        initializers                    ;definitive initializers
        )
    ;; check sanity of the request
    (unless (and (numberp slotsn)
                 (> slotsn 0))
      (error "`%s' is not a defined structure"
             (prin1-to-string name)))
    (unless (evenp (length args))
      (error "Slot initializers `%s' not of even length"
             (prin1-to-string args)))
    ;; analyze the initializers provided by the call
    (multiple-value-bind
        (speckwds specvals)             ;keywords and values given 
        (unzip-list args)               ; by the user
      ;; check that all the arguments are introduced by keywords 
      (unless (every (function keywordp) speckwds)
        (error "All of the names in `%s' should be keywords"
               (prin1-to-string speckwds)))
      ;; check that all the keywords are known
      (dolist (kwd speckwds)
        (unless (numberp (cdr (assoc kwd indices)))
          (error "`%s' is not a valid slot name for %s"
                 (prin1-to-string kwd) (prin1-to-string name))))
      ;; update initforms
      (setq initalist
            (pairlis speckwds
                     (do* ;;protect values from further evaluation
                         ((ptr specvals (cdr ptr))
                          (val (car ptr) (car ptr))
                          (result '()))
                         ((endp ptr) (nreverse result))
                       (setq result
                             (cons (list 'quote val)
                                   result)))
                     (copy-sequence initforms)))
      ;; compute definitive initializers
      (setq initializers
            (do* ;;gather the values of the most definitive forms
                ((ptr indices (cdr ptr))
                 (key (caar ptr) (caar ptr))
                 (result '()))
                ((endp ptr) (nreverse result))
              (setq result
                    (cons (eval (cdr (assoc key initalist))) result))))
      ;; do real initialization
      (apply (function vector)
             (cons name initializers)))))

;;;; end of cl-structs.el

;;;; end of cl.el
