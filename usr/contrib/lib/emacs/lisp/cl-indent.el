;; Lisp mode, and its idiosyncratic commands.
;; Copyright (C) 1987 Free Software Foundation, Inc.
;; Written by Richard Mlynarik July 1987

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;>> TODO
;; :foo
;;   bar
;; :baz
;;   zap
;; &key (like &body)??

;; &rest 1 in lambda-lists doesn't work
;;  -- really want (foo bar
;;                  baz)
;;     not (foo bar
;;              baz)
;;  Need something better than &rest for such cases


;;; Hairy lisp indentation.

(defvar lisp-indent-maximum-backtracking 3
  "*Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is  0, no backtracking will occur and forms such as  flet
may not be correctly indented.")

(defvar lisp-tag-indentation 1
  "*Indentation of tags relative to containing list.
This variable is used by the function  lisp-indent-tagbody.")

(defvar lisp-tag-body-indentation 3
  "*Indentation of non-tagged lines relative to containing list.
This variable is used by the function  lisp-indent-tagbody  to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
he special form.  If the value is  t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by lisp-body-indent.")


(defun common-lisp-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to contining lists.
          ;; `foo' has a path of (0 4 1) in `((a b c (d foo) f) g)'
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (elt state 1))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))
      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth lisp-indent-maximum-backtracking))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem function method)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq function (downcase (buffer-substring tem (point))))
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (get tem 'common-lisp-indent-hook))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function
                                               (1+ (match-beginning 0)))
                           method (get (intern-soft function)
                                       'common-lisp-indent-hook)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (get tem 'lisp-indent-hook)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (condition-case ()
                             (progn
                               (forward-sexp 1)
                               (if (>= (point) indent-point)
                                   nil
                                 (parse-partial-sexp (point)
                                                     indent-point 1 t)
                                 (setq n (1+ n))
                                 t))
                           (error nil))))
              (setq path (cons n path)))

            ;; backwards compatibility.
            (cond ((null function))
                  ((null method)
                   (if (null (cdr path))
                       ;; (package prefix was stripped off above)
                       (setq method (cond ((string-match "\\`def"
                                                         function)
                                           '(4 (&whole 4 &rest 1) &body))
                                          ((string-match "\\`\\(with\\|do\\)-"
                                                         function)
                                           '(4 &body))))))
                  ;; backwards compatibility.  Bletch.
                  ((eq method 'defun)
                   (setq method '(4 (&whole 4 &rest 1) &body))))

            (cond ((and (memq (char-after (1- containing-sexp)) '(?\' ?\`))
                        (not (eql (char-after (- containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((eql (char-after (1- containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-hook)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path)
                                           normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column lisp-body-indent))
                                          (t
                                           ;; other body form
                                           normal-indent))))
                  ((symbolp method)
                   (setq calculated (funcall method
                                             path state indent-point
                                             sexp-column normal-indent)))
                  (t
                   (setq calculated (lisp-indent-259
                                      method path state indent-point
                                      sexp-column normal-indent)))))
          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (if (not calculated)
              (condition-case ()
                   (progn (backward-up-list 1)
                          (setq depth (1+ depth)))
                (error (setq depth lisp-indent-maximum-backtracking))))))
      calculated)))


(defun lisp-indent-report-bad-format (m)
  (error "%s has a badly-formed %s property: %s"
         ;; Love them free variable references!!
         function 'common-lisp-indent-hook m))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun lisp-indent-259 (method path state indent-point
                        sexp-column normal-indent)
  (catch 'exit
    (let ((p path)
          (containing-form-start (elt state 1))
          n tem tail)
      ;; Isn't tail-recursion wonderful?
      (while p
        ;; This while loop is for destructuring.
        ;; p is set to (cdr p) each iteration.
        (if (not (consp method)) (lisp-indent-report-bad-format method))
        (setq n (1- (car p))
              p (cdr p)
              tail nil)
        (while n
          ;; This while loop is for advancing along a method
          ;; until the relevant (possibly &rest/&body) pattern
          ;; is reached.
          ;; n is set to (1- n) and method to (cdr method)
          ;; each iteration.
; (message "trying %s for %s %s" method p function) (sit-for 1)
          (setq tem (car method))

          (or (eq tem 'nil)             ;default indentation
;             (eq tem '&lambda)         ;abbrev for (&whole 4 (&rest 1))
              (and (eq tem '&body) (null (cdr method)))
              (and (eq tem '&rest)
                   (consp (cdr method)) (null (cdr (cdr method))))
              (integerp tem)            ;explicit indentation specified
              (and (consp tem)          ;destructuring
                   (eq (car tem) '&whole)
                   (or (symbolp (car (cdr tem)))
                       (integerp (car (cdr tem)))))
              (and (symbolp tem)        ;a function to call to do the work.
                   (null (cdr method)))
              (lisp-indent-report-bad-format method))

          (cond ((and tail (not (consp tem)))
                 ;; indent tail of &rest in same way as first elt of rest
                 (throw 'exit normal-indent))
                ((eq tem '&body)
                 ;; &body means (&rest <lisp-body-indent>)
                 (throw 'exit
                   (if (and (= n 0)     ;first body form
                            (null p))   ;not in subforms
                       (+ sexp-column
                          lisp-body-indent)
                       normal-indent)))
                ((eq tem '&rest)
                 ;; this pattern holds for all remaining forms
                 (setq tail (> n 0)
                       n 0
                       method (cdr method)))
                ((> n 0)
                 ;; try next element of pattern
                 (setq n (1- n)
                       method (cdr method))
                 (if (< n 0)
                     ;; Too few elements in pattern.
                     (throw 'exit normal-indent)))
                ((eq tem 'nil)
                 (throw 'exit (list normal-indent containing-form-start)))
;               ((eq tem '&lambda)
;                ;; abbrev for (&whole 4 &rest 1)
;                (throw 'exit
;                  (cond ((null p)
;                         (list (+ sexp-column 4) containing-form-start))
;                        ((null (cdr p))
;                         (+ sexp-column 1))
;                        (t normal-indent))))
                ((integerp tem)
                 (throw 'exit
                   (if (null p)         ;not in subforms
                       (list (+ sexp-column tem) containing-form-start)
                       normal-indent)))
                ((symbolp tem)          ;a function to call
                 (throw 'exit
                   (funcall tem path state indent-point
                            sexp-column normal-indent)))
                (t
                 ;; must be a destructing frob
                 (if (not (null p))
                     ;; descend
                     (setq method (cdr (cdr tem))
                           n nil)
                   (setq tem (car (cdr tem)))
                   (throw 'exit
                     (cond (tail
                            normal-indent)
                           ((eq tem 'nil)
                            (list normal-indent
                                  containing-form-start))
                           ((integerp tem)
                            (list (+ sexp-column tem)
                                  containing-form-start))
                           (t
                            (funcall tem path state indent-point
                                     sexp-column normal-indent))))))))))))

(defun lisp-indent-tagbody (path state indent-point sexp-column normal-indent)
  (if (not (null (cdr path)))
      normal-indent
    (save-excursion
      (goto-char indent-point)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (list (cond ((looking-at "\\sw\\|\\s_")
                   ;; a tagbody tag
                   (+ sexp-column lisp-tag-indentation))
                  ((integerp lisp-tag-body-indentation)
                   (+ sexp-column lisp-tag-body-indentation))
                  ((eq lisp-tag-body-indentation 't)
                   (condition-case ()
                       (progn (backward-sexp 1) (current-column))
                     (error (1+ sexp-column))))
                  (t (+ sexp-column lisp-body-indent)))
;            (cond ((integerp lisp-tag-body-indentation)
;                   (+ sexp-column lisp-tag-body-indentation))
;                  ((eq lisp-tag-body-indentation 't)
;                   normal-indent)
;                  (t
;                   (+ sexp-column lisp-body-indent)))
            (elt state 1)
            ))))

(defun lisp-indent-do (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 3)
      (let ((lisp-tag-body-indentation lisp-body-indent))
        (funcall (function lisp-indent-tagbody)
		 path state indent-point sexp-column normal-indent))
    (funcall (function lisp-indent-259)
	     '((&whole nil &rest
 		;; the following causes wierd indentation
 		;;(&whole 1 1 2 nil)
		)
	       (&whole nil &rest 1))
	     path state indent-point sexp-column normal-indent)))

(defun lisp-indent-function-lambda-hack (path state indent-point
                                         sexp-column normal-indent)
  ;; indent (function (lambda () <newline> <body-forms>)) kludgily.
  (if (or (cdr path) ; wtf?
          (> (car path) 3))
      ;; line up under previous body form
      normal-indent
    ;; line up under function rather than under lambda in order to
    ;;  conserve horizontal space.  (Which is what #' is for.)
    (condition-case ()
        (save-excursion
          (backward-up-list 2)
          (forward-char 1)
          (if (looking-at "\\(lisp:+\\)?function\\(\\Sw\\|\\S_\\)")
              (+ lisp-body-indent -1 (current-column))
              (+ sexp-column lisp-body-indent)))
       (error (+ sexp-column lisp-body-indent)))))


(let ((l '((block 1)
	   (catch 1)
           (case        (4 &rest (&whole 2 &rest 1)))
           (ccase . case) (ecase . case)
           (typecase . case) (etypecase . case) (ctypecase . case)
           (catch 1)
           (cond        (&rest (&whole 2 &rest 1)))
           (block 1)
           (defvar      (4 2 2))
           (defconstant . defvar) (defparameter . defvar)
           (define-modify-macro
                        (4 &body))
           (define-setf-method
                        (4 (&whole 4 &rest 1) &body))
           (defsetf     (4 (&whole 4 &rest 1) 4 &body))
           (defun       (4 (&whole 4 &rest 1) &body))
           (defmacro . defun) (deftype . defun)
           (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                         &rest (&whole 2 &rest 1)))
           (destructuring-bind
                        ((&whole 6 &rest 1) 4 &body))
           (do          lisp-indent-do)
           (do* . do)
           (dolist      ((&whole 4 2 1) &body))
           (dotimes . dolist)
           (eval-when	1)
           (flet        ((&whole 4 &rest (&whole 1 (&whole 4 &rest 1) &body))
                         &body))
           (labels . flet)
           (macrolet . flet)
           ;; `else-body' style
           (if          (nil nil &body))
           ;; single-else style (then and else equally indented)
           (if          (&rest nil))
           ;(lambda     ((&whole 4 &rest 1) &body))
           (lambda      ((&whole 4 &rest 1)
                         &rest lisp-indent-function-lambda-hack))
           (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
           (let* . let)
           (locally	1)
           ;(loop ...)
           (multiple-value-bind
                        ((&whole 6 &rest 1) 4 &body))
           (multiple-value-call
			(4 &body))
           (multiple-value-list 1)
           (multiple-value-prog1 1)
           (multiple-value-setq
			(4 2))
           ;; Combines the worst features of BLOCK, LET and TAGBODY
           (prog        ((&whole 4 &rest 1) &rest lisp-indent-tagbody))
           (prog* . prog)
           (prog1 1)
           (prog2 2)
           (progn 0)
           (progv       (4 4 &body))
           (return 0)
           (return-from (nil &body))
           (tagbody     lisp-indent-tagbody)
           (throw 1)
           (unless 1)
           (unwind-protect
                        (5 &body))
           (when 1))))
  (while l
    (put (car (car l)) 'common-lisp-indent-hook
         (if (symbolp (cdr (car l)))
             (get (cdr (car l)) 'common-lisp-indent-hook)
             (car (cdr (car l)))))
    (setq l (cdr l))))


;(defun foo (x)
;  (tagbody
;   foo
;     (bar)
;   baz
;     (when (losing)
;       (with-big-loser
;           (yow)
;         ((lambda ()
;            foo)
;          big)))
;     (flet ((foo (bar baz zap)
;              (zip))
;            (zot ()
;              quux))
;       (do ()
;           ((lose)
;            (foo 1))
;         (quux)
;        foo
;         (lose))
;       (cond ((x)
;              (win 1 2
;                   (foo)))
;             (t
;              (lose
;                3))))))
          

;(put 'while    'common-lisp-indent-hook 1)
;(put 'defwrapper'common-lisp-indent-hook ...)
;(put 'def 'common-lisp-indent-hook ...)
;(put 'defflavor        'common-lisp-indent-hook ...)
;(put 'defsubst 'common-lisp-indent-hook ...)

;;(put 'define-restart-name 'common-lisp-indent-hook '1)
;(put 'with-restart 'common-lisp-indent-hook '((1 4 ((* 1))) (2 &body)))
;(put 'restart-case 'common-lisp-indent-hook '((1 4) (* 2 ((0 1) (* 1)))))
;(put 'define-condition 'common-lisp-indent-hook '((1 6) (2 6 ((* 1))) (3 4 ((* 1))) (4 &body)))
;(put 'with-condition-handler 'common-lisp-indent-hook '((1 4 ((* 1))) (2 &body)))
;(put 'condition-case 'common-lisp-indent-hook '((1 4) (* 2 ((0 1) (1 3) (2 &body)))))


;;;; Turn it on.
;(setq lisp-indent-hook 'common-lisp-indent-hook)

;; To disable this stuff, (setq lisp-indent-hook 'lisp-indent-hook)

