;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Written by Dick King (king@kestrel).

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


;;; This is a rudimentry backquote package written by D. King,
 ;;; king@kestrel, on 8/31/85.  (` x) is a macro
 ;;; that expands to a form that produces x.  (` (a b ..)) is
 ;;; a macro that expands into a form that produces a list of what a b
 ;;; etc. would have produced.  Any element can be of the form
 ;;; (, <form>) in which case the resulting form evaluates
 ;;; <form> before putting it into place, or (,@ <form>), in which
 ;;; case the evaluation of <form> is arranged for and each element
 ;;; of the result (which must be a (possibly null) list) is inserted.
;;; As an example, the immediately following macro push (v l) could
 ;;; have been written 
;;;    (defmacro push (v l)
;;;         (` (setq (, l) (cons (,@ (list v l))))))
 ;;; although
;;;    (defmacro push (v l)
;;;         (` (setq (, l) (cons (, v) (, l)))))
 ;;; is far more natural.  The magic atoms ,
 ;;; and ,@ are user-settable and list-valued.  We recommend that
 ;;; things never be removed from this list lest you break something
 ;;; someone else wrote in the dim past that comes to be recompiled in
 ;;; the distant future.

;;; LIMITATIONS: tail consing is not handled correctly.  Do not say
 ;;; (` (a . (, b))) - say (` (a (,@ b)))
 ;;; which works even if b is not list-valued.
;;; No attempt is made to handle vectors.  (` [a (, b) c]) doesn't work.
;;; Sorry, you must say things like
 ;;; (` (a (,@ 'b))) to get (a . b) and 
 ;;; (` ((, ',) c)) to get (, c) - [(` (a , b)) will work but is a bad habit]
;;; I haven't taught it the joys of nconc.
;;; (` atom) dies.  (` (, atom)) or anything else is okay.

;;; BEWARE BEWARE BEWARE
 ;;; inclusion of (,atom) rather than (, atom) or (,@atom) rather than
 ;;; (,@ atom) will result in errors that will show up very late.
 ;;; This is so crunchy that I am considering including a check for
 ;;; this or changing the syntax to ... ,(<form>).  RMS: opinion?


(provide 'backquote)

;;; a raft of general-purpose macros follows.  See the nearest
 ;;; Commonlisp manual.
(defmacro bq-push (v l)
  "Pushes evaluated first form onto second unevaluated object
a list-value atom"
  (list 'setq l (list 'cons v l)))

(defmacro bq-caar (l)
  (list 'car (list 'car l)))

(defmacro bq-cadr (l)
  (list 'car (list 'cdr l)))

(defmacro bq-cdar (l)
  (list 'cdr (list 'car l)))


;;; These two advertised variables control what characters are used to
 ;;; unquote things.  I have included , and ,@ as the unquote and
 ;;; splice operators, respectively, to give users of MIT CADR machine
 ;;; derivitive machines a warm, cosy feeling.

(defconst backquote-unquote '(,)
  "*A list of all objects that stimulate unquoting in `.  Memq test.")


(defconst backquote-splice '(,@)
  "*A list of all objects that stimulate splicing in `.  Memq test.")


;;; This is the interface 
(defmacro ` (form)
  "(` FORM) Expands to a form that will generate FORM.
FORM is `almost quoted' -- see backquote.el for a description."
  (bq-make-maker form))

;;; We develop the method for building the desired list from
 ;;; the end towards the beginning.  The contract is that there be a
 ;;; variable called state and a list called tailmaker, and that the form
 ;;; (cons state tailmaker) deliver the goods.  Exception - if the
 ;;; state is quote the tailmaker is the form itself.
;;; This function takes a form and returns what I will call a maker in
 ;;; what follows.  Evaluating the maker would produce the form,
 ;;; properly evaluated according to , and ,@ rules.
;;; I work backwards - it seemed a lot easier.  The reason for this is
 ;;; if I'm in some sort of a routine building a maker and I switch
 ;;; gears, it seemed to me easier to jump into some other state and
 ;;; glue what I've already done to the end, than to to prepare that
 ;;; something and go back to put things together.
(defun bq-make-maker (form)
  "Given one argument, a `mostly quoted' object, produces a maker.
See backquote.el for details"
  (let ((tailmaker (quote nil)) (qc 0) (ec 0) (state nil))
    (mapcar 'bq-iterative-list-builder (reverse form))
    (and state
	 (cond ((eq state 'quote)
		(list state tailmaker))
	       ((= (length tailmaker) 1)
		(funcall (bq-cadr (assq state bq-singles)) tailmaker))
	       (t (cons state tailmaker))))))

;;; There are exceptions - we wouldn't want to call append of one
 ;;; argument, for example.
(defconst bq-singles '((quote bq-quotecar)
		       (append car)
		       (list bq-make-list)
		       (cons bq-id)))

(defun bq-id (x) x)

(defun bq-quotecar (x) (list 'quote (car x)))

(defun bq-make-list (x) (cons 'list x))

;;; fr debugging use only
;(defun funcalll (a b) (funcall a b))
;(defun funcalll (a b) (debug nil 'enter state tailmaker a b)
;  (let ((ans (funcall a b))) (debug  nil 'leave state tailmaker)
;       ans))

;;; Given a state/tailmaker pair that already knows how to make a
 ;;; partial tail of the desired form, this function knows how to add
 ;;; yet another element to the burgening list.  There are four cases;
 ;;; the next item is an atom (which will certainly be quoted); a 
 ;;; (, xxx), which will be evaluated and put into the list at the top
 ;;; level; a (,@ xxx), which will be evaluated and spliced in, or
 ;;; some other list, in which case we first compute the form's maker,
 ;;; and then we either launch into the quoted case if the maker's
 ;;; top level function is quote, or into the comma case if it isn't.
;;; The fourth case reduces to one of the other three, so here we have
 ;;; a choice of three ways to build tailmaker, and cit turns out we
 ;;; use five possible values of state (although someday I'll add
 ;;; nconcto the possible values of state).
;;; This maintains the invariant that (cons state tailmaker) is the
 ;;; maker for the elements of the tail we've eaten so far.
(defun bq-iterative-list-builder (form)
  "Called by bq-make-maker.  Adds a new item form to tailmaker, 
changing state if need be, so tailmaker and state constitute a recipie
for making the list so far."
  (cond ((atom form)
	 (funcall (bq-cadr (assq state bq-quotefns)) form))
	((memq (car form) backquote-unquote)
	 (funcall (bq-cadr (assq state bq-evalfns)) (bq-cadr form)))
	((memq (car form) backquote-splice)
	 (funcall (bq-cadr (assq state bq-splicefns)) (bq-cadr form)))
	(t
	 (let ((newform (bq-make-maker form)))
	   (if (and (listp newform) (eq (car newform) 'quote))
	       (funcall (bq-cadr (assq state bq-quotefns)) (bq-cadr newform))
	     (funcall (bq-cadr (assq state bq-evalfns)) newform))))
	))

;;; We do a 2-d branch on the form of splicing and the old state.
 ;;; Here's fifteen functions' names...
(defconst bq-splicefns '((nil bq-splicenil)
			 (append bq-spliceappend)
			 (list bq-splicelist)
			 (quote bq-splicequote)
			 (cons bq-splicecons)))

(defconst bq-evalfns '((nil bq-evalnil)
		       (append bq-evalappend)
		       (list bq-evallist)
		       (quote bq-evalquote)
		       (cons bq-evalcons)))

(defconst bq-quotefns '((nil bq-quotenil)
			(append bq-quoteappend)
			(list bq-quotelist)
			(quote bq-quotequote)
			(cons bq-quotecons)))

;;; The name of each function is
 ;;; (concat 'bq- <type-of-element-addition> <old-state>)
;;; I'll comment the non-obvious ones before the definitions...
 ;;; In what follows, uppercase letters and form will always be
 ;;; metavariables that don't need commas in backquotes, and I will
 ;;; assume the existence of something like matches that takes a
 ;;; backquote-like form and a value, binds metavariables and returns
 ;;; t if the pattern match is successful, returns nil otherwise.  I
 ;;; will write such a goodie someday.

;;;   (setq tailmaker
 ;;;      (if (matches ((quote X) Y) tailmaker)
 ;;;          (` ((quote (form X)) Y))
 ;;;        (` ((list form (quote X)) Y))))
 ;;;  (setq state 'append)
(defun bq-quotecons (form)
  (if (and (listp (car tailmaker))
	   (eq (bq-caar tailmaker) 'quote))
      (setq tailmaker
	    (list (list 'quote (list form (bq-cadr (car tailmaker))))
		  (bq-cadr tailmaker))) 
    (setq tailmaker
	  (list (list 'list
		      (list 'quote form)
		      (car tailmaker))
		(bq-cadr tailmaker))))
  (setq state 'append))

(defun bq-quotequote (form)
  (bq-push form tailmaker))

;;; Could be improved to convert (list 'a 'b 'c .. 'w x) 
 ;;;                          to (append '(a b c .. w) x)
 ;;; when there are enough elements
(defun bq-quotelist (form)
  (bq-push (list 'quote form) tailmaker))

;;; (setq tailmaker
 ;;;  (if (matches ((quote X) (,@ Y)))
 ;;;      (` ((quote (, (cons form X))) (,@ Y)))))
(defun bq-quoteappend (form)
  (cond ((and (listp tailmaker)
	   (listp (car tailmaker))
	   (eq (bq-caar tailmaker) 'quote))
	 (rplaca (bq-cdar tailmaker)
		 (cons form (car (bq-cdar tailmaker)))))
	(t (bq-push (list 'quote (list form)) tailmaker))))

(defun bq-quotenil (form)
  (setq tailmaker (list form))
  (setq state 'quote))

;;; (if (matches (X Y) tailmaker)  ; it must
 ;;;    (` ((list form X) Y)))
(defun bq-evalcons (form)
  (setq tailmaker
	(list (list 'list form (car tailmaker))
	      (bq-cadr tailmaker)))
  (setq state 'append))

;;;  (if (matches (X Y Z (,@ W)))
 ;;;     (progn (setq state 'append)
 ;;;            (` ((list form) (quote (X Y Z (,@ W))))))
 ;;;     (progn (setq state 'list)
 ;;;            (list form 'X 'Y .. )))  ;  quote each one there is
(defun bq-evalquote (form)
  (cond ((< (length tailmaker) 3)
	 (setq tailmaker
	       (cons form
		     (mapcar (function (lambda (x)
					 (list 'quote x)))
			     tailmaker)))
	 (setq state 'list))
	(t
	 (setq tailmaker
	       (list (list 'list form)
		     (list 'quote tailmaker)))
	 (setq state 'append))))

(defun bq-evallist (form)
  (bq-push form tailmaker))

;;;  (cond ((matches ((list (,@ X)) (,@ Y)))
 ;;;        (` ((list form  (,@ X)) (,@ Y))))
 ;;;       ((matches (X))
 ;;;        (` (form (,@ X))) (setq state 'cons))
 ;;;       ((matches ((,@ X)))
 ;;;        (` (form (,@ X)))))
(defun bq-evalappend (form)
  (cond ((and (listp tailmaker)
	   (listp (car tailmaker))
	   (eq (bq-caar tailmaker) 'list))
	 (rplacd (car tailmaker)
		 (cons form (bq-cdar tailmaker))))
	((= (length tailmaker) 1)
	 (setq tailmaker (cons form tailmaker))
	 (setq state 'cons))
	(t (bq-push (list 'list form) tailmaker))))

(defun bq-evalnil (form)
  (setq tailmaker (list form))
  (setq state 'list))

;;; (if (matches (X Y))  ; it must
 ;;;    (progn (setq state 'append)
 ;;;           (` (form (cons X Y)))))   ; couldn't think of anything clever
(defun bq-splicecons (form)
  (setq tailmaker
	(list form
	      (list 'cons (car tailmaker) (bq-cadr tailmaker))))
  (setq state 'append))

(defun bq-splicequote (form)
  (setq tailmaker (list form (list 'quote (list tailmaker))))
  (setq state 'append))

(defun bq-splicelist (form)
  (setq tailmaker (list form (cons 'list tailmaker)))
  (setq state 'append))

(defun bq-spliceappend (form)
  (bq-push form tailmaker))

(defun bq-splicenil (form)
  (setq state 'append)
  (setq tailmaker (list form)))



