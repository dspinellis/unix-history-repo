;;; hide-ifdef-mode.el   Hides selected code within ifdef.
;;;
;;; Copyright (C) 1988 Brian Marick and Daniel LaLiberte
;;; Written by Brian Marick, at Gould, Computer Systems Division, Urbana IL.
;;; Extensively modified by Daniel LaLiberte (while at Gould).
;;;
;;; You may freely modify and distribute this, but keep a record
;;; of modifications and send comments to:
;;; 	 liberte@a.cs.uiuc.edu  or  ihnp4!uiucdcs!liberte
;;; I will continue to upgrade hide-ifdef-mode
;;; with your contributions and will eventually offer it to FSF.
;;;
;;; $Header: hide-ifdef-mode.el,v 1.7 88/02/16 03:12:58 liberte Exp $
;;;
;;; $Log:	hide-ifdef-mode.el,v $
;;; Revision 1.7  88/02/16  03:12:58  liberte
;;; Fixed comments and doc strings.
;;; Added optional prefix arg for ifdef motion commands.
;;; 
;;; Revision 1.6  88/02/05  00:36:18  liberte
;;; Bug fixes.
;;; 1. A multi-line comment that starts on an #ifdef line
;;;    now ends on that line.
;;; 2. Fix bad function name: hide-hif-ifdef-toggle-read-only
;;; 3. Make ifdef-block hiding work outside of ifdefs.
;;; 
;;; Revision 1.5  88/01/31  23:19:31  liberte
;;; Major clean up.
;;;   Prefix internal names with "hif-".
;;; 
;;; Revision 1.4  88/01/30  14:09:38  liberte
;;; Add hide-ifdef-hiding and hide-ifdef-mode to minor-mode-alist.
;;; 
;;; Revision 1.3  88/01/29  00:38:19  liberte
;;; Fix three bugs.
;;; 1. Function "defined" is just like lookup.
;;; 2. Skip to newline or cr in case text is hidden.
;;; 3. Use car of token list if just one symbol.
;;;
;;; Revision 1.2  88/01/28  23:32:46  liberte
;;; Use hide-ifdef-mode-prefix-key.
;;; Copy current-local-map so other buffers do not get
;;; hide-ifdef-mode bindings.
;;;
;;;--------------------------------------------------------------
;;; To initialize, toggle the hide-ifdef minor mode with
;;;
;;; M-x hide-ifdef-mode
;;;
;;; This will set up key bindings and call hide-ifdef-mode-hook if it
;;; has a value.  To explicitly hide ifdefs using a buffer-local
;;; define list (default empty), type
;;;
;;; M-x hide-ifdefs  or C-c h
;;;
;;; Hide-ifdef suppresses the display of code that the preprocessor wouldn't
;;; pass through.  The support of constant expressions in #if lines is 
;;; limited to identifiers, parens, and the operators: &&, ||, !, and
;;; "defined".  Please extend this.
;;;
;;; The hidden code is marked by ellipses (...).  Be
;;; cautious when editing near ellipses, since the hidden text is
;;; still in the buffer, and you can move the point into it and modify
;;; text unawares.  If you don't want to see the ellipses, set 
;;; selective-display-ellipses to nil.  But this can be dangerous.
;;; You can make your buffer read-only while hide-ifdef-hiding by setting
;;; hide-ifdef-read-only to a non-nil value.  You can toggle this 
;;; variable with hide-ifdef-toggle-read-only (C-c C-q).
;;;
;;; You can undo the effect of hide-ifdefs by typing
;;;
;;; M-x show-ifdefs  or C-c s
;;;
;;; Use M-x hide-ifdef-define (C-c d) to define a symbol.
;;; Use M-x hide-ifdef-undef (C-c u) to undefine a symbol.
;;;
;;; If you define or undefine a symbol while hide-ifdef-mode is in effect,
;;; the display will be updated.  Only the define list for the current
;;; buffer will be affected.  You can save changes to the local define
;;; list with hide-ifdef-set-define-alist.  This adds entries 
;;; to hide-ifdef-define-alist.
;;;
;;; If you have defined a hide-ifdef-mode-hook, you can set
;;; up a list of symbols that may be used by hide-ifdefs as in the
;;; following example:
;;;
;;; (setq hide-ifdef-mode-hook
;;;      '(lambda ()
;;;	 (if (not hide-ifdef-define-alist)
;;;	     (setq hide-ifdef-define-alist
;;;		  '((list1 ONE TWO)
;;;		    (list2 TWO THREE)
;;;		    )))
;;;	 (hide-ifdef-use-define-alist 'list2) ; use list2 by default
;;;	 ))
;;;
;;; You can call hide-ifdef-use-define-alist (C-c u) at any time to specify
;;; another list to use.
;;;
;;; To cause ifdefs to be hidden as soon as hide-ifdef-mode is called,
;;; set hide-ifdef-initially to non-nil.
;;;
;;; If you set hide-ifdef-lines to t, hide-ifdefs hides all the #ifdef lines.
;;; In the absence of highlighting, that might be a bad idea.  If you set
;;; hide-ifdef-lines to nil (the default), the surrounding preprocessor
;;; lines will be displayed.  That can be confusing in its own
;;; right.  Other variations on display are possible, but not much
;;; better.
;;;
;;; You can explicitly hide or show individual ifdef blocks irrespective
;;; of the define list by using hide-ifdef-block and show-ifdef-block.
;;;
;;; You can move the point between ifdefs with forward-ifdef, backward-ifdef,
;;; up-ifdef, down-ifdef, next-ifdef, and previous-ifdef.
;;;
;;; If you have minor-mode-alist in your mode line (the default) two labels
;;; may appear.  "Ifdef" will appear when hide-ifdef-mode is active.  "Hiding"
;;; will appear when text may be hidden ("hide-ifdef-hiding" is non-nil).



(defvar hide-ifdef-mode-map nil
  "Keymap used with hide-ifdef mode")

(defconst hide-ifdef-mode-prefix-key "\C-c"
  "Prefix key for all hide-ifdef-mode commands.")

(defvar hide-ifdef-mode-map-before nil
  "Buffer-local variable to store a copy of the local keymap
	before hide-ifdef-mode modifies it.")

(defun define-hide-ifdef-mode-map ()
  (if hide-ifdef-mode-map
      ()				; dont redefine it.
    (setq hide-ifdef-mode-map (make-sparse-keymap))
    (define-key hide-ifdef-mode-map "d" 'hide-ifdef-define)
    (define-key hide-ifdef-mode-map "u" 'hide-ifdef-undef)
    (define-key hide-ifdef-mode-map "D" 'hide-ifdef-set-define-alist)
    (define-key hide-ifdef-mode-map "U" 'hide-ifdef-use-define-alist)
  
    (define-key hide-ifdef-mode-map "h" 'hide-ifdefs)
    (define-key hide-ifdef-mode-map "s" 'show-ifdefs)
    (define-key hide-ifdef-mode-map "\C-h" 'hide-ifdef-block)
    (define-key hide-ifdef-mode-map "\C-s" 'show-ifdef-block)
  
    (define-key hide-ifdef-mode-map "\C-f" 'forward-ifdef)
    (define-key hide-ifdef-mode-map "\C-b" 'backward-ifdef)
    (define-key hide-ifdef-mode-map "\C-d" 'down-ifdef)
    (define-key hide-ifdef-mode-map "\C-u" 'up-ifdef)
    (define-key hide-ifdef-mode-map "\C-n" 'next-ifdef)
    (define-key hide-ifdef-mode-map "\C-p" 'previous-ifdef)
    (define-key hide-ifdef-mode-map "\C-q" 'hide-ifdef-toggle-read-only)
    (define-key hide-ifdef-mode-map
      (where-is-internal 'toggle-read-only nil t)
      'hide-ifdef-toggle-outside-read-only)
    )
  (fset 'hide-ifdef-mode-map hide-ifdef-mode-map)  ; the function is the map
  )

(defun hif-update-mode-line ()
  "Update mode-line by setting buffer-modified to itself."
  (set-buffer-modified-p (buffer-modified-p)))


(defvar hide-ifdef-mode nil
  "non-nil when hide-ifdef-mode is activated.")

(defvar hide-ifdef-hiding nil
  "non-nil when text may be hidden.")

(or (assq 'hide-ifdef-hiding minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-hiding " Hiding")
                minor-mode-alist)))

(or (assq 'hide-ifdef-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-mode " Ifdef")
                minor-mode-alist)))


(defun hide-ifdef-mode (arg)
  "Toggle hide-ifdef-mode.  Thus this is a minor mode, albeit a large one.
With arg, turn hide-ifdef-mode on iff arg is positive.
In hide-ifdef-mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of hide-ifdef-env is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.  
        Use hide-ifdef-set-define-alist to save the current hide-ifdef-env
        and hide-ifdef-use-define-alist to set the current hide-ifdef-env
        from one of the lists in hide-ifdef-define-alist.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether hide-ifdefs should be called when hide-ifdef-mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After show-ifdefs, read-only status is restored to previous value.

\\{hide-ifdef-mode-map}"

  (interactive "P")
  (make-local-variable 'hide-ifdef-mode)
  (setq hide-ifdef-mode
	(if (null arg)
	    (not hide-ifdef-mode)
	  (> (prefix-numeric-value arg) 0)))
  
  (hif-update-mode-line)

  (if hide-ifdef-mode
      (progn
	; fix c-mode syntax table so we can recognize whole symbols.
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?\| ".")

	; inherit global values
	(make-local-variable 'hide-ifdef-env)
	(setq hide-ifdef-env (default-value 'hide-ifdef-env))

	(make-local-variable 'hide-ifdef-hiding)
	(setq hide-ifdef-hiding (default-value 'hide-ifdef-hiding))

	(make-local-variable 'hif-outside-read-only)
	(setq hif-outside-read-only buffer-read-only)

	(make-local-variable 'ide-ifdef-mode-map-before)
	(setq hide-ifdef-mode-map-before (current-local-map))
	(use-local-map (copy-keymap (current-local-map)))
	(local-unset-key hide-ifdef-mode-prefix-key)
	(local-set-key hide-ifdef-mode-prefix-key 'hide-ifdef-mode-map)
	(define-hide-ifdef-mode-map)

	(run-hooks 'hide-ifdef-mode-hook)

	(if hide-ifdef-initially
	    (hide-ifdefs)
	  (show-ifdefs))
	(message "Enter hide-ifdef-mode.")
	)
     ; else end hide-ifdef-mode
    (if hide-ifdef-hiding
	(show-ifdefs))
    (use-local-map hide-ifdef-mode-map-before)
    (message "Exit hide-ifdef-mode.")
    ))
  

;; from outline.el with docstring fixed.
(defun hif-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is \\n (newline character) then text is shown, while if FLAG is \\^M
\(control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect (progn
		      (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t) )
      (set-buffer-modified-p modp))
    ))

(defun hif-show-all ()
  "Show all of the text in the current buffer."
  (interactive)
  (hif-outline-flag-region (point-min) (point-max) ?\n))

(defun hide-ifdef-region (start end)
  "START is the start of a #if or #else form.  END is the ending part.
Everything including these lines is made invisible."
  (hif-outline-flag-region start end ?\^M)
  )

(defun hif-show-ifdef-region (start end)
  "Everything between START and END is made visible."
  (hif-outline-flag-region start end ?\n)
  )



;===%%SF%% evaluation (Start)  ===

(defvar hide-ifdef-evaluator 'eval
  "The evaluator is given a canonical form and returns T if text under
that form should be displayed.")

(defvar hif-undefined-symbol nil
  "...is by default considered to be false.")

(defvar hide-ifdef-env nil
  "An alist of defined symbols and their values.")


(defun hif-set-var (var value)
  "Prepend (var value) pair to hide-ifdef-env."
  (setq hide-ifdef-env (cons (cons var value) hide-ifdef-env)))


(defun hif-lookup (var)
;  (message "hif-lookup %s" var)
  (let ((val (assoc var hide-ifdef-env)))
    (if val
	(cdr val)
      hif-undefined-symbol)))

(defun hif-defined (var)
  (hif-lookup var)
  ; when #if expressions are fully supported, defined result should be 1
  ;  (if (assoc var  hide-ifdef-env)
  ;      1
  ;    nil)
)


;===%%SF%% evaluation (End)  ===



;===%%SF%% parsing (Start)  ===
;;;  The code that understands what ifs and ifdef in files look like.

(defconst hif-cpp-prefix "\\(^\\|\r\\)[ \t]*#[ \t]*")
(defconst hif-ifndef-regexp (concat hif-cpp-prefix "ifndef"))
(defconst hif-ifx-regexp (concat hif-cpp-prefix "if\\(n?def\\)?[ \t]+"))
(defconst hif-else-regexp (concat hif-cpp-prefix "else"))
(defconst hif-endif-regexp (concat hif-cpp-prefix "endif"))
(defconst hif-ifx-else-endif-regexp
  (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp))


(defun hif-infix-to-prefix (token-list)
  "Convert list of tokens in infix into prefix list"
;  (message "hif-infix-to-prefix: %s" token-list)
  (if (= 1 (length token-list))
      (` (hif-lookup (quote (, (car token-list)))))
    (hif-parse-if-exp token-list))
  )

; pattern to match initial identifier, !, &&, ||, (, or ).
(defconst hif-token-regexp "^\\(!\\|&&\\|||\\|[()]\\|\\w+\\)")
(defconst hif-end-of-comment "\\*/")


(defun hif-tokenize (expr-string)
  "Separate string into a list of tokens"
  (let ((token-list nil)
	(expr-start 0)
	(expr-length (length expr-string)))

    (while (< expr-start expr-length) 
;      (message "expr-start = %d" expr-start) (sit-for 1)
      (cond
	((string-match "^[ \t]+" expr-string expr-start)
	   ; skip whitespace
	 (setq expr-start (match-end 0))
	 ; stick newline in string so ^ matches on the next string-match
	 (aset expr-string (1- expr-start) ?\n)
	 )

	((string-match "^/\\*" expr-string expr-start)
	 (setq expr-start (match-end 0))
	 (aset expr-string (1- expr-start) ?\n)
	 (or
	   (string-match hif-end-of-comment
			 expr-string expr-start) ; eat comment
	   (string-match "$" expr-string expr-start)) ; multi-line comment
	 (setq expr-start (match-end 0))
	 (aset expr-string (1- expr-start) ?\n)
	 )

	((string-match hif-token-regexp expr-string expr-start)
	  (let ((token (substring expr-string expr-start (match-end 0))))
	    (setq expr-start (match-end 0))
	    (aset expr-string (1- expr-start) ?\n)
;	    (message "token: %s" token) (sit-for 1)
	    (setq token-list
		  (cons
		    (cond
		      ((string-equal token "||") 'or)
		      ((string-equal token "&&") 'and)
		      ((string-equal token "!")  'not)
		      ((string-equal token "defined") 'hif-defined)
		      ((string-equal token "(") 'lparen)
		      ((string-equal token ")") 'rparen)
		      (t (intern token)))
		    token-list))
	    ))
	  (t (error "Bad #if expression: %s" expr-string))
	  ))
    (nreverse token-list)
    ))

;;;-----------------------------------------------------------------
;;; Translate C preprocessor #if expressions using recursive descent.
;;; This parser is limited to the operators &&, ||, !, and "defined".

(defun hif-parse-if-exp (token-list)
  "Parse the TOKEN-LIST.  Return translated list in prefix form."
  (hif-nexttoken)
  (prog1
      (hif-expr)
    (if token ; is there still a token?
	(error "Error: unexpected token: %s" token)))
  )

(defun hif-nexttoken ()
  "Pop the next token from token-list into the let variable \"token\"."
  (setq token (car token-list))
  (setq token-list (cdr token-list))
  token
  )

(defun hif-expr ()
  "Parse and expression of the form
       expr : term | expr '||' term."
  (let ((result (hif-term)))
    (while (eq  token 'or)
      (hif-nexttoken)
      (setq result (list 'or result (hif-term))))
  result
  ))

(defun hif-term ()
  "Parse a term of the form
       term : factor | term '&&' factor."
  (let ((result (hif-factor)))
    (while (eq token 'and)
      (hif-nexttoken)
      (setq result (list 'and result (hif-factor))))
    result
    ))

(defun hif-factor ()
  "Parse a factor of the form
       factor : '!' factor | '(' expr ')' | 'defined(' id ')' | id."
  (cond
    ((eq token 'not)
     (hif-nexttoken)
     (list 'not (hif-factor)))

    ((eq token 'lparen)
     (hif-nexttoken)
     (let ((result (hif-expr)))
       (if (not (eq token 'rparen))
	   (error "Bad token in parenthesized expression: %s" token)
	 (hif-nexttoken)
	 result)))

    ((eq token 'hif-defined)
     (hif-nexttoken)
     (if (not (eq token 'lparen))
	 (error "Error: expected \"(\" after \"define\""))
     (hif-nexttoken)
     (let ((ident token))
       (if (memq token '(or and not hif-defined lparen rparen))
	   (error "Error: unexpected token: %s" token))
       (hif-nexttoken)
       (if (not (eq token 'rparen))
	   (error "Error: expected \")\" after identifier"))
       (hif-nexttoken)
       (` (hif-defined (quote (, ident))))
       ))

    (t ; identifier
      (let ((ident token))
	(if (memq ident '(or and))
	    (error "Error: missing identifier"))
	(hif-nexttoken)
	(` (hif-lookup (quote (, ident))))
	))

    ))

;;;----------- end of parser -----------------------


(defun hif-canonicalize ()
  "When at beginning of #ifX, returns a canonical (evaluatable)
       form for the expression."
  (save-excursion
    (let ((negate (looking-at hif-ifndef-regexp)))
      (re-search-forward hif-ifx-regexp)
      (let* ((expr-string
	      (buffer-substring (point)
				(progn (skip-chars-forward "^\n\r") (point))))
	     (expr (hif-infix-to-prefix (hif-tokenize expr-string))))
;	(message "hif-canonicalized: %s" expr)
	(if negate
	    (list 'not expr)
	  expr)))))


(defun hif-find-any-ifX ()
  "Position at beginning of next #if, #ifdef, or #ifndef, including one on
this line."
;  (message "find ifX at %d" (point))
  (prog1
      (re-search-forward hif-ifx-regexp (point-max) t)
    (beginning-of-line)))


(defun hif-find-next-relevant ()
  "Position at beginning of next #ifdef, #ifndef, #else, #endif,
NOT including one on this line."
;  (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-forward hif-ifx-else-endif-regexp (point-max) t)
      (beginning-of-line))
  )

(defun hif-find-previous-relevant ()
  "Position at beginning of previous #ifdef, #ifndef, #else, #endif,
NOT including one on this line."
;  (message "hif-find-previous-relevant at %d" (point))
  (beginning-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-backward hif-ifx-else-endif-regexp (point-min) t)
     (beginning-of-line)
    )
  )


(defun hif-looking-at-ifX ()		;; Should eventually see #if
  (looking-at hif-ifx-regexp))
(defun hif-looking-at-endif ()
  (looking-at hif-endif-regexp))
(defun hif-looking-at-else ()
  (looking-at hif-else-regexp))



(defun hif-ifdef-to-endif ()
  "If positioned at #ifX or #else form, skip to corresponding #endif."
;  (message "hif-ifdef-to-endif at %d" (point)) (sit-for 1)
  (hif-find-next-relevant)
  (cond ((hif-looking-at-ifX)
	 (hif-ifdef-to-endif) ; find endif of nested if
	 (hif-ifdef-to-endif)) ; find outer endif or else
	((hif-looking-at-else)
	 (hif-ifdef-to-endif)) ; find endif following else
	((hif-looking-at-endif)
	 'done)
	(t
	 (error "Missmatched #ifdef #endif pair"))
	))


(defun hif-endif-to-ifdef ()
  "If positioned at #endif form, skip backward to corresponding #ifX."
;  (message "hif-endif-to-ifdef at %d" (point))
  (let ((start (point)))
    (hif-find-previous-relevant)
    (if (= start (point))
	(error "Missmatched #ifdef #endif pair")))
  (cond ((hif-looking-at-endif)
	 (hif-endif-to-ifdef) ; find beginning of nested if
	 (hif-endif-to-ifdef)) ; find beginning of outer if or else
	((hif-looking-at-else)
	 (hif-endif-to-ifdef))
	((hif-looking-at-ifX)
	 'done)
	(t ; never gets here
	 )))


(defun forward-ifdef (&optional arg)
  "Move point to beginning of line of the next ifdef-endif.
       With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (backward-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (- arg))
    (let ((start (point)))
      (if (not (hif-looking-at-ifX))
	  (hif-find-next-relevant))
      (if (hif-looking-at-ifX)
	  (hif-ifdef-to-endif)
	(goto-char start)
	(error "No following #ifdef")
	))))


(defun backward-ifdef (&optional arg)
  "Move point to beginning of the previous ifdef-endif.
       With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (forward-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (beginning-of-line)
    (let ((start (point)))
      (if (not (hif-looking-at-endif))
	  (hif-find-previous-relevant))
      (if (hif-looking-at-endif)
	  (hif-endif-to-ifdef)
	(goto-char start)
	(error "No previous #ifdef")
	))))



(defun down-ifdef ()
  "Move point to beginning of nested ifdef or else-part."
    (interactive)
    (let ((start (point)))
      (hif-find-next-relevant)
      (if (or (hif-looking-at-ifX) (hif-looking-at-else))
	  ()
	(goto-char start)
	(error "No following #ifdef")
	)))


(defun up-ifdef ()
  "Move point to beginning of enclosing ifdef or else-part."
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (if (not (hif-looking-at-endif))
	(hif-find-previous-relevant))
    (if (hif-looking-at-endif)
	(hif-endif-to-ifdef))
      (if (= start (point))
	  (error "No previous #ifdef")
	)))

(defun next-ifdef (&optional arg)
  "Move to the beginning of the next #ifX, #else, or #endif.
       With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (previous-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (hif-find-next-relevant)
    (if (eolp)
	(progn
	  (beginning-of-line)
	  (error "No following #ifdefs, #elses, or #endifs")
	  ))))

(defun previous-ifdef (&optional arg)
  "Move to the beginning of the previous #ifX, #else, or #endif.
       With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (next-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (let ((start (point)))
      (hif-find-previous-relevant)
      (if (= start (point))
	  (error "No previous #ifdefs, #elses, or #endifs")
	))))


;===%%SF%% parsing (End)  ===


;===%%SF%% hide-ifdef-hiding (Start)  ===


;;; A range is a structure with four components:
;;; ELSE-P	True if there was an else clause for the ifdef.
;;; START	The start of the range. (beginning of line)
;;; ELSE	The else marker (beginning of line)
;;;			Only valid if ELSE-P is true.
;;; END		The end of the range.  (beginning of line)

(defun hif-make-range (else-p start end &optional else)
  (list else-p start else end))

(defun hif-range-else-p (range)  (elt range 0))
(defun hif-range-start (range) (elt range 1))
(defun hif-range-else (range) (elt range 2))
(defun hif-range-end (range) (elt range 3))



;;; Find-Range
;;; The workhorse, it delimits the #if region.  Reasonably simple:
;;; Skip until an #else or #endif is found, remembering positions.  If
;;; an #else was found, skip some more, looking for the true #endif.

(defun hif-find-range ()
  "Returns a Range structure describing the current #if region.
Point is left unchanged."
;  (message "hif-find-range at %d" (point))
  (save-excursion
    (beginning-of-line)
    (let ((start (point))
	  (else-p nil)
	  (else nil)
	  (end nil))
      ;; Part one.  Look for either #endif or #else.
      ;; This loop-and-a-half dedicated to E. Dijkstra.
      (hif-find-next-relevant)
      (while (hif-looking-at-ifX)		; Skip nested ifdef
	(hif-ifdef-to-endif)
	(hif-find-next-relevant))
      ;; Found either a #else or an #endif.
      (cond ((hif-looking-at-else)
	     (setq else-p t)
	     (setq else (point)))
	    (t
	     (setq end (point)) ; (save-excursion (end-of-line) (point))
	     ))
      ;; If found #else, look for #endif.
      (if else-p
	  (progn
	    (hif-find-next-relevant)
	    (while (hif-looking-at-ifX)	; Skip nested ifdef
	      (hif-ifdef-to-endif)
	      (hif-find-next-relevant))
	    (if (hif-looking-at-else)
		(error "Found two elses in a row?  Broken!"))
	    (setq end (point))  ; (save-excursion (end-of-line) (point))
	    ))
      (hif-make-range else-p start end else))))

	  
;;; A bit slimy.
;;; NOTE:  If there's an #ifdef at the beginning of the file, we can't
;;; hide it.  There's no previous newline to replace.  If we added
;;; one, we'd throw off all the counts.  Feh.

(defun hif-hide-line (point)
  "Hide the line containing point.  Does nothing if
hide-ifdef-lines is nil."
  (if hide-ifdef-lines
      (save-excursion
	(goto-char point)
	(let ((modp (buffer-modified-p)))
	  (unwind-protect
	      (progn
		(beginning-of-line)
		(if (not (= (point) 1))
		    (hide-ifdef-region (1- (point)) (point))))
	    (set-buffer-modified-p modp))
	  ))
    ))
		  

;;;  Hif-Possibly-Hide
;;;  There are four cases.  The #ifX expression is "taken" if it
;;;  the hide-ifdef-evaluator returns T.  Presumably, this means the code
;;;  inside the #ifdef would be included when the program was
;;;  compiled.  
;;;
;;;  Case 1:  #ifX taken, and there's an #else.
;;;	The #else part must be hidden.  The #if (then) part must be
;;;	processed for nested #ifX's.
;;;  Case 2:  #ifX taken, and there's no #else.
;;;	The #if part must be processed for nested #ifX's.
;;;  Case 3:  #ifX not taken, and there's an #else.
;;;	The #if part must be hidden.  The #else part must be processed
;;;	for nested #ifs.
;;;  Case 4:  #ifX not taken, and there's no #else.
;;;	The #ifX part must be hidden.
;;;
;;;  Further processing is done by narrowing to the relevant region
;;;  and just recursively calling hide-ifdef-guts.
;;;
;;;  When hif-possibly-hide returns, point is at the end of the
;;;  possibly-hidden range.

(defun hif-recurse-on (start end)
  "Call hide-ifdef-guts after narrowing to end of START line and END
line."
  (save-excursion
    (save-restriction
      (goto-char start)
      (end-of-line)
      (narrow-to-region (point) end)
      (hide-ifdef-guts))))

(defun hif-possibly-hide ()
  "Called at #ifX expression, this hides those parts that should be
hidden, according to judgement of hide-ifdef-evaluator."
;  (message "hif-possibly-hide") (sit-for 1)
    (let ((test (hif-canonicalize))
	  (range (hif-find-range)))
;      (message "test = %s" test) (sit-for 1)
      
      (hif-hide-line (hif-range-end range))
      (if (funcall hide-ifdef-evaluator test)
	  (cond ((hif-range-else-p range) ; case 1
		 (hif-hide-line (hif-range-else range))
		 (hide-ifdef-region (hif-range-else range) 
				    (1- (hif-range-end range)))
		 (hif-recurse-on (hif-range-start range)
				 (hif-range-else range)))
		(t ; case 2
		 (hif-recurse-on (hif-range-start range)
				 (hif-range-end range))))
	(cond ((hif-range-else-p range) ; case 3
	       (hif-hide-line (hif-range-else range))
	       (hide-ifdef-region (hif-range-start range)
				  (1- (hif-range-else range)))
	       (hif-recurse-on (hif-range-else range)
			       (hif-range-end range)))
	      (t ; case 4
	       (hide-ifdef-region (point)
				  (1- (hif-range-end range))))
	      ))
      (hif-hide-line (hif-range-start range))	; Always hide start.
      (goto-char (hif-range-end range))
      (end-of-line)
      ))



(defun hide-ifdef-guts ()
  "Does the work of hide-ifdefs, except for the work that's pointless
to redo on a recursive entry."
;  (message "hide-ifdef-guts")
  (save-excursion
    (goto-char (point-min))
    (while (hif-find-any-ifX)
      (hif-possibly-hide))))

;===%%SF%% hide-ifdef-hiding (End)  ===


;===%%SF%% exports (Start)  ===

(defvar hide-ifdef-initially nil
  "*Non-nil if hide-ifdefs should be called when hide-ifdef-mode
	is first activated.")

(defvar hide-ifdef-hiding nil
  "Non-nil if text might be hidden.")

(defvar hide-ifdef-read-only nil
  "*Set to non-nil if you want buffer to be read-only while hiding text.")

(defvar hif-outside-read-only nil
  "Internal variable.  Saves the value of buffer-read-only while hiding.")

(defvar hide-ifdef-lines nil
  "*Set to t if you don't want to see the #ifX, #else, and #endif lines.")

(defun hide-ifdef-toggle-read-only ()
  "Toggle hide-ifdef-read-only."
  (interactive)
  (setq hide-ifdef-read-only (not hide-ifdef-read-only))
  (message "Hide-Read-Only %s"
	   (if hide-ifdef-read-only "ON" "OFF"))
  (if hide-ifdef-hiding
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))
  (hif-update-mode-line)
  )

(defun hide-ifdef-toggle-outside-read-only ()
  "Replacement for toggle-read-only within hide-ifdef-mode."
  (interactive)
  (setq hif-outside-read-only (not hif-outside-read-only))
  (message "Read only %s"
	   (if hif-outside-read-only "ON" "OFF"))
  (setq buffer-read-only
	(or (and hide-ifdef-hiding hide-ifdef-read-only)
	    hif-outside-read-only)
	)
  (hif-update-mode-line)
  )

      
(defun hide-ifdef-define (var)
  "Define a VAR so that #ifdef VAR would be included."
  (interactive "SDefine what? ")
  (hif-set-var var t)
  (if hide-ifdef-hiding (hide-ifdefs)))

(defun hide-ifdef-undef (var)
  "Undefine a VAR so that #ifdef VAR would not be included."
  (interactive "SUndefine what? ")
  (hif-set-var var nil)
  (if hide-ifdef-hiding (hide-ifdefs)))


(defun hide-ifdefs ()
  "Hide the contents of some #ifdefs.  Assume that defined symbols have
been added to hide-ifdef-env.  The text hidden is the text that would not
be included by the C preprocessor if it were given the file with those
symbols defined.

Turn off hiding by calling show-ifdef."

  (interactive)
  (message "Hiding...")
  (if (not hide-ifdef-mode)
      (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
  (if hide-ifdef-hiding
      (show-ifdefs))			; Otherwise, deep confusion.
  (if buffer-read-only (toggle-read-only)) ; make it writable temporarily
  (setq selective-display t)
  (setq hide-ifdef-hiding t)
  (hide-ifdef-guts)
  (if (or hide-ifdef-read-only hif-outside-read-only)
      (toggle-read-only) ; make it read only
    )
  (message "Hiding done")
  )


(defun show-ifdefs ()
  "Cancel the effects of hide-ifdef.  The contents of all #ifdefs is shown."
  (interactive)
  (if buffer-read-only (toggle-read-only)) ; make it writable temporarily
  (setq selective-display nil)	; defaults
  (hif-show-all)
  (if hif-outside-read-only
      (toggle-read-only)) ; make it read only
  (setq hide-ifdef-hiding nil)
  )


(defun hif-find-ifdef-block ()
  "Utilitiy for hide and show ifdef-block.  Set top and bottom of ifdef block."
  (let (max-bottom)
  (save-excursion
    (beginning-of-line)
    (if (not (or (hif-looking-at-else) (hif-looking-at-ifX)))
	(up-ifdef))
    (setq top (point))
    (hif-ifdef-to-endif)
    (setq max-bottom (1- (point)))
    )
  (save-excursion
    (beginning-of-line)
    (if (not (hif-looking-at-endif))
	(hif-find-next-relevant))
    (while (hif-looking-at-ifX)
      (hif-ifdef-to-endif)
      (hif-find-next-relevant)
      )
    (setq bottom (min max-bottom (1- (point))))
    ))
  )


(defun hide-ifdef-block ()
  "Hide the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (if (not hide-ifdef-mode)
      (hide-ifdef-mode 1))
  (if buffer-read-only (toggle-read-only))
  (setq selective-display t)
  (let (top bottom)
    (hif-find-ifdef-block) ; set top and bottom - dynamic scoping
    (hide-ifdef-region top bottom)
    (if hide-ifdef-lines
	(progn
	  (hif-hide-line top)
	  (hif-hide-line (1+ bottom))))
    (setq hide-ifdef-hiding t)
    )
  (if (or hide-ifdef-read-only hif-outside-read-only)
      (toggle-read-only))
  )


(defun show-ifdef-block ()
  "Show the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (let ((old-read-only buffer-read-only))
    (if old-read-only (toggle-read-only))
    (if hide-ifdef-lines
	(save-excursion
	  (beginning-of-line)
	  (hif-show-ifdef-region (1- (point)) (progn (end-of-line) (point))))

      (let (top bottom)
	(hif-find-ifdef-block)
	(hif-show-ifdef-region (1- top) bottom))
      )

    ; restore read only status since we dont know if all is shown.
    (if old-read-only (toggle-read-only))
    ))



;;;  defininition alist support

(defvar hide-ifdef-define-alist nil
  "A global assoc list of pre-defined symbol lists")

(defun hif-compress-define-list (env)
  "Compress the define list ENV into a list of defined symbols only."
  (let ((defs (mapcar '(lambda (arg)
			 (if (hif-lookup (car arg)) (car arg)))
		      env))
	(new-defs nil))
    (while defs
      (if (car defs)
	  (setq new-defs (cons (car defs) new-defs)))
      (setq defs (cdr defs)))
    new-defs
    ))

(defun hide-ifdef-set-define-alist (name)
  "Set the association for NAME to hide-ifdef-env."
  (interactive "SSet define list: ")
  (setq hide-ifdef-define-alist
	(cons (cons name (hif-compress-define-list hide-ifdef-env))
	      hide-ifdef-define-alist))
  )

(defun hide-ifdef-use-define-alist (name)
  "Set hide-ifdef-env to the define list specified by NAME."
  (interactive "SUse define list: ")
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
	(setq hide-ifdef-env
	      (mapcar '(lambda (arg) (cons arg t))
		      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (hide-ifdefs))
    )
  )

;===%%SF%% exports (End)  ===
