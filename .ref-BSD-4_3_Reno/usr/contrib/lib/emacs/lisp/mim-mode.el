;; Mim (MDL in MDL) mode.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


(provide 'mim-mode)

(autoload 'fast-syntax-check-mim "mim-syntax"
	  "Checks Mim syntax quickly.
Answers correct or incorrect, cannot point out the error context."
	  t)

(autoload 'slow-syntax-check-mim "mim-syntax"
	  "Check Mim syntax slowly.
Points out the context of the error, if the syntax is incorrect."
	  t)

(defvar mim-mode-hysterical-bindings t
  "*Non-nil means bind list manipulation commands to Meta keys as well as
Control-Meta keys for historical reasons.  Otherwise, only the latter keys
are bound.")

(defvar mim-mode-map nil)

(defvar mim-mode-syntax-table nil)

(if mim-mode-syntax-table
    ()
  (let ((i -1))
    (setq mim-mode-syntax-table (make-syntax-table))
    (while (< i ?\ )
      (modify-syntax-entry (setq i (1+ i)) "    " mim-mode-syntax-table))
    (while (< i 127)
      (modify-syntax-entry (setq i (1+ i)) "_   " mim-mode-syntax-table))
    (setq i (1- ?a))
    (while (< i ?z)
      (modify-syntax-entry (setq i (1+ i)) "w   " mim-mode-syntax-table))
    (setq i (1- ?A))
    (while (< i ?Z)
      (modify-syntax-entry (setq i (1+ i)) "w   " mim-mode-syntax-table))
    (setq i (1- ?0))
    (while (< i ?9)
      (modify-syntax-entry (setq i (1+ i)) "w   " mim-mode-syntax-table))
    (modify-syntax-entry ?:  "     " mim-mode-syntax-table) ; make : symbol delimiter
    (modify-syntax-entry ?,  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?.  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?'  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?`  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?~  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?\; "'    " mim-mode-syntax-table) ; comments are prefixed objects
    (modify-syntax-entry ?#  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?%  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?!  "'    " mim-mode-syntax-table)
    (modify-syntax-entry ?\" "\"   " mim-mode-syntax-table)
    (modify-syntax-entry ?\\ "\\   " mim-mode-syntax-table)
    (modify-syntax-entry ?\( "\()  " mim-mode-syntax-table)
    (modify-syntax-entry ?\< "\(>  " mim-mode-syntax-table)
    (modify-syntax-entry ?\{ "\(}  " mim-mode-syntax-table)
    (modify-syntax-entry ?\[ "\(]  " mim-mode-syntax-table)
    (modify-syntax-entry ?\) "\)(  " mim-mode-syntax-table)
    (modify-syntax-entry ?\> "\)<  " mim-mode-syntax-table)
    (modify-syntax-entry ?\} "\){  " mim-mode-syntax-table)
    (modify-syntax-entry ?\] "\)[  " mim-mode-syntax-table)))

(defconst mim-whitespace "\000- ")

(defvar mim-mode-hook nil
  "*User function run after mim mode initialization.  Usage:
\(setq mim-mode-hook '(lambda () ... your init forms ...)).")

(define-abbrev-table 'mim-mode-abbrev-table nil)

(defconst indent-mim-hook 'indent-mim-hook
  "Controls (via properties) indenting of special forms.
\(put 'FOO 'indent-mim-hook n\), integer n, means lines inside
<FOO ...> will be indented n spaces from start of form.
\(put 'FOO 'indent-mim-hook 'DEFINE\) is like above but means use
value of mim-body-indent as offset from start of form.
\(put 'FOO 'indent-mim-hook <cons>\) where <cons> is a list or pointted list
of integers, means indent each form in <FOO ...> by the amount specified
in <cons>.  When <cons> is exhausted, indent remaining forms by
mim-body-indent unless <cons> is a pointted list, in which case the last
cdr is used.  Confused? Here is an example:
\(put 'FROBIT 'indent-mim-hook '\(4 2 . 1\)\)
<FROBIT
     <CHOMP-IT>
   <CHOMP-SOME-MORE>
  <DIGEST>
  <BELCH>
  ...>
Finally, the property can be a function name (read the code).")

(defvar indent-mim-comment t
  "*Non-nil means indent string comments.")

(defvar mim-body-indent 2
  "*Amount to indent in special forms which have DEFINE property on
indent-mim-hook.")

(defvar indent-mim-arglist t
  "*nil means indent arglists like ordinary lists.
t means strings stack under start of arglist and variables stack to
right of them.  Otherwise, strings stack under last string (or start
of arglist if none) and variables stack to right of them.
Examples (for values 'stack, t, nil):

\(FOO \"OPT\" BAR             \(FOO \"OPT\" BAR            \(FOO \"OPT\" BAR
           BAZ MUMBLE                 BAZ MUMBLE      BAZ MUMBLE
     \"AUX\"                  \"AUX\"                     \"AUX\"
     BLETCH ...             BLETCH ...                BLETCH ...")

(put 'DEFINE 'indent-mim-hook 'DEFINE)
(put 'DEFMAC 'indent-mim-hook 'DEFINE)
(put 'BIND 'indent-mim-hook 'DEFINE)
(put 'PROG 'indent-mim-hook 'DEFINE)
(put 'REPEAT 'indent-mim-hook 'DEFINE)
(put 'CASE 'indent-mim-hook 'DEFINE)
(put 'FUNCTION 'indent-mim-hook 'DEFINE)
(put 'MAPF 'indent-mim-hook 'DEFINE)
(put 'MAPR 'indent-mim-hook 'DEFINE)
(put 'UNWIND 'indent-mim-hook (cons (* 2 mim-body-indent) mim-body-indent))

(defvar mim-down-parens-only t
  "*nil means treat ADECLs and ATOM trailers like structures when
moving down a level of structure.")

(defvar mim-stop-for-slop t
  "*Non-nil means {next previous}-mim-object consider any
non-whitespace character in column 0 to be a toplevel object, otherwise
only open paren syntax characters will be considered.")

(fset 'mdl-mode 'mim-mode)

(defun mim-mode ()
  "Major mode for editing Mim (MDL in MDL) code.
Commands:
    If value of mim-mode-hysterical-bindings is non-nil, then following
commands are assigned to escape keys as well (e.g. M-f = M-C-f).
The default action is bind the escape keys.
  Tab        Indents the current line as MDL code.
  Delete     Converts tabs to spaces as it moves back.
  M-C-f      Move forward over next mim object.
  M-C-b      Move backward over previous mim object.
  M-C-p      Move to beginning of previous toplevel mim object.
  M-C-n      Move to the beginning of the next toplevel mim object.
  M-C-a      Move to the top of surrounding toplevel mim form.
  M-C-e      Move to the end of surrounding toplevel mim form.
  M-C-u      Move up a level of mim structure backwards.
  M-C-d      Move down a level of mim structure forwards.
  M-C-t      Transpose mim objects on either side of point.
  M-C-k      Kill next mim object.
  M-C-h      Place mark at end of next mim object.
  M-C-o      Insert a newline before current line and indent.
  M-Delete   Kill previous mim object.
  M-^        Join current line to previous line.
  M-\\        Delete whitespace around point.
  M-;        Move to existing comment or insert empty comment if none.
  M-Tab      Indent following mim object and all contained lines.
Other Commands:
  Use \\[describe-function] to obtain documentation.
  replace-in-mim-object  find-mim-definition  fast-syntax-check-mim
  slow-syntax-check-mim  backward-down-mim-object  forward-up-mim-object
Variables:
  Use \\[describe-variable] to obtain documentation.
  mim-mode-hook  indent-mim-comment  indent-mim-arglist  indent-mim-hook
  mim-body-indent  mim-down-parens-only  mim-stop-for-slop
  mim-mode-hysterical-bindings
Entry to this mode calls the value of mim-mode-hook if non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not mim-mode-map)
      (progn
	(setq mim-mode-map (make-sparse-keymap))
	(define-key mim-mode-map "\e\^o" 'open-mim-line)
	(define-key mim-mode-map "\e\^q" 'indent-mim-object)
	(define-key mim-mode-map "\e\^p" 'previous-mim-object)
	(define-key mim-mode-map "\e\^n" 'next-mim-object)
	(define-key mim-mode-map "\e\^a" 'beginning-of-DEFINE)
	(define-key mim-mode-map "\e\^e" 'end-of-DEFINE)
	(define-key mim-mode-map "\e\^t" 'transpose-mim-objects)
	(define-key mim-mode-map "\e\^u" 'backward-up-mim-object)	
	(define-key mim-mode-map "\e\^d" 'forward-down-mim-object)	 
	(define-key mim-mode-map "\e\^h" 'mark-mim-object)
	(define-key mim-mode-map "\e\^k" 'forward-kill-mim-object)	
	(define-key mim-mode-map "\e\^f" 'forward-mim-object)	   
	(define-key mim-mode-map "\e\^b" 'backward-mim-object)
	(define-key mim-mode-map "\e^" 'raise-mim-line)
	(define-key mim-mode-map "\e\\" 'fixup-whitespace)
	(define-key mim-mode-map "\177" 'backward-delete-char-untabify)
	(define-key mim-mode-map "\e\177" 'backward-kill-mim-object)
	(define-key mim-mode-map "\^j" 'newline-and-mim-indent)
	(define-key mim-mode-map "\e;" 'begin-mim-comment)
	(define-key mim-mode-map "\t" 'indent-mim-line)
	(define-key mim-mode-map "\e\t" 'indent-mim-object)
	(if (not mim-mode-hysterical-bindings)
	    nil
	  ;; i really hate this but too many people are accustomed to these.
	  (define-key mim-mode-map "\e!" 'line-to-top-of-window)
	  (define-key mim-mode-map "\eo" 'open-mim-line)
	  (define-key mim-mode-map "\ep" 'previous-mim-object)
	  (define-key mim-mode-map "\en" 'next-mim-object)
	  (define-key mim-mode-map "\ea" 'beginning-of-DEFINE)
	  (define-key mim-mode-map "\ee" 'end-of-DEFINE)
	  (define-key mim-mode-map "\et" 'transpose-mim-objects)
	  (define-key mim-mode-map "\eu" 'backward-up-mim-object)
	  (define-key mim-mode-map "\ed" 'forward-down-mim-object)
	  (define-key mim-mode-map "\ek" 'forward-kill-mim-object)
	  (define-key mim-mode-map "\ef" 'forward-mim-object)
	  (define-key mim-mode-map "\eb" 'backward-mim-object))))
  (use-local-map mim-mode-map)
  (set-syntax-table mim-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  ;; Most people use string comments.
  (make-local-variable 'comment-start)
  (setq comment-start ";\"")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";\"")
  (make-local-variable 'comment-end)
  (setq comment-end "\"")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'indent-mim-comment)
  ;; tell generic indenter how to indent.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-mim-line)
  ;; look for that paren
  (make-local-variable 'blink-matching-paren-distance)
  (setq blink-matching-paren-distance nil)
  ;; so people who dont like tabs can turn them off locally in indenter.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode t)
  (setq local-abbrev-table mim-mode-abbrev-table)
  (setq major-mode 'mim-mode)
  (setq mode-name "Mim")
  (run-hooks 'mim-mode-hook))

(defun line-to-top-of-window ()
  "Move current line to top of window."
  (interactive)				; for lazy people
  (recenter 0))

(defun forward-mim-object (arg)
  "Move forward across Mim object.
With ARG, move forward that many objects."
  (interactive "p")
  ;; this function is wierd because it emulates the behavior of the old
  ;; (gosling) mim-mode - if the arg is 1 and we are `inside' an ADECL,
  ;; more than one character into the ATOM part and not sitting on the
  ;; colon, then we move to the DECL part (just past colon) instead of
  ;; the end of the object (the entire ADECL).  otherwise, ADECL's are
  ;; atomic objects.  likewise for ATOM trailers.
  (if (= (abs arg) 1)
      (if (inside-atom-p)
	  ;; Move to end of ATOM or to trailer (!) or to ADECL (:).
	  (forward-sexp arg)
	;; Either scan an sexp or move over one bracket.
	(forward-mim-objects arg t))
    ;; in the multi-object case, don't perform any magic.
    ;; treats ATOM trailers and ADECLs atomically, stops at unmatched
    ;; brackets with error.
    (forward-mim-objects arg)))

(defun inside-atom-p ()
  ;; Returns t iff inside an atom (takes account of trailers)
  (let ((c1 (preceding-char))
	(c2 (following-char)))
    (and (or (= (char-syntax c1) ?w) (= (char-syntax c1) ?_) (= c1 ?!))
	 (or (= (char-syntax c2) ?w) (= (char-syntax c2) ?_) (= c2 ?!)))))

(defun forward-mim-objects (arg &optional skip-bracket-p)
  ;; Move over arg objects ignoring ADECLs and trailers.  If
  ;; skip-bracket-p is non-nil, then move over one bracket on error.
  (let ((direction (sign arg)))
    (condition-case conditions
	(while (/= arg 0)
	  (forward-sexp direction)
	  (if (not (inside-adecl-or-trailer-p direction))
	      (setq arg (- arg direction))))
      (error (if (not skip-bracket-p)
		 (signal 'error (cdr conditions))
	       (skip-mim-whitespace direction)
	       (goto-char (+ (point) direction)))))
    ;; If we moved too far move back to first interesting character.
    (if (= (point) (buffer-end direction)) (skip-mim-whitespace (- direction)))))
				    
(defun backward-mim-object (&optional arg)
  "Move backward across Mim object.
With ARG, move backward that many objects."
  (interactive "p")
  (forward-mim-object (if arg (- arg) -1)))

(defun mark-mim-object (&optional arg)
  "Mark following Mim object.
With ARG, mark that many following (preceding, ARG < 0) objects."
  (interactive "p")
  (push-mark (save-excursion (forward-mim-object (or arg 1)) (point))))

(defun forward-kill-mim-object (&optional arg)
  "Kill following Mim object.
With ARG, kill that many objects."
  (interactive "*p")
  (kill-region (point) (progn (forward-mim-object (or arg 1)) (point))))

(defun backward-kill-mim-object (&optional arg)
  "Kill preceding Mim object.
With ARG, kill that many objects."
  (interactive "*p")
  (forward-kill-mim-object (- (or arg 1))))

(defun raise-mim-line (&optional arg)
  "Raise following line, fixing up whitespace at join.
With ARG raise that many following lines.
A negative ARG will raise current line and previous lines."
  (interactive "*p")
  (let* ((increment (sign (or arg (setq arg 1))))
	 (direction (if (> arg 0) 1 0)))
    (save-excursion
      (while (/= arg 0)
	;; move over eol and kill it
	(forward-line direction)
	(delete-region (point) (1- (point)))
	(fixup-whitespace)
	(setq arg (- arg increment))))))

(defun forward-down-mim-object (&optional arg)
  "Move down a level of Mim structure forwards.
With ARG, move down that many levels forwards (backwards, ARG < 0)."
  (interactive "p")
  ;; another wierdo - going down `inside' an ADECL or ATOM trailer
  ;; depends on the value of mim-down-parens-only.  if nil, treat
  ;; ADECLs and trailers as structured objects.
  (let ((direction (sign (or arg (setq arg 1)))))
    (if (and (= (abs arg) 1) (not mim-down-parens-only))
	(goto-char
	  (save-excursion
	    (skip-mim-whitespace direction)
	    (if (> direction 0) (re-search-forward "\\s'*"))
	    (or (and (let ((c (next-char direction)))
		       (or (= (char-syntax c) ?_)
			   (= (char-syntax c) ?w)))
		     (progn (forward-sexp direction)
			    (if (inside-adecl-or-trailer-p direction)
				(point))))
		(scan-lists (point) direction -1)
		(buffer-end direction))))
      (while (/= arg 0)
	(goto-char (or (scan-lists (point) direction -1) (buffer-end direction)))
	(setq arg (- arg direction))))))

(defun backward-down-mim-object (&optional arg)
  "Move down a level of Mim structure backwards.
With ARG, move down that many levels backwards (forwards, ARG < 0)."
  (interactive "p")
  (forward-down-mim-object (if arg (- arg) -1)))

(defun forward-up-mim-object (&optional arg)
  "Move up a level of Mim structure forwards
With ARG, move up that many levels forwards (backwards, ARG < 0)."
  (interactive "p")
  (let ((direction (sign (or arg (setq arg 1)))))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) direction 1) (buffer-end arg)))
      (setq arg (- arg direction)))
    (if (< direction 0) (backward-prefix-chars))))

(defun backward-up-mim-object (&optional arg)
  "Move up a level of Mim structure backwards
With ARG, move up that many levels backwards (forwards, ARG > 0)."
  (interactive "p")
  (forward-up-mim-object (if arg (- arg) -1)))

(defun replace-in-mim-object (old new)
  "Replace string in following Mim object."
  (interactive "*sReplace in object: \nsReplace %s with: ")
  (save-restriction
    (narrow-to-region (point) (save-excursion (forward-mim-object 1) (point)))
    (replace-string old new)))
  
(defun transpose-mim-objects (&optional arg)
  "Transpose Mim objects around point.
With ARG, transpose preceding object that many times with following objects.
A negative ARG will transpose backwards."
  (interactive "*p")
  (transpose-subr 'forward-mim-object (or arg 1)))

(defun beginning-of-DEFINE (&optional arg move)
  "Move backward to beginning of surrounding or previous toplevel Mim form.
With ARG, do it that many times.  Stops at last toplevel form seen if buffer
end is reached."
  (interactive "p")
  (let ((direction (sign (or arg (setq arg 1)))))
    (if (not move) (setq move t))
    (if (< direction 0) (goto-char (1+ (point))))
    (while (and (/= arg 0) (re-search-backward "^<" nil move direction))
      (setq arg (- arg direction)))
    (if (< direction 0)
	(goto-char (1- (point))))))

(defun end-of-DEFINE (&optional arg)
  "Move forward to end of surrounding or next toplevel mim form.
With ARG, do it that many times.  Stops at end of last toplevel form seen
if buffer end is reached."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (if (< arg 0)
      (beginning-of-DEFINE (- (1- arg)))
    (if (not (looking-at "^<")) (setq arg (1+ arg)))
    (beginning-of-DEFINE (- arg) 'move)
    (beginning-of-DEFINE 1))
  (forward-mim-object 1)
  (forward-line 1))

(defun next-mim-object (&optional arg)
  "Move to beginning of next toplevel Mim object.
With ARG, do it that many times.  Stops at last object seen if buffer end
is reached."
  (interactive "p")
  (let ((search-string (if mim-stop-for-slop "^\\S " "^\\s("))
	(direction (sign (or arg (setq arg 1)))))
    (if (> direction 0)
	(goto-char (1+ (point))))		; no error if end of buffer
    (while (and (/= arg 0)
		(re-search-forward search-string nil t direction))
      (setq arg (- arg direction)))
    (if (> direction 0)
	(goto-char (1- (point))))		; no error if beginning of buffer
    ;; scroll to top of window if moving forward and end not visible.
    (if (not (or (< direction 0)
		 (save-excursion (forward-mim-object 1)
				 (pos-visible-in-window-p (point)))))
	(recenter 0))))

(defun previous-mim-object (&optional arg)
  "Move to beginning of previous toplevel Mim object.
With ARG do it that many times.  Stops at last object seen if buffer end
is reached."
  (interactive "p")
  (next-mim-object (- (or arg 1))))

(defun calculate-mim-indent (&optional parse-start)
  "Calculate indentation for Mim line.  Returns column."
  (save-excursion			; some excursion, huh, toto?
    (beginning-of-line)
    (let ((indent-point (point)) retry state containing-sexp last-sexp
	  desired-indent start peek where paren-depth)
      (if parse-start
	  (goto-char parse-start)	; should be containing environment
	(catch 'from-the-top
	  ;; find a place to start parsing.  going backwards is fastest.
	  ;; forward-sexp signals error on encountering unmatched open.
	  (setq retry t)
	  (while retry
	    (condition-case nil (forward-sexp -1) (error (setq retry nil)))
	    (if (looking-at ".?[ \t]*\"")
		;; cant parse backward in presence of strings, go forward.
		(progn
		  (goto-char indent-point)
		  (re-search-backward "^\\s(" nil 'move 1)  ; to top of object
		  (throw 'from-the-top nil)))
	    (setq retry (and retry (/= (current-column) 0))))
	  (skip-chars-backward mim-whitespace)
	  (if (not (bobp)) (forward-char -1))     ; onto unclosed open
	  (backward-prefix-chars)))
      ;; find outermost containing sexp if we started inside an sexp.
      (while (< (point) indent-point)    
	(setq state (parse-partial-sexp (point) indent-point 0)))
      ;; find usual column to indent under (not in string or toplevel).
      ;; on termination, state will correspond to containing environment
      ;; (if retry is nil), where will be position of character to indent
      ;; under normally, and desired-indent will be the column to indent to
      ;; except if inside form, string, or at toplevel.  point will be in
      ;; in column to indent to unless inside string.
      (setq retry t)
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
	;; find innermost containing sexp.
	(setq retry nil)
	(setq last-sexp (car (nthcdr 2 state)))
	(setq containing-sexp (car (cdr state)))
	(goto-char (1+ containing-sexp))	  ; to last unclosed open
	(if (and last-sexp (> last-sexp (point)))
	    ;; is the last sexp a containing sexp?
	    (progn (setq peek (parse-partial-sexp last-sexp indent-point 0))
		   (if (setq retry (car (cdr peek))) (setq state peek))))
	(if retry
	    nil
	  (setq where (1+ containing-sexp))   ; innermost containing sexp
	  (goto-char where)
	  (cond
	   ((not last-sexp)		      ; indent-point after bracket
	    (setq desired-indent (current-column)))
	   ((= (preceding-char) ?\<)	      ; it's a form
	    (cond ((> (progn (forward-sexp 1) (point)) last-sexp)
		   (goto-char where))	      ; only one frob
		  ((> (save-excursion (forward-line 1) (point)) last-sexp)
		   (skip-chars-forward " \t") ; last-sexp is on same line
		   (setq where (point)))	      ; as containing-sexp
		  ((progn
		     (goto-char last-sexp)
		     (beginning-of-line)
		     (parse-partial-sexp (point) last-sexp 0 t)
		     (or (= (point) last-sexp)
			 (save-excursion
			   (= (car (parse-partial-sexp (point) last-sexp 0))
			      0))))
		   (backward-prefix-chars)    ; last-sexp 1st on line or 1st
		   (setq where (point)))        ; frob on that line level 0
		  (t (goto-char where))))     ; punt, should never occur
	   ((and indent-mim-arglist	      ; maybe hack arglist    
		 (= (preceding-char) ?\()     ; its a list
		 (save-excursion	      ; look for magic atoms
		   (setq peek 0)	      ; using peek as counter
		   (forward-char -1)	      ; back over containing paren
		   (while (and (< (setq peek (1+ peek)) 6)
			       (condition-case nil
				   (progn (forward-sexp -1) t)
				 (error nil))))
		   (and (< peek 6) (looking-at "DEFINE\\|DEFMAC\\|FUNCTION"))))
	    ;; frobs stack under strings they belong to or under first
	    ;; frob to right of strings they belong to unless luser has
	    ;; frob (non-string) on preceding line with different
	    ;; indentation.  strings stack under start of arglist unless
	    ;; mim-indent-arglist is not t, in which case they stack
	    ;; under the last string, if any, else the start of the arglist.
	    (let ((eol 0) last-string)
	      (while (< (point) last-sexp)      ; find out where the strings are
		(skip-chars-forward mim-whitespace last-sexp)		
		(if (> (setq start (point)) eol)
		    (progn                    ; simultaneously keeping track
		      (setq where (min where start))
		      (end-of-line)	      ; of indentation of first frob
		      (setq eol (point))	      ; on each line
		      (goto-char start)))
		(if (= (following-char) ?\")
		    (progn (setq last-string (point))
			   (forward-sexp 1)
			   (if (= last-string last-sexp)
			       (setq where last-sexp)
			     (skip-chars-forward mim-whitespace last-sexp)
			     (setq where (point))))
		  (forward-sexp 1)))
	      (goto-char indent-point)	           ; if string is first on
	      (skip-chars-forward " \t" (point-max)) ; line we are indenting, it 
	      (if (= (following-char) ?\")         ; goes under arglist start
		  (if (and last-string (not (equal indent-mim-arglist t)))
		      (setq where last-string)     ; or under last string.
		    (setq where (1+ containing-sexp)))))
	    (goto-char where)
	    (setq desired-indent (current-column)))
	   (t				      ; plain vanilla structure
	    (cond ((> (save-excursion (forward-line 1) (point)) last-sexp)
		   (skip-chars-forward " \t") ; last-sexp is on same line
		   (setq where (point)))	      ; as containing-sexp
		  ((progn
		     (goto-char last-sexp)
		     (beginning-of-line)
		     (parse-partial-sexp (point) last-sexp 0 t)
		     (or (= (point) last-sexp)
			 (save-excursion
			   (= (car (parse-partial-sexp (point) last-sexp 0))
			      0))))
		     (backward-prefix-chars)  ; last-sexp 1st on line or 1st
		     (setq where (point)))      ; frob on that line level 0
		  (t (goto-char where)))      ; punt, should never occur
	    (setq desired-indent (current-column))))))
      ;; state is innermost containing environment unless toplevel or string.
      (if (car (nthcdr 3 state))	      ; inside string
	  (progn
	    (if last-sexp		      ; string must be next
		(progn (goto-char last-sexp)
		       (forward-sexp 1)
		       (search-forward "\"")
		       (forward-char -1))
	      (goto-char indent-point)	      ; toplevel string, look for it
	      (re-search-backward "[^\\]\"")
	      (forward-char 1))
	    (setq start (point))		      ; opening double quote
	    (skip-chars-backward " \t")
	    (backward-prefix-chars)
	    ;; see if the string is really a comment.
       	    (if (and (looking-at ";[ \t]*\"") indent-mim-comment)
		;; it's a comment, line up under the start unless disabled.
		(goto-char (1+ start))
	      ;; it's a string, dont mung the indentation.
	      (goto-char indent-point)
	      (skip-chars-forward " \t"))
	    (setq desired-indent (current-column))))
      ;; point is sitting in usual column to indent to and if retry is nil
      ;; then state corresponds to containing environment.  if desired
      ;; indentation not determined, we are inside a form, so call hook.
      (or desired-indent
	  (and indent-mim-hook
	       (not retry)
	       (setq desired-indent
		     (funcall indent-mim-hook state indent-point)))
	  (setq desired-indent (current-column)))
      (goto-char indent-point)		; back to where we started
      desired-indent)))			; return column to indent to

(defun indent-mim-hook (state indent-point)
  "Compute indentation for Mim special forms.  Returns column or nil."
  (let ((containing-sexp (car (cdr state))) (current-indent (point)))
    (save-excursion
      (goto-char (1+ containing-sexp))
      (backward-prefix-chars)
      ;; make sure we are looking at a symbol.  if so, see if it is a special
      ;; symbol.  if so, add the special indentation to the indentation of
      ;; the start of the special symbol, unless the property is not
      ;; an integer and not nil (in this case, call the property, it must
      ;; be a function which returns the appropriate indentation or nil and
      ;; does not change the buffer).
      (if (looking-at "\\sw\\|\\s_")
	  (let* ((start (current-column))
		 (function
		  (intern-soft (buffer-substring (point)
						 (progn (forward-sexp 1)
							(point)))))
		 (method (get function 'indent-mim-hook)))
	    (if (or (if (equal method 'DEFINE) (setq method mim-body-indent))
		    (integerp method))
		;; only use method if its first line after containing-sexp.
		;; we could have done this in calculate-mim-indent, but someday
		;; someone might want to format frobs in a special form based
		;; on position instead of indenting uniformly (like lisp if),
		;; so preserve right for posterity.  if not first line,
		;; calculate-mim-indent already knows right indentation -
		;; give luser chance to change indentation manually by changing
		;; 1st line after containing-sexp.
		(if (> (progn (forward-line 1) (point)) (car (nthcdr 2 state)))
		    (+ method start))
	      (goto-char current-indent)
	      (if (consp method)
		  ;; list or pointted list of explicit indentations
		  (indent-mim-offset state indent-point)
		(if (and (symbolp method) (fboundp method))
		    ;; luser function - s/he better know what's going on.
		    ;; should take state and indent-point as arguments - for
		    ;; description of state, see parse-partial-sexp
		    ;; documentation the function is guaranteed the following:
		    ;; (1) state describes the closest surrounding form,
		    ;; (2) indent-point is the beginning of the line being
		    ;; indented, (3) point points to char in column that would
		    ;; normally be used for indentation, (4) function is bound
		    ;; to the special ATOM.  See indent-mim-offset for example
		    ;; of a special function.
		    (funcall method state indent-point)))))))))

(defun indent-mim-offset (state indent-point)
  ;; offset forms explicitly according to list of indentations.
  (let ((mim-body-indent mim-body-indent)
	(indentations (get function 'indent-mim-hook))
	(containing-sexp (car (cdr state)))
	(last-sexp (car (nthcdr 2 state)))
	indentation)
    (goto-char (1+ containing-sexp))
    ;; determine wheich of the indentations to use.
    (while (and (< (point) indent-point)
		(condition-case nil
		    (progn (forward-sexp 1)
			   (parse-partial-sexp (point) indent-point 1 t))
		  (error nil)))
      (skip-chars-backward " \t")
      (backward-prefix-chars)
      (if (= (following-char) ?\;)
	  nil					    ; ignore comments
	(setq indentation (car indentations))
	(if (integerp (setq indentations (cdr indentations)))
	    ;; if last cdr is integer, that is indentation to use for all
	    ;; all the rest of the forms.
	    (progn (setq mim-body-indent indentations)
		   (setq indentations nil)))))
    (goto-char (1+ containing-sexp))
    (+ (current-column) (or indentation mim-body-indent))))

(defun indent-mim-comment (&optional start)
  "Indent a one line (string) Mim comment following object, if any."
  (let* ((old-point (point)) (eol (progn (end-of-line) (point))) state last-sexp)
    ;; this function assumes that comment indenting is enabled.  it is caller's
    ;; responsibility to check the indent-mim-comment flag before calling.
    (beginning-of-line)
    (catch 'no-comment
      (setq state (parse-partial-sexp (point) eol))
      ;; determine if there is an existing regular comment.  a `regular'
      ;; comment is defined as a commented string which is the last thing
      ;; on the line and does not extend beyond the end of the line.
      (if (or (not (setq last-sexp (car (nthcdr 2 state))))
	      (car (nthcdr 3 state)))
	  ;; empty line or inside string (multiple line).
	  (throw 'no-comment nil))	
      ;; could be a comment, but make sure its not the only object.
      (beginning-of-line)
      (parse-partial-sexp (point) eol 0 t)
      (if (= (point) last-sexp)
	  ;; only one object on line
	  (throw 'no-comment t))
      (goto-char last-sexp)
      (skip-chars-backward " \t")
      (backward-prefix-chars)
      (if (not (looking-at ";[ \t]*\""))
	  ;; aint no comment
	  (throw 'no-comment nil))
      ;; there is an existing regular comment
      (delete-horizontal-space)
      ;; move it to comment-column if possible else to tab-stop
      (if (< (current-column) comment-column)
	  (indent-to comment-column)
	(tab-to-tab-stop)))
    (goto-char old-point)))
	
(defun indent-mim-line ()
  "Indent line of Mim code."
  (interactive "*")
  (let* ((position (- (point-max) (point)))
	 (bol (progn (beginning-of-line) (point)))
	 (indent (calculate-mim-indent)))
    (skip-chars-forward " \t")
    (if (/= (current-column) indent)
	(progn (delete-region bol (point)) (indent-to indent)))
    (if (> (- (point-max) position) (point)) (goto-char (- (point-max) position)))))

(defun newline-and-mim-indent ()
  "Insert newline at point and indent."
  (interactive "*")
  ;; commented code would correct indentation of line in arglist which
  ;; starts with string, but it would indent every line twice.  luser can
  ;; just say tab after typing string to get same effect.
  ;(if indent-mim-arglist (indent-mim-line))
  (newline)
  (indent-mim-line))

(defun open-mim-line (&optional lines)
  "Insert newline before point and indent.
With ARG insert that many newlines."
  (interactive "*p")
  (beginning-of-line)
  (let ((indent (calculate-mim-indent)))
    (while (> lines 0)
      (newline)
      (forward-line -1)
      (indent-to indent)
      (setq lines (1- lines)))))

(defun indent-mim-object (&optional dont-indent-first-line)
  "Indent object following point and all lines contained inside it.
With ARG, idents only contained lines (skips first line)."
  (interactive "*P")
  (let (end bol indent start)
    (save-excursion (parse-partial-sexp (point) (point-max) 0 t)
		    (setq start (point))
		    (forward-sexp 1)
		    (setq end (- (point-max) (point))))
    (save-excursion
      (if (not dont-indent-first-line) (indent-mim-line))
      (while (progn (forward-line 1) (> (- (point-max) (point)) end))
	(setq indent (calculate-mim-indent start))
	(setq bol (point))
	(skip-chars-forward " \t")
	(if (/= indent (current-column))
	    (progn (delete-region bol (point)) (indent-to indent)))
	(if indent-mim-comment (indent-mim-comment))))))
  
(defun find-mim-definition (name)
  "Search for definition of function, macro, or gfcn.
You need type only enough of the name to be unambiguous."
  (interactive "sName: ")
  (let (where)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
	  (progn
	    (re-search-forward
	     (concat "^<\\(DEFINE\\|\\DEFMAC\\|FCN\\|GFCN\\)\\([ \t]*\\)"
		     name))
	    (setq where (point)))
	(error (error "Can't find %s" name))))
    (if where
	(progn (push-mark)
	       (goto-char where)
	       (beginning-of-line)
	       (recenter 0)))))
    
(defun begin-mim-comment ()
  "Move to existing comment or insert empty comment."
  (interactive "*")
  (let* ((eol (progn (end-of-line) (point)))
	 (bol (progn (beginning-of-line) (point))))
    ;; check for existing comment first.
    (if (re-search-forward ";[ \t]*\"" eol t)
	;; found it.  indent if desired and go there.
	(if indent-mim-comment
	    (let ((where (- (point-max) (point))))
	      (indent-mim-comment)
	      (goto-char (- (point-max) where))))
      ;; nothing there, make a comment.
      (let (state last-sexp)
	;; skip past all the sexps on the line
	(goto-char bol)
	(while (and (equal (car (setq state (parse-partial-sexp (point) eol 0)))
			   0)
		    (car (nthcdr 2 state)))
	  (setq last-sexp (car (nthcdr 2 state))))
	(if (car (nthcdr 3 state))
	    nil					    ; inside a string, punt
      (delete-region (point) eol)			    ; flush trailing whitespace
      (if (and (not last-sexp) (equal (car state) 0))
	  (indent-to (calculate-mim-indent))	    ; empty, indent like code
	(if (> (current-column) comment-column)	    ; indent to comment column
	    (tab-to-tab-stop)			    ; unless past it, else to
	  (indent-to comment-column)))		    ; tab-stop
      ;; if luser changes comment-{start end} to something besides semi
      ;; followed by zero or more whitespace characters followed by string
      ;; delimiters, the code above fails to find existing comments, but as
      ;; taa says, `let the losers lose'.
      (insert comment-start)
      (save-excursion (insert comment-end)))))))

(defun skip-mim-whitespace (direction)
  (if (>= direction 0)
      (skip-chars-forward mim-whitespace (point-max))
    (skip-chars-backward mim-whitespace (point-min))))

(defun inside-adecl-or-trailer-p (direction)
  (if (>= direction 0)
      (looking-at ":\\|!-")
    (or (= (preceding-char) ?:)
	(looking-at "!-"))))
		  
(defun sign (n)
  "Returns -1 if N < 0, else 1."
  (if (>= n 0) 1 -1))

(defun abs (n)
  "Returns the absolute value of N."
  (if (>= n 0) n (- n)))

(defun next-char (direction)
  "Returns preceding-char if DIRECTION < 0, otherwise following-char."
  (if (>= direction 0) (following-char) (preceding-char)))
