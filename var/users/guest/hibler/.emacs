(defvar matching-paren-delay 1
  "How long to display a matching paren for show-matching-paren.")

;(setq load-path (list "/a/guest/hibler/emacs/lisp"))

(setq inhibit-startup-message 't)
(setq echo-keystrokes 5)
(setq default-case-fold-search 'nil)
(setq make-backup-files 'nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

(global-set-key "\b" 'backward-char)
(global-set-key "\eg" 'goto-line)
(global-set-key "\e?" 'help-command)
(global-set-key "\C-x\C-z" 'save-buffers-kill-emacs)
(global-set-key "\e\C-r" 'query-replace-regexp)
(global-set-key "\e\C-l" 'my-recenter)
(global-set-key "\e)" 'show-matching-paren)
(defun my-recenter ()
  "Place current line at top of window."
  (interactive)
  (recenter 0))
(defun show-matching-paren (arg)
  "Show the paren matching the paren under point.  Leaves mark at match
if found.  If ARG is non-zero, point is left at matching paren and mark
at original."
  (interactive "p")
  (let (oldpoint (point))
    (push-mark oldpoint t)
    (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
	  ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
	  (t (ding)))
    (if (not (eq (point) oldpoint))
	(if (= arg 1)
	    (progn
	      (sit-for matching-paren-delay)
	      (exchange-point-and-mark))))))

;
; Name completion stuff from Leigh:
; I like '-', '.', and '_' to be considered parts of words so
; dabbrevs can expand them correctly, and so filename-expansion skips
; over them. This does present a problem with *, but so be it.

(modify-syntax-entry ?- "w   " lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w   " lisp-mode-syntax-table)

(modify-syntax-entry ?- "w   " emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w   " emacs-lisp-mode-syntax-table)

(modify-syntax-entry ?- "w   " c-mode-syntax-table)
(modify-syntax-entry ?. "w   " c-mode-syntax-table)
(modify-syntax-entry ?_ "w   " c-mode-syntax-table)

(modify-syntax-entry ?- "w   " text-mode-syntax-table)
(modify-syntax-entry ?. "w   " text-mode-syntax-table)
(modify-syntax-entry ?_ "w   " text-mode-syntax-table)

(modify-syntax-entry ?- "w   " (standard-syntax-table))
(modify-syntax-entry ?. "w   " (standard-syntax-table))
(modify-syntax-entry ?_ "w   " (standard-syntax-table))

;
; This switches tab and space so space will be the one that completes as
; far as possible, which is the one we usually want. 
;
(define-key minibuffer-local-must-match-map "\040" 'minibuffer-complete)
(define-key minibuffer-local-must-match-map "\011" 'minibuffer-complete-word)
(define-key minibuffer-local-completion-map "\040" 'minibuffer-complete)
(define-key minibuffer-local-completion-map "\011" 'minibuffer-complete-word)

;
; C stuff
;
(setq c-mode-hook 'C-HOOK)
(defun C-HOOK ()
  (setq c-indent-level 8)
  (setq c-continued-statement-offset 8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8))
;
; Terminal stuff
;
(if (equal (getenv "TERM") "300h")
    (setq meta-flag t))
;
; X stuff
;
(setq DISPLAY (getenv "DISPLAY"))
(cond (DISPLAY
       (load "term/x-win")  ; Have to load this for Gnu to work in X.
       (load "x-mouse")
       (load "utah/mouse")  ; Load in mouse hooks, Works GREAT.
;      (x-create-x-window "=80x58+0+0")
       (defun big-window ()
	 "Expand to a maximum sized X-window for the HP-bobcat"
	 (interactive)
	 (x-set-font "6x10")
	 (x-set-window-edges 168 75 0 0))
       (defun unbig-window ()
	 "Go back to a normal sized X-window for the HP-bobcat"
	 (interactive)
	 (x-set-font "8x13")
	 (x-set-window-edges 80 58 0 0))))
;
; GDB stuff
;
(setq kgdb-command-name "/nvmbin/gdb")
(autoload 'kgdb "gdb"
  "\
Run gdb on kernel FILE and image file CORE in buffer *kgdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  t)
