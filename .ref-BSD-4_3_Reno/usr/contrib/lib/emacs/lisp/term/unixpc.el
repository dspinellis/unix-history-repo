;;; AT&T UnixPC keyboard definitions
;;; Brant Cheikes (brant@linc.cis.upenn.edu, manta!brant)
;;; 4 August 1987
;;;
;;; Tested on: GNU Emacs 18.47.1 of Fri Jul 24 1987 on manta (usg-unix-v)
;;;
;;; The AT&T Unix PC (aka PC7300, 3B1) has a bizarre keyboard with
;;; lots of interestingly labeled function keys.  This file tries to
;;; assign useful actions to the function keys.  Note that the Shift
;;; and Ctrl keys have the same effect on function keys, so Shift-F1
;;; is the same as Ctrl-F1.
;;;
;;; Most of the information needed to create this file was taken from
;;; documentation found in lisp/keypad.el
;;;
;;; Bug: The "Beg" and "End" (unshifted) keys are not supported because
;;; they generate <esc>9 and <esc>0 respectively, and I know not how to
;;; deal with them.

(require 'keypad)

;;; There seem to be three prefixes for AT&T UnixPC function keys:
;;; "<esc>O", "<esc>N", and "<esc>[".  There seem to be a couple
;;; keys that just generate "<esc><digit>".
;;;
;;; Note: for each mapping, I indicate the key on the Unix PC followed
;;; by the Emacs command it is bound to (if any).  Note that when I
;;; couldn't figure out anything useful to do with a key, I simply bound
;;; it to 'previous-line, arbitrarily.  My goal was to get keys to do
;;; "mnemonic" things.

(defvar ATT-map-1 nil
  "The bulk of the function keys on the AT&T Unix PC.")
(defvar ATT-map-2 nil
  "A few other random function keys on the AT&T Unix PC.")
(defvar ATT-map-3 nil
  "Some really random function keys on the AT&T Unix PC.")

(defun enable-unixpc-keys ()
  "Enable the use of the AT&T Unix PC function keys.  Because of the
nature of the Unix PC, this unavoidably breaks several standard Emacs
prefixes; therefore, it is not done by default, but only if you give
this command."
  (interactive)
  (global-set-key "\eO" ATT-map-1)
  (global-set-key "\eN" ATT-map-2)
  (global-set-key "\e[" ATT-map-3))

;;; Create a few new keypad defaults.  Here's what I think I'm doing here:
;;; I look through "keypad.el" to find any unused entries in function-keymap
;;; and then create my own bindings for them here.  Then I use the newly
;;; created ?x string in the setup-terminal-keymap.

(keypad-default "2" 'advertised-undo)
(keypad-default "4" 'save-buffers-kill-emacs)
(keypad-default "5" 'save-buffer)
(keypad-default "6" 'beginning-of-buffer)
(keypad-default "8" 'end-of-buffer)
(keypad-default "w" 'kill-word)
(keypad-default "p" 'fill-paragraph)
(keypad-default "," 'copy-region-as-kill)

(if ATT-map-1
    nil
  (setq ATT-map-1 (make-keymap))   ; <ESC>O commands
  (setup-terminal-keymap ATT-map-1
			 '(("a" . ?\^d)	; Clear Line (kill-line)
			   ("A" . ?\^d)	; Shift-Clear Line (kill-line)
			   ("b" . ?u)	; Ref
			   ("B" . ?u)	; Rstrt
			   ("c" . ?u)	; F1
			   ("d" . ?u)	; F2
			   ("e" . ?u)	; F3
			   ("f" . ?u)	; F4
			   ("g" . ?u)	; F5
			   ("h" . ?u)	; F6
			   ("i" . ?u)	; F7
			   ("j" . ?u)	; F8
			   ("k" . ?4)	; Exit (save-buffers-kill-emacs)
			   ("K" . ?4)	; Shift-Exit (save-buffers-kill-emacs)
			   ("m" . ??)	; Help (help-command)
			   ("M" . ??)	; Shift-Help (help-command)
			   ("n" . ?u)	; Creat
			   ("N" . ?u)	; Shift-Creat
			   ("o" . ?5)	; Save (save-buffer)
			   ("O" . ?5)	; Shift-Save (save-buffer)
			   ("r" . ?u)	; Opts
			   ("R" . ?u)	; Shift-Opts
			   ("s" . ?2)	; Undo (advertised-undo)
			   ("S" . ?2)	; Shift-Undo (advertised-undo)
			   ("t" . ?p)	; Redo (fill-paragraph)
			   ("T" . ?p)	; Shift-Redo (fill-paragraph)
			   ("u" . ?u)	; Cmd
			   ("U" . ?u)	; Shift-Cmd
			   ("v" . ?e)	; Open (open-line)
			   ("V" . ?\^d)	; Close (kill-line)
			   ("w" . ?u)	; Cancl
			   ("W" . ?u)	; Shift-Cancl
			   ("x" . ?\^c) ; Find (isearch-forward)
			   ("X" . ?f)	; Shift-Find (re-search-forward)
			   ("y" . ?0)	; Rplac (yank)
			   ("Y" . ?0)	; Shift-Rplac (yank)
			   ("z" . ?u)	; Print
			   )))

(if ATT-map-2
    nil
  (setq ATT-map-2 (make-keymap))   ; <ESC>N commands
  (setup-terminal-keymap ATT-map-2
			 '(("a" . ?C)	; Rfrsh (recenter)
			   ("B" . ?6)	; Shift-Beg (beginning-of-buffer)
			   ("c" . ?0)	; Move (yank)
			   ("C" . ?0)	; Shift-Move (yank)
			   ("d" . ?,)	; Copy (copy-region-as-kill)
			   ("D" . ?,)	; Shift-Copy (copy-region-as-kill)
			   ("e" . ?k)   ; Dlete (kill-region)
			   ("E" . ?k)	; Shift-Dlete (kill-region)
			   ("f" . ?.)   ; Dlete Char (delete-char)
			   ("F" . ?w)	; Shift-Dlete Char (kill-word)
			   ("g" . ?P)	; Prev (scroll-down)
			   ("G" . ?P)	; Shift-Prev (scroll-down)
			   ("h" . ?N)	; Next (scroll-up)
			   ("H" . ?N)	; Shift-Next (scroll-up)
			   ("i" . ?s)	; Mark (set-mark-command)
			   ("I" . ?s)	; Slect (set-mark-command)
			   ("j" . ?u)	; Input Mode
			   ("J" . ?u)	; Shift-Input Mode
			   ("K" . ?1)	; Shift-LeftArrow (backward-word)
			   ("L" . ?3)	; Shift-RightArrow (forward-word)
			   ("M" . ?h)	; Shift-Home (move-to-window-line)
			   ("N" . ?8)	; Shift-End (end-of-buffer)
			   )))

(if ATT-map-3
    nil
  (setq ATT-map-3 (make-keymap))   ; <ESC>[ commands
  (setup-terminal-keymap ATT-map-3
			 '(("A" . ?u)	; Up Arrow (previous-line)
			   ("B" . ?d)	; Down Arrow (next-line)
			   ("C" . ?r)	; Right Arrow (forward-char)
			   ("D" . ?l)	; Left Arrow (backward-char)
			   ("H" . ?h)	; Home (move-to-window-line)
			   ("J" . ?C)	; Clear (recenter)
			   ("S" . ?9)	; Shift-DownArrow (forward-paragraph)
			   ("T" . ?7)	; Shift-UpArrow (backward-paragraph)
			   ("U" . ?N)	; Page (scroll-up)
			   ("V" . ?P)   ; Shift-Page (scroll-down)
			   )))
