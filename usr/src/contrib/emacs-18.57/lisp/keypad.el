;; Terminal-independent keypad and function key bindings.
;; Copyright (C) 1986 Free Software Foundation, Inc.

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


;; These keys are handled by a two-level process.
;; The first level, terminal-dependent, maps input sequences
;; into the function keys that they represent.
;; The second level, terminal-independent but customized by users,
;; map function keys into meanings.

;; This file takes care of the second level of mapping.
;; The first, terminal-dependent, level is handled by the
;; terminal-specific files term/*.el.

;; The second-level mapping is done by a keymap, function-keymap.
;; Here we document the meanings of the "characters" defined by
;; function-keymap.

;; What do these letters mean?
;; When we say that ``a stands for the clear-all-tabs key'',
;; we mean that you should attach to the letter `a' in function-keymap
;; whatever command you want to be executed when you type the
;; clear-all-tabs key on any terminal.  The terminal-dependent
;; files will attempt to make this work.  If a terminal has no
;; clear-all-tabs key that can be recognized, it makes no difference
;; what binding you give to `a' in function-keymap.

;; a -- clear all tabs key
;; c -- erase key
;; d -- down-arrow
;; e -- enter key
;; f -- find key or search key
;; h -- home-position key
;; k -- delete key or remove key.
;; l -- left-arrow
;; p -- portrait mode
;; q -- landscape mode
;; r -- right-arrow
;; s -- select key
;; t -- clear tab this column key
;; u -- up-arrow
;; x -- do key
;; ? -- help

;; - -- keypad key labelled `-'.
;; . -- keypad key labelled `.'.
;; , -- keypad key labelled `,'.
;; 0 ... 9 -- keypad key labelled with that digit,
;;   but only if that key is not also an arrow key.

;; C-@, C-a, ... C-x -- numbered function keys 0 through 24.
;; These are used for function keys with no labels but numbers,
;; and may also be used for function keys with labels
;; that we have not defined letters for.

;; A -- insert line key
;; C -- clear screen key
;; D -- delete character key.
;; E -- clear to end of line key
;; F -- scroll forward key
;; H -- home-down
;; I -- insert character key
;;       If there is just an "insert" key, it should be this.
;; L -- delete line key
;; M -- exit insert mode key
;; N -- next page key
;; P -- previous page key
;; R -- scroll reverse key
;; S -- clear to end of screen key
;; T -- set tab this column key

(defun keypad-default (char definition)
  (or (lookup-key function-keymap char)
      (define-key function-keymap char definition)))

;; Here are the standard command meanings we give to the various
;; function key names.  Because this file is loaded after the user's
;; init file, we are careful to avoid overriding any definitions
;; already stored in function-keymap by the init file or (less often)
;; by the terminal-specific term/*.el file.

(keypad-default "l" 'backward-char)
(keypad-default "r" 'forward-char)
(keypad-default "D" 'delete-char)
(keypad-default "u" 'previous-line)
(keypad-default "d" 'next-line)
(keypad-default "N" 'scroll-up)
(keypad-default "P" 'scroll-down)
(keypad-default "C" 'recenter)
(keypad-default "?" 'help-for-help)
(keypad-default "s" 'set-mark-command)
(keypad-default "k" 'kill-region)
(keypad-default "f" 're-search-forward)

(keypad-default "\C-a" 'beginning-of-line)
(keypad-default "\C-b" 'end-of-line)
(keypad-default "\C-c" 'isearch-forward)
(keypad-default "\C-d" 'kill-line)

(keypad-default "." 'delete-char)
(keypad-default "0" 'yank)
(keypad-default "e" 'open-line)
(keypad-default "1" 'backward-word)
(keypad-default "3" 'forward-word)
(keypad-default "7" 'backward-paragraph)
(keypad-default "9" 'forward-paragraph)
(keypad-default "h" 'move-to-window-line)

(defun setup-terminal-keymap (map translations)
  "Set up keymap MAP to forward to function-keymap according to TRANSLATIONS.
TRANSLATIONS is an alist; each element of it looks like (FROMSTRING . TOCHAR).
For each such pair, we define the key sequence FROMSTRING in MAP
to forward to the definition of character TOCHAR in function-keymap.
\"Forwarding\" means that subsequent redefinition of TOCHAR in
function-keymap will be seen automatically in MAP as well.

This function is used by files term/*.el to set up the mapping from the
escape sequences sent by function keys on particular terminals (FROMSTRINGs)
into Emacs standard function key slots (TOCHARs).
An actual definition (such as a symbol) may be given in place of TOCHAR.
Generally, MAP is a prefix keymap which will be attached to a key
that is the common prefix sent by all function keys (often ESC O or ESC [)."
  (while translations
    (define-key map (car (car translations))
      (if (numberp (cdr (car translations)))
	  (cons function-keymap (cdr (car translations)))
	(cdr (car translations))))
    (setq translations (cdr translations))))

(defun function-key-sequence (char)
  "Return key sequence for function key that on this terminal
translates into slot CHAR in function-keymap.
Or return nil if there is none."
  (car (where-is-internal (cons function-keymap char) (current-local-map))))

(provide 'keypad)
