;; Map s4 function key escape sequences
;; into the standard slots in function-keymap where we can;
;; set up terminal-specific bindings where we must
;;
;; by: Eric S. Raymond == {ihnp4,rutgers,seismo}!cbmvax!snark!eric

(require 'keypad)

;; First, map as many keys as possible to terminal-independent keycaps

(defvar META-RB-map nil
  "The META-RB-map maps the ESC-[ function keys on the s4 keyboard.")

(if (not META-RB-map)
    (progn
     (setq META-RB-map (lookup-key global-map "\e["))
     (if (not (keymapp META-RB-map))
	 (setq META-RB-map (make-sparse-keymap)))  ;; <ESC>[ commands

     (setup-terminal-keymap META-RB-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("U" . ?N)	   ; 'Page' -> next page
	      ("V" . ?P)	   ; 'Shift-Page' -> prev page
	      ("H" . ?h)	   ; 'Home' -> home-key
	;;    ("J" . ??)	   ; 'Clear' -> unmapped
	))))

(defun enable-arrow-keys ()
  "Enable the use of the s4 arrow keys for cursor motion.
Because of the nature of the s4, this unavoidably breaks
the standard Emacs command ESC [; therefore, it is not done by default,
but only if you give this command in your .emacs."
  (global-set-key "\e[" META-RB-map))

(defvar META-N-map nil
  "META-N-map maps the ESC-N function keys on the s4 keyboard.")

(if (not META-N-map)
    (progn

     (setq META-N-map (lookup-key global-map "\eN"))
     (if (not (keymapp META-N-map))
	 (setq META-N-map (make-sparse-keymap)))  ;; <ESC>N commands
     (setup-terminal-keymap META-N-map '(
		("a" . ?C)	; 'Rfrsh' -> redraw screen
	;;	("A" . ??)	; 'Clear' -> unmapped
	;;	("c" . ??)	; 'Move' -> unmapped
	;;	("d" . ??)	; 'Copy' -> unmapped
	;;	("B" . ??)	; 'Shift-Beg' -> unmapped
	;;	("M" . ??)	; 'Shift-Home' -> unmapped
	;;	("N" . ??)	; 'Shift-End' -> unmapped
		("e" . ?k)	; 'Dlete' -> generic delete (kill-region)
		("f" . ?.)	; 'Dlete Char' -> keypad .
		("g" . ?1)	; 'Prev' -> keypad 1 (backward-word)
		("h" . ?3)	; 'Next' -> keypad 3 (forward-word)
		("i" . ?s)	; 'Mark' -> select
	;;	("I" . ??)	; 'Select' -> MAPPED BELOW
	;;	("j" . ??)	; 'Input Mode' -> unmapped
	))

     (define-key global-map "\eN" META-N-map)))

(defvar META-O-map nil
  "META-O-map maps the META-O function keys on the s4 keyboard.")

(if (not META-O-map)
    (progn

     (setq META-O-map (lookup-key global-map "\eO"))
     (if (not (keymapp META-O-map))
	 (setq META-O-map (make-sparse-keymap)))  ;; <ESC>O commands
     (setup-terminal-keymap META-O-map '(
		("a" . ?E)	; 'Clear-Line' -> Clear to EOL
		("A" . ?S)	; 'Shift-Clear-Line' -> Clear to EOS
		("b" . ?\C-@)	; 'Ref' -> function key 0
		("c" . ?\C-a)	; 'F1' -> function key 1
		("d" . ?\C-b)	; 'F2' -> function key 2
		("e" . ?\C-c)	; 'F3' -> function key 3
		("f" . ?\C-d)	; 'F4' -> function key 4
		("g" . ?\C-e)	; 'F5' -> function key 5
		("h" . ?\C-f)	; 'F6' -> function key 6
		("i" . ?\C-g)	; 'F7' -> function key 7
		("j" . ?\C-h)	; 'F8' -> function key 8
	;;	("k" . ??)	; 'Exit' -> MAPPED BELOW
		("m" . ??)	; 'Help' -> help-command
	;;	("n" . ??)	; 'Creat' -> unmapped
	;;	("o" . ??)	; 'Save' -> MAPPED BELOW
	;;	("r" . ??)	; 'Opts' -> unmapped
	;;	("s" . ??)	; 'Undo' -> MAPPED BELOW
		("t" . ?x)	; 'Redo' -> 'do' key
	;;	("u" . ??)	; 'Cmd' -> MAPPED BELOW
	;;	("v" . ??)	; 'Open' -> MAPPED BELOW
	;;	("V" . ??)	; 'Close' -> unmapped
	;;	("w" . ??)	; 'Cancel' -> MAPPED BELOW
		("x" . ?f)	; 'Find' -> find/replace
	;;	("y" . ??)	; 'Rplac' -> MAPPED BELOW
	;;	("z" . ??)	; 'Print' -> MAPPED BELOW
	))

     (define-key global-map "\eO" META-O-map)))

(defvar META-P-map nil
  "META-P-map maps the META-P function keys on the s4 keyboard.")

(if (not META-P-map)
    (progn

     (setq META-P-map (lookup-key global-map "\eP"))
     (if (not (keymapp META-P-map))
	 (setq META-P-map (make-sparse-keymap)))  ;; <ESC>P commands
     (setup-terminal-keymap META-P-map '(
		("a" . ?1)	; Ctrl-1 -> keypad 1
		("b" . ?2)	; Ctrl-2 -> keypad 2
		("c" . ?3)	; Ctrl-3 -> keypad 3
		("d" . ?4)	; Ctrl-4 -> keypad 4
		("e" . ?5)	; Ctrl-5 -> keypad 5
		("f" . ?6)	; Ctrl-6 -> keypad 6
		("g" . ?7)	; Ctrl-7 -> keypad 7
		("h" . ?8)	; Ctrl-8 -> keypad 8
		("i" . ?9)	; Ctrl-9 -> keypad 9
		("j" . ?0)	; Ctrl-0 -> keypad 0
		("k" . ?-)	; Ctrl-- -> keypad -
	))

     (define-key global-map "\eP" META-P-map)))

;; Now do terminal-specific mappings of keys with no standard-keycap equivalent

;;;(define-key esc-map "9" 'beginning-of-buffer)		;'Begin'
;;;(define-key esc-map "0" 'end-of-buffer)			;'End'
(define-key META-N-map "I" 'narrow-to-region)		;'Select'
(define-key META-O-map "k" 'save-buffers-kill-emacs)	;'Exit'
(define-key META-O-map "o" 'save-buffer)		;'Save'
(define-key META-O-map "s" 'undo)			;'Undo'
(define-key META-O-map "u" 'execute-extended-command)	;'Cmd'
(define-key META-O-map "v" 'find-file)			;'Open'
(define-key META-O-map "w" 'keyboard-quit)		;'Cancl'
(define-key META-O-map "y" 'replace-regexp)		;'Rplac'
(define-key META-O-map "z" 'lpr-buffer)			;'Print'
