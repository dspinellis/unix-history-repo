;; Map VT100 function key escape sequences
;; into the standard slots in function-keymap.

(require 'keypad)

(defvar CSI-map nil
  "The CSI-map maps the CSI function keys on the VT100 keyboard.
The CSI keys are the arrow keys.")

(if (not CSI-map)
    (progn
     (setq CSI-map (lookup-key global-map "\e["))
     (if (not (keymapp CSI-map))
	 (setq CSI-map (make-sparse-keymap)))  ;; <ESC>[ commands

     (setup-terminal-keymap CSI-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)))))	   ; left-arrow

(defun enable-arrow-keys ()
  "Enable the use of the VT100 arrow keys for cursor motion.
Because of the nature of the VT100, this unavoidably breaks
the standard Emacs command ESC [; therefore, it is not done by default,
but only if you give this command."
  (interactive)
  (global-set-key "\e[" CSI-map))

(defvar SS3-map nil
  "SS3-map maps the SS3 function keys on the VT100 keyboard.
The SS3 keys are the numeric keypad keys in keypad application mode
\(DECKPAM).  SS3 is DEC's name for the sequence <ESC>O which is
the common prefix of what these keys transmit.")

(if (not SS3-map)
    (progn

     (setq SS3-map (lookup-key global-map "\eO"))
     (if (not (keymapp SS3-map))
	 (setq SS3-map (make-keymap)))  ;; <ESC>O commands
     (setup-terminal-keymap SS3-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("M" . ?e)	   ; Enter
	      ("P" . ?\C-a)	   ; PF1
	      ("Q" . ?\C-b)	   ; PF2
	      ("R" . ?\C-c)	   ; PF3
	      ("S" . ?\C-d)	   ; PF4
	      ("l" . ?,)	   ; ,
	      ("m" . ?-)	   ; -
	      ("n" . ?.)	   ; .
	      ("p" . ?0)	   ; 0
	      ("q" . ?1)	   ; 1
	      ("r" . ?2)	   ; 2
	      ("s" . ?3)	   ; 3
	      ("t" . ?4)	   ; 4
	      ("u" . ?5)	   ; 5
	      ("v" . ?6)	   ; 6
	      ("w" . ?7)	   ; 7
	      ("x" . ?8)	   ; 8
	      ("y" . ?9)))	   ; 9

     (define-key global-map "\eO" SS3-map)))
