;; vt200 series terminal stuff.
;; April 1985, Joe Kelsey

(require 'keypad)

(defvar CSI-map nil
  "The CSI-map maps the CSI function keys on the VT200 keyboard.
The CSI keys are the dark function keys, and are only active in
VT200-mode, except for the arrow keys.")

(defun enable-arrow-keys ()
  "Enable the use of the VT200 arrow keys and dark function keys.
Because of the nature of the VT200, this unavoidably breaks
the standard Emacs command ESC [; therefore, it is not done by default,
but only if you give this command."
  (interactive)
  (global-set-key "\e[" CSI-map))

;; I suggest that someone establish standard mappings for all of
;; the VT200 CSI function keys into the function-keymap.

(if CSI-map
    nil
  (setq CSI-map (make-keymap))		; <ESC>[ commands
  (setup-terminal-keymap CSI-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("1~" . ?f)	   ; Find
	      ("2~" . ?I)	   ; Insert Here
	      ("3~" . ?k)	   ; Re-move
	      ("4~" . ?s)	   ; Select
	      ("5~" . ?P)	   ; Prev Screen
	      ("6~" . ?N)	   ; Next Screen
	      ("17~" . ?\C-f)	   ; F6
	      ("18~" . ?\C-g)	   ; F7
	      ("19~" . ?\C-h)	   ; F8
	      ("20~" . ?\C-i)	   ; F9
	      ("21~" . ?\C-j)	   ; F10
	      ("23~" . ESC-prefix) ; F11 (ESC)
	      ("24~" . ?\C-l)	   ; F12
	      ("25~" . ?\C-m)	   ; F13
	      ("26~" . ?\C-n)	   ; F14
	      ("31~" . ?\C-q)	   ; F17
	      ("32~" . ?\C-r)	   ; F18
	      ("33~" . ?\C-s)	   ; F19
	      ("34~" . ?\C-t)	   ; F20
	      ("28~" . ??)	   ; Help
	      ("29~" . ?x))))	   ; Do

(defvar SS3-map nil
  "SS3-map maps the SS3 function keys on the VT200 keyboard.
The SS3 keys are the numeric keypad keys in keypad application mode
\(DECKPAM).  SS3 is DEC's name for the sequence <ESC>O which is
the common prefix of what these keys transmit.")

(if SS3-map
    nil
  (setq SS3-map (make-keymap))		; <ESC>O commands
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

     (define-key global-map "\eO" SS3-map))
