;; vt200 series terminal stuff.
;; April 1985, Joe Kelsey

(defvar CSI-map nil
  "The CSI-map maps the CSI function keys on the VT201 keyboard.
The CSI keys are the dark function keys, and are only active in
VT200-mode, except for the arrow keys.  The functions provided are:

Arrows: The obvious definitions: backward-char, next-line, forward-char,
                                             previous-line.

Editing Keys:
  Find                  re-search-forward
  Insert Here           open-line
  Remove                kill-region
  Select                set-mark-command
  Prev Screen           scroll-up
  Next Screen           scroll-down

Top row keys:
  F11                   ESC-prefix
  Help                  help-command
  Do                    eval-expression

You can bind other function keys by doing:
  (define-key CSI-map \"<key>\" '<function>)
where <key> is the function key with the CSI (<ESC>[) stripped off
and <function> is the name of the function to map the key to.")

(defvar SS3-map nil
  "SS3-map maps the SS3 function keys on the VT201 keyboard.
The SS3 keys are the numeric keypad keys in keypad application mode
(DECKPAM).  SS3 is the ASCII-8bit character for the 7-bit escape
sequence <ESC>O.  The functions provided are:

  -----------------------------------------------------------------
  |PF1            |PF2            |PF3            |PF4            |
  |beginning-of-  |               |               |               |
  |line           |end-of-line    |isearch-forward|kill-line      |
  |---------------+---------------+---------------+---------------|
  |7              |8              |9              |-              |
  |forward-       |backward-      |               |               |
  |paragraph      |paragraph      |kill-region    |kill-word      |
  |---------------+---------------+---------------+---------------|
  |4              |5              |6              |,              |
  |               |beginning-of-  |               |               |
  |end-of-buffer  |buffer         |yank           |delete-char    |
  |---------------+---------------+---------------+---------------|
  |1              |2              |3              |Enter          |
  |               |               |               |               |
  |forward-word   |backward-word  |quoted-insert  |               |
  |---------------+---------------+---------------|               | 
  |0                              |.              |               |
  |                               |beginning-of-  |               |
  |beginning-of-next-line         |previous-line  |open-line      |
  -----------------------------------------------------------------")

(if (not CSI-map)
    (progn
     (setq CSI-map (make-keymap))  ;; <ESC>[ commands

     (define-key CSI-map "A" 'previous-line)               ;; up arrow
     (define-key CSI-map "B" 'next-line)                   ;; down-arrow
     (define-key CSI-map "C" 'forward-char)                ;; right-arrow
     (define-key CSI-map "D" 'backward-char)               ;; left-arrow

     (define-key CSI-map "1~" 're-search-forward)          ;; Find
     (define-key CSI-map "2~" 'open-line)                  ;; Insert Here
     (define-key CSI-map "3~" 'kill-region)                ;; Re-move

     (define-key CSI-map "4~" 'set-mark-command)           ;; Select
     (define-key CSI-map "5~" 'scroll-down)                ;; Prev Screen
     (define-key CSI-map "6~" 'scroll-up)                  ;; Next Screen

     (define-key CSI-map "23~" 'ESC-prefix)                ;; F11 (ESC)

     (define-key CSI-map "28~" 'help-command)              ;; Help
     (define-key CSI-map "29~" 'eval-expression)           ;; Do

     (setq SS3-map (make-keymap))  ;; <ESC>O commands

     (define-key SS3-map "A" 'previous-line)               ;; up arrow
     (define-key SS3-map "B" 'next-line)                   ;; down-arrow
     (define-key SS3-map "C" 'forward-char)                ;; right-arrow
     (define-key SS3-map "D" 'backward-char)               ;; left-arrow

     (define-key SS3-map "M" 'open-line)                   ;; Enter

     (define-key SS3-map "P" 'beginning-of-line)           ;; PF1
     (define-key SS3-map "Q" 'end-of-line)                 ;; PF2
     (define-key SS3-map "R" 'isearch-forward)             ;; PF3
     (define-key SS3-map "S" 'kill-line)                   ;; PF4

     (define-key SS3-map "l" 'delete-char)                 ;; ,
     (define-key SS3-map "m" 'kill-word)                   ;; -

     (define-key SS3-map "n" 'beginning-of-previous-line)  ;; .
     (define-key SS3-map "p" 'beginning-of-next-line)      ;; 0

     (define-key SS3-map "q" 'forward-word)                ;; 1
     (define-key SS3-map "r" 'backward-word)               ;; 2
     (define-key SS3-map "s" 'quoted-insert)               ;; 3

     (define-key SS3-map "t" 'end-of-buffer)               ;; 4
     (define-key SS3-map "u" 'beginning-of-buffer)         ;; 5
     (define-key SS3-map "v" 'yank)                        ;; 6

     (define-key SS3-map "w" 'forward-paragraph)           ;; 7
     (define-key SS3-map "x" 'backward-paragraph)          ;; 8
     (define-key SS3-map "y" 'kill-region)                 ;; 9

     (define-key global-map "\eO" SS3-map)
     ))

(defun vt200-enable-arrows ()
  "Redefine Emacs so that VT200 arrow keys and function keys work.
This is not done automatically because it inescapably causes the
standard Emacs command ESC [ to stop working normally."
  (interactive)
  (define-key global-map "\e[" CSI-map))


(defun beginning-of-next-line ()
  "Move to the beginning of the next line."
  (interactive)
  (forward-line 1))

(defun beginning-of-previous-line ()
  "Move to the beginning of the previous line."
  (interactive)
  (forward-line -1))
