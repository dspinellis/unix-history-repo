;; vt100 series terminal stuff.
;; April 1985, Joe Kelsey

(defvar SS3-map nil
  "SS3-map maps the SS3 function keys on the VT100 keyboard.
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

(if (not SS3-map)
    (progn

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

     (define-key global-map "\eO" SS3-map)))

(defun beginning-of-next-line ()
  "Move to the beginning of the next line."
  (interactive)
  (forward-line 1))

(defun beginning-of-previous-line ()
  "Move to the beginning of the previous line."
  (interactive)
  (forward-line -1))
