;; Terminal emulator for GNU Emacs.
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.
;; Written by Richard Mlynarik, November 1986.

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

;;>>TODO
;;>> terminfo?
;;>> ** Nothing can be done about emacs' meta-lossage **
;;>>  (without redoing keymaps `sanely' -- ask Mly for details)

;;>> One probably wants to do setenv MORE -c when running with
;;>>   more-processing enabled.

(provide 'terminal)
(require 'ehelp)

(defvar terminal-escape-char ?\C-^
  "*All characters except for this are passed verbatim through the
terminal-emulator.  This character acts as a prefix for commands
to the emulator program itself.  Type this character twice to send
it through the emulator.  Type ? after typing it for a list of
possible commands.
This variable is local to each terminal-emulator buffer.")

(defvar terminal-scrolling t
  "*If non-nil, the terminal-emulator will `scroll' when output occurs
past the bottom of the screen.  If nil, output will `wrap' to the top
of the screen.
This variable is local to each terminal-emulator buffer.")

(defvar terminal-more-processing t
  "*If non-nil, do more-processing.
This variable is local to each terminal-emulator buffer.")

;; If you are the sort of loser who uses scrolling without more breaks
;; and expects to actually see anything, you should probably set this to
;; around 400
(defvar terminal-redisplay-interval 5000
  "*Maximum number of characters which will be processed by the
terminal-emulator before a screen redisplay is forced.
Set this to a large value for greater throughput,
set it smaller for more frequent updates but overall slower
performance.")

(defvar terminal-more-break-insertion
  "*** More break -- Press space to continue ***")

(defvar terminal-escape-map nil)
(defvar terminal-map nil)
(defvar terminal-more-break-map nil)
(if terminal-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'te-pass-through)
    ;(define-key map "\C-l"
    ;  '(lambda () (interactive) (te-pass-through) (redraw-display)))
    (setq terminal-map map)))

;(setq terminal-escape-map nil)
(if terminal-escape-map
    nil
  (let ((map (make-keymap)))
    ;(fillarray map 'te-escape-extended-command-unread)
    (fillarray map 'undefined)
    (let ((s "0"))
      (while (<= (aref s 0) ?9)
	(define-key map s 'digit-argument)
	(aset s 0 (1+ (aref s 0)))))
    (define-key map "b" 'switch-to-buffer)
    (define-key map "o" 'other-window)
    (define-key map "e" 'te-set-escape-char)
    (define-key map "\C-l" 'redraw-display)
    (define-key map "\C-o" 'te-flush-pending-output)
    (define-key map "m" 'te-toggle-more-processing)
    (define-key map "x" 'te-escape-extended-command)
    (define-key map "?" 'te-escape-help)
    (define-key map (char-to-string help-char) 'te-escape-help)
    (setq terminal-escape-map map)))

(defvar te-escape-command-alist ())
;(setq te-escape-command-alist ())
(if te-escape-command-alist
    nil
  (setq te-escape-command-alist
	'(("Set Escape Character" . te-set-escape-char)
	  ("Refresh" . redraw-display)
	  ("Record Output" . te-set-output-log)
	  ("Photo" . te-set-output-log)
	  ("Tofu" . te-tofu) ;; confuse the uninitiated
	  ("Stuff Input" . te-stuff-string)
	  ("Flush Pending Output" . te-flush-pending-output)
	  ("Enable More Processing" . te-enable-more-processing)
	  ("Disable More Processing" . te-disable-more-processing)
	  ("Scroll at end of page" . te-do-scrolling)
	  ("Wrap at end of page" . te-do-wrapping)
	  ("Switch To Buffer" . switch-to-buffer)
	  ("Other Window" . other-window)
	  ("Kill Buffer" . kill-buffer)
	  ("Help" . te-escape-help)
	  ("Set Redisplay Interval" . te-set-redisplay-interval)
	  )))

;(setq terminal-more-break-map nil)
(if terminal-more-break-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'te-more-break-unread)
    (define-key map (char-to-string help-char) 'te-more-break-help)
    (define-key map " " 'te-more-break-resume)
    (define-key map "\C-l" 'redraw-display)
    (define-key map "\C-o" 'te-more-break-flush-pending-output)
    ;;>>> this isn't right
    ;(define-key map "\^?" 'te-more-break-flush-pending-output) ;DEL
    (define-key map "\r" 'te-more-break-advance-one-line)

    (setq terminal-more-break-map map)))
  

;;;;  escape map

(defun te-escape ()
  (interactive)
  (let (s 
	(local (current-local-map))
	(global (current-global-map)))
    (unwind-protect
	(progn
	  (use-global-map terminal-escape-map)
	  (use-local-map terminal-escape-map)
	  (setq s (read-key-sequence
		    (if prefix-arg
			(format "Emacs Terminal escape> %d "
				(prefix-numeric-value prefix-arg))
		        "Emacs Terminal escape> "))))
      (use-global-map global)
      (use-local-map local))
    (message "")
    (cond ((string= s (make-string 1 terminal-escape-char))
	   (setq last-command-char terminal-escape-char)
	   (let ((terminal-escape-char -259))
	     (te-pass-through)))
	  ((setq s (lookup-key terminal-escape-map s))
	   (call-interactively s)))))

(defun te-escape-help ()
  "Provide help on commands available after terminal-escape-char is typed."
  (interactive)
  (message "Terminal emulator escape help...")
  (let ((char (single-key-description terminal-escape-char)))
    (with-electric-help
      (function (lambda ()
	 (princ (format "Terminal-emulator escape, invoked by \"%s\"
Type \"%s\" twice to send a single \"%s\" through.

Other chars following \"%s\" are interpreted as follows:\n"
			char char char char))

	 (princ (substitute-command-keys "\\{terminal-escape-map}\n"))
	 (princ (format "\nSubcommands of \"%s\" (%s)\n"
			(where-is-internal 'te-escape-extended-command
					   terminal-escape-map t)
			'te-escape-extended-command))
	 (let ((l (if (fboundp 'sortcar)
		      (sortcar (copy-sequence te-escape-command-alist)
			       'string<)
		      (sort (copy-sequence te-escape-command-alist)
			    (function (lambda (a b)
                              (string< (car a) (car b))))))))
	   (while l
	     (let ((doc (or (documentation (cdr (car l)))
			    "Not documented")))
	       (if (string-match "\n" doc)
		   ;; just use first line of documentation
		   (setq doc (substring doc 0 (match-beginning 0))))
	       (princ "  \"")
	       (princ (car (car l)))
	       (princ "\":\n     ")
	       (princ doc)
	       (write-char ?\n))
	     (setq l (cdr l))))
	 nil)))))

			

(defun te-escape-extended-command ()
  (interactive)
  (let ((c (let ((completion-ignore-case t))
	     (completing-read "terminal command: "
			      te-escape-command-alist
			      nil t))))
    (if c
	(catch 'foo
	  (setq c (downcase c))
	  (let ((l te-escape-command-alist))
	    (while l
	      (if (string= c (downcase (car (car l))))
		  (throw 'foo (call-interactively (cdr (car l))))
		(setq l (cdr l)))))))))

;; not used.
(defun te-escape-extended-command-unread ()
  (interactive)
  (setq unread-command-char last-input-char)
  (te-escape-extended-command))

(defun te-set-escape-char (c)
  "Change the terminal-emulator escape character."
  (interactive "cSet escape character to: ")
  (let ((o terminal-escape-char))
    (message (if (= o c)
		 "\"%s\" is escape char"
	         "\"%s\" is now escape; \"%s\" passes though")
	     (single-key-description c)
	     (single-key-description o))
    (setq terminal-escape-char c)))


(defun te-stuff-string (string)
  "Read a string to send to through the terminal emulator
as though that string had been typed on the keyboard.

Very poor man's file transfer protocol."
  (interactive "sStuff string: ")
  (process-send-string te-process string))

(defun te-set-output-log (name)
  "Record output from the terminal emulator in a buffer."
  (interactive (list (if te-log-buffer
			 nil
		       (read-buffer "Record output in buffer: "
				    (format "%s output-log"
					    (buffer-name (current-buffer)))
				    nil))))
  (if (or (null name) (equal name ""))
      (progn (setq te-log-buffer nil)
	     (message "Output logging off."))
    (if (get-buffer name)
	nil
      (save-excursion
	(set-buffer (get-buffer-create name))
	(fundamental-mode)
	(buffer-flush-undo (current-buffer))
	(erase-buffer)))
    (setq te-log-buffer (get-buffer name))
    (message "Recording terminal emulator output into buffer \"%s\""
	     (buffer-name te-log-buffer))))

(defun te-tofu ()
  "Discontinue output log."
  (interactive)
  (te-set-output-log nil))
  

(defun te-toggle (sym arg)
  (set sym (cond ((not (numberp arg)) arg)
		 ((= arg 1) (not (symbol-value sym)))
		 ((< arg 0) nil)
		 (t t))))

(defun te-toggle-more-processing (arg)
  (interactive "p")
  (message (if (te-toggle 'terminal-more-processing arg)
	       "More processing on" "More processing off"))
  (if terminal-more-processing (setq te-more-count -1)))

(defun te-toggle-scrolling (arg)
  (interactive "p")
  (message (if (te-toggle 'terminal-scrolling arg)
	       "Scroll at end of page" "Wrap at end of page")))

(defun te-enable-more-processing ()
  "Enable ** MORE ** processing"
  (interactive)
  (te-toggle-more-processing t))

(defun te-disable-more-processing ()
  "Disable ** MORE ** processing"
  (interactive)
  (te-toggle-more-processing nil))

(defun te-do-scrolling ()
  "Scroll at end of page (yuck)"
  (interactive)
  (te-toggle-scrolling t))

(defun te-do-wrapping ()
  "Wrap to top of window at end of page"
  (interactive)
  (te-toggle-scrolling nil))


(defun te-set-redisplay-interval (arg)
  "Set the maximum interval (in output characters) between screen updates.
Set this number to large value for greater throughput,
set it smaller for more frequent updates (but overall slower performance."
  (interactive "NMax number of output chars between redisplay updates: ")
  (setq arg (max arg 1))
  (setq terminal-redisplay-interval arg
	te-redisplay-count 0))

;;;; more map

;; every command -must- call te-more-break-unwind
;; or grave lossage will result

(put 'te-more-break-unread 'suppress-keymap t)
(defun te-more-break-unread ()
  (interactive)
  (if (= last-input-char terminal-escape-char)
      (call-interactively 'te-escape)
    (message "Continuing from more break (\"%s\" typed, %d chars output pending...)"
	     (single-key-description last-input-char)
	     (te-pending-output-length))
    (setq te-more-count 259259)
    (te-more-break-unwind)
    (let ((terminal-more-processing nil))
      (te-pass-through))))

(defun te-more-break-resume ()
  "Proceed past the **MORE** break,
allowing the next page of output to appear"
  (interactive)
  (message "Continuing from more break")
  (te-more-break-unwind))

(defun te-more-break-help ()
  "Provide help on commands available in a terminal-emulator **MORE** break"
  (interactive)
  (message "Terminal-emulator more break help...")
  (sit-for 0)
  (with-electric-help
    (function (lambda ()
      (princ "Terminal-emulator more break.\n\n")
      (princ (format "Type \"%s\" (te-more-break-resume)\n%s\n"
		     (where-is-internal 'te-more-break-resume
					terminal-more-break-map t)
		     (documentation 'te-more-break-resume)))
      (princ (substitute-command-keys "\\{terminal-more-break-map}\n"))
      (princ "Any other key is passed through to the program
running under the terminal emulator and disables more processing until
all pending output has been dealt with.")
      nil))))


(defun te-more-break-advance-one-line ()
  "Allow one more line of text to be output before doing another more break."
  (interactive)
  (setq te-more-count 1)
  (te-more-break-unwind))

(defun te-more-break-flush-pending-output ()
  "Discard any output which has been received by the terminal emulator but
not yet proceesed and then proceed from the more break."
  (interactive)
  (te-more-break-unwind)
  (te-flush-pending-output))

(defun te-flush-pending-output ()
  "Discard any as-yet-unprocessed output which has been received by
the terminal emulator."
  (interactive)
  ;; this could conceivably be confusing in the presence of
  ;; escape-sequences spanning process-output chunks
  (if (null (cdr te-pending-output))
      (message "(There is no output pending)")
    (let ((length (te-pending-output-length)))
      (message "Flushing %d chars of pending output" length)
      (setq te-pending-output
	    (list 0 (format "\n*** %d chars of pending output flushed ***\n"
			    length)))
      (te-update-pending-output-display)
      (te-process-output nil)
      (sit-for 0))))


(defun te-pass-through ()
  "Send the last character typed through the terminal-emulator
without any interpretation"
  (interactive)
  (if (eql last-input-char terminal-escape-char)
      (call-interactively 'te-escape)
    (and terminal-more-processing
	 (null (cdr te-pending-output))
	 (te-set-more-count nil))
    (send-string te-process (make-string 1 last-input-char))
    (te-process-output t))) 

(defun te-set-window-start ()
  (let* ((w (get-buffer-window (current-buffer)))
	 (h (if w (window-height w))))
    (cond ((not w)) ; buffer not displayed
	  ((>= h (/ (- (point) (point-min)) (1+ te-width)))
	   ;; this is the normal case
	   (set-window-start w (point-min)))
	  ;; this happens if some vandal shrinks our window.
	  ((>= h (/ (- (point-max) (point)) (1+ te-width)))
	   (set-window-start w (- (point-max) (* h (1+ te-width)) -1)))
	  ;; I give up.
	  (t nil))))

(defun te-pending-output-length ()
  (let ((length (car te-pending-output))
	(tem (cdr te-pending-output)))
    (while tem
      (setq length (+ length (length (car tem))) tem (cdr tem)))
    length))

;;;; more break hair

(defun te-more-break ()
  (te-set-more-count t)
  (make-local-variable 'te-more-old-point)
  (setq te-more-old-point (point))
  (make-local-variable 'te-more-old-local-map)
  (setq te-more-old-local-map (current-local-map))
  (use-local-map terminal-more-break-map)
  (make-local-variable 'te-more-old-filter)
  (setq te-more-old-filter (process-filter te-process))
  (make-local-variable 'te-more-old-mode-line-format)
  (setq te-more-old-mode-line-format mode-line-format
	mode-line-format (list "--   **MORE**  "
			       mode-line-buffer-identification
			       "%-"))
  (set-process-filter te-process
    (function (lambda (process string)
		(save-excursion
		  (set-buffer (process-buffer process))
		  (setq te-pending-output (nconc te-pending-output
						 (list string))))
		  (te-update-pending-output-display))))
  (te-update-pending-output-display)
  (if (eq (window-buffer (selected-window)) (current-buffer))
      (message "More break "))
  (or (eobp)
      (null terminal-more-break-insertion)
      (save-excursion
	(forward-char 1)
	(delete-region (point) (+ (point) te-width))
	(insert terminal-more-break-insertion)))
  (run-hooks 'terminal-more-break-hook)
  (sit-for 0) ;get display to update
  (throw 'te-process-output t))

(defun te-more-break-unwind ()
  (use-local-map te-more-old-local-map)
  (set-process-filter te-process te-more-old-filter)
  (goto-char te-more-old-point)
  (setq mode-line-format te-more-old-mode-line-format)
  (set-buffer-modified-p (buffer-modified-p))
  (let ((buffer-read-only nil))
    (cond ((eobp))
	  (terminal-more-break-insertion
	   (forward-char 1)
	   (delete-region (point)
			  (+ (point) (length terminal-more-break-insertion)))
	   (insert-char ?\  te-width)
	   (goto-char te-more-old-point)))
    (setq te-more-old-point nil)
    (let ((te-more-count 259259))
      (te-newline)))
  ;(sit-for 0)
  (te-process-output t))

(defun te-set-more-count (newline)
  (let ((line (/ (- (point) (point-min)) (1+ te-width))))
    (if newline (setq line (1+ line)))
    (cond ((= line te-height)
	   (setq te-more-count te-height))
	  ;>>>> something is strange.  Investigate this!
	  ((= line (1- te-height))
	   (setq te-more-count te-height))
	  ((or (< line (/ te-height 2))
	       (> (- te-height line) 10))
	   ;; break at end of this page
	   (setq te-more-count (- te-height line)))
	  (t
	   ;; migrate back towards top (ie bottom) of screen.
	   (setq te-more-count (- te-height
				  (if (> te-height 10) 2 1)))))))


;;;; More or less straight-forward terminal escapes

;; ^j, meaning `newline' to non-display programs.
;; (Who would think of ever writing a system which doesn't understand
;;  display terminals natively?  Un*x:  The Operating System of the Future.)
(defun te-newline ()
  "Move down a line, optionally do more processing, perhaps wrap/scroll,
move to start of new line, clear to end of line."
  (end-of-line)
  (cond ((not terminal-more-processing))
	((< (setq te-more-count (1- te-more-count)) 0)
	 (te-set-more-count t))
	((eql te-more-count 0)
	 ;; this doesn't return
	 (te-more-break)))
  (if (eobp)
      (progn
	(delete-region (point-min) (+ (point-min) te-width))
	(goto-char (point-min))
	(if terminal-scrolling
	    (progn (delete-char 1)
		   (goto-char (point-max))
		   (insert ?\n))))
    (forward-char 1)
    (delete-region (point) (+ (point) te-width)))
  (insert-char ?\  te-width)
  (beginning-of-line)
  (te-set-window-start))

;; ^p ^j
;; Handle the `do' or `nl' termcap capability.
;;>> I am not sure why this broken, obsolete, capability is here.
;;>> Perhaps it is for VIle.  No comment was made about why it
;;>> was added (in "Sun Dec  6 01:22:27 1987  Richard Stallman")
(defun te-down-vertically-or-scroll ()
  "Move down a line vertically, or scroll at bottom."
  (let ((column (current-column)))
    (end-of-line)
    (if (eobp)
	(progn
	  (delete-region (point-min) (+ (point-min) te-width))
	  (goto-char (point-min))
	  (delete-char 1)
	  (goto-char (point-max))
	  (insert ?\n)
	  (insert-char ?\  te-width)
	  (beginning-of-line))
      (forward-line 1))
    (move-to-column column))
  (te-set-window-start))

; ^p = x+32 y+32
(defun te-move-to-position ()
  ;; must offset by #o40 since cretinous unix won't send a 004 char through
  (let ((y (- (te-get-char) 32))
	(x (- (te-get-char) 32)))
    (if (or (> x te-width)
	    (> y te-height))
	() ;(error "fucked %d %d" x y)
      (goto-char (+ (point-min) x (* y (1+ te-width))))
      ;(te-set-window-start?)
      ))
  (setq te-more-count -1))



;; ^p c
(defun te-clear-rest-of-line ()
  (save-excursion
    (let ((n (- (point) (progn (end-of-line) (point)))))
      (delete-region (point) (+ (point) n))
      (insert-char ?\  (- n)))))


;; ^p C
(defun te-clear-rest-of-screen ()
  (save-excursion
    (te-clear-rest-of-line)
    (while (progn (end-of-line) (not (eobp)))
      (forward-char 1) (end-of-line)
      (delete-region (- (point) te-width) (point))
      (insert-char ?\  te-width))))
      

;; ^p ^l
(defun te-clear-screen ()
  ;; regenerate buffer to compensate for (nonexistent!!) bugs.
  (erase-buffer)
  (let ((i 0))
    (while (< i te-height)
      (setq i (1+ i))
      (insert-char ?\  te-width)
      (insert ?\n)))
  (delete-region (1- (point-max)) (point-max))
  (goto-char (point-min))
  (setq te-more-count -1))


;; ^p ^o count+32
(defun te-insert-lines ()
  (if (not (bolp))
      ();(error "fooI")
    (save-excursion
      (let* ((line (- te-height (/ (- (point) (point-min)) (1+ te-width)) -1))
	     (n (min (- (te-get-char) ?\ ) line))
	     (i 0))
	(delete-region (- (point-max) (* n (1+ te-width))) (point-max))
	(if (eql (point) (point-max)) (insert ?\n))
	(while (< i n)
	  (setq i (1+ i))
	  (insert-char ?\  te-width)
	  (or (eql i line) (insert ?\n))))))
  (setq te-more-count -1))


;; ^p ^k count+32
(defun te-delete-lines ()
  (if (not (bolp))
      ();(error "fooD")
    (let* ((line (- te-height (/ (- (point) (point-min)) (1+ te-width)) -1))
	   (n (min (- (te-get-char) ?\ ) line))
	   (i 0))
      (delete-region (point)
		     (min (+ (point) (* n (1+ te-width))) (point-max)))
      (save-excursion
	(goto-char (point-max))
	(while (< i n)
	  (setq i (1+ i))
	  (insert-char ?\  te-width)
	  (or (eql i line) (insert ?\n))))))
  (setq te-more-count -1))

;; ^p ^a
(defun te-beginning-of-line ()
  (beginning-of-line))

;; ^p ^b
(defun te-backward-char ()
  (if (not (bolp))
      (backward-char 1)))

;; ^p ^f
(defun te-forward-char ()
  (if (not (eolp))
      (forward-char 1)))


;; 0177
(defun te-delete ()
  (if (bolp)
      ()
    (delete-region (1- (point)) (point))
    (insert ?\ )
    (forward-char -1)))

;; ^p ^g
(defun te-beep ()
  (beep))


;; ^p _ count+32
(defun te-insert-spaces ()
  (let* ((p (point))
	 (n (min (- (te-get-char) 32)
		 (- (progn (end-of-line) (point)) p))))
    (if (<= n 0)
	nil
      (delete-char (- n))
      (goto-char p)
      (insert-char ?\  n))
    (goto-char p)))

;; ^p d count+32  (should be ^p ^d but cretinous un*x won't send ^d chars!!!)
(defun te-delete-char ()
  (let* ((p (point))
	 (n (min (- (te-get-char) 32)
		 (- (progn (end-of-line) (point)) p))))
    (if (<= n 0)
	nil
      (insert-char ?\  n)
      (goto-char p)
      (delete-char n))
    (goto-char p)))



;; disgusting unix-required shit
;;  Are we living twenty years in the past yet?

(defun te-losing-unix ()
  ;(what lossage)
  ;(message "fucking-unix: %d" char)
  )

;; ^i
(defun te-output-tab ()
  (let* ((p (point))
	 (x (- p (progn (beginning-of-line) (point))))
	 (l (min (- 8 (logand x 7))
		 (progn (end-of-line) (- (point) p)))))
    (goto-char (+ p l))))

;; Also:
;;  ^m => beginning-of-line (for which it -should- be using ^p ^a, right?!!)
;;  ^g => te-beep (for which it should use ^p ^g)
;;  ^h => te-backward-char (for which it should use ^p ^b)



(defun te-filter (process string)
  (let* ((obuf (current-buffer))
	 (m meta-flag))
    ;; can't use save-excursion, as that preserves point, which we don't want
    (unwind-protect
	(progn
	  (set-buffer (process-buffer process))
	  (goto-char te-saved-point)
	  (and (bufferp te-log-buffer)
	       (if (null (buffer-name te-log-buffer))
		   ;; killed
		   (setq te-log-buffer nil)
		 (set-buffer te-log-buffer)
		 (goto-char (point-max))
		 (insert string)
		 (set-buffer (process-buffer process))))
	  (setq te-pending-output (nconc te-pending-output (list string)))
	  (te-update-pending-output-display)
	  ;; this binding is needed because emacs looks at meta-flag when
	  ;;  the keystroke is read from the keyboard, not when it is about
	  ;;  to be fed into a keymap (or returned by read-char)
	  ;; There still could be some screws, though.
	  (let ((meta-flag m))
	    (te-process-output (eq (current-buffer)
				   (window-buffer (selected-window)))))
	  (set-buffer (process-buffer process))
	  (setq te-saved-point (point)))
      (set-buffer obuf))))

;; fucking unix has -such- braindamaged lack of tty control...
(defun te-process-output (preemptable)
  ;;>> There seems no good reason to ever disallow preemption
  (setq preemptable t)
  (catch 'te-process-output
    (let ((buffer-read-only nil)
	  (string nil) ostring start char (matchpos nil))
      (while (cdr te-pending-output)
	(setq ostring string
	      start (car te-pending-output)
	      string (car (cdr te-pending-output))
	      char (aref string start))
	(if (eql (setq start (1+ start)) (length string))
	    (progn (setq te-pending-output
			   (cons 0 (cdr (cdr te-pending-output)))
			 start 0
			 string (car (cdr te-pending-output)))
		   (te-update-pending-output-display))
	    (setcar te-pending-output start))
	(if (and (> char ?\037) (< char ?\377))
	    (cond ((eolp)
		   ;; unread char
		   (if (eql start 0)
		       (setq te-pending-output
			     (cons 0 (cons (make-string 1 char)
					   (cdr te-pending-output))))
		       (setcar te-pending-output (1- start)))
		   (te-newline))
		  ((null string)
		   (delete-char 1) (insert char)
		   (te-redisplay-if-necessary 1))
		  (t
		   (let ((end (or (and (eq ostring string) matchpos)
				  (setq matchpos (string-match
						   "[\000-\037\177-\377]"
						   string start))
				  (length string))))
		     (delete-char 1) (insert char)
		     (setq char (point)) (end-of-line)
		     (setq end (min end (+ start (- (point) char))))
		     (goto-char char)
		     (if (eql end matchpos) (setq matchpos nil))
		     (delete-region (point) (+ (point) (- end start)))
		     (insert (if (and (eql start 0)
				      (eql end (length string)))
				 string
			         (substring string start end)))
		     (if (eql end (length string))
			 (setq te-pending-output
			       (cons 0 (cdr (cdr te-pending-output))))
		         (setcar te-pending-output end))
		     (te-redisplay-if-necessary (1+ (- end start))))))
	  ;; I suppose if I split the guts of this out into a separate
	  ;;  function we could trivially emulate different terminals
	  ;; Who cares in any case?  (Apart from stupid losers using rlogin)
	  (funcall
	    (if (eql char ?\^p)
	        (or (cdr (assq (te-get-char)
			       '((?= . te-move-to-position)
				 (?c . te-clear-rest-of-line)
				 (?C . te-clear-rest-of-screen)
				 (?\C-o . te-insert-lines)
				 (?\C-k . te-delete-lines)
				 ;; not necessary, but help sometimes.
				 (?\C-a . te-beginning-of-line)
				 (?\C-b . te-backward-char)
				 ;; should be C-d, but un*x
				 ;;  pty's won't send \004 through!
                                 ;; Can you believe this?
				 (?d . te-delete-char)
				 (?_ . te-insert-spaces)
				 ;; random
				 (?\C-f . te-forward-char)
				 (?\C-g . te-beep)
				 (?\C-j . te-down-vertically-or-scroll)
				 (?\C-l . te-clear-screen)
				 )))
		    'te-losing-unix)
	        (or (cdr (assq char
			       '((?\C-j . te-newline)
				 (?\177 . te-delete)
				 ;; Did I ask to be sent these characters?
				 ;; I don't remember doing so, either.
				 ;; (Perhaps some operating system or
				 ;; other is completely incompetent...)
				 (?\C-m . te-beginning-of-line) ;fuck me harder
				 (?\C-g . te-beep)             ;again and again!
				 (?\C-h . te-backward-char)     ;wa12id!!
				 (?\C-i . te-output-tab))))     ;(spiked)
		    'te-losing-unix)))		      ;That feels better
	  (te-redisplay-if-necessary 1))
	(and preemptable
	     (input-pending-p)
	     ;; preemptable output!  Oh my!!
	     (throw 'te-process-output t)))))
  ;; We must update window-point in every window displaying our buffer
  (let* ((s (selected-window))
	 (w s))
    (while (not (eq s (setq w (next-window w))))
      (if (eq (window-buffer w) (current-buffer))
	  (set-window-point w (point))))))

(defun te-get-char ()
  (if (cdr te-pending-output)
      (let ((start (car te-pending-output))
	    (string (car (cdr te-pending-output))))
	(prog1 (aref string start)
	  (if (eql (setq start (1+ start)) (length string))
	      (setq te-pending-output (cons 0 (cdr (cdr te-pending-output))))
	      (setcar te-pending-output start))))
    (catch 'char
      (let ((filter (process-filter te-process)))
	(unwind-protect
	    (progn
	      (set-process-filter te-process
				  (function (lambda (p s)
                                    (or (eql (length s) 1)
                                        (setq te-pending-output (list 1 s)))
                                    (throw 'char (aref s 0)))))
	      (accept-process-output te-process))
	  (set-process-filter te-process filter))))))


(defun te-redisplay-if-necessary (length)
  (and (<= (setq te-redisplay-count (- te-redisplay-count length)) 0)
       (eq (current-buffer) (window-buffer (selected-window)))
       (waiting-for-user-input-p)
       (progn (te-update-pending-output-display)
	      (sit-for 0)
	      (setq te-redisplay-count terminal-redisplay-interval))))

(defun te-update-pending-output-display ()
  (if (null (cdr te-pending-output))
      (setq te-pending-output-info "")      
    (let ((length (te-pending-output-length)))
      (if (< length 1500)
	  (setq te-pending-output-info "")
	(setq te-pending-output-info (format "(%dK chars output pending) "
					     (/ (+ length 512) 1024))))))
  ;; update mode line
  (set-buffer-modified-p (buffer-modified-p)))


(defun te-sentinel (process message)
  (cond ((eq (process-status process) 'run))
	((null (buffer-name (process-buffer process)))) ;deleted
	(t (let ((b (current-buffer)))
	     (save-excursion
	       (set-buffer (process-buffer process))
	       (setq buffer-read-only nil)
	       (fundamental-mode)
	       (goto-char (point-max))
	       (delete-blank-lines)
	       (delete-horizontal-space)
	       (insert "\n*******\n" message "*******\n"))
	     (if (and (eq b (process-buffer process))
		      (waiting-for-user-input-p))
		 (progn (goto-char (point-max))
			(recenter -1)))))))

(defvar te-stty-string "stty -nl new dec echo"
  "Command string (to be interpreted by \"sh\") which sets the modes
of the virtual terminal to be appropriate for interactive use.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defun terminal-emulator (buffer program args &optional width height)
  "Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program an keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behaviour
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started.

Presently with `termcap' only; if somebody sends us code to make this
work with `terminfo' we will try to use it."
  (interactive
    (cons (save-excursion
	    (set-buffer (get-buffer-create "*terminal*"))
	    (buffer-name (if (or (not (boundp 'te-process))
				 (null te-process)
				 (not (eq (process-status te-process)
					  'run)))
			     (current-buffer)
			   (generate-new-buffer "*terminal*"))))
	  (append
	    (let* ((default-s
		     ;; Default shell is same thing M-x shell uses.
		     (or explicit-shell-file-name
			 (getenv "ESHELL")
			 (getenv "SHELL")
			 "/bin/sh"))
		   (s (read-string
		       (format "Run program in emulator: (default %s) "
			       default-s))))
	      (if (equal s "")
		  (list default-s '())
		(te-parse-program-and-args s))))))
  (switch-to-buffer buffer)
  (if (null width) (setq width (- (window-width (selected-window)) 1)))
  (if (null height) (setq height (- (window-height (selected-window)) 1)))
  (terminal-mode)
  (setq te-width width te-height height)
  (setq mode-line-buffer-identification
	(list (format "Emacs terminal %dx%d: %%b  " te-width te-height)
	      'te-pending-output-info))
  (let ((buffer-read-only nil))
    (te-clear-screen))
  (let (process)
    (while (setq process (get-buffer-process (current-buffer)))
      (if (y-or-n-p (format "Kill process %s? " (process-name process)))
	  (delete-process process)
	(error "Process %s not killed" (process-name process)))))
  (condition-case err
      (let ((termcap
             ;; Because of Unix Brain Death(tm), we can't change
             ;;  the terminal type of a running process, and so
             ;;  terminal size and scrollability are wired-down
             ;;  at this point.  ("Detach?  What's that?")
             (concat (format "emacs-virtual:co#%d:li#%d:%s"
                             ;; Sigh.  These can't be dynamically changed.
                             te-width te-height (if terminal-scrolling
                                                    "" "ns:"))
                     ;;-- Basic things
                     ;; cursor-motion, bol, forward/backward char
                     "cm=^p=%+ %+ :cr=^p^a:le=^p^b:nd=^p^f:"
                     ;; newline, clear eof/eof, audible bell
                     "nw=^j:ce=^pc:cd=^pC:cl=^p^l:bl=^p^g:"
                     ;; insert/delete char/line
                     "IC=^p_%+ :DC=^pd%+ :AL=^p^o%+ :DL=^p^k%+ :"
                     ;;-- Not-widely-known (ie nonstandard) flags, which mean
                     ;; o writing in the last column of the last line
                     ;;   doesn't cause idiotic scrolling, and
                     ;; o don't use idiotische c-s/c-q sogenannte
                     ;;   ``flow control'' auf keinen Fall.
                     "LP:NF:"
                     ;;-- For stupid or obsolete programs
                     "ic=^p_!:dc=^pd!:al=^p^o!:dl=^p^k!:ho=^p=  :"
                     ;;-- For disgusting programs.
                     ;; (VI? What losers need these, I wonder?)
                     "im=:ei=:dm=:ed=:mi:do=^p^j:nl=^p^j:bs:")))
	(if (fboundp 'start-subprocess)
	    ;; this winning function would do everything, except that
	    ;;  rms doesn't want it.
	    (setq te-process (start-subprocess "terminal-emulator"
			       program args
			       'channel-type 'terminal
			       'filter 'te-filter
			       'buffer (current-buffer)
			       'sentinel 'te-sentinel
			       'modify-environment
			         (list (cons "TERM" "emacs-virtual")
				       (cons "TERMCAP" termcap))))
	  ;; so instead we resort to this...
	  (setq te-process (start-process "terminal-emulator" (current-buffer)
			     "/bin/sh" "-c"
			     ;; Yuck!!! Start a shell to set some terminal
			     ;; control characteristics.  Then start the
			     ;; "env" program to setup the terminal type
			     ;; Then finally start the program we wanted.
			     (format "%s; exec %s TERM=emacs-virtual %s %s"
                                     te-stty-string
				     (te-quote-arg-for-sh
				       (concat exec-directory "env"))
				     (te-quote-arg-for-sh
				       (concat "TERMCAP=" termcap))
				     (mapconcat 'te-quote-arg-for-sh
						(cons program args) " "))))
	  (set-process-filter te-process 'te-filter)
	  (set-process-sentinel te-process 'te-sentinel)))
    (error (fundamental-mode)
	   (signal (car err) (cdr err))))
  ;; sigh
  (if (default-value 'meta-flag)
      (progn (message
 "Note:  Meta key disabled due to maybe-eventually-reparable braindamage")
	     (sit-for 1)))
  (message "Entering emacs terminal-emulator...  Type %s %s for help"
	   (single-key-description terminal-escape-char)
	   (mapconcat 'single-key-description
		      (where-is-internal 'te-escape-help
					 terminal-escape-map
					 t)
		      " "))
  (setq inhibit-quit t)			;sport death
  (use-local-map terminal-map)
  (run-hooks 'terminal-mode-hook))

(defun te-parse-program-and-args (s)
  (cond ((string-match "\\`\\([a-zA-Z0-9-+=_.@/:]+[ \t]*\\)+\\'" s)
	 (let ((l ()) (p 0))
	   (while p
	     (setq l (cons (if (string-match
				"\\([a-zA-Z0-9-+=_.@/:]+\\)\\([ \t]+\\)*"
				s p)
			       (prog1 (substring s p (match-end 1))
				 (setq p (match-end 0))
				 (if (eql p (length s)) (setq p nil)))
			       (prog1 (substring s p)
				 (setq p nil)))
			   l)))
	   (setq l (nreverse l))
	   (list (car l) (cdr l))))
	((and (string-match "[ \t]" s) (not (file-exists-p s)))
	 (list shell-file-name (list "-c" (concat "exec " s))))
	(t (list s ()))))

(put 'terminal-mode 'mode-class 'special)
;; This is only separated out from function terminal-emulator
;; to keep the latter a little more managable.
(defun terminal-mode ()
  "Set up variables for use f the terminal-emualtor.
One should not call this -- it is an internal function
of the terminal-emulator"
  (kill-all-local-variables)
  (buffer-flush-undo (current-buffer))
  (setq major-mode 'terminal-mode)
  (setq mode-name "terminal")
; (make-local-variable 'Helper-return-blurb)
; (setq Helper-return-blurb "return to terminal simulator")
  (setq mode-line-process '(": %s"))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (make-local-variable 'terminal-escape-char)
  (setq terminal-escape-char (default-value 'terminal-escape-char))
  (make-local-variable 'terminal-scrolling)
  (setq terminal-scrolling (default-value 'terminal-scrolling))
  (make-local-variable 'terminal-more-processing)
  (setq terminal-more-processing (default-value 'terminal-more-processing))
  (make-local-variable 'terminal-redisplay-interval)
  (setq terminal-redisplay-interval (default-value 'terminal-redisplay-interval))
  (make-local-variable 'te-width)
  (make-local-variable 'te-height)
  (make-local-variable 'te-process)
  (make-local-variable 'te-pending-output)
  (setq te-pending-output (list 0))
  (make-local-variable 'te-saved-point)
  (setq te-saved-point (point-min))
  (make-local-variable 'te-pending-output-info) ;for the mode line
  (setq te-pending-output-info "")
  (make-local-variable 'inhibit-quit)
  ;(setq inhibit-quit t)
  (make-local-variable 'te-log-buffer)
  (setq te-log-buffer nil)
  (make-local-variable 'te-more-count)
  (setq te-more-count -1)
  (make-local-variable 'te-redisplay-count)
  (setq te-redisplay-count terminal-redisplay-interval)
  ;;>> Nothing can be done about this without decruftifying
  ;;>>  emacs keymaps.
  (make-local-variable 'meta-flag) ;sigh
  (setq meta-flag nil)
  ;(use-local-map terminal-mode-map)
  ;; terminal-mode-hook is called above in function terminal-emulator
  )

;;;; what a complete loss

(defun te-quote-arg-for-sh (fuckme)
  (cond ((string-match "\\`[a-zA-Z0-9-+=_.@/:]+\\'"
		       fuckme)
	 fuckme)
	((not (string-match "[$]" fuckme))
	 ;; "[\"\\]" are special to sh and the lisp reader in the same way
	 (prin1-to-string fuckme))
	(t
	 (let ((harder "")
	       (cretin 0)
	       (stupid 0))
	   (while (cond ((>= cretin (length fuckme))
			 nil)
			;; this is the set of chars magic with "..." in `sh'
			((setq stupid (string-match "[\"\\$]"
						    fuckme cretin))
			 t)
			(t (setq harder (concat harder
						(substring fuckme cretin)))
			   nil))
	     (setq harder (concat harder (substring fuckme cretin stupid)
                                  ;; Can't use ?\\ since `concat'
                                  ;; unfortunately does prin1-to-string
                                  ;; on fixna.  Amazing.
				  "\\"
				  (substring fuckme
					     stupid
					     (1+ stupid)))
		   cretin (1+ stupid)))
	   (concat "\"" harder "\"")))))
