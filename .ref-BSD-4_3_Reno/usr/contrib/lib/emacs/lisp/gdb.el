;; Run gdb under Emacs
;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.

;; Some ideas are due to  Masanobu. 

;; This file is part of GNU Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; Description of GDB interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using gdb to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command GDB to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Gdb will be invoked on this file, in a
;; window named *gdb-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the gdb command next on \M-n
;; (def-gdb next "\M-n")

;; This causes the emacs command gdb-next to be defined, and runs
;; gdb-display-frame after the command.

;; gdb-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the gdb window.  For example after a gdb-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the gdb buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; gdb-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'shell)

(defvar gdb-prompt-pattern "^(.*gdb[+]?) *"
  "A regexp to recognize the prompt for gdb or gdb+.") 

(defvar gdb-mode-map nil
  "Keymap for gdb-mode.")

(if gdb-mode-map
   nil
  (setq gdb-mode-map (copy-keymap shell-mode-map))
  (define-key gdb-mode-map "\C-l" 'gdb-refresh))

(define-key ctl-x-map " " 'gdb-break)
(define-key ctl-x-map "&" 'send-gdb-command)

;;Of course you may use `def-gdb' with any other gdb command, including
;;user defined ones.   

(defmacro def-gdb (name key &optional doc)
  (let* ((fun (intern (format "gdb-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'gdb-call cstr))
	  (list 'define-key 'gdb-mode-map key  (list 'quote fun)))))

(def-gdb "step"   "\M-s" "Step one source line with display")
(def-gdb "stepi"  "\M-i" "Step one instruction with display")
(def-gdb "next"   "\M-n" "Step one source line (skip functions)")
(def-gdb "cont"   "\M-c" "Continue with display")

(def-gdb "finish" "\C-c\C-f" "Finish executing current function")
(def-gdb "up"     "\M-u"   "Go up N stack frames (numeric arg) with display")
(def-gdb "down"   "\M-d"   "Go down N stack frames (numeric arg) with display")

(defun gdb-mode ()
  "Major mode for interacting with an inferior Gdb process.
The following commands are available:

\\{gdb-mode-map}

\\[gdb-display-frame] displays in the other window
the last line referred to in the gdb buffer.

\\[gdb-step],\\[gdb-next], and \\[gdb-nexti] in the gdb window,
call gdb to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[gdb-break].

Commands:
Many commands are inherited from shell mode. 
Additionally we have:

\\[gdb-display-frame] display frames file in other window
\\[gdb-step] advance one line in program
\\[gdb-next] advance one line in program (skip over calls).
\\[send-gdb-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gdb-mode)
  (setq mode-name "Inferior Gdb")
  (setq mode-line-process '(": %s"))
  (use-local-map gdb-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'gdb-last-frame)
  (setq gdb-last-frame nil)
  (make-local-variable 'gdb-last-frame-displayed-p)
  (setq gdb-last-frame-displayed-p t)
  (make-local-variable 'gdb-delete-prompt-marker)
  (setq gdb-delete-prompt-marker nil)
  (make-local-variable 'gdb-filter-accumulator)
  (setq gdb-filter-accumulator nil)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern gdb-prompt-pattern)
  (run-hooks 'shell-mode-hook 'gdb-mode-hook))

(defvar current-gdb-buffer nil)

(defvar gdb-command-name "gdb"
  "Pathname for executing gdb.")

(defun gdb (path)
  "Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  (interactive "FRun gdb on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*gdb-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-shell (concat "gdb-" file) gdb-command-name nil "-fullname"
		"-cd" default-directory file)
    (gdb-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'gdb-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gdb-sentinel)
    (gdb-set-buffer)))

(defun gdb-set-buffer ()
  (cond ((eq major-mode 'gdb-mode)
	(setq current-gdb-buffer (current-buffer)))))

;; This function is responsible for inserting output from GDB
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that GDB prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun gdb-filter (proc string)
  (let ((inhibit-quit t))
    (if gdb-filter-accumulator
	(gdb-filter-accumulate-marker proc
				      (concat gdb-filter-accumulator string))
	(gdb-filter-scan-input proc string))))

(defun gdb-filter-accumulate-marker (proc string)
  (setq gdb-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq gdb-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq gdb-last-frame-displayed-p nil)
		  (gdb-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq gdb-filter-accumulator string)))
	(gdb-filter-insert proc "\032")
	(gdb-filter-scan-input proc (substring string 1)))
    (setq gdb-filter-accumulator string)))

(defun gdb-filter-scan-input (proc string)
  (if (equal string "")
      (setq gdb-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (gdb-filter-insert proc (substring string 0 start))
		   (gdb-filter-accumulate-marker proc
						 (substring string start)))
	    (gdb-filter-insert proc string)))))

(defun gdb-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (gdb-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (gdb-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun gdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gdb buffer.
	     (set-buffer obuf))))))


(defun gdb-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (gdb-display-frame))

(defun gdb-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from GDB.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (gdb-set-buffer)
  (and gdb-last-frame (not nodisplay)
       (or (not gdb-last-frame-displayed-p) (not noauto))
       (progn (gdb-display-line (car gdb-last-frame) (cdr gdb-last-frame))
	      (setq gdb-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun gdb-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun gdb-call (command)
  "Invoke gdb COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq gdb-delete-prompt-marker (point-marker))
  (gdb-set-buffer)
  (send-string (get-buffer-process current-gdb-buffer)
	       (concat command "\n")))

(defun gdb-maybe-delete-prompt ()
  (if (and gdb-delete-prompt-marker
	   (> (point-max) (marker-position gdb-delete-prompt-marker)))
      (let (start)
	(goto-char gdb-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq gdb-delete-prompt-marker nil))))

(defun gdb-break ()
  "Set GDB breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (send-string (get-buffer-process current-gdb-buffer)
		 (concat "break " file-name ":" line "\n"))))

(defun gdb-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar gdb-commands nil
  "List of strings or functions used by send-gdb-command.
It is for customization by you.")

(defun send-gdb-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the gdb buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list gdb-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of gdb-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg gdb-commands)))
    (setq addr (gdb-read-address))
    (if (eq (current-buffer) current-gdb-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-gdb-buffer)
    (goto-char (dot-max))
    (insert-string comm)))
