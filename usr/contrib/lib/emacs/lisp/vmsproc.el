;; Run asynchronous VMS subprocesses under Emacs
;; Copyright (C) 1986 Free Software Foundation, Inc.

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

;; Written by Mukesh Prasad.

(defvar display-subprocess-window nil
  "If non-nil, the suprocess window is displayed whenever input is received.")

(defvar command-prefix-string "$ "
  "String to insert to distinguish commands entered by user.")

(defvar subprocess-running nil)
(defvar command-mode-map nil)

(if command-mode-map
    nil
  (setq command-mode-map (make-sparse-keymap))
  (define-key command-mode-map "\C-m" 'command-send-input)
  (define-key command-mode-map "\C-u" 'command-kill-line))

(defun subprocess-input (name str)
   "Handles input from a subprocess.  Called by Emacs."
   (if display-subprocess-window
      (display-buffer subprocess-buf))
   (let ((old-buffer (current-buffer)))
      (set-buffer subprocess-buf)
      (goto-char (point-max))
      (insert str)
      (insert ?\n)
      (set-buffer old-buffer)))

(defun subprocess-exit (name)
   "Called by Emacs upon subprocess exit."
   (setq subprocess-running nil))

(defun start-subprocess ()
   "Spawns an asynchronous subprocess with output redirected to
the buffer *COMMAND*.  Within this buffer, use C-m to send
the last line to the subprocess or to bring another line to
the end."
   (if subprocess-running
       (return t))
   (setq subprocess-buf (get-buffer-create "*COMMAND*"))
   (save-excursion
	(set-buffer subprocess-buf)
	(use-local-map command-mode-map))
   (setq subprocess-running (spawn-subprocess 1 'subprocess-input
                                               'subprocess-exit))
   ;; Initialize subprocess so it doesn't panic and die upon
   ;; encountering the first error.
   (and subprocess-running
        (send-command-to-subprocess 1 "ON SEVERE_ERROR THEN CONTINUE")))

(defun subprocess-command ()
  "Starts asynchronous subprocess if not running and switches to its window."
  (interactive)
  (if (not subprocess-running)
      (start-subprocess))
  (and subprocess-running
      (progn (pop-to-buffer subprocess-buf) (goto-char (point-max)))))

(defun command-send-input ()
  "If at last line of buffer, sends the current line to
the spawned subprocess.  Otherwise brings back current
line to the last line for resubmission."
  (interactive)
  (beginning-of-line)
  (let ((current-line (buffer-substring (point)
                                        (progn (end-of-line) (point)))))
    (if (eobp)
      (progn
        (if (not subprocess-running)
            (start-subprocess))
        (if subprocess-running
          (progn
            (beginning-of-line)
            (send-command-to-subprocess 1 current-line)
            (if command-prefix-string
              (progn (beginning-of-line) (insert command-prefix-string)))
            (next-line 1))))
      ;; else -- if not at last line in buffer
      (end-of-buffer)
      (backward-char)
      (next-line 1)
      (if (string-equal command-prefix-string
                (substring current-line 0 (length command-prefix-string)))
	  (insert (substring current-line (length command-prefix-string)))
          (insert current-line)))))

(defun command-kill-line()
  "Kills the current line.  Used in command mode."
  (interactive)
  (beginning-of-line)
  (kill-line))

(define-key esc-map "$" 'subprocess-command)
