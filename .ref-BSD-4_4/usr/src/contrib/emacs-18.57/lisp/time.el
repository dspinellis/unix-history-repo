;; Display time and load in mode line of Emacs.
;; Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.

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


(defvar display-time-mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
Default is system-dependent, and is the same as used by Rmail.")

(defvar display-time-process nil)

(defvar display-time-interval 60
  "*Seconds between updates of time in the mode line.")

(defvar display-time-string nil)

(defun display-time ()
  "Display current time and load level in mode line of each buffer.
Updates automatically every minute.
If display-time-day-and-date is non-nil, the current day and date
are displayed as well."
  (interactive)
  (let ((live (and display-time-process
		   (eq (process-status display-time-process) 'run))))
    (if (not live)
	(progn
	  (if display-time-process
	      (delete-process display-time-process))
	  (or global-mode-string (setq global-mode-string '("")))
	  (or (memq 'display-time-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(display-time-string))))
	  (setq display-time-string "")
	  (setq display-time-process
		(start-process "display-time" nil
			       (concat exec-directory "wakeup")
			       (int-to-string display-time-interval)))
	  (process-kill-without-query display-time-process)
	  (set-process-sentinel display-time-process 'display-time-sentinel)
	  (set-process-filter display-time-process 'display-time-filter)))))

(defun display-time-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq display-time-string ""))
  ;; Force mode-line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun display-time-filter (proc string)
  (let ((time (current-time-string))
	(load (condition-case ()
		  (if (zerop (car (load-average))) ""
		    (format "%03d" (car (load-average))))
		(error "")))
	(mail-spool-file (or display-time-mail-file
			     (getenv "MAIL")
			     (concat rmail-spool-directory
				     (or (getenv "LOGNAME")
					 (getenv "USER")
					 (user-login-name)))))
	hour pm)
    (setq hour (read (substring time 11 13)))
    (setq pm (>= hour 12))
    (if (> hour 12)
	(setq hour (- hour 12))
      (if (= hour 0)
	  (setq hour 12)))
    (setq display-time-string
	  (concat (format "%d" hour) (substring time 13 16)
		  (if pm "pm " "am ")
		  (if (string= load "")
		      ""
		    (concat (substring load 0 -2) "." (substring load -2)))
		  (if (and (file-exists-p mail-spool-file)
			   ;; file not empty?
			   (> (nth 7 (file-attributes mail-spool-file)) 0))
		      " Mail"
		    "")))
    ;; Append the date if desired.
    (if display-time-day-and-date
	(setq display-time-string
	      (concat (substring time 0 11) display-time-string))))
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))
