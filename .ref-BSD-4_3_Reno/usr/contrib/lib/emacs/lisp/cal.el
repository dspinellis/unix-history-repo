;; Record version number of Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

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
;;
;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@a.cs.uiuc.edu           1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801
;;
;; The author gratefully acknowledges the patient help of Richard Stallman
;; in making this function into a reasonable piece of code!
;;
;; Modification for month-offset arguments suggested and implemented by
;;  Constantine Rasmussen            Sun Microsystems, East Coast Division
;;  (617) 671-0404                   2 Federal Street;  Billerica, Ma.  01824
;;  ARPA: cdr@sun.com   USENET: {cbosgd,decvax,hplabs,seismo}!sun!suneast!cdr
;;
;; Modification to mark current day with stars suggested by
;;  Franklin Davis		     Thinking Machines Corp
;;  (617) 876-1111                   245 First Street, Cambridge, MA  02142
;;  fad@think.com

(defvar calendar-hook nil
  "List of functions called after the calendar buffer has been prepared with
the calendar of the current month.  This can be used, for example, to highlight
today's date with asterisks--a function star-date is included for this purpose.
The variable offset-calendar-hook is the list of functions called when the
calendar function was called for a past or future month.")

(defvar offset-calendar-hook nil
  "List of functions called after the calendar buffer has been prepared with
the calendar of a past or future month.  The variable calendar-hook is the
list of functions called when the calendar function was called for the
current month.")

(defun calendar (&optional month-offset)
  "Display three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

An optional prefix argument ARG causes the calendar displayed to be
ARG months in the future if ARG is positive or in the past if ARG is
negative; in this case the cursor goes on the first day of the month.

The Gregorian calendar is assumed.

After preparing the calendar window, the hooks calendar-hook are run
when the calendar is for the current month--that is, the was no prefix
argument.  If the calendar is for a future or past month--that is, there
was a prefix argument--the hooks offset-calendar-hook are run.  Thus, for
example, setting calendar-hooks to 'star-date will cause today's date to be
replaced by asterisks to highlight it in the window."
  (interactive "P")
  (if month-offset (setq month-offset (prefix-numeric-value month-offset)))
  (let ((today (make-marker)))
    (save-excursion
      (set-buffer (get-buffer-create "*Calendar*"))
      (setq buffer-read-only t)
      (let*
	  ((buffer-read-only nil)
	   ;; Get today's date and extract the day, month and year.
	   (date (current-time-string))
	   (garbage (string-match
		      " \\([A-Z][a-z][a-z]\\) *\\([0-9]*\\) .* \\([0-9]*\\)$"
		      date))
	   (day (or (and month-offset 1) 
		    (string-to-int
		      (substring date (match-beginning 2) (match-end 2)))))
	   (month
	     (cdr (assoc
                    (substring date (match-beginning 1) (match-end 1))
                    '(("Jan" . 1) ("Feb" . 2)  ("Mar" . 3)  ("Apr" . 4)
                      ("May" . 5) ("Jun" . 6)  ("Jul" . 7)  ("Aug" . 8)
                      ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))
	   (year (string-to-int
		   (substring date (match-beginning 3) (match-end 3)))))
	(erase-buffer)
	;; If user requested a month in the future or the past,
	;; advance the variables MONTH and YEAR to describe that one.
	(cond
          (month-offset
            (let ((year-month (+ (+ (* year 12) (- month 1)) month-offset)))
              (setq month (+ (% year-month 12) 1))
              (setq year (/ year-month 12)))))
	;; Generate previous month, starting at left margin.
	(generate-month;; previous month
	  (if (= month 1) 12 (1- month))
	  (if (= month 1) (1- year) year)
	  0)
	;; Generate this month, starting at column 24,
	;; and record where today's date appears, in the marker TODAY.
	(goto-char (point-min))
	(set-marker today (generate-month month year 24 day))
	;; Generate the following month, starting at column 48.
	(goto-char (point-min))
	(generate-month
	  (if (= month 12) 1 (1+ month))
	  (if (= month 12) (1+ year) year)
	  48)))
    ;; Display the buffer and put cursor on today's date.
    ;; Do it in another window, but if this buffer is already visible,
    ;; just select its window.
    (pop-to-buffer "*Calendar*")
    (goto-char (marker-position today))
    ;; Make TODAY point nowhere so it won't slow down buffer editing until GC.
    (set-marker today nil))
  ;; Make the window just tall enough for its contents.
  (let ((h (1- (window-height)))
        (l (count-lines (point-min) (point-max))))
    (or (one-window-p t)
        (<= h l)
        (shrink-window (- h l))))
  (if month-offset
      (run-hooks 'offset-calendar-hook)
      (run-hooks 'calendar-hook)))

(defun leap-year-p (year)
  "Returns true if YEAR is a Gregorian leap year, and false if not."
  (or
    (and (=  (% year   4) 0)
         (/= (% year 100) 0))
    (= (% year 400) 0)))

(defun day-number (month day year)
  "Return day-number within year (origin-1) of the date MONTH DAY YEAR.
For example, (day-number 1 1 1987) returns the value 1,
while (day-number 12 31 1980) returns 366."
;;
;; an explanation of the calculation can be found in PascAlgorithms by
;; Edward and Ruth Reingold, Scott-foresman/Little, Brown, 1988.
;;
  (let ((day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
        (progn
          (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
          (if (leap-year-p year)
              (setq day-of-year (1+ day-of-year)))))
    day-of-year))

(defun day-of-week (month day year)
  "Returns the day-of-the-week index of MONTH DAY, YEAR.
Value is 0 for Sunday, 1 for Monday, etc."
;;
;; Done by calculating the number of days elapsed since the (imaginary)
;; Gregorian date Sunday, December 31, 1 BC and taking that number mod 7.
;;
  (%
    (-
      (+ (day-number month day year)
         (* 365 (1- year))
         (/ (1- year) 4))
      (let ((correction (* (/ (1- year) 100) 3)))
        (if (= (% correction 4) 0)
            (/ correction 4)
            (1+ (/ correction 4)))))
    7))

(defun generate-month (month year indent &optional day)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar, inserted
in the buffer starting at the line on which point is currently located, but
indented INDENT spaces.  The position in the buffer of the optional
parameter DAY is returned.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on
the line."
  (let* ((first-day-of-month (day-of-week month 1 year) 7)
         (first-saturday (- 7 first-day-of-month))
         (last-of-month
           (if (and (leap-year-p year) (= month 2))
               29
               (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))
         (month-name
           (aref ["January" "February" "March" "April" "May" "June"
                  "July" "August" "September" "October" "November" "December"]
                  (1- month))))
    (insert-indented (format "   %s %d" month-name year) indent t)
    (insert-indented " S  M Tu  W Th  F  S" indent t)
    (insert-indented "" indent);; move point to appropriate spot on line
    (let ((i 0))               ;; add blank days before the first of the month
      (while (<= (setq i (1+ i)) first-day-of-month)
        (insert "   ")))
    (let ((i 0)
          (day-marker))        ;; put in the days of the month
      (while (<= (setq i (1+ i)) last-of-month)
        (insert (format "%2d " i))
        (and
          day
          (= i day)            ;; save the location of the specified day
          (setq day-marker (- (point) 2)))
        (and (= (% i 7) (% first-saturday 7))
             (/= i last-of-month)
             (insert-indented "" 0 t)        ;; force onto following line
             (insert-indented "" indent)))   ;; go to proper spot on line
      day-marker)))

(defun insert-indented (string indent &optional newline)
  "Insert STRING at column INDENT.
If the optional parameter NEWLINE is true, leave point at start of next
line, inserting a newline if there was no next line; otherwise, leave point
after the inserted text.  Value is always `t'."
  ;; Try to move to that column.
  (move-to-column indent)
  ;; If line is too short, indent out to that column.
  (if (< (current-column) indent)
      (indent-to indent))
  (insert string)
  ;; Advance to next line, if requested.
  (if newline
      (progn
	(end-of-line)
	(if (eobp)
            (newline)
	  (forward-line 1))))
  t)

(defun star-date ()
  "Replace today's date with asterisks in the calendar window.
This function can be used with the calendar-hook run after the
calendar window has been prepared."
  (let ((buffer-read-only nil))
    (forward-char 1)
    (delete-backward-char 2)
    (insert "**")
    (backward-char 1)))

