;; Copyright (C) 1985, 1987 Free Software Foundation

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

; Randomize the seed in the random number generator.
(random t)

; Important pinheaddery for GNU Emacs.
; Expects file emacs/etc/yow.lines to be in ITS-style LINS format
;  (ie strings terminated by ascii 0 characters.  Leading whitespace ignored)
; Everything up to the first \000 is a comment.
(defun yow (&optional n interactive)
  "Return or display a Zippy quotation"
  (interactive
    (if current-prefix-arg
	(list (prefix-numeric-value current-prefix-arg) t)
      (list nil t)))
  (if (null yow-vector)
      (setq yow-vector (snarf-yows)))
  (cond (n)
	((>= (setq n (% (random) (length yow-vector))) 0))
	(t (setq n (- n))))
  (let ((yow (aref yow-vector n)))
    (cond ((not interactive)
	   yow)
	  ((not (string-match "\n" yow))
	   (delete-windows-on (get-buffer-create "*Help*"))
	   (message yow))
	  (t
	   (message "Yow!")
	   (with-output-to-temp-buffer "*Help*"
	     (princ yow))))))

(defvar yow-vector nil "Pertinent pinhead statements")
(defun snarf-yows (&optional file)
  (save-excursion
    (let ((buf (generate-new-buffer " yow"))
	  (result '())
	  (cursor-in-echo-area t))
      (message "Am I CONSING yet?...")
      (set-buffer buf)
      (insert-file-contents (or file
				(expand-file-name "yow.lines" exec-directory)))
      (search-forward "\0")
      (while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
	(let ((beg (point)))
	  (search-forward "\0")
	  (setq result (cons (buffer-substring beg (1- (point)))
			     result))))
      (kill-buffer buf)
      (message "I have SEEN the CONSING!!" (length result))
      (apply 'vector (nreverse result)))))

; Yowza!! Feed zippy quotes to the doctor. Watch results.
; fun, fun, fun. Entertainment for hours...
;
; written by Kayvan Aghaiepour

(defun psychoanalyze-pinhead ()
  "Zippy goes to the analyst."
  (interactive)
  (doctor)				; start the psychotherapy
  (if (null yow-vector)
      (setq yow-vector (snarf-yows)))
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (insert-string (yow))
    (sit-for 0)
    (doctor-ret-or-read 1)
    (doctor-ret-or-read 1)))

