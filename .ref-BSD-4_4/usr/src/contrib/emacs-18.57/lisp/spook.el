;; Spook phrase utility
;; Copyright (C) 1988 Free Software Foundation

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


; Steve Strassmann (straz@media-lab.media.mit.edu) didn't write
; this, and even if he did, he really didn't mean for you to use it
; in an anarchistic way.
; May 1987

; To use this:
;  Make sure you have the variable SPOOK-PHRASES-FILE pointing to 
;  a valid phrase file. Phrase files are in the same format as
;  zippy's yow.lines (ITS-style LINS format). 
;  Strings are terminated by ascii 0 characters. Leading whitespace ignored.
;  Everything up to the first \000 is a comment.
;
;  Just before sending mail, do M-x spook.
;  A number of phrases will be inserted into your buffer, to help
;  give your message that extra bit of attractiveness for automated
;  keyword scanners.

; Variables
(defvar spook-phrases-file (concat exec-directory "spook.lines")
   "Keep your favorite phrases here.")

(defvar spook-phrase-default-count 15
   "Default number of phrases to insert")

(defvar spook-vector nil
  "Important phrases for NSA mail-watchers")

; Randomize the seed in the random number generator.
(random t)

; Call this with M-x spook.
(defun spook ()
  "Adds that special touch of class to your outgoing mail."
  (interactive)
  (if (null spook-vector)
      (setq spook-vector (snarf-spooks)))
  (shuffle-vector spook-vector)
  (let ((start (point)))
    (insert ?\n)
    (spook1 (min (- (length spook-vector) 1) spook-phrase-default-count))
    (insert ?\n)
    (fill-region-as-paragraph start (point) nil)))

(defun spook1 (arg)
  "Inserts a spook phrase ARG times."
  (cond ((zerop arg) t)
	(t (insert (aref spook-vector arg))
	   (insert " ")
	   (spook1 (1- arg)))))

(defun snarf-spooks ()
  "Reads in the phrase file"
  (message "Checking authorization...")
  (save-excursion
    (let ((buf (generate-new-buffer "*spook*"))
	  (result '()))
      (set-buffer buf)
      (insert-file-contents (expand-file-name spook-phrases-file))
      (search-forward "\0")
      (while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
	(let ((beg (point)))
	  (search-forward "\0")
	  (setq result (cons (buffer-substring beg (1- (point)))
			     result))))
      (kill-buffer buf)
      (message "Checking authorization... Approved.")
      (setq spook-vector (apply 'vector result)))))

(defun pick-random (n)
  "Returns a random number from 0 to N-1 inclusive."
  (% (logand 0777777 (random)) n))

; Thanks to Ian G Batten <BattenIG@CS.BHAM.AC.UK>
; [of the University of Birmingham Computer Science Department]
; for the iterative version of this shuffle.
;
(defun shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)"
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (pick-random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)
