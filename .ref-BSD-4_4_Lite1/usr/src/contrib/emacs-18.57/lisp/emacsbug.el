;; Command to report Emacs bugs to appropriate mailing list.
;; Not fully installed because it can work only on Internet hosts.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


;; >> This should be an address which is accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(defvar bug-gnu-emacs "bug-gnu-emacs@prep.ai.mit.edu"
  "Address of site maintaining mailing list for Gnu emacs bugs.")

(defun report-emacs-bug (topic)
  "Report a bug in Gnu emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (mail nil bug-gnu-emacs topic)
  (goto-char (point-max))
  (insert "\nIn " (emacs-version) "\n\n")
  (message (substitute-command-keys "Type \\[mail-send] to send bug report.")))

