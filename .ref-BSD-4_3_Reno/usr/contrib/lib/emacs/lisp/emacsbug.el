;; Command to report Emacs bugs to appropriate mailing list.
;; Not fully installed because it can work only on Internet hosts.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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

