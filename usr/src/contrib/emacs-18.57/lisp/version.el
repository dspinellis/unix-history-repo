;; Record version number of Emacs.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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


;; The following line is modified automatically
;; by loading inc-version.el, each time a new Emacs is dumped.
(defconst emacs-version "18.57.37" "\
Version numbers of this version of Emacs.")

(defconst emacs-build-time (current-time-string) "\
Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defun emacs-version () "\
Return string describing the version of Emacs that is running."
  (interactive)
  (if (interactive-p)
      (message "%s" (emacs-version))
    (format "GNU Emacs %s of %s %s on %s (%s)"
	    emacs-version
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time (string-match "[0-9]*$" emacs-build-time))
	    emacs-build-system system-type)))

;;Local variables:
;;version-control: never
;;End:
