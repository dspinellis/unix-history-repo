;; Load this file to add a new level (starting at zero)
;; to the Emacs version number recorded in version.el.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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


(insert-file-contents "lisp/version.el")

(re-search-forward "emacs-version \"[0-9.]*")
(insert ".0")

;; Delete the share-link with the current version
;; so that we do not alter the current version.
(delete-file "lisp/version.el")
(write-region (point-min) (point-max) "lisp/version.el" nil 'nomsg)
