;; Insert or remove underlining (done by overstriking) in Emacs.
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


(defun underline-region (start end)
  "Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "r")
  (save-excursion
   (let ((end1 (make-marker)))
     (move-marker end1 (max start end))
     (goto-char (min start end))
     (while (< (point) end1)
       (or (looking-at "[_\^@- ]")
	   (insert "_"))
       (forward-char 1)))))

(defun ununderline-region (start end)
  "Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "r")
  (save-excursion
   (let ((end1 (make-marker)))
     (move-marker end1 (max start end))
     (goto-char (min start end))
     (while (search-forward "_" end1 t)
       (delete-char -2)))))
