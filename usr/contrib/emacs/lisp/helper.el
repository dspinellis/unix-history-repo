;; helper - utility help package for modes which want to provide help
;; without relinquishing control, e.g. `electric' modes.

;; Copyright (C) 1985 Richard M. Stallman and K. Shane Hartman

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


(provide 'helper)			; hey, here's a helping hand.

;; Bind this to a string for <blank> in "... Other keys <blank>".
;; Helper-help uses this to construct help string when scrolling.
;; Defaults to "return"
(defvar Helper-return-blurb nil)

;; Bind this to your major mode (for documentation by C-h m)
;; Defaults to major-mode.
(defvar Helper-major-mode nil)

;; Bind this to your pretty mode name (for C-h m)
;; Defaults to mode-name
(defvar Helper-mode-name nil) 

;; Keymap implementation doesn't work too well for non-standard loops.
;; But define it anyway for those who can use it.  Non-standard loops
;; will probably have to use Helper-help.  You can't autoload the
;; keymap either.


(defvar Helper-help-map nil)
(if Helper-help-map
    nil
  (setq Helper-help-map (make-keymap))
  (fillarray Helper-help-map 'undefined)
  (define-key Helper-help-map "m" 'Helper-describe-mode)
  (define-key Helper-help-map "c" 'Helper-describe-key-briefly)
  (define-key Helper-help-map "k" 'Helper-describe-key)
  (define-key Helper-help-map "f" 'Helper-describe-function)
  (define-key Helper-help-map "v" 'Helper-describe-variable)
  (define-key Helper-help-map "?" 'Helper-help-options)
  (define-key Helper-help-map "\C-h" 'Helper-help-options)
  (fset 'Helper-help-map Helper-help-map))

(defun Helper-help-scroller ()
  (save-window-excursion
    (goto-char (window-start (selected-window)))
    (if (get-buffer-window "*Help*")
	(pop-to-buffer "*Help*")
      (switch-to-buffer "*Help*"))
    (goto-char (point-min))
    (let ((continue t) state)
      (while continue
	(setq state (+ (* 2 (if (pos-visible-in-window-p (point-max)) 1 0))
		       (if (pos-visible-in-window-p (point-min)) 1 0)))
	(message
	 (nth state
	      '("Space forward, Delete back. Other keys %s"
		"Space scrolls forward. Other keys %s"
		"Delete scrolls back. Other keys %s"
		"Type anything to %s"))
	 (or (and (boundp 'Helper-return-blurb) Helper-return-blurb)
	     "return"))
	(setq continue (read-char))
	(cond ((and (memq continue '(?\ ?\C-v)) (< state 2))
	       (scroll-up))
	      ((= continue ?\C-l)
	       (recenter))
	      ((and (= continue ?\177) (zerop (% state 2)))
	       (scroll-down))
	      (t (setq continue nil)))))))

(defun Helper-help-options ()
  "Describe help options."
  (interactive)
  (message "c (key briefly), m (mode), k (key), v (variable), f (function)")
  (sit-for 4))

(defun Helper-describe-key-briefly (key)
  "Briefly describe binding of KEYS."
  (interactive "kDescribe key briefly: ")
  (describe-key-briefly key)
  (sit-for 4))

(defun Helper-describe-key (key)
  "Describe binding of KEYS."
  (interactive "kDescribe key: ")
  (save-window-excursion (describe-key key))
  (Helper-help-scroller))

(defun Helper-describe-function ()
  "Describe a function.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-function))
  (Helper-help-scroller))

(defun Helper-describe-variable ()
  "Describe a variable.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-variable))
  (Helper-help-scroller))

(defun Helper-describe-mode ()
  "Describe the current mode."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*Help*"))
    (erase-buffer)
    (insert (or (and (boundp 'Helper-mode-name) Helper-mode-name) mode-name)
	    " Mode\n"
	    (documentation (or (and (boundp 'Helper-major-mode) Helper-major-mode)
			       major-mode))))
  (Helper-help-scroller))

(defun Helper-describe-bindings ()
  "Describe current local key bindings."
  (interactive)
  (message "Making binding list...")
  (let ((global-map (make-sparse-keymap))) ; fake out describe-bindings
    (save-window-excursion (describe-bindings)))
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (point-min) (point))
    (while (search-forward "undefined" nil t)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))))
  (Helper-help-scroller))

(defun Helper-help ()
  "Provide help for current mode."
  (interactive)
  (let ((continue t) c)
    (while continue
      (message "Help (Type ? for further options)")
      (setq c (upcase (read-char)))
      (if (setq continue (or (= c ??) (= c ?\C-h)))
	  (Helper-help-options)
	(cond ((= c ?M)
	       (Helper-describe-mode))
	      ((= c ?C)
	       (call-interactively 'Helper-describe-key-briefly))
	      ((= c ?K)
	       (call-interactively 'Helper-describe-key))
	      ((= c ?F)
	       (Helper-describe-function))
	      ((= c ?V)
	       (Helper-describe-variable))
	      (t (ding)))))))

