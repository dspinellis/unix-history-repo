;;;
;;; Support (cleanly) for Sun function keys.  Provides help facilities,
;;; better diagnostics, etc.
;;;
;;; To use: make sure your .ttyswrc binds 'F1' to <ESC> * F1 <CR> and so on.
;;;         load this lot from your start_up
;;;
;;; 
;;;    Copyright (C) 1986 Free Software Foundation, Inc.
;;; 
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

;;;
;;; Batten@uk.ac.bham.multics (Ian G. Batten)
;;;

(defun sun-function-keys-dispatch (arg)
  "Dispatcher for function keys."
  (interactive "p")
  (let* ((key-stroke (read t))
         (command (assq key-stroke sun-function-keys-command-list)))
    (cond (command (funcall (cdr command) arg))
          (t (error "Unbound function key %s" key-stroke)))))

(defvar sun-function-keys-command-list 
  '((F1 . sun-function-keys-describe-bindings)
    (R8 . previous-line)                ; arrow keys
    (R10 . backward-char)
    (R12 . forward-char)
    (R14 . next-line)))

(defun sun-function-keys-bind-key (arg1 arg2)
  "Bind a specified key."
  (interactive "xFunction Key Cap Label:
CCommand To Use:")
  (setq sun-function-keys-command-list 
        (cons (cons arg1 arg2) sun-function-keys-command-list)))

(defun sun-function-keys-describe-bindings (arg)
  "Describe the function key bindings we're running"
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (sun-function-keys-write-bindings
     (sort (copy-sequence sun-function-keys-command-list)
           '(lambda (x y) (string-lessp (car x) (car y)))))))

(defun sun-function-keys-write-bindings (list)
  (cond ((null list)
         t)
        (t
         (princ (format "%s: %s\n" 
                        (car (car list))
                        (cdr (car list))))
         (sun-function-keys-write-bindings (cdr list)))))
    
(global-set-key "\e*" 'sun-function-keys-dispatch)

(make-variable-buffer-local 'sun-function-keys-command-list)
