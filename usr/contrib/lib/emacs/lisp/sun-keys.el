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
;;; This file is part of GNU Emacs.
;;; 
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but without any warranty.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.
;;; 
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; document "GNU Emacs copying permission notice".   An exact copy
;;; of the document is supposed to have been given to you along with
;;; GNU Emacs so that you can know how you may redistribute it all.
;;; It should be in a file named COPYING.  Among other things, the
;;; copyright notice and this notice must be preserved on all copies.
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
