;; Parse switches controlling how Emacs interfaces with X window system.
;; Copyright (C) 1986, 1988 Free Software Foundation, Inc.

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

(defconst window-system-version window-system-version
  "*Window system version number now in use.")

(defvar x-sigio-bug nil
  "Non-NIL means don't use interrupts for input when using X.")

(defvar x-processed-defaults nil
  "Non-NIL means that user's X defaults have already been processed.")

(defvar x-switches nil
  "Alist of command switches and values for X window system interface.
You can set this in your init file, if you want some defaults
for these switches.  Example:
  (setq x-switches '((\"-r\" . t) (\"-font\" . \"foo\") (\"-b\" . \"8\")))
This feature is currently broken for X11.")

(if (= window-system-version 10)
    (setq command-switch-alist
	  (append '(("-r" . x-handle-switch)
		    ("-i" . x-handle-switch)
		    ("-font" . x-handle-switch)
		    ("-w" . x-handle-switch)
		    ("-b" . x-handle-switch)
		    ("-ib" . x-handle-switch)
		    ("-fg" . x-handle-switch)
		    ("-bg" . x-handle-switch)
		    ("-bd" . x-handle-switch)
		    ("-cr" . x-handle-switch)
		    ("-ms" . x-handle-switch))
		  command-switch-alist))
  (setq command-switch-alist
	(append '(("-rn" . x-ignore-arg)
		  ("-r" . ignore)
		  ("-i" . ignore)
		  ("-rn" . x-ignore-arg)
		  ("-font" . x-ignore-arg)
		  ("-fn" . x-ignore-arg)
		  ("-wn" . x-ignore-arg)
		  ("-in" . x-ignore-arg)
		  ("-w" . x-ignore-arg)
		  ("-geometry" . x-ignore-arg)
		  ("-b" . x-ignore-arg)
		  ("-ib" . x-ignore-arg)
		  ("-fg" . x-ignore-arg)
		  ("-bg" . x-ignore-arg)
		  ("-bd" . x-ignore-arg)
		  ("-cr" . x-ignore-arg)
		  ("-ms" . x-ignore-arg))
		command-switch-alist)))

(defun x-ignore-arg (&rest ignore)
  (setq command-line-args-left (cdr command-line-args-left)))

;; This is run after the command args are parsed.
(defun x-handle-switch (switch)
  (if (x-handle-switch-1 switch (car command-line-args-left))
      (setq command-line-args-left (cdr command-line-args-left))))

(defun x-handle-switch-1 (switch arg)
  (cond ((string= switch "-r")
	 (x-flip-color)
	 nil)
	((string= switch "-i")
	 (x-set-icon t)
	 nil)
	((string= switch "-font")
	 (x-set-font arg)
	 t)
	((string= switch "-b")
	 (x-set-border-width (string-to-int arg))
	 t)
	((string= switch "-ib")
	 (x-set-internal-border-width (string-to-int arg))
	 t)
	((string= switch "-w")
	 (x-create-x-window arg)
	 t)
	((string= switch "-fg")
	 (x-set-foreground-color arg)
	 t)
	((string= switch "-bg")
	 (x-set-background-color arg)
	 t)
	((string= switch "-bd")
	 (x-set-border-color arg)
	 t)
	((string= switch "-cr")
	 (x-set-cursor-color arg)
	 t)
	((string= switch "-ms")
	 (x-set-mouse-color arg)
	 t)))

;; Convert a string of the form "WWxHH+XO+YO",
;; where WW, HH, XO and YO are numerals,
;; into a list (WW HH XO YO).
;; "xHH" may be omitted; then 0 is used for HH.
;; XO and YO may be preceded by - instead of + to make them negative.
;; Either YO or both XO and YO may be omitted; zero is used.
(defun x-parse-edge-spec (arg)
  (let ((cols-by-font 0)
	(rows-by-font 0)
	(xoffset 0)
	(yoffset 0))
    (if (string-match "^=" arg)
	(setq cols-by-font (x-extract-number))
      (error "Invalid X window size/position spec"))
    (if (string-match "^x" arg)		;get rows-by-font
	(setq rows-by-font (x-extract-number)))
    (if (string-match "^[-+]" arg)
	(setq xoffset (x-extract-number)))
    (if (string-match "^[-+]" arg)
	(setq yoffset (x-extract-number)))
    (or (equal arg "")
	(error "Invalid X window size/position spec"))
    (list cols-by-font rows-by-font xoffset yoffset)))

;; Subroutine to extract the next numeral from the front of arg,
;; returning it and shortening arg to remove its text.
;; If arg is negative, subtract 1 before returning it.
(defun x-extract-number ()
  (if (string-match "^[x=]" arg)
      (setq arg (substring arg 1)))
  (or (string-match "[-+]?[0-9]+" arg)
      (error "Invalid X window size/position spec"))
  (prog1
      (+ (string-to-int arg)
	 (if (string-match "^-" arg) -1 0))
    (setq arg
	  (substring arg
		     (or (string-match "[^0-9]" arg 1)
			 (length arg))))))

(defun x-get-default-args ()
  (setq x-processed-defaults t)
  (let (value)
    (if (not (string= (setq value (x-get-default "bodyfont")) ""))
	(x-handle-switch-1 "-font"  value))
    (if (string-match "on" (x-get-default "bitmapicon"))
	(x-handle-switch-1 "-i" t))
    (if (not (string= (setq value (x-get-default "borderwidth")) ""))
	(x-handle-switch-1 "-b" value))
    (if (not (string= (setq value (x-get-default "internalborder")) ""))
	(x-handle-switch-1 "-ib" value))
    (if (not (string= (setq value (x-get-default "foreground")) ""))
	(x-handle-switch-1 "-fg" value))
    (if (not (string= (setq value (x-get-default "background")) ""))
	(x-handle-switch-1 "-bg" value))
    (if (not (string= (setq value (x-get-default "border")) ""))
	(x-handle-switch-1 "-bd" value))
    (if (not (string= (setq value (x-get-default "cursor")) ""))
	(x-handle-switch-1 "-cr" value))
    (if (not (string= (setq value (x-get-default "mouse")) ""))
	(x-handle-switch-1 "-ms" value))
    (if (string-match "on" (x-get-default "reversevideo"))
	(x-handle-switch-1 "-r" t))))

(defun x-new-display (display)
  "This function takes one argument, the display where you wish to
continue your editing session.  Your current window will be unmapped and
the current display will be closed.  The new X display will be opened and
the rubber-band outline of the new window will appear on the new X display."
  (interactive "sDisplay to switch emacs to:  ")
  (x-change-display display)
  (x-get-default-args))

;; So far we have only defined some functions.
;; Now we start processing X-related switches
;; and redefining commands and variables,
;; only if Emacs has been compiled to support direct interface to X.

(if (eq window-system 'x)
    (progn
      (require 'x-mouse)
      (if (= window-system-version 10)
	  (progn
	    ;; xterm.c depends on using interrupt-driven input.
	    (set-input-mode t nil)

	    ;; Not defvar!  This is not DEFINING this variable, just specifying
	    ;; a value for it.
	    (setq window-setup-hook 'x-pop-up-window)

	    ;; Process switch settings made by .emacs file.
	    (while x-switches
	      (x-handle-switch-1 (car (car x-switches)) (cdr (car x-switches)))
	      (setq x-switches (cdr x-switches)))))

      ;; On certain systems, turn off use of sigio, because it's broken.
      (if x-sigio-bug
	  (set-input-mode nil nil))

      (put 'suspend-emacs 'disabled
	   "Suspending a program running in an X window is silly
and you would not be able to start it again.  Just switch windows instead.\n")
      (setq suspend-hook '(lambda () (error "Suspending an emacs running under X makes no sense")))
      (substitute-key-definition 'suspend-emacs nil global-map)
      (substitute-key-definition 'suspend-emacs nil esc-map)
      (substitute-key-definition 'suspend-emacs nil ctl-x-map)
      ;; Not needed any more -- done in C.
      ;; (if (not x-processed-defaults) (x-get-default-args))
))
