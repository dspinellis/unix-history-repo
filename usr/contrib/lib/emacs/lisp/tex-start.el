; This file is for use by TeX82 (see man page) to allow switching to
;  Emacs at a line number given on the command line
; It assumes that it has been called by:
;	emacs -l tex-start -e startline <linenumber> <file>

(defun startline ()
  ;(setq command-line-args (cdr command-line-args))
  (find-file (car (cdr command-line-args-left)))
  (goto-char (point-min))
  (forward-line (1- (string-to-int (car command-line-args-left))))
  (setq command-line-args-left ()))
