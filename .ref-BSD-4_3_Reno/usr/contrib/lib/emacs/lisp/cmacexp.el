
(defun c-macro-expand (beg end)
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  (interactive "r")
  (let ((outbuf (get-buffer-create "*Macroexpansion*"))
	(tempfile "%%macroexpand%%")
	process
	last-needed)
    (save-excursion
      (set-buffer outbuf)
      (erase-buffer))
    (setq process (start-process "macros" outbuf "/lib/cpp"))
    (set-process-sentinel process '(lambda (&rest x)))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char beg)
	(beginning-of-line)
	(setq last-needed (point))
	(if (re-search-backward "^[ \t]*#" nil t)
	    (progn
	      ;; Skip continued lines.
	      (while (progn (end-of-line) (= (preceding-char) ?\\))
		(forward-line 1))
	      ;; Skip the last line of the macro definition we found.
	      (forward-line 1)
	      (setq last-needed (point)))))
      (write-region (point-min) last-needed tempfile nil 'nomsg)
      (process-send-string process (concat "#include \"" tempfile "\"\n"))
      (process-send-string process "\n")
      (process-send-region process beg end)
      (process-send-string process "\n")
      (process-send-eof process))
    (while (eq (process-status process) 'run)
      (accept-process-output))
    (delete-file tempfile)
    (save-excursion
      (set-buffer outbuf)
      (goto-char (point-max))
      (re-search-backward "\n# [12] \"\"")
      (forward-line 2)
      (while (eolp) (delete-char 1))
      (delete-region (point-min) (point)))
    (display-buffer outbuf)))
