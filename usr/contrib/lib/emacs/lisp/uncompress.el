(defun uncompress-while-visiting ()
  "Temporary \"major mode\" used for .Z files, to uncompress the contents.
It then selects a major mode from the uncompressed file name and contents."
  (if (and (not (null buffer-file-name))
	   (string-match "\\.Z$" buffer-file-name))
      (set-visited-file-name
       (substring buffer-file-name 0 (match-beginning 0))))
  (message "Uncompressing...")
  (shell-command-on-region (point-min) (point-max) "uncompress" t)
  (message "Uncompressing...done")
  (set-buffer-modified-p nil)
  (normal-mode))

(setq auto-mode-alist
      (cons '("\\.Z$" . uncompress-while-visiting) auto-mode-alist))

(defun find-compressed-version ()
  "Hook to read and uncompress the compressed version of a file."
  ;; Just pretend we had visited the compressed file,
  ;; and uncompress-while-visiting will do the rest.
  (if (file-exists-p (concat buffer-file-name ".Z"))
      (progn
	(setq buffer-file-name (concat buffer-file-name ".Z"))
	(insert-file-contents buffer-file-name t)
	(goto-char (point-min))
	(setq error nil)
	t)))

(setq find-file-not-found-hooks
      (cons 'find-compressed-version find-file-not-found-hooks))
