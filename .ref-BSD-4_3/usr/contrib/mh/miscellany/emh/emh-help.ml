; emh-help.ml :: implements the emh help command
; Wed Oct  5 12:17:00 1983	/mtr  <mrose@uci-750a>


(if (! (is-bound &mhelp))
    (setq-default &mhelp "MH help"))

(defun

    (&mh-help
	(error-occured (delete-buffer &mhelp))
	(save-excursion 
	    (pop-to-buffer &mhelp)
	    (setq needs-checkpointing 0)
	    (use-local-map "&mh-keymap")
	    (erase-buffer)
	    (insert-string 
"[Use ^U- prefix to specify additional arguments]\n\n"
"  Top level commands:                  ^X-f    list folders\n"
"    ^X-m    send mail                  ^X-r    read mail\n\n"
"  emh mode commands:\n"
"    s: show current message            n: show next message\n"
"    p: show prev message               c: compose new message\n"
"    f: forward current message         r: reply to current message\n"
"    d: delete current message          m: file current message\n"
"    i: incorporate new mail\n"
"    ?: display command summary\n"
	    )
	    (beginning-of-file)
	    (setq mode-string "emh")
	    (setq mode-line-format "  %b: ESC-^V to scroll (%m) %M %[%p%]"))

	(novalue)
    )
)

(novalue)
