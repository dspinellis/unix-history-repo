;  This autoloaded file implements the "?" command of mhe
(defun 
    (&mh-help		; ? has been typed;  pop up help buffer.
	(save-excursion
	    (pop-to-buffer "MH help")
	    (use-local-map "&mh-keymap")
	    (if (= 0 (buffer-size))
		(insert-string 
"    n: next line			p: previous line\n"
"    d: delete (mark for del)		^: put (mark for moving)\n"
"    u: unmark message			t: type message\n"
"    m: mail a message			i: incorporate new mail\n"
"    r: reply to message 		f: forward a message\n"
"    e: edit message			g: get a new folder\n"
"    b: get a bboard folder		^X^C: exit.\n"
"    !: repeat last ^ command.		x: run 'extended' command\n"
"Also, you can use the following regular Emacs commands:\n"
"^N ^P ^S ^R ^U ^V ESC-V ESC-> ESC-< ^X-N ^X-P ^X-1 ^X-D ^X-^V ESC-X\n"
"\n"
"Edit the buffer marking messages as you wish. When you are done, type ^X^C.\n"
"Then messages marked for deletion will be deleted, messages marked\n"
"for moving will be moved, and MHE will exit. \n\n"
"The keyboard has the same meaning in the type buffer ('t' command) as it\n"
"does in the header buffer; this allows you to type mhe commands when the\n"
"cursor is in the window of a message being typed. This convenience means\n"
"that you cannot edit a message in a type window. The 'e' command is for\n"
"that purpose. 'E' is just like 't', except that the keys have their edit\n"
"meanings rather than their mhe header command meanings.\n\n"
"In any of the submodes, such as editing a message ('e' command), sending\n"
"a message ('m' command), or replying to a message ('r' command), exit with\n"
"^X^C or with your own standard `get out of Emacs' command.\n\n"
"At any time in any mode in any window, you can push down to an instance of\n"
"your login shell with the ^_ command. When you terminate that shell (with\n"
"^D, or whatever your EOF character is), control will return to mhe exactly\n"
"where you left it. You cannot run Emacs or another instance of mhe in this\n"
"shell."
			       ))
	    (beginning-of-file)
	    (setq mode-line-format
		  "{%b}	Type ESC-^V to scroll help window, or ^X-1 to erase it.")
	    (setq buffer-is-modified 0)
	)
    )
)
