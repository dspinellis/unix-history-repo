Date: 25 Apr 1983 10:29-PDT (Monday)
From: Jim Rees  <jim@uw-beaver>
Subject: rnews.ml
Message-Id: <83/04/25 1029.783@uw-beaver>

	Emacs Readnews Distribution

This is an mlisp package, rnews.ml, for reading the news in Gosling
Emacs.  It is similar to rmail.ml, and in fact depends on some of the
functions in rmail.ml.  Our rmail.ml is modified, but I think this will
also work with the unmodified rmail.ml.

This version is for use with news 2.10.  There is a different version for
use with news 2.9.

To install rnews, install rnews.ml in your emacs/maclib directory.  In your
profile, put an autoload for rnews.ml:

	(autoload "rnews" "rnews.ml")

Then enter emacs and do ESC-X-rnews.

The rnews directory is kept in your ~/Messages directory, so if you
don't use rmail you might have to make this directory before you can
use rnews.

There is an info page, written by grkermit!chris.  To install it, change to
your Emacs databases directory and run "dbadd info emacs:rnews" with the
info page as standard input.  To be complete you should change all the info
nodes pointing to rnews, for example "up", "previous", and "next", but it
will work if you don't change them.

If you have any suggestions or criticisms, or want your name added to the
distribution list for new releases and updates, send mail to
emacs-rnews-request at uw-beaver via uucp or Arpanet.  You can send mail to
the list by mailing to emacs-rnews at uw-beaver.

Here are the info page and rnews.ml:

File: emacs     Node: rnews     Up: Packages

 Unix Emacs readnews facility.

"rnews" is used for reading news.  Executing it places your news
directory into a window and enters a special command interpretation loop.
The commands that it understands are:

p          move to the previous article.
n          move to the next article.
f          move forward in the current article.
b and <backspace>
           move backward in the current article.
d          delete the current article. (the one indicated by '>')
u          undelete the current article.
D          delete the article at the cursor.
U          undelete the article at the cursor.
r          reply to the current article.
m          enter smail, to send mail.
a          append the current article to a file.
F          Post a followup to the current article.
P          Post an article.
q          quit out of Rnews, saving undeleted articles.
?          display the info page for rnews
<          go to the first article
>          go to the last article
s          asks for a number, skips down that number of articles
g          asks for a number, goes to the article that many lines from the top
`          decrypts a net.jokes "rot 13" article
H          show the entire article header, including "junk" lines
^X-U       unsubscribe to the current group
:          execute an emacs extended command
<space> and <return>
           make the article at the cursor be the current article

When in Post mode:

^Xd        positions you in the Distribution: field of the message,
           creating it if necessary.


While in rnews, the current article is indicated with an ">" on the left
margin.  You can move the cursor around with C-N, C-P, C-V, arrow keys,
searches etc.  If you move the cursor, and want to see an article far from
the current article, <space> or <return> makes the article at the cursor
current.

If you provide an argument to the rnews command (usually with ^U) you will
be put into rnews, but the directory will not be updated to include new
articles.  This is much faster and is useful, for example, if you just want
to post a new article or re-read an old one.

------------------------------ Rnews.ml----------------------------------
(if (! (is-bound rmail-default-log))
    (load "rmail.ml"))
(message "Loading the news system, please wait...")
(sit-for 0)

; Unix Emacs readnews facility.

; "rnews" is used for reading news.  Executing it places your news
; directory into a window and enters a special command interpretation loop.
; The commands that it understands are:
;  p	move to the previous message.
;  n	move to the next message.
;  f	move forward in the current message.
;  b	move backward in the current message.
;  d	delete the current message.
;  u	undelete the last deleted message.
;  D	delete the article at the cursor.
;  U    undelete the article at the cursor.
;  r	reply to the current message.
;  m	enter smail, to send mail.
;  a	append the current message to a file.
;  F	Post a followup to the current message.
;  P	Post a message.
;  q	quit out of RMail, appending all undeleted messages to mbox.
;  ?	display the info page for rnews
;  <	go to the first article
;  >	go to the last article
;  s	asks for a number, skips down that number of articles
;  g	asks for a number, goes to the article that many lines from the top
;  `	decrypts a net.jokes "rot 13" article
;  H	show the entire article header, including "junk" lines
;  ^X-U	unsubscribe to the current group
;  :	execute an emacs extended command
;  <space> and <return>
;	   make the article at the cursor be the current article

; When in Post mode,
;  ^Xd	positions you in the Distribution: field of the message,
; 	creating it if necessary.

; If you want replies sent back along the path by which they arrived,
; change the 1 in (setq internet-replies 1) to 0.  You will need to do
; this if your mail program doesn't do automatic uucp routing.

; "smail" is used for sending mail.  It places you in a buffer for
; constructing the message and locally defines a few commands:
;  ^X^S	send the mail -- if all went well the window will disappear,
;	otherwise a message indicating which addresses failed will appear
;	at the bottom of the acreen.  Unfortunatly, the way the mailers on
;	Unix work, the message will have been sent to those addresses which
;	succeded and not to the others, so you have to delete some
;	addresses and fix up the others before you resend the message.
;  ^Xt	positions you in the To: field of the message.
;  ^Xc	positions you in the Cc: field of the message, creating it if it
;	doesn't already exist.
; 		The abbrev facility is used for mail address expansion,
; 		the file /usr/local/lib/emacs/RMailAbbrevs should contain
; 		abbrev definitions to expand login names to their
;		proper mail address.  This gets used at CMU since we have
;		7 VAXen, 4 10's and countless 11's;  remembering where a
;		person usually logs in is nearly impossible.
;  ^Xs	positions you in the Subject: field of the message.
;  ^Xa	positions you to the end of the body of the message, ready to
; 	append more text.

(defun
    (rnews nbx internet-replies
	this-from this-id this-length

	(setq internet-replies 1)
	(setq nbx (concat (getenv "HOME") "/Messages/Newsbox"))
	(message "Please wait while I read the news...")
	(sit-for 0)
	(save-window-excursion
	    (pop-to-buffer "rnews-directory")
	    (setq mode-line-format
		(concat "     News from message file "
		    (substr nbx 1 -1)
		    "      %M   %[%p%]"))
	    (setq mode-string "RNews")
	    (erase-buffer)
	    (set-mark)
	    (if (! prefix-argument-provided)
		(filter-region (concat "readnews -e >> " nbx)))
	    (read-file nbx)
	    (end-of-file)
	    (setq case-fold-search 0)
	    (if (error-occured (re-search-reverse "^[>N ]"))
		(beginning-of-file)
		(next-line)
	    )
	    (error-occured
		(re-replace-string "^" "N "))
	    (setq case-fold-search 1)
	    (save-excursion
		(temp-use-buffer "Full Header")
		(setq needs-checkpointing 0)
	    )
	    (rnews-position)
	    (rnews-mark)
	    (sit-for 0)
	    (message "Type ^C to exit rnews; ? for help")
	    (recursive-edit)
	    (pop-to-buffer "rnews-directory")
	    (rnews-erase-messages)
	    (if buffer-is-modified (write-current-file))
	    (temp-use-buffer "current-message")
	    (setq buffer-is-modified 0)
	)
	(novalue)
    )
)

(defun
    (rnews-position
	(beginning-of-line)
	(if (! (looking-at "^>"))
	    (progn
		(beginning-of-file)
		(if (error-occured (re-search-forward "^>"))
		    (if (error-occured (re-search-forward "^N"))
			(progn
			    (end-of-file)
			    (previous-line)
			)
		    )
		)
		(beginning-of-line)
	    )
	)
    )
)

(defun
    (rnews-pickup rnews-file
	(beginning-of-line)
	(save-excursion
	    (provide-prefix-argument 2 (forward-character))
	    (set-mark)
	    (search-forward " ")
	    (backward-character)
	    (copy-region-to-buffer "Scratch Stuff")
	    (temp-use-buffer "Scratch Stuff")
	    (setq needs-checkpointing 0)
	    (beginning-of-file)
	    (set-mark)
	    (error-occured
		(replace-string "." "/"))
	    (end-of-line)
	    (setq rnews-file (region-to-string))
	    (pop-to-buffer "current-message")
	    (setq needs-checkpointing 0)
	    (setq this-from "")
	    (setq this-length "")
	    (setq this-id "")
	    (if (error-occured
		    (read-file (concat "/usr/spool/news/" rnews-file)))
		(progn
		    (erase-buffer)
		    (setq mode-line-format
			(concat "Expired article " rnews-file)))
		(progn
		    (setq case-fold-search 1)
		    (beginning-of-file)
		    (if (error-occured (search-forward "\n\n"))
			(message "Garbled header")
			(rnews-fix-header))
		    (set-rnews-mode-line-format))
	    )
	)
    )

    (rnews-fix-header
	(set-mark)
	(beginning-of-file)
	(copy-region-to-buffer "Full Header")
	(narrow-region)
	(convert-head "Posted:" "Date:")
	(convert-head "Title:" "Subject:")
	(convert-head "Article-I.D.:" "Message-ID:")
	(error-occured
	    (re-search-forward "^From:[ \t]")
	    (end-of-line)
	    (set-mark)
	    (re-search-reverse "[!:@]")
	    (re-search-reverse "[!: ]")
	    (forward-character)
	    (setq this-from (region-to-string))
	    (beginning-of-file)
	)
	(error-occured
	    (re-search-forward "^Lines:[ \t]*\\(.*\\)")
	    (region-around-match 1)
	    (setq this-length (region-to-string))
	    (beginning-of-file)
	    (re-replace-string "^Lines:.*\n" "")
	    (beginning-of-file)
	)
	(error-occured
	    (re-search-forward "^Message-ID:[ \t]*\\(.*\\)")
	    (region-around-match 1)
	    (setq this-id (region-to-string))
	    (beginning-of-file)
	    (re-replace-string "^Message-ID:.*\n" "")
	    (beginning-of-file)
	)
	(error-occured (re-replace-string "^.*-version:.*\n" ""))
	(error-occured (re-replace-string "^Path:.*\n" ""))
	(error-occured (re-replace-string "^Sender:.*\n" ""))
	(error-occured (re-replace-string "^.*received:.*\n" ""))
	(widen-region)
	(if (= (length this-length) 0)
	    			; The constant 38 comes from the average
				; line length of 356 articles in net.*
	    (setq this-length (concat "~" (/ (buffer-size) 38))))
    )

    (convert-head re1 re2
	(setq re1 (concat "^" (arg 1)))
	(setq re2 (concat "^" (arg 2)))
	(if (error-occured (re-search-forward re2))
	    (error-occured (re-replace-string re1 (arg 2)))
	    (beginning-of-file))
	(error-occured (re-replace-string (concat re1 ".*\n") ""))
    )

    (rnews-show-full-header
	(save-window-excursion
	    (temp-use-buffer "current-message")
	    (beginning-of-file)
	    (set-mark)
	    (error-occured
		(search-forward "\n\n")
		(erase-region)
		(yank-buffer "Full Header")
	    )
	)
    )

    (rnews-next-page
	(save-excursion
	    (pop-to-buffer "current-message")
	    (next-page)
	    (set-rnews-mode-line-format)
	)
    )
    
    (rnews-previous-page
	(save-excursion
	    (pop-to-buffer "current-message")
	    (previous-page)
	    (set-rnews-mode-line-format)
	)
    )

    (set-rnews-mode-line-format from
	(save-excursion 
	    (temp-use-buffer "Scratch Stuff")
	    (erase-buffer)
	    (insert-string this-from)
	    (set-mark)
	    (beginning-of-line)
	    (error-occured (replace-string "%" "%%"))
	    (setq from (region-to-string))
	    (use-old-buffer "current-message")
	    (end-of-file)
	    (setq mode-line-format
		(concat
		    "From: " from
		    (if (!= (length this-length) 0)
			(concat "  Lines: " this-length)
			"")
		    "  %[%p%]"
		    (if (dot-is-visible)
			""
			"  --More--")
		)))
    )
)

(defun
    (rnews-erase-messages
	(save-excursion
	    (pop-to-buffer "rnews-directory")
	    (beginning-of-file)
	    (error-occured
		(while 1
		    (re-search-forward "^.D")
		    (beginning-of-line)
		    (set-mark)
		    (end-of-line)
		    (forward-character)
		    (erase-region)
		)
	    )
	)
    )
)

(defun
    (rnews-com
	(argc)
	(rnews)
	(exit-emacs)
    )

    (rnews-next-message
	(rnews-position)
	(delete-next-character)
	(insert-character ' ')
	(beginning-of-line)
	(next-line)
	(if (eobp) (progn (previous-line)
			  (message "You're at the last message already")))
	(delete-next-character)
	(insert-character '>')
	(rnews-pickup)
    )

    (rnews-previous-message
	(rnews-position)
	(delete-next-character)
	(insert-character ' ')
	(previous-line)
	(beginning-of-line)
	(delete-next-character)
	(insert-character '>')
	(rnews-pickup)
    )

    (rnews-delete-message
	(rnews-position)
	(forward-character)
	(delete-next-character)
	(insert-character 'D')
	(beginning-of-line)
    )

    (rnews-undelete-message
	(rnews-position)
	(forward-character)
	(delete-next-character)
	(insert-character ' ')
	(beginning-of-line)
    )

    (rnews-delete-message-at-cursor
	(beginning-of-line)	
	(forward-character)
	(delete-next-character)
	(insert-character 'D')
	(beginning-of-line)
    )

    (rnews-undelete-message-at-cursor
	(beginning-of-line)
	(forward-character)
	(delete-next-character)
	(insert-character ' ')
	(beginning-of-line)
    )

    (rnews-goto-message n
	(setq n (get-tty-string "Goto message number: "))
	(rnews-unmark)
	(beginning-of-file)
	(provide-prefix-argument n (next-line))
	(rnews-mark)
    )
)

(defun
    (rnews-help
	(&info "emacs" "rnews")))

(defun
    (rnews-reply subject dest excess refs
	(setq subject "")
	(setq dest "")
	(setq excess "")
	(save-window-excursion
	    (pop-to-buffer "current-message")
	    (setq case-fold-search 1)
	    (beginning-of-file)
	    (search-forward "\n\n")
	    (set-mark)
	    (beginning-of-file)
	    (narrow-region)
	    (error-occured
		(re-search-forward "^Subject:[ \t]*\\(.*\\)")
		(region-around-match 1)
		(setq subject (region-to-string))
		(if (!= (substr subject 1 3) "Re:")
		    (setq subject (concat "Re: " subject))
		)
	    )
	    (save-excursion
		(temp-use-buffer "Full Header")
		(beginning-of-file)
		    (if internet-replies
			(if (error-occured (re-search-forward
					       "^Reply-To:[ \t]*\\(.*\\)"))
			    (setq dest this-from)
			    (progn
				(region-around-match 1)
				(setq dest (region-to-string)))
			)
			(progn
			    (if (error-occured (re-search-forward
				    "^Path:[ \t]*[^ \t!]*!\\(.*\\)"))
				(re-search-forward
				    "^From:[ \t]*[^ \t!]*!\\(.*\\)"))
			    (region-around-match 1)
			    (setq dest (region-to-string))))
	    )
	    (beginning-of-file)
	    (error-occured edest
		(save-excursion 
		    (temp-use-buffer "Scratch Stuff")
		    (setq needs-checkpointing 0)
		    (erase-buffer)
		    (insert-string dest)
		    (set-mark)
		    (beginning-of-file)
		    (if (! (error-occured
			(re-search-forward " (\\(.*\\))")))
			(progn
			    (region-around-match 1)
			    (setq dest (region-to-string))
			    (beginning-of-file)
			    (insert-string (concat dest "  <"))
			    (re-replace-string "  *(.*" ">")
			    (end-of-line)
			    (set-mark)
			    (beginning-of-line)
			    (setq dest (region-to-string))
			)
		    )
		    (error-occured 
			(re-replace-string
			    "  *at  *[^,\n]*\\| *@ *[^,\n]*\\| *([^)\n]*)\\| *<[^>\n]*>"
			    ""))
		    (error-occured
			(re-replace-string ".*!" ""))
		    (setq edest (region-to-string))
		)
		(error-occured
		    (re-search-forward "^Date:[ \t]*\\(.*\\)")
		    (region-around-match 1)
		)
		(setq excess (concat
				 "In-Reply-To: "
				 edest "'s message of "
				 (region-to-string)
				 "\n"))
		(beginning-of-file)
	        (if (error-occured
			(re-search-forward "^References:[ \t]*\\(.*\\)")
			(region-around-match 1)
			(setq refs (concat (region-to-string) " " this-id)))
		    (setq refs this-id))
	    )
	    (widen-region)
	    (pop-to-buffer "send-mail")
	    (setq needs-checkpointing 0)
	    (setq case-fold-search 1)
	    (erase-buffer)
	    (insert-string subject)
	    (newline)
	    (insert-string dest)
	    (newline)
	    (insert-string excess)
	    (insert-string (concat "References: " refs "\n"))
	    (do-mail-setup)
	)
	(rnews-position)
	(if (looking-at "^>")
	    (progn
		(forward-character)
		(delete-next-character)
		(insert-character 'A')
		(beginning-of-line)))
    )
)

(defun
    (rnews-followup newsgroups subject refs
	(setq newsgroups "")
	(setq subject "")
	(setq refs "")
	(save-window-excursion
	    (pop-to-buffer "current-message")
	    (beginning-of-file)
	    (search-forward "\n\n")
	    (set-mark)
	    (beginning-of-file)
	    (narrow-region)
	    (error-occured
		(re-search-forward "^Newsgroups:[ \t]*\\(.*\\)")
		(region-around-match 1)
		(setq newsgroups (region-to-string))
	    )
	    (save-excursion
		(temp-use-buffer "Scratch Stuff")
		(setq needs-checkpointing 0)
		(erase-buffer)
		(insert-string newsgroups)
		(beginning-of-file)
		(error-occured
		    (replace-string "general" "followup"))
		(set-mark)
		(end-of-file)
		(setq newsgroups (region-to-string))
	    )
	    (beginning-of-file)
	    (error-occured
		(re-search-forward "^Subject:[ \t]*\\(.*\\)")
		(region-around-match 1)
		(setq subject (region-to-string))
		(if (!= (substr subject 1 3) "Re:")
		    (setq subject (concat "Re: " subject))
		)
	    )
	    (beginning-of-file)
	    (if (error-occured
		    (re-search-forward "^References:[ \t]*\\(.*\\)")
		    (region-around-match 1)
		    (setq refs (concat (region-to-string) " " this-id)))
		(setq refs this-id))
	    (beginning-of-file)
	    (widen-region)
	    (pop-to-buffer "send-mail")
	    (setq needs-checkpointing 0)
	    (setq case-fold-search 1)
	    (erase-buffer)
	    (insert-string (concat "Newsgroups: " newsgroups))
	    (newline)
	    (insert-string (concat "Subject: " subject))
	    (newline)
	    (insert-string (concat "References: " refs))
	    (newline)
	    (newline)
	    (rnews-do-post)
	)
	(rnews-position)
	(if (looking-at "^>")
	    (progn
		(forward-character)
		(delete-next-character)
		(insert-character 'F')
		(beginning-of-line)))
    )

    (rnews-post
	(save-window-excursion
	    (pop-to-buffer "send-mail")
	    (setq needs-checkpointing 0)
	    (setq case-fold-search 1)
	    (erase-buffer)
	    (insert-string "Newsgroups: \nSubject: \n\n")
	    (beginning-of-file)
	    (end-of-line)
	    (rnews-do-post)
	)
    )

    (rnews-do-post rnews-do-send
	(setq rnews-do-send 1)
	(setq right-margin 72)
	(local-bind-to-key "exit-emacs" "\^X\^S")
	(local-bind-to-key "exit-emacs" "\^X\^F")
	(local-bind-to-key "rnews-abort-send" "\^X\^A")
	(local-bind-to-key "justify-paragraph" "\ej")
	(local-bind-to-key "rnews-post-goto-dist" "\^Xd")
	(while (= rnews-do-send 1)
	    (progn
		(recursive-edit)
		(if (= rnews-do-send 1)
		    (rnews-call-inews))))
    )

    (rnews-post-goto-dist	; Move to the "Distribution:" field
	(beginning-of-file)
	(if (error-occured (re-search-forward "^Distribution:.*"))
	    (progn (re-search-forward "\n\n\\|^Subject:.*\n.")
		(backward-character)
		(backward-character)
		(insert-string "\nDistribution: ")))
    )

    (rnews-call-inews inews-errors
	(save-excursion
	    (beginning-of-file)
	    (set-mark)
	    (end-of-file)
	    (copy-region-to-buffer "Delivery-errors")
	)
	(message "Sending...")
	(sit-for 0)
	(save-window-excursion
	    (temp-use-buffer "Delivery-errors")
	    (setq needs-checkpointing 0)
	    (beginning-of-file)
	    (set-mark)
	    (end-of-file)
	    (filter-region "inews -h")
	    (beginning-of-file)
	    (set-mark)
	    (error-occured (re-replace-string "\n\n* *" "; "))
	    (end-of-line)
	    (setq inews-errors (region-to-string))
	    (if (= (length inews-errors) 0)
		(progn
		    (setq rnews-do-send 0)
		    (message "Posted")
		)
		(message inews-errors)
	    )
	)
    )

    (rnews-abort-send
	(if (!= "y" (substr (get-tty-string
				"Do you really want to abort the message? ")
			    1 1))
	    (error-message "Turkey!"))
	(setq rnews-do-send 0)
	(exit-emacs)
    )
)

(defun
    (rnews-unmark
	(error-occured
	    (rnews-position)
	    (delete-next-character)
	    (insert-character ' ')
	    (beginning-of-line)))
    
    (rnews-mark
	(beginning-of-line)
	(if (error-occured
		(if (eobp)
		    (re-search-reverse "^.")
		    (progn
			(re-search-forward "^.")
			(beginning-of-line)))
	    )
	    (message "No messages")
	    (progn
		(delete-next-character)
		(insert-character '>')
		(rnews-pickup))
	)
    )

    (rnews-first-message
	(rnews-unmark)
	(beginning-of-file)
	(rnews-mark)
    )

    (rnews-last-message
	(rnews-unmark)
	(end-of-file)
	(rnews-mark)
    )

    (rnews-skip n
	(setq n (get-tty-string "Skip messages: "))
	(rnews-unmark)
	(provide-prefix-argument n (next-line))
	(rnews-mark)
    )

    (rnews-this-message save-dot
	(setq save-dot (dot))
	(rnews-unmark)
	(goto-character save-dot)
	(rnews-mark)
    )

    (rnews-decrypt-joke
	(save-window-excursion
	    (temp-use-buffer "current-message")
	    (beginning-of-file)
	    (search-forward "\n\n")
	    (set-mark)
	    (end-of-file)
	    (filter-region "tr a-zA-Z n-za-mN-ZA-M")
	)
    )

    (rnews-unsubscribe group newsrc
	(beginning-of-line)
	(save-excursion
	    (provide-prefix-argument 2 (forward-character))
	    (set-mark)
	    (search-forward " ")
	    (search-reverse "/")
	    (setq group (region-to-string))
	    (temp-use-buffer ".newsrc")
	    (if (error-occured (setq newsrc (getenv "NEWSRC")))
		(setq newsrc (concat (getenv "HOME") "/.newsrc")))
	    (read-file newsrc)
	    (if (error-occured (search-forward (concat group ":")))
		(message "Already unsubscribed")
		(progn
		    (delete-previous-character)
		    (insert-character '!')
		    (write-current-file)
		    (message (concat "Unsubscribed from " group))))
	)
    )
)

(save-excursion i
    (temp-use-buffer "rnews-directory")
    (setq i ' ')
    (while (< i 128)
	(local-bind-to-key "illegal-operation" i)
	(setq i (+ i 1)))
    (local-bind-to-key "rnews-next-page" 'f')
    (local-bind-to-key "rnews-previous-page" 'b')
    (local-bind-to-key "rnews-previous-page" '^H')
    (local-bind-to-key "rnews-next-message" 'n')
    (local-bind-to-key "rnews-previous-message" 'p')
    (local-bind-to-key "rnews-delete-message" 'd')
    (local-bind-to-key "rnews-undelete-message" 'u')
    (local-bind-to-key "rnews-delete-message-at-cursor" 'D')
    (local-bind-to-key "rnews-undelete-message-at-cursor" 'U')
    (local-bind-to-key "rnews-help" '?')
    (local-bind-to-key "exit-emacs" 'q')
    (local-bind-to-key "rnews-reply" 'r')
    (local-bind-to-key "rnews-followup" 'F')
    (local-bind-to-key "rnews-post" 'P')
    (local-bind-to-key "smail" 'm')
    (local-bind-to-key "rnews-goto-message" 'g')
    (local-bind-to-key "rnews-first-message" '<')
    (local-bind-to-key "rnews-last-message" '>')
    (local-bind-to-key "rnews-skip" 's')
    (local-bind-to-key "rmail-append" 'a')
    (local-bind-to-key "rmail-shell" '!')
    (local-bind-to-key "execute-extended-command" ':')
    (local-bind-to-key "rnews-this-message" ' ')
    (local-bind-to-key "rnews-this-message" '\r')
    (local-bind-to-key "rnews-decrypt-joke" '`')
    (local-bind-to-key "rnews-show-full-header" 'H')
    (local-bind-to-key "rnews-unsubscribe" "\^Xu")
)
