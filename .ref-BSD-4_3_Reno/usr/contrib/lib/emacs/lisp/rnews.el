;;; USENET news reader for gnu emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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

;; Created Sun Mar 10,1985 at 21:35:01 ads and sundar@hernes.ai.mit.edu
;; Should do the point pdl stuff sometime
;; finito except pdl.... Sat Mar 16,1985 at 06:43:44
;; lets keep the summary stuff out until we get it working ..
;;	sundar@hermes.ai.mit.edu Wed Apr 10,1985 at 16:32:06
;; hack slash maim. mly@prep.ai.mit.edu Thu 18 Apr, 1985 06:11:14
;; modified to correct reentrance bug, to not bother with groups that
;;   received no new traffic since last read completely, to find out
;;   what traffic a group has available much more quickly when
;;   possible, to do some completing reads for group names - should
;;   be much faster...
;;	KING@KESTREL.arpa, Thu Mar 13 09:03:28 1986
;; made news-{next,previous}-group skip groups with no new messages; and
;; added checking for unsubscribed groups to news-add-news-group
;;	tower@prep.ai.mit.edu Jul 18 1986
;; bound rmail-output to C-o; and changed header-field commands binding to
;; agree with the new C-c C-f usage in sendmail
;; 	tower@prep Sep  3 1986
;; added news-rotate-buffer-body
;;	tower@prep Oct 17 1986
;; made messages more user friendly, cleanuped news-inews
;; move posting and mail code to new file rnewpost.el
;;	tower@prep Oct 29 1986
;; added caesar-region, rename news-caesar-buffer-body, hacked accordingly
;;	tower@prep Nov 21 1986
;; added (provide 'rnews)	tower@prep 22 Apr 87
(provide 'rnews)
(require 'mail-utils)

(autoload 'rmail-output "rmailout"
  "Append this message to Unix mail file named FILE-NAME."
  t)

(autoload 'news-reply "rnewspost"
  "Compose and post a reply to the current article on USENET.
While composing the reply, use \\[mail-yank-original] to yank the original
message into it."
  t)

(autoload 'news-mail-other-window "rnewspost"
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  t)

(autoload 'news-post-news "rnewspost"
  "Begin editing a new USENET news article to be posted."
  t)

(autoload 'news-mail-reply "rnewspost"
  "Mail a reply to the author of the current article.
While composing the reply, use \\[mail-yank-original] to yank the original
message into it."
  t)

(defvar rmail-last-file (expand-file-name "~/mbox.news"))

;Now in paths.el.
;(defvar news-path "/usr/spool/news/"
;  "The root directory below which all news files are stored.")

(defvar news-startup-file "$HOME/.newsrc" "Contains ~/.newsrc")
(defvar news-certification-file "$HOME/.news-dates" "Contains ~/.news-dates")

;; random headers that we decide to ignore.
(defvar news-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:"
  "All random fields within the header of a message.")

(defvar news-mode-map nil)
(defvar news-read-first-time-p t)
;; Contains the (dotified) news groups of which you are a member. 
(defvar news-user-group-list nil)

(defvar news-current-news-group nil)
(defvar news-current-group-begin nil)
(defvar news-current-group-end  nil)
(defvar news-current-certifications nil
   	"An assoc list of a group name and the time at which it is
known that the group had no new traffic")
(defvar news-current-certifiable nil
	"The time when the directory we are now working on was written")

(defvar news-message-filter nil
  "User specifiable filter function that will be called during
formatting of the news file")

;(defvar news-mode-group-string "Starting-Up"
;  "Mode line group name info is held in this variable")
(defvar news-list-of-files nil
  "Global variable in which we store the list of files
associated with the current newsgroup")
(defvar news-list-of-files-possibly-bogus nil
  "variable indicating we only are guessing at which files are available.
Not currently used.")

;; association list in which we store lists of the form
;; (pointified-group-name (first last old-last))
(defvar news-group-article-assoc nil)
  
(defvar news-current-message-number 0 "Displayed Article Number")
(defvar news-total-current-group 0 "Total no of messages in group")

(defvar news-unsubscribe-groups ())
(defvar news-point-pdl () "List of visited news messages.")
(defvar news-no-jumps-p t)
(defvar news-buffer () "Buffer into which news files are read.")

(defmacro news-push (item ref)
  (list 'setq ref (list 'cons item ref)))

(defmacro news-cadr (x) (list 'car (list 'cdr x)))
(defmacro news-cdar (x) (list 'cdr (list 'car x)))
(defmacro news-caddr (x) (list 'car (list 'cdr (list 'cdr x))))
(defmacro news-cadar (x) (list 'car (list 'cdr (list 'car x))))
(defmacro news-caadr (x) (list 'car (list 'car (list 'cdr x))))
(defmacro news-cdadr (x) (list 'cdr (list 'car (list 'cdr x))))

(defmacro news-wins (pfx index)
  (` (file-exists-p (concat (, pfx) "/" (int-to-string (, index))))))

(defvar news-max-plausible-gap 2
	"* In an rnews directory, the maximum possible gap size.
A gap is a sequence of missing messages between two messages that exist.
An empty file does not contribute to a gap -- it ends one.")

(defun news-find-first-and-last (prefix base)
  (and (news-wins prefix base)
       (cons (news-find-first-or-last prefix base -1)
	     (news-find-first-or-last prefix base 1))))

(defmacro news-/ (a1 a2)
;; a form of / that guarantees that (/ -1 2) = 0
  (if (zerop (/ -1 2))
      (` (/ (, a1) (, a2)))
    (` (if (< (, a1) 0)
	   (- (/ (- (, a1)) (, a2)))
	 (/ (, a1) (, a2))))))

(defun news-find-first-or-last (pfx base dirn)
  ;; first use powers of two to find a plausible ceiling
  (let ((original-dir dirn))
    (while (news-wins pfx (+ base dirn))
      (setq dirn (* dirn 2)))
    (setq dirn (news-/ dirn 2))
    ;; Then use a binary search to find the high water mark
    (let ((offset (news-/ dirn 2)))
      (while (/= offset 0)
	(if (news-wins pfx (+ base dirn offset))
	    (setq dirn (+ dirn offset)))
	(setq offset (news-/ offset 2))))
    ;; If this high-water mark is bogus, recurse.
    (let ((offset (* news-max-plausible-gap original-dir)))
      (while (and (/= offset 0) (not (news-wins pfx (+ base dirn offset))))
	(setq offset (- offset original-dir)))
      (if (= offset 0)
	  (+ base dirn)
	(news-find-first-or-last pfx (+ base dirn offset) original-dir)))))

(defun rnews ()
"Read USENET news for groups for which you are a member and add or
delete groups.
You can reply to articles posted and send articles to any group.

Type \\[describe-mode] once reading news to get a list of rnews commands."
  (interactive)
  (let ((last-buffer (buffer-name)))
    (make-local-variable 'rmail-last-file)
    (switch-to-buffer (setq news-buffer (get-buffer-create "*news*")))
    (news-mode)
    (setq news-buffer-save last-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (set-buffer-modified-p t)
    (sit-for 0)
    (message "Getting new USENET news...")
    (news-set-mode-line)
    (news-get-certifications)
    (news-get-new-news)))

(defun news-group-certification (group)
  (cdr-safe (assoc group news-current-certifications)))


(defun news-set-current-certifiable ()
  ;; Record the date that corresponds to the directory you are about to check
  (let ((file (concat news-path
		      (string-subst-char ?/ ?. news-current-news-group))))
    (setq news-current-certifiable
	  (nth 5 (file-attributes
		  (or (file-symlink-p file) file))))))

(defun news-get-certifications ()
  ;; Read the certified-read file from last session
  (save-excursion
    (save-window-excursion
      (setq news-current-certifications
	    (car-safe
	     (condition-case var
		 (let*
		     ((file (substitute-in-file-name news-certification-file))
		      (buf (find-file-noselect file)))
		   (and (file-exists-p file)
			(progn
			  (switch-to-buffer buf 'norecord)
			  (unwind-protect
			      (read-from-string (buffer-string))
			    (kill-buffer buf)))))
	       (error nil)))))))

(defun news-write-certifications ()
  ;; Write a certification file.
  ;; This is an assoc list of group names with doubletons that represent
  ;; mod times of the directory when group is read completely.
  (save-excursion
    (save-window-excursion
      (with-output-to-temp-buffer
	  "*CeRtIfIcAtIoNs*"
	  (print news-current-certifications))
      (let ((buf (get-buffer "*CeRtIfIcAtIoNs*")))
	(switch-to-buffer buf)
	(write-file (substitute-in-file-name news-certification-file))
	(kill-buffer buf)))))

(defun news-set-current-group-certification ()
  (let ((cgc (assoc news-current-news-group news-current-certifications)))
    (if cgc (setcdr cgc news-current-certifiable)
      (news-push (cons news-current-news-group news-current-certifiable)
		 news-current-certifications))))

(defun news-set-minor-modes ()
  "Creates a minor mode list that has group name, total articles,
and attribute for current article."
  (setq news-minor-modes (list (cons 'foo
				     (concat news-current-message-number
					     "/"
					     news-total-current-group
					     (news-get-attribute-string)))))
  ;; Detect Emacs versions 18.16 and up, which display
  ;; directly from news-minor-modes by using a list for mode-name.
  (or (boundp 'minor-mode-alist)
      (setq minor-modes news-minor-modes)))

(defun news-set-message-counters ()
  "Scan through current news-groups filelist to figure out how many messages
are there. Set counters for use with minor mode display."
    (if (null news-list-of-files)
	(setq news-current-message-number 0)))

(if news-mode-map
    nil
  (setq news-mode-map (make-keymap))
  (suppress-keymap news-mode-map)
  (define-key news-mode-map "." 'beginning-of-buffer)
  (define-key news-mode-map " " 'scroll-up)
  (define-key news-mode-map "\177" 'scroll-down)
  (define-key news-mode-map "n" 'news-next-message)
  (define-key news-mode-map "c" 'news-make-link-to-message)
  (define-key news-mode-map "p" 'news-previous-message)
  (define-key news-mode-map "j" 'news-goto-message)
  (define-key news-mode-map "q" 'news-exit)
  (define-key news-mode-map "e" 'news-exit)
  (define-key news-mode-map "\ej" 'news-goto-news-group)
  (define-key news-mode-map "\en" 'news-next-group)
  (define-key news-mode-map "\ep" 'news-previous-group)
  (define-key news-mode-map "l" 'news-list-news-groups)
  (define-key news-mode-map "?" 'describe-mode)
  (define-key news-mode-map "g" 'news-get-new-news)
  (define-key news-mode-map "f" 'news-reply)
  (define-key news-mode-map "m" 'news-mail-other-window)
  (define-key news-mode-map "a" 'news-post-news)
  (define-key news-mode-map "r" 'news-mail-reply)
  (define-key news-mode-map "o" 'news-save-item-in-file)
  (define-key news-mode-map "\C-o" 'rmail-output)
  (define-key news-mode-map "t" 'news-show-all-headers)
  (define-key news-mode-map "x" 'news-force-update)
  (define-key news-mode-map "A" 'news-add-news-group)
  (define-key news-mode-map "u" 'news-unsubscribe-current-group)
  (define-key news-mode-map "U" 'news-unsubscribe-group)
  (define-key news-mode-map "\C-c\C-r" 'news-caesar-buffer-body))

(defun news-mode ()
  "News Mode is used by M-x rnews for reading USENET Newsgroups articles.
New readers can find additional help in newsgroup: news.announce.newusers .
All normal editing commands are turned off.
Instead, these commands are available:

.	move point to front of this news article (same as Meta-<).
Space	scroll to next screen of this news article.
Delete  scroll down previous page of this news article.
n	move to next news article, possibly next group.
p	move to previous news article, possibly previous group.
j	jump to news article specified by numeric position.
M-j     jump to news group.
M-n     goto next news group.
M-p     goto previous news group.
l       list all the news groups with current status.
?       print this help message.
C-c C-r caesar rotate all letters by 13 places in the article's body (rot13).
g       get new USENET news.
f       post a reply article to USENET.
a       post an original news article.
A       add a newsgroup. 
o	save the current article in the named file (append if file exists).
C-o	output this message to a Unix-format mail file (append it).
c       \"copy\" (actually link) current or prefix-arg msg to file.
	warning: target directory and message file must be on same device
		(UNIX magic)
t       show all the headers this news article originally had.
q	quit reading news after updating .newsrc file.
e	exit updating .newsrc file.
m	mail a news article.  Same as C-x 4 m.
x       update last message seen to be the current message.
r	mail a reply to this news article.  Like m but initializes some fields.
u       unsubscribe from current newsgroup.
U       unsubscribe from specified newsgroup."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'news-read-first-time-p)
  (setq news-read-first-time-p t)
  (make-local-variable 'news-current-news-group)
;  (setq news-current-news-group "??")
  (make-local-variable 'news-current-group-begin)
  (setq news-current-group-begin 0)
  (make-local-variable 'news-current-message-number)
  (setq news-current-message-number 0)
  (make-local-variable 'news-total-current-group)
  (make-local-variable 'news-buffer-save)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'news-point-pdl)
;  This breaks it.  I don't have time to figure out why. -- RMS
;  (make-local-variable 'news-group-article-assoc)
  (setq major-mode 'news-mode)
  (if (boundp 'minor-mode-alist)
      ;; Emacs versions 18.16 and up.
      (setq mode-name '("NEWS" news-minor-modes))
    ;; Earlier versions display minor-modes via a special mechanism.
    (setq mode-name "NEWS"))
  (news-set-mode-line)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map news-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (run-hooks 'news-mode-hook))

(defun string-subst-char (new old string)
  (let (index)
    (setq old (regexp-quote (char-to-string old))
	  string (substring string 0))
    (while (setq index (string-match old string))
      (aset string index new)))
  string)

;; update read message number
(defmacro news-update-message-read (ngroup nno)
  (list 'setcar
	(list 'news-cdadr
	      (list 'assoc ngroup 'news-group-article-assoc))
	nno))

(defun news-parse-range (number-string)
  "Parse string representing range of numbers of he form <a>-<b>
to a list (a . b)"
  (let ((n (string-match "-" number-string)))
    (if n
	(cons (string-to-int (substring number-string 0 n))
	      (string-to-int (substring number-string (1+ n))))
      (setq n (string-to-int number-string))
      (cons n n))))

;(defun is-in (elt lis)
;  (catch 'foo
;    (while lis
;      (if (equal (car lis) elt)
;	  (throw 'foo t)
;	(setq lis (cdr lis))))))

(defun news-get-new-news ()
  "Get new USENET news, if there is any for the current user."
  (interactive)
  (if (not (null news-user-group-list))
      (news-update-newsrc-file))
  (setq news-group-article-assoc ())
  (setq news-user-group-list ())
  (message "Looking up %s file..." news-startup-file)
  (let ((file (substitute-in-file-name news-startup-file))
	(temp-user-groups ()))
    (save-excursion
      (let ((newsrcbuf (find-file-noselect file))
	    start end endofline tem)
	(set-buffer newsrcbuf)
	(goto-char 0)
	(while (search-forward ": " nil t)
	  (setq end (point))
	  (beginning-of-line)
	  (setq start (point))
	  (end-of-line)
	  (setq endofline (point))
	  (setq tem (buffer-substring start (- end 2)))
	  (let ((range (news-parse-range
			(buffer-substring end endofline))))
	    (if (assoc tem news-group-article-assoc)
		(message "You are subscribed twice to %s; I ignore second"
			 tem)	      
	      (setq temp-user-groups (cons tem temp-user-groups)
		    news-group-article-assoc
		    (cons (list tem (list (car range)
					  (cdr range)
					  (cdr range)))
			  news-group-article-assoc)))))
	(kill-buffer newsrcbuf)))      
    (setq temp-user-groups (nreverse temp-user-groups))
    (message "Prefrobnicating...")
    (switch-to-buffer news-buffer)
    (setq news-user-group-list temp-user-groups)
    (while (and temp-user-groups
		(not (news-read-files-into-buffer
		      (car temp-user-groups) nil)))
      (setq temp-user-groups (cdr temp-user-groups)))
    (if (null temp-user-groups)
	(message "No news is good news.")
      (message ""))))

(defun news-list-news-groups ()
  "Display all the news groups to which you belong."
  (interactive)
  (with-output-to-temp-buffer "*Newsgroups*"
    (save-excursion
      (set-buffer standard-output)
      (insert
	"News Group        Msg No.       News Group        Msg No.\n")
      (insert
	"-------------------------       -------------------------\n")
      (let ((temp news-user-group-list)
	    (flag nil))
	(while temp
	  (let ((item (assoc (car temp) news-group-article-assoc)))
	    (insert (car item))
	    (indent-to (if flag 52 20))
	    (insert (int-to-string (news-cadr (news-cadr item))))
	    (if flag
		(insert "\n")
	      (indent-to 33))
	    (setq temp (cdr temp) flag (not flag))))))))

;; Mode line hack
(defun news-set-mode-line ()
  "Set mode line string to something useful."
  (setq mode-line-process
	(concat " "
		(if (integerp news-current-message-number)
		    (int-to-string news-current-message-number)
		 "??")
		"/"
		(if (integerp news-current-group-end)
		    (int-to-string news-current-group-end)
		  news-current-group-end)))
  (setq mode-line-buffer-identification
	(concat "NEWS: "
		news-current-news-group
		;; Enough spaces to pad group name to 17 positions.
		(substring "                 "
			   0 (max 0 (- 17 (length news-current-news-group))))))
  (set-buffer-modified-p t)
  (sit-for 0))

(defun news-goto-news-group (gp)
  "Takes a string and goes to that news group."
  (interactive (list (completing-read "NewsGroup: "
				      news-group-article-assoc)))
  (message "Jumping to news group %s..." gp)
  (news-select-news-group gp)
  (message "Jumping to news group %s... done." gp))

(defun news-select-news-group (gp)
  (let ((grp (assoc gp news-group-article-assoc)))
    (if (null grp)
 	(error "Group not subscribed to in file %s." news-startup-file)
      (progn
	(news-update-message-read news-current-news-group
				  (news-cdar news-point-pdl))
	(news-read-files-into-buffer  (car grp) nil)
	(news-set-mode-line)))))

(defun news-goto-message (arg)
  "Goes to the article ARG in current newsgroup."
  (interactive "p")
  (if (null current-prefix-arg)
      (setq arg (read-no-blanks-input "Go to article: " "")))
  (news-select-message arg))

(defun news-select-message (arg)
  (if (stringp arg) (setq arg (string-to-int arg)))
  (let ((file (concat news-path
		      (string-subst-char ?/ ?. news-current-news-group)
		      "/" arg)))
    (if (file-exists-p file)
	(let ((buffer-read-only ()))
	  (if (= arg 
		 (or (news-cadr (memq (news-cdar news-point-pdl) news-list-of-files))
		     0))
	      (setcdr (car news-point-pdl) arg))
	  (setq news-current-message-number arg)
	  (news-read-in-file file)
	  (news-set-mode-line))
      (error "Article %d nonexistent" arg))))

(defun news-force-update ()
  "updates the position of last article read in the current news group"
  (interactive)
  (setcdr (car news-point-pdl) news-current-message-number)
  (message "Updated to %d" news-current-message-number))

(defun news-next-message (arg)
  "Move ARG messages forward within one newsgroup.
Negative ARG moves backward.
If ARG is 1 or -1, moves to next or previous newsgroup if at end."
  (interactive "p")
  (let ((no (+ arg news-current-message-number)))
    (if (or (< no news-current-group-begin) 
	    (> no news-current-group-end))
	(cond ((= arg 1)
	       (news-set-current-group-certification)
	       (news-next-group))
	      ((= arg -1)
	       (news-previous-group))
	      (t (error "Article out of range")))
      (let ((plist (news-get-motion-lists
		     news-current-message-number
		     news-list-of-files)))
	(if (< arg 0)
	    (news-select-message (nth (1- (- arg)) (car (cdr plist))))
	  (news-select-message (nth (1- arg) (car plist))))))))

(defun news-previous-message (arg)
  "Move ARG messages backward in current newsgroup.
With no arg or arg of 1, move one message
and move to previous newsgroup if at beginning.
A negative ARG means move forward."
  (interactive "p")
  (news-next-message (- arg)))

(defun news-move-to-group (arg)
  "Given arg move forward or backward to a new newsgroup."
  (let ((cg news-current-news-group))
    (let ((plist (news-get-motion-lists cg news-user-group-list))
	  ngrp)
      (if (< arg 0)
	  (or (setq ngrp (nth (1- (- arg)) (news-cadr plist)))
	      (error "No previous news groups"))
	(or (setq ngrp (nth arg (car plist)))
	    (error "No more news groups")))
      (news-select-news-group ngrp))))

(defun news-next-group ()
  "Moves to the next user group."
  (interactive)
;  (message "Moving to next group...")
  (news-move-to-group 0)
  (while (null news-list-of-files)
    (news-move-to-group 0)))
;  (message "Moving to next group... done.")

(defun news-previous-group ()
  "Moves to the previous user group."
  (interactive)
;  (message "Moving to previous group...")
  (news-move-to-group -1)
  (while (null news-list-of-files)
    (news-move-to-group -1)))
;  (message "Moving to previous group... done.")

(defun news-get-motion-lists (arg listy)
  "Given a msgnumber/group this will return a list of two lists;
one for moving forward and one for moving backward."
  (let ((temp listy)
	(result ()))
    (catch 'out
      (while temp
	(if (equal (car temp) arg)
	    (throw 'out (cons (cdr temp) (list result)))
	  (setq result (nconc (list (car temp)) result))
	  (setq temp (cdr temp)))))))

;; miscellaneous io routines
(defun news-read-in-file (filename)
  (erase-buffer)
  (let ((start (point)))
  (insert-file-contents filename)
  (news-convert-format)
  (goto-char start)
  (forward-line 1)
  (if (eobp)
      (message "(Empty file?)")
    (goto-char start))))

(defun news-convert-format ()
  (save-excursion
    (save-restriction
      (let* ((start (point))
	     (end (condition-case ()
		      (progn (search-forward "\n\n") (point))
		    (error nil)))
	     has-from has-date)
       (cond (end
	      (narrow-to-region start end)
	      (goto-char start)
	      (setq has-from (search-forward "\nFrom:" nil t))
	      (cond ((and (not has-from) has-date)
		     (goto-char start)
		     (search-forward "\nDate:")
		     (beginning-of-line)
		     (kill-line) (kill-line)))
	      (news-delete-headers start)
	      (goto-char start)))))))

(defun news-show-all-headers ()
  "Redisplay current news item with all original headers"
  (interactive)
  (let (news-ignored-headers
	(buffer-read-only ()))
    (erase-buffer)
    (news-set-mode-line)
    (news-read-in-file
     (concat news-path
	     (string-subst-char ?/ ?. news-current-news-group)
	     "/" (int-to-string news-current-message-number)))))

(defun news-delete-headers (pos)
  (goto-char pos)
  (and (stringp news-ignored-headers)
       (while (re-search-forward news-ignored-headers nil t)
	 (beginning-of-line)
	 (delete-region (point)
			(progn (re-search-forward "\n[^ \t]")
			       (forward-char -1)
			       (point))))))

(defun news-exit ()
  "Quit news reading session and update the .newsrc file."
  (interactive)
  (if (y-or-n-p "Do you really wanna quit reading news ? ")
      (progn (message "Updating %s..." news-startup-file)
	     (news-update-newsrc-file)
	     (news-write-certifications)
	     (message "Updating %s... done" news-startup-file)
	     (message "Now do some real work")
	     (and (fboundp 'bury-buffer) (bury-buffer (current-buffer)))
	     (switch-to-buffer news-buffer-save)
	     (setq news-user-group-list ()))
    (message "")))

(defun news-update-newsrc-file ()
  "Updates the .newsrc file in the users home dir."
  (let ((newsrcbuf (find-file-noselect
		     (substitute-in-file-name news-startup-file)))
	(tem news-user-group-list)
	group)
    (save-excursion
      (if (not (null news-current-news-group))
	  (news-update-message-read news-current-news-group
				(news-cdar news-point-pdl)))
      (switch-to-buffer newsrcbuf)
      (while tem
	(setq group (assoc (car tem)
			   news-group-article-assoc))
	(if (= (news-cadr (news-cadr group)) (news-caddr (news-cadr group)))
	    nil
	  (goto-char 0)
	  (if (search-forward (concat (car group) ": ") nil t)
	      (kill-line nil)
	    (insert (car group) ": \n") (backward-char 1))
	  (insert (int-to-string (car (news-cadr group))) "-"
		  (int-to-string (news-cadr (news-cadr group)))))
	(setq tem (cdr tem)))
     (while news-unsubscribe-groups
       (setq group (assoc (car news-unsubscribe-groups)
			  news-group-article-assoc))
       (goto-char 0)
       (if (search-forward (concat (car group) ": ") nil t)
	   (progn
	      (backward-char 2)
	      (kill-line nil)
	      (insert "! " (int-to-string (car (news-cadr group)))
		      "-" (int-to-string (news-cadr (news-cadr group))))))
       (setq news-unsubscribe-groups (cdr news-unsubscribe-groups)))
     (save-buffer)
     (kill-buffer (current-buffer)))))


(defun news-unsubscribe-group (group)
  "Removes you from newgroup GROUP."
  (interactive (list (completing-read  "Unsubscribe from group: "
				      news-group-article-assoc)))
  (news-unsubscribe-internal group))

(defun news-unsubscribe-current-group ()
  "Removes you from the newsgroup you are now reading."
  (interactive)
  (if (y-or-n-p "Do you really want to unsubscribe from this group ? ")
      (news-unsubscribe-internal news-current-news-group)))

(defun news-unsubscribe-internal (group)
  (let ((tem (assoc group news-group-article-assoc)))
    (if tem
	(progn
	  (setq news-unsubscribe-groups (cons group news-unsubscribe-groups))
	  (news-update-message-read group (news-cdar news-point-pdl))
	  (if (equal group news-current-news-group)
	      (news-next-group))
	  (message ""))
      (error "Not subscribed to group: %s" group))))

(defun news-save-item-in-file (file)
  "Save the current article that is being read by appending to a file."
  (interactive "FSave item in file: ")
  (append-to-file (point-min) (point-max) file))

(defun news-get-pruned-list-of-files (gp-list end-file-no)
  "Given a news group it finds all files in the news group.
The arg must be in slashified format.
Using ls was found to be too slow in a previous version."
  (let
      ((answer
	(and
	 (not (and end-file-no
		   (equal (news-set-current-certifiable)
		     (news-group-certification gp-list))
		   (setq news-list-of-files nil
			 news-list-of-files-possibly-bogus t)))
	 (let* ((file-directory (concat news-path
					(string-subst-char ?/ ?. gp-list)))
		tem
		(last-winner
		 (and end-file-no
		      (news-wins file-directory end-file-no)
		      (news-find-first-or-last file-directory end-file-no 1))))
	   (setq news-list-of-files-possibly-bogus t news-list-of-files nil)
	   (if last-winner
	       (progn
		 (setq news-list-of-files-possibly-bogus t
		       news-current-group-end last-winner)
		 (while (> last-winner end-file-no)
		   (news-push last-winner news-list-of-files)
		   (setq last-winner (1- last-winner)))
		 news-list-of-files)
	     (if (or (not (file-directory-p file-directory))
		     (not (file-readable-p file-directory)))
		 nil
	       (setq news-list-of-files
		     (condition-case error
			 (directory-files file-directory)
		       (file-error
			(if (string= (nth 2 error) "permission denied")
			    (message "Newsgroup %s is read-protected"
				     gp-list)
			  (signal 'file-error (cdr error)))
			nil)))
	       (setq tem news-list-of-files)
	       (while tem
		 (if (or (not (string-match "^[0-9]*$" (car tem)))
			 ;; dont get confused by directories that look like numbers
			 (file-directory-p
			  (concat file-directory "/" (car tem)))
			 (<= (string-to-int (car tem)) end-file-no))
		     (setq news-list-of-files
			   (delq (car tem) news-list-of-files)))
		 (setq tem (cdr tem)))
	       (if (null news-list-of-files)
		   (progn (setq news-current-group-end 0)
			  nil)
		 (setq news-list-of-files
		       (mapcar 'string-to-int news-list-of-files))
		 (setq news-list-of-files (sort news-list-of-files '<))
		 (setq news-current-group-end
		       (elt news-list-of-files
			    (1- (length news-list-of-files))))
		 news-list-of-files)))))))
    (or answer (progn (news-set-current-group-certification) nil))))

(defun news-read-files-into-buffer (group reversep)
  (let* ((files-start-end (news-cadr (assoc group news-group-article-assoc)))
	 (start-file-no (car files-start-end))
	 (end-file-no (news-cadr files-start-end))
	 (buffer-read-only nil))
    (setq news-current-news-group group)
    (setq news-current-message-number nil)
    (setq news-current-group-end nil)
    (news-set-mode-line)
    (news-get-pruned-list-of-files group end-file-no)
    (news-set-mode-line)
    ;; @@ should be a lot smarter than this if we have to move
    ;; @@ around correctly.
    (setq news-point-pdl (list (cons (car files-start-end)
				     (news-cadr files-start-end))))
    (if (null news-list-of-files)
	(progn (erase-buffer)
	       (setq news-current-group-end end-file-no)
	       (setq news-current-group-begin end-file-no)
	       (setq news-current-message-number end-file-no)
	       (news-set-mode-line)
;	       (message "No new articles in " group " group.")
	       nil)
      (setq news-current-group-begin (car news-list-of-files))
      (if reversep
	  (setq news-current-message-number news-current-group-end)
	(if (> (car news-list-of-files) end-file-no)
	    (setcdr (car news-point-pdl) (car news-list-of-files)))
	(setq news-current-message-number news-current-group-begin))
      (news-set-message-counters)
      (news-set-mode-line)
      (news-read-in-file (concat news-path
				 (string-subst-char ?/ ?. group)
				 "/"
				 (int-to-string
				   news-current-message-number)))
      (news-set-message-counters)
      (news-set-mode-line)
      t)))

(defun news-add-news-group (gp)
  "Resubscribe to or add a USENET news group named GROUP (a string)."
; @@ (completing-read ...)
; @@ could be based on news library file ../active (slightly facist)
; @@ or (expensive to compute) all directories under the news spool directory
  (interactive "sAdd news group: ")
  (let ((file-dir (concat news-path (string-subst-char ?/ ?. gp))))
    (save-excursion
      (if (null (assoc gp news-group-article-assoc))
	  (let ((newsrcbuf (find-file-noselect
			    (substitute-in-file-name news-startup-file))))
	    (if (file-directory-p file-dir)
		(progn
		  (switch-to-buffer newsrcbuf)
		  (goto-char 0)
		  (if (search-forward (concat gp "! ") nil t)
		      (progn
			(message "Re-subscribing to group %s." gp)
			;;@@ news-unsubscribe-groups isn't being used
			;;(setq news-unsubscribe-groups
			;;    (delq gp news-unsubscribe-groups))
			(backward-char 2)
			(delete-char 1)
			(insert ":"))
		    (progn
		      (message
		       "Added %s to your list of newsgroups." gp)
		      (end-of-buffer)
		      (insert gp ": 1-1\n")))
		  (search-backward gp nil t)
		  (let (start end endofline tem)
		    (search-forward ": " nil t)
		    (setq end (point))
		    (beginning-of-line)
		    (setq start (point))
		    (end-of-line)
		    (setq endofline (point))
		    (setq tem (buffer-substring start (- end 2)))
		    (let ((range (news-parse-range
				  (buffer-substring end endofline))))
		      (setq news-group-article-assoc
			    (cons (list tem (list (car range)
						  (cdr range)
						  (cdr range)))
				  news-group-article-assoc))))
		  (save-buffer)
		  (kill-buffer (current-buffer)))
	      (message "Newsgroup %s doesn't exist." gp)))
	(message "Already subscribed to group %s." gp)))))

(defun news-make-link-to-message (number newname)
	"Forges a link to an rnews message numbered number (current if no arg)
Good for hanging on to a message that might or might not be
automatically deleted."
  (interactive "P
FName to link to message: ")
  (add-name-to-file
   (concat news-path
	   (string-subst-char ?/ ?. news-current-news-group)
	   "/" (if number
		   (prefix-numeric-value number)
		 news-current-message-number))
   newname))

;;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;;; modified by tower@prep Nov 86
(defun caesar-region (&optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (cond ((not (numberp n)) (setq n 13))
	((< n 0) (setq n (- 26 (% (- n) 26))))
	(t (setq n (% n 26))))		;canonicalize N
  (if (not (zerop n))		; no action needed for a rot of 0
      (progn
	(if (or (not (boundp 'caesar-translate-table))
		(/= (aref caesar-translate-table ?a) (+ ?a n)))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table...")
	      (setq caesar-translate-table (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      (message "Building caesar-translate-table... done")))
	(let ((from (region-beginning))
	      (to (region-end))
	      (i 0) str len)
	  (setq str (buffer-substring from to))
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  (goto-char from)
	  (kill-region from to)
	  (insert str)))))

;;; news-caesar-buffer-body written by paul@media-lab.mit.edu  Wed Oct 1, 1986
;;; hacked further by tower@prep.ai.mit.edu
(defun news-caesar-buffer-body (&optional rotnum)
  "Caesar rotates all letters in the current buffer by 13 places.
Used to encode/decode possibly offensive messages (commonly in net.jokes).
With prefix arg, specifies the number of places to rotate each letter forward.
Mail and USENET news headers are not rotated."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (save-excursion
    (let ((buffer-status buffer-read-only))
      (setq buffer-read-only nil)
      ;; setup the region
      (set-mark (if (progn (goto-char (point-min))
			    (search-forward
			     (concat "\n"
				     (if (equal major-mode 'news-mode)
					 ""
				       mail-header-separator)
				     "\n") nil t))
		     (point)
		   (point-min)))
      (goto-char (point-max))
      (caesar-region rotnum)
      (setq buffer-read-only buffer-status))))
