;;; Netnews reader for gnu emacs
;; Copyright (C) 1985 Free Software Foundation

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


;;; Created Sun Mar 10,1985 at 21:35:01 ads and sundar
;;; Should do the point pdl stuff sometime
;;; finito except pdl.... Sat Mar 16,1985 at 06:43:44
;;; lets keep the summary stuff out until we get it working ..
;;; sundar              Wed Apr 10,1985 at 16:32:06
;;; hack slash maim. mly Thu 18 Apr, 1985 06:11:14
;;; news-add-news-group / 'stead of . bug tower Mon Mar  3 15:39:44 EST 1986
;;; news-mail-reply from anywhere in buffer tower Wed Mar 12 11:15:03 EST 1986
;;; modified to correct reentrance bug, to not bother with groups that
;;;   received no new traffic since last read completely, to find out
;;;   what traffic a group has available much more quickly when
;;;   possible, to do some completing reads for group names - should
;;;   be much faster...
;;;      KING@KESTREL, Thu Mar 13 09:03:28 1986
;;; fixed doc error   tower Sun Mar 16 14:25:43 EST 1986
(require 'mail-utils)

;Now in paths.el.
;(defvar news-path "/usr/spool/news/"
;  "The root directory below which all news files are stored.")
;(defvar news-inews-program "inews"
;  "Function to post news.")

(defvar news-startup-file "$HOME/.newsrc" "Contains ~/.newsrc")
(defvar news-certification-file "$HOME/.news-dates" "Contains ~/.news-dates")

;;; random headers that we decide to ignore.
(defvar news-ignored-headers
  "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Followup-To:\\|^Expires:\\|^Date-Received:\\|^Organization:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:"
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
known that the grop had no new traffic")
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

(defmacro caar (x) (list 'car (list 'car x)))
(defmacro cadr (x) (list 'car (list 'cdr x)))
(defmacro cdar (x) (list 'cdr (list 'car x)))
(defmacro caddr (x) (list 'car (list 'cdr (list 'cdr x))))
(defmacro cadar (x) (list 'car (list 'cdr (list 'car x))))
(defmacro caadr (x) (list 'car (list 'car (list 'cdr x))))
(defmacro cdadr (x) (list 'cdr (list 'car (list 'cdr x))))

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

(defmacro // (a1 a2)
;;; a form of / that guarantees that (/ -1 2) = 0
  (if (zerop (/ -1 2))
      (` (/ (, a1) (, a2)))
    (` (if (< (, a1) 0)
	   (- (/ (- (, a1)) (, a2)))
	 (/ (, a1) (, a2))))))

(defun news-find-first-or-last (pfx base dirn)
  ;; first use powers of two to find a plausible cieling
  (let ((original-dir dirn))
    (while (news-wins pfx (+ base dirn))
      (setq dirn (* dirn 2)))
    (setq dirn (// dirn 2))
    ;;; Then use a binary search to find the high water mark
    (let ((offset (// dirn 2)))
      (while (/= offset 0)
	(if (news-wins pfx (+ base dirn offset))
	    (setq dirn (+ dirn offset)))
	(setq offset (// offset 2))))
    ;;; If this high-water mark is bogus, recurse.
    (let ((offset (* news-max-plausible-gap original-dir)))
      (while (and (/= offset 0) (not (news-wins pfx (+ base dirn offset))))
	(setq offset (- offset original-dir)))
      (if (= offset 0)
	  (+ base dirn)
	(news-find-first-or-last pfx (+ base dirn offset) original-dir)))))

(defun rnews ()
  "Read netnews for groups for which you are a member and add or delete groups.
You can reply to articles posted and send articles to any group.
  Type Help m once reading news to get a list of rnews commands."
  (interactive)
  (let ((last-buffer (buffer-name)))
    (switch-to-buffer (setq news-buffer (get-buffer-create "*news*")))
    (news-mode)
    (setq news-buffer-save last-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (set-buffer-modified-p t)
    (sit-for 0)
    (message "Getting new net news...")
    (news-set-mode-line)
    (news-get-certifications)
    (news-get-new-news)))

(defun news-group-certification (group)
  (cdr-safe (assoc group news-current-certifications)))


(defun news-set-current-certifiable ()
  ;;; Record the date that corresponds to the directory you are about to check
  (let ((file (concat news-path
		      (string-subst-char ?/ ?. news-current-news-group))))
    (setq news-current-certifiable
	  (nth 5 (file-attributes
		  (or (file-symlink-p file) file))))))

(defun news-get-certifications ()
  ;;; Read the certified-read file from last session
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
  ;;; Write a certification file.  This is an assoc list of group names with
  ;;;doubletons that represent mod times of the directory when group is read
  ;;;completely.
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
      (push (cons news-current-news-group news-current-certifiable)
	    news-current-certifications))))

(defun news-set-minor-modes ()
  "Creates a minor mode list that has group name, total articles,
and attribute for current article."
  (setq minor-modes (list (cons 'foo
				(concat news-current-message-number
					"/"
					news-total-current-group
					(news-get-attribute-string))))))

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
  (define-key news-mode-map "t" 'news-show-all-headers)
  (define-key news-mode-map "x" 'news-force-update)
  (define-key news-mode-map "A" 'news-add-news-group)
  (define-key news-mode-map "u" 'news-unsubscribe-current-group)
  (define-key news-mode-map "U" 'news-unsubscribe-group))

(defun news-mode ()
  "News Mode is used by M-x rnews for editing News files.
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
g       get new net news.
f       post follow-up article to the net.
a       post a news article.
A       add a newsgroup. 
o	save the current article in the named file (append if file exists).
c       \"copy\" (actually link) current or prefix-arg msg to file.
	warning: target directory and message file must be on same device
		(UNIX magic)
t       show all the headers this news article originally had.
q	quit reading news after updating .newsrc file.
e	exit updating .newsrc file.
m	mail a news article.  Same as C-x 4 m.
x       update last message seen to be the current message.
r	reply to this news article.  Like m but initializes some fields.
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
  (setq mode-name "NEWS")
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

;;; update read message number
(defmacro news-update-message-read (ngroup nno)
  (list 'setcar
	(list 'cdadr
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
  "Get new netnews if there is any for the current user."
  (interactive)
  (if (not (null news-user-group-list))
	(news-update-newsrc-file))
  (setq news-group-article-assoc ())
  (setq news-user-group-list ())
  (message "Looking up .newsrc file...")
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

;	    (if (is-in tem temp-user-groups)
;		(progn
;		  (setq temp-user-groups (delq tem temp-user-groups))
;		  (setq news-group-article-assoc 
;			(delq (assoc tem news-group-article-assoc)
;			news-group-article-assoc))
;		  (message "Subscribed to the same group twice?")))

	    (setq temp-user-groups (cons tem temp-user-groups)
		  news-group-article-assoc
		    (cons (list tem (list (car range)
					  (cdr range)
					  (cdr range)))
			  news-group-article-assoc))))
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
  (if (null news-user-group-list)
      (message "No user groups read yet!")
    (let ((buffer-read-only ()))
      (setq mode-line-format "--%%--[q: to goback, space: scroll-forward, delete:scroll-backward] %M --%--")
      (local-set-key " " 'scroll-up)
      (local-set-key "\177" 'scroll-down)
      (local-set-key "q" 'news-get-back)
      (goto-char 0)
      (save-excursion
        (erase-buffer)
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
	      (insert (int-to-string (cadr (cadr item))))
	      (if flag
		  (insert "\n")
		  (indent-to 33))
	      (setq temp (cdr temp) flag (not flag)))))))))

(defun news-get-back ()
  "Called when you quit from seeing the news groups list."
  (interactive)
  (let ((buffer-read-only ()))
    (erase-buffer)
    (local-set-key "q" 'news-exit)
    (news-set-mode-line)
    (news-read-in-file
      (concat news-path
	      (string-subst-char ?/ ?. news-current-news-group)
	      "/" (int-to-string news-current-message-number)))))

(defun strcpyn (str1 str2 len)
  (if (string= str2 "")
      str1
    (while (>= len 0)
      (aset str1 len (aref str2 len))
      (setq len (1- len)))
    str1))

;; Mode line hack
(defun news-set-mode-line ()
  "Set mode line string to something useful."
  (let ((tem (1- (length news-current-news-group)))
	(idx 0)
	(buffer-modified-p ()))
    (setq mode-line-format 
	  (concat "--%1*%1*-NEWS: "
		  (if (> tem 15)
		      news-current-news-group
		    (let ((string (make-string 16 ? )))
		      (setq idx 0)
		      (while (<= idx tem)
			(aset string idx (aref news-current-news-group idx))
			(setq idx (1+ idx)))
		      string))
		  " ["
		  (if (integerp news-current-message-number)
		      (int-to-string news-current-message-number)
		   "??")
		 "/"
		 (if (integerp news-current-group-end)
		     (int-to-string news-current-group-end)
		   news-current-group-end)
		 "] %M ----%3p-%-"))
    (set-buffer-modified-p t)
    (sit-for 0)))

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
	(error "No more news groups")
      (progn
	(news-update-message-read news-current-news-group
				  (cdar news-point-pdl))
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
		 (or (cadr (memq (cdar news-point-pdl) news-list-of-files))
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
	       (news-next-group)
	       (while (null news-list-of-files)
		 (news-next-group)))
	      ((= arg -1)
	       (news-previous-group)
	       (while (null news-list-of-files)
		 (news-previous-group)))
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
	  (or (setq ngrp (nth (1- (- arg)) (cadr plist)))
	      (error "No more news groups"))
	(or (setq ngrp (nth arg (car plist)))
	    (error "No previous news groups")))
      (news-select-news-group ngrp))))

(defun news-next-group ()
  "Moves to the next user group."
  (interactive)
;  (message "Moving to next group...")
  (news-move-to-group 0))
;  (message "Moving to next group... done.")

(defun news-previous-group ()
  "Moves to the previous user group."
  (interactive)
;  (message "Moving to previous group...")
  (news-move-to-group -1))
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
  (let (news-ignored-headers)
    (news-get-back)))

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
  "Quit news reading session and update the newsrc file."
  (interactive)
  (if (y-or-n-p "Do you really wanna quit reading news ? ")
      (progn (message "Updating .newsrc...")
	     (news-update-newsrc-file)
	     (news-write-certifications)
	     (message "Updating .newsrc... done")
	     (message "Now do some real work")
	     (and (fboundp 'bury-buffer) (bury-buffer (current-buffer)))
	     (switch-to-buffer news-buffer-save)
	     (setq news-user-group-list ()))
    (message "")))

(defun news-update-newsrc-file ()
  "Updates the newsrc file in the users home dir."
  (let ((newsrcbuf (find-file-noselect
		     (substitute-in-file-name news-startup-file)))
	(tem news-user-group-list)
	group)
    (save-excursion
      (if (not (null news-current-news-group))
	  (news-update-message-read news-current-news-group
				(cdar news-point-pdl)))
      (switch-to-buffer newsrcbuf)
      (while tem
	(setq group (assoc (car tem)
			   news-group-article-assoc))
	(if (= (cadr (cadr group)) (caddr (cadr group)))
	    nil
	  (goto-char 0)
	  (if (search-forward (concat (car group) ": ") nil t)
	      (kill-line nil)
	    (insert (car group) ": \n") (backward-char 1))
	  (insert (int-to-string (car (cadr group))) "-"
		  (int-to-string (cadr (cadr group)))))
	(setq tem (cdr tem)))
     (while news-unsubscribe-groups
       (setq group (assoc (car news-unsubscribe-groups)
			  news-group-article-assoc))
       (goto-char 0)
       (if (search-forward (concat (car group) ": ") nil t)
	   (progn
	      (backward-char 2)
	      (kill-line nil)
	      (insert "! " (int-to-string (car (cadr group)))
		      "-" (int-to-string (cadr (cadr group))))))
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
	  (news-update-message-read group (cdar news-point-pdl))
	  (if (equal group news-current-news-group)
	      (news-next-group))
	  (message "Member-p of %s ==> nil" group))
      (error "No such group: %s" group))))

(defun news-save-item-in-file (file)
  "Save the current article that is being read by appending to a file."
  (interactive "FSave item in file: ")
  (append-to-file (point-min) (point-max) file))

(defun news-get-pruned-list-of-files (gp-list end-file-no)
  "Given a news group it does an ls to give all files in the news group.
The arg must be in slashified format."
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
		   (push last-winner news-list-of-files)
		   (setq last-winner (1- last-winner)))
		 news-list-of-files)
	     (if (not (file-directory-p file-directory))
		 nil
	       (setq news-list-of-files
		     (setq tem (directory-files file-directory)))
	       (while tem
		 (if (or (not (string-match "^[0-9]*$" (car tem)))
					; dont get confused by directories that look like numbers
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
  (let* ((files-start-end (cadr (assoc group news-group-article-assoc)))
	 (start-file-no (car files-start-end))
	 (end-file-no (cadr files-start-end))
	 (buffer-read-only nil))

    (setq news-current-news-group group)
    (setq news-current-message-number nil)
    (setq news-current-group-end nil)
    (news-set-mode-line)
    (news-get-pruned-list-of-files group end-file-no)
    (news-set-mode-line)
    ;; should be a lot smarter than this if we have to move
    ;; around correctly.
    (setq news-point-pdl (list (cons (car files-start-end)
				     (cadr files-start-end))))
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


;;; Replying and posting news items are done by these functions.
;;; imported from rmail and modified to work with rnews ...
;;; Mon Mar 25,1985 at 03:07:04 ads@mit-hermes.
;;; this is done so that rnews can operate independently from rmail.el and sendmail and
;;; dosen't have to autoload these functions.

;;;>> Nuked by Mly to autoload those functions again, as the duplication of
;;;>>  code was making maintenance too difficult.

(defvar news-reply-mode-map () "Mode map used by news-reply.")

(or news-reply-mode-map
    (progn (setq news-reply-mode-map (make-keymap))
	   (define-key news-reply-mode-map "\C-c?" 'describe-mode)
	   (define-key news-reply-mode-map "\C-ct" 'mail-to)
	   (define-key news-reply-mode-map "\C-cb" 'mail-bcc)
	   (define-key news-reply-mode-map "\C-cc" 'mail-cc)
	   (define-key news-reply-mode-map "\C-cs" 'mail-subject)
	   (define-key news-reply-mode-map "\C-cy" 'mail-yank-original)
	   (define-key news-reply-mode-map "\C-c\C-c" 'news-inews)
	   (define-key news-reply-mode-map "\C-c\C-s" 'news-inews)))

(defun news-reply-mode ()
  "Major mode for editing news to be posted on netnews.
Like Text Mode but with these additional commands:
\\{news-reply-mode-map}"
  (interactive)
  ;; require...
  (or (fboundp 'mail-setup) (load "sendmail"))
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map news-reply-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'news-reply-mode)
  (setq mode-name "News")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^" mail-header-separator "$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^" mail-header-separator "$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'news-reply-mode-hook))

(defun news-setup (to subject in-reply-to newsgroups replybuffer)
  (setq mail-reply-buffer replybuffer)
  (let ((mail-setup-hook nil))
    (if (null to)
	;; this hack is needed so that inews wont be confused by 
	;; the fcc: and bcc: fields
	(let ((mail-self-blind nil)
	      (mail-archive-file-name nil))
	  (mail-setup to subject in-reply-to nil replybuffer)
	  (beginning-of-line)
	  (kill-line 1)
	  (goto-char (point-max)))
      (mail-setup to subject in-reply-to nil replybuffer))
    (goto-char (point-max))
    (if (let ((case-fold-search t))
	  (re-search-backward "^Subject:" (point-min) t))
	(progn (beginning-of-line)
	       (insert "Newsgroups: " (or newsgroups "") "\n")
	       (if (not newsgroups)
		   (backward-char 1)
		 (goto-char (point-max)))))
    (run-hooks 'news-setup-hook)))
   
(defun news-inews ()
  "Send a news message using inews."
  (interactive)
  (let* (newsgroups subject
	 (case-fold-search nil))
    (save-restriction
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (narrow-to-region (point-min) (point))
      (setq newsgroups (mail-fetch-field "newsgroups")
	    subject (mail-fetch-field "subject")))
    (widen)
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (message "Posting to the net...")
    (call-process-region (point) (point-max) 
			 news-inews-program nil 0 nil
			 "-t" subject
			 "-n" newsgroups)
    (message "Posting to the net... done")
    (set-buffer-modified-p nil)
    (delete-windows-on (current-buffer))
    (and (fboundp 'bury-buffer) (bury-buffer (current-buffer)))))
		       
(defun news-mail-reply ()
  "Mail a reply to the author of the current article.
While composing the reply, use \\[mail-yank-original] to yank the original message into it."
  (interactive)
  (let (from cc subject date to reply-to
	     (buffer (current-buffer)))
    (save-restriction
      (narrow-to-region (point-min) (progn (goto-line (point-min))
					   (search-forward "\n\n")
					   (- (point) 2)))
      (setq from (mail-fetch-field "from")
	    subject (mail-fetch-field "subject")
	    reply-to (mail-fetch-field "reply-to")
	    date (mail-fetch-field "date"))
      (setq to from)
      (pop-to-buffer "*mail*")
      (mail nil
	    (if reply-to reply-to to)
	    subject
	    (let ((stop-pos (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
	      (concat (if stop-pos (substring from 0 stop-pos) from)
		      "'s message of "
		      date))
	    nil
	   buffer))))
  
(defun news-reply ()
  "Compose and send a reply to the current article to the net.
While composing the reply, use \\[mail-yank-original] to yank the original message into it."
  (interactive)
  (if (y-or-n-p "Are you sure you want to reply to the net? ")
      (let (from cc subject date to newsgroups
		 (buffer (current-buffer)))
	(save-restriction
	  (narrow-to-region (point-min) (progn (search-forward "\n\n")
					       (- (point) 2)))
	  (setq from (mail-fetch-field "from")
		subject (mail-fetch-field "subject")
		date (mail-fetch-field "date")
		newsgroups (mail-fetch-field "newsgroups"))
	  (pop-to-buffer "*post-news*")
	  (news-reply-mode)
	  (erase-buffer)
	  (news-setup
	   nil
	   subject
	   (let ((stop-pos (string-match "  *at \\|  *@ \\| *(\\| *<" from)))
	     (concat (if stop-pos (substring from 0 stop-pos) from)
		     "'s message of "
		     date))
	   newsgroups
	   buffer)))
    (message "")))

(defun news-post-news ()
  "Begin editing a news article to be posted."
  (interactive)
  (pop-to-buffer "*post-news*")
  (news-reply-mode)
  (erase-buffer)
  (news-setup () () () () ()))
 
(defun news-add-news-group (gp)
  "Add you to news group named GROUP (a string)."
; (completing-read ...)
  (interactive "sAdd news group: ")
  (let ((file-dir (concat news-path (string-subst-char ?/ ?. gp))))
    (save-excursion
     (if (null (assoc gp news-group-article-assoc))
	 (let ((newsrcbuf (find-file-noselect
			   (substitute-in-file-name news-startup-file))))
	   (if (file-directory-p file-dir)
	       (progn
		 (switch-to-buffer newsrcbuf)
		 (end-of-buffer)
		 (insert (string-subst-char ?. ?\ gp) ": 1-1\n")
		 (save-buffer)
		 (kill-buffer (current-buffer))
		 (message "Added %s to your current list of newsgroups." gp))
	    (message "Newsgroup %s doesn't exist." gp)))
       (message "Already subscribed to group %s." gp)))))

(defun news-mail-other-window ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (mail-other-window nil nil nil nil nil (current-buffer)))

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


