;; Info package for Emacs  -- could use a "create node" feature.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

(provide 'info)

(defvar Info-history nil
  "List of info nodes user has visited.
Each element of list is a list (FILENAME NODENAME BUFFERPOS).")

(defvar Info-enable-edit nil
  "Non-nil means the \\[Info-edit] command in Info can edit the current node.")

(defvar Info-enable-active-nodes t
  "Non-nil allows Info to execute Lisp code associated with nodes.
The Lisp code is executed when the node is selected.")

(defvar Info-directory nil
  "Default directory for Info documentation files.")

(defvar Info-current-file nil
  "Info file that Info is now looking at, or nil.")

(defvar Info-current-subfile nil
  "Info subfile that is actually in the *info* buffer now,
or nil if current info file is not split into subfiles.")

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker (make-marker)
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defun info ()
  "Enter Info, the documentation browser."
  (interactive)
  (if (get-buffer "*info*")
      (switch-to-buffer "*info*")
    (Info-directory)))

;; Go to an info node specified as separate filename and nodename.
;; no-going-back is non-nil if recovering from an error in this function;
;; it says do not attempt further (recursive) error recovery.
(defun Info-find-node (filename nodename &optional no-going-back)
  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (if filename
      (let (temp)
	(setq filename (substitute-in-file-name filename))
	(setq temp (expand-file-name filename
				     ;; Use Info's default dir
				     ;; unless the filename starts with `./'.
				     (if (not (string-match "^\\./" filename))
					 Info-directory)))
	(if (file-exists-p temp)
	    (setq filename temp)
	  (setq temp (expand-file-name (downcase filename) Info-directory))
	  (if (file-exists-p temp)
	      (setq filename temp)
	    (error "Info file %s does not exist"
		   (expand-file-name filename Info-directory))))))
  ;; Record the node we are leaving.
  (if (and Info-current-file (not no-going-back))
      (setq Info-history
	    (cons (list Info-current-file Info-current-node (point))
		  Info-history)))
  ;; Go into info buffer.
  (switch-to-buffer "*info*")
  (or (eq major-mode 'Info-mode)
      (Info-mode))
  (widen)
  (setq Info-current-node nil)
  (unwind-protect
      (progn
	;; Switch files if necessary
	(or (null filename)
	    (equal Info-current-file filename)
	    (let ((buffer-read-only nil))
	      (setq Info-current-file nil
		    Info-current-subfile nil)
	      (erase-buffer)
	      (insert-file-contents filename t)
	      (set-buffer-modified-p nil)
	      (setq default-directory (file-name-directory filename))
	      ;; See whether file has a tag table.  Record the location if yes.
	      (set-marker Info-tag-table-marker nil)
	      (goto-char (point-max))
	      (forward-line -8)
	      (or (equal nodename "*")
		  (not (search-forward "\^_\nEnd tag table\n" nil t))
		  (let (pos)
		    ;; We have a tag table.  Find its beginning.
		    ;; Is this an indirect file?
		    (search-backward "\nTag table:\n")
		    (setq pos (point))
		    (if (save-excursion
			  (forward-line 2)
			  (looking-at "(Indirect)\n"))
			;; It is indirect.  Copy it to another buffer
			;; and record that the tag table is in that buffer.
			(save-excursion
			  (let ((buf (current-buffer)))
			    (set-buffer (get-buffer-create " *info tag table*"))
			    (erase-buffer)
			    (insert-buffer-substring buf)
			    (set-marker Info-tag-table-marker
					(match-end 0))))
		     (set-marker Info-tag-table-marker pos))))
	      (setq Info-current-file
		    (file-name-sans-versions buffer-file-name))))
	(if (equal nodename "*")
	    (progn (setq Info-current-node nodename)
		   (Info-set-mode-line))
	  ;; Search file for a suitable node.
	  ;; First get advice from tag table if file has one.
	  ;; Also, if this is an indirect info file,
	  ;; read the proper subfile into this buffer.
	  (let ((guesspos (point-min)))
	    (if (marker-position Info-tag-table-marker)
		(save-excursion
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (goto-char Info-tag-table-marker)
		  (if (search-forward (concat "Node: " nodename "\177") nil t)
		      (progn
			(setq guesspos (read (current-buffer)))
			;; If this is an indirect file,
			;; determine which file really holds this node
			;; and read it in.
			(if (not (eq (current-buffer) (get-buffer "*info*")))
			    (setq guesspos
				  (Info-read-subfile guesspos))))
		    (error "No such node: \"%s\"" nodename))))
	    (goto-char (max (point-min) (- guesspos 1000))))
	  ;; Now search from our advised position (or from beg of buffer)
	  ;; to find the actual node.
	  (let ((regexp (concat "Node: *" (regexp-quote nodename) " *[,\t\n]")))
	    (catch 'foo
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward regexp beg t)
		      (throw 'foo t))))
	      (error "No such node: %s" nodename)))
	  (Info-select-node)))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back
	(let ((hist (car Info-history)))
	  (setq Info-history (cdr Info-history))
	  (Info-find-node (nth 0 hist) (nth 1 hist) t)
	  (goto-char (nth 2 hist)))))
  (goto-char (point-min)))

(defun Info-read-subfile (nodepos)
  (set-buffer (marker-buffer Info-tag-table-marker))
  (goto-char (point-min))
  (search-forward "\n\^_")
  (let (lastfilepos
	lastfilename)
    (forward-line 2)
    (catch 'foo
      (while (not (looking-at "\^_"))
	(let ((beg (point))
	      thisfilepos thisfilename)
	  (search-forward ": ")
	  (setq thisfilename  (buffer-substring beg (- (point) 2)))
	  (setq thisfilepos (read (current-buffer)))
	  (if (> thisfilepos nodepos)
	      (throw 'foo t))
	  (setq lastfilename thisfilename)
	  (setq lastfilepos thisfilepos))))
    (set-buffer (get-buffer "*info*"))
    (or (equal Info-current-subfile lastfilename)
	(let ((buffer-read-only nil))
	  (setq buffer-file-name nil)
	  (widen)
	  (erase-buffer)
	  (insert-file-contents lastfilename)
	  (set-buffer-modified-p nil)
	  (setq Info-current-subfile lastfilename)))
    (goto-char (point-min))
    (search-forward "\n\^_")
    (+ (- nodepos lastfilepos) (point))))

;; Select the info node that point is in.
(defun Info-select-node ()
  (save-excursion
   ;; Find beginning of node.
   (search-backward "\n\^_")
   (forward-line 2)
   ;; Get nodename spelled as it is in the node.
   (re-search-forward "Node:[ \t]*")
   (setq Info-current-node
	 (buffer-substring (point)
			   (progn
			    (skip-chars-forward "^,\t\n")
			    (point))))
   (Info-set-mode-line)
   ;; Find the end of it, and narrow.
   (beginning-of-line)
   (let (active-expression)
     (narrow-to-region (point)
		       (if (re-search-forward "\n[\^_\f]" nil t)
			   (prog1
			    (1- (point))
			    (if (looking-at "[\n\^_\f]*execute: ")
				(progn
				  (goto-char (match-end 0))
				  (setq active-expression
					(read (current-buffer))))))
			 (point-max)))
     (if Info-enable-active-nodes (eval active-expression)))))

(defun Info-set-mode-line ()
  (setq mode-line-buffer-identification
	(concat
	 "Info:  ("
	 (if Info-current-file
	     (file-name-nondirectory Info-current-file)
	   "")
	 ")"
	 (or Info-current-node ""))))

;; Go to an info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

(defun Info-goto-node (nodename)
  "Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME."
  (interactive "sGoto node: ")
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (if (= (match-beginning 1) (match-end 1))
		       ""
		     (substring nodename (match-beginning 2) (match-end 2)))
	  nodename (substring nodename (match-beginning 3) (match-end 3)))
    (let ((trim (string-match "\\s *\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s *\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename))))

(defvar Info-last-search nil
  "Default regexp for Info S command to search for.")

(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in."
  (interactive "sSearch (regexp): ")
  (if (equal regexp "")
      (setq regexp Info-last-search)
    (setq Info-last-search regexp))
  (let ((found ()) current
	(onode Info-current-node)
	(ofile Info-current-file)
	(opoint (point))
	(osubfile Info-current-subfile))
    (save-excursion
      (save-restriction
	(widen)
	(if (null Info-current-subfile)
	    (progn (re-search-forward regexp) (setq found (point)))
	  (condition-case err
	      (progn (re-search-forward regexp) (setq found (point)))
	    (search-failed nil)))))
    (if (not found) ;can only happen in subfile case -- else would have erred
	(unwind-protect
	    (let ((list ()))
	      (set-buffer (marker-buffer Info-tag-table-marker))
	      (goto-char (point-min))
	      (search-forward "\n\^_\nIndirect:")
	      (save-restriction
		(narrow-to-region (point)
				  (progn (search-forward "\n\^_")
					 (1- (point))))
		(goto-char (point-min))
		(search-forward (concat "\n" osubfile ": "))
		(beginning-of-line)
		(while (not (eobp))
		  (re-search-forward "\\(^.*\\): [0-9]+$")
		  (goto-char (+ (match-end 1) 2))
		  (setq list (cons (cons (read (current-buffer))
					 (buffer-substring (match-beginning 1)
							   (match-end 1)))
				   list))
		  (goto-char (1+ (match-end 0))))
		(setq list (nreverse list)
		      current (car (car list))
		      list (cdr list)))
	      (while list
		(message "Searching subfile %s..." (cdr (car list)))
		(Info-read-subfile (car (car list)))
		(setq list (cdr list))
		(goto-char (point-min))
		(if (re-search-forward regexp nil t)
		    (setq found (point) list ())))
	      (if found
		  (message "")
		(signal 'search-failed (list regexp))))
	  (if (not found)
	      (progn (Info-read-subfile opoint)
		     (goto-char opoint)
		     (Info-select-node)))))
    (widen)
    (goto-char found)
    (Info-select-node)
    (or (and (equal onode Info-current-node)
	     (equal ofile Info-current-file))
	(setq Info-history (cons (list ofile onode opoint)
				 Info-history)))))

(defun Info-extract-pointer (name &optional errorname)
  (save-excursion
   (goto-char (point-min))
   (forward-line 1)
   (if (re-search-backward (concat name ":") nil t)
       nil
     (error (concat "Node has no " (capitalize (or errorname name)))))
   (goto-char (match-end 0))
   (Info-following-node-name)))

(defun Info-following-node-name (&optional allowedchars)
  (skip-chars-forward " \t")
  (buffer-substring
   (point)
   (progn
     (while (looking-at (concat "[" (or allowedchars "^,\t\n") "]"))
       (skip-chars-forward (concat (or allowedchars "^,\t\n") "("))
       (if (looking-at "(")
	   (skip-chars-forward "^)")))
     (skip-chars-backward " ")
     (point))))

(defun Info-next ()
  "Go to the next node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "next")))

(defun Info-prev ()
  "Go to the previous node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous")))

(defun Info-up ()
  "Go to the superior node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "up")))

(defun Info-last ()
  "Go back to the last node visited."
  (interactive)
  (or Info-history
      (error "This is the first Info node you looked at"))
  (let (filename nodename opoint)
    (setq filename (car (car Info-history)))
    (setq nodename (car (cdr (car Info-history))))
    (setq opoint (car (cdr (cdr (car Info-history)))))
    (setq Info-history (cdr Info-history))
    (Info-find-node filename nodename)
    (setq Info-history (cdr Info-history))
    (goto-char opoint)))

(defun Info-directory ()
  "Go to the Info directory node."
  (interactive)
  (Info-find-node "dir" "top"))

(defun Info-follow-reference (footnotename)
  "Follow cross reference named NAME to the node it refers to.
NAME may be an abbreviation of the reference name."
  (interactive
   (let ((completion-ignore-case t)
	 completions str i)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "\\*note[ \n\t]*\\([^:]*\\):" nil t)
	 (setq str (buffer-substring
		    (match-beginning 1)
		    (1- (point))))
	 (setq i 0)
	 (while (setq i (string-match "[ \n\t]+" str i))
	   (setq str (concat (substring str 0 i) " "
			     (substring str (match-end 0))))
	   (setq i (1+ i)))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     (if completions
	 (list (completing-read "Follow reference named: " completions nil t))
       (error "No cross-references in this node"))))
  (let (target beg i (str (concat "\\*note " footnotename)))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "[ \t\n]+" (substring str (1+ i))))
      (setq i (+ i 6)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward str nil t)
	  (error "No cross-reference named %s" footnotename))
      (goto-char (+ (match-beginning 0) 5))
      (setq target (Info-extract-menu-node-name "Bad format cross reference")))
    (while (setq i (string-match "[ \t\n]+" target i))
      (setq target (concat (substring target 0 i) " "
			   (substring target (match-end 0))))
      (setq i (+ i 1)))
    (Info-goto-node target)))

(defun Info-extract-menu-node-name (&optional errmessage)
  (skip-chars-forward " \t\n")
  (let ((beg (point))
	str i)
    (skip-chars-forward "^:")
    (forward-char 1)
    (setq str
	  (if (looking-at ":")
	      (buffer-substring beg (1- (point)))
	    (Info-following-node-name "^.,\t\n")))
    (while (setq i (string-match "\n" str i))
      (aset str i ?\ ))
    str))

(defun Info-menu-item-sequence (list)
  (while list
    (Info-menu-item (car list))
    (setq list (cdr list))))

(defun Info-menu (menu-item)
  "Go to node for menu item named (or abbreviated) NAME."
  (interactive
   (let ((completions '())
	 ;; If point is within a menu item, use that item as the default
	 (default nil)
	 (p (point))
	 (last nil))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (while (re-search-forward
		"\n\\* \\([^:\t\n]*\\):" nil t)
	 (if (and (null default)
		  (prog1 (if last (< last p) nil)
		    (setq last (match-beginning 0)))
		  (<= p last))
	     (setq default (car (car completions))))
	 (setq completions (cons (cons (buffer-substring
					 (match-beginning 1)
					 (match-end 1))
				       (match-beginning 1))
				 completions)))
       (if (and (null default) last
		(< last p)
		(<= p (progn (end-of-line) (point))))
	   (setq default (car (car completions)))))
     (let ((item nil))
       (while (null item)
	 (setq item (let ((completion-ignore-case t))
		      (completing-read (if default
					   (format "Menu item (default %s): "
						   default)
					   "Menu item: ")
				       completions nil t)))
	 ;; we rely on the bug (which RMS won't change for his own reasons)
	 ;; that ;; completing-read accepts an input of "" even when the
	 ;; require-match argument is true and "" is not a valid possibility
	 (if (string= item "")
	     (if default
		 (setq item default)
	         ;; ask again
	         (setq item nil))))
       (list item))))
  (Info-goto-node (Info-extract-menu-item menu-item)))
  
(defun Info-extract-menu-item (menu-item)
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "\n* menu:" nil t)
	(error "No menu in this node"))
    (or (search-forward (concat "\n* " menu-item) nil t)
	(error "No such item in menu"))
    (beginning-of-line)
    (forward-char 2)
    (Info-extract-menu-node-name)))

(defun Info-extract-menu-counting (count)
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "\n* menu:" nil t)
	(error "No menu in this node"))
    (or (search-forward "\n* " nil t count)
	(error "Too few items in menu"))
    (Info-extract-menu-node-name)))

(defun Info-first-menu-item ()
  "Go to the node of the first menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting 1)))

(defun Info-second-menu-item ()
  "Go to the node of the second menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting 2)))

(defun Info-third-menu-item ()
  "Go to the node of the third menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting 3)))

(defun Info-fourth-menu-item ()
  "Go to the node of the fourth menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting 4)))

(defun Info-fifth-menu-item ()
  "Go to the node of the fifth menu item."
  (interactive)
  (Info-goto-node (Info-extract-menu-counting 5)))

(defun Info-exit ()
  "Exit Info by selecting some other buffer."
  (interactive)
  (switch-to-buffer (prog1 (other-buffer (current-buffer))
			   (bury-buffer (current-buffer)))))

(defun Info-undefined ()
  "Make command be undefined in Info."
  (interactive)
  (ding))

(defun Info-help ()
  "Enter the Info tutorial."
  (interactive)
  (Info-find-node "info"
		  (if (< (window-height) 23)
		      "Help-Small-Screen"
		    "Help")))

(defun Info-summary ()
  "Display a brief summary of all Info commands."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (erase-buffer)
    (insert (documentation 'Info-mode))
    (goto-char (point-min))
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to Info"))
		    (if (/= ?\  (setq ch (read-char)))
			(progn (setq unread-command-char ch) nil)
		      flag))
	(scroll-up)))))

(defvar Info-mode-map nil
  "Keymap containing Info commands.")
(if Info-mode-map
    nil
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (define-key Info-mode-map "." 'beginning-of-buffer)
  (define-key Info-mode-map " " 'scroll-up)
  (define-key Info-mode-map "1" 'Info-first-menu-item)
  (define-key Info-mode-map "2" 'Info-second-menu-item)
  (define-key Info-mode-map "3" 'Info-third-menu-item)
  (define-key Info-mode-map "4" 'Info-fourth-menu-item)
  (define-key Info-mode-map "5" 'Info-fifth-menu-item)
  (define-key Info-mode-map "6" 'undefined)
  (define-key Info-mode-map "7" 'undefined)
  (define-key Info-mode-map "8" 'undefined)
  (define-key Info-mode-map "9" 'undefined)
  (define-key Info-mode-map "0" 'undefined)
  (define-key Info-mode-map "?" 'Info-summary)
  (define-key Info-mode-map "b" 'beginning-of-buffer)
  (define-key Info-mode-map "d" 'Info-directory)
  (define-key Info-mode-map "e" 'Info-edit)
  (define-key Info-mode-map "f" 'Info-follow-reference)
  (define-key Info-mode-map "g" 'Info-goto-node)
  (define-key Info-mode-map "h" 'Info-help)
  (define-key Info-mode-map "l" 'Info-last)
  (define-key Info-mode-map "m" 'Info-menu)
  (define-key Info-mode-map "n" 'Info-next)
  (define-key Info-mode-map "p" 'Info-prev)
  (define-key Info-mode-map "q" 'Info-exit)
  (define-key Info-mode-map "s" 'Info-search)
  (define-key Info-mode-map "u" 'Info-up)
  (define-key Info-mode-map "\177" 'scroll-down))

(defun Info-mode ()
  "Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes
which discuss related topics.  Info has commands to follow
the references and show you other nodes.

h	Invoke the Info tutorial.

Selecting other nodes:
n	Move to the \"next\" node of this node.
p	Move to the \"previous\" node of this node.
u	Move \"up\" from this node.
m	Pick menu item specified by name (or abbreviation).
	Picking a menu item causes another node to be selected.
f	Follow a cross reference.  Reads name of reference.
l	Move to the last node you were at.

Moving within a node:
Space	scroll forward a page.     DEL  scroll backward.
b	Go to beginning of node.

Advanced commands:
q	Quit Info: reselect previously selected buffer.
e	Edit contents of selected node.
1	Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
g	Move to node specified by name.
	You may include a filename as well, as (FILENAME)NODENAME.
s	Search through this Info file for specified regexp,
	and select the node in which the next occurrence is found."
  (kill-all-local-variables)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (use-local-map Info-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (make-local-variable 'Info-history)
  (Info-set-mode-line))

(defvar Info-edit-map nil
  "Local keymap used within `e' command of Info.")
(if Info-edit-map
    nil
  (setq Info-edit-map (copy-keymap text-mode-map))
  (define-key Info-edit-map "\C-c\C-c" 'Info-cease-edit))

(defun Info-edit-mode ()
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of Info-cease-edit
which returns to Info mode for browsing.
\\{Info-edit-map}"
  )

(defun Info-edit ()
  "Edit the contents of this Info node.
Allowed only if variable Info-enable-edit is non-nil."
  (interactive)
  (or Info-enable-edit
      (error "Editing info nodes is not enabled"))
  (use-local-map Info-edit-map)
  (setq major-mode 'Info-edit-mode)
  (setq mode-name "Info Edit")
  (kill-local-variable 'mode-line-buffer-identification)
  (setq buffer-read-only nil)
  ;; Make mode line update.
  (set-buffer-modified-p (buffer-modified-p))
  (message (substitute-command-keys
	     "Editing: Type \\[Info-cease-edit] to return to info")))

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (use-local-map Info-mode-map)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (Info-set-mode-line)
  (setq buffer-read-only t)
  ;; Make mode line update.
  (set-buffer-modified-p (buffer-modified-p))
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))
