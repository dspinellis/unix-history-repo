;; Info package for Emacs  -- could use a "create node" feature.
;; Copyright (C) 1985 Richard M. Stallman.

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

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker (make-marker)
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defun info ()
  "Create a buffer for Info, the documentation browser program."
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
	(setq temp (expand-file-name filename Info-directory))
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
  (Info-mode)
  (widen)
  (setq Info-current-file nil Info-current-node nil)
  (unwind-protect
      (progn
	;; Switch files if necessary
	(or (null filename)
	    (equal buffer-file-name filename)
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (insert-file-contents filename t)
	      ;; See whether file has a tag table.  Record the location if yes.
	      (move-marker Info-tag-table-marker nil)
	      (goto-char (point-max))
	      (forward-line -8)
	      (if (search-forward "\^_\nEnd tag table\n" nil t)
		  (progn
		   (search-backward "\nTag table:\n")
		   (move-marker Info-tag-table-marker (match-end 0))))))
	(if (equal nodename "*")
	    (progn (setq Info-current-file buffer-file-name
			 Info-current-node nodename)
		   (Info-set-mode-line))
	  ;; Search file for a suitable node.
	  ;; First get advice from tag table if file has one.
	  (if (marker-position Info-tag-table-marker)
	      (progn
		(goto-char Info-tag-table-marker)
		(if (search-forward (concat "Node: " nodename "\177") nil t)
		    (goto-char (max (point-min) (- (read (current-buffer)) 1000)))
		  (error "No such node: %s" nodename)))
	    (goto-char (point-min)))
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
   (setq Info-current-file buffer-file-name)
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
  (setq mode-line-format
	(concat 
	 "--%1*%1*-Info:  ("
	 (if buffer-file-name
	     (file-name-nondirectory buffer-file-name)
	   "")
	 ")"
	 (or Info-current-node "")
	 "   %M   %[(%m)%]----%3p-%-")))

;; Go to an info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

(defun Info-goto-node (nodename)
  "Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME."
  (interactive "sGoto node: ")
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (substring nodename (match-beginning 2) (match-end 2))
	  nodename (substring nodename (match-beginning 3) (match-end 3)))
    (let ((trim (string-match "\\s *\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s *\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename))))

(defun Info-tagify ()
  "Create or update tag table of current info file."
  (interactive)
  ;; Save and restore point and restrictions.
  ;; save-restrictions would not work
  ;; because it records the old max relative to the end.
  ;; We record it relative to the beginning.
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point)))
    (unwind-protect
	(progn
	  (widen)
	  (goto-char (point-min))
	  (let ((regexp "Node:[ \t]*\\([^,\n\t]\\)*[,\t\n]")
		(case-fold-search t)
		list)
	    (while (search-forward "\n\^_" nil t)
	      (forward-line 1)
	      (let ((beg (point)))
		(forward-line 1)
		(if (re-search-backward regexp beg t)
		    (setq list
			  (cons (list (buffer-substring
				        (match-beginning 1)
					(match-end 1))
				      beg)
				list)))))
	    (goto-char (point-max))
	    (forward-line -8)
	    (let ((buffer-read-only nil))
	      (if (search-forward "\^_\nEnd tag table\n" nil t)
		  (let ((end (point)))
		    (search-backward "\nTag table:\n")
		    (beginning-of-line)
		    (delete-region (point) end)))
	      (goto-char (point-max))
	      (insert "\^_\f\nTag table:\n")
	      (move-marker Info-tag-table-marker (point))
	      (setq list (nreverse list))
	      (while list
		(insert "Node: " (car (car list)) ?\177)
		(princ (car (cdr (car list))) (current-buffer))
		(insert ?\n)
		(setq list (cdr list)))
	      (insert "\^_\nEnd tag table\n"))))
      (goto-char opoint)
      (narrow-to-region omin (min omax (point-max))))))

(defun Info-validate ()
  "Check that every node pointer points to an existing node."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((allnodes '(("*")))
	    (regexp "Node:[ \t]*\\([^,\n\t]*\\)[,\t\n]")
	    (case-fold-search t)
	    (tags-losing nil)
	    (lossages ()))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point)))
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(setq allnodes
		      (cons (list (downcase
				    (buffer-substring
				      (match-beginning 1)
				      (progn
					(goto-char (match-end 1))
					(skip-chars-backward " \t")
					(point))))
				  (progn
				    (end-of-line)
				    (and (re-search-backward "prev[ious]*:"
							     beg t)
					 (progn
					   (goto-char (match-end 0))
					   (downcase
					     (Info-following-node-name)))))
				  (point))
			    allnodes)))))
	(goto-char (point-min))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point))
		thisnode next)
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(save-restriction
		  (search-forward "\n\^_" nil 'move)
		  (narrow-to-region beg (point))
		  (setq thisnode (downcase
				   (buffer-substring
				     (match-beginning 1)
				     (progn
				       (goto-char (match-end 1))
				       (skip-chars-backward " \t")
				       (point)))))
		  (end-of-line)
		  (and (search-backward "next:" nil t)
		       (setq next (Info-validate-node-name "Next"))
		       (if (equal (car (cdr (assoc next allnodes)))
				  thisnode)
			   ;; allow multiple `next' pointers to one node
			   (let ((tem lossages))
			     (while tem
			       (if (and (equal (car (cdr (car tem)))
					       "Previous-pointer in Next")
					(equal (car (cdr (cdr (car tem))))
					       next))
				   (setq lossages (delq (car tem) lossages)))
			       (setq tem (cdr tem))))
			 (setq lossages
			       (cons (list thisnode
					   "Previous-pointer in Next"
					   next)
				     lossages))))
		  (end-of-line)
		  (if (search-backward "prev[ious]*:" nil t)
		      (Info-validate-node-name "Previous"))
		  (end-of-line)
		  (if (search-backward "up:" nil t)
		      (Info-validate-node-name "Up"))
		  (if (search-forward "\n* Menu:" nil t)
		      (while (search-forward "\n* " nil t)
			(Info-validate-node-name
			 (concat "menu item "
				 (buffer-substring (point)
						   (save-excursion
						     (skip-chars-forward "^:")
						     (point))))
			 (Info-extract-menu-node-name))))
		  (goto-char (point-min))
		  (while (re-search-forward "\\*note[ \n]*[^:\t]*:" nil t)
		    (goto-char (+ (match-beginning 0) 5))
		    (skip-chars-forward " \n")
		    (Info-validate-node-name
		     (concat "reference "
			     (buffer-substring (point)
					       (save-excursion
						 (skip-chars-forward "^:")
						 (point))))
		     (Info-extract-menu-node-name "Bad format cross-reference")))))))
	(setq tags-losing (not (Info-validate-tags-table)))
	(if (or lossages tags-losing)
	    (with-output-to-temp-buffer " *problems in info file*"
	      (while lossages
		(princ "In node \"")
		(princ (car (car lossages)))
		(princ "\", invalid ")
		(let ((tem (nth 1 (car lossages))))
		  (cond ((string-match "\n" tem)
			 (princ (substring tem 0 (match-beginning 0)))
			 (princ "..."))
			(t
			 (princ tem))))
		(princ ": ")
		(let ((tem (nth 2 (car lossages))))
		  (cond ((string-match "\n" tem)
			 (princ (substring tem 0 (match-beginning 0)))
			 (princ "..."))
			(t
			 (princ tem))))
		(terpri)
		(setq lossages (cdr lossages)))
	      (if tags-losing (princ "\nTags table must be recomputed\n")))
	  (message "File appears valid"))))))

(defun Info-validate-node-name (kind &optional name)
  (if name
      nil
    (goto-char (match-end 0))
    (skip-chars-forward " \t")
    (if (= (following-char) ?\()
	nil
      (setq name
	    (buffer-substring
	     (point)
	     (progn
	      (skip-chars-forward "^,\t\n")
	      (skip-chars-backward " ")
	      (point))))))
  (if (null name)
      nil
    (setq name (downcase name))
    (or (and (> (length name) 0) (= (aref name 0) ?\())
	(assoc name allnodes)
	(setq lossages
	      (cons (list thisnode kind name) lossages))))
  name)

(defun Info-validate-tags-table ()
  (goto-char (point-min))
  (if (not (search-forward "\^_\nEnd tag table\n" nil t))
      t
    (not (catch 'losing
	   (let* ((end (match-beginning 0))
		  (start (progn (search-backward "\nTag table:\n")
				(1- (match-end 0))))
		  tem)
	     (setq tem allnodes)
	     (while tem
	       (goto-char start)
	       (or (equal (car (car tem)) "*")
		   (search-forward (concat "Node: "
					   (car (car tem))
					   "\177")
				   end t)
		   (throw 'losing 'x))
	       (setq tem (cdr tem)))
	     (goto-char (1+ start))
	     (while (looking-at ".*Node: \\(.*\\)\177\\([0-9]+\\)$")
	       (setq tem (assoc (buffer-substring
				  (match-beginning 1)
				  (match-end 1))
				allnodes))
	       (if (or (not tem)
		       (< 1000 (progn
				 (goto-char (match-beginning 2))
				 (setq tem (- (car (cdr (cdr tem)))
					      (read (current-buffer))))
				 (if (> tem 0) tem (- tem)))))
		   (throw 'losing 'y)))
	     (forward-line 1))
	   (or (looking-at "End tag table\n")
	       (throw 'losing 'z))
	   nil))))

(defvar Info-last-search nil
  "Default regexp for Info S command to search for.")

(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in."
  (interactive "sSearch (regexp): ")
  (if (equal regexp "")
      (setq regexp Info-last-search)
    (setq Info-last-search regexp))
  (let (found)
    (save-excursion
     (save-restriction
      (widen)
      (re-search-forward regexp)
      (setq found (point))))
    (if found
	(progn
	 (widen)
	 (goto-char found)
	 (Info-select-node)))))

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
    (skip-chars-forward (or allowedchars "^,\t\n"))
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
       (while (re-search-forward "\\*note[ \n]*[^:\t]*:" nil t)
	 (setq str (buffer-substring
		    (+ (match-beginning 0) 6)
		    (1- (point))))
	 (setq i 0)
	 (while (setq i (string-match "\n" str i))
	   (aset str i ?\ ))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     (if completions
	 (list (completing-read "Follow reference named: " completions nil t))
       (error "No cross-references in this node"))))
  (let (target beg i (str (concat "\\*note " footnotename)))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "[ \n]*" (substring str (1+ i))))
      (setq i (+ i 5)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward str nil t)
	  (error "No cross-reference named %s" footnotename))
      (goto-char (+ (match-beginning 0) 5))
      (setq target (Info-extract-menu-node-name "Bad format cross reference")))
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
   (let (completions
	 (completion-ignore-case t))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (while (re-search-forward
		"\n\\* \\([^:\t\n]*\\):" nil t)
	 (setq completions
	       (cons (cons (buffer-substring
			     (match-beginning 1)
			     (match-end 1))
			   (match-beginning 1))
		     completions))))
     (list (completing-read "Menu item: " completions nil t))))
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
  (Info-set-mode-line))

(defvar Info-edit-map nil
  "Local keymap used within `e' command of Info.")
(if Info-edit-map
    nil
  (setq Info-edit-map (copy-alist text-mode-map))
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
  (setq mode-line-format default-mode-line-format)
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
