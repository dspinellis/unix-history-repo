;; Convert texinfo files to info files.
;; Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

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


(defvar texinfo-format-syntax-table nil)

(defvar texinfo-vindex)
(defvar texinfo-findex)
(defvar texinfo-cindex)
(defvar texinfo-pindex)
(defvar texinfo-tindex)
(defvar texinfo-kindex)
(defvar texinfo-last-node)
(defvar texinfo-node-names)

(if texinfo-format-syntax-table
    nil
  (setq texinfo-format-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " texinfo-format-syntax-table)
  (modify-syntax-entry ?\\ " " texinfo-format-syntax-table)
  (modify-syntax-entry ?@ "\\" texinfo-format-syntax-table)
  (modify-syntax-entry ?\^q "\\" texinfo-format-syntax-table)
  (modify-syntax-entry ?\[ "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\] "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\( "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\) "." texinfo-format-syntax-table)
  (modify-syntax-entry ?{ "(}" texinfo-format-syntax-table)
  (modify-syntax-entry ?} "){" texinfo-format-syntax-table)
  (modify-syntax-entry ?\' "." texinfo-format-syntax-table))

(defun texinfo-format-buffer (&optional notagify)
  "Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually."
  (interactive "P")
  (let ((lastmessage "Formatting Info file..."))
    (message lastmessage)
    (texinfo-format-buffer-1)
    (if notagify
	nil
      (if (> (buffer-size) 30000)
	  (progn
	    (message (setq lastmessage "Making tags table for Info file..."))
	    (Info-tagify)))
      (if (> (buffer-size) 100000)
	  (progn
	    (message (setq lastmessage "Splitting Info file..."))
	    (Info-split))))
    (message (concat lastmessage
		     (if (interactive-p) "done.  Now save it." "done.")))))

(defun texinfo-format-buffer-1 ()
  (let (texinfo-format-filename
	texinfo-example-start
	texinfo-command-start
	texinfo-command-end
	texinfo-command-name
	texinfo-last-node
	texinfo-vindex
	texinfo-findex
	texinfo-cindex
	texinfo-pindex
	texinfo-tindex
	texinfo-kindex
	texinfo-stack
	texinfo-node-names
	outfile
	(fill-column fill-column)
	(input-buffer (current-buffer))
	(input-directory default-directory))
    (save-excursion
      (goto-char (point-min))
      (search-forward "@setfilename")
      (setq texinfo-command-end (point))
      (setq outfile (texinfo-parse-line-arg)))
    (find-file outfile)
    (texinfo-mode)
    (set-syntax-table texinfo-format-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (goto-char (point-min))
    (search-forward "@setfilename")
    (beginning-of-line)
    (delete-region (point-min) (point))
    ;; Remove @bye at end of file, if it is there.
    (goto-char (point-max))
    (if (search-backward "@bye" nil t)
	(delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
	(insert "\n"))
    ;; Scan the whole buffer, converting to Info format.
    (texinfo-format-scan)
    ;; Return data for indices.
    (goto-char (point-min))
    (list outfile
	  texinfo-vindex texinfo-findex texinfo-cindex
	  texinfo-pindex texinfo-tindex texinfo-kindex)))

(defvar texinfo-region-buffer-name "*Info Region*"
  "*Name of the temporary buffer used by \\[texinfo-format-region].")

(defun texinfo-format-region (region-beginning region-ending)
  "Convert the the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer."
  (interactive "r")
  (message "Converting region to Info format...")
  (let (texinfo-command-start
	texinfo-command-end
	texinfo-command-name
	texinfo-vindex
	texinfo-findex
	texinfo-cindex
	texinfo-pindex
	texinfo-tindex
	texinfo-kindex
	texinfo-stack
	texinfo-format-filename
	texinfo-example-start
	texinfo-last-node
	texinfo-node-names
	(fill-column fill-column)
	(input-buffer (current-buffer))
	(input-directory default-directory)
	filename-beginning
	filename-ending)

;;; Find a buffer to use.

    (switch-to-buffer (get-buffer-create texinfo-region-buffer-name))

    ;; Insert the region into the buffer.
    (erase-buffer)

    (save-excursion
      (set-buffer input-buffer)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Initialize the buffer with the filename
	  ;; or else explain that a filename is needed.
	  (or (search-forward "@setfilename"
			      (save-excursion (forward-line 100) (point)) t)
	      (error "The texinfo file needs a line saying: @setfilename <name>"))
	  (beginning-of-line)
	  (setq filename-beginning (point))
	  (forward-line 1)
	  (setq filename-ending (point)))))

    ;; Insert the @setfilename line into the buffer.
    (insert-buffer-substring input-buffer
			     (min filename-beginning region-beginning)  
			     filename-ending)
    
    ;; Insert the region into the buffer.
    (insert-buffer-substring input-buffer
			     (max region-beginning filename-ending)
			     region-ending)

    (texinfo-mode)

    ;; Install a syntax table useful for scanning command operands.
    (set-syntax-table texinfo-format-syntax-table)
    
    ;; If the region includes the effective end of the data,
    ;; discard everything after that.
    (goto-char (point-max))
    (if (re-search-backward "^@bye" nil t)
	(delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
	(insert "\n"))

    ;; Now convert for real.
    (goto-char (point-min))
    (texinfo-format-scan)
    (goto-char (point-min)))

  (message "Done."))

;; Perform those texinfo-to-info conversions that apply to the whole input
;; uniformly.
(defun texinfo-format-scan ()
  ;; Convert left and right quotes to typewriter font quotes.
  (goto-char (point-min))
  (while (search-forward "``" nil t)
    (replace-match "\""))
  (goto-char (point-min))
  (while (search-forward "''" nil t)
    (replace-match "\""))
  ;; Scan for @-commands.
  (goto-char (point-min))
  (while (search-forward "@" nil t)
    (if (looking-at "[@{}'` *]")
	;; Handle a few special @-followed-by-one-char commands.
	(if (= (following-char) ?*)
	    ;; @* has no effect, since we are not filling.
	    (delete-region (1- (point)) (1+ (point)))
	  ;; The other characters are simply quoted.  Delete the @.
	  (delete-char -1)
	  (forward-char 1))
      ;; @ is followed by a command-word; find the end of the word.
      (setq texinfo-command-start (1- (point)))
      (if (= (char-syntax (following-char)) ?w)
	  (forward-word 1)
	(forward-char 1))
      (setq texinfo-command-end (point))
      ;; Call the handler for this command.
      (setq texinfo-command-name
	    (intern (buffer-substring (1+ texinfo-command-start)
				      texinfo-command-end)))
      (let ((cmd (get texinfo-command-name 'texinfo-format)))
	(if cmd (funcall cmd)
	  (texinfo-unsupported)))))
  (cond (texinfo-stack
	 (goto-char (nth 2 (car texinfo-stack)))
	 (error "Unterminated @%s" (car (car texinfo-stack))))))

(put 'begin 'texinfo-format 'texinfo-format-begin)
(defun texinfo-format-begin ()
  (texinfo-format-begin-end 'texinfo-format))

(put 'end 'texinfo-format 'texinfo-format-end)
(defun texinfo-format-end ()
  (texinfo-format-begin-end 'texinfo-end))

(defun texinfo-format-begin-end (prop)
  (setq texinfo-command-name (intern (texinfo-parse-line-arg)))
  (setq cmd (get texinfo-command-name prop))
  (if cmd (funcall cmd)
    (texinfo-unsupported)))

(defun texinfo-parse-line-arg ()
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
	   (setq texinfo-command-end (1+ (point))))
	  ((looking-at "{")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq texinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid texinfo command arg format")))
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun texinfo-parse-expanded-arg ()
  (goto-char texinfo-command-end)
  (let ((start (point))
	marker)
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
	   (setq texinfo-command-end (1+ (point))))
	  ((looking-at "{")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq texinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid texinfo command arg format")))
    (setq marker (move-marker (make-marker) texinfo-command-end))
    (texinfo-format-expand-region start (point))
    (setq texinfo-command-end (marker-position marker))
    (move-marker marker nil)
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun texinfo-format-expand-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (let (texinfo-command-start
	  texinfo-command-end
	  texinfo-command-name
	  texinfo-stack)
      (texinfo-format-scan))
    (goto-char (point-max))))

(defun texinfo-parse-arg-discard ()
  (prog1 (texinfo-parse-line-arg)
	 (texinfo-discard-command)))

(defun texinfo-discard-command ()
  (delete-region texinfo-command-start texinfo-command-end))

(defun texinfo-format-parse-line-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (skip-chars-forward " ")
    (while (not (eolp))
      (setq beg (point))
      (re-search-forward "[\n,]")
      (setq next (point))
      (if (bolp) (setq next (1- next)))
      (forward-char -1)
      (skip-chars-backward " ")
      (setq end (point))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next)
      (skip-chars-forward " "))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (search-forward "{")
    (while (/= (preceding-char) ?\})
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (re-search-forward "[},]")
      (setq next (point))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (cond ((< beg end)
	     (goto-char beg)
	     (while (search-forward "\n" end t)
	       (replace-match " "))))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-defun-args ()
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (end-of-line)
    (setq texinfo-command-end (1+ (point)))
    (let ((marker (move-marker (make-marker) texinfo-command-end)))
      (texinfo-format-expand-region start (point))
      (setq texinfo-command-end (marker-position marker))
      (move-marker marker nil))
    (goto-char start)
    (let ((args '())
	  beg end)
      (skip-chars-forward " ")
      (while (not (eolp))
	(cond ((looking-at "{")
	       (setq beg (1+ (point)))
	       (forward-list 1)
	       (setq end (1- (point))))
	      (t
	       (setq beg (point))
	       (re-search-forward "[\n ]")
	       (forward-char -1)
	       (setq end (point))))
	(setq args (cons (buffer-substring beg end) args))
	(skip-chars-forward " "))
      (forward-char 1)
      (nreverse args))))

(put 'setfilename 'texinfo-format 'texinfo-format-setfilename)
(defun texinfo-format-setfilename ()
  (let ((arg (texinfo-parse-arg-discard)))
    (setq texinfo-format-filename
	  (file-name-nondirectory (expand-file-name arg)))
    (insert "Info file: "
	    texinfo-format-filename ",    -*-Text-*-\n"
	    "produced by texinfo-format-buffer\nfrom "
	    (if (buffer-file-name input-buffer)
		(concat "file: "
			(file-name-sans-versions
			 (file-name-nondirectory
			  (buffer-file-name input-buffer))))
	      (concat "buffer " (buffer-name input-buffer)))
	    "\n\n")))

(put 'node 'texinfo-format 'texinfo-format-node)
(defun texinfo-format-node ()
  (let* ((args (texinfo-format-parse-line-args))
	 (name (nth 0 args))
	 (next (nth 1 args))
	 (prev (nth 2 args))
	 (up (nth 3 args)))
    (texinfo-discard-command)
    (setq texinfo-last-node name)
    (let ((tem (downcase name)))
      (if (assoc tem texinfo-node-names)
	  (error "Duplicate node name: %s" name)
	(setq texinfo-node-names (cons tem texinfo-node-names))))
    (or (bolp)
	(insert ?\n))
    (insert "\^_\nFile: " texinfo-format-filename
	    "  Node: " name)
    (if prev
	(insert ", Prev: " prev))
    (if up
	(insert ", Up: " up))
    (if next
	(insert ", Next: " next))
    (insert ?\n)))

(put 'menu 'texinfo-format 'texinfo-format-menu)
(defun texinfo-format-menu ()
  (texinfo-discard-line)
  (insert "* Menu:\n\n"))

(put 'menu 'texinfo-end 'texinfo-discard-command)
(defun texinfo-discard-line ()
  (goto-char texinfo-command-end)
  (skip-chars-forward " \t")
  (or (eolp)
      (error "Extraneous text at end of command line."))
  (goto-char texinfo-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line."))
  (delete-region (point) (progn (forward-line 1) (point))))

; @xref {NODE, FNAME, NAME, FILE, DOCUMENT}
; -> *Note FNAME: (FILE)NODE
;   If FILE is missing,
;    *Note FNAME: NODE
;   If FNAME is empty and NAME is present
;    *Note NAME: Node
;   If both NAME and FNAME are missing
;    *Note NODE::
;   texinfo ignores the DOCUMENT argument.
; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;   If FILE is specified, (FILE)NODE is used for xrefs.
;   If fifth argument DOCUMENT is specified, produces
;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;    of DOCUMENT
(put 'xref 'texinfo-format 'texinfo-format-xref)
(defun texinfo-format-xref ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
	  (insert (car args) "::")
	(insert (or fname (car args)) ": ")
	(if (nth 3 args)
	    (insert "(" (nth 3 args) ")"))
	(insert (car args))))))

(put 'pxref 'texinfo-format 'texinfo-format-pxref)
(defun texinfo-format-pxref ()
  (texinfo-format-xref)
  (or (save-excursion
	(forward-char -2)
	(looking-at "::"))
      (insert ".")))

;@inforef{NODE, FNAME, FILE}
;Like @xref{NODE, FNAME,,FILE} in texinfo.
;In Tex, generates "See Info file FILE, node NODE"
(put 'inforef 'texinfo-format 'texinfo-format-inforef)
(defun texinfo-format-inforef ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))))

(put 'chapheading 'texinfo-format 'texinfo-format-chapter)
(put 'ichapter 'texinfo-format 'texinfo-format-chapter)
(put 'chapter 'texinfo-format 'texinfo-format-chapter)
(put 'iappendix 'texinfo-format 'texinfo-format-chapter)
(put 'appendix 'texinfo-format 'texinfo-format-chapter)
(put 'iunnumbered 'texinfo-format 'texinfo-format-chapter)
(put 'unnumbered 'texinfo-format 'texinfo-format-chapter)
(defun texinfo-format-chapter ()
  (texinfo-format-chapter-1 ?*))

(put 'heading 'texinfo-format 'texinfo-format-section)
(put 'isection 'texinfo-format 'texinfo-format-section)
(put 'section 'texinfo-format 'texinfo-format-section)
(put 'iappendixsection 'texinfo-format 'texinfo-format-section)
(put 'appendixsection 'texinfo-format 'texinfo-format-section)
(put 'iappendixsec 'texinfo-format 'texinfo-format-section)
(put 'appendixsec 'texinfo-format 'texinfo-format-section)
(put 'iunnumberedsec 'texinfo-format 'texinfo-format-section)
(put 'unnumberedsec 'texinfo-format 'texinfo-format-section)
(defun texinfo-format-section ()
  (texinfo-format-chapter-1 ?=))

(put 'subheading 'texinfo-format 'texinfo-format-subsection)
(put 'isubsection 'texinfo-format 'texinfo-format-subsection)
(put 'subsection 'texinfo-format 'texinfo-format-subsection)
(put 'iappendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'appendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'iunnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'unnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(defun texinfo-format-subsection ()
  (texinfo-format-chapter-1 ?-))

(put 'subsubheading 'texinfo-format 'texinfo-format-subsubsection)
(put 'isubsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'subsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'iappendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'appendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'iunnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'unnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(defun texinfo-format-subsubsection ()
  (texinfo-format-chapter-1 ?.))

(defun texinfo-format-chapter-1 (belowchar)
  (let ((arg (texinfo-parse-arg-discard)))
    (insert ?\n arg ?\n "@SectionPAD " belowchar ?\n)
    (forward-line -2)))

(put 'SectionPAD 'texinfo-format 'texinfo-format-sectionpad)
(defun texinfo-format-sectionpad ()
  (let ((str (texinfo-parse-arg-discard)))
    (forward-char -1)
    (let ((column (current-column)))
      (forward-char 1)
      (while (> column 0)
	(insert str)
	(setq column (1- column))))
    (insert ?\n)))

(put '\. 'texinfo-format 'texinfo-format-\.)
(defun texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

(put '\: 'texinfo-format 'texinfo-format-\:)
(defun texinfo-format-\: ()
  (texinfo-discard-command))

(put 'center 'texinfo-format 'texinfo-format-center)
(defun texinfo-format-center ()
  (texinfo-discard-command)
  (let ((indent-tabs-mode nil))
    (center-line)))

;; @itemize pushes (itemize "COMMANDS" STARTPOS) on texinfo-stack.
;; @enumerate pushes (enumerate 0 STARTPOS).
;; @item dispatches to the texinfo-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the @end is reached.

(defun texinfo-push-stack (check arg)
  (setq texinfo-stack
	(cons (list check arg texinfo-command-start)
	      texinfo-stack)))

(defun texinfo-pop-stack (check)
  (if (null texinfo-stack)
      (error "Unmatched @end %s" check))
  (if (not (eq (car (car texinfo-stack)) check))
      (error "@end %s matches @%s"
	     check (car (car texinfo-stack))))
  (prog1 (cdr (car texinfo-stack))
	 (setq texinfo-stack (cdr texinfo-stack))))

(put 'itemize 'texinfo-format 'texinfo-itemize)
(defun texinfo-itemize ()
  (texinfo-push-stack 'itemize (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'itemize 'texinfo-end 'texinfo-end-itemize)
(defun texinfo-end-itemize ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'itemize)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'enumerate 'texinfo-format 'texinfo-enumerate)
(defun texinfo-enumerate ()
  (texinfo-push-stack 'enumerate 0)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'enumerate 'texinfo-end 'texinfo-end-enumerate)
(defun texinfo-end-enumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'enumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'table 'texinfo-format 'texinfo-table)
(defun texinfo-table ()
  (texinfo-push-stack 'table (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'ftable 'texinfo-format 'texinfo-ftable)
(defun texinfo-ftable ()
  (texinfo-push-stack 'table "@code")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'description 'texinfo-format 'texinfo-description)
(defun texinfo-description ()
  (texinfo-push-stack 'table "@asis")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'table 'texinfo-end 'texinfo-end-table)
(put 'ftable 'texinfo-end 'texinfo-end-table)
(put 'description 'texinfo-end 'texinfo-end-table)
(defun texinfo-end-table ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'table)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; At the @end, indent all the lines within the construct
;; except those marked with backspace.  FROM says where
;; construct started.
(defun texinfo-do-itemize (from)
  (save-excursion
   (while (progn (forward-line -1)
		 (>= (point) from))
     (if (= (following-char) ?\b)
	 (save-excursion
	   (delete-char 1)
	   (end-of-line)
	   (delete-char 6))
       (if (not (looking-at "[ \t]*$"))
	   (save-excursion (insert "     ")))))))

(put 'item 'texinfo-format 'texinfo-item)
(put 'itemx 'texinfo-format 'texinfo-item)
(defun texinfo-item ()
  (funcall (get (car (car texinfo-stack)) 'texinfo-item)))

(put 'itemize 'texinfo-item 'texinfo-itemize-item)
(defun texinfo-itemize-item ()
  (texinfo-discard-line)
  (insert "\b   " (nth 1 (car texinfo-stack)) " \n")
  (forward-line -1))

(put 'enumerate 'texinfo-item 'texinfo-enumerate-item)
(defun texinfo-enumerate-item ()
  (texinfo-discard-line)
  (let ((next (1+ (car (cdr (car texinfo-stack))))))
    (setcar (cdr (car texinfo-stack)) next)
    (insert ?\b (format "%3d. " next) ?\n))
  (forward-line -1))

(put 'table 'texinfo-item 'texinfo-table-item)
(defun texinfo-table-item ()
  (let ((arg (texinfo-parse-arg-discard))
	(itemfont (car (cdr (car texinfo-stack)))))
    (insert ?\b itemfont ?\{ arg "}\n     \n"))
  (forward-line -2))

(put 'ifinfo 'texinfo-format 'texinfo-discard-line)
(put 'ifinfo 'texinfo-end 'texinfo-discard-command)

(put 'iftex 'texinfo-format 'texinfo-format-iftex)
(defun texinfo-format-iftex ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end iftex\n")
			(point))))

(put 'tex 'texinfo-format 'texinfo-format-tex)
(defun texinfo-format-tex ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end tex\n")
			(point))))

(put 'titlepage 'texinfo-format 'texinfo-format-titlepage)
(defun texinfo-format-titlepage ()
  (delete-region texinfo-command-start
		 (progn (search-forward "@end titlepage\n")
			(point))))

(put 'endtitlepage 'texinfo-format 'texinfo-discard-line)

(put 'ignore 'texinfo-format 'texinfo-format-ignore)
(defun texinfo-format-ignore ()
  (delete-region texinfo-command-start
		 (progn (search-forward "@end ignore\n")
			(point))))

(put 'endignore 'texinfo-format 'texinfo-discard-line)

(put 'var 'texinfo-format 'texinfo-format-var)
(defun texinfo-format-var ()
  (insert (upcase (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))

(put 'asis 'texinfo-format 'texinfo-format-noop)
(put 'b 'texinfo-format 'texinfo-format-noop)
(put 't 'texinfo-format 'texinfo-format-noop)
(put 'i 'texinfo-format 'texinfo-format-noop)
(put 'r 'texinfo-format 'texinfo-format-noop)
(put 'key 'texinfo-format 'texinfo-format-noop)
(put 'w 'texinfo-format 'texinfo-format-noop)
(defun texinfo-format-noop ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

(put 'code 'texinfo-format 'texinfo-format-code)
(put 'samp 'texinfo-format 'texinfo-format-code)
(put 'file 'texinfo-format 'texinfo-format-code)
(put 'kbd 'texinfo-format 'texinfo-format-code)
(put 'cite 'texinfo-format 'texinfo-format-code)
(defun texinfo-format-code ()
  (insert "`" (texinfo-parse-arg-discard) "'")
  (goto-char texinfo-command-start))

(put 'emph 'texinfo-format 'texinfo-format-emph)
(put 'strong 'texinfo-format 'texinfo-format-emph)
(defun texinfo-format-emph ()
  (insert "*" (texinfo-parse-arg-discard) "*")
  (goto-char texinfo-command-start))

(put 'defn 'texinfo-format 'texinfo-format-defn)
(put 'dfn 'texinfo-format 'texinfo-format-defn)
(defun texinfo-format-defn ()
  (insert "\"" (texinfo-parse-arg-discard) "\"")
  (goto-char texinfo-command-start))

(put 'bullet 'texinfo-format 'texinfo-format-bullet)
(defun texinfo-format-bullet ()
  (texinfo-discard-command)
  (insert "*"))

(put 'smallexample 'texinfo-format 'texinfo-format-example)
(put 'example 'texinfo-format 'texinfo-format-example)
(put 'quotation 'texinfo-format 'texinfo-format-example)
(put 'lisp 'texinfo-format 'texinfo-format-example)
(put 'display 'texinfo-format 'texinfo-format-example)
(put 'format 'texinfo-format 'texinfo-format-example)
(put 'flushleft 'texinfo-format 'texinfo-format-example)
(defun texinfo-format-example ()
  (texinfo-push-stack 'example nil)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'smallexample 'texinfo-end 'texinfo-end-example)
(put 'example 'texinfo-end 'texinfo-end-example)
(put 'quotation 'texinfo-end 'texinfo-end-example)
(put 'lisp 'texinfo-end 'texinfo-end-example)
(put 'display 'texinfo-end 'texinfo-end-example)
(put 'format 'texinfo-end 'texinfo-end-example)
(put 'flushleft 'texinfo-end 'texinfo-end-example)
(defun texinfo-end-example ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'example)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'exdent 'texinfo-format 'texinfo-format-exdent)
(defun texinfo-format-exdent ()
  (texinfo-discard-command)
  (delete-region (point)
		 (progn
		  (skip-chars-forward " ")
		  (point)))
  (insert ?\b)
  ;; Cancel out the deletion that texinfo-do-itemize
  ;; is going to do at the end of this line.
  (save-excursion
    (end-of-line)
    (insert "\n     ")))

(put 'ctrl 'texinfo-format 'texinfo-format-ctrl)
(defun texinfo-format-ctrl ()
  (let ((str (texinfo-parse-arg-discard)))
    (insert (logand 31 (aref str 0)))))

(put 'TeX 'texinfo-format 'texinfo-format-TeX)
(defun texinfo-format-TeX ()
  (texinfo-parse-arg-discard)
  (insert "TeX"))

(put 'copyright 'texinfo-format 'texinfo-format-copyright)
(defun texinfo-format-copyright ()
  (texinfo-parse-arg-discard)
  (insert "(C)"))

(put 'minus 'texinfo-format 'texinfo-format-minus)
(defun texinfo-format-minus ()
  (texinfo-parse-arg-discard)
  (insert "-"))

(put 'dots 'texinfo-format 'texinfo-format-dots)
(defun texinfo-format-dots ()
  (texinfo-parse-arg-discard)
  (insert "..."))

(put 'refill 'texinfo-format 'texinfo-format-refill)
(defun texinfo-format-refill ()
  (texinfo-discard-command)
  (fill-paragraph nil))

;; Index generation

(put 'vindex 'texinfo-format 'texinfo-format-vindex)
(defun texinfo-format-vindex ()
  (texinfo-index 'texinfo-vindex))

(put 'cindex 'texinfo-format 'texinfo-format-cindex)
(defun texinfo-format-cindex ()
  (texinfo-index 'texinfo-cindex))

(put 'findex 'texinfo-format 'texinfo-format-findex)
(defun texinfo-format-findex ()
  (texinfo-index 'texinfo-findex))

(put 'pindex 'texinfo-format 'texinfo-format-pindex)
(defun texinfo-format-pindex ()
  (texinfo-index 'texinfo-pindex))

(put 'tindex 'texinfo-format 'texinfo-format-tindex)
(defun texinfo-format-tindex ()
  (texinfo-index 'texinfo-tindex))

(put 'kindex 'texinfo-format 'texinfo-format-kindex)
(defun texinfo-format-kindex ()
  (texinfo-index 'texinfo-kindex))

(defun texinfo-index (indexvar)
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (set indexvar
	 (cons (list arg texinfo-last-node)
	       (symbol-value indexvar)))))

(defconst texinfo-indexvar-alist
  '(("cp" . texinfo-cindex)
    ("fn" . texinfo-findex)
    ("vr" . texinfo-vindex)
    ("tp" . texinfo-tindex)
    ("pg" . texinfo-pindex)
    ("ky" . texinfo-kindex)))

(put 'printindex 'texinfo-format 'texinfo-format-printindex)
(defun texinfo-format-printindex ()
  (let ((indexelts (symbol-value
		    (cdr (assoc (texinfo-parse-arg-discard)
				texinfo-indexvar-alist))))
	opoint)
    (insert "\n* Menu:\n\n")
    (setq opoint (point))
    (texinfo-print-index nil indexelts)
    (if (eq system-type 'vax-vms) 
	(texinfo-sort-region opoint (point))
      (shell-command-on-region opoint (point) "sort -fd" 1))))

(defun texinfo-print-index (file indexelts)
  (while indexelts
    (if (stringp (car (car indexelts)))
	(insert "* " (car (car indexelts))
		": " (if file (concat "(" file ")") "")
		(nth 1 (car indexelts)) ".\n")
      ;; index entries from @include'd file
      (texinfo-print-index (nth 1 (car indexelts))
			   (nth 2 (car indexelts))))
    (setq indexelts (cdr indexelts))))


;;;; Lisp Definitions

(defun texinfo-format-defun ()
  (texinfo-push-stack 'defun nil)
  (setq fill-column (- fill-column 5))
  (texinfo-format-defun-1 t))

(defun texinfo-format-defunx ()
  (texinfo-format-defun-1 nil))

(defun texinfo-format-defun-1 (first-p)
  (let ((args (texinfo-format-parse-defun-args))
	(type (get texinfo-command-name 'texinfo-defun-type)))
    (texinfo-discard-command)
    (if (eq type 'arg)
	(progn (setq type (car args))
	       (setq args (cdr args))))
    (let ((formatter (get texinfo-command-name 'texinfo-defun-format-type)))
      (if formatter
	  (setq type (funcall formatter type args))))
    ;; Delete extra newline inserted after previous header line.
    (if (not first-p)
	(delete-char -1))
    (insert "* " type ": " (car args))
    (let ((args (cdr args)))
      (while args
	(insert " "
		(if (= ?& (aref (car args) 0))
		    (car args)
		  (upcase (car args))))
	(setq args (cdr args))))
    ;; Insert extra newline so that paragraph filling does not mess
    ;; with header line.
    (insert "\n\n")
    (rplaca (cdr (cdr (car texinfo-stack))) (point))
    (let ((indexvar (get texinfo-command-name 'texinfo-defun-index))
	  (formatter (get texinfo-command-name 'texinfo-defun-format-index)))
      (set indexvar
	   (cons (list (if formatter (funcall formatter type args) (car args))
		       texinfo-last-node)
		 (symbol-value indexvar))))))

(defun texinfo-end-defun ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((start (nth 1 (texinfo-pop-stack 'defun))))
    (texinfo-do-itemize start)
    ;; Delete extra newline inserted after header.
    (save-excursion
      (goto-char start)
      (delete-char -1))))

(put 'deffn 'texinfo-format 'texinfo-format-defun)
(put 'deffnx 'texinfo-format 'texinfo-format-defunx)
(put 'deffn 'texinfo-end 'texinfo-end-defun)
(put 'deffn 'texinfo-defun-type 'arg)
(put 'deffnx 'texinfo-defun-type 'arg)
(put 'deffn 'texinfo-defun-index 'texinfo-findex)
(put 'deffnx 'texinfo-defun-index 'texinfo-findex)

(put 'defun 'texinfo-format 'texinfo-format-defun)
(put 'defunx 'texinfo-format 'texinfo-format-defunx)
(put 'defun 'texinfo-end 'texinfo-end-defun)
(put 'defun 'texinfo-defun-type "Function")
(put 'defunx 'texinfo-defun-type "Function")
(put 'defun 'texinfo-defun-index 'texinfo-findex)
(put 'defunx 'texinfo-defun-index 'texinfo-findex)

(put 'defmac 'texinfo-format 'texinfo-format-defun)
(put 'defmacx 'texinfo-format 'texinfo-format-defunx)
(put 'defmac 'texinfo-end 'texinfo-end-defun)
(put 'defmac 'texinfo-defun-type "Macro")
(put 'defmacx 'texinfo-defun-type "Macro")
(put 'defmac 'texinfo-defun-index 'texinfo-findex)
(put 'defmacx 'texinfo-defun-index 'texinfo-findex)

(put 'defspec 'texinfo-format 'texinfo-format-defun)
(put 'defspecx 'texinfo-format 'texinfo-format-defunx)
(put 'defspec 'texinfo-end 'texinfo-end-defun)
(put 'defspec 'texinfo-defun-type "Special form")
(put 'defspecx 'texinfo-defun-type "Special form")
(put 'defspec 'texinfo-defun-index 'texinfo-findex)
(put 'defspecx 'texinfo-defun-index 'texinfo-findex)

(put 'defvr 'texinfo-format 'texinfo-format-defun)
(put 'defvrx 'texinfo-format 'texinfo-format-defunx)
(put 'defvr 'texinfo-end 'texinfo-end-defun)
(put 'defvr 'texinfo-defun-type 'arg)
(put 'defvrx 'texinfo-defun-type 'arg)
(put 'defvr 'texinfo-defun-index 'texinfo-vindex)
(put 'defvrx 'texinfo-defun-index 'texinfo-vindex)

(put 'defvar 'texinfo-format 'texinfo-format-defun)
(put 'defvarx 'texinfo-format 'texinfo-format-defunx)
(put 'defvar 'texinfo-end 'texinfo-end-defun)
(put 'defvar 'texinfo-defun-type "Variable")
(put 'defvarx 'texinfo-defun-type "Variable")
(put 'defvar 'texinfo-defun-index 'texinfo-vindex)
(put 'defvarx 'texinfo-defun-index 'texinfo-vindex)

(put 'defopt 'texinfo-format 'texinfo-format-defun)
(put 'defoptx 'texinfo-format 'texinfo-format-defunx)
(put 'defopt 'texinfo-end 'texinfo-end-defun)
(put 'defopt 'texinfo-defun-type "User Option")
(put 'defoptx 'texinfo-defun-type "User Option")
(put 'defopt 'texinfo-defun-index 'texinfo-vindex)
(put 'defoptx 'texinfo-defun-index 'texinfo-vindex)

(put 'deftp 'texinfo-format 'texinfo-format-defun)
(put 'deftpx 'texinfo-format 'texinfo-format-defunx)
(put 'deftp 'texinfo-end 'texinfo-end-defun)
(put 'deftp 'texinfo-defun-type 'arg)
(put 'deftpx 'texinfo-defun-type 'arg)
(put 'deftp 'texinfo-defun-index 'texinfo-tindex)
(put 'deftpx 'texinfo-defun-index 'texinfo-tindex)

;;; Object-oriented stuff is a little hairier.

(put 'defop 'texinfo-format 'texinfo-format-defun)
(put 'defopx 'texinfo-format 'texinfo-format-defunx)
(put 'defop 'texinfo-end 'texinfo-end-defun)
(put 'defop 'texinfo-defun-type 'arg)
(put 'defopx 'texinfo-defun-type 'arg)
(put 'defop 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defopx 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defop 'texinfo-defun-index 'texinfo-findex)
(put 'defopx 'texinfo-defun-index 'texinfo-findex)
(put 'defop 'texinfo-defun-format-index 'texinfo-format-defop-index)
(put 'defopx 'texinfo-defun-format-index 'texinfo-format-defop-index)

(put 'defmethod 'texinfo-format 'texinfo-format-defun)
(put 'defmethodx 'texinfo-format 'texinfo-format-defunx)
(put 'defmethod 'texinfo-end 'texinfo-end-defun)
(put 'defmethod 'texinfo-defun-type "Operation")
(put 'defmethodx 'texinfo-defun-type "Operation")
(put 'defmethod 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defmethodx 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defmethod 'texinfo-defun-index 'texinfo-findex)
(put 'defmethodx 'texinfo-defun-index 'texinfo-findex)
(put 'defmethod 'texinfo-defun-format-index 'texinfo-format-defop-index)
(put 'defmethodx 'texinfo-defun-format-index 'texinfo-format-defop-index)

(defun texinfo-format-defop-type (type args)
  (format "%s on %s" type (car args)))

(defun texinfo-format-defop-index (type args)
  (format "%s on %s" (car (cdr args)) (car args)))

(put 'defcv 'texinfo-format 'texinfo-format-defun)
(put 'defcvx 'texinfo-format 'texinfo-format-defunx)
(put 'defcv 'texinfo-end 'texinfo-end-defun)
(put 'defcv 'texinfo-defun-type 'arg)
(put 'defcvx 'texinfo-defun-type 'arg)
(put 'defcv 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defcvx 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defcv 'texinfo-defun-index 'texinfo-vindex)
(put 'defcvx 'texinfo-defun-index 'texinfo-vindex)
(put 'defcv 'texinfo-defun-format-index 'texinfo-format-defcv-index)
(put 'defcvx 'texinfo-defun-format-index 'texinfo-format-defcv-index)

(put 'defivar 'texinfo-format 'texinfo-format-defun)
(put 'defivarx 'texinfo-format 'texinfo-format-defunx)
(put 'defivar 'texinfo-end 'texinfo-end-defun)
(put 'defivar 'texinfo-defun-type "Instance variable")
(put 'defivarx 'texinfo-defun-type "Instance variable")
(put 'defivar 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defivarx 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defivar 'texinfo-defun-index 'texinfo-vindex)
(put 'defivarx 'texinfo-defun-index 'texinfo-vindex)
(put 'defivar 'texinfo-defun-format-index 'texinfo-format-defcv-index)
(put 'defivarx 'texinfo-defun-format-index 'texinfo-format-defcv-index)

(defun texinfo-format-defcv-type (type args)
  (format "%s of %s" type (car args)))

(defun texinfo-format-defcv-index (type args)
  (format "%s of %s" (car (cdr args)) (car args)))

;; process included files
(put 'include 'texinfo-format 'texinfo-format-include)
(defun texinfo-format-include ()
  (let ((filename (texinfo-parse-arg-discard))
	(default-directory input-directory)
	subindex)
    (setq subindex
	  (save-excursion
	    (progn (find-file
		    (cond ((file-readable-p (concat filename ".texinfo"))
			   (concat filename ".texinfo"))
			  ((file-readable-p (concat filename ".tex"))
			   (concat filename ".tex"))
			  ((file-readable-p filename)
			   filename)
			  (t (error "@include'd file %s not found"
				    filename))))
		   (texinfo-format-buffer-1))))
    (texinfo-subindex 'texinfo-vindex (car subindex) (nth 1 subindex))
    (texinfo-subindex 'texinfo-findex (car subindex) (nth 2 subindex))
    (texinfo-subindex 'texinfo-cindex (car subindex) (nth 3 subindex))
    (texinfo-subindex 'texinfo-pindex (car subindex) (nth 4 subindex))
    (texinfo-subindex 'texinfo-tindex (car subindex) (nth 5 subindex))
    (texinfo-subindex 'texinfo-kindex (car subindex) (nth 6 subindex))))

(defun texinfo-subindex (indexvar file content)
  (set indexvar (cons (list 'recurse file content)
		      (symbol-value indexvar))))


;; Lots of bolio constructs do nothing in texinfo.

(put 'page 'texinfo-format 'texinfo-discard-line-with-args)
(put 'c 'texinfo-format 'texinfo-discard-line-with-args)
(put 'comment 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setchapternewpage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'contents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'summarycontents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'nopara 'texinfo-format 'texinfo-discard-line-with-args)
(put 'noindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setx 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setq 'texinfo-format 'texinfo-discard-line-with-args)
(put 'settitle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'defindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'synindex 'texinfo-format 'texinfo-discard-line-with-args)
(put 'hsize 'texinfo-format 'texinfo-discard-line-with-args)
(put 'parindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'lispnarrowing 'texinfo-format 'texinfo-discard-line-with-args)
(put 'itemindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'headings 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-end 'texinfo-discard-line-with-args)
(put 'bye 'texinfo-format 'texinfo-discard-line)
(put 'smallbook 'texinfo-format 'texinfo-discard-line)

(defun texinfo-discard-line-with-args ()
  (goto-char texinfo-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))

;; Sort an index which is in the current buffer between START and END.
;; Used on VMS, where the `sort' utility is not available.
(defun texinfo-sort-region (start end)
  (require 'sort)
  (save-restriction
    (narrow-to-region start end)
    (sort-subr nil 'forward-line 'end-of-line 'texinfo-sort-startkeyfun)))

;; Subroutine for sorting an index.
;; At start of a line, return a string to sort the line under.
(defun texinfo-sort-startkeyfun ()
  (let ((line
	 (buffer-substring (point) (save-excursion (end-of-line) (point)))))
    ;; Canonicalize whitespace and eliminate funny chars.
    (while (string-match "[ \t][ \t]+\\|[^a-z0-9 ]+" line)
      (setq line (concat (substring line 0 (match-beginning 0))
			 " "
			 (substring line (match-end 0) (length line)))))
    line))

;; Some cannot be handled

(defun texinfo-unsupported ()
  (error "%s is not handled by texinfo"
	 (buffer-substring texinfo-command-start texinfo-command-end)))

(defun batch-texinfo-format ()
  "Runs  texinfo-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texinfo-format $docs/ ~/*.texinfo\"."
  (if (not noninteractive)
      (error "batch-texinfo-format may only be used -batch."))
  (let ((version-control t)
	(auto-save-default nil)
	(find-file-run-dired nil)
	(kept-old-versions 259259)
	(kept-new-versions 259259))
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left
		     (nconc (directory-files file)
			    (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (buffer-flush-undo (current-buffer))
	      (set-buffer-modified-p nil)
	      (texinfo-mode)
	      (message "texinfo formatting %s..." file)
	      (texinfo-format-buffer nil)
	      (if (buffer-modified-p)
		  (progn (message "Saving modified %s" (buffer-file-name))
			 (save-buffer))))
	  (error
	   (message ">> Error: %s" (prin1-to-string err))
	   (message ">>  point at")
	   (let ((s (buffer-substring (point)
				      (min (+ (point) 100)
					   (point-max))))
		 (tem 0))
	     (while (setq tem (string-match "\n+" s tem))
	       (setq s (concat (substring s 0 (match-beginning 0))
			       "\n>>  "
			       (substring s (match-end 0)))
		     tem (1+ tem)))
	     (message ">>  %s" s)))
	  (setq error 1)))
      (kill-emacs error))))
