;;; C comment mode - An auto-filled comment mode for gnu c-mode.
;;;
;;; Author:  	Robert Mecklenburg
;;;		Computer Science Dept.
;;;          	University of Utah
;;; From: mecklen@utah-gr.UUCP (Robert Mecklenburg)
;;;   Also hartzell@Boulder.Colorado.EDU
;;; (c) 1986, University of Utah
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, provided the people they give it to can.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I have written a "global comment" minor-mode which performs auto-fill,
;;; fill-paragraph, and auto-indentation functions.  This function only
;;; works for comments which occupy an entire line (not comments to the
;;; right of code).  The mode has several options set through variables.
;;; If the variable c-comment-starting-blank is non-nil multi-line
;;; comments come out like this:
;;; 
;;; 	/*
;;; 	 * Your favorite 
;;; 	 * multi-line comment.
;;; 	 */
;;; 
;;; otherwise they look like this:
;;; 
;;; 	/* Your Favorite
;;; 	 * multi-line comment.
;;; 	 */
;;; 
;;; If the variable c-comment-hanging-indent is non-nil K&R style comments
;;; are indented automatically like this:
;;; 
;;; 	/* my_func - For multi-line comments with hanging indent
;;; 	 *	     the text is lined up after the dash.
;;; 	 */
;;; 
;;; otherwise the text "the text" (!) is lined up under my_func.  If a
;;; comment fits (as typed) on a single line it remains a single line
;;; comment even if c-comment-starting-blank is set.  If
;;; c-comment-indenting is non-nil hitting carriage return resets the
;;; indentation for the next line to the current line's indentation
;;; (within the comment) like this:
;;; 
;;; 	/* Typing along merrily....
;;; 	 *     Now I indent with spaces, when I hit return
;;; 	 *     the indentation is automatically set to 
;;; 	 *     ^ here.
;;; 	 */
;;; 
;;; Due to my lack of understanding of keymaps this permanently resets M-q
;;; to my own fill function.  I would like to have the comment mode
;;; bindings only in comment mode but I can't seem to get that to work.
;;; If some gnu guru can clue me in, I'd appreciate it.
;;;
(defvar c-comment-starting-blank t
  "*Controls whether global comments have an initial blank line.")
(defvar c-comment-indenting t
  "*If set global comments are indented to the level of the previous line.")
(defvar c-comment-hanging-indent t
  "*If true, comments will be automatically indented to the dash.")
(defvar c-hang-already-done t
  "If true we have performed the haning indent already for this comment.")


;;;
;;; c-comment-map - This is a sparse keymap for comment mode which
;;; 		    gets inserted when c-comment is called.
;;; 
(defvar c-comment-mode-map ()
  "Keymap used in C comment mode.")
(if c-comment-mode-map
    ()
  (setq c-comment-mode-map (copy-keymap c-mode-map))
  (define-key c-comment-mode-map "\e\r" 'newline)
  (define-key c-comment-mode-map "\eq" 'set-fill-and-fill)
  (define-key c-comment-mode-map "\r" 'set-fill-and-return))
 
;;;
;;; c-comment - This is a filled comment mode which can format
;;; 		indented text, do hanging indents, and symetric
;;; 		placement of comment delimiters.
;;; 
(defun c-comment ()
  "Edit a C comment with filling and indentation.
This performs hanging indentation, symmetric placement of delimiters,
 and Indented-Text mode style indentation.  Type 'M-x apropos
c-comment' for information on options."
  (interactive)
  (let
      ;; Save old state.
      ((auto-fill-hook (if c-comment-indenting
			   'do-indented-auto-fill 'do-auto-fill))
;       (comment-start nil)
       (comment-multi-line t)
       (comment-start-skip "/*\\*+[ 	]*")
       (paragraph-start-ref paragraph-start)
       fill-prefix paragraph-start paragraph-separate opoint)

    ;; Determine if we are inside a comment.
    (setq in-comment
	  (save-excursion
	    (and (re-search-backward "/\\*\\|\\*/" 0 t)
		 (string= "/*" (buffer-substring (point) (+ (point) 2))))))

    ;; Indent the comment and set the fill prefix to comment continuation
    ;; string.  If we are already in a comment get the indentation on
    ;; the current line.
    (setq c-hang-already-done nil)

    ;; Set the beginning of the comment and insert the blank line if needed.
    (use-local-map c-comment-mode-map)
    (if (not in-comment)
	(progn (c-indent-line)
	       (insert "/* ")
	       (setq fill-prefix (get-current-fill (point)))
	       (recursive-edit)

	       ;; If the comment fits on one line, place the close
	       ;; comment at the end of the line.  Otherwise, newline.
	       (setq opoint (point))
	       (if (and (save-excursion (beginning-of-line)
					(search-forward "/*" opoint t))
			(<= (+ (current-column) 3) 79))
		   (insert " */")
		 (insert "\n*/"))

	       (c-indent-line))
      (progn (setq fill-prefix (get-current-fill (point)))
	     (recursive-edit)
	     (search-forward "*/" (buffer-size) t)
	     (forward-line 1)))

    ;; If starting blank enabled, insert a newline, etc., but only if
    ;; this comment requires multiple lines.
    (if c-comment-starting-blank
	(save-excursion
	  (setq opoint (point))
	  (forward-line -1)
	  (if (or (null (search-forward "/*" opoint t))
		  (null (search-forward "*/" opoint t)))
	      (progn
		(search-backward "/*")
		(re-search-forward comment-start-skip opoint t)
		(setq fill-prefix (get-current-fill (point)))
		(if (not (looking-at "\n"))
		    (insert ?\n fill-prefix))))))
;		    (indent-new-comment-line))))))

    ;; Move cursor to indentation.
    (c-indent-line)
    (use-local-map c-mode-map)
    )
  )


;;;
;;; set-fill-and-fill - Get the current fill for this line and fill
;;; 			the paragraph.
;;; 
(defun set-fill-and-fill (arg)
  "Get the fill-prefix and fill the current paragraph."

  (interactive "P")
  (setq fill-prefix (get-current-fill (point)))
  (fill-paragraph arg))

;;;
;;; set-fill-and-return - Set the current fill prefix and
;;; 			  indent-new-comment-line.
;;; 
(defun set-fill-and-return ()
  "Set the current fill prefix and move to the next line."

  (interactive)
  (if c-comment-indenting
      (setq fill-prefix (get-current-fill (point))))
  (insert ?\n fill-prefix))

;;;
;;; do-indented-auto-fill - Perform the auto-fill function, but get
;;; 			    the fill-prefix first.
;;; 
(defun do-indented-auto-fill ()
  "Perform auto-fill, but get fill-prefix first."

  (let ((opoint (point)))
    (save-excursion
      (move-to-column (1+ fill-column))
      (skip-chars-backward "^ \t\n")
      (if (bolp)
	  (re-search-forward "[ \t]" opoint t))
      ;; If there is a space on the line before fill-point,
      ;; and nonspaces precede it, break the line there.
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))

	  ;; If we are wrapping to a new line, figure out the indentation on
	  ;; the current line first.
	  (progn
	    (setq fill-prefix (get-current-fill opoint))
	    (insert ?\n fill-prefix)))))
;	    (indent-new-comment-line)))))
  )


;;;
;;; get-current-fill - Get the fill-prefix for the current line.  This
;;; 		       assumes that the valid fill prefix is between
;;; 		       (beginning-of-line) and (point).
;;; 
(defun get-current-fill (pnt)
  "Get the current fill prefix.
A valid fill prefix must be between the beginning of the line and point."

  (let ((opoint pnt) fill last-char)
    (save-excursion
      (beginning-of-line)
      (setq fill
	    (buffer-substring (point)
			      (progn
				(re-search-forward comment-start-skip opoint t)
				(point))))

      ;; Be sure there is trailing white space.
      (setq last-char (substring fill (1- (length fill)) (length fill)))
      (if (and (not (string= " " last-char))
	       (not (string= "	" last-char)))
	  (setq fill (concat fill " ")))

      (setq fill (replace-letter fill "/" " "))

      ;; Get the hanging indentation if we haven't already.
      (if (and c-comment-hanging-indent (not c-hang-already-done))
	  (let ((curr (point))
		(opnt (progn (end-of-line) (point))))
	    (beginning-of-line)
	    (if (search-forward " - " opnt t)
		(progn
		  (setq fill (concat fill (make-string (- (point) curr) 32)))
		  (setq c-hang-already-done t)))))

      ;; Set the paragraph delimiters.
      (setq paragraph-start (concat paragraph-start-ref
				    "\\|^"
				    (regexp-quote
				     (substring fill
						0 (1- (length fill))))
				    "$"))
      (setq paragraph-separate paragraph-start))
    fill)
  )
  

;;;
;;; replace-letter - Given a string, an old letter and a new letter,
;;; 		     perform the substitution.
;;; 
(defun replace-letter (str old-letter new-letter)
  (let (new-str c
	(sp 0)
	(size (length str)))
    (while (< sp size)
      (setq c (substring str sp (1+ sp)))
      (setq new-str (concat new-str (if (string= c old-letter) new-letter c)))
      (setq sp (1+ sp)))
    new-str))
