;;; Simple BibTeX mode for GNU Emacs
;;; Bengt Martensson 87-06-28
;;; changes by Marc Shapiro shapiro@inria.inria.fr 15-oct-1986
;;;  (align long lines nicely; C-c C-o checks for the "OPT" string;
;;;   TAB goes to the end of the string; use lower case; use
;;;   run-hooks)
;;; Marc Shapiro 19-oct-1987
;;;  add X window menu option; bug fixes. TAB, LFD, C-c " and C-c C-o now
;;;  behave consistently; deletion never occurs blindly.
;;; Marc Shapiro 3-nov-87
;;;  addition for France: DEAthesis
;;; Skip Montanaro <steinmetz!sprite!montanaro> 7-dec-87, Shapiro 10-dec-87
;;;  before inserting an entry, make sure we are outside of a bib entry
;;; Marc Shapiro 14-dec-87
;;;  Cosmetic fixes.  Fixed small bug in bibtex-move-outside-of-entry.

;;; NOTE by Marc Shapiro, 14-dec-87:
;;; (bibtex-x-environment) binds an X menu for bibtex mode to x-button-c-right.
;;; Trouble is, in Emacs 18.44 you can't have a mode-specific mouse binding,
;;; so it will remain active in all windows.  Yuck!

;;; Bengt Martensson 88-05-06:
;;; Added Sun menu support.  Locally bound to right mouse button in 
;;; bibtex-mode.  Emacs 18.49 allows local mouse bindings!!
;;; Commented out vtxxx-keys and DEAthesis.  Changed documentation slightly.

(defvar bibtex-mode-syntax-table nil "")
(defvar bibtex-mode-abbrev-table nil "")
(define-abbrev-table 'bibtex-mode-abbrev-table ())
(defvar bibtex-mode-map (make-sparse-keymap) "")

(defun bibtex-mode () 
  "Major mode for editing bibtex files.  Commands:
\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX
book entry.

The optional fields are preceded by ""OPT"", thus ignored by BibTeX.
Use \\[bibtex-remove-opt] to remove ""OPT"" on the current line.

Use \\[bibtex-find-it] to position the dot at the end of the string on the same line.
Use \\[bibtex-next-position] to move to the next position to fill in.  Use \\[kill-current-line]
to kill the whole line.

M-x bibtex-x-environment binds a mode-specific X menu to control+right
mouse button.
M-x bibtex-sun-environment binds a mode-specific Sun menu to right
mouse button.

Fields:
    address
           Publisher's address
    annote
           Long annotation used for annotated bibliographies (begins sentence)
    author
           Name(s) of author(s), in BibTeX name format
    booktitle
           Book title when the thing being referenced isn't the whole book.
           For book entries, the title field should be used instead.
    chapter
           Chapter number
    edition
           Edition of a book (e.g., ""second"")
    editor
           Name(s) of editor(s), in BibTeX name format.
           If there is also an author field, then the editor field should be
           for the book or collection that the work appears in
    howpublished
            How something strange has been published (begins sentence)
    institution
           Sponsoring institution
    journal
           Journal name (macros are provided for many)
    key
           Alphabetizing and labeling key (needed when no author or editor)
    month
           Month (macros are provided)
    note
           To help the reader find a reference (begins sentence)
    number
           Number of a journal or technical report
    organization
           Organization (sponsoring a conference)
    pages
           Page number or numbers (use `--' to separate a range)
    publisher
           Publisher name
    school
           School name (for theses)
    series
           The name of a series or set of books.
           An individual book will will also have it's own title
    title
           The title of the thing being referenced
    type
           Type of a Techreport (e.g., ""Research Note"") to be used instead of
           the default ""Technical Report""
    volume
           Volume of a journal or multivolume work
    year
           Year---should contain only numerals
---------------------------------------------------------
Entry to this mode calls the value of bibtex-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if (not bibtex-mode-syntax-table)
      (setq bibtex-mode-syntax-table (copy-syntax-table)))
  (set-syntax-table bibtex-mode-syntax-table)
  (modify-syntax-entry ?\$ "$$  ")
  (modify-syntax-entry ?\% "<   ")
  (modify-syntax-entry ?\f ">   ")
  (modify-syntax-entry ?\n ">   ")
  (modify-syntax-entry ?'  "w   ")
  (modify-syntax-entry ?@  "w   ")
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)


  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq local-abbrev-table bibtex-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \f\n\t]*$")

  (define-key bibtex-mode-map "\t" 'bibtex-find-it)
  (define-key bibtex-mode-map "\n" 'bibtex-next-position)
  ;;(define-key bibtex-mode-map "\e[25~" 'bibtex-next-position)
  (define-key bibtex-mode-map "\C-c\"" 'bibtex-remove-double-quotes)
  ;;(define-key bibtex-mode-map "\C-c\eOS" 'kill-current-line)
  (define-key bibtex-mode-map "\C-c\C-k" 'kill-current-line)
  (define-key bibtex-mode-map "\C-c\C-a" 'bibtex-Article)
  (define-key bibtex-mode-map "\C-c\C-b" 'bibtex-Book)
  ;;(define-key bibtex-mode-map "\C-c\C-d" 'bibtex-DEAthesis)
  (define-key bibtex-mode-map "\C-c\C-c" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-i" 'bibtex-InBook)
  (define-key bibtex-mode-map "\C-ci" 'bibtex-InCollection)
  (define-key bibtex-mode-map "\C-cI" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-m" 'bibtex-Manual)
  (define-key bibtex-mode-map "\C-cm" 'bibtex-MastersThesis)
  (define-key bibtex-mode-map "\C-cM" 'bibtex-Misc)
  (define-key bibtex-mode-map "\C-c\C-o" 'bibtex-remove-opt)
  (define-key bibtex-mode-map "\C-c\C-p" 'bibtex-PhdThesis)
  (define-key bibtex-mode-map "\C-cp" 'bibtex-Proceedings)
  (define-key bibtex-mode-map "\C-c\C-t" 'bibtex-TechReport)
  (define-key bibtex-mode-map "\C-c\C-s" 'bibtex-string)
  (define-key bibtex-mode-map "\C-c\C-u" 'bibtex-Unpublished)
  (define-key bibtex-mode-map "\C-c?" 'describe-mode)

					; nice alignements
  (auto-fill-mode 1)
  (setq left-margin 17)

  (run-hooks 'bibtex-mode-hook))

(defun bibtex-move-outside-of-entry ()
   "Make sure we are outside of a bib entry"
   (if (or
	(= (point) (point-max))
	(= (point) (point-min))
	(looking-at "[ \n]*@")
	)
	t
	(progn
	  (backward-paragraph)
	  (forward-paragraph)))
   (re-search-forward "[ \t\n]*" (point-max) t))

(defun bibtex-entry (entry-type required optional)
  (bibtex-move-outside-of-entry)
  (insert (concat "@" entry-type "{,\n\n}\n\n"))
  (previous-line 3)
  (insert (mapconcat 'bibtex-make-entry required ",\n"))
  (if required (insert ",\n"))
  (insert (mapconcat 'bibtex-make-opt-entry optional ",\n"))
  (up-list -1)
  (forward-char 1))

(defun bibtex-make-entry (str)
  (interactive "s")
  (concat "  " str " = \t"""""))

(defun bibtex-make-opt-entry (str)
  (interactive "s")
  (concat "  OPT" str " = \t"""""))
  
(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article" '("author" "title" "journal" "year")
		'("volume" "number" "pages" "month" "note")))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book" '("author" "title" "publisher" "year")
		'("editor" "volume" "series" "address"
			   "edition" "month" "note")))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet" '("title")
		'("author" "howpublished" "address" "month" "year" "note")))

;;; France: Dipl\^{o}me d'Etudes Approfondies (similar to Master's)
;(defun bibtex-DEAthesis ()
;  (interactive)
;  (bibtex-entry "DEAthesis" '("author" "title" "school" "year")
;		'("address" "month" "note")))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook" '("author" "title" "chapter" "publisher" "year")
		'("editor" "pages" "volume" "series" "address"
			   "edition" "month" "note")))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection" '("author" "title" "booktitle"
					  "publisher" "year")
		'("editor" "chapter" "pages" "address" "month" "note")))


(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings" '("author" "title" "booktitle" "year")
		'("editor" "pages" "organization" "publisher"
			   "address" "month" "note")))

(defun bibtex-Manual ()
  (interactive)
  (bibtex-entry "Manual" '("title")
		'("author" "organization" "address" "edition" "year"
			   "month" "note")))

(defun bibtex-MastersThesis ()
  (interactive)
  (bibtex-entry "MastersThesis" '("author" "title" "school" "year")
		'("address" "month" "note")))

(defun bibtex-Misc ()
  (interactive)
  (bibtex-entry "Misc" '()
		'("author" "title" "howpublished" "year" "month" "note")))

(defun bibtex-PhdThesis ()
  (interactive)
  (bibtex-entry "PhDThesis" '("author" "title" "school" "year")
		'("address" "month" "note")))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings" '("title" "year")
		'("editor" "publisher" "organization"
			   "address" "month" "note")))
(defun bibtex-TechReport ()
  (interactive)
  (bibtex-entry "TechReport" '("author" "title" "institution" "year")
		'("type" "number" "address" "month" "note")))


(defun bibtex-Unpublished ()
  (interactive)
  (bibtex-entry "Unpublished" '("author" "title" "note")
		'("year" "month")))

(defun bibtex-string ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert "@string{ = """"}\n")
  (previous-line 1)
  (forward-char 8))

(defun bibtex-next-position ()
  "Finds next position to write in."
  (interactive)
  (forward-line 1)
  (bibtex-find-it))

(defun bibtex-find-it ()
  (interactive)
  "Find position on current line (if possible) to add entry text."
  (beginning-of-line)
  (let ((beg (point)))
    (end-of-line)
    (search-backward "," beg t)
	  (backward-char 1)
	  (if (looking-at """")
	      t
	      (forward-char 1))
    ))

(defun bibtex-remove-opt ()
  "Removes the 'OPT' starting optional arguments."
  (interactive)
  (beginning-of-line)
  (forward-char 2)
  (if (looking-at "OPT")
      (delete-char 3))
  (bibtex-find-it))

(defun kill-current-line ()
  "Kills the current line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun bibtex-remove-double-quotes ()
  "Removes """" around string."
  (interactive)
  (bibtex-find-it)
  (let
      ((here (point))
       (eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (search-forward """" eol t)
	(progn
	  (delete-char -1)
	  (if (search-forward """" eol t)
	      (delete-char -1)
	    ))
      (goto-char here))
    )
  )


;;; X window menus for bibtex mode

(defun bibtex-x-help (arg)
  "Mouse commands for BibTeX mode"

  (let ((selection
	 (x-popup-menu
	  arg
	  '("BibTeX commands"
	    ("Document types"
	     ("article in Conference Proceedings" . bibtex-InProceedings)
	     ("article in journal" . bibtex-Article)
	     ("Book" . bibtex-Book)
	     ("Booklet" . bibtex-Booklet)
	     ("Master's Thesis" . bibtex-MastersThesis)
	     ;;("DEA Thesis" . bibtex-DEAthesis)
	     ("PhD. Thesis" . bibtex-PhdThesis)
	     ("Technical Report" . bibtex-TechReport)
	     ("technical Manual" . bibtex-Manual)
	     ("Conference Proceedings" . bibtex-Proceedings)
	     ("in a Book" . bibtex-InBook)
	     ("in a Collection" . bibtex-InCollection)
	     ("miscellaneous" . bibtex-Misc)
	     ("unpublished" . bibtex-Unpublished)
	     )
	    ("others"
	     ("next field" . bibtex-next-position)
	     ("to end of field" . bibtex-find-it)
	     ("remove OPT" . bibtex-remove-opt)
	     ("remove quotes" . bibtex-remove-double-quotes)
	     ("remove this line" . kill-current-line)
	     ("describe BibTeX mode" . describe-mode)
	     ("string" . bibtex-string))))))
    (and selection (call-interactively selection))))

(defun bibtex-x-environment ()
  "Set up X menus for BibTeX mode.  Call it as bibtex-mode-hook, or interactively"
  (interactive)
  (require 'x-mouse)
  (define-key mouse-map x-button-c-right 'bibtex-x-help)
  )

;; Please don't send anything to bug-gnu-emacs about these Sunwindows functions
;; since we aren't interested.  See etc/SUN-SUPPORT for the reasons why
;; we consider this nothing but a distraction from our work.

(defmenu bibtex-sun-entry-menu 
  ("Article In Conf. Proc."
   (lambda () (eval-in-window *menu-window* (bibtex-InProceedings))))
  ("Article In Journal"
   (lambda () (eval-in-window *menu-window* (bibtex-Article))))
  ("Book"
   (lambda () (eval-in-window *menu-window* (bibtex-Book))))
  ("Booklet"
   (lambda () (eval-in-window *menu-window* (bibtex-Booklet))))
  ("Master's Thesis"
   (lambda () (eval-in-window *menu-window* (bibtex-MastersThesis))))
  ;;("DEA Thesis" bibtex-DEAthesis)
  ("PhD. Thesis"
   (lambda () (eval-in-window *menu-window* (bibtex-PhdThesis))))
  ("Technical Report"
   (lambda () (eval-in-window *menu-window* (bibtex-TechReport))))
  ("Technical Manual"
   (lambda () (eval-in-window *menu-window* (bibtex-Manual))))
  ("Conference Proceedings"
   (lambda () (eval-in-window *menu-window* (bibtex-Proceedings))))
  ("In A Book"
   (lambda () (eval-in-window *menu-window* (bibtex-InBook))))
  ("In A Collection"
   (lambda () (eval-in-window *menu-window* (bibtex-InCollection))))
  ("Miscellaneous"
   (lambda () (eval-in-window *menu-window* (bibtex-Misc))))
  ("Unpublished"
   (lambda () (eval-in-window *menu-window* (bibtex-Unpublished)))))

(defmenu bibtex-sun-menu
  ("BibTeX menu")
  ("add entry" . bibtex-sun-entry-menu)
  ("add string"
   (lambda () (eval-in-window *menu-window* (bibtex-string))))
  ;("next field" bibtex-next-position)
  ;("to end of field" bibtex-find-it)
;  ("remove OPT"
;   (lambda () (eval-in-window *menu-window* (bibtex-remove-opt))))
;  ("remove quotes"
;   (lambda () (eval-in-window *menu-window* (bibtex-remove-double-quotes))))
;  ("remove this line"
;   (lambda () (eval-in-window *menu-window* (kill-current-line))))
  ("describe BibTeX mode"
   (lambda () (eval-in-window *menu-window* (describe-mode))))
  ("Main Emacs menu" . emacs-menu))
 
(defun bibtex-sun-menu-eval (window x y)
  "Pop-up menu of BibTeX commands."
  (sun-menu-evaluate window (1+ x) (1- y) 'bibtex-sun-menu))

(defun bibtex-sun-environment ()
  "Set up sun menus for BibTeX mode.  Call it as bibtex-mode-hook, or interactively"
  (interactive)
  (local-set-mouse  '(text right) 'bibtex-sun-menu-eval))

