; Modula-2 editing support package
; Author Mick Jordan
; amended Peter Robinson
; ported to GNU Michael Schmidt
;;;From: "Michael Schmidt" <michael@pbinfo.UUCP>
;;;Modified by Tom Perrine <Perrin@LOGICON.ARPA> (TEP)


;;; Added by TEP
(defvar m2-mode-syntax-table nil
  "Syntax table in use in Modula-2-mode buffers.")

(defvar m2-compile-command "m2c"
  "Command to compile Modula-2 programs")

(defvar m2-link-command "m2l"
  "Command to link Modula-2 programs")

(defvar m2-link-name nil
  "Name of the executable.")


(if m2-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( ". 1" table)
    (modify-syntax-entry ?\) ". 4" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq m2-mode-syntax-table table)))

;;; Added by TEP
(defvar m2-mode-map nil
  "Keymap used in Modula-2 mode.")

(if m2-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\^i" 'm2-tab)
    (define-key map "\C-cb" 'm2-begin)
    (define-key map "\C-cc" 'm2-case)
    (define-key map "\C-cd" 'm2-definition)
    (define-key map "\C-ce" 'm2-else)
    (define-key map "\C-cf" 'm2-for)
    (define-key map "\C-ch" 'm2-header)
    (define-key map "\C-ci" 'm2-if)
    (define-key map "\C-cm" 'm2-module)
    (define-key map "\C-cl" 'm2-loop)
    (define-key map "\C-co" 'm2-or)
    (define-key map "\C-cp" 'm2-procedure)
    (define-key map "\C-c\C-w" 'm2-with)
    (define-key map "\C-cr" 'm2-record)
    (define-key map "\C-cs" 'm2-stdio)
    (define-key map "\C-ct" 'm2-type)
    (define-key map "\C-cu" 'm2-until)
    (define-key map "\C-cv" 'm2-var)
    (define-key map "\C-cw" 'm2-while)
    (define-key map "\C-cx" 'm2-export)
    (define-key map "\C-cy" 'm2-import)
    (define-key map "\C-c{" 'm2-begin-comment)
    (define-key map "\C-c}" 'm2-end-comment)
    (define-key map "\C-c\C-z" 'suspend-emacs)
    (define-key map "\C-c\C-v" 'm2-visit)
    (define-key map "\C-c\C-t" 'm2-toggle)
    (define-key map "\C-c\C-l" 'm2-link)
    (define-key map "\C-c\C-c" 'm2-compile)
    (setq m2-mode-map map)))

(defvar m2-indent 5 "*This variable gives the indentation in Modula-2-Mode")
  
(defun modula-2-mode ()
"This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing
Control-C followed by the first character of the construct.
\\{m2-mode-map}
  Control-c b begin         Control-c c case
  Control-c d definition    Control-c e else
  Control-c f for           Control-c h header
  Control-c i if            Control-c m module
  Control-c l loop          Control-c o or
  Control-c p procedure     Control-c Control-w with
  Control-c r record        Control-c s stdio
  Control-c t type          Control-c u until
  Control-c v var           Control-c w while
  Control-c x export        Control-c y import
  Control-c { begin-comment Control-c } end-comment
  Control-c Control-z suspend-emacs     Control-c Control-t toggle
  Control-c Control-c compile           Control-x ` next-error
  Control-c Control-l link

   m2-indent controls the number of spaces for each indentation.
   m2-compile-command holds the command to compile a Modula-2 program.
   m2-link-command holds the command to link a Modula-2 program."
  (interactive)
  (kill-all-local-variables)
  (use-local-map m2-mode-map)
  (setq major-mode 'modula-2-mode)
  (setq mode-name "Modula-2")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 75)
  (set-syntax-table m2-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'm2-mode-hook))

(defun m2-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (let ((hpos (current-indentation)))
    (newline)
    (indent-to hpos)))

(defun m2-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) m2-indent)) m2-indent)))

(defun m2-begin ()
  "Insert a BEGIN keyword and indent for the next line."
  (interactive)
  (insert "BEGIN")
  (m2-newline)
  (m2-tab))

(defun m2-case ()
  "Build skeleton CASE statment, prompting for the <expression>."
  (interactive)
  (insert "CASE " (read-string ": ") " OF")
  (m2-newline)
  (m2-newline)
  (insert "END (* case *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-definition ()
  "Build skeleton DEFINITION MODULE, prompting for the <module name>."
  (interactive)
  (insert "DEFINITION MODULE ")
  (let ((name (read-string "Name: ")))
    (insert name ";\n\n\n\nEND " name ".\n"))
  (previous-line 3))

(defun m2-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (m2-newline)
  (backward-delete-char-untabify m2-indent ())
  (insert "ELSE")
  (m2-newline)
  (m2-tab))

(defun m2-for ()
  "Build skeleton FOR loop statment, prompting for the loop parameters."
  (interactive)
  (insert "FOR " (read-string "init: ") " TO " (read-string "end: "))
  (let ((by (read-string "by: ")))
    (if (not (string-equal by ""))
	(insert " BY " by)))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END (* for *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "(*\n    Title: \t")
  (insert (read-string "Title: "))
  (insert "\n    Created:\t")
  (insert (current-time-string))
  (insert "\n    Author: \t")
  (insert (user-full-name))
  (insert (concat "\n\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "*)\n\n"))

(defun m2-if ()
  "Insert skeleton IF statment, prompting for <boolean-expression>."
  (interactive)
  (insert "IF " (read-string "<boolean-expression>: ") " THEN")
  (m2-newline)
  (m2-newline)
  (insert "END (* if *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-loop ()
  "Build skeleton LOOP (with END)."
  (interactive)
  (insert "LOOP")
  (m2-newline)
  (m2-newline)
  (insert "END (* loop *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-module ()
  "Build skeleton IMPLEMENTATION MODULE, prompting for <module-name>."
  (interactive)
  (insert "IMPLEMENTATION MODULE ")
  (let ((name (read-string "Name: ")))
    (insert name ";\n\n\n\nEND " name ".\n"))
  (previous-line 3))

(defun m2-or ()
  (interactive)
  (m2-newline)
  (backward-delete-char-untabify m2-indent)
  (insert "|")
  (m2-newline)
  (m2-tab))

(defun m2-procedure ()
  (interactive)
  (insert "PROCEDURE ")
  (let ((name (read-string "Name: " ))
	args)
    (insert name " (")
    (insert (read-string "Arguments: ") ")")
    (setq args (read-string "Result Type: "))
    (if (not (string-equal args ""))
	(insert " : " args))
    (insert ";")
    (m2-newline)
    (insert "BEGIN")
    (m2-newline)
    (m2-newline)
    (insert "END ")
    (insert name)
    (insert ";")
    (end-of-line 0)
    (m2-tab)))

(defun m2-with ()
  (interactive)
  (insert "WITH ")
  (insert (read-string ": "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END (* with *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-record ()
  (interactive)
  (insert "RECORD")
  (m2-newline)
  (m2-newline)
  (insert "END (* record *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-stdio ()
  (interactive)
  (insert "
>FROM TextIO IMPORT 
   WriteCHAR, ReadCHAR, WriteINTEGER, ReadINTEGER,
   WriteCARDINAL, ReadCARDINAL, WriteBOOLEAN, ReadBOOLEAN,
   WriteREAL, ReadREAL, WriteBITSET, ReadBITSET,
   WriteBasedCARDINAL, ReadBasedCARDINAL, WriteChars, ReadChars,
   WriteString, ReadString, WhiteSpace, EndOfLine;

>FROM SysStreams IMPORT sysIn, sysOut, sysErr;

"))

(defun m2-type ()
  (interactive)
  (insert "TYPE")
  (m2-newline)
  (m2-tab))

(defun m2-until ()
  (interactive)
  (insert "REPEAT")
  (m2-newline)
  (m2-newline)
  (insert "UNTIL ")
  (insert (read-string ": ") ";")
  (end-of-line 0)
  (m2-tab))

(defun m2-var ()
  (interactive)
  (m2-newline)
  (insert "VAR")
  (m2-newline)
  (m2-tab))

(defun m2-while ()
  (interactive)
  (insert "WHILE ")
  (insert (read-string ": "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END (* while *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-export ()
  (interactive)
  (insert "EXPORT QUALIFIED "))

(defun m2-import ()
  (interactive)
  (insert "FROM ")
  (insert (read-string "Module: "))
  (insert " IMPORT "))

(defun m2-begin-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "(*  "))

(defun m2-end-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column))
  (insert "*)"))

(defun m2-compile ()
  (interactive)
  (setq modulename (buffer-name))
  (compile (concat m2-compile-command " " modulename)))

(defun m2-link ()
  (interactive)
  (setq modulename (buffer-name))
  (if m2-link-name
      (compile (concat m2-link-command " " m2-link-name))
    (compile (concat m2-link-command " "
		     (setq m2-link-name (read-string "Name of executable: "
						     modulename))))))

(defun execute-monitor-command (command)
  (let* ((shell shell-file-name)
	 (csh (equal (file-name-nondirectory shell) "csh")))
    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun m2-visit ()
  (interactive)
  (let ((deffile nil)
	(modfile nil)
	modulename)
    (save-excursion
      (setq modulename
	    (read-string "Module name: "))
      (switch-to-buffer "*Command Execution*")
      (execute-monitor-command (concat "m2whereis " modulename))
      (goto-char (point-min))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.def\\) *$")
		 (setq deffile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.mod\\) *$")
		 (setq modfile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (if (not (or deffile modfile))
	  (error "I can find neither definition nor implementation of %s"
		 modulename)))
    (cond (deffile
	    (find-file deffile)
	    (if modfile
		(save-excursion
		  (find-file modfile))))
	  (modfile
	   (find-file modfile)))))

(defun m2-toggle ()
  "Toggle between .mod and .def files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -4) ".def")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4) ".mod")))
	((string-equal (substring (buffer-name) -4) ".mod")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4)  ".def")))
	((string-equal (substring (buffer-name) -3) ".mi")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".md")))
	((string-equal (substring (buffer-name) -3) ".md")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".mi")))))
