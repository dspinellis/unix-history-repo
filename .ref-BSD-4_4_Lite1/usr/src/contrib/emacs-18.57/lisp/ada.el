; Ada editing support package in GNUlisp.  v1.0
; Author: Vincent Broman <broman@bugs.nosc.mil>  May 1987.
; (borrows heavily from Mick Jordan's Modula-2 package for GNU,
; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)


(setq auto-mode-alist (cons (cons "\\.ada$" 'ada-mode) auto-mode-alist))

(defvar ada-mode-syntax-table nil
  "Syntax table in use in Ada-mode buffers.")

(let ((table (make-syntax-table)))
  (modify-syntax-entry ?_ "_" table)
  (modify-syntax-entry ?\# "_" table)
  (modify-syntax-entry ?\( "()" table)
  (modify-syntax-entry ?\) ")(" table)
  (modify-syntax-entry ?$ "." table)
  (modify-syntax-entry ?* "." table)
  (modify-syntax-entry ?/ "." table)
  (modify-syntax-entry ?+ "." table)
  (modify-syntax-entry ?- "." table)
  (modify-syntax-entry ?= "." table)
  (modify-syntax-entry ?\& "." table)
  (modify-syntax-entry ?\| "." table)
  (modify-syntax-entry ?< "." table)
  (modify-syntax-entry ?> "." table)
  (modify-syntax-entry ?\[ "." table)
  (modify-syntax-entry ?\] "." table)
  (modify-syntax-entry ?\{ "." table)
  (modify-syntax-entry ?\} "." table)
  (modify-syntax-entry ?. "." table)
  (modify-syntax-entry ?\\ "." table)
  (modify-syntax-entry ?: "." table)
  (modify-syntax-entry ?\; "." table)
  (modify-syntax-entry ?\' "." table)
  (modify-syntax-entry ?\" "\"" table)
  (setq ada-mode-syntax-table table))

(defvar ada-mode-map nil
  "Keymap used in Ada mode.")

(let ((map (make-sparse-keymap)))
  (define-key map "\C-m" 'ada-newline)
  (define-key map "\C-?" 'backward-delete-char-untabify)
  (define-key map "\C-i" 'ada-tab)
  (define-key map "\C-c\C-i" 'ada-untab)
  (define-key map "\C-c<" 'ada-backward-to-same-indent)
  (define-key map "\C-c>" 'ada-forward-to-same-indent)
  (define-key map "\C-ch" 'ada-header)
  (define-key map "\C-c(" 'ada-paired-parens)
  (define-key map "\C-c-" 'ada-inline-comment)
  (define-key map "\C-c\C-a" 'ada-array)
  (define-key map "\C-cb" 'ada-exception-block)
  (define-key map "\C-cd" 'ada-declare-block)
  (define-key map "\C-c\C-e" 'ada-exception)
  (define-key map "\C-cc" 'ada-case)
  (define-key map "\C-c\C-k" 'ada-package-spec)
  (define-key map "\C-ck" 'ada-package-body)
  (define-key map "\C-c\C-p" 'ada-procedure-spec)
  (define-key map "\C-cp" 'ada-subprogram-body)
  (define-key map "\C-c\C-f" 'ada-function-spec)
  (define-key map "\C-cf" 'ada-for-loop)
  (define-key map "\C-cl" 'ada-loop)
  (define-key map "\C-ci" 'ada-if)
  (define-key map "\C-cI" 'ada-elsif)
  (define-key map "\C-ce" 'ada-else)
  (define-key map "\C-c\C-v" 'ada-private)
  (define-key map "\C-c\C-r" 'ada-record)
  (define-key map "\C-c\C-s" 'ada-subtype)
  (define-key map "\C-cs" 'ada-separate)
  (define-key map "\C-c\C-t" 'ada-type)
  (define-key map "\C-ct" 'ada-tabsize)
;;    (define-key map "\C-c\C-u" 'ada-use)
;;    (define-key map "\C-c\C-w" 'ada-with)
  (define-key map "\C-cw" 'ada-while-loop)
  (define-key map "\C-c\C-w" 'ada-when)
  (define-key map "\C-cx" 'ada-exit)
  (define-key map "\C-cC" 'ada-compile)
  (define-key map "\C-cB" 'ada-bind)
  (define-key map "\C-cE" 'ada-find-listing)
  (define-key map "\C-cL" 'ada-library-name)
  (define-key map "\C-cO" 'ada-options-for-bind)
  (setq ada-mode-map map))

(defvar ada-indent 4 "*Value is the number of columns to indent in Ada-Mode.")
  
(defun ada-mode ()
"This is a mode intended to support program development in Ada.
Most control constructs and declarations of Ada can be inserted in the buffer
by typing Control-C followed by a character mnemonic for the construct.

C-c C-a  array         	C-c b    exception block
C-c C-e  exception      C-c d    declare block
C-c C-k  package spec   C-c k    package body
C-c C-p  procedure spec C-c p    proc/func body
C-c C-f  func spec      C-c f    for loop
                        C-c i    if
                        C-c I    elsif
                        C-c e    else
C-c C-v  private        C-c l    loop
C-c C-r  record         C-c c    case
C-c C-s  subtype        C-c s    separate
C-c C-t  type           C-c t    tab spacing for indents
C-c C-w  when           C-c w    while
                        C-c x    exit
C-c (    paired parens  C-c -    inline comment
                        C-c h    header sec
C-c C    compile        C-c B    bind
C-c E    find error list
C-c L    name library   C-c O    options for bind

C-c < and C-c > move backward and forward respectively to the next line
having the same (or lesser) level of indentation.

Variable ada-indent controls the number of spaces for indent/undent.

\\{ada-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ada-mode-map)
  (setq major-mode 'ada-mode)
  (setq mode-name "Ada")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 72)
  (set-syntax-table ada-mode-syntax-table)
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
  (setq comment-start "--")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'ada-mode-hook))

(defun ada-tabsize (s)
  "changes spacing used for indentation. Reads spacing from minibuffer."
  (interactive "nnew indentation spacing: ")
  (setq ada-indent s))

(defun ada-newline ()
  "Start new line and indent to current tab stop."
  (interactive)
  (let ((ada-cc (current-indentation)))
    (newline)
    (indent-to ada-cc)))

(defun ada-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) ada-indent)) ada-indent)))

(defun ada-untab ()
  "Delete backwards to previous tab stop."
  (interactive)
  (backward-delete-char-untabify ada-indent nil))

(defun ada-go-to-this-indent (step indent-level)
  "Move point repeatedly by <step> lines till the current line
has given indent-level or less, or the start/end of the buffer is hit.
Ignore blank lines, statement labels, block/loop names."
  (while (and
	  (zerop (forward-line step))
	  (or (looking-at "^[ 	]*$")
	      (looking-at "^[ 	]*--")
	      (looking-at "^<<[A-Za-z0-9_]+>>")
	      (looking-at "^[A-Za-z0-9_]+:")
	      (> (current-indentation) indent-level)))
    nil))

(defun ada-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer."
  (interactive)
  (ada-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun ada-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer."
  (interactive)
  (ada-go-to-this-indent 1 (current-indentation))
  (back-to-indentation))

(defun ada-array ()
  "Insert array type definition, prompting for component type,
leaving the user to type in the index subtypes."
  (interactive)
  (insert "array ()")
  (backward-char)
  (insert (read-string "index subtype[s]: "))
  (end-of-line)
  (insert " of ;")
  (backward-char)
  (insert (read-string "component-type: "))
  (end-of-line))

(defun ada-case ()
  "Build skeleton case statment, prompting for the selector expression.
starts up the first when clause, too."
  (interactive)
  (insert "case ")
  (insert (read-string "selector expression: ") " is")
  (ada-newline)
  (ada-newline)
  (insert "end case;")
  (end-of-line 0)
  (ada-tab)
  (ada-tab)
  (ada-when))

(defun ada-declare-block ()
  "Insert a block with a declare part and indent for the 1st declaration."
  (interactive)
  (let ((ada-block-name (read-string "[block name]: ")))
    (insert "declare")
    (cond
      ( (not (string-equal ada-block-name ""))
	(beginning-of-line)
	(open-line 1)
	(insert ada-block-name ":")
	(next-line 1)
	(end-of-line)))
    (ada-newline)
    (ada-newline)
    (insert "begin")
    (ada-newline)
    (ada-newline)
    (if (string-equal ada-block-name "")
      (insert "end;")
      (insert "end " ada-block-name ";"))
   )
  (end-of-line -2)
  (ada-tab))

(defun ada-exception-block ()
  "Insert a block with an exception part and indent for the 1st line of code."
  (interactive)
  (let ((block-name (read-string "[block name]: ")))
    (insert "begin")
    (cond
      ( (not (string-equal block-name ""))
	(beginning-of-line)
	(open-line 1)
	(insert block-name ":")
	(next-line 1)
	(end-of-line)))
    (ada-newline)
    (ada-newline)
    (insert "exception")
    (ada-newline)
    (ada-newline)
    (cond
      ( (string-equal block-name "")
	(insert "end;"))
      ( t
	(insert "end " block-name ";")))
   )
  (end-of-line -2)
  (ada-tab))

(defun ada-exception ()
  "Undent and insert an exception part into a block. Reindent."
  (interactive)
  (ada-untab)
  (insert "exception")
  (ada-newline)
  (ada-tab))

(defun ada-else ()
  "Add an else clause inside an if-then-end-if clause."
  (interactive)
  (ada-untab)
  (insert "else")
  (ada-newline)
  (ada-tab))

(defun ada-exit ()
  "Insert an exit statement, prompting for loop name and condition."
  (interactive)
  (insert "exit")
  (let ((ada-loop-name (read-string "[name of loop to exit]: ")))
    (if (not (string-equal ada-loop-name "")) (insert " " ada-loop-name)))
  (let ((ada-exit-condition (read-string "[exit condition]: ")))
    (if (not (string-equal ada-exit-condition ""))
	(if (string-match "^ *[Ww][Hh][Ee][Nn] +" ada-exit-condition)
	    (insert " " ada-exit-condition)
	  (insert " when " ada-exit-condition))))
  (insert ";"))

(defun ada-when ()
  "Start a case statement alternative with a when clause."
  (interactive)
  (ada-untab)     ; we were indented in code for the last alternative.
  (insert "when ")
  (insert (read-string "'|'-delimited choice list: ") " =>")
  (ada-newline)
  (ada-tab))

(defun ada-for-loop ()
  "Build a skeleton for-loop statement, prompting for the loop parameters."
  (interactive)
  (insert "for ")
  (let* ((ada-loop-name (read-string "[loop name]: "))
	 (ada-loop-is-named (not (string-equal ada-loop-name ""))))
    (if ada-loop-is-named
	(progn
	  (beginning-of-line)
	  (open-line 1)
	  (insert ada-loop-name ":")
	  (next-line 1)
	  (end-of-line 1)))
    (insert (read-string "loop variable: ") " in ")
    (insert (read-string "range: ") " loop")
    (ada-newline)
    (ada-newline)
    (insert "end loop")
    (if ada-loop-is-named (insert " " ada-loop-name))
    (insert ";"))
  (end-of-line 0)
  (ada-tab))

(defun ada-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "--\n--  Title: \t")
  (insert (read-string "Title: "))
  (insert "\n--  Created:\t" (current-time-string))
  (insert "\n--  Author: \t" (user-full-name))
  (insert "\n--\t\t<" (user-login-name) "@" (system-name) ">\n--\n"))

(defun ada-if ()
  "Insert skeleton if statment, prompting for a boolean-expression."
  (interactive)
  (insert "if ")
  (insert (read-string "condition: ") " then")
  (ada-newline)
  (ada-newline)
  (insert "end if;")
  (end-of-line 0)
  (ada-tab))

(defun ada-elsif ()
  "Add an elsif clause to an if statement, prompting for the boolean-expression."
  (interactive)
  (ada-untab)
  (insert "elsif ")
  (insert (read-string "condition: ") " then")
  (ada-newline)
  (ada-tab))

(defun ada-loop ()
  "insert a skeleton loop statement.  exit statement added by hand."
  (interactive)
  (insert "loop ")
  (let* ((ada-loop-name (read-string "[loop name]: "))
	 (ada-loop-is-named (not (string-equal ada-loop-name ""))))
    (if ada-loop-is-named
	(progn
	  (beginning-of-line)
	  (open-line 1)
	  (insert ada-loop-name ":")
	  (forward-line 1)
	  (end-of-line 1)))
    (ada-newline)
    (ada-newline)
    (insert "end loop")
    (if ada-loop-is-named (insert " " ada-loop-name))
    (insert ";"))
  (end-of-line 0)
  (ada-tab))

(defun ada-package-spec ()
  "Insert a skeleton package specification."
  (interactive)
  (insert "package ")
  (let ((ada-package-name (read-string "package name: " )))
    (insert ada-package-name " is")
    (ada-newline)
    (ada-newline)
    (insert "end " ada-package-name ";")
    (end-of-line 0)
    (ada-tab)))

(defun ada-package-body ()
  "Insert a skeleton package body --  includes a begin statement."
  (interactive)
  (insert "package body ")
  (let ((ada-package-name (read-string "package name: " )))
    (insert ada-package-name " is")
    (ada-newline)
    (ada-newline)
    (insert "begin")
    (ada-newline)
    (insert "end " ada-package-name ";")
    (end-of-line -1)
    (ada-tab)))

(defun ada-private ()
  "Undent and start a private section of a package spec. Reindent."
  (interactive)
  (ada-untab)
  (insert "private")
  (ada-newline)
  (ada-tab))

(defun ada-get-arg-list ()
  "Read from user a procedure or function argument list.
Add parens unless arguments absent, and insert into buffer.
Individual arguments are arranged vertically if entered one-at-a-time.
Arguments ending with ';' are presumed single and stacked."
  (insert " (")
  (let ((ada-arg-indent (current-column))
	(ada-args (read-string "[arguments]: ")))
    (if (string-equal ada-args "")
	(backward-delete-char 2)
      (progn
	(while (string-match ";$" ada-args)
	  (insert ada-args)
	  (newline)
	  (indent-to ada-arg-indent)
	  (setq ada-args (read-string "next argument: ")))
	(insert ada-args ")")))))

(defun ada-function-spec ()
  "Insert a function specification.  Prompts for name and arguments."
  (interactive)
  (insert "function ")
  (insert (read-string "function name: "))
  (ada-get-arg-list)
  (insert " return ")
  (insert (read-string "result type: ")))

(defun ada-procedure-spec ()
  "Insert a procedure specification, prompting for its name and arguments."
  (interactive)
  (insert "procedure ")
  (insert (read-string "procedure name: " ))
  (ada-get-arg-list))

(defun get-ada-subprogram-name ()
  "Return (without moving point or mark) a pair whose CAR is
the name of the function or procedure whose spec immediately precedes point,
and whose CDR is the column nbr the procedure/function keyword was found at."
  (save-excursion
    (let ((ada-proc-indent 0))
      (if (re-search-backward
  ;;;; Unfortunately, comments are not ignored in this string search.
	     "[PpFf][RrUu][OoNn][Cc][EeTt][DdIi][UuOo][RrNn]" nil t)
	  (if (or (looking-at "\\<[Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]\\>")
		  (looking-at "\\<[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\>"))
	      (progn
		(setq ada-proc-indent (current-column))
		(forward-word 2)
		(let ((p2 (point)))
		  (forward-word -1)
		  (cons (buffer-substring (point) p2) ada-proc-indent)))
	      (get-ada-subprogram-name))
	   (cons "NAME?" ada-proc-indent)))))

(defun ada-subprogram-body ()
  "Insert frame for subprogram body.
Invoke right after ada-function-spec or ada-procedure-spec."
  (interactive)
  (insert " is")
  (let ((ada-subprogram-name-col (get-ada-subprogram-name)))
    (newline)
    (indent-to (cdr ada-subprogram-name-col))
    (ada-newline)
    (insert "begin")
    (ada-newline)
    (ada-newline)
    (insert "end " (car ada-subprogram-name-col) ";"))
  (end-of-line -2)
  (ada-tab))

(defun ada-separate ()
  "Finish a body stub with 'is separate'."
  (interactive)
  (insert " is")
  (ada-newline)
  (ada-tab)
  (insert "separate;")
  (ada-newline)
  (ada-untab))

;(defun ada-with ()
;  "Inserts a with clause, prompting for the list of units depended upon."
;  (interactive)
;  (insert "with ")
;  (insert (read-string "list of units depended upon: ") ";"))
;
;(defun ada-use ()
;  "Inserts a use clause, prompting for the list of packages used."
;  (interactive)
;  (insert "use ")
;  (insert (read-string "list of packages to use: ") ";"))

(defun ada-record ()
  "Insert a skeleton record type declaration."
  (interactive)
  (insert "record")
  (ada-newline)
  (ada-newline)
  (insert "end record;")
  (end-of-line 0)
  (ada-tab))

(defun ada-subtype ()
  "Start insertion of a subtype declaration, prompting for the subtype name."
  (interactive)
  (insert "subtype " (read-string "subtype name: ") " is ;")
  (backward-char)
  (message "insert subtype indication."))

(defun ada-type ()
  "Start insertion of a type declaration, prompting for the type name."
  (interactive)
  (insert "type " (read-string "type name: "))
  (let ((disc-part (read-string "discriminant specs: ")))
    (if (not (string-equal disc-part ""))
	(insert "(" disc-part ")")))
  (insert " is ")
  (message "insert type definition."))

(defun ada-while-loop ()
  (interactive)
  (insert "while ")
  (let* ((ada-loop-name (read-string "loop name: "))
	 (ada-loop-is-named (not (string-equal ada-loop-name ""))))
    (if ada-loop-is-named
	(progn
	  (beginning-of-line)
	  (open-line 1)
	  (insert ada-loop-name ":")
	  (next-line 1)
	  (end-of-line 1)))
    (insert (read-string "entry condition: ") " loop")
    (ada-newline)
    (ada-newline)
    (insert "end loop")
    (if ada-loop-is-named (insert " " ada-loop-name))
    (insert ";"))
  (end-of-line 0)
  (ada-tab))

(defun ada-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun ada-inline-comment ()
  "Start a comment after the end of the line, indented at least COMMENT-COLUMN.
If starting after END-COMMENT-COLUMN, start a new line."
  (interactive)
  (end-of-line)
  (if (> (current-column) end-comment-column) (newline))
  (if (< (current-column) comment-column) (indent-to comment-column))
  (insert " -- "))

(defun ada-display-comment ()
"Inserts 3 comment lines, making a display comment."
  (interactive)
  (insert "--\n-- \n--")
  (end-of-line 0))

;; Much of this is specific to Ada-Ed

(defvar ada-lib-dir-name "lib" "*Current ada program library directory.")
(defvar ada-bind-opts "" "*Options to supply for binding.")

(defun ada-library-name (ada-lib-name)
  "Specify name of ada library directory for later compilations."
  (interactive "Dname of ada library directory: ")
  (setq ada-lib-dir-name ada-lib-name))

(defun ada-options-for-bind ()
  "Specify options, such as -m and -i, needed for adabind."
  (setq ada-bind-opts (read-string "-m and -i options for adabind: ")))

(defun ada-compile (ada-prefix-arg)
  "Save the current buffer and compile it into the current program library.
Initialize the library if a prefix arg is given."
  (interactive "P")
  (let* ((ada-init (if (null ada-prefix-arg) "" "-n "))
	(ada-source-file (buffer-name)))
    (compile
     (concat "adacomp " ada-init "-l " ada-lib-dir-name " " ada-source-file))))

(defun ada-find-listing ()
  "Find listing file for ada source in current buffer, using other window."
  (interactive)
  (find-file-other-window (concat (substring (buffer-name) 0 -4) ".lis"))
  (search-forward "*** ERROR"))

(defun ada-bind ()
  "Bind the current program library, using the current binding options."
  (interactive)
  (compile (concat "adabind " ada-bind-opts " " ada-lib-dir-name)))
