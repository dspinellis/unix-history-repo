;;			 --- Simula Mode for GNU Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.

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

;; Written by Ole Bj|rn Hessen.
;; Disclaimer: This is my first lisp program > 10 lines, and -- most of
;; all an experiment using reg-exp to represent forms on the screen.
;; The parser parses simula backward, an impossible job.
;; Well, I nearly lost!! Luckily, hhe@ifi.uio.no plan to make a better one.

(defvar simula-label "^[A-Za-z_{|}]+:")
(defvar simula-CE "else\\b\\|when\\b\\|otherwise\\b")
(defvar simula-CB "end\\b\\|!\\|comment\\b")
(defvar simula-BE "end\\b")
(defvar simula-BB "begin\\b")
(defvar simula-FB "if\\b\\|while\\b\\|inspect\\b\\|for\\b")
(defvar simula-eol "\n")
(defvar simula-eof "@")			;the form is postfixed by this string

(defvar simula-extended-form nil
  "non-nil if want non-standard slowly (extended) form checking")

(defvar simula-mode-syntax-table nil
  "Syntax table in simula-mode buffers.")

(defvar simula-mode-abbrev-table nil
  "abbrev table in simula-mode buffers")

(defvar simula-indent-mode 'simula-Nice-indent-mode)
;;most users want this feature...

(defvar Read-Simula-Keywords nil
  "non-nil if read keywords already")

(define-abbrev-table 'simula-mode-abbrev-table ())

(defvar Simula-Keyword-Abbrev-File "simula.defns"
  "nil if not to load the Capitalize Keywords feature")

(defvar simula-mode-ignore-directives t
  "Set to non nil if doesn't use % comment type lines.")

(if simula-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\n "."    table)
    (modify-syntax-entry ?\f "."    table)
    (modify-syntax-entry ?\" "\""  table)
    (modify-syntax-entry ?'  "\""   table)
    (modify-syntax-entry ?(  "()"   table)
    (modify-syntax-entry ?)  ")("   table)
    (modify-syntax-entry ?*  "."    table)
    (modify-syntax-entry ?+  "."    table)
    (modify-syntax-entry ?,  "."    table)
    (modify-syntax-entry ?-  "."    table)
    (modify-syntax-entry ?.  "_"    table)
    (modify-syntax-entry ?_  "w"    table)
    (modify-syntax-entry ?/  "."    table)
    (modify-syntax-entry ?:  "."    table)
    (modify-syntax-entry ?;  ">"    table)
    (modify-syntax-entry ?<  "."    table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?[  "(]"   table)
    (modify-syntax-entry ?\\ "."    table)
    (modify-syntax-entry ?]  ")["   table)
    (modify-syntax-entry ?^  "."    table)
    (modify-syntax-entry ?\|  "w"   table)
    (modify-syntax-entry ?\{  "w"   table)
    (modify-syntax-entry ?\}  "w"   table)
    (modify-syntax-entry ?!  "<"    table)
    (setq simula-mode-syntax-table table)))

(defvar simula-mode-map ()
  "Keymap used in simula mode.")

(if simula-mode-map
    ()
  (setq simula-mode-map (make-sparse-keymap))
  (define-key simula-mode-map "\t" 'simula-indent)
  (define-key simula-mode-map "\r" 'simula-abbrev-expand-and-lf)
  (define-key simula-mode-map "" 'backward-delete-char-untabify))


(defun simula-mode ()
  "This is a mode intended to support program development in Simula.."
  (interactive)
  (kill-all-local-variables)
  (use-local-map simula-mode-map)
  (setq major-mode 'simula-mode)
  (setq mode-name "Simula")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 75)
  (set-syntax-table simula-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t]*$\\|\\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'simula-null-indent)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)	;put a newline at end!
  (make-local-variable 'comment-start)
  (setq comment-start "! ")
  (make-local-variable 'comment-end)
  (setq comment-end " ;")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "!+ *")
  (make-local-variable 'comment-column)
  (setq comment-start-skip "! *")	;not quite right, but..
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (setq local-abbrev-table simula-mode-abbrev-table)
  ;;Capitalize-Simula-Keywords ought to run a hook!!!
  (if Simula-Keyword-Abbrev-File
      (progn
	(setq abbrev-mode t)
	(if Read-Simula-Keywords
	    ()
	  (condition-case err
	      (read-abbrev-file Simula-Keyword-Abbrev-File)
	    (file-error
	     (with-output-to-temp-buffer "*Help*"
	       (princ "Simula Mode can't load the Capitalize Simula ")
	       (princ "Keyword abbrev file\n\n")
	       (princ "Please do one of the following:\n")
	       (princ "1. Include this line in your .emacs file:\n")
	       (princ "   (setq Simula-Keyword-Abbrev-File nil)\n")
	       (princ "2. Make a decent abbrev file by your self\n")
	       (princ "3. Mail obh@ifi.uio.no requesting the abbrev file\n"))))
	  (setq Read-Simula-Keywords t))))
  (funcall simula-indent-mode)		;set indentation
  (run-hooks 'simula-mode-hook))

(defun simula-null-indent ()
  (interactive))

(setq simula-seen-FE nil)		;if seen FE during parsing; non-nil 
(setq simula-form-starter nil)		;string, the FB.
(setq simula-form nil)			;string, the assembled form
(setq simula-FB-hpos nil)		;FB's Hpos
(setq simula-BB-hpos nil)		;BB's Hpos
(setq simula-hpos nil)			;Hpos of preceeding simula form
(setq simula-lf-count nil)		;A count of lf seen during parsing
(setq simula-stack  nil)		;A stack of regions representing form
(setq simula-assemble nil)		;non-nil if assembling forms on stack
(setq simula-debug nil)			;t if debugging forms


;; some simple stack routines.
(defun simula-push (v)
  (if simula-assemble (setq simula-stack (cons v simula-stack))))

(defun simula-pop ()
  (prog1 (car simula-stack)
    (setq simula-stack (cdr simula-stack))))
;;The concepts of a stack is now obsolete...
;;Major rewrite is wanted..

(defun simula-inside-simple-string ()
  ;returns t if inside a simulask simple string
  (save-excursion
    (skip-chars-backward "^\"\n'")
    (if (bolp) nil
      (let ((count 1))
	(while (not (bolp))
	  (forward-char -1)
	  (skip-chars-backward "^\"\n'")
	  (setq count (1+ count)))
	(= (% count 2) 0)))))
	  

;;ignore line starting with a %.
;;form is evaled until line is not a compiler directive
;;way is t if going forward
;;returns with value of form
;;didn't found how to use the right kind of scoping, so shit!!!
;; -- HELP --

(defun ignore-simula-directives (pedohejform &optional pedohejway)
  (interactive)
  (if simula-mode-ignore-directives (funcall pedohejform)
    (let ((pedohejval (funcall pedohejform)) (pedohejhere (point)))
      (beginning-of-line)
      (while				;while directive line
	  (cond
	    ((not (= (following-char) ?%)) nil)
	    ((or (bobp) (eobp)) nil)	;and not beginning(end) of buffer
	    (t))
	(if pedohejway (forward-line) (forward-char -1))
	(setq pedohejval (funcall pedohejform)) ;execute form once more
	(setq pedohejhere (point))	;and goto beginning of that line.
	(beginning-of-line))
      (if (not (= (following-char) ?%)) (goto-char pedohejhere))
      pedohejval)))			;return FROM if skipped something
;Have you seen anybody prefixing a variable with my special password?
;No? Good!


;We are on a line which is _not_ a '%'-line directive,
;and inside or _just_ after a '! blabla ;' or a 'end blabla ;' comment.
;Our job is to skip that comment, returning position skipping from or
;just nil if this is no comment

(defun maybe-skip-simula-comment ()
  (let ((here (point)) last-end tmp tmp1)
    (ignore-simula-directives
      (function
	(lambda ()
	  (search-backward ";" (point-min) 0)
	  (while (simula-inside-simple-string)
	    (search-backward "\"")
	    (search-backward ";" (point-min) 0)))))
    (re-search-forward
      "^%\\|\"\\|!\\|\\bcomment\\b\\|\\bend\\b" here 0)
    (while (or (= (setq tmp (preceding-char)) ?%)
	       (= tmp ?\"))
      (if (= tmp ?\") (search-forward "\"" here 0)
	(forward-line 1)
	(if (> (point) here) (goto-char here)))
      (re-search-forward
	"^%\\|\"\\|!\\|\\bcomment\\b\\|\\bend\\b" here 0))
    (if (= here (point)) nil		;no comment between "; blabla "
      (if (= (preceding-char) ?!)
	  (progn			;a "; ! blabla " commentt
	    (forward-char -1)
	    here)			;ignore semicolon.
	(forward-word -1)
	(if (looking-at "comment")
	    here			;a "; comment blabla " string
;; this is a end-comment
	  (setq last-end (point))	;remember where end started
	  (while
	      (and			;skip directive lines
		(progn			;and strings.
		  (setq tmp1
			(re-search-forward
			  "^%\\|\"\\|!\\|\\bcomment\\b\\|\\bend\\b\\|\\bwhen\\b\\|\\belse\\b\\|\\botherwise\\b" here 0))
		  (while (and tmp1
			      (or (= (setq tmp (preceding-char)) ?%)
				  (= tmp ?\")))
		    (if (= tmp ?\") (search-forward "\"" here 0)
		      (forward-line 1))
		    (setq tmp1 (re-search-forward
				 "^%\\|\"\\|!\\|\\bcomment\\b\\|\\bend\\b\\|\\bwhen\\b\\|\\belse\\b\\|\\botherwise\\b" here 0)))
		  tmp1)
		(cond
		  ((= (preceding-char) ?!) ;a "end ! " is part of end-comment
		   (if last-end		;skip it.
		       t
		     (forward-char -1) nil)) ;seen e.g. "end else !"
					;skip back over word
		  ((progn (forward-word -1) nil))
		  ((looking-at "comment")
		   (if (not last-end) 
		       nil
		     (forward-word 1) t))
		  (t (setq last-end (if (looking-at "end") (point) nil))
		     (forward-word 1) t))))
	  (if (looking-at "!\\|\\bcomment")
	      here
	    (if last-end
		(progn (goto-char last-end) here)
	      (goto-char here)
	      nil)))))))


;;save this block form
(defun save-simula-BB-BE()
  (let ((end (point)) (beg nil))
    (simula-push end)
    (simula-back-level)			;goto before the begin at this level
    (if (not simula-BB-hpos)		;save column number if this the first
	(setq simula-BB-hpos (current-column)))
    (setq beg (point))
    (end-of-line)
    (simula-push			;save unto stack a block level.
      (concat
	"BEGIN"
	(if (> (point) end) ()
	  (setq simula-lf-count (1+ simula-lf-count))
	  simula-eol)			;there is a lf after the begin
	" o "
	(progn
	  (forward-line 2)
	  (if (> (point) end) ()
	    (setq simula-lf-count (1+ simula-lf-count))
	    simula-eol))))		;and before the end.
    (simula-push beg)
    (goto-char beg)))


    	  

;;assumes we are inside a begin blabla end sentence.
;;returns _before_ the begin
(defun simula-back-level()
  (interactive)
  (let ((end-comment))
    (while
	(and
	  (not (bobp))
	  (ignore-simula-directives
	    (function
	      (lambda ()
		(re-search-backward "\\bend\\b\\|\\bbegin\\b" (point-min) 0)
		(while (simula-inside-simple-string)
		  (search-backward "\"")
		  (re-search-backward "\\bend\\b\\|\\bbegin\\b" (point-min) 0))
		t)))
	  (if (looking-at "begin")
	      (if (maybe-skip-simula-comment) ;ignore begin in (end)comments
		  (progn (if (looking-at "end") (forward-word 1)) t)
		nil)			;else exit while.
	    (if (setq end-comment (maybe-skip-simula-comment))
		(if (looking-at "comment\\|!") t ;then not an end-comment
		  (goto-char end-comment)
		  (simula-back-level)
		  t)
	      (simula-back-level)
	      t)))))
      (if (not (looking-at "begin"))
	  (error "No matching BEGIN !!!")))
	    


;on entry cursor is on the line we should indent. It indent this line and
;predicts the next line's hpos at return value!!
(defun simula-find-indent (&optional predict-next)
  (interactive)
  (let
      ((not-stop t)			;set to nil if stop parsing, 0 at bolp
       (simexp 0)			;simexp= simula-lf-count, + simula exp.
       tmp ch				;last read character
       indent)				;hpos to indent lines line to.
    (end-of-line)
    (ignore-simula-directives		;ignore if this is a directive line
      (function (lambda () (skip-chars-backward " \t"))))
    (if (maybe-skip-simula-comment)
	(if (looking-at "end") (forward-word 1)))
    (setq simula-lf-count 0
	  simula-assemble t
	  simula-BB-hpos nil
	  simula-FB-hpos nil
	  simula-hpos nil
	  simula-seen-FE nil
	  simula-form nil
	  simula-form-starter nil	;string representing the form-starter
	  simula-stack (list (point)	;a stack of regions or strings.
			     simula-eof))
    (while not-stop
      (setq simexp (1+ simexp))		;count up simula expressions seen.
      (skip-chars-backward " \t")	;skip ignoring whitespace
      (if (bobp)
	  (setq not-stop nil)		;stop at start og buffer
	(if (= (char-syntax (setq ch (preceding-char))) ?w)
	    (forward-word -1)		;back over item (ie. word or char.)
	  (forward-char -1))
	(cond
	  ((eolp)			;passed a new-line
	   (cond
	     ((numberp not-stop)	;if zero, then stop parsing.
	      (setq not-stop nil)
	      (forward-char 1))
	     (t				;else count up lf's
	       (if (/= simula-lf-count (1- simexp))
		   (setq simula-lf-count (1+ simula-lf-count)))
	       (setq simexp simula-lf-count) ;reset simexp.
	       (simula-push (1+ (point))) ;don't assemble newlines in
	       (ignore-simula-directives ;simula-form
		 (function (lambda () (skip-chars-backward " \t\n"))))
	       (simula-push simula-eol)	;save the newline
	       (simula-push (point)))))	;ignore region skipped

	  ((= ch ?\")
	   (save-simula-string))	;skip the string

	  ((= ch ?\')
	   (forward-char -1)
	   (if (search-backward "'" (point-min) t)
	       (forward-char -1)	;skip to before '
	     (error "Unbalanced Character Quote")))

	  ((= ch ?:) (forward-word -1))
	  
	  ((= ch ?\;)				;semicolon
	   (setq tmp (maybe-skip-simula-comment))  ;is this a comment?
	   (if (and tmp (looking-at "!\\|comment"))
	       (simula-parsed-over (1+ tmp)) 	;ignore comments
	     (cond
	       ((and (> simula-lf-count 1)  ;abort parsing if FE last exp in
		     (= simula-lf-count (1- simexp)))  ;line only 
		(setq not-stop nil)	;stop parsing
		(simula-stack-trick))	;goto "next-line"
	       ((if (not tmp) nil	;do more parsing, but forget
		  (forward-word 1)	;the end-comment
		  (simula-parsed-over tmp)
		  nil))
	       ((= simexp 1) (setq simula-seen-FE t))
	       ((> simula-lf-count 0)
		(simula-push (1+ (point)))
		(setq simula-assemble nil)))))  ;assemble only the last form

	  ((looking-at simula-BB)
	   (setq simula-seen-FE nil)	;forget the past
 	   (if (> simula-lf-count 1)
	       (setq not-stop (simula-stack-trick)) ;stop here!!
	     (if (not simula-assemble)
		 (progn
		   (setq simula-stack (list (point)
					    (concat "/n o " simula-eof))
			 simula-assemble t)))
	     (if (not simula-BB-hpos)
		 (setq simula-BB-hpos (current-column)))))

	  ((and (looking-at simula-CE)
		(setq tmp (maybe-skip-simula-comment)))
	   (forward-word 1)		;skip past end.
	   (simula-parsed-over tmp))

	  ((looking-at simula-BE) (save-simula-BB-BE))

	  ((and (not indent)		;if already found, skip this FB
		(looking-at simula-FB))
	    (setq simula-form-starter
		  (buffer-substring (point) (match-end 0)))
	    (setq simula-FB-hpos (current-column))
	    (if (not (setq indent (Simula-Form-Handler)))
		(setq simula-FB-hpos nil simula-form nil))
	    (if simula-seen-FE ()	;if not seen FE, stop parsing
	      (setq not-stop nil)	;and indent from this line
	      (beginning-of-line))))))

    (setq simula-hpos (current-simula-indentation)) ;save indentation
    (if simula-form
	(if (and predict-next simula-seen-FE)
	    (setcdr indent (cdr (Simula-Default-Handler))))
      (setq indent (Simula-Default-Handler)))
    indent))


(defun simula-parsed-over (from)
  (skip-chars-backward "\t") ;skip whitespace before comment.
  (simula-push from)			;forget from
  (save-excursion
    (end-of-line)			;if passed newline don't forget 
    (if (< (point) from)		;that
	(progn
	  (simula-push simula-eol)
	  (setq simula-lf-count (1+ simula-lf-count)))))
  (simula-push (point)))		;mark region to be skipped past


;;some better names wanted.
(defun simula-stack-trick ()
  ;;axiom: if skipped back over 2-* lines, then use the indentation
  ;;of the line after the line where the BB was found. Or if skipped past
  ;;at least two lines and see ";" + newline. Use next lines indentation.
  ;;that means one must fix the stack..
  (forward-line 1)
  (ignore-simula-directives
    (function
      (lambda () (skip-chars-forward " \t\n")
	(while (= (following-char) ?\!)
	  (search-forward ";" (point-max) 0)
	  (skip-chars-forward " \t\n"))))
    t)
  (let ((pointer simula-stack))
    (while pointer
      (if (and (numberp (car pointer))
	       (> (point) (car pointer)))
	  (setq simula-stack pointer pointer nil)
	(setq pointer (cdr pointer))))) nil)
    
	    
(defun save-simula-string ()
  (simula-push (point))			;skip string contents
  (skip-chars-backward "^\"\n" (point-min))
  (if (= (preceding-char) ?\") nil
    (error "UnBalanced String Quote \". "))
  (simula-push (point))
  (forward-char -1))			;save the "" unto stack.


(defun Simula-Form-Handler ()
  (let ((handler (intern-soft
		   (concat "Simula-" (capitalize simula-form-starter)
			   "-Handler"))))
    (if handler (funcall handler) nil)))


(defun Simula-Default-Handler ()
  (prog1
      (if (and simula-seen-FE
	       (not simula-extended-form)
	       (not (or simula-BB-hpos simula-form)))
	  (list simula-hpos '(0 0))
	(Simula-Default-Form-Handler Simula-Default-Form))
    (setq simula-form nil)))
  


(defun Simula-Default-Form-Handler (form)
  (simula-collapse-stack)		;get assembled form
  (let ((indentation (get-indent-amount form)))
    (if (not indentation) nil
      (setq simula-hpos
	    (if (not (bolp))
		(save-excursion
		  (beginning-of-line)
		  (current-simula-indentation))
	      (current-simula-indentation))
	    indentation (cons (simula-indent-calc (car indentation))
			      (cdr indentation)))
      indentation)))			;return (hpos (abs relhpos))

(defun simula-collapse-stack ()
  (let ((last-beg (if simula-assemble (point) (simula-pop)))
	(pointer simula-stack))
    (while pointer
      (if (stringp (car pointer)) (setq pointer (cdr pointer))
	(if last-beg
	    (progn
	      (setcar pointer (buffer-substring last-beg (car pointer)))
	      (setq last-beg nil pointer (cdr pointer)))
	  (setq last-beg (car pointer))
	  (setcar pointer (car (cdr pointer))) ;delete cons-cell
	  (setcdr pointer (cdr (cdr pointer))))))
    (setq simula-form (apply 'concat simula-stack)
	  simula-stack (list (point) simula-form))))

(defun get-indent-amount (indent-form-list)
  (if indent-form-list
      (if (string-match (car (car indent-form-list)) simula-form)
	  (progn
	    (if simula-debug
		(with-output-to-temp-buffer "* forms *"
		  (print
		    (concat (car (car indent-form-list))"<---->" simula-form))))
	    (cdr (car indent-form-list)))
	(get-indent-amount (cdr indent-form-list)))
    nil))



;axiom: (bolp) eq t
(defun current-simula-indentation ()
  (if (looking-at simula-label)		;skip labels
      (re-search-forward simula-label))	;ignore labels
  (skip-chars-forward " \t")		;skip to first non-blank
  (current-column))			;and return with column nubmer


(defun simula-indent-calc (amount)
  (if amount
      (let ((from (car amount)))
	(+ (car (cdr amount))
	   (cond
	     ((= 0 from) simula-hpos)	;axiom: exists
	     ((and simula-FB-hpos (= 1 from)) simula-FB-hpos)
	     ((and simula-BB-hpos (= 2 from)) simula-BB-hpos)
	     (simula-hpos))))
    simula-hpos))


(defun simula-indent-line (to)
  (beginning-of-line)
  (if (= (following-char) ?\%) ()
    (let ((space (% to tab-width)) (tabs (/ to tab-width)))
      (if (looking-at simula-label)	;indent line after label
	  (progn
	    (re-search-forward simula-label) ;ignore labels
	    (if (> (current-column) to)
		(setq tabs 0 space 1)
	      (insert-char ?\t 1)	;try fill to nearest tab position
	      (if (> (current-column) to) ;else fill blanks.
		  (backward-delete-char 1))
	      (setq to (- to (current-column)))
	      (setq tabs (/ to tab-width) space (% to tab-width)))))
      (insert-char ?\t tabs)		;insert all the necessary tabs and 
      (insert-char ?\ space)		;spaces to indent line
      (delete-region
	(point) (progn (skip-chars-forward " \t" (point-max)) (point))))))


(defun simula-abbrev-expand-and-lf (arg)
  (interactive "p")
  (expand-abbrev)
  (insert-char ?\n 1)
  (forward-char -1)
  (let ((indent (save-excursion (simula-find-indent t))))
    (if (progn (beginning-of-line)
	       (skip-chars-forward " \t")
	       (/= (following-char) ?!)) ;Only indent lines not starting with
					;a comment or something like it..
	(simula-indent-line (car indent)))
    (forward-line 1)
    (simula-indent-line (simula-indent-calc (car (cdr indent))))))

(defun simula-indent ()
  (interactive)
  (simula-indent-line (car (save-excursion (simula-find-indent)))))
  
(defun Simula-While-Handler ()
  (Simula-Default-Form-Handler Simula-While-Form))

(defun Simula-If-Handler ()
  (Simula-Default-Form-Handler Simula-If-Form))

(defun Simula-Inspect-Handler ()
  (Simula-Default-Form-Handler Simula-Inspect-Form))

(defun Simula-For-Handler ()
  (Simula-Default-Form-Handler Simula-For-Form))


;;;;;; Nice Mode..
(defun simula-Nice-indent-mode ()
  (interactive)
  (setq Simula-While-Form
	'( ("while.*begin.*end;@" (0 0) (1 0))
	   ("while .*do.*begin\n.*\n.*end;@" (1 0) (0 0))
	   ("while .*do.*begin\n.*@" (1 3) (1 3))
	   ("while .*do.*begin.*@" (0 0) (1 3))
	   ("while .*do\n.*begin\n.*\n.*end;@" (2 0) (0 0))
	   ("while .*do\n.*begin\n.*@" (2 3) (2 3))
	   ("while .*do\n.*begin@" (1 3) (2 3))
	   ("while .*do\n.*;@" (1 3) (0 0))
	   ("while .*do\n.*@" (1 3) (1 3))
	   ("while .*do@" (0 0) (1 3))))
  (setq Simula-Default-Form
	'( ("begin.*end;@" (0 0) (0 0))
	   ("while .*do.*begin\n.*\n.*end;@" (0 0) (0 0))
	   ("begin.*@" (0 0) (2 3))
	   ("begin\n.*\n.*end.*@" (0 0) (0 0))
	   ("begin\n.*end;@" (2 3) (0 0))
	   ("begin\n.*\n.*end;@" (2 0) (0 0))
	   ("begin\n.*@" (2 3) (2 3))
	   ("begin\n.*\n@" (2 3) (2 3))
	   ("begin\n*.*\n*.*@" (2 3) (2 3))
	   (".*;@" (0 0) (0 0))
	   ("\n.*;@" (0 0) (0 0))
	   ("\n.*@" (0 0) (0 0))
	   ("." (0 0) (0 3))))
  (setq Simula-If-Form
	'( ("if.*begin.*end;@" (0 0) (1 0))
	   ("if .*begin.*@" (0 0) (2 3))
	   ("if .*else@" (0 0) (0 0))
	   ("if .*;@" (0 0) (0 0))
	   ("if .*@" (0 0) (0 3))
	   ("if .*begin.*\n.*@" (2 3) (2 3))
	   ("if .*\n.*;@" (0 3) (0 0))
	   ("if .*\n.*begin.*end.*@" (0 3) (0 0))
	   ("if .*\n.*begin.*@" (0 3) (2 3))
	   ("if .*else\n.*@" (0 3) (0 0))
	   ("if .*\n.*begin.*\n.*@" (2 3) (2 3))
	   ("if .*\n.*begin.*\n.*\n.*end.*@" (2 0) (0 0))
	   ("if .*begin.*\n.*\n.*end;.*@" (0 0) (0 0))
	   ("if .*begin.*\n.*\n.*end@" (2 0) (0 0))
	   ("else if.*@" (0 0) (0 3))
	   ("else@" (0 0) (0 3))
	   ("else.*begin.*@" (0 0) (2 3))
	   ("else.*begin.*\n.*@" (2 3) (2 3))
	   ("else.*begin.*\n.*\n.*end;@" (2 0) (0 0))
	   ("else .*;@" (0 0) (0 0))
	   ("else\n.*begin@" (0 3) (2 3))
	   ("else\n.*begin\n.*@" (2 3) (2 3))
	   ("else\n.*begin\n.*\n.*end.*@" (2 0) (0 0))))
  (setq Simula-For-Form
	'( ("for .*begin.*end;@" (0 0) (1 0))
	   ("for .*do.*;@" (0 0) (0 0))
	   ("for .*do@" (0 0) (1 3))
	   ("for .*do\n.*begin@" (1 3) (2 3))
	   ("for .*do\n.*begin\n.*@" (2 3) (2 3))
	   ("for .*do\n.*begin\n.*\n.*end.*@" (1 3) (0 0))
	   ("for .*do\n.*;@" (1 3) (0 0))
	   ("for .*do\n.*begin.*\n.*end.*@" (1 3) (0 0))
	   ("for .*do.*begin@" (0 0) (1 3))
	   ("for .*do.*begin\n.*end.*@" (1 3) (0 0))
	   ("for .*do.*begin\n.*@" (1 3) (1 3))
	   ("for .*do.*begin\n.*\n.*end.*@" (1 0) (0 0))))
  (setq Simula-Inspect-Form
	'( ("inspect .*do.*;@" (0 0) (0 0))
	   ("inspect .*do@" (0 0) (1 3))
	   ("inspect .*do\n.*begin.*end.*@" (1 3) (0 0))
	   ("inspect .*do\n.*begin.*@" (1 3) (2 3))
	   ("inspect .*do\n.*begin\n.*end.*@" (2 3) (0 0))
	   ("inspect .*do\n.*begin\n.*\n.*end.*@" (2 0) (0 0))
	   ("inspect .*do.*begin@" (0 0) (2 3))
	   ("inspect .*do.*begin\n.*end.*@" (2 3) (0 0))
	   ("inspect .*do.*begin\n.*@" (2 3) (2 3))
	   ("inspect .*do.*begin\n.*\n.*end.*;@" (2 0) (0 0))
	   ("inspect .*;@" (0 0) (0 0))
	   ("inspect .*@" (0 0) (0 3))
	   ("otherwise@" (0 0) (0 3))
	   ("otherwise\n.*begin@" (0 3) (2 3))
	   ("otherwise\n.*begin\n.*end.*@" (2 3) (0 0))
	   ("otherwise\n.*begin\n.*@" (2 3) (2 3))
	   ("otherwise\n.*begin\n.*\n.*end.*@" (2 0) (0 0))
	   ("otherwise .*begin .*end.*@" (0 0) (0 0))
	   ("otherwise .*begin.*@" (0 0) (2 3))
	   ("otherwise .*begin\n.*end.*@" (2 3) (0 0))
	   ("otherwise .*begin\n.*@" (2 3) (2 3))
	   ("otherwise .*begin\n.*\n.*end.*@" (2 0) (0 0))
	   ("when .*do@" (0 3) (0 6))
	   ("when .*do.*;@" (0 3) (0 0))
	   ("when .*do.*@" (0 3) (0 3))
	   ("when .*do\n.*begin@" (0 6) (2 3))
	   ("when .*do\n.*begin\n.*end;@" (2 3) (0 0))
	   ("when .*do\n.*begin\n.*@" (2 3) (2 3))
	   ("when .*do\n.*begin\n.*\n.*end;@" (2 0) (0 0))
	   ("when .*do\n.*begin\n.*\n.*end@" (2 0) (0 3))
	   ("when .*do\n.*begin .*end;@" (0 6) (0 0))
	   ("when .*do\n.*begin .*end@" (0 6) (0 3)))))

(defun simula-Simed-indent-mode ()
  ;;Should only indent after begin, so this is a overkill
  ;;Hopefully, I'll do better when I care for it.
  (interactive)
  (setq Simula-While-Form
	'( ("while .*do.*begin\n.*\nend;@" (1 0) (0 0))
	   ("while .*do.*begin\n.*@" (1 3) (1 3))
	   ("while .*do.*begin.*@" (0 0) (1 3))
	   ("while .*do\n.*begin\n.*\n.*end;@" (1 0) (0 0))
	   ("while .*do\n.*begin\n.*@" (2 3) (2 3))
	   ("while .*do\n.*begin@" (1 0) (1 3))
	   ("while .*do\n.*;@" (1 3) (0 0))
	   ("while .*do\n.*@" (1 3) (1 3))
	   ("while .*do@" (0 0) (1 0))))
  (setq Simula-Default-Form
	'( ("begin.*end;@" (0 0) (0 0))
	   ("begin.*@" (0 0) (2 3))
	   ("begin\n.*\nend" (0 0) (0 0))
	   ("begin\n.*end;@" (2 3) (0 0))
	   ("begin\n.*@" (2 3) (2 3))
	   ("begin\n*.*\n*.*@" (2 3) (2 3))
	   (".*;@" (0 0) (0 0))
	   ("\n.*;@" (0 0) (0 0))
	   ("\n.*@" (0 0) (0 0))
	   ("." (0 0) (0 3))))
  (setq Simula-If-Form
	'( ("if .*begin.*@" (0 0) (0 3))
	   ("if .*else@" (0 0) (0 0))
	   ("if .*;@" (0 0) (0 0))
	   ("if .*@" (0 0) (0 0))
	   ("if .*begin.*\n.*@" (0 3) (0 3))
	   ("if .*\n.*;@" (0 3) (0 0))
	   ("if .*\n.*begin.*end.*@" (0 0) (0 0))
	   ("if .*\n.*begin.*@" (0 0) (0 3))
	   ("if .*else\n.*@" (0 0) (0 0))
	   ("if .*\n.*begin.*\n.*@" (0 3) (0 3))
	   ("if .*\n.*begin.*\n.*\n.*end.*@" (0 0) (0 0))
	   ("if .*begin.*\n.*\n.*end;.*@" (0 0) (0 0))
	   ("if .*begin.*\n.*\n.*end@" (0 0) (0 0))
	   ("else if.*@" (0 0) (0 0))
	   ("else@" (0 0) (0 0))
	   ("else.*begin.*@" (0 0) (0 3))
	   ("else.*begin.*\n.*@" (0 3) (0 3))
	   ("else.*begin.*\n.*\n.*end;@" (0 0) (0 0))
	   ("else .*;@" (0 0) (0 0))
	   ("else\n.*begin@" (0 0) (0 3))
	   ("else\n.*begin\n.*@" (0 3) (0 3))
	   ("else\n.*begin\n.*\n.*end.*@" (0 0) (0 0))))
  (setq Simula-For-Form
	'( ("for .*do.*;@" (0 0) (0 0))
	   ("for .*do@" (0 0) (0 0))
	   ("for .*do\n.*begin@" (0 0) (0 3))
	   ("for .*do\n.*begin\n.*@" (0 3) (0 3))
	   ("for .*do\n.*begin\n.*\n.*end.*@" (0 0) (0 0))
	   ("for .*do\n.*;@" (0 3) (0 0))
	   ("for .*do\n.*begin.*\n.*end.*@" (0 0) (0 0))
	   ("for .*do.*begin@" (0 0) (0 3))
	   ("for .*do.*begin\n.*end.*@" (0 3) (0 0))
	   ("for .*do.*begin\n.*@" (0 3) (0 3))
	   ("for .*do.*begin\n.*\n.*end.*@" (0 0) (0 0))))
  (setq Simula-Inspect-Form
	'( ("inspect .*do.*;@" (0 0) (0 0))
	   ("inspect .*do@" (0 0) (0 0))
	   ("inspect .*do\n.*begin.*end.*@" (0 3) (0 0))
	   ("inspect .*do\n.*begin.*@" (0 0) (0 3))
	   ("inspect .*do\n.*begin\n.*end.*@" (0 0) (0 0))
 	   ("inspect .*do\n.*begin\n.*\n.*end.*@" (0 0) (0 0))
	   ("inspect .*do.*begin@" (0 0) (0 3))
	   ("inspect .*do.*begin\n.*end.*@" (0 3) (0 0))
	   ("inspect .*do.*begin\n.*@" (0 3) (0 3))
	   ("inspect .*do.*begin\n.*\n.*end.*;@" (0 0) (0 0))
	   ("inspect .*;@" (0 0) (0 0))
	   ("inspect .*@" (0 0) (0 0))
	   ("otherwise@" (0 0) (0 0))
	   ("otherwise\n.*begin@" (0 0) (0 3))
	   ("otherwise\n.*begin\n.*end.*@" (0 3) (0 0))
	   ("otherwise\n.*begin\n.*@" (0 3) (0 3))
	   ("otherwise\n.*begin\n.*\n.*end.*@" (0 0) (0 0))
	   ("otherwise .*begin .*end.*@" (0 0) (0 0))
	   ("otherwise .*begin.*@" (0 0) (0 3))
	   ("otherwise .*begin\n.*end.*@" (0 3) (0 0))
	   ("otherwise .*begin\n.*@" (0 3) (0 3))
	   ("otherwise .*begin\n.*\n.*end.*@" (0 0) (0 0))
	   ("when .*do@" (0 0) (0 0))
	   ("when .*do.*;@" (0 0) (0 0))
	   ("when .*do.*@" (0 0) (0 0))
	   ("when .*do\n.*begin@" (0 0) (0 3))
	   ("when .*do\n.*begin\n.*end;@" (0 3) (0 0))
	   ("when .*do\n.*begin\n.*@" (0 3) (0 3))
	   ("when .*do\n.*begin\n.*\n.*end;@" (0 0) (0 0))
	   ("when .*do\n.*begin\n.*\n.*end@" (0 0) (0 0))
	   ("when .*do\n.*begin .*end;@" (0 3) (0 0))
	   ("when .*do\n.*begin .*end@" (0 3) (0 0)))))
