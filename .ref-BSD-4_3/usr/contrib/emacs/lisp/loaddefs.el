;; Define standard autoloads and keys of other files, for Emacs.
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


;; Know which function the debuger is!
(setq debugger 'debug)

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

(defconst paragraph-start "^[ \t\n\f]"
  "*Regexp for beginning of a line that starts OR separates paragraphs.")
(defconst paragraph-separate "^[ \t\f]*$"
  "*Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change paragraph-start also.")

(defconst sentence-end   "[.?!][]\")]*\\($\\|\t\\|  \\)[ \t\n]*"
  "*Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless.")

(defconst page-delimiter "^\014"
  "*Regexp describing line-beginnings that separate pages.")

(defconst case-replace t
  "*Non-nil means query-replace should preserve case in replacements.")

;; indent.el may not be autoloading, but it still loses
;; if lisp-mode is ever called before this defvar is done.
(defvar indent-line-function
  'indent-to-left-margin
  "Function to indent current line.")

(defconst only-global-abbrevs nil
  "*t means user plans to use global abbrevs only.
Makes the commands to define mode-specific abbrevs define global ones instead.")

(defconst abbrev-file-name "~/.abbrev_defs"
  "*Default name of file to read abbrevs from.")

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      '(".o" ".elc" "~" ".dvi" ".toc" ".log" ".aux" ".lof"))

(defvar compile-command "make -k"
  "*Last shell command used to do a compilation; default for next compilation.")

(defvar dired-listing-switches "-al"
  "*Switches passed to ls for Dired.  MUST contain the `l' option.
CANNOT contain the `F' option.")

(defconst lpr-switches nil
  "*List of strings to pass as extra switch args to lpr when it is invoked.")

(defvar tags-file-name nil
  "*File name of tag table.
To switch to a new tag table, setting this variable is sufficient.
Use the `etags' program to make a tag table file.")

(defconst shell-prompt-pattern
  "^[^#$%>]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not reexecuted.")

(defconst ledit-save-files t
  "*Non-nil means Ledit should save files before transferring to Lisp.")
(defconst ledit-go-to-lisp-string "%?lisp"
  "*Shell commands to execute to resume Lisp job.")
(defconst ledit-go-to-liszt-string "%?liszt"
  "*Shell commands to execute to resume Lisp compiler job.")

(defconst display-time-day-and-date nil
  "*Non-nil means M-x display-time should display day and date as well as time.")

;;; Determine mode according to filename

(defvar auto-mode-alist
	'(("\\.text$" . text-mode)
	  ("\\.mss$" . text-mode)
	  ("\\.tex$" . TeX-mode)
	  ("\\.TeX$" . TeX-mode)
	  ("\\.sty$" . TeX-mode)
	  ("\\.texinfo$" . texinfo-mode)
	  ;; Mailer puts message to be edited in /tmp/Re.... or Message
	  ("^/tmp/Re" . non-saved-text-mode)
	  ;; some news reader is reported to use this
	  ("^/tmp/fol/" . non-saved-text-mode)
 	  ("/Message[0-9]*$" . text-mode)
          ("\\.c$" . c-mode)
          ("\\.h$" . c-mode)
          ("\\.y$" . c-mode)
          ("\\.scm$" . scheme-mode)
	  ("\\.scm.[0-9]*$" . scheme-mode)
	  ("/\\..*emacs" . emacs-lisp-mode)
          ("\\.el$" . emacs-lisp-mode)
          ("\\.ml$" . lisp-mode)
	  ("\\.l$" . lisp-mode)
	  ("\\.lisp$" . lisp-mode))
  "Alist of filename patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION).
Visiting a file whose name matches REGEXP causes FUNCTION to be called.")

;; Autoload random libraries.
;; Alphabetical order by library name.

(autoload 'add-change-log-entry "add-log"
  "\
Find change log file and add an entry for today.
With ARG, prompt for name and site of person."
  t)

(autoload 'occur-menu "aton"
  "\
Show menu of lines containing match for REGEXP.
Enters recursive edit on text showing an entry for each matching line.
User can move to an entry and then exit with \\[exit-recursive-edit] to
move to the line in the original buffer described by the selected entry.
Abort with \\[abort-recursive-edit] to avoid moving in the original buffer.

If REGEXP is empty then THE EXACT SAME menu is presented again,
with cursor initially at the next successive entry.
This is useful for stepping through located lines rapidly in order."
  t)

(autoload '\` "backquote"
  "\
(` FORM) Expands to a form that will generate FORM.
FORM is `almost quoted' -- see backquote.el for a description."
  nil t)

(autoload 'byte-compile-file "bytecomp"
  "\
Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending \"c\" to the end of FILENAME."
  t)

(autoload 'byte-recompile-directory "bytecomp"
  "\
Recompile every .el file in DIRECTORY that needs recompilation.
This is if a .elc file exists but is older than the .el file.
If the .elc file does not exist, offer to compile the .el file
only if a prefix argument has been specified." 
  t)

(autoload 'batch-byte-compile "bytecomp"
  "\
Runs byte-compile-file on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\""
  nil)


(autoload 'list-command-history "chistory"
  "\
List history of commands typed to minibuffer.
The number of commands listed is controlled by  list-command-history-max.
Calls value of  list-command-history-filter  (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode."
  t)

(autoload 'command-history-mode "chistory"
  "\
Major mode for examining commands from  command-history.
The number of commands listed is controlled by  list-command-history-max.
The command history is filtered by  list-command-history-filter  if non-nil.

Like Emacs-Lisp Mode except that characters do not insert themselves and
Digits provide prefix arguments.  Tab does not indent.  Instead these
commands are provided:
     LF, CR	Move to the next line in the history.
     Delete	Move to the previous line in the history.

Calls the value of  command-history-hook  if that is non-nil
The Command History listing is recomputed each time this mode is
invoked."
  t)

(autoload 'repeat-matching-complex-command "chistory"
  "\
Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you
select a form for evaluation.  If PATTERN is empty (or nil), every form
in the command history is offered.  The form is placed in the minibuffer
for editing and the result is evaluated.  Prefix args don't count."
  t)


(autoload 'compare-windows "compare-w"
  "\
Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as fas as they match."
  t)

(autoload 'compile "compile"
  "\
Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it."
  t)

(autoload 'grep "compile"
  "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  t)

(define-key ctl-x-map "`" 'next-error)

(autoload 'next-error "compile"
  "\
Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  t)

(autoload 'debug "debug"
  "\
Enter debugger.  Returns if user says \"continue\".
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer.")

(autoload 'cancel-debug-on-entry "debug"
  "\
Undoes effect of debug-on-entry on FUNCTION."
  t)

(autoload 'debug-on-entry "debug"
  "\
Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined."
  t)

(define-key ctl-x-map "d" 'dired)

(autoload 'dired "dired"
  "\
\"Edit\" directory DIRNAME.  Delete some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  t)

(define-key ctl-x-4-map "d" 'dired-other-window)

(autoload 'dired-other-window "dired"
  "\
\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another window."
  t)

(autoload 'dired-noselect "dired"
  "\
Find or create a dired buffer, return it, don't select it.
Call like dired.")

(autoload 'dissociated-press "dissociate"
  "\
Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG words of continuity.
If ARG is negative, require -ARG chars of continuity.
Default is 2." t)

(autoload 'doctor "doctor"
  "\
Switch to *doctor* buffer and start giving psychotherapy."
  t)

(autoload 'electric-buffer-list "ebuff-menu"
  "\
Vaguely like ITS lunar select buffer;
combining typeoutoid buffer listing with menuoid buffer selection.
This pops up a buffer describing the set of emacs buffers.
If the very next character typed is a space then the buffer list is killed.

Otherwise, one may use \\[next-line] and \\[previous-line] to move around in the buffer list window
and select a buffer by typing Space when the cursor is on the
appropriate line of the buffer-list window.
Other commands are much like those of buffer-menu-mode.
Type C-h after calling \\[electric-buffer-list] for more information.

Calls the value of  electric-buffer-menu-mode-hook  if that is non-nil."
  t)


(autoload 'electric-command-history "echistory"
  "\
Major mode for examining and redoing commands from  command-history.
The number of command listed is controlled by  list-command-history-max.
The command history is filtered by  list-command-history-filter  if non-nil.
Combines typeout Command History list window with menu like selection
of an expression from the history for re-evaluation in the *original* buffer.

The history displayed is filtered by  list-command-history-filter  if non-nil.

This pops up a window with the Command History listing.  If the very
next character typed is Space, the listing is killed and the previous
window configuration is restored.  Otherwise, you can browse in the
Command History with  Return  moving down and  Delete  moving up, possibly
selecting an expression to be redone with Space or quitting with `Q'.

Like Emacs-Lisp Mode except that characters do not insert themselves and
Tab and linefeed do not indent.  Instead these commands are provided:
Space or !	edit then evaluate current line in history inside
		   the ORIGINAL buffer which invoked this mode.
		   The previous window configuration is restored
		   unless the invoked command changes it.
C-c C-c, C-], Q	Quit and restore previous window configuration.
LF, CR		Move to the next line in the history.
Delete		Move to the previous line in the history.
?		Provides a complete list of commands.

Calls the value of  electric-command-history-hook  if that is non-nil
The Command History listing is recomputed each time this mode is invoked."
  t)

(autoload 'hanoi "hanoi"
  "\
Towers of Hanoi diversion.  Arg is number of rings."
  t)

(autoload 'Helper-help "helper"
  "\
Provide help for current mode."
  t)

(autoload 'Helper-describe-bindings "helper"
  "\
Describe local key bindings of current mode."
  t)


(autoload 'info "info"
  "\
Enter Info documentation browser."
  t)

(autoload 'ledit-mode "ledit"
  "\
Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  M-C-d	-- record defun at or after point
	   for later transmission to Lisp job.
  M-C-r -- record region for later transmission to Lisp job.
  C-x z -- transfer to Lisp job and transmit saved text.
  M-C-c -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)"
  t)

(autoload 'ledit-from-lisp-mode "ledit")

(autoload 'run-lisp "shell"
  "\
Run an inferior Lisp process, input and output via buffer *lisp*."
  t)

(autoload 'lpr-buffer "lpr"
  "\
Print contents of buffer as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'print-buffer "lpr"
  "\
Print contents of buffer as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'lpr-region "lpr"
  "\
Print contents of region as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'print-region "lpr"
  "\
Print contents of region as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'append-kbd-macro "macros"
  "\
Append kbd macro NAME in file FILE, as Lisp code to define the macro.
Use  load  to load the file.
Third argument KEYS non-nil means also record the keys it is on.
 (This is the prefix argument, when calling interactively.)"
  t)

(define-key ctl-x-map "q" 'kbd-macro-query)

(autoload 'kbd-macro-query "macros"
  "\
Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 DEL -- skip the rest of the macro; start next repetition.
 C-d -- skip rest of the macro and don't repeat it any more.
 C-r -- enter a recursive edit, then on exit ask again for a character
 C-l -- redisplay screen and ask again."
  t)

(autoload 'write-kbd-macro "macros"
  "\
Save kbd macro NAME in file FILE, as Lisp code to define the macro.
Use  load  to load the file.
Third argument KEYS non-nil means also record the keys it is on.
 (This is the prefix argument, when calling interactively.)
Fourth argument APPENDFLAG non-nil meams append to FILE's existing contents."
  t)

(autoload 'make-command-summary "makesum"
  "\
Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  t)

(autoload 'define-mail-alias "mailalias"
  "Define NAME as a mail-alias that translates to DEFINITION."
  t)

(autoload 'manual-entry "man"
  "\
Display Unix manual entry for TOPIC."
  t)

(autoload 'mh-rmail "mh-e"
  "\
Read your mail with mh." t)

(autoload 'mh-smail "mh-e"
  "\
Compose and send mail with mh." t)

(autoload 'convert-mocklisp-buffer "mlconvert"
  "\
Convert buffer of Mocklisp code to real Lisp that GNU Emacs can run."
  t)

(setq disabled-command-hook 'disabled-command-hook)

(autoload 'disabled-command-hook "novice")
(autoload 'enable-command "novice"
  "\
Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t)

(autoload 'disable-command "novice"
  "\
Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t)

(autoload 'nroff-mode "nroff-mode"
  "\
Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs text-mode-hook, then nroff-mode-hook.
Also, try nroff-electric-mode, for automatically inserting
closing requests for requests that are used in matched pairs."
  t)

(autoload 'list-options "options"
  "\
Display a list of Emacs user options, with values and documentation."
  t)

(autoload 'edit-options "options"
  "\
Edit a list of Emacs user option values.
Selects a buffer containing such a list,
in which there are commands to set the option values.
Type \\[describe-mode] in that buffer for a list of commands."
  t)

(autoload 'outline-mode "outline"
  "\
Set up Emacs for editing an outline, doing selective hiding of text."
  t)

(autoload 'edit-picture "picture"
  "\
Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  M-`	  Move left after insertion.
  M-'	  Move right after insertion.
  M--	  Move up after insertion.
  M-=	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  C-p	  Move vertically to SAME column in previous line.
  C-n	  Move vertically to SAME column in next line.
  C-e	  Move to column following last non-whitespace character.
  C-f	  Move right inserting spaces if required.
  C-b	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting charecter.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in local tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  Delete  Clear (replace) ARG columns before point, moving back over them.
  C-k	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  C-o	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c k	  Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c y	  Like C-c C-y except rectangle is taken from named register.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  edit-picture-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys."
  t)

(fset 'picture-mode 'edit-picture)

(autoload 'clear-rectangle "rect"
  "\
Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  t)

(autoload 'delete-rectangle "rect"
  "\
Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  t)

(autoload 'delete-extract-rectangle "rect"
  "\
Return and delete contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle.")

(autoload 'extract-rectangle "rect"
  "\
Return contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle.")

(autoload 'insert-rectangle "rect"
  "\
Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point,
its second line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.")

(autoload 'kill-rectangle "rect"
  "\
Delete rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use delete-extract-rectangle."
  t)

(autoload 'open-rectangle "rect"
  "\
Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but insted winds up to the right of the rectangle."
  t)

(autoload 'yank-rectangle "rect"
  "\
Yank the last killed rectangle with upper left corner at point."
  t)

(autoload 'rnews "rnews"
  "\
Read netnews for groups for which you are a member and add or delete groups.
You can reply to articles posted and send articles to any group.
Type \\[describe-mode] once reading news to get a list of rnews commands."
  t)

(autoload 'rmail "rmail"
  "\
Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in Rmail Mode.
Type \\[describe-mode] once editing that file, for a list of Rmail commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  t)

(defconst rmail-dont-reply-to-names
  nil ;(concat (getenv "USER"))
  "*A regular expression specifying names to prune in replying to messages.
nil means dont reply to yourself.")

(defconst rmail-ignored-headers
   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:"
   "*Gubbish headers one would rather not see.")

(defvar rmail-delete-after-output nil
  "*Non-nil means automatically delete a message that is copied to a file.")

;;; Others are in paths.el.


(autoload 'scheme-mode "scheme"
  "\
Set up things for editing scheme code,
or for running emacs as an inferior editor under scheme
to edit and zap functions."
  t)
  

;; Useful to set in site-init.el
(defconst send-mail-function 'sendmail-send-it
  "Function to call to send the current buffer as mail.
The headers are delimited by a string found in mail-header-separator.")

(defconst mail-self-blind nil
  "*Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

(defconst mail-interactive nil
  "*Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

(defconst mail-yank-ignored-headers
   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^to:\\|^cc:\\|^subject:\\|^in-reply-to:\\|^return-path:"
   "Delete these headers from old message when it's inserted in a reply.")

(defconst mail-header-separator "--text follows this line--"
  "*Line used to separate headers from text in messages being composed.")

(defconst mail-archive-file-name nil
  "*Name of file to write all outgoing messages in, or nil for none.")

(defvar mail-aliases t
  "Alias of mail address aliases,
or t meaning should be initialized from .mailrc.")

(autoload 'mail-other-window "sendmail"
  "\
Like mail command but displays in other window."
  t)

(autoload 'mail "sendmail"
  "\
Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields.  Type C-c ? for a list of them.

If mail-self-blind is non-nil, a bcc to yourself is inserted
when the message is initialized.

If mail-setup-hook is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, CC and IN-REPLY-TO specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c y."
  t)

(define-key ctl-x-4-map "m" 'mail-other-window)
(define-key ctl-x-map "m" 'mail)


(autoload 'shell "shell"
  "\
Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See shell-mode."
  t)

(autoload 'spell-buffer "spell"
  "\
Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped."
  t)

(autoload 'spell-region "spell"
  "\
Like spell-buffer but applies only to region.
From program, applies from START to END."
  t)

(define-key esc-map "$" 'spell-word)
(autoload 'spell-word "spell"
  "\
Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and query-replace the entire buffer to substitute it."
  t)

(autoload 'spell-string "spell"
  "\
Check spelling of string supplied as argument."
  t)

(autoload 'untabify "tabify"
  "\
Convert all tabs in region to multiple spaces, preserving columns.
The variable tab-width controls the action."
  t)

(autoload 'tabify "tabify"
  "\
Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
The variable tab-width controls the action."
  t)

(define-key esc-map "." 'find-tag)

(autoload 'find-tag "tags"
  "\
Find next tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  t)

(define-key ctl-x-4-map "." 'find-tag-other-window)

(autoload 'find-tag-other-window "tags"
  "\
Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  t)

(autoload 'list-tags "tags"
  "\
Display list of tags in file FILE."
  t)

(autoload 'next-file "tags"
  "\
Select next file among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  t)

(autoload 'tags-apropos "tags"
  "\
Display list of all tags in tag table that contain STRING."
  t)

(define-key esc-map "," 'tags-loop-continue)
(autoload 'tags-loop-continue "tags"
  "\
Continue last tags-search or tags-query-replace command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  t)

(autoload 'tag-table-files "tags"
  "\
Return a list of files in the current tag table.")

(autoload 'tags-query-replace "tags"
  "\
Query-replace FROM with TO through all files listed in tag table.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  t)

(autoload 'tags-search "tags"
  "\
Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  t)

(autoload 'visit-tags-table "tags"
  "\
Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory."
  t)

(autoload 'telnet "telnet"
  "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-telnet*.
Normally input is edited in Emacs and sent a line at a time."
  t)

(autoload 'tex-mode "tex-mode"
  "\
Major mode for editing TeX input files.\n\
Activates $ and \" with TeX meaning, makes ()[]{} have proper syntax.\n\
M-$ erects barrier to speed up matching $ searches.\n\
Turning on TeX-mode calls the value of the variable TeX-mode-hook,\n\
if that value is non-nil."
  t)
(fset 'TeX-mode 'tex-mode)

(autoload 'texinfo-format-buffer "texinfo"
  "\
Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer
visiting the Info file name specified in the @setfilename command."
  t)

(autoload 'texinfo-mode "texinfo"
  "\
Major mode for editing texinfo files.
These are files that are input for TEX and also to be turned
into Info files by M-x texinfo-format-buffer.
These files must be written in a very restricted and
modified version of TEX input format.

As for editing commands, like text-mode except for syntax table,
which is set up so expression commands skip texinfo bracket groups."
  t)

(autoload 'display-time "time"
  "\
Display current time and load level in mode line of each buffer.
Updates automatically every minute."
  t)

(autoload 'underline-region "underline"
  "\
Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
  t)

(autoload 'ununderline-region "underline"
  "\
Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  t)

(autoload 'ask-user-about-lock "userlock"
  "\
Ask user what to do when he wants to edit FILE but it is locked by USER.
This function has a choice of three things to do:
  do (signal 'buffer-file-locked (list FILE USER))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can rewrite it to use any criterion you like to choose which one to do."
  nil)

(autoload 'view-file "view"
  "\
View FILE in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  t)

(autoload 'view-buffer "view"
  "\
View BUFFER in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  t)

(autoload 'view-mode "view"
  "\
Major mode for viewing text but not editing it."
  t)


(autoload 'yow "yow"
  "Return or display a Zippy quotation" t)
(autoload 'psychoanalyze-pinhead "yow"
  "Zippy goes to the analyst" t)


(define-key esc-map "\C-f" 'forward-sexp)
(define-key esc-map "\C-b" 'backward-sexp)
(define-key esc-map "\C-u" 'backward-up-list)
(define-key esc-map "\C-@" 'mark-sexp)
(define-key esc-map "\C-d" 'down-list)
(define-key esc-map "\C-k" 'kill-sexp)
(define-key esc-map "\C-n" 'forward-list)
(define-key esc-map "\C-p" 'backward-list)
(define-key esc-map "\C-a" 'beginning-of-defun)
(define-key esc-map "\C-e" 'end-of-defun)
(define-key esc-map "\C-h" 'mark-defun)
(define-key esc-map "(" 'insert-parentheses)
(define-key esc-map ")" 'move-past-close-and-reindent)

(define-key ctl-x-map "\C-e" 'eval-last-sexp)

(define-key ctl-x-map "/" 'point-to-register)
(define-key ctl-x-map "j" 'register-to-point)
(define-key ctl-x-map "x" 'copy-to-register)
(define-key ctl-x-map "g" 'insert-register)
(define-key ctl-x-map "r" 'copy-rectangle-to-register)

(define-key esc-map "q" 'fill-paragraph)
(define-key esc-map "g" 'fill-region)
(define-key ctl-x-map "." 'set-fill-prefix)

(define-key esc-map "[" 'backward-paragraph)
(define-key esc-map "]" 'forward-paragraph)
(define-key esc-map "h" 'mark-paragraph)
(define-key esc-map "a" 'backward-sentence)
(define-key esc-map "e" 'forward-sentence)
(define-key esc-map "k" 'kill-sentence)
(define-key ctl-x-map "\177" 'backward-kill-sentence)

(define-key ctl-x-map "[" 'backward-page)
(define-key ctl-x-map "]" 'forward-page)
(define-key ctl-x-map "\C-p" 'mark-page)
(put 'narrow-to-region 'disabled t)
(define-key ctl-x-map "p" 'narrow-to-page)
(define-key ctl-x-map "l" 'count-lines-page)

(defun isearch-forward ()
  "\
Do incremental search forward.
As you type characters, they add to the search string and are found.
Type Delete to cancel characters from end of search string.
Type ESC to exit, leaving point at location found.
Type C-S to search again forward, C-R to search again backward.
Type C-W to yank word from buffer onto end of search string and search for it.
Type C-Y to yank rest of line onto end of search string, etc.
Type C-Q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x apropos on search-.*-char to find them.
C-G while searching or when search has failed
 cancels input back to what has been found successfully.
C-G when search is successful aborts and moves point to starting point."
  (interactive)
  (isearch t))

(defun isearch-forward-regexp ()
  "\
Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See  isearch-forward  for more info."
  (interactive)
  (isearch t t))

(defun isearch-backward ()
  "\
Do incremental search backward.
See  isearch-forward  for more information."
  (interactive)
  (isearch nil))

(defun isearch-backward-regexp ()
  "\
Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See  isearch-forward  for more info."
  (interactive)
  (isearch nil t))

(defvar search-last-string ""
  "Last string search for by a search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted.")

(defconst search-repeat-char ?\C-s
  "Character to repeat incremental search forwards.")
(defconst search-reverse-char ?\C-r
  "Character to repeat incremental search backwards.")
(defconst search-exit-char ?\e
  "Character to exit incremental search.")
(defconst search-delete-char ?\177
  "Character to delete from incremental search string.")
(defconst search-quote-char ?\C-q
  "Character to quote special characters for incremental search.")
(defconst search-yank-word-char ?\C-w
  "Character to pull next word from buffer into search string.")
(defconst search-yank-line-char ?\C-y
  "Character to pull rest of line from buffer into search string.")
(defconst search-exit-option t
  "Non-nil means random control characters terminate incremental search.")

(defvar isearch-slow-window-lines 1
  "*Number of lines in slow search display windows.")
(defvar isearch-slow-speed 1200
  "*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached.")

(autoload 'isearch "isearch")

(define-key global-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward)
(define-key esc-map "\C-s" 'isearch-forward-regexp)

(defun query-replace (from-string to-string &optional arg)
  "\
Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
  (interactive "sQuery replace: \nsQuery replace %s with: \nP")
  (perform-replace from-string to-string t nil arg))

(defun query-replace-regexp (regexp to-string &optional arg)
  "\
Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (interactive "sQuery replace regexp: \nsQuery replace regexp %s with: \nP")
  (perform-replace regexp to-string t t arg))

(defun replace-string (from-string to-string &optional delimited)
  "\
Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
  (interactive "sReplace string: \nsReplace string %s with: \nP")
  (perform-replace from-string to-string nil nil delimited))

(defun replace-regexp (regexp to-string &optional delimited)
  "\
Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if case-replace and case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (interactive "sReplace regexp: \nsReplace regexp %s with: \nP")
  (perform-replace regexp to-string nil t delimited))

(define-key esc-map "%" 'query-replace)

(autoload 'perform-replace "replace")

(define-key ctl-x-map "\C-a" 'add-mode-abbrev)
(define-key ctl-x-map "\+" 'add-global-abbrev)
(define-key ctl-x-map "\C-h" 'inverse-add-mode-abbrev)
(define-key ctl-x-map "\-" 'inverse-add-global-abbrev)
(define-key esc-map "'" 'abbrev-prefix-mark)
