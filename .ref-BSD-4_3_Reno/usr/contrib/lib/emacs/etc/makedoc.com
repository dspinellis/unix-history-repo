$ ! VMS command file to create or update the file `DOC.' which contains
$ ! documentation strings for the functions and variables preloaded in Emacs.
$ ! This command file should be run when you build Emacs for the first time and
$ ! again if any documentation strings change in the source files listed here.
$
$ old = f$environment("default")
$ set default emacs_library:[etc]
$ on error then goto done
$ if f$search("emacs_library:[etc]make-docfile.exe") .nes. "" then goto version44
$ doit := $emacs_library:[etc]make_docfile
$ rest = "[lisp]lisp_mode.elc [lisp]text_mode.elc [lisp]c_mode.elc [lisp]buff_menu.elc [lisp]vms_patch.elc"
$ goto doit
$version44:
$ doit := $emacs_library:[etc]make-docfile
$ rest = "[lisp]lisp-mode.elc [lisp]text-mode.elc [lisp]c-mode.elc [lisp]buff-menu.elc [lisp]vms-patch.elc"
$
$doit:
$ set default emacs_library:[000000]
$ doit -o [etc]DOC
$ doit := 'doit' -a [etc]DOC
$ doit [src]dispnew.c [src]scroll.c
$ doit [src]xdisp.c [src]window.c [src]term.c [src]cm.c
$ doit [src]emacs.c [src]keyboard.c [src]macros.c
$ doit [src]keymap.c [src]sysdep.c [src]buffer.c
$ doit [src]filelock.c [src]insdel.c [src]marker.c
$ doit [src]minibuf.c [src]fileio.c [src]dired.c
$ doit [src]filemode.c [src]cmds.c [src]casefiddle.c
$ doit [src]indent.c [src]search.c [src]regex.c
$ doit [src]undo.c [src]alloc.c [src]data.c [src]doc.c
$ doit [src]editfns.c [src]callint.c [src]eval.c
$ doit [src]fns.c [src]print.c [src]lread.c [src]abbrev.c
$ doit [src]syntax.c [src]mocklisp.c
$ doit [src]bytecode.c [src]process.c [src]callproc.c [src]doprnt.c
$ doit [src]vmsfns.c
$
$ doit [lisp]simple.elc [lisp]help.elc
$ doit [lisp]files.elc [lisp]window.elc
$ doit [lisp]indent.elc [lisp]loaddefs.el
$ doit [lisp]paths.el [lisp]startup.elc
$ doit [lisp]lisp.elc [lisp]page.elc
$ doit [lisp]register.elc [lisp]paragraphs.elc
$ doit [lisp]fill.elc [lisp]isearch.elc
$ doit [lisp]replace.elc [lisp]abbrev.elc
$ doit [lisp]subr.elc [lisp]vmsproc.elc
$ doit 'rest'
$
$done:
$ set default 'old'
