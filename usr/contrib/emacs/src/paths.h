/* the default search path for Lisp function "load" */
#define PATH_LOADSEARCH ":/usr/new/lib/emacs/lisp"

/* the extra search path for programs to invoke.
 This is appended to whatever the PATH environment variable says. */
#define PATH_EXEC "/usr/new/lib/emacs/etc"

/* the name of the directory that contains lock files
 with which we record what files are being modified in Emacs.
 This directory should be writable by everyone.
 THE STRING MUST END WITH A SLASH!!!  */
#define PATH_LOCK "/usr/new/lib/emacs/lock/"

/* the name of the file !!!SuperLock!!! in the directory
 specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "/usr/new/lib/emacs/lock/!!!SuperLock!!!"
