/* The default search path for Lisp function "load".
   This sets load-path.  */
#define PATH_LOADSEARCH "/usr/contrib/lib/emacs/lisp"

/* the extra search path for programs to invoke.
 This is appended to whatever the PATH environment variable says
 to set the Lisp variable exec-path and the first file namein it
  sets the Lisp variable exec-directory.  */
#define PATH_EXEC "/usr/contrib/lib/emacs/etc"

/* the name of the directory that contains lock files
 with which we record what files are being modified in Emacs.
 This directory should be writable by everyone.
 THE STRING MUST END WITH A SLASH!!!  */
#define PATH_LOCK "/var/emacs/lock/"

/* the name of the file !!!SuperLock!!! in the directory
 specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "/var/emacs/lock/!!!SuperLock!!!"
