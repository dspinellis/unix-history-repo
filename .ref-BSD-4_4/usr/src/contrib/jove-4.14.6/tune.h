/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define TUNED		1	/* don't touch this */

#include "sysdep.h"

#ifdef	UNIX
# define SUBPROCS	1	/* only on UNIX systems (NOT INCORPORATED YET) */
# define IPROCS		1	/* interactive processes */
# ifdef	BSD4_2
#   define PTYPROCS	1	/* use pseudo-ttys */
# else
#   define PIPEPROCS	1	/* use pipes */
# endif
#endif	/* UNIX */

#define BACKUPFILES	1	/* enable the backup files code */
#define F_COMPLETION	1	/* filename completion */
#define ABBREV		1	/* word abbreviation mode */

#ifdef	UNIX
# define ID_CHAR	1	/* include code to IDchar */
/* # define WIRED_TERMS 1 */	/* include code for wired terminals */
#endif

#define LISP		1	/* include the code for Lisp Mode */
#define CMT_FMT		1	/* include the comment formatting routines */

#ifdef	UNIX
/* Use the load average for various commands.
 * Do not define LOAD_AV if you lack a load average
 * system call and kmem is read protected.
 */
/* # define LOAD_AV	1 */

# define BIFF		1	/* if you have biff (or the equivalent) */
# define SPELL		1	/* spell words and buffer commands */
#endif

#define DFLT_MODE	0666	/* file will be created with this mode */

/* If the compiler does not support void, use -Dvoid=int or
 * typedef int	void;
 */

/* USE_PROTOTYPE must be defined for compilers that support prototypes
 * but are NOT ANSI C, i.e. do not have __STDC__ == 1.
 */

/* NO_PTRPROTO must be defined for compilers that support prototypes,
 * but do NOT support prototypes for pointers to functions.
 * It seems that some MSDOS and Mac compilers fall in this category.
 */
#ifdef	MSDOS
# define NO_PTRPROTO 1
#endif

#ifdef	MAC
# define USE_PROTOTYPES	1
# define NO_PTRPROTO	1
#endif

/* These are here since they define things in tune.c.  If you add things to
   tune.c, add them here too, if necessary. */

extern char
	*d_tempfile,
	*p_tempfile,
	*Recover,
	*Joverc,

#ifdef	PIPEPROCS
	*Portsrv,
	*Kbd_Proc,
#endif

#ifdef	MSDOS
	CmdDb[],
#else
	*CmdDb,
#endif

	TmpFilePath[],
	Shell[],
	ShFlags[];
