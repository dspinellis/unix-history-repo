/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)shell.h	5.3 (Berkeley) %G%
 */

/*
 * The follow should be set to reflect the type of system you have:
 *	JOBS -> 1 if you have Berkeley job control, 0 otherwise.
 *	SYMLINKS -> 1 if your system includes symbolic links, 0 otherwise.
 *	DIRENT -> 1 if your system has the SVR3 directory(3X) routines.
 *	UDIR -> 1 if you want the shell to simulate the /u directory.
 *	SHORTNAMES -> 1 if your linker cannot handle long names.
 *	define BSD if you are running 4.2 BSD or later.
 *	define SYSV if you are running under System V.
 *	define DEBUG to turn on debugging.
 *
 * When debugging is on, debugging info will be written to $HOME/trace and
 * a quit signal will generate a core dump.
 */

#include <sys/cdefs.h>

#define JOBS 1
#define SYMLINKS 1
#define DIRENT 1
#define UDIR 0
#define ATTY 0
#define SHORTNAMES 0
#define BSD
/* #define SYSV */
/* #define DEBUG */

typedef void *pointer;

#ifdef __STDC__
#ifndef NULL
#define NULL (void *)0
#endif
#else /* not __STDC__ */
#ifndef NULL
#define NULL 0
#endif
#endif /* __STDC__ */
#define STATIC	/* empty */
#define MKINIT	/* empty */

extern char nullstr[1];		/* null string */


#ifdef DEBUG
#define TRACE(param)	trace param
#else
#define TRACE(param)
#endif
