/* Copyright (c) 1979 Regents of the University of California */
/*
 * This file defines certain local parameters
 * A symbol should be defined in Makefile for conditional
 * compilation, e.g. CORY for U.C.B. Cory Hall 11/70 and
 * tested here and elsewhere.
 */

/*
 * Fundamental definitions which may vary from system to system.
 *
 *	BUFSIZ		The i/o buffering size; also limits word size
 *	SHELLPATH	Where the shell will live; initalizes $shell
 *	SRCHPATH	The directories in the default search path
 *	MAILINTVL	How often to mailcheck; more often is more expensive
 */

#include <pagsiz.h>
#define	BUFSIZ		BSIZE
#define	SHELLPATH	"/bin/csh"
#define	OTHERSH		"/bin/sh"
/*
 * Note that the first component of SRCHPATH is set to /etc for root
 * in the file sh.c.
 */
#define	SRCHPATH	".", "/usr/ucb", "/bin", "/usr/bin"
#define	MAILINTVL	300				/* 5 minutes */

/*
 * NCARGS and NOFILE are from <sys/param.h> which we choose not
 * to wholly include
 */
#define	NCARGS	5120		/* Max. chars in an argument list */

/*
 * The shell moves std in/out/diag and the old std input away from units
 * 0, 1, and 2 so that it is easy to set up these standards for invoked
 * commands.  If possible they should go into descriptors closed by exec.
 */
#define	NOFILE	20		/* Max number of open files */
#define	FSHIN	16		/* Preferred desc for shell input */
#define	FSHOUT	17		/* ... shell output */
#define	FSHDIAG	18		/* ... shell diagnostics */
#define	FOLDSTD	19		/* ... old std input */

#define	V7

#ifdef	NORMAL6
#undef	V7
#define V6
#include <retrofit.h>
#define	NCARGS	510
#define	NOFILE	15
#define	FSHIN	3
#define	FSHOUT	4
#define	FSHDIAG	5
#define	FOLDSTD	6
#endif
