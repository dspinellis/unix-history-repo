/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sh.local.h	5.4 (Berkeley) 2/15/89
 */

/*
 * This file defines certain local parameters
 * A symbol should be defined in Makefile for local conditional
 * compilation, e.g. IIASA or ERNIE, to be tested here and elsewhere.
 */

/*
 * Fundamental definitions which may vary from system to system.
 *
 *	BUFSIZ		The i/o buffering size; also limits word size
 *	MAILINTVL	How often to mailcheck; more often is more expensive
 */

#undef BUFSIZ
#define	BUFSIZ	1024		/* default buffer size */
#define FORKSLEEP	10	/* delay loop on non-interactive fork failure */
#define	MAILINTVL	600	/* 10 minutes */

/*
 * The shell moves std in/out/diag and the old std input away from units
 * 0, 1, and 2 so that it is easy to set up these standards for invoked
 * commands.
 */
#define	FSHTTY	15		/* /dev/tty when manip pgrps */
#define	FSHIN	16		/* Preferred desc for shell input */
#define	FSHOUT	17		/* ... shell output */
#define	FSHDIAG	18		/* ... shell diagnostics */
#define	FOLDSTD	19		/* ... old std input */

#if defined(vax) || defined(tahoe)
#define	copy(to, from, size)	bcopy(from, to, size)
#endif

#ifdef PROF
#define	exit(n)	done(n)
#endif
