/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)local.h	3.12 (Berkeley) %G%
 */

/*
 * Things of local interest.
 */

#define RUNCOM		".windowrc"
#define ESCAPEC		ctrl('p')
#define NLINE		48			/* default text buffer size */

#ifdef TERMINFO
#define _PATH_CAPTOINFO	"/usr/5bin/captoinfo"
#define _PATH_TIC	"/usr/5bin/tic"
#define _PATH_RM	"/bin/rm"
#endif
