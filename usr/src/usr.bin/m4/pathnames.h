/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	5.3 (Berkeley) %G%
 */

/*
 * definitions of diversion files. If the name of
 * the file is changed, adjust UNIQUE to point to the
 * wildcard (*) character in the filename.
 */
#define	DIVNAM	"/tmp/m4*XXXXXX"	/* unix diversion files */
#define	UNIQUE	7			/* unique char location */
