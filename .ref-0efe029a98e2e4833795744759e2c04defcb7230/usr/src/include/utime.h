/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)utime.h	5.1 (Berkeley) %G%
 */

struct utimbuf {
	time_t actime;		/* Access time */
	time_t modtime;		/* Modification time */
};

#if __STDC__ || c_plusplus
int utime(char *, struct utimbuf *);
#else
int utime();
#endif
