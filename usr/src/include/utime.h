/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)utime.h	5.2 (Berkeley) %G%
 */

struct utimbuf {
	time_t actime;		/* Access time */
	time_t modtime;		/* Modification time */
};

#include <sys/cdefs.h>

__BEGIN_DECLS
int utime __P((char *, struct utimbuf *));
__END_DECLS
