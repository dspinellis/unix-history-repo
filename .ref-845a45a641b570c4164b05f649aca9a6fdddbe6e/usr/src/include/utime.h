/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)utime.h	8.1 (Berkeley) %G%
 */

#ifndef	_UTIME_H_
#define	_UTIME_H_

struct utimbuf {
	time_t actime;		/* Access time */
	time_t modtime;		/* Modification time */
};

#include <sys/cdefs.h>

__BEGIN_DECLS
int utime __P((const char *, const struct utimbuf *));
__END_DECLS

#endif /* !_UTIME_H_ */
