/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fnmatch.h	8.1 (Berkeley) %G%
 */

#ifndef	_FNMATCH_H_
#define	_FNMATCH_H_

#define	FNM_NOMATCH	1	/* Match failed. */

#define	FNM_NOESCAPE	0x01	/* Disable backslash escaping. */
#define	FNM_PATHNAME	0x02	/* Slash must be matched by slash. */
#define	FNM_PERIOD	0x04	/* Period must be matched by period. */

#include <sys/cdefs.h>

__BEGIN_DECLS
#ifndef	_POSIX_SOURCE
int	 fnmatch __P((const char *, const char *, int));
#endif
__END_DECLS

#endif /* !_FNMATCH_H_ */
