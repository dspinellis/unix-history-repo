/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ranlib.h	8.1 (Berkeley) %G%
 */

#ifndef _RANLIB_H_
#define	_RANLIB_H_

#define	RANLIBMAG	"__.SYMDEF"	/* archive file name */
#define	RANLIBSKEW	3		/* creation time offset */

struct ranlib {
	union {
		long ran_strx;		/* string table index */
		char *ran_name;		/* in memory symbol name */
	} ran_un;
	long ran_off;			/* archive file offset */
};

#endif /* !_RANLIB_H_ */
