/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ranlib.h	5.2 (Berkeley) %G%
 */

#ifndef _RANLIB_H_
#define	_RANLIB_H_

#define	RANLIBMAG	"__.SYMDEF"	/* archive file name */
#define	RANLIBSKEW	3		/* creation time offset */

struct ranlib {
	union {
		off_t ran_strx;		/* string table index */
		char *ran_name;		/* in memory symbol name */
	} ran_un;
	off_t ran_off;			/* archive file offset */
};

#endif /* !_RANLIB_H_ */
