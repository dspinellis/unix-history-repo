/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chuck Karish of Mindcraft, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)utsname.h	8.1 (Berkeley) %G%
 */

#ifndef	_SYS_UTSNAME_H
#define	_SYS_UTSNAME_H

struct utsname {
	char	sysname[256];	/* Name of this OS. */
	char	nodename[256];	/* Name of this network node. */
	char	release[256];	/* Release level. */
	char	version[256];	/* Version level. */
	char	machine[256];	/* Hardware type. */
};

#include <sys/cdefs.h>

__BEGIN_DECLS
int	uname __P((struct utsname *));
__END_DECLS

#endif	/* !_SYS_UTSNAME_H */
