/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)subr_xxx.c	7.4 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "conf.h"
#include "inode.h"
#include "dir.h"
#include "user.h"
#include "buf.h"
#include "proc.h"
#include "fs.h"
#include "vm.h"
#include "cmap.h"
#include "uio.h"

/*
 * Routine placed in illegal entries in the bdevsw and cdevsw tables.
 */
nodev()
{

	return (ENODEV);
}

/*
 * Null routine; placed in insignificant entries
 * in the bdevsw and cdevsw tables.
 */
nulldev()
{

	return (0);
}

/*
 * Definitions of various trivial functions;
 * usually expanded inline rather than being defined here.
 */
#if !defined(vax) && !defined(tahoe)
imin(a, b)
{

	return (a < b ? a : b);
}

imax(a, b)
{

	return (a > b ? a : b);
}

unsigned
min(a, b)
	u_int a, b;
{

	return (a < b ? a : b);
}

unsigned
max(a, b)
	u_int a, b;
{

	return (a > b ? a : b);
}
#endif

#if !defined(vax) && !defined(tahoe)
ffs(mask)
	register long mask;
{
	register int i;

	for(i = 1; i < NSIG; i++) {
		if (mask & 1)
			return (i);
		mask >>= 1;
	}
	return (0);
}
#endif

#if !defined(vax)
bcmp(s1, s2, len)
	register char *s1, *s2;
	register unsigned len;
{

	while (len--)
		if (*s1++ != *s2++)
			return (1);
	return (0);
}

strlen(s1)
	register char *s1;
{
	register int len;

	for (len = 0; *s1++ != '\0'; len++)
		/* void */;
	return (len);
}
#endif
