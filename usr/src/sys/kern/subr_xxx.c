/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)subr_xxx.c	7.7 (Berkeley) %G%
 */

#include "errno.h"

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
	unsigned a, b;
{

	return (a < b ? a : b);
}

unsigned
max(a, b)
	unsigned a, b;
{

	return (a > b ? a : b);
}
#endif

#if !defined(vax) && !defined(tahoe) && !defined(hp300)
ffs(mask)
	register long mask;
{
	register int bit;

	if (!mask)
		return(0);
	for (bit = 1;; ++bit)
		if (mask&0x01)
			return(bit);
		mask >>= 1;
	}
}
#endif

#if !defined(vax) && !defined(hp300)
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
