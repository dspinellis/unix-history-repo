/*
 * Copyright (c) 1982, 1986, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)subr_xxx.c	7.9 (Berkeley) %G%
 */

/*
 * Miscellaneous trivial functions, including many
 * that are often inline-expanded or done in assembler.
 */
#include "types.h"
#include "machine/cpu.h"
#include "errno.h"

/*
 * Unsupported device function (e.g. writing to read-only device).
 */
enodev()
{

	return (ENODEV);
}

/*
 * Unconfigured device function; driver not configured.
 */
enxio()
{

	return (ENXIO);
}

/*
 * Return error for unsupported operation.
 */
eopnotsupp()
{

	return (EOPNOTSUPP);
}

/*
 * Generic null operation, always returns success.
 */
nullop()
{

	return (0);
}

/*
 * Definitions of various trivial functions;
 * usually expanded inline rather than being defined here.
 */
#if !defined(vax) && !defined(tahoe)
imin(a, b)
	int a, b;
{

	return (a < b ? a : b);
}

imax(a, b)
	int a, b;
{

	return (a > b ? a : b);
}

unsigned int
min(a, b)
	unsigned int a, b;
{

	return (a < b ? a : b);
}

unsigned int
max(a, b)
	unsigned int a, b;
{

	return (a > b ? a : b);
}

long
lmin(a, b)
	long a, b;
{

	return (a < b ? a : b);
}

long
lmax(a, b)
	long a, b;
{

	return (a > b ? a : b);
}

unsigned long
ulmin(a, b)
	unsigned long a, b;
{

	return (a < b ? a : b);
}

unsigned long
ulmax(a, b)
	unsigned long a, b;
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
	for (bit = 1;; ++bit) {
		if (mask&0x01)
			return(bit);
		mask >>= 1;
	}
}
#endif

#if !defined(vax) && !defined(hp300)
bcmp(v1, v2, len)
	void *v1, *v2;
	register unsigned len;
{
	register u_char *s1 = v1, *s2 = v2;

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
		;
	return (len);
}
#endif
