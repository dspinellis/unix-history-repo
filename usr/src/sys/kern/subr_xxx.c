/*
 * Copyright (c) 1982, 1986, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)subr_xxx.c	7.10 (Berkeley) %G%
 */

/*
 * Miscellaneous trivial functions, including many
 * that are often inline-expanded or done in assembler.
 */
#include "param.h"
#include "systm.h"
#include "machine/cpu.h"

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
 * Unsupported ioctl function.
 */
enoioctl()
{

	return (ENOTTY);
}

/*
 * Unsupported system function.
 * This is used for an otherwise-reasonable operation
 * that is not supported by the current system binary.
 */
enosys()
{

	return (ENOSYS);
}

/*
 * Return error for operation not supported
 * on a specific object or file type.
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
#ifdef NEED_MINMAX
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
#endif /* NEED_MINMAX */

#ifdef NEED_FFS
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
#endif /* NEED_FFS */

#ifdef NEED_BCMP
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
#endif /* NEED_BCMP */

#ifdef NEED_STRLEN
strlen(s1)
	register char *s1;
{
	register int len;

	for (len = 0; *s1++ != '\0'; len++)
		;
	return (len);
}
#endif /* NEED_STRLEN */
