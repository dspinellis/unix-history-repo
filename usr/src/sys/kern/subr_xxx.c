/*
 * Copyright (c) 1982, 1986, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)subr_xxx.c	7.14 (Berkeley) %G%
 */

/*
 * Miscellaneous trivial functions, including many
 * that are often inline-expanded or done in assembler.
 */
#include <sys/param.h>
#include <sys/systm.h>

#include <machine/cpu.h>

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
