/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)cfree.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

void
cfree(p)
	void *p;
{
	(void)free(p);
}
