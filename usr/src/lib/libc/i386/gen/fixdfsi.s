/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fixdfsi.s	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

__fixdfsi(d)
double d;
{
	int val;
	asm("fistl %0" : "=g" (val) : "f" (d) );
	return(val);
}
