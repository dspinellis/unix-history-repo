/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)modf.s	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * modf(value, iptr): return fractional part of value, and stores the
 * integral part into iptr (a pointer to double).
 *
 * Written by Sean Eric Fagan (sef@kithrup.COM)
 * Sun Mar 11 20:27:30 PST 1990
 */

/* With CHOP mode on, frndint behaves as TRUNC does.  Useful. */
double
modf(double value, double *iptr)
{
	double temp;
	short i87flag, i87temp;
	__asm ("fnstcw %0" : "=g" (i87flag) : );
	i87temp = i87flag | 0xc00 ; /* turn on chop mode [truncation] */
	__asm ("fldcw %0" : : "g" (i87temp));
	__asm ("frndint" : "=f" (temp) : "0" (value)); /* temp = int of value */
	__asm ("fldcw %0" : : "g" (i87flag));
	*iptr = temp;
	return (value - temp);
}
