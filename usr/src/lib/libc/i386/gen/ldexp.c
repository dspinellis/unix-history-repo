/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ldexp.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * ldexp(value, exp): return value * (2 ** exp).
 *
 * Written by Sean Eric Fagan (sef@kithrup.COM)
 * Sun Mar 11 20:27:09 PST 1990
 */

/*
 * We do the conversion in C to let gcc optimize it away, if possible.
 * The "fxch ; fstp" stuff is because value is still on the stack
 * (stupid 8087!).
 */
double
ldexp (double value, int exp)
{
	double temp, texp, temp2;
	texp = exp;
	asm ("fscale ; fxch %%st(1) ; fstp%L1 %1 "
		: "=f" (temp), "=0" (temp2)
		: "0" (texp), "f" (value));
	return (temp);
}
