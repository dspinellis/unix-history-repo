/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ldexp.c	5.1 (Berkeley) 4/23/90";
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
