/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)isinf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

isnan(d)
	double d;
{
	register struct IEEEdp {
		u_int manl : 32;
		u_int manh : 20;
		u_int  exp : 11;
		u_int sign :  1;
	} *p = (struct IEEEdp *)&d;

	return(p->exp == 2047 && (p->manh || p->manl));
}

isinf(d)
	double d;
{
	register struct IEEEdp {
		u_int manl : 32;
		u_int manh : 20;
		u_int  exp : 11;
		u_int sign :  1;
	} *p = (struct IEEEdp *)&d;

	return(p->exp == 2047 && !p->manh && !p->manl);
}
