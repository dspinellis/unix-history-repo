/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)bcopy.c	7.4 (Berkeley) %G%";
#endif not lint

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bcopy.c	7.4 (Berkeley) %G%";
#endif not lint

/* block copy from from to to, count bytes */
bcopy(from, to, count)
#ifdef vax
	char *from, *to;
	int count;
{
#ifndef vms
	asm("	movc3	12(ap),*4(ap),*8(ap)");
	/* ARGSUSED */
#else
	lib$movc3(&count, from, to);
#endif
}
#else
	register char *from, *to;
	register int count;
{
	while ((count--) > 0)	/* mjm */
		*to++ = *from++;
}
#endif
