/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)TIME.c 1.1 10/30/80";

extern char *ctime();

TIME(alfap)

	register char *alfap;
{
	register char *ap, *cp;
	register int i;
	long a;

	time(&a);
	cp = ctime(&a);
	ap = alfap;
	for (cp = cp + 10, i = 10; i; *ap++ = *cp++, i--);
}
