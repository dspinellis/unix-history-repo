/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deliv2.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

hash (s)
char *s;
{
	int c, n;
	for(n=0; c= *s; s++)
		n += (c*n+ c << (n%4));
	return(n>0 ? n : -n);
}

err (s, a)
char *s;
{
	fprintf(stderr, "Error: ");
	fprintf(stderr, s, a);
	putc('\n', stderr);
	exit(1);
}

prefix(t, s)
char *t, *s;
{
	int c;

	while ((c= *t++) == *s++)
		if (c==0) return(1);
	return(c==0 ? 1: 0);
}

char *
mindex(s, c)
char *s;
{
	register char *p;
	for( p=s; *p; p++)
		if (*p ==c)
			return(p);
	return(0);
}

zalloc(m,n)
{
	char *calloc();
	int t;
# if D1
	fprintf(stderr, "calling calloc for %d*%d bytes\n",m,n);
# endif
	t = (int) calloc(m,n);
# if D1
	fprintf(stderr, "calloc returned %o\n", t);
# endif
	return(t);
}
