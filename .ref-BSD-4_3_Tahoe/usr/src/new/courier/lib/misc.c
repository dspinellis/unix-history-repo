#ifndef lint
static char sccsid[] = "@(#)misc.c	4.2 (Berkeley) 7/7/83";
#endif

#include <stdio.h>
#include "courier.h"

Unspecified *
Allocate(n)
	LongCardinal n;
{
	Unspecified *p;
	extern char *malloc();

	if (n > 100000) {
		fprintf(stderr,
			"Ridiculous request to memory allocator (%d words).\n",
			n);
		exit(1);
	}
	if (n == 0)
		return (0);
	p = (Unspecified *) malloc(n*sizeof(Unspecified));
	if (p == 0) {
		fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	return (p);
}

Deallocate(p)
	Unspecified *p;
{
	if (p != 0)
		free((char *) p);
}

PackString(p, buf, flag)
	String *p;
	Unspecified *buf;
	Boolean flag;
{
	Cardinal n;

	n = strlen(*p);
	if (flag) {
		PackCardinal(&n, buf, 1);
		strncpy(buf+1, *p, n);
	}
	return (1 + n/2 + n%2);
}

UnpackString(p, buf)
	String *p;
	Unspecified *buf;
{
	Cardinal n;

	UnpackCardinal(&n, buf);
	*p = (String) Allocate(n/2 + 1);
	buf++;
	strncpy(*p, buf, n);
	(*p)[n] = 0;
	return (1 + n/2 + n%2);
}
