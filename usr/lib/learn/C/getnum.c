#ifndef lint
static char sccsid[] = "@(#)getnum.c	1.1	(Berkeley)	6/8/88";
#endif not lint

#include <stdio.h>

getnum()
{
	int c, n;

	n = 0;
	while ((c=getchar()) >= '0' && c <= '9')
		n = n*10 + c - '0';
	if (c == EOF)
		return(-1);
	return(n);
}
