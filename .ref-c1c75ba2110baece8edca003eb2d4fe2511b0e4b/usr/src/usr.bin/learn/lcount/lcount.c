#ifndef lint
static char sccsid[] = "@(#)lcount.c	4.2	(Berkeley)	%G%";
#endif not lint

#include "stdio.h"

main()	/* count lines in something */
{
	register n, c;

	n = 0;
	while ((c = getchar()) != EOF)
		if (c == '\n')
			n++;
	printf("%d\n", n);
}
