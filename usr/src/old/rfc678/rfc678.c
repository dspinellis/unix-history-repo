char	*sccsid = "@(#)rfc678.c	4.1 (Berkeley) 4.1";

#include <stdio.h>

char linebuf[1024];
/*
 * rfc678
 */
main()
{
	register char *cp;
	int lineno = 0;

	while (gets(linebuf)) {
		for (cp = linebuf; *cp; cp++)
			if (*cp == 010) {
				strcpy(cp-1, cp+1);
				cp -= 2;
			}
		if (++lineno <= 59)
			printf("%s\n", linebuf);
		else if (lineno == 60)
			printf("%s\f\n", linebuf);
		lineno %= 66;
	}
}
