#include "ex.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June/August 1977
 */

STATIC	int jcount, jnoop();

join(c)
	char c;
{
	register *a1;
	register char *cp, *cp1;

	cp = genbuf;
	*cp = 0;
	for (a1 = addr1; a1 <= addr2; a1++) {
		getline(*a1);
		cp1 = linebuf;
		if (a1 != addr1 && c == 0) {
			while (*cp1 == ' ' || *cp1 == '\t')
				cp1++;
			if (*cp1 && cp > genbuf && cp[-1] != ' ' && cp[-1] != '\t')
				*cp++ = ' ';
		}
		while (*cp++ = *cp1++)
			if (cp > &genbuf[LBSIZE-2])
				error("Line overflow|Result line of join would be longer than 510 characters");
		cp--;
	}
	cp = genbuf;
	cp1 = linebuf;
	while (*cp1++ = *cp++)
		continue;
	delete();
	jcount = 1;
	append(jnoop, --addr1);
}

STATIC int
jnoop()
{
	return(--jcount);
}
