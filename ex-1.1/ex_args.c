#
/*
 * Ex - a text editor
 * Version 1.0 September, 1977
 */
#include "ex.h"

getargs()
{
	register c;
	register char *cp, *fp;
	int *dp, *gp;

	cp = linebuf, dp = genbuf;
	for (;;) {
		c = getchar();
		if (endcmd(c))
			break;
		if (white(c))
			continue;
		*dp = cp;
		do {
			switch (c) {
				case '\\':
					c = getchar() | 0200;
				default:
					if (cp > &linebuf[LBSIZE - 2])
flong:
						error("Argument buffer overflow@- limit 510 characters");
					*cp++ = c;
					break;
				case '`':
					fp = altfile;
					if (*fp == 0)
						error("No alternate filename@to substitute for `");
					goto filexp;
				case '%':
					fp = savedfile;
					if (*fp == 0)
						error("No current filename@to substitute for %%");
filexp:
					while (*fp) {
						if (cp > &linebuf[LBSIZE - 2])
							goto flong;
						*cp++ = *fp++;
					}
					break;
			}
			c = getchar();
		} while (!white(c) && !endcmd(c));
		ungetchar(c);
		*cp++ = 0;
		dp++;
	}
	*dp = 0;
	return (dp - (gp = genbuf));
}
