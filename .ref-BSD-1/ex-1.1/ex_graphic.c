#include "ex.h"
#include "ex_tty.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

dographic()
{
	register char *lp;
	register c;

	for (lp = linebuf; c = *lp++;) {
		switch (c) {
			case '\b':
				if (!OS)
					c = '?';
				break;
			default:
				if (c < ' ' || c == 0177)
					c = '?';
				else if (UPPERCASE && ucletter(c)) {
					putchar('\\');
					c = letter(c);
				}
				break;
			case '\t':
				break;
		}
		putchar(c);
	}
}

doulg()
{
	register char *lp, *gp;
	char *maxgp;
	register c;
	char csw;
	int col;

	gp = genbuf;
	*gp = 0;
	maxgp = gp;
	col = 0;
	for (lp = linebuf; c = *lp++;) {
		switch (c) {
			case '\t':
				while ((col & 7) != 7) {
					*gp++ = ' ';
					if (gp >= &genbuf[LBSIZE - 2])
						goto ovflo;
					col++;
				}
				break;
			default:
				if (gp >= maxgp)
					break;
				c =| (*gp & QUOTE);
				break;
			case '_':
				if (gp >= maxgp)
					c = QUOTE;
				else
					c = *gp | QUOTE;
				break;
			case '\b':
				if (gp > genbuf) {
					gp--;
					col--;
				}
				continue;
		}
		if (gp >= &genbuf[LBSIZE - 2])
ovflo:
			error("Line too long with iu|Line too long to print with indicateul");
		*gp++ = c;
		if (gp > maxgp)
			maxgp = gp;
		col++;
	}
	*maxgp = 0;
	strcLIN(genbuf);
	for (lp = linebuf, gp = genbuf; c = *lp; gp++, lp++)
		if (c & QUOTE) {
			c =& 0177;
			if (c == 0)
				*lp = '_', *gp = ' ';
			else
				*lp = c, *gp = '-';
		} else
			*gp = ' ';
	--gp;
	while (gp >= genbuf && *gp == ' ')
		--gp;
	gp[1] = 0;
}
