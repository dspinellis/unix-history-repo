#include "ex.h"
#include "ex_tty.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 *
 * Check linebuf overflow more carefully.
 * Think about when ai allowed more.
 */


static	junkbs;

int	peekc;
int	lastc	'\n';

getchar()
{
	register c;

	c = peekc;
	if (c != 0) {
		peekc = 0;
		return (c);
	}
	if (globp) {
		if (*globp)
			return (*globp++);
		globp = 0;
		lastc = EOF;
		return (EOF);
	}
again:
	if (read(0, &lastc, 1) == 1) {
		lastc =& 0177;
		if (junk(lastc)) {
			checkjunk(lastc);
			goto again;
		}
		if (lastc == '\n')
			noteinp();
		return (lastc);
	}
	lastc = EOF;
	return (EOF);
}

peekchar()
{

	if (peekc == 0)
		peekc = getchar();
	return (peekc);
}

STATIC	int	lastin;
extern	char	aiflag;

gettty()
{
	register c;
	register char *lp, *gp;
	char hadup, hadnl;
	int offset, numbline();
	extern int (*Pline)();

	lp = linebuf;
	hadnl = 0;
	offset = Pline == &numbline ? 8 : 0;
	if (intty && !inglobal) {
		if (offset)
			printf("\240\240%4d", dot - zero + 1);
		if (value(AUTOINDENT) ^ (aiflag != 0)) {
			setcol(lastin + offset);
			while ((c = read(0, &genbuf, 512)) == 0) {
				if (lastin == 0 && gTTY(0) == -1)
					return (EOF);
				lastin = backtab(lastin);
				setcol(lastin + offset);
			}
			hadup = 0;
			if ((genbuf[0] == '^' || genbuf[0] == '0') && c == 1) {
				if (genbuf[0] == '0')
					lastin = 0;
				if (!OS) {
					notech(1);
					putchar('\b');
					putchar(' ' | QUOTE);
					putchar('\b');
				}
				setcol(offset);
				hadup = 1;
				genbuf[0] = 0;
				c = 0;
			}
			if (c > 0) {
				c--;
				if (genbuf[c] == '\n') {
					if (c == 1 && genbuf[0] == '.') {
						noteinp();
						return(EOF);
					}
					hadnl++;
				} else
					c++;
				genbuf[c] = 0;
				spruce(genbuf);
				if (hadup == 0 && genbuf[0]) {
					lastin = smunch(lastin, genbuf);
					for (c = lastin; c >= 7; c =- 8)
						*lp++ = '\t';
					for (; c > 0; c--)
						*lp++ = ' ';
				}
				gp = genbuf;
				while (*lp++ = *gp++)
					continue;
				lp--;
			}
		} else if (offset) {
			printf("  ");
			flush();
		}
		while (hadnl == 0) {
/*
			if (lp > linebuf) {
				partinp();
				spruce(linebuf);
				dographic();
				lp = linebuf;
				while (*lp)
					lp++;
				flush();
			}
*/
			c = read(0, lp, linebuf + LBSIZE - lp);
			if (c <= 0 && lp == linebuf)
				return (EOF);
			lp =+ c;
			*lp = 0;
			if (lp >= &linebuf[LBSIZE - 2])
				goto toolong;
			if (lp[-1] == '\n') {
				*--lp = 0;
				hadnl = 1;
			}
		}
		noteinp();
	} else {
		while ((c = getchar()) != '\n') {
			if (c == EOF) {
				if (inglobal)
					ungetchar(EOF);
				return (EOF);
			}
			c =& 0177;
			*lp++ = c;
			if (lp >= &linebuf[LBSIZE-2])
toolong:
				error("Input line too long@- limit 510 characters");
		}
	}
	*lp++ = 0;
	spruce(linebuf);
	if (linebuf[0] == '.' && linebuf[1] == 0)
		return (EOF);
	return (0);
}

backtab(i)
	register int i;
{
	register int j;

	j = i % value(SHIFTWIDTH);
	if (j == 0)
		j = value(SHIFTWIDTH);
	i =- j;
	if (i < 0)
		i = 0;
	return (i);
}

smunch(col, ocp)
	register int col;
	char *ocp;
{
	register char *cp;

	cp = ocp;
	for (;;)
		switch (*cp++) {
			case ' ':
				col++;
				continue;
			case '\t':
				col = (col + 8) &~ 7;
				continue;
			default:
				cp--;
				strcpy(ocp, cp);
				return (col);
		}
}

spruce(ocp)
	register char *ocp;
{
	register char *cp;
	register c;

	cp = ocp;
	while (c = *ocp++) {
		c =& 0177;
		if (junk(c)) {
			checkjunk(c);
			continue;
		}
		*cp++ = c;
	}
	*cp++ = 0;
}

char	cntrlhm[]	"CTRL(H) discarded\n";

checkjunk(c)
	char c;
{

	if (junkbs == 0 && c == '\b') {
		write(2, cntrlhm, sizeof cntrlhm);
		junkbs = 1;
	}
}

setin(addr)
	int *addr;
{

	if (addr == zero)
		lastin = 0;
	else
		lastin = smunch(0, getline(*addr));
	return (addr);
}

junk(c)
	register int c;
{
	return (c == 0 || value(BEAUTIFY) && c < ' ' && c != '\t' && c != '\n' && c != '\f');
}
