#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB October, 1977
 */

/*
 * Column returns the number of
 * columns occupied by printing the
 * characters through position cp of the
 * current line.
 */
column(cp)
	register char *cp;
{

	if (cp == 0)
		cp = &linebuf[LBSIZE - 2];
	return (qcolumn(cp, 0));
}

qcolumn(lim, gp)
	register char *lim, *gp;
{
	register int x;
	int (*OO)();

	OO = Outchar;
	Outchar = &qcount;
	vcntcol = 0;
	if (lim != NIL)
		x = lim[1], lim[1] = 0;
	pline(0);
	if (lim != NIL)
		lim[1] = x;
	if (gp)
		while (*gp)
			putchar(*gp++);
	Outchar = OO;
	return (vcntcol);
}

qcount(c)
	char c;
{

	switch (c) {
		case '\t':
			vcntcol =+ 8;
			vcntcol =& ~07;
			break;
		default:
			vcntcol++;
	}
}
