#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September, 1977
 */

/*
 * Clear the entire (physical and logical) screen
 */
vclear()
{
	register int i, j;

	vputs(CLEAR);
	destcol = 0;
	outcol = 0;
	destline = 0;
	outline = 0;
	vclrbyte(vtube0, LINES * COLUMNS);
}

/*
 * Put out a control sequence to the terminal.
 */
vputs(cp)
	register char *cp;
{

	while (*cp)
		vputc(*cp++);
}

/*
 * Clear bytes logically
 */
vclrbyte(cp, i)
	register char *cp;
	register int i;
{

	if (i > 0)
		do
			*cp++ = 0;
		while (--i != 0);
}

/*
 * Clear a physical line
 */
vclrlin(l, tp)
	int l, *tp;
{

	vigoto(l, 0);
	if (!holdat)
		putchar(tp > dol ? '~' : '@');
	vclreol();
}

/*
 * Clear to the end of the current physical line
 */
vclreol()
{
	register int i;
	register char *tp;

	if (destcol == VCOLUMNS)
		return;
	destline =+ destcol / VCOLUMNS;
	destcol =% VCOLUMNS;
#ifdef DEBUG
	if (destline < 0 || destline > TUBELINES)
		error("Internal error: vclreol@- please tell someone");
#endif
	i = VCOLUMNS - destcol;
	tp = vtube[destline] + destcol;
	if (*tp == 0)
		return;
	if (CE) {
		vcsync();
		vputs(CE);
		vclrbyte(tp, i);
		return;
	}
	while (i > 0 && *tp != 0) {
		vputchar(' ');
		--i, tp++;
	}
}
#endif
