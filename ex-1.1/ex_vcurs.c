#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September, 1977
 */

/*
 * Fix the cursor to be positioned in the correct place
 */
vfixcurs()
{

	vsetcurs(cursor);
}

/*
 * Set the cursor at nc
 */
vsetcurs(nc)
	register char *nc;
{
	register int col;

	col = column(nc);
	if (linebuf[0])
		col--;
	vgotoCL(col);
	cursor = nc;
}

/*
 * Invisible goto
 */
vigoto(y, x)
	int y, x;
{

	if (y < 0)
		error("Internal error: vigoto@- please tell someone");
	destline = y;
	destcol = x;
}

/*
 * Sync the cursor... i.e. make the current invisible position
 * the current actual physical position.
 */
vcsync()
{

	vgoto(destline, destcol);
}

/*
 * Goto output position y on line x.
 * X may be greater than VCOLUMNS and is folded here
 * the screen is rolled up if need be.
 */
vgotoCL(x)
{

	vgoto(vliny[vcline], x);
}

vgoto(y, x)
	int y, x;
{
	register char *tp;

	if (y < 0)
		error("Internal error: vgoto@- please tell someone");
	/*
	 * Fold the possibly too large value of x.
	 */
	if (x >= COLUMNS) {
		y =+ x / COLUMNS;
		x =% COLUMNS;
	}
	if (outcol >= COLUMNS) {
		outline =+ outcol / COLUMNS;
		outcol =% COLUMNS;
	}
	if (!CA) {
		/*
		 * We are doing a one line open on a terminal
		 * without cursor addressing.
		 */
		if (y != outline)
			error("Line too long@for single line open");
		if (x + 1 < outcol - x || (outcol > x && !BS))
			vputc('\r'), outcol = 0;
		tp = vtube[LINES - 1] + outcol;
		while (outcol != x)
			if (outcol < x) {
				if (*tp == 0)
					*tp = ' ';
				vputc(*tp++), outcol++;
			} else
				vputc('\b'), outcol--;
		destcol = outcol = x;
		return;
	}
	if (y >= LINES - 1 && (y >= LINES || !splitw))
		vrollup(&y);
	destline = y;
	destcol = x;
	fgoto();
}

/*
 * Process the character c onto the screen at the current position.
 */
vputchar(c)
	int c;
{
	register char *tp;
	int y;

	if (destcol >= VCOLUMNS && !visual && !CA)
		error("Line too long@for single line open");
	if (destcol >= COLUMNS) {
		destline =+ destcol / COLUMNS;
		destcol =% VCOLUMNS;
	}
	if (destline > VLINES) {
		if (splitw) {
			if (destline > LINES || destcol >= VCOLUMNS) {
				beep();
				return;
			}
			goto ok;
		}
		y = destline;
		vrollup(&y);
	}
ok:
	tp = vtube[destline] + destcol;
	switch (c) {
		case '\t':
			do
				vputchar(' ');
			while (destcol & 07);
			return;
		case ' ':
			if (*tp == 0) {
				*tp = ' ';
				destcol++;
				return;
			}
		default:
			if (c < ' ' || c == 0177)
				c = '?';
			if (*tp == c) {
				destcol++;
				return;
			}
			if (destline != outline || destcol != outcol) {
				vcsync();
				tp = vtube[destline] + destcol;
			}
			*tp = c;
			vputc(c);
			destcol++, outcol++;
			return;
	}
}
#endif
