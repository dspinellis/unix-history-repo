# include	"curses.ext"
# include	"cr_ex.h"

# define	HARDTABS	8

extern char	*tgoto();
int		plodput();

/*
 * Terminal driving and line formatting routines.
 * Basic motion optimizations are done here as well
 * as formatting of lines (printing of control characters,
 * line numbering and the like).
 *
 * 1/26/81 (Berkeley) @(#)cr_put.c	1.1
 */

/*
 * Sync the position of the output cursor.
 * Most work here is rounding for terminal boundaries getting the
 * column position implied by wraparound or the lack thereof and
 * rolling up the screen to get destline on the screen.
 */

static int	outcol, outline, destcol, destline, plodcnt;

WINDOW		*_win;

mvcur(ly, lx, y, x)
int	ly, lx, y, x; {

#ifdef DEBUG
	fprintf(outf, "MVCUR: moving cursor from (%d,%d) to (%d,%d)\n", ly, lx, y, x);
#endif
	destcol = x;
	destline = y;
	outcol = lx;
	outline = ly;
	fgoto();
}

fgoto() {

	reg char	*cgp;
	reg int		l, c;

	if (destcol > COLS - 1) {
		destline += destcol / COLS;
		destcol %= COLS;
	}
	if (outcol > COLS - 1) {
		l = (outcol + 1) / COLS;
		outline += l;
		outcol %= COLS;
		if (AM == 0) {
			while (l > 0) {
				putchar('\n');
				if (_pfast)
					putchar('\r');
				l--;
			}
			outcol = 0;
		}
		if (outline > LINES - 1) {
			destline -= outline - (LINES - 1);
			outline = LINES - 1;
		}
	}
	if (destline > LINES - 1) {
		l = destline;
		destline = LINES - 1;
		if (outline < LINES - 1) {
			c = destcol;
			if (_pfast == 0 && !CA)
				destcol = 0;
			fgoto();
			destcol = c;
		}
		while (l > LINES - 1) {
			putchar('\n');
			l--;
			if (_pfast == 0)
				outcol = 0;
		}
	}
	if (destline < outline && !(CA || UP != NULL))
		destline = outline;
	cgp = tgoto(CM, destcol, destline);
	if (CA)
		if (plod(strlen(cgp)) > 0)
			plod(0);
		else
			tputs(cgp, 0, _putchar);
	else
		plod(0);
	outline = destline;
	outcol = destcol;
}

char
_putchar(c)
reg char	c; {

	putchar(c);
#ifdef DEBUG
	fprintf(outf, "_PUTCHAR(%s)\n", unctrl(c));
#endif
}

extern bool	plodflg;
extern int	plodcnt;

plod(cnt)
int	cnt; {

	reg int		i, j, k;
	reg int		soutcol, soutline;
	reg char	c;

	plodcnt = plodflg = cnt;
	soutcol = outcol;
	soutline = outline;
	if (HO) {
		if (GT)
			i = (destcol / HARDTABS) + (destcol % HARDTABS);
		else
			i = destcol;
        if (destcol >= outcol) {
                j = destcol / HARDTABS - outcol / HARDTABS;
                if (GT && j)
                        j += destcol % HARDTABS;
		else
			j = destcol - outcol;
        } else
			if (outcol - destcol <= i && (BS || BC))
				i = j = outcol - destcol;
			else
				j = i + 1;
		k = outline - destline;
		if (k < 0)
			k = -k;
		j += k;
		if (i + destline < j) {
			tputs(HO, 0, plodput);
			outcol = outline = 0;
		} else if (LL) {
			k = (LINES - 1) - destline;
			if (i + k + 2 < j) {
				tputs(LL, 0, plodput);
				outcol = 0;
				outline = LINES - 1;
			}
		}
	}
	if (GT)
        i = destcol % HARDTABS + destcol / HARDTABS;
	else
		i = destcol;
/*
	if (BT && outcol > destcol && (j = (((outcol+7) & ~7) - destcol - 1) >> 3)) {
		j *= (k = strlen(BT));
		if ((k += (destcol&7)) > 4)
			j += 8 - (destcol&7);
		else
			j += k;
	} else
*/
		j = outcol - destcol;
	/*
	 * If we will later need a \n which will turn into a \r\n by
	 * the system or the terminal, then don't bother to try to \r.
	 */
	if ((NONL || !_pfast) && outline < destline)
		goto dontcr;
	/*
	 * If the terminal will do a \r\n and there isn't room for it,
	 * then we can't afford a \r.
	 */
	if (NC && outline >= destline)
		goto dontcr;
	/*
	 * If it will be cheaper, or if we can't back up, then send
	 * a return preliminarily.
	 */
	if (j > i + 1 || outcol > destcol && !BS && !BC) {
		plodput('\r');
		if (NC) {
			plodput('\n');
			outline++;
		}
		outcol = 0;
	}
dontcr:
	while (outline < destline) {
		outline++;
		plodput('\n');
		if (plodcnt < 0)
			goto out;
		if (NONL || _pfast == 0)
			outcol = 0;
	}
	if (BT)
		k = strlen(BT);
	while (outcol > destcol) {
		if (plodcnt < 0)
			goto out;
/*
		if (BT && outcol - destcol > 4+k) {
			tputs(BT, 0, plodput);
			outcol--;
			outcol &= ~7;
			continue;
		}
*/
		outcol--;
		if (BC)
			tputs(BC, 0, plodput);
		else
			plodput('\b');
	}
	while (outline > destline) {
		outline--;
		tputs(UP, 0, plodput);
		if (plodcnt < 0)
			goto out;
	}
	if (GT && destcol - outcol > 1) {
        for (;;) {
                i = (outcol / HARDTABS + 1) * HARDTABS;
                if (i > destcol)
                        break;
			if (TA)
				tputs(TA, 0, plodput);
			else
				plodput('\t');
			outcol = i;
		}
		if (destcol - outcol > 4 && i < COLS && (BC || BS)) {
			if (TA)
				tputs(TA, 0, plodput);
			else
				plodput('\t');
			outcol = i;
			while (outcol > destcol) {
				outcol--;
				if (BC)
					tputs(BC, 0, plodput);
				else
					plodput('\b');
			}
		}
	}
	while (outcol < destcol) {
		if (_win != NULL)
			if (plodflg)	/* avoid a complex calculation */
				plodcnt--;
			else {
				c = _win->_y[outline-_win->_begy][outcol-_win->_begx];
				if ((c&_STANDOUT) == (curscr->_flags&_STANDOUT))
					putchar(c);
				else
					goto nondes;
			}
		else
nondes:
		     if (ND)
			tputs(ND, 0, plodput);
		else
			plodput(' ');
		outcol++;
		if (plodcnt < 0)
			goto out;
	}
out:
	if (plodflg) {
		outcol = soutcol;
		outline = soutline;
	}
	return(plodcnt);
}

/*
 * Move (slowly) to destination.
 * Hard thing here is using home cursor on really deficient terminals.
 * Otherwise just use cursor motions, hacking use of tabs and overtabbing
 * and backspace.
 */

static	bool	plodflg;

plodput(c)
reg char	c; {

	if (plodflg)
		plodcnt--;
	else {
		putchar(c);
#ifdef DEBUG
		fprintf(outf, "PLODPUT(%s)\n", unctrl(c));
#endif
	}
}

/*
 * Put with padding
 */
putpad(cp)
reg char	*cp; {

	fflush(stdout);
#ifdef DEBUG
	fprintf(outf, "PUTPAD: _puts(\"%s\")\n", cp);
#endif
	_puts(cp);
}
