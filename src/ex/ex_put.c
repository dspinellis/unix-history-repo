/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_tty.h"

/*
 * Terminal driving and line formatting routines.
 * Basic motion optimizations are done here as well
 * as formatting of lines (printing of control characters,
 * line numbering and the like).
 */

/*
 * The routines outchar, putchar and pline are actually
 * variables, and these variables point at the current definitions
 * of the routines.  See the routine setflav.
 * We sometimes make outchar be routines which catch the characters
 * to be printed, e.g. if we want to see how long a line is.
 * During open/visual, outchar and putchar will be set to
 * routines in the file ex_vput.c (vputchar, vinschar, etc.).
 */
int	(*Outchar)() = termchar;
int	(*Putchar)() = normchar;
int	(*Pline)() = normline;

int (*
setlist(t))()
	bool t;
{
	register int (*P)();

	listf = t;
	P = Putchar;
	Putchar = t ? listchar : normchar;
	return (P);
}

int (*
setnumb(t))()
	bool t;
{
	register int (*P)();

	numberf = t;
	P = Pline;
	Pline = t ? numbline : normline;
	return (P);
}

/*
 * Format c for list mode; leave things in common
 * with normal print mode to be done by normchar.
 */
listchar(c)
	register short c;
{

	c &= (TRIM|QUOTE);
	switch (c) {

	case '\t':
	case '\b':
		outchar('^');
		c = ctlof(c);
		break;

	case '\n':
		break;

	case '\n' | QUOTE:
		outchar('$');
		break;

	default:
		if (c & QUOTE)
			break;
		if (c < ' ' && c != '\n' || c == DELETE)
			outchar('^'), c = ctlof(c);
		break;
	}
	normchar(c);
}

/*
 * Format c for printing.  Handle funnies of upper case terminals
 * and crocky hazeltines which don't have ~.
 */
normchar(c)
	register short c;
{
	register char *colp;

	c &= (TRIM|QUOTE);
	if (c == '~' && HZ) {
		normchar('\\');
		c = '^';
	}
	if (c & QUOTE)
		switch (c) {

		case ' ' | QUOTE:
		case '\b' | QUOTE:
			break;

		case QUOTE:
			return;

		default:
			c &= TRIM;
		}
	else if (c < ' ' && (c != '\b' || !OS) && c != '\n' && c != '\t' || c == DELETE)
		putchar('^'), c = ctlof(c);
	else if (UPPERCASE)
		if (isupper(c)) {
			outchar('\\');
			c = tolower(c);
		} else {
			colp = "({)}!|^~'`";
			while (*colp++)
				if (c == *colp++) {
					outchar('\\');
					c = colp[-2];
					break;
				}
		}
	outchar(c);
}

/*
 * Print a line with a number.
 */
numbline(i)
	int i;
{

	if (shudclob)
		slobber(' ');
	printf("%6d  ", i);
	normline();
}

/*
 * Normal line output, no numbering.
 */
normline()
{
	register char *cp;

	if (shudclob)
		slobber(linebuf[0]);
	/* pdp-11 doprnt is not reentrant so can't use "printf" here
	   in case we are tracing */
	for (cp = linebuf; *cp;)
		putchar(*cp++);
	if (!inopen)
		putchar('\n' | QUOTE);
}

/*
 * Given c at the beginning of a line, determine whether
 * the printing of the line will erase or otherwise obliterate
 * the prompt which was printed before.  If it won't, do it now.
 */
slobber(c)
	int c;
{

	shudclob = 0;
	switch (c) {

	case '\t':
		if (Putchar == listchar)
			return;
		break;

	default:
		return;

	case ' ':
	case 0:
		break;
	}
	if (OS)
		return;
	flush();
	putch(' ');
	if (BC)
		tputs(BC, 0, putch);
	else
		putch('\b');
}

/*
 * The output buffer is initialized with a useful error
 * message so we don't have to keep it in data space.
 */
static	char linb[66] = {
	'E', 'r', 'r', 'o', 'r', ' ', 'm', 'e', 's', 's', 'a', 'g', 'e', ' ',
	'f', 'i', 'l', 'e', ' ', 'n', 'o', 't', ' ',
	'a', 'v', 'a', 'i', 'l', 'a', 'b', 'l', 'e', '\n', 0
};
static	char *linp = linb + 33;

/*
 * Phadnl records when we have already had a complete line ending with \n.
 * If another line starts without a flush, and the terminal suggests it,
 * we switch into -nl mode so that we can send lineffeeds to avoid
 * a lot of spacing.
 */
static	bool phadnl;

/*
 * Indirect to current definition of putchar.
 */
putchar(c)
	int c;
{

	(*Putchar)(c);
}

/*
 * Termchar routine for command mode.
 * Watch for possible switching to -nl mode.
 * Otherwise flush into next level of buffering when
 * small buffer fills or at a newline.
 */
termchar(c)
	int c;
{

	if (pfast == 0 && phadnl)
		pstart();
	if (c == '\n')
		phadnl = 1;
	else if (linp >= &linb[63])
		flush1();
	*linp++ = c;
	if (linp >= &linb[63]) {
		fgoto();
		flush1();
	}
}

flush()
{

	flush1();
	flush2();
}

/*
 * Flush from small line buffer into output buffer.
 * Work here is destroying motion into positions, and then
 * letting fgoto do the optimized motion.
 */
flush1()
{
	register char *lp;
	register short c;

	*linp = 0;
	lp = linb;
	while (*lp)
		switch (c = *lp++) {

		case '\r':
			destline += destcol / COLUMNS;
			destcol = 0;
			continue;

		case '\b':
			if (destcol)
				destcol--;
			continue;

		case ' ':
			destcol++;
			continue;

		case '\t':
			destcol += value(TABSTOP) - destcol % value(TABSTOP);
			continue;

		case '\n':
			destline += destcol / COLUMNS + 1;
			if (destcol != 0 && destcol % COLUMNS == 0)
				destline--;
			destcol = 0;
			continue;

		default:
			fgoto();
			for (;;) {
				if (AM == 0 && outcol == COLUMNS)
					fgoto();
				c &= TRIM;
				putch(c);
				if (c == '\b') {
					outcol--;
					destcol--;
				} else if (c >= ' ' && c != DELETE) {
					outcol++;
					destcol++;
					if (XN && outcol % COLUMNS == 0)
						putch('\n');
				}
				c = *lp++;
				if (c <= ' ')
					break;
			}
			--lp;
			continue;
		}
	linp = linb;
}

flush2()
{

	fgoto();
	flusho();
	pstop();
}

/*
 * Sync the position of the output cursor.
 * Most work here is rounding for terminal boundaries getting the
 * column position implied by wraparound or the lack thereof and
 * rolling up the screen to get destline on the screen.
 */
fgoto()
{
	register int l, c;

	if (destcol > COLUMNS - 1) {
		destline += destcol / COLUMNS;
		destcol %= COLUMNS;
	}
	if (outcol > COLUMNS - 1) {
		l = (outcol + 1) / COLUMNS;
		outline += l;
		outcol %= COLUMNS;
		if (AM == 0) {
			while (l > 0) {
				if (pfast)
					putch('\r');
				putch('\n');
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
			if (pfast == 0 && (!CA || holdcm))
				destcol = 0;
			fgoto();
			destcol = c;
		}
		while (l > LINES - 1) {
			putch('\n');
			l--;
			if (pfast == 0)
				outcol = 0;
		}
	}
	if (destline < outline && !(CA && !holdcm || UP != NOSTR))
		destline = outline;
	if (motion())
		return;
	if (CA && !holdcm) {
		tputs(cgoto(), 0, putch);
		outline = destline;
		outcol = destcol;
	} else
		plod();
}

/*
 * Tab to column col by flushing and then setting destcol.
 * Used by "set all".
 */
tab(col)
	int col;
{

	flush1();
	destcol = col;
}

/*
 * Routine to decide how to do a basic motion,
 * choosing between local motions and cursor addressing.
 */ 
motion()
{
	register int v, h;
	/*
	 * V is vertical distance, then cost with cr.
	 * H is horizontal distance, then direct move cost.
	 */

	if (!BS)
		return (0);
	v = destline - outline;
	if (v < 0)
		if (CA && !holdcm || UP) {
			if (!UP)
				return (0);
			v = -v;
		} else
			destline = outline;
	h = destcol;
	if (!v || pfast) {
		h -= outcol;
		if (h < 0)
			h = -h;
	}
	h += v;
	if (pfast || !NONL) {
		if (outcol)
			v++;
		v += destcol;
	} else
		v = 5;
	if (v >= 4 && h >= 4)
		return (0);
	plod();
	return (1);
}

/*
 * Move (slowly) to destination.
 * Hard thing here is using home cursor on really deficient terminals.
 * Otherwise just use cursor motions, hacking use of tabs and overtabbing
 * and backspace.
 */
plod()
{
	register int i, j, k;

	if (HO && (!CA || holdcm)) {
		if (GT)
			i = (destcol >> 3) + (destcol & 07);
		else
			i = destcol;
		if (destcol >= outcol)
			if (GT && (j = ((destcol - (outcol &~ 07)) >> 3)))
				j += destcol & 07;
			else
				j = destcol - outcol;
		else
			if (outcol - destcol <= i && (BS || BC))
				i = j = outcol - destcol;
			else
				j = i + 1;
		k = outline - destline;
		if (k < 0)
			k = -k;
		j += k;
		if (i + destline < j) {
			tputs(HO, 0, putch);
			outcol = outline = 0;
		} else if (LL) {
			k = (LINES - 1) - destline;
			if (i + k + 2 < j) {
				tputs(LL, 0, putch);
				outcol = 0;
				outline = LINES - 1;
			}
		}
	}
	if (GT && (!CA || holdcm))
		i = (destcol >> 3) + (destcol & 07);
	else
		i = destcol;
	if ((!NONL || outline >= destline) && (!NC || outline < destline) &&
	    (outcol - destcol > i + 1 || outcol > destcol && !BS && !BC)) {
		putch('\r');
		if (NC) {
			putch('\n');
			outline++;
		}
		outcol = 0;
	}
	while (outline < destline) {
		outline++;
		putch('\n');
		if (NONL || pfast == 0)
			outcol = 0;
	}
	while (outcol > destcol) {
		outcol--;
		if (BC)
			tputs(BC, 0, putch);
		else
			putch('\b');
	}
	while (outline > destline) {
		outline--;
		putS(UP);
	}
	if (GT && destcol - outcol > 1) {
		while ((i = ((outcol + 8) &~ 7)) <= destcol) {
			if (TA)
				tputs(TA, 0, putch);
			else
				putch('\t');
			outcol = i;
		}
		if (destcol - outcol > 4 && (BC || BS)) {
			if (TA)
				tputs(TA, 0, putch);
			else
				putch('\t');
			outcol = i;
			while (outcol > destcol) {
				outcol--;
				if (BC)
					tputs(BC, 0, putch);
				else
					putch('\b');
			}
		}
	}
	while (outcol < destcol) {
		if (inopen && ND)
			putS(ND);
		else
			putch(' ');
		outcol++;
	}
}

/*
 * An input line arrived.
 * Calculate new (approximate) screen line position.
 * Approximate because kill character echoes newline with
 * no feedback and also because of long input lines.
 */
noteinp()
{

	outline++;
	if (outline > LINES - 1)
		outline = LINES - 1;
	destline = outline;
	destcol = outcol = 0;
}

/*
 * Something weird just happened and we
 * lost track of whats happening out there.
 * Since we cant, in general, read where we are
 * we just reset to some known state.
 * On cursor addressible terminals setting to unknown
 * will force a cursor address soon.
 */
termreset()
{

	endim();
	destcol = 0;
	destline = LINES - 1;
	if (CA) {
		outcol = UKCOL;
		outline = UKCOL;
	} else {
		outcol = destcol;
		outline = destline;
	}
}

/*
 * Low level buffering, with the ability to drain
 * buffered output without printing it.
 */
char	*obp = obuf;

draino()
{

	obp = obuf;
}

flusho()
{

	if (obp != obuf) {
		write(1, obuf, obp - obuf);
		obp = obuf;
	}
}

putnl()
{

	putchar('\n');
}

putS(cp)
	char *cp;
{

	if (cp == NULL)
		return;
	while (*cp)
		putch(*cp++);
}


putch(c)
	int c;
{

	*obp++ = c;
	if (obp >= &obuf[sizeof obuf])
		flusho();
}

/*
 * Miscellaneous routines related to output.
 */

/*
 * Cursor motion.
 */
char *
cgoto()
{

	return (tgoto(CM, destcol, destline));
}

/*
 * Put with padding
 */
putpad(cp)
	char *cp;
{

	flush();
	tputs(cp, 0, putch);
}

/*
 * Set output through normal command mode routine.
 */
setoutt()
{

	Outchar = termchar;
}

/*
 * Printf (temporarily) in list mode.
 */
/*VARARGS2*/
lprintf(cp, dp)
	char *cp, *dp;
{
	register int (*P)();

	P = setlist(1);
	printf(cp, dp);
	Putchar = P;
}

/*
 * Newline + flush.
 */
putNFL()
{

	putnl();
	flush();
}

/*
 * Try to start -nl mode.
 */
pstart()
{

	if (NONL)
		return;
 	if (!value(OPTIMIZE))
		return;
	if (ruptible == 0 || pfast)
		return;
	fgoto();
	flusho();
	pfast = 1;
	normtty++;
	tty.sg_flags = normf & ~(ECHO|CRMOD);
	sTTY(1);
}

/*
 * Stop -nl mode.
 */
pstop()
{

	if (inopen)
		return;
	phadnl = 0;
	linp = linb;
	draino();
	normal(normf);
	pfast &= ~1;
}

/*
 * Prep tty for open mode.
 */
ostart()
{
	int f;

	if (!intty)
		error("Open and visual must be used interactively");
	gTTY(1);
	normtty++;
	f = tty.sg_flags;
#ifdef V6
	tty.sg_flags = (normf &~ (ECHO|CRMOD)) | RAW;
#else
	tty.sg_flags = (normf &~ (ECHO|CRMOD)) | CBREAK;
#endif
	sTTY(1);
	putpad(VS);
	pfast |= 2;
	return (f);
}

/*
 * Stop open, restoring tty modes.
 */
ostop(f)
	int f;
{

	pfast = (f & CRMOD) == 0;
	termreset(), fgoto(), flusho();
	normal(f);
	putpad(VE);
}

#ifdef V6
/*
 * Into cooked mode for interruptibility.
 */
vcook()
{

	tty.sg_flags &= ~RAW;
	sTTY(1);
}

/*
 * Back into raw mode.
 */
vraw()
{

	tty.sg_flags |= RAW;
	sTTY(1);
}
#endif

/*
 * Restore flags to normal state f.
 */
normal(f)
	int f;
{

	if (normtty > 0) {
		setty(f);
		normtty--;
	}
}

/*
 * Straight set of flags to state f.
 */
setty(f)
	int f;
{
	register int ot = tty.sg_flags;

	tty.sg_flags = f;
	sTTY(1);
	return (ot);
}

gTTY(i)
	int i;
{

	ignore(gtty(i, &tty));
}

sTTY(i)
	int i;
{

#ifdef V6
	stty(i, &tty);
#else
	ioctl(i, TIOCSETN, &tty);
#endif
}

/*
 * Print newline, or blank if in open/visual
 */
noonl()
{

	putchar(Outchar != termchar ? ' ' : '\n');
}
