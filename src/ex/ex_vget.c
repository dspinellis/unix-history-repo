/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * Input routines for open/visual.
 * We handle upper case only terminals in visual and reading from the
 * echo area here as well as notification on large changes
 * which appears in the echo area.
 */

/*
 * Return the key.
 */
ungetkey(c)
	char c;
{

	if (Peekkey != ATTN)
		Peekkey = c;
}

/*
 * Return a keystroke, but never a ^@.
 */
getkey()
{
	register char c;

	do
		c = getbr();
	while (c == 0);
	return (c);
}

/*
 * Tell whether next keystroke would be a ^@.
 */
peekbr()
{

	Peekkey = getbr();
	return (Peekkey == 0);
}

short	precbksl;

/*
 * Get a keystroke, including a ^@.
 * If an key was returned with ungetkey, that
 * comes back first.  Next comes unread input (e.g.
 * from repeating commands with .), and finally new
 * keystrokes.
 *
 * The hard work here is in mapping of \ escaped
 * characters on upper case only terminals.
 */
getbr()
{
	char ch;
	register int c, d;
	register char *colp;

getATTN:
	if (Peekkey) {
		c = Peekkey;
		Peekkey = 0;
		return (c);
	}
	if (vglobp) {
		if (*vglobp)
			return (*vglobp++);
		return (ESCAPE);
	}
#ifdef TRACE
	if (trace)
		fflush(trace);
#endif
	flusho();
again:
	if (read(0, &ch, 1) != 1) {
		if (errno == EINTR)
			goto getATTN;
		error("Input read error");
	}
	c = ch & TRIM;

#ifdef UCVISUAL
	/*
	 * The algorithm here is that of the UNIX kernel.
	 * See the description in the programmers manual.
	 */
	if (UPPERCASE) {
		if (isupper(c))
			c = tolower(c);
		if (c == '\\') {
			if (precbksl < 2)
				precbksl++;
			if (precbksl == 1)
				goto again;
		} else if (precbksl) {
			d = 0;
			if (islower(c))
				d = toupper(c);
			else {
				colp = "({)}!|^~'~";
				while (d = *colp++)
					if (d == c) {
						d = *colp++;
						break;
					} else
						colp++;
			}
			if (precbksl == 2) {
				if (!d) {
					Peekkey = c;
					precbksl = 0;
					c = '\\';
				}
			} else if (d)
				c = d;
			else {
				Peekkey = c;
				precbksl = 0;
				c = '\\';
			}
		}
		if (c != '\\')
			precbksl = 0;
	}
#endif
#ifdef TRACE
	if (trace) {
		if (!techoin) {
			tfixnl();
			techoin = 1;
			fprintf(trace, "*** Input: ");
		}
		tracec(c);
	}
#endif
	return (c);
}

/*
 * Get a key, but if a delete, quit or attention
 * is typed return 0 so we will abort a partial command.
 */
getesc()
{
	register int c;

	c = getkey();
	switch (c) {

	case ATTN:
	case QUIT:
		ungetkey(c);
		return (0);

	case ESCAPE:
		return (0);
	}
	return (c);
}

/*
 * Peek at the next keystroke.
 */
peekkey()
{

	Peekkey = getkey();
	return (Peekkey);
}

/*
 * Read a line from the echo area, with single character prompt c.
 * A return value of 1 means the user blewit or blewit away.
 */
readecho(c)
	char c;
{
	char *sc = cursor;
	int (*OP)();
	bool waste;

	if (WBOT == WECHO)
		vclean();
	else
		vclrech(0);
	splitw++;
	vgoto(WECHO, 0);
	putchar(c);
	vclreol();
	vgoto(WECHO, 1);
	cursor = linebuf; linebuf[0] = 0; genbuf[0] = c;
	if (peekbr()) {
		if (!INS[0] || (INS[0] & (QUOTE|TRIM)) == OVERBUF)
			goto blewit;
		vglobp = INS;
	}
	OP = Pline; Pline = normline;
	ignore(vgetline(0, genbuf + 1, &waste));
	vscrap();
	Pline = OP;
	if (Peekkey != ATTN && Peekkey != QUIT) {
		cursor = sc;
		vclreol();
		return (0);
	}
blewit:
	splitw = 0;
	vclean();
	vshow(dot, NOLINE);
	vnline(sc);
	return (1);
}

/*
 * A complete command has been defined for
 * the purposes of repeat, so copy it from
 * the working to the previous command buffer.
 */
setLAST()
{

	if (vglobp)
		return;
	lastreg = vreg;
	lasthad = Xhadcnt;
	lastcnt = Xcnt;
	*lastcp = 0;
	CP(lastcmd, workcmd);
}

/*
 * Gather up some more text from an insert.
 * If the insertion buffer oveflows, then destroy
 * the repeatability of the insert.
 */
addtext(cp)
	char *cp;
{

	if (vglobp)
		return;
	addto(INS, cp);
	if ((INS[0] & (QUOTE|TRIM)) == OVERBUF)
		lastcmd[0] = 0;
}

setDEL()
{

	setBUF(DEL);
}

/*
 * Put text from cursor upto wcursor in BUF.
 */
setBUF(BUF)
	register char *BUF;
{
	register int c;
	register char *wp = wcursor;

	c = *wp;
	*wp = 0;
	BUF[0] = 0;
	addto(BUF, cursor);
	*wp = c;
}

addto(buf, str)
	register char *buf, *str;
{

	if ((buf[0] & (QUOTE|TRIM)) == OVERBUF)
		return;
	if (strlen(buf) + strlen(str) + 1 >= VBSIZE) {
		buf[0] = OVERBUF;
		return;
	}
	ignore(strcat(buf, str));
}

/*
 * Note a change affecting a lot of lines, or non-visible
 * lines.  If the parameter must is set, then we only want
 * to do this for open modes now; return and save for later
 * notification in visual.
 */
noteit(must)
	bool must;
{
	register int sdl = destline, sdc = destcol;

	if (notecnt < 2 || !must && state == VISUAL)
		return (0);
	splitw++;
	if (WBOT == WECHO)
		vmoveitup(1);
	vigoto(WECHO, 0);
	printf("%d %sline", notecnt, notesgn);
	if (notecnt > 1)
		putchar('s');
	if (*notenam) {
		printf(" %s", notenam);
		if (*(strend(notenam) - 1) != 'e')
			putchar('e');
		putchar('d');
	}
	vclreol();
	notecnt = 0;
	if (state != VISUAL)
		vcnt = vcline = 0;
	splitw = 0;
	if (state == ONEOPEN || state == CRTOPEN)
		vup1();
	destline = sdl; destcol = sdc;
	return (1);
}

/*
 * Rrrrringgggggg.
 * If possible, use flash (VB).
 */
beep()
{

	if (VB)
		vputp(VB, 0);
	else
		vputc(CTRL(g));
}

/*
 * Map the command input character c,
 * for keypads and labelled keys which do cursor
 * motions.  I.e. on an adm3a we might map ^K to ^P.
 * DM1520 for example has a lot of mappable characters.
 */
map(c)
	register int c;
{
	register int d;
	register char *cp = MA;

	if (cp == 0)
		return (c);
	while (d = *cp++) {
		if (c == d)
			return (*cp);
		if (*cp++ == 0)
			return (c);
	}
	return (c);
}

/*
 * Get a count from the keyed input stream.
 * A zero count is indistinguishable from no count.
 */
vgetcnt()
{
	register int c, cnt;

	cnt = 0;
	for (;;) {
		c = getkey();
		if (!isdigit(c))
			break;
		cnt *= 10, cnt += c - '0';
	}
	ungetkey(c);
	Xhadcnt = 1;
	Xcnt = cnt;
	return(cnt);
}
