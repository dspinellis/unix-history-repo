/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ex_vget.c	6.7 (Berkeley) %G%";
#endif not lint

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
	int c;		/* mjm: char --> int */
{

	if (Peekkey != ATTN)
		Peekkey = c;
}

/*
 * Return a keystroke, but never a ^@.
 */
getkey()
{
	register int c;		/* mjm: char --> int */

	do {
		c = getbr();
		if (c==0)
			beep();
	} while (c == 0);
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
jmp_buf	readbuf;
int	doingread = 0;

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
	int cnt;
#define BEEHIVE
#ifdef BEEHIVE
	static char Peek2key;
#endif
	extern short slevel, ttyindes;

getATTN:
	if (Peekkey) {
		c = Peekkey;
		Peekkey = 0;
		return (c);
	}
#ifdef BEEHIVE
	if (Peek2key) {
		c = Peek2key;
		Peek2key = 0;
		return (c);
	}
#endif
	if (vglobp) {
		if (*vglobp)
			return (lastvgk = *vglobp++);
		lastvgk = 0;
		return (ESCAPE);
	}
	if (vmacp) {
		if (*vmacp)
			return(*vmacp++);
		/* End of a macro or set of nested macros */
		vmacp = 0;
		if (inopen == -1)	/* don't screw up undo for esc esc */
			vundkind = VMANY;
		inopen = 1;	/* restore old setting now that macro done */
		vch_mac = VC_NOTINMAC;
	}
	flusho();
again:
	if (setjmp(readbuf))
		goto getATTN;
	doingread = 1;
	c = read(slevel == 0 ? 0 : ttyindes, &ch, 1);
	doingread = 0;
	if (c != 1) {
		if (errno == EINTR)
			goto getATTN;
		error("Input read error");
	}
	c = ch & TRIM;
#ifdef BEEHIVE
	if (XB && slevel==0 && c == ESCAPE) {
		if (read(0, &Peek2key, 1) != 1)
			goto getATTN;
		Peek2key &= TRIM;
		switch (Peek2key) {
		case 'C':	/* SPOW mode sometimes sends \EC for space */
			c = ' ';
			Peek2key = 0;
			break;
		case 'q':	/* f2 -> ^C */
			c = CTRL(c);
			Peek2key = 0;
			break;
		case 'p':	/* f1 -> esc */
			Peek2key = 0;
			break;
		}
	}
#endif

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
	lastvgk = 0;
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

	case CTRL(v):
	case CTRL(q):
		c = getkey();
		return (c);

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
	register char *sc = cursor;
	register int (*OP)();
	bool waste;
	register int OPeek;

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
	ignore(vgetline(0, genbuf + 1, &waste, c));
	if (Outchar == termchar)
		putchar('\n');
	vscrap();
	Pline = OP;
	if (Peekkey != ATTN && Peekkey != QUIT && Peekkey != CTRL(h)) {
		cursor = sc;
		vclreol();
		return (0);
	}
blewit:
	OPeek = Peekkey==CTRL(h) ? 0 : Peekkey; Peekkey = 0;
	splitw = 0;
	vclean();
	vshow(dot, NOLINE);
	vnline(sc);
	Peekkey = OPeek;
	return (1);
}

/*
 * A complete command has been defined for
 * the purposes of repeat, so copy it from
 * the working to the previous command buffer.
 */
setLAST()
{

	if (vglobp || vmacp)
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
		vmoveitup(1, 1);
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

map(c,maps)
	register int c;
	register struct maps *maps;
{
	register int d;
	register char *p, *q;
	char b[10];	/* Assumption: no keypad sends string longer than 10 */

	/*
	 * Mapping for special keys on the terminal only.
	 * BUG: if there's a long sequence and it matches
	 * some chars and then misses, we lose some chars.
	 *
	 * For this to work, some conditions must be met.
	 * 1) Keypad sends SHORT (2 or 3 char) strings
	 * 2) All strings sent are same length & similar
	 * 3) The user is unlikely to type the first few chars of
	 *    one of these strings very fast.
	 * Note: some code has been fixed up since the above was laid out,
	 * so conditions 1 & 2 are probably not required anymore.
	 * However, this hasn't been tested with any first char
	 * that means anything else except escape.
	 */
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"map(%c): ",c);
#endif
	/*
	 * If c==0, the char came from getesc typing escape.  Pass it through
	 * unchanged.  0 messes up the following code anyway.
	 */
	if (c==0)
		return(0);

	b[0] = c;
	b[1] = 0;
	for (d=0; maps[d].mapto; d++) {
#ifdef MDEBUG
		if (trace)
			fprintf(trace,"\ntry '%s', ",maps[d].cap);
#endif
		if (p = maps[d].cap) {
			for (q=b; *p; p++, q++) {
#ifdef MDEBUG
				if (trace)
					fprintf(trace,"q->b[%d], ",q-b);
#endif
				if (*q==0) {
					/*
					 * Is there another char waiting?
					 *
					 * This test is oversimplified, but
					 * should work mostly. It handles the
					 * case where we get an ESCAPE that
					 * wasn't part of a keypad string.
					 */
					if ((c=='#' ? peekkey() : fastpeekkey()) == 0) {
#ifdef MDEBUG
						if (trace)
							fprintf(trace,"fpk=0: will return '%c'",c);
#endif
						/*
						 * Nothing waiting.  Push back
						 * what we peeked at & return
						 * failure (c).
						 *
						 * We want to be able to undo
						 * commands, but it's nonsense
						 * to undo part of an insertion
						 * so if in input mode don't.
						 */
#ifdef MDEBUG
						if (trace)
							fprintf(trace, "Call macpush, b %d %d %d\n", b[0], b[1], b[2]);
#endif
						macpush(&b[1],maps == arrows);
#ifdef MDEBUG
						if (trace)
							fprintf(trace, "return %d\n", c);	
#endif
						return(c);
					}
					*q = getkey();
					q[1] = 0;
				}
				if (*p != *q)
					goto contin;
			}
			macpush(maps[d].mapto,maps == arrows);
			c = getkey();
#ifdef MDEBUG
			if (trace)
				fprintf(trace,"Success: push(%s), return %c",maps[d].mapto, c);
#endif
			return(c);	/* first char of map string */
			contin:;
		}
	}
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"Fail: push(%s), return %c", &b[1], c);
#endif
	macpush(&b[1],0);
	return(c);
}

/*
 * Push st onto the front of vmacp. This is tricky because we have to
 * worry about where vmacp was previously pointing. We also have to
 * check for overflow (which is typically from a recursive macro)
 * Finally we have to set a flag so the whole thing can be undone.
 * canundo is 1 iff we want to be able to undo the macro.  This
 * is false for, for example, pushing back lookahead from fastpeekkey(),
 * since otherwise two fast escapes can clobber our undo.
 */
macpush(st, canundo)
char *st;
int canundo;
{
	char tmpbuf[BUFSIZ];

	if (st==0 || *st==0)
		return;
#ifdef MDEBUG
	if (trace)
		fprintf(trace, "macpush(%s), canundo=%d\n",st,canundo);
#endif
	if ((vmacp ? strlen(vmacp) : 0) + strlen(st) > BUFSIZ)
		error("Macro too long@ - maybe recursive?");
	if (vmacp) {
		strcpy(tmpbuf, vmacp);
		if (!FIXUNDO)
			canundo = 0;	/* can't undo inside a macro anyway */
	}
	strcpy(vmacbuf, st);
	if (vmacp)
		strcat(vmacbuf, tmpbuf);
	vmacp = vmacbuf;
	/* arrange to be able to undo the whole macro */
	if (canundo) {
#ifdef notdef
		otchng = tchng;
		vsave();
		saveall();
		inopen = -1;	/* no need to save since it had to be 1 or -1 before */
		vundkind = VMANY;
#endif
		vch_mac = VC_NOCHANGE;
	}
}

#ifdef TRACE
visdump(s)
char *s;
{
	register int i;

	if (!trace) return;

	fprintf(trace, "\n%s: basWTOP=%d, basWLINES=%d, WTOP=%d, WBOT=%d, WLINES=%d, WCOLS=%d, WECHO=%d\n",
		s, basWTOP, basWLINES, WTOP, WBOT, WLINES, WCOLS, WECHO);
	fprintf(trace, "   vcnt=%d, vcline=%d, cursor=%d, wcursor=%d, wdot=%d\n",
		vcnt, vcline, cursor-linebuf, wcursor-linebuf, wdot-zero);
	for (i=0; i<TUBELINES; i++)
		if (vtube[i] && *vtube[i])
			fprintf(trace, "%d: '%s'\n", i, vtube[i]);
	tvliny();
}

vudump(s)
char *s;
{
	register line *p;
	char savelb[1024];

	if (!trace) return;

	fprintf(trace, "\n%s: undkind=%d, vundkind=%d, unddel=%d, undap1=%d, undap2=%d,\n",
		s, undkind, vundkind, lineno(unddel), lineno(undap1), lineno(undap2));
	fprintf(trace, "  undadot=%d, dot=%d, dol=%d, unddol=%d, truedol=%d\n",
		lineno(undadot), lineno(dot), lineno(dol), lineno(unddol), lineno(truedol));
	fprintf(trace, "  [\n");
	CP(savelb, linebuf);
	fprintf(trace, "linebuf = '%s'\n", linebuf);
	for (p=zero+1; p<=truedol; p++) {
		fprintf(trace, "%o ", *p);
		getline(*p);
		fprintf(trace, "'%s'\n", linebuf);
	}
	fprintf(trace, "]\n");
	CP(linebuf, savelb);
}
#endif

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

/*
 * fastpeekkey is just like peekkey but insists the character come in
 * fast (within 1 second). This will succeed if it is the 2nd char of
 * a machine generated sequence (such as a function pad from an escape
 * flavor terminal) but fail for a human hitting escape then waiting.
 */
fastpeekkey()
{
	int trapalarm();
	int (*Oint)();
	register int c;

	/*
	 * If the user has set notimeout, we wait forever for a key.
	 * If we are in a macro we do too, but since it's already
	 * buffered internally it will return immediately.
	 * In other cases we force this to die in 1 second.
	 * This is pretty reliable (VMUNIX rounds it to .5 - 1.5 secs,
	 * but UNIX truncates it to 0 - 1 secs) but due to system delays
	 * there are times when arrow keys or very fast typing get counted
	 * as separate.  notimeout is provided for people who dislike such
	 * nondeterminism.
	 */
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"\nfastpeekkey: ",c);
#endif
	Oint = signal(SIGINT, trapalarm);
	if (value(TIMEOUT) && inopen >= 0) {
		signal(SIGALRM, trapalarm);
#ifdef MDEBUG
		alarm(10);
		if (trace)
			fprintf(trace, "set alarm ");
#else
		alarm(1);
#endif
	}
	CATCH
		c = peekkey();
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"[OK]",c);
#endif
		alarm(0);
	ONERR
		c = 0;
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"[TIMEOUT]",c);
#endif
	ENDCATCH
#ifdef MDEBUG
	if (trace)
		fprintf(trace,"[fpk:%o]",c);
#endif
	signal(SIGINT,Oint);
	return(c);
}

trapalarm() {
	alarm(0);
	if (vcatch)
		longjmp(vreslab,1);
}
