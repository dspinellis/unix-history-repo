/* Copyright (c) 1981 Regents of the University of California */
static char *sccsid = "@(#)ex_voper.c	7.2	%G%";
#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

#define	blank()		isspace(wcursor[0])
#define	forbid(a)	if (a) goto errlab;

char	vscandir[2] =	{ '/', 0 };

/*
 * Decode an operator/operand type command.
 * Eventually we switch to an operator subroutine in ex_vops.c.
 * The work here is setting up a function variable to point
 * to the routine we want, and manipulation of the variables
 * wcursor and wdot, which mark the other end of the affected
 * area.  If wdot is zero, then the current line is the other end,
 * and if wcursor is zero, then the first non-blank location of the
 * other line is implied.
 */
operate(c, cnt)
	register int c, cnt;
{
	register int i;
	int (*moveop)(), (*deleteop)();
	register int (*opf)();
	bool subop = 0;
	char *oglobp, *ocurs;
	register line *addr;
	line *odot;
	static char lastFKND, lastFCHR;
	short d;

	moveop = vmove, deleteop = vdelete;
	wcursor = cursor;
	wdot = NOLINE;
	notecnt = 0;
	dir = 1;
	switch (c) {

	/*
	 * d		delete operator.
	 */
	case 'd':
		moveop = vdelete;
		deleteop = beep;
		break;

	/*
	 * s		substitute characters, like c\040, i.e. change space.
	 */
	case 's':
		ungetkey(' ');
		subop++;
		/* fall into ... */

	/*
	 * c		Change operator.
	 */
	case 'c':
		if (c == 'c' && workcmd[0] == 'C' || workcmd[0] == 'S')
			subop++;
		moveop = vchange;
		deleteop = beep;
		break;

	/*
	 * !		Filter through a UNIX command.
	 */
	case '!':
		moveop = vfilter;
		deleteop = beep;
		break;

	/*
	 * y		Yank operator.  Place specified text so that it
	 *		can be put back with p/P.  Also yanks to named buffers.
	 */
	case 'y':
		moveop = vyankit;
		deleteop = beep;
		break;

	/*
	 * =		Reformat operator (for LISP).
	 */
#ifdef LISPCODE
	case '=':
		forbid(!value(LISP));
		/* fall into ... */
#endif

	/*
	 * >		Right shift operator.
	 * <		Left shift operator.
	 */
	case '<':
	case '>':
		moveop = vshftop;
		deleteop = beep;
		break;

	/*
	 * r		Replace character under cursor with single following
	 *		character.
	 */
	case 'r':
		vmacchng(1);
		vrep(cnt);
		return;

	default:
		goto nocount;
	}
	vmacchng(1);
	/*
	 * Had an operator, so accept another count.
	 * Multiply counts together.
	 */
	if (isdigit(peekkey()) && peekkey() != '0') {
		cnt *= vgetcnt();
		Xcnt = cnt;
		forbid (cnt <= 0);
	}

	/*
	 * Get next character, mapping it and saving as
	 * part of command for repeat.
	 */
	c = map(getesc(),arrows);
	if (c == 0)
		return;
	if (!subop)
		*lastcp++ = c;
nocount:
	opf = moveop;
	switch (c) {

	/*
	 * b		Back up a word.
	 * B		Back up a word, liberal definition.
	 */
	case 'b':
	case 'B':
		dir = -1;
		/* fall into ... */

	/*
	 * w		Forward a word.
	 * W		Forward a word, liberal definition.
	 */
	case 'W':
	case 'w':
		wdkind = c & ' ';
		forbid(lfind(2, cnt, opf, 0) < 0);
		vmoving = 0;
		break;

	/*
	 * E		to end of following blank/nonblank word
	 */
	case 'E':
		wdkind = 0;
		goto ein;

	/*
	 * e		To end of following word.
	 */
	case 'e':
		wdkind = 1;
ein:
		forbid(lfind(3, cnt - 1, opf, 0) < 0);
		vmoving = 0;
		break;

	/*
	 * (		Back an s-expression.
	 */
	case '(':
		dir = -1;
		/* fall into... */

	/*
	 * )		Forward an s-expression.
	 */
	case ')':
		forbid(lfind(0, cnt, opf, (line *) 0) < 0);
		markDOT();
		break;

	/*
	 * {		Back an s-expression, but don't stop on atoms.
	 *		In text mode, a paragraph.  For C, a balanced set
	 *		of {}'s.
	 */
	case '{':
		dir = -1;
		/* fall into... */

	/*
	 * }		Forward an s-expression, but don't stop on atoms.
	 *		In text mode, back paragraph.  For C, back a balanced
	 *		set of {}'s.
	 */
	case '}':
		forbid(lfind(1, cnt, opf, (line *) 0) < 0);
		markDOT();
		break;

	/*
	 * %		To matching () or {}.  If not at ( or { scan for
	 *		first such after cursor on this line.
	 */
	case '%':
		vsave();
		i = lmatchp((line *) 0);
#ifdef TRACE
		if (trace)
			fprintf(trace, "after lmatchp in %, dot=%d, wdot=%d, dol=%d\n", lineno(dot), lineno(wdot), lineno(dol));
#endif
		getDOT();
		forbid(!i);
		if (opf != vmove)
			if (dir > 0)
				wcursor++;
			else
				cursor++;
		else
			markDOT();
		vmoving = 0;
		break;

	/*
	 * [		Back to beginning of defun, i.e. an ( in column 1.
	 *		For text, back to a section macro.
	 *		For C, back to a { in column 1 (~~ beg of function.)
	 */
	case '[':
		dir = -1;
		/* fall into ... */

	/*
	 * ]		Forward to next defun, i.e. a ( in column 1.
	 *		For text, forward section.
	 *		For C, forward to a } in column 1 (if delete or such)
	 *		or if a move to a { in column 1.
	 */
	case ']':
		if (!vglobp)
			forbid(getkey() != c);
		forbid (Xhadcnt);
		vsave();
		i = lbrack(c, opf);
		getDOT();
		forbid(!i);
		markDOT();
		if (ospeed > B300)
			hold |= HOLDWIG;
		break;

	/*
	 * ,		Invert last find with f F t or T, like inverse
	 *		of ;.
	 */
	case ',':
		forbid (lastFKND == 0);
		c = isupper(lastFKND) ? tolower(lastFKND) : toupper(lastFKND);
		i = lastFCHR;
		if (vglobp == 0)
			vglobp = "";
		subop++;
		goto nocount;

	/*
	 * 0		To beginning of real line.
	 */
	case '0':
		wcursor = linebuf;
		vmoving = 0;
		break;

	/*
	 * ;		Repeat last find with f F t or T.
	 */
	case ';':
		forbid (lastFKND == 0);
		c = lastFKND;
		i = lastFCHR;
		subop++;
		goto nocount;

	/*
	 * F		Find single character before cursor in current line.
	 * T		Like F, but stops before character.
	 */
	case 'F':	/* inverted find */
	case 'T':
		dir = -1;
		/* fall into ... */

	/*
	 * f		Find single character following cursor in current line.
	 * t		Like f, but stope before character.
	 */
	case 'f':	/* find */
	case 't':
		if (!subop) {
			i = getesc();
			if (i == 0)
				return;
			*lastcp++ = i;
		}
		if (vglobp == 0)
			lastFKND = c, lastFCHR = i;
		for (; cnt > 0; cnt--)
			forbid (find(i) == 0);
		vmoving = 0;
		switch (c) {

		case 'T':
			wcursor++;
			break;

		case 't':
			wcursor--;
		case 'f':
fixup:
			if (moveop != vmove)
				wcursor++;
			break;
		}
		break;

	/*
	 * |		Find specified print column in current line.
	 */
	case '|':
		if (Pline == numbline)
			cnt += 8;
		vmovcol = cnt;
		vmoving = 1;
		wcursor = vfindcol(cnt);
		break;

	/*
	 * ^		To beginning of non-white space on line.
	 */
	case '^':
		wcursor = vskipwh(linebuf);
		vmoving = 0;
		break;

	/*
	 * $		To end of line.
	 */
	case '$':
		if (opf == vmove) {
			vmoving = 1;
			vmovcol = 20000;
		} else
			vmoving = 0;
		if (cnt > 1) {
			if (opf == vmove) {
				wcursor = 0;
				cnt--;
			} else
				wcursor = linebuf;
			/* This is wrong at EOF */
			wdot = dot + cnt;
			break;
		}
		if (linebuf[0]) {
			wcursor = strend(linebuf) - 1;
			goto fixup;
		}
		wcursor = linebuf;
		break;

	/*
	 * h		Back a character.
	 * ^H		Back a character.
	 */
	case 'h':
	case CTRL(h):
		dir = -1;
		/* fall into ... */

	/*
	 * space	Forward a character.
	 */
	case 'l':
	case ' ':
		forbid (margin() || opf == vmove && edge());
		while (cnt > 0 && !margin())
			wcursor += dir, cnt--;
		if (margin() && opf == vmove || wcursor < linebuf)
			wcursor -= dir;
		vmoving = 0;
		break;

	/*
	 * D		Delete to end of line, short for d$.
	 */
	case 'D':
		cnt = INF;
		goto deleteit;

	/*
	 * X		Delete character before cursor.
	 */
	case 'X':
		dir = -1;
		/* fall into ... */
deleteit:
	/*
	 * x		Delete character at cursor, leaving cursor where it is.
	 */
	case 'x':
		if (margin())
			goto errlab;
		vmacchng(1);
		while (cnt > 0 && !margin())
			wcursor += dir, cnt--;
		opf = deleteop;
		vmoving = 0;
		break;

	default:
		/*
		 * Stuttered operators are equivalent to the operator on
		 * a line, thus turn dd into d_.
		 */
		if (opf == vmove || c != workcmd[0]) {
errlab:
			beep();
			vmacp = 0;
			return;
		}
		/* fall into ... */

	/*
	 * _		Target for a line or group of lines.
	 *		Stuttering is more convenient; this is mostly
	 *		for aesthetics.
	 */
	case '_':
		wdot = dot + cnt - 1;
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * H		To first, home line on screen.
	 *		Count is for count'th line rather than first.
	 */
	case 'H':
		wdot = (dot - vcline) + cnt - 1;
		if (opf == vmove)
			markit(wdot);
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * -		Backwards lines, to first non-white character.
	 */
	case '-':
		wdot = dot - cnt;
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * ^P		To previous line same column.  Ridiculous on the
	 *		console of the VAX since it puts console in LSI mode.
	 */
	case 'k':
	case CTRL(p):
		wdot = dot - cnt;
		if (vmoving == 0)
			vmoving = 1, vmovcol = column(cursor);
		wcursor = 0;
		break;

	/*
	 * L		To last line on screen, or count'th line from the
	 *		bottom.
	 */
	case 'L':
		wdot = dot + vcnt - vcline - cnt;
		if (opf == vmove)
			markit(wdot);
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * M		To the middle of the screen.
	 */
	case 'M':
		wdot = dot + ((vcnt + 1) / 2) - vcline - 1;
		if (opf == vmove)
			markit(wdot);
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * +		Forward line, to first non-white.
	 *
	 * CR		Convenient synonym for +.
	 */
	case '+':
	case CR:
		wdot = dot + cnt;
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * ^N		To next line, same column if possible.
	 *
	 * LF		Linefeed is a convenient synonym for ^N.
	 */
	case CTRL(n):
	case 'j':
	case NL:
		wdot = dot + cnt;
		if (vmoving == 0)
			vmoving = 1, vmovcol = column(cursor);
		wcursor = 0;
		break;

	/*
	 * n		Search to next match of current pattern.
	 */
	case 'n':
		vglobp = vscandir;
		c = *vglobp++;
		goto nocount;

	/*
	 * N		Like n but in reverse direction.
	 */
	case 'N':
		vglobp = vscandir[0] == '/' ? "?" : "/";
		c = *vglobp++;
		goto nocount;

	/*
	 * '		Return to line specified by following mark,
	 *		first white position on line.
	 *
	 * `		Return to marked line at remembered column.
	 */
	case '\'':
	case '`':
		d = c;
		c = getesc();
		if (c == 0)
			return;
		c = markreg(c);
		forbid (c == 0);
		wdot = getmark(c);
		forbid (wdot == NOLINE);
		forbid (Xhadcnt);
		vmoving = 0;
		wcursor = d == '`' ? ncols[c - 'a'] : 0;
		if (opf == vmove && (wdot != dot || (d == '`' && wcursor != cursor)))
			markDOT();
		if (wcursor) {
			vsave();
			getline(*wdot);
			if (wcursor > strend(linebuf))
				wcursor = 0;
			getDOT();
		}
		if (ospeed > B300)
			hold |= HOLDWIG;
		break;

	/*
	 * G		Goto count'th line, or last line if no count
	 *		given.
	 */
	case 'G':
		if (!Xhadcnt)
			cnt = lineDOL();
		wdot = zero + cnt;
		forbid (wdot < one || wdot > dol);
		if (opf == vmove)
			markit(wdot);
		vmoving = 0;
		wcursor = 0;
		break;

	/*
	 * /		Scan forward for following re.
	 * ?		Scan backward for following re.
	 */
	case '/':
	case '?':
		forbid (Xhadcnt);
		vsave();
		ocurs = cursor;
		odot = dot;
		wcursor = 0;
		if (readecho(c))
			return;
		if (!vglobp)
			vscandir[0] = genbuf[0];
		oglobp = globp; CP(vutmp, genbuf); globp = vutmp;
		d = peekc;
fromsemi:
		ungetchar(0);
		fixech();
		CATCH
#ifndef CBREAK
			/*
			 * Lose typeahead (ick).
			 */
			vcook();
#endif
			addr = address(cursor);
#ifndef CBREAK
			vraw();
#endif
		ONERR
#ifndef CBREAK
			vraw();
#endif
slerr:
			globp = oglobp;
			dot = odot;
			cursor = ocurs;
			ungetchar(d);
			splitw = 0;
			vclean();
			vjumpto(dot, ocurs, 0);
			return;
		ENDCATCH
		if (globp == 0)
			globp = "";
		else if (peekc)
			--globp;
		if (*globp == ';') {
			/* /foo/;/bar/ */
			globp++;
			dot = addr;
			cursor = loc1;
			goto fromsemi;
		}
		dot = odot;
		ungetchar(d);
		c = 0;
		if (*globp == 'z')
			globp++, c = '\n';
		if (any(*globp, "^+-."))
			c = *globp++;
		i = 0;
		while (isdigit(*globp))
			i = i * 10 + *globp++ - '0';
		if (any(*globp, "^+-."))
			c = *globp++;
		if (*globp) {
			/* random junk after the pattern */
			beep();
			goto slerr;
		}
		globp = oglobp;
		splitw = 0;
		vmoving = 0;
		wcursor = loc1;
		if (i != 0)
			vsetsiz(i);
		if (opf == vmove) {
			if (state == ONEOPEN || state == HARDOPEN)
				outline = destline = WBOT;
			if (addr != dot || loc1 != cursor)
				markDOT();
			if (loc1 > linebuf && *loc1 == 0)
				loc1--;
			if (c)
				vjumpto(addr, loc1, c);
			else {
				vmoving = 0;
				if (loc1) {
					vmoving++;
					vmovcol = column(loc1);
				}
				getDOT();
				if (state == CRTOPEN && addr != dot)
					vup1();
				vupdown(addr - dot, NOSTR);
			}
			return;
		}
		lastcp[-1] = 'n';
		getDOT();
		wdot = addr;
		break;
	}
	/*
	 * Apply.
	 */
	if (vreg && wdot == 0)
		wdot = dot;
	(*opf)(c);
	wdot = NOLINE;
}

/*
 * Find single character c, in direction dir from cursor.
 */
find(c)
	char c;
{

	for(;;) {
		if (edge())
			return (0);
		wcursor += dir;
		if (*wcursor == c)
			return (1);
	}
}

/*
 * Do a word motion with operator op, and cnt more words
 * to go after this.
 */
word(op, cnt)
	register int (*op)();
	int cnt;
{
	register int which;
	register char *iwc;
	register line *iwdot = wdot;

	if (dir == 1) {
		iwc = wcursor;
		which = wordch(wcursor);
		while (wordof(which, wcursor)) {
			if (cnt == 1 && op != vmove && wcursor[1] == 0) {
				wcursor++;
				break;
			}
			if (!lnext())
				return (0);
			if (wcursor == linebuf)
				break;
		}
		/* Unless last segment of a change skip blanks */
		if (op != vchange || cnt > 1)
			while (!margin() && blank())
				wcursor++;
		else
			if (wcursor == iwc && iwdot == wdot && *iwc)
				wcursor++;
		if (op == vmove && margin())
			wcursor--;
	} else {
		if (!lnext())
			return (0);
		while (blank())
			if (!lnext())
				return (0);
		if (!margin()) {
			which = wordch(wcursor);
			while (!margin() && wordof(which, wcursor))
				wcursor--;
		}
		if (wcursor < linebuf || !wordof(which, wcursor))
			wcursor++;
	}
	return (1);
}

/*
 * To end of word, with operator op and cnt more motions
 * remaining after this.
 */
eend(op)
	register int (*op)();
{
	register int which;

	if (!lnext())
		return;
	while (blank())
		if (!lnext())
			return;
	which = wordch(wcursor);
	while (wordof(which, wcursor)) {
		if (wcursor[1] == 0) {
			wcursor++;
			break;
		}
		if (!lnext())
			return;
	}
	if (op != vchange && op != vdelete && wcursor > linebuf)
		wcursor--;
}

/*
 * Wordof tells whether the character at *wc is in a word of
 * kind which (blank/nonblank words are 0, conservative words 1).
 */
wordof(which, wc)
	char which;
	register char *wc;
{

	if (isspace(*wc))
		return (0);
	return (!wdkind || wordch(wc) == which);
}

/*
 * Wordch tells whether character at *wc is a word character
 * i.e. an alfa, digit, or underscore.
 */
wordch(wc)
	char *wc;
{
	register int c;

	c = wc[0];
	return (isalpha(c) || isdigit(c) || c == '_');
}

/*
 * Edge tells when we hit the last character in the current line.
 */
edge()
{

	if (linebuf[0] == 0)
		return (1);
	if (dir == 1)
		return (wcursor[1] == 0);
	else
		return (wcursor == linebuf);
}

/*
 * Margin tells us when we have fallen off the end of the line.
 */
margin()
{

	return (wcursor < linebuf || wcursor[0] == 0);
}
