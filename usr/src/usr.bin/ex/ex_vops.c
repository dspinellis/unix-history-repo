/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * This file defines the operation sequences which interface the
 * logical changes to the file buffer with the internal and external
 * display representations.
 */

/*
 * Undo.
 *
 * Undo is accomplished in two ways.  We often for small changes in the
 * current line know how (in terms of a change operator) how the change
 * occurred.  Thus on an intelligent terminal we can undo the operation
 * by another such operation, using insert and delete character
 * stuff.  The pointers vU[AD][12] index the buffer vutmp when this
 * is possible and provide the necessary information.
 *
 * The other case is that the change involved multiple lines or that
 * we have moved away from the line or forgotten how the change was
 * accomplished.  In this case we do a redisplay and hope that the
 * low level optimization routines (which don't look for winning
 * via insert/delete character) will not lose too badly.
 */
char	*vUA1, *vUA2;
char	*vUD1, *vUD2;

vUndo()
{

	/*
	 * Avoid UU which clobbers ability to do u.
	 */
	if (vundkind == VCAPU || vUNDdot != dot) {
		beep();
		return;
	}
	CP(vutmp, linebuf);
	vUD1 = linebuf; vUD2 = strend(linebuf);
	putmk1(dot, vUNDsav);
	getDOT();
	vUA1 = linebuf; vUA2 = strend(linebuf);
	vundkind = VCAPU;
	if (state == ONEOPEN || state == HARDOPEN) {
		vjumpto(dot, vUNDcurs, 0);
		return;
	}
	vdirty(vcline, 1);
	vsyncCL();
	vfixcurs();
}

vundo()
{
	register int cnt;
	register line *addr;
	register char *cp;
	char temp[LBSIZE];
	bool savenote;
	int (*OO)();
	short oldhold = hold;

	switch (vundkind) {

	case VMANYINS:
		wcursor = 0;
		addr1 = undap1;
		addr2 = undap2 - 1;
		vsave();
		YANKreg('1');
		notecnt = 0;
		/* fall into ... */

	case VMANY:
	case VMCHNG:
		vsave();
		addr = dot - vcline;
		notecnt = 1;
		if (undkind == UNDPUT && undap1 == undap2) {
			beep();
			break;
		}
		/*
		 * Undo() call below basically replaces undap1 to undap2-1
		 * with dol through unddol-1.  Hack screen image to
		 * reflect this replacement.
		 */
		vreplace(undap1 - addr, undap2 - undap1,
		    undkind == UNDPUT ? 0 : unddol - dol);
		savenote = notecnt;
		undo(1);
		if (vundkind != VMCHNG || addr != dot)
			killU();
		vundkind = VMANY;
		cnt = dot - addr;
		if (cnt < 0 || cnt > vcnt || state != VISUAL) {
			vjumpto(dot, NOSTR, '.');
			break;
		}
		if (!savenote)
			notecnt = 0;
		vcline = cnt;
		vrepaint(vmcurs);
		vmcurs = 0;
		break;

	case VCHNG:
	case VCAPU:
		vundkind = VCHNG;
		strcpy(temp, vutmp);
		strcpy(vutmp, linebuf);
		doomed = column(vUA2 - 1) - column(vUA1 - 1);
		strcLIN(temp);
		cp = vUA1; vUA1 = vUD1; vUD1 = cp;
		cp = vUA2; vUA2 = vUD2; vUD2 = cp;
		cursor = vUD1;
		if (state == HARDOPEN) {
			doomed = 0;
			vsave();
			vopen(dot, WBOT);
			vnline(cursor);
			break;
		}
		/*
		 * Pseudo insert command.
		 */
		vcursat(cursor);
		OO = Outchar; Outchar = vinschar; hold |= HOLDQIK;
		vprepins();
		temp[vUA2 - linebuf] = 0;
		for (cp = &temp[vUA1 - linebuf]; *cp;)
			putchar(*cp++);
		Outchar = OO; hold = oldhold;
		endim();
		physdc(cindent(), cindent() + doomed);
		doomed = 0;
		vdirty(vcline, 1);
		vsyncCL();
		if (cursor > linebuf && cursor >= strend(linebuf))
			cursor--;
		vfixcurs();
		break;

	case VNONE:
		beep();
		break;
	}
}

/*
 * Initialize undo information before an append.
 */
vnoapp()
{

	vUD1 = vUD2 = cursor;
}

/*
 * All the rest of the motion sequences have one or more
 * cases to deal with.  In the case wdot == 0, operation
 * is totally within current line, from cursor to wcursor.
 * If wdot is given, but wcursor is 0, then operation affects
 * the inclusive line range.  The hardest case is when both wdot
 * and wcursor are given, then operation affects from line dot at
 * cursor to line wdot at wcursor.
 */

/*
 * Move is simple, except for moving onto new lines in hardcopy open mode.
 */
vmove()
{
	register int cnt;

	if (wdot) {
		if (wdot < one || wdot > dol) {
			beep();
			return;
		}
		cnt = wdot - dot;
		wdot = NOLINE;
		if (cnt)
			killU();
		vupdown(cnt, wcursor);
		return;
	}

	/*
	 * When we move onto a new line, save information for U undo.
	 */
	if (vUNDdot != dot) {
		vUNDsav = *dot;
		vUNDcurs = wcursor;
		vUNDdot = dot;
	}

	/*
	 * In hardcopy open, type characters to left of cursor
	 * on new line, or back cursor up if its to left of where we are.
	 * In any case if the current line is ``rubbled'' i.e. has trashy
	 * looking overstrikes on it or \'s from deletes, we reprint
	 * so it is more comprehensible (and also because we can't work
	 * if we let it get more out of sync since column() won't work right.
	 */
	if (state == HARDOPEN) {
		register char *cp;
		if (rubble) {
			register int c;
			int oldhold = hold;

			sethard();
			cp = wcursor;
			c = *cp;
			*cp = 0;
			hold |= HOLDDOL;
			vreopen(WTOP, lineDOT(), vcline);
			hold = oldhold;
			*cp = c;
		} else if (wcursor > cursor) {
			vfixcurs();
			for (cp = cursor; *cp && cp < wcursor;) {
				register int c = *cp++ & TRIM;

				putchar(c ? c : ' ');
			}
		}
	}
	vsetcurs(wcursor);
}

/*
 * Delete operator.
 *
 * Hard case of deleting a range where both wcursor and wdot
 * are specified is treated as a special case of change and handled
 * by vchange (although vchange may pass it back if it degenerates
 * to a full line range delete.)
 */
vdelete(c)
	char c;
{
	register char *cp;
	register int i;

	if (wdot) {
		if (wcursor) {
			vchange('d');
			return;
		}
		if ((i = xdw()) < 0)
			return;
		if (state != VISUAL) {
			vgoto(LINE(0), 0);
			vputchar('@');
		}
		wdot = dot;
		vremote(i, delete, 0);
		notenam = "delete";
		DEL[0] = 0;
		killU();
		vreplace(vcline, i, 0);
		if (wdot > dol)
			vcline--;
		vrepaint(NOSTR);
		return;
	}
	if (wcursor < linebuf)
		wcursor = linebuf;
	if (cursor == wcursor) {
		beep();
		return;
	}
	i = vdcMID();
	cp = cursor;
	setDEL();
	CP(cp, wcursor);
	if (cp > linebuf && (cp[0] == 0 || c == '#'))
		cp--;
	if (state == HARDOPEN) {
		bleep(i, cp);
		cursor = cp;
		return;
	}
	physdc(column(cursor - 1), i);
	DEPTH(vcline) = 0;
	vreopen(LINE(vcline), lineDOT(), vcline);
	vsyncCL();
	vsetcurs(cp);
}

/*
 * Change operator.
 *
 * In a single line we mark the end of the changed area with '$'.
 * On multiple whole lines, we clear the lines first.
 * Across lines with both wcursor and wdot given, we delete
 * and sync then append (but one operation for undo).
 */
vchange(c)
	char c;
{
	register char *cp;
	register int i, ind, cnt;
	line *addr;

	if (wdot) {
		/*
		 * Change/delete of lines or across line boundaries.
		 */
		if ((cnt = xdw()) < 0)
			return;
		getDOT();
		if (wcursor && cnt == 1) {
			/*
			 * Not really.
			 */
			wdot = 0;
			if (c == 'd') {
				vdelete(c);
				return;
			}
			goto smallchange;
		}
		if (cursor && wcursor) {
			/*
			 * Across line boundaries, but not
			 * necessarily whole lines.
			 * Construct what will be left.
			 */
			*cursor = 0;
			strcpy(genbuf, linebuf);
			getline(*wdot);
			if (strlen(genbuf) + strlen(wcursor) > LBSIZE - 2) {
				getDOT();
				beep();
				return;
			}
			strcat(genbuf, wcursor);
			if (c == 'd' && *vpastwh(genbuf) == 0) {
				/*
				 * Although this is a delete
				 * spanning line boundaries, what
				 * would be left is all white space,
				 * so take it all away.
				 */
				wcursor = 0;
				getDOT();
				op = 0;
				notpart(lastreg);
				notpart('1');
				vdelete(c);
				return;
			}
			ind = -1;
		} else if (c == 'd' && wcursor == 0) {
			vdelete(c);
			return;
		} else
#ifdef LISPCODE
			/*
			 * We are just substituting text for whole lines,
			 * so determine the first autoindent.
			 */
			if (value(LISP) && value(AUTOINDENT))
				ind = lindent(dot);
			else
#endif
				ind = whitecnt(linebuf);
		i = vcline >= 0 ? LINE(vcline) : WTOP;

		/*
		 * Delete the lines from the buffer,
		 * and remember how the partial stuff came about in
		 * case we are told to put.
		 */
		addr = dot;
		vremote(cnt, delete, 0);
		setpk();
		notenam = "delete";
		if (c != 'd')
			notenam = "change";
		/*
		 * If DEL[0] were nonzero, put would put it back
		 * rather than the deleted lines.
		 */
		DEL[0] = 0;
		if (cnt > 1)
			killU();

		/*
		 * Now hack the screen image coordination.
		 */
		vreplace(vcline, cnt, 0);
		wdot = NOLINE;
		noteit(0);
		vcline--;
		if (addr <= dol)
			dot--;

		/*
		 * If this is a across line delete/change,
		 * cursor stays where it is; just splice together the pieces
		 * of the new line.  Otherwise generate a autoindent
		 * after a S command.
		 */
		if (ind >= 0) {
			*genindent(ind) = 0;
			vdoappend(genbuf);
		} else {
			vmcurs = cursor;
			strcLIN(genbuf);
			vdoappend(linebuf);
		}

		/*
		 * Indicate a change on hardcopies by
		 * erasing the current line.
		 */
		if (c != 'd' && state != VISUAL && state != HARDOPEN) {
			int oldhold = hold;

			hold |= HOLDAT, vclrlin(i, dot), hold = oldhold;
		}

		/*
		 * Open the line (logically) on the screen, and 
		 * update the screen tail.  Unless we are really a delete
		 * go off and gather up inserted characters.
		 */
		vcline++;
		if (vcline < 0)
			vcline = 0;
		vopen(dot, i);
		vsyncCL();
		noteit(1);
		if (c != 'd') {
			if (ind >= 0) {
				cursor = linebuf;
				linebuf[0] = 0;
				vfixcurs();
			} else {
				ind = 0;
				vcursat(cursor);
			}
			vappend('x', 1, ind);
			return;
		}
		if (*cursor == 0 && cursor > linebuf)
			cursor--;
		vrepaint(cursor);
		return;
	}

smallchange:
	/*
	 * The rest of this is just low level hacking on changes
	 * of small numbers of characters.
	 */
	if (wcursor < linebuf)
		wcursor = linebuf;
	if (cursor == wcursor) {
		beep();
		return;
	}
	i = vdcMID();
	cp = cursor;
	if (state != HARDOPEN)
		vfixcurs();

	/*
	 * Put out the \\'s indicating changed text in hardcopy,
	 * or mark the end of the change with $ if not hardcopy.
	 */
	if (state == HARDOPEN) 
		bleep(i, cp);
	else {
		vcursbef(wcursor);
		putchar('$');
		i = cindent();
	}

	/*
	 * Remember the deleted text for possible put,
	 * and then prepare and execute the input portion of the change.
	 */
	cursor = cp;
	setDEL();
	CP(cursor, wcursor);
	if (state != HARDOPEN) {
		vcursaft(cursor - 1);
		doomed = i - cindent();
	} else {
/*
		sethard();
		wcursor = cursor;
		cursor = linebuf;
		vgoto(outline, value(NUMBER) << 3);
		vmove();
*/
		doomed = 0;
	}
	prepapp();
	vappend('c', 1, 0);
}

/*
 * Open new lines.
 *
 * Tricky thing here is slowopen.  This causes display updating
 * to be held off so that 300 baud dumb terminals don't lose badly.
 * This also suppressed counts, which otherwise say how many blank
 * space to open up.  Counts are also suppressed on intelligent terminals.
 * Actually counts are obsoleted, since if your terminal is slow
 * you are better off with slowopen.
 */
voOpen(c, cnt)
	char c;
	register int cnt;
{
	register int ind = 0, i;
	short oldhold = hold;

	if (value(SLOWOPEN) || value(REDRAW) && AL && DL)
		cnt = 1;
	vsave();
	setLAST();
	if (value(AUTOINDENT))
		ind = whitecnt(linebuf);
	if (c == 'O') {
		vcline--;
		dot--;
		if (dot > zero)
			getDOT();
	}
	if (value(AUTOINDENT)) {
#ifdef LISPCODE
		if (value(LISP))
			ind = lindent(dot + 1);
#endif
	}
	killU();
	prepapp();
	if (FIXUNDO)
		vundkind = VMANY;
	if (state != VISUAL)
		c = WBOT + 1;
	else {
		c = vcline < 0 ? WTOP - cnt : LINE(vcline) + DEPTH(vcline);
		if (c < ZERO)
			c = ZERO;
		i = LINE(vcline + 1) - c;
		if (i < cnt && c <= WBOT && (!AL || !DL))
			vinslin(c, cnt - i, vcline);
	}
	*genindent(ind) = 0;
	vdoappend(genbuf);
	vcline++;
	oldhold = hold;
	hold |= HOLDROL;
	vopen(dot, c);
	hold = oldhold;
	if (value(SLOWOPEN))
		/*
		 * Oh, so lazy!
		 */
		vscrap();
	else
		vsync1(LINE(vcline));
	cursor = linebuf;
	linebuf[0] = 0;
	vappend('o', 1, ind);
}

/*
 * > < and = shift operators.
 *
 * Note that =, which aligns lisp, is just a ragged sort of shift,
 * since it never distributes text between lines.
 */
char	vshnam[2] = { 'x', 0 };

vshftop()
{
	register line *addr;
	register int cnt;

	if ((cnt = xdw()) < 0)
		return;
	addr = dot;
	vremote(cnt, vshift, 0);
	vshnam[0] = op;
	notenam = vshnam;
	dot = addr;
	vreplace(vcline, cnt, cnt);
	if (state == HARDOPEN)
		vcnt = 0;
	vrepaint(NOSTR);
}

/*
 * !.
 *
 * Filter portions of the buffer through unix commands.
 */
vfilter()
{
	register line *addr;
	register int cnt;
	char *oglobp, d;

	if ((cnt = xdw()) < 0)
		return;
	if (vglobp)
		vglobp = uxb;
	if (readecho('!'))
		return;
	oglobp = globp; globp = genbuf + 1;
	d = peekc; ungetchar(0);
	CATCH
		fixech();
		unix0(0);
	ONERR
		splitw = 0;
		ungetchar(d);
		vrepaint(cursor);
		globp = oglobp;
		return;
	ENDCATCH
	ungetchar(d); globp = oglobp;
	addr = dot;
	CATCH
		vgoto(WECHO, 0); flusho();
		vremote(cnt, filter, 2);
	ONERR
		vdirty(0, LINES);
	ENDCATCH
	if (dot == zero && dol > zero)
		dot = one;
	splitw = 0;
	notenam = "";
	/*
	 * BUG: we shouldn't be depending on what undap2 and undap1 are,
	 * since we may be inside a macro.  What's really wanted is the
	 * number of lines we read from the filter.  However, the mistake
	 * will be an overestimate so it only results in extra work,
	 * it shouldn't cause any real screwups.
	 */
	vreplace(vcline, cnt, undap2 - undap1);
	dot = addr;
	if (dot > dol) {
		dot--;
		vcline--;
	}
	vrepaint(NOSTR);
}

/*
 * Xdw exchanges dot and wdot if appropriate and also checks
 * that wdot is reasonable.  Its name comes from
 *	xchange dotand wdot
 */
xdw()
{
	register char *cp;
	register int cnt;
/*
	register int notp = 0;
 */

	if (wdot == NOLINE || wdot < one || wdot > dol) {
		beep();
		return (-1);
	}
	vsave();
	setLAST();
	if (dot > wdot) {
		register line *addr;

		vcline -= dot - wdot;
		addr = dot; dot = wdot; wdot = addr;
		cp = cursor; cursor = wcursor; wcursor = cp;
	}
	/*
	 * If a region is specified but wcursor is at the begining
	 * of the last line, then we move it to be the end of the
	 * previous line (actually off the end).
	 */
	if (cursor && wcursor == linebuf && wdot > dot) {
		wdot--;
		getDOT();
		if (vpastwh(linebuf) >= cursor)
			wcursor = 0;
		else {
			getline(*wdot);
			wcursor = strend(linebuf);
			getDOT();
		}
		/*
		 * Should prepare in caller for possible dot == wdot.
		 */
	}
	cnt = wdot - dot + 1;
	if (vreg) {
		vremote(cnt, YANKreg, vreg);
/*
		if (notp)
			notpart(vreg);
 */
	}

	/*
	 * Kill buffer code.  If delete operator is c or d, then save
	 * the region in numbered buffers.
	 *
	 * BUG:			This may be somewhat inefficient due
	 *			to the way named buffer are implemented,
	 *			necessitating some optimization.
	 */
	vreg = 0;
	if (any(op, "cd")) {
		vremote(cnt, YANKreg, '1');
/*
		if (notp)
			notpart('1');
 */
	}
	return (cnt);
}

/*
 * Routine for vremote to call to implement shifts.
 */
vshift()
{

	shift(op, 1);
}

/*
 * Replace a single character with the next input character.
 * A funny kind of insert.
 */
vrep(cnt)
	register int cnt;
{
	register int i, c;

	if (cnt > strlen(cursor)) {
		beep();
		return;
	}
	i = column(cursor + cnt - 1);
	vcursat(cursor);
	doomed = i - cindent();
	if (!vglobp) {
		c = getesc();
		if (c == 0) {
			vfixcurs();
			return;
		}
		ungetkey(c);
	}
	CP(vutmp, linebuf);
	if (FIXUNDO)
		vundkind = VCHNG;
	wcursor = cursor + cnt;
	vUD1 = cursor; vUD2 = wcursor;
	CP(cursor, wcursor);
	prepapp();
	vappend('r', cnt, 0);
	*lastcp++ = INS[0];
	setLAST();
}

/*
 * Yank.
 *
 * Yanking to string registers occurs for free (essentially)
 * in the routine xdw().
 */
vyankit()
{
	register int cnt;

	if (wdot) {
		if ((cnt = xdw()) < 0)
			return;
		vremote(cnt, yank, 0);
		setpk();
		notenam = "yank";
		if (FIXUNDO)
			vundkind = VNONE;
		DEL[0] = 0;
		wdot = NOLINE;
		if (notecnt <= vcnt - vcline && notecnt < value(REPORT))
			notecnt = 0;
		vrepaint(cursor);
		return;
	}
	takeout(DEL);
}

/*
 * Set pkill variables so a put can
 * know how to put back partial text.
 * This is necessary because undo needs the complete
 * line images to be saved, while a put wants to trim
 * the first and last lines.  The compromise
 * is for put to be more clever.
 */
setpk()
{

	if (wcursor) {
		pkill[0] = cursor;
		pkill[1] = wcursor;
	}
}
