/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_vput.c	7.4 (Berkeley) %G%";
#endif not lint

#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * Deal with the screen, clearing, cursor positioning, putting characters
 * into the screen image, and deleting characters.
 * Really hard stuff here is utilizing insert character operations
 * on intelligent terminals which differs widely from terminal to terminal.
 */
vclear()
{

#ifdef ADEBUG
	if (trace)
		tfixnl(), fprintf(trace, "------\nvclear\n");
#endif
	tputs(CL, LINES, putch);
	destcol = 0;
	outcol = 0;
	destline = 0;
	outline = 0;
	if (inopen)
		vclrbyte(vtube0, WCOLS * (WECHO - ZERO + 1));
}

/*
 * Clear memory.
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
 * Clear a physical display line, high level.
 */
vclrlin(l, tp)
	int l;
	line *tp;
{

	vigoto(l, 0);
	if ((hold & HOLDAT) == 0)
		putchar(tp > dol ? ((UPPERCASE || HZ) ? '^' : '~') : '@');
	if (state == HARDOPEN)
		sethard();
	vclreol();
}

/*
 * Clear to the end of the current physical line
 */
vclreol()
{
	register int i, j;
	register char *tp;

	if (destcol == WCOLS)
		return;
	destline += destcol / WCOLS;
	destcol %= WCOLS;
	if (destline < 0 || destline > WECHO)
		error("Internal error: vclreol");
	i = WCOLS - destcol;
	tp = vtube[destline] + destcol;
	if (CE) {
		if (IN && *tp || !ateopr()) {
			vcsync();
			vputp(CE, 1);
		}
		vclrbyte(tp, i);
		return;
	}
	if (*tp == 0)
		return;
	while (i > 0 && (j = *tp & (QUOTE|TRIM))) {
		if (j != ' ' && (j & QUOTE) == 0) {
			destcol = WCOLS - i;
			vputchar(' ');
		}
		--i, *tp++ = 0;
	}
}

/*
 * Clear the echo line.
 * If didphys then its been cleared physically (as
 * a side effect of a clear to end of display, e.g.)
 * so just do it logically.
 * If work here is being held off, just remember, in
 * heldech, if work needs to be done, don't do anything.
 */
vclrech(didphys)
	bool didphys;
{

	if (Peekkey == ATTN)
		return;
	if (hold & HOLDECH) {
		heldech = !didphys;
		return;
	}
	if (!didphys && (CD || CE)) {
		splitw++;
		/*
		 * If display is retained below, then MUST use CD or CE
		 * since we don't really know whats out there.
		 * Vigoto might decide (incorrectly) to do nothing.
		 */
		if (DB) {
			vgoto(WECHO, 0);
			vputp(CD ? CD : CE, 1);
		} else {
			if (XT) {
				/*
				 * This code basically handles the t1061
				 * where positioning at (0, 0) won't work
				 * because the terminal won't let you put
				 * the cursor on it's magic cookie.
				 *
				 * Should probably be XS above, or even a
				 * new X? glitch, but right now t1061 is the
				 * only terminal with XT.
				 */
				vgoto(WECHO, 0);
				vputp(DL, 1);
			} else {
				vigoto(WECHO, 0);
				vclreol();
			}
		}
		splitw = 0;
		didphys = 1;
	}
	if (didphys)
		vclrbyte(vtube[WECHO], WCOLS);
	heldech = 0;
}

/*
 * Fix the echo area for use, setting
 * the state variable splitw so we wont rollup
 * when we move the cursor there.
 */
fixech()
{

	splitw++;
	if (state != VISUAL && state != CRTOPEN) {
		vclean();
		vcnt = 0;
	}
	vgoto(WECHO, 0); flusho();
}

/*
 * Put the cursor ``before'' cp.
 */
vcursbef(cp)
	register char *cp;
{

	if (cp <= linebuf)
		vgotoCL(value(NUMBER) << 3);
	else
		vgotoCL(column(cp - 1) - 1);
}

/*
 * Put the cursor ``at'' cp.
 */
vcursat(cp)
	register char *cp;
{

	if (cp <= linebuf && linebuf[0] == 0)
		vgotoCL(value(NUMBER) << 3);
	else
		vgotoCL(column(cp - 1));
}

/*
 * Put the cursor ``after'' cp.
 */
vcursaft(cp)
	register char *cp;
{

	vgotoCL(column(cp));
}

/*
 * Fix the cursor to be positioned in the correct place
 * to accept a command.
 */
vfixcurs()
{

	vsetcurs(cursor);
}

/*
 * Compute the column position implied by the cursor at ``nc'',
 * and move the cursor there.
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
 * Move the cursor invisibly, i.e. only remember to do it.
 */
vigoto(y, x)
	int y, x;
{

	destline = y;
	destcol = x;
}

/*
 * Move the cursor to the position implied by any previous
 * vigoto (or low level hacking with destcol/destline as in readecho).
 */
vcsync()
{

	vgoto(destline, destcol);
}

/*
 * Goto column x of the current line.
 */
vgotoCL(x)
	register int x;
{

	if (splitw)
		vgoto(WECHO, x);
	else
		vgoto(LINE(vcline), x);
}

/*
 * Invisible goto column x of current line.
 */
vigotoCL(x)
	register int x;
{

	if (splitw)
		vigoto(WECHO, x);
	else
		vigoto(LINE(vcline), x);
}

/*
 * Move cursor to line y, column x, handling wraparound and scrolling.
 */
vgoto(y, x)
	register int y, x;
{
	register char *tp;
	register int c;

	/*
	 * Fold the possibly too large value of x.
	 */
	if (x >= WCOLS) {
		y += x / WCOLS;
		x %= WCOLS;
	}
	if (y < 0)
		error("Internal error: vgoto");
	if (outcol >= WCOLS) {
		if (AM) {
			outline += outcol / WCOLS;
			outcol %= WCOLS;
		} else
			outcol = WCOLS - 1;
	}

	/*
	 * In a hardcopy or glass crt open, print the stuff
	 * implied by a motion, or backspace.
	 */
	if (state == HARDOPEN || state == ONEOPEN) {
		if (y != outline)
			error("Line too long for open");
		if (x + 1 < outcol - x || (outcol > x && !BS))
			destcol = 0, fgoto();
		tp = vtube[WBOT] + outcol;
		while (outcol != x)
			if (outcol < x) {
				if (*tp == 0)
					*tp = ' ';
				c = *tp++ & TRIM;
				vputc(c && (!OS || EO) ? c : ' '), outcol++;
			} else {
				if (BC)
					vputp(BC, 0);
				else
					vputc('\b');
				outcol--;
			}
		destcol = outcol = x;
		destline = outline;
		return;
	}

	/*
	 * If the destination position implies a scroll, do it.
	 */
	destline = y;
	if (destline > WBOT && (!splitw || destline > WECHO)) {
		endim();
		vrollup(destline);
	}

	/*
	 * If there really is a motion involved, do it.
	 * The check here is an optimization based on profiling.
	 */
	destcol = x;
	if ((destline - outline) * WCOLS != destcol - outcol) {
		if (!MI)
			endim();
		fgoto();
	}
}

/*
 * This is the hardest code in the editor, and deals with insert modes
 * on different kinds of intelligent terminals.  The complexity is due
 * to the cross product of three factors:
 *
 *	1. Lines may display as more than one segment on the screen.
 *	2. There are 2 kinds of intelligent terminal insert modes.
 *	3. Tabs squash when you insert characters in front of them,
 *	   in a way in which current intelligent terminals don't handle.
 *
 * The two kinds of terminals are typified by the DM2500 or HP2645 for
 * one and the CONCEPT-100 or the FOX for the other.
 *
 * The first (HP2645) kind has an insert mode where the characters
 * fall off the end of the line and the screen is shifted rigidly
 * no matter how the display came about.
 *
 * The second (CONCEPT-100) kind comes from terminals which are designed
 * for forms editing and which distinguish between blanks and ``spaces''
 * on the screen, spaces being like blank, but never having had
 * and data typed into that screen position (since, e.g. a clear operation
 * like clear screen).  On these terminals, when you insert a character,
 * the characters from where you are to the end of the screen shift
 * over till a ``space'' is found, and the null character there gets
 * eaten up.
 *
 *
 * The code here considers the line as consisting of several parts
 * the first part is the ``doomed'' part, i.e. a part of the line
 * which is being typed over.  Next comes some text up to the first
 * following tab.  The tab is the next segment of the line, and finally
 * text after the tab.
 *
 * We have to consider each of these segments and the effect of the
 * insertion of a character on them.  On terminals like HP2645's we
 * must simulate a multi-line insert mode using the primitive one
 * line insert mode.  If we are inserting in front of a tab, we have
 * to either delete characters from the tab or insert white space
 * (when the tab reaches a new spot where it gets larger) before we
 * insert the new character.
 *
 * On a terminal like a CONCEPT our strategy is to make all
 * blanks be displayed, while trying to keep the screen having ``spaces''
 * for portions of tabs.  In this way the terminal hardward does some
 * of the hacking for compression of tabs, although this tends to
 * disappear as you work on the line and spaces change into blanks.
 *
 * There are a number of boundary conditions (like typing just before
 * the first following tab) where we can avoid a lot of work.  Most
 * of them have to be dealt with explicitly because performance is
 * much, much worse if we don't.
 *
 * A final thing which is hacked here is two flavors of insert mode.
 * Datamedia's do this by an insert mode which you enter and leave
 * and by having normal motion character operate differently in this
 * mode, notably by having a newline insert a line on the screen in
 * this mode.  This generally means it is unsafe to move around
 * the screen ignoring the fact that we are in this mode.
 * This is possible on some terminals, and wins big (e.g. HP), so
 * we encode this as a ``can move in insert capability'' mi,
 * and terminals which have it can do insert mode with much less
 * work when tabs are present following the cursor on the current line.
 */

/*
 * Routine to expand a tab, calling the normal Outchar routine
 * to put out each implied character.  Note that we call outchar
 * with a QUOTE.  We use QUOTE internally to represent a position
 * which is part of the expansion of a tab.
 */
vgotab()
{
	register int i = tabcol(destcol, value(TABSTOP)) - destcol;

	do
		(*Outchar)(QUOTE);
	while (--i);
}

/*
 * Variables for insert mode.
 */
int	linend;			/* The column position of end of line */
int	tabstart;		/* Column of start of first following tab */
int	tabend;			/* Column of end of following tabs */
int	tabsize;		/* Size of the following tabs */
int	tabslack;		/* Number of ``spaces'' in following tabs */
int	inssiz;			/* Number of characters to be inserted */
int	inscol;			/* Column where insertion is taking place */
int	shft;			/* Amount tab expansion shifted rest of line */
int	slakused;		/* This much of tabslack will be used up */

/*
 * This routine MUST be called before insert mode is run,
 * and brings all segments of the current line to the top
 * of the screen image buffer so it is easier for us to
 * maniuplate them.
 */
vprepins()
{
	register int i;
	register char *cp = vtube0;

	for (i = 0; i < DEPTH(vcline); i++) {
		vmaktop(LINE(vcline) + i, cp);
		cp += WCOLS;
	}
}

vmaktop(p, cp)
	register int p;
	char *cp;
{
	register int i;
	char temp[TUBECOLS];

	if (p < 0 || vtube[p] == cp)
		return;
	for (i = ZERO; i <= WECHO; i++)
		if (vtube[i] == cp) {
			copy(temp, vtube[i], WCOLS);
			copy(vtube[i], vtube[p], WCOLS);
			copy(vtube[p], temp, WCOLS);
			vtube[i] = vtube[p];
			vtube[p] = cp;
			return;
		}
	error("Line too long");
}

/*
 * Insert character c at current cursor position.
 * Multi-character inserts occur only as a result
 * of expansion of tabs (i.e. inssize == 1 except
 * for tabs) and code assumes this in several place
 * to make life simpler.
 */
vinschar(c)
	int c;		/* mjm: char --> int */
{
	register int i;
	register char *tp;

	if ((!IM || !EI) && ((hold & HOLDQIK) || !value(REDRAW) || value(SLOWOPEN))) {
		/*
		 * Don't want to try to use terminal
		 * insert mode, or to try to fake it.
		 * Just put the character out; the screen
		 * will probably be wrong but we will fix it later.
		 */
		if (c == '\t') {
			vgotab();
			return;
		}
		vputchar(c);
		if (DEPTH(vcline) * WCOLS + !value(REDRAW) >
		    (destline - LINE(vcline)) * WCOLS + destcol)
			return;
		/*
		 * The next line is about to be clobbered
		 * make space for another segment of this line
		 * (on an intelligent terminal) or just remember
		 * that next line was clobbered (on a dumb one
		 * if we don't care to redraw the tail.
		 */
		if (AL) {
			vnpins(0);
		} else {
			c = LINE(vcline) + DEPTH(vcline);
			if (c < LINE(vcline + 1) || c > WBOT)
				return;
			i = destcol;
			vinslin(c, 1, vcline);
			DEPTH(vcline)++;
			vigoto(c, i);
			vprepins();
		}
		return;
	}
	/*
	 * Compute the number of positions in the line image of the
	 * current line.  This is done from the physical image
	 * since that is faster.  Note that we have no memory
	 * from insertion to insertion so that routines which use
	 * us don't have to worry about moving the cursor around.
	 */
	if (*vtube0 == 0)
		linend = 0;
	else {
		/*
		 * Search backwards for a non-null character
		 * from the end of the displayed line.
		 */
		i = WCOLS * DEPTH(vcline);
		if (i == 0)
			i = WCOLS;
		tp = vtube0 + i;
		while (*--tp == 0)
			if (--i == 0)
				break;
		linend = i;
	}

	/*
	 * We insert at a position based on the physical location
	 * of the output cursor.
	 */
	inscol = destcol + (destline - LINE(vcline)) * WCOLS;
	if (c == '\t') {
		/*
		 * Characters inserted from a tab must be
		 * remembered as being part of a tab, but we can't
		 * use QUOTE here since we really need to print blanks.
		 * QUOTE|' ' is the representation of this.
		 */
		inssiz = tabcol(inscol, value(TABSTOP)) - inscol;
		c = ' ' | QUOTE;
	} else
		inssiz = 1;

	/*
	 * If the text to be inserted is less than the number
	 * of doomed positions, then we don't need insert mode,
	 * rather we can just typeover.
	 */
	if (inssiz <= doomed) {
		endim();
		if (inscol != linend)
			doomed -= inssiz;
		do
			vputchar(c);
		while (--inssiz);
		return;
	}

	/*
	 * Have to really do some insertion, thus
	 * stake out the bounds of the first following
	 * group of tabs, computing starting position,
	 * ending position, and the number of ``spaces'' therein
	 * so we can tell how much it will squish.
	 */
	tp = vtube0 + inscol;
	for (i = inscol; i < linend; i++)
		if (*tp++ & QUOTE) {
			--tp;
			break;
		}
	tabstart = tabend = i;
	tabslack = 0;
	while (tabend < linend) {
		i = *tp++;
		if ((i & QUOTE) == 0)
			break;
		if ((i & TRIM) == 0)
			tabslack++;
		tabsize++;
		tabend++;
	}
	tabsize = tabend - tabstart;

	/*
	 * For HP's and DM's, e.g. tabslack has no meaning.
	 */
	if (!IN)
		tabslack = 0;
#ifdef IDEBUG
	if (trace) {
		fprintf(trace, "inscol %d, inssiz %d, tabstart %d, ",
			inscol, inssiz, tabstart);
		fprintf(trace, "tabend %d, tabslack %d, linend %d\n",
			tabend, tabslack, linend);
	}
#endif

	/*
	 * The real work begins.
	 */
	slakused = 0;
	shft = 0;
	if (tabsize) {
		/*
		 * There are tabs on this line.
		 * If they need to expand, then the rest of the line
		 * will have to be shifted over.  In this case,
		 * we will need to make sure there are no ``spaces''
		 * in the rest of the line (on e.g. CONCEPT-100)
		 * and then grab another segment on the screen if this
		 * line is now deeper.  We then do the shift
		 * implied by the insertion.
		 */
		if (inssiz >= doomed + tabcol(tabstart, value(TABSTOP)) - tabstart) {
			if (IN)
				vrigid();
			vneedpos(value(TABSTOP));
			vishft();
		}
	} else if (inssiz > doomed)
		/*
		 * No tabs, but line may still get deeper.
		 */
		vneedpos(inssiz - doomed);
	/*
	 * Now put in the inserted characters.
	 */
	viin(c);

	/*
	 * Now put the cursor in its final resting place.
	 */
	destline = LINE(vcline);
	destcol = inscol + inssiz;
	vcsync();
}

/*
 * Rigidify the rest of the line after the first
 * group of following tabs, typing blanks over ``spaces''.
 */
vrigid()
{
	register int col;
	register char *tp = vtube0 + tabend;

	for (col = tabend; col < linend; col++)
		if ((*tp++ & TRIM) == 0) {
			endim();
			vgotoCL(col);
			vputchar(' ' | QUOTE);
		}
}

/*
 * We need cnt more positions on this line.
 * Open up new space on the screen (this may in fact be a
 * screen rollup).
 *
 * On a dumb terminal we may infact redisplay the rest of the
 * screen here brute force to keep it pretty.
 */
vneedpos(cnt)
	int cnt;
{
	register int d = DEPTH(vcline);
	register int rmdr = d * WCOLS - linend;

	if (cnt <= rmdr - IN)
		return;
	endim();
	vnpins(1);
}

vnpins(dosync)
	int dosync;
{
	register int d = DEPTH(vcline);
	register int e;

	e = LINE(vcline) + DEPTH(vcline);
	if (e < LINE(vcline + 1)) {
		vigoto(e, 0);
		vclreol();
		return;
	}
	DEPTH(vcline)++;
	if (e < WECHO) {
		e = vglitchup(vcline, d);
		vigoto(e, 0); vclreol();
		if (dosync) {
			int (*Ooutchar)() = Outchar;
			Outchar = vputchar;
			vsync(e + 1);
			Outchar = Ooutchar;
		}
	} else {
		vup1();
		vigoto(WBOT, 0);
		vclreol();
	}
	vprepins();
}

/*
 * Do the shift of the next tabstop implied by
 * insertion so it expands.
 */
vishft()
{
	int tshft = 0;
	int j;
	register int i;
	register char *tp = vtube0;
	register char *up;
	short oldhold = hold;

	shft = value(TABSTOP);
	hold |= HOLDPUPD;
	if (!IM && !EI) {
		/*
		 * Dumb terminals are easy, we just have
		 * to retype the text.
		 */
		vigotoCL(tabend + shft);
		up = tp + tabend;
		for (i = tabend; i < linend; i++)
			vputchar(*up++);
	} else if (IN) {
		/*
		 * CONCEPT-like terminals do most of the work for us,
		 * we don't have to muck with simulation of multi-line
		 * insert mode.  Some of the shifting may come for free
		 * also if the tabs don't have enough slack to take up
		 * all the inserted characters.
		 */
		i = shft;
		slakused = inssiz - doomed;
		if (slakused > tabslack) {
			i -= slakused - tabslack;
			slakused -= tabslack;
		}
		if (i > 0 && tabend != linend) {
			tshft = i;
			vgotoCL(tabend);
			goim();
			do
				vputchar(' ' | QUOTE);
			while (--i);
		}
	} else {
		/*
		 * HP and Datamedia type terminals have to have multi-line
		 * insert faked.  Hack each segment after where we are
		 * (going backwards to where we are.)  We then can
		 * hack the segment where the end of the first following
		 * tab group is.
		 */
		for (j = DEPTH(vcline) - 1; j > (tabend + shft) / WCOLS; j--) {
			vgotoCL(j * WCOLS);
			goim();
			up = tp + j * WCOLS - shft;
			i = shft;
			do {
				if (*up)
					vputchar(*up++);
				else
					break;
			} while (--i);
		}
		vigotoCL(tabstart);
		i = shft - (inssiz - doomed);
		if (i > 0) {
			tabslack = inssiz - doomed;
			vcsync();
			goim();
			do
				vputchar(' ');
			while (--i);
		}
	}
	/*
	 * Now do the data moving in the internal screen
	 * image which is common to all three cases.
	 */
	tp += linend;
	up = tp + shft;
	i = linend - tabend;
	if (i > 0)
		do
			*--up = *--tp;
		while (--i);
	if (IN && tshft) {
		i = tshft;
		do
			*--up = ' ' | QUOTE;
		while (--i);
	}
	hold = oldhold;
}

/*
 * Now do the insert of the characters (finally).
 */
viin(c)
	int c;		/* mjm: char --> int */
{
	register char *tp, *up;
	register int i, j;
	register bool noim = 0;
	int remdoom;
	short oldhold = hold;

	hold |= HOLDPUPD;
	if (tabsize && (IM && EI) && inssiz - doomed > tabslack)
		/*
		 * There is a tab out there which will be affected
		 * by the insertion since there aren't enough doomed
		 * characters to take up all the insertion and we do
		 * have insert mode capability.
		 */
		if (inscol + doomed == tabstart) {
			/*
			 * The end of the doomed characters sits right at the
			 * start of the tabs, then we don't need to use insert
			 * mode; unless the tab has already been expanded
			 * in which case we MUST use insert mode.
			 */
			slakused = 0;
			noim = !shft;
		} else {
			/*
			 * The last really special case to handle is case
			 * where the tab is just sitting there and doesn't
			 * have enough slack to let the insertion take
			 * place without shifting the rest of the line
			 * over.  In this case we have to go out and
			 * delete some characters of the tab before we start
			 * or the answer will be wrong, as the rest of the
			 * line will have been shifted.  This code means
			 * that terminals with only insert chracter (no
			 * delete character) won't work correctly.
			 */
			i = inssiz - doomed - tabslack - slakused;
			i %= value(TABSTOP);
			if (i > 0) {
				vgotoCL(tabstart);
				godm();
				for (i = inssiz - doomed - tabslack; i > 0; i--)
					vputp(DC, DEPTH(vcline));
				enddm();
			}
		}

	/* 
	 * Now put out the characters of the actual insertion.
	 */
	vigotoCL(inscol);
	remdoom = doomed;
	for (i = inssiz; i > 0; i--) {
		if (remdoom > 0) {
			remdoom--;
			endim();
		} else if (noim)
			endim();
		else if (IM && EI) {
			vcsync();
			goim();
		}
		vputchar(c);
	}

	if (!IM || !EI) {
		/*
		 * We are a dumb terminal; brute force update
		 * the rest of the line; this is very much an n^^2 process,
		 * and totally unreasonable at low speed.
		 *
		 * You asked for it, you get it.
		 */
		tp = vtube0 + inscol + doomed;
		for (i = inscol + doomed; i < tabstart; i++)
			vputchar(*tp++);
		hold = oldhold;
		vigotoCL(tabstart + inssiz - doomed);
		for (i = tabsize - (inssiz - doomed) + shft; i > 0; i--)
			vputchar(' ' | QUOTE);
	} else {
		if (!IN) {
			/*
			 * On terminals without multi-line
			 * insert in the hardware, we must go fix the segments
			 * between the inserted text and the following
			 * tabs, if they are on different lines.
			 *
			 * Aaargh.
			 */
			tp = vtube0;
			for (j = (inscol + inssiz - 1) / WCOLS + 1;
			    j <= (tabstart + inssiz - doomed - 1) / WCOLS; j++) {
				vgotoCL(j * WCOLS);
				i = inssiz - doomed;
				up = tp + j * WCOLS - i;
				goim();
				do
					vputchar(*up++);
				while (--i && *up);
			}
		} else {
			/*
			 * On terminals with multi line inserts,
			 * life is simpler, just reflect eating of
			 * the slack.
			 */
			tp = vtube0 + tabend;
			for (i = tabsize - (inssiz - doomed); i >= 0; i--) {
				if ((*--tp & (QUOTE|TRIM)) == QUOTE) {
					--tabslack;
					if (tabslack >= slakused)
						continue;
				}
				*tp = ' ' | QUOTE;
			}
		}
		/*
		 * Blank out the shifted positions to be tab positions.
		 */
		if (shft) {
			tp = vtube0 + tabend + shft;
			for (i = tabsize - (inssiz - doomed) + shft; i > 0; i--)
				if ((*--tp & QUOTE) == 0)
					*tp = ' ' | QUOTE;
		}
	}

	/*
	 * Finally, complete the screen image update
	 * to reflect the insertion.
	 */
	hold = oldhold;
	tp = vtube0 + tabstart; up = tp + inssiz - doomed;
	for (i = tabstart; i > inscol + doomed; i--)
		*--up = *--tp;
	for (i = inssiz; i > 0; i--)
		*--up = c;
	doomed = 0;
}

/*
 * Go into ``delete mode''.  If the
 * sequence which goes into delete mode
 * is the same as that which goes into insert
 * mode, then we are in delete mode already.
 */
godm()
{

	if (insmode) {
		if (eq(DM, IM))
			return;
		endim();
	}
	vputp(DM, 0);
}

/*
 * If we are coming out of delete mode, but
 * delete and insert mode end with the same sequence,
 * it wins to pretend we are now in insert mode,
 * since we will likely want to be there again soon
 * if we just moved over to delete space from part of
 * a tab (above).
 */
enddm()
{

	if (eq(DM, IM)) {
		insmode = 1;
		return;
	}
	vputp(ED, 0);
}

/*
 * In and out of insert mode.
 * Note that the code here demands that there be
 * a string for insert mode (the null string) even
 * if the terminal does all insertions a single character
 * at a time, since it branches based on whether IM is null.
 */
goim()
{

	if (!insmode)
		vputp(IM, 0);
	insmode = 1;
}

endim()
{

	if (insmode) {
		vputp(EI, 0);
		insmode = 0;
	}
}

/*
 * Put the character c on the screen at the current cursor position.
 * This routine handles wraparound and scrolling and understands not
 * to roll when splitw is set, i.e. we are working in the echo area.
 * There is a bunch of hacking here dealing with the difference between
 * QUOTE, QUOTE|' ', and ' ' for CONCEPT-100 like terminals, and also
 * code to deal with terminals which overstrike, including CRT's where
 * you can erase overstrikes with some work.  CRT's which do underlining
 * implicitly which has to be erased (like CONCEPTS) are also handled.
 */
vputchar(c)
	register int c;
{
	register char *tp;
	register int d;

	c &= (QUOTE|TRIM);
#ifdef TRACE
	if (trace)
		tracec(c);
#endif
	/* Fix problem of >79 chars on echo line. */
	if (destcol >= WCOLS-1 && splitw && destline == WECHO)
		pofix();
	if (destcol >= WCOLS) {
		destline += destcol / WCOLS;
		destcol %= WCOLS;
	}
	if (destline > WBOT && (!splitw || destline > WECHO))
		vrollup(destline);
	tp = vtube[destline] + destcol;
	switch (c) {

	case '\t':
		vgotab();
		return;

	case ' ':
		/*
		 * We can get away without printing a space in a number
		 * of cases, but not always.  We get away with doing nothing
		 * if we are not in insert mode, and not on a CONCEPT-100
		 * like terminal, and either not in hardcopy open or in hardcopy
		 * open on a terminal with no overstriking, provided,
		 * in all cases, that nothing has ever been displayed
		 * at this position.  Ugh.
		 */
		if (!insmode && !IN && (state != HARDOPEN || OS) && (*tp&TRIM) == 0) {
			*tp = ' ';
			destcol++;
			return;
		}
		goto def;

	case QUOTE:
		if (insmode) {
			/*
			 * When in insert mode, tabs have to expand
			 * to real, printed blanks.
			 */
			c = ' ' | QUOTE;
			goto def;
		}
		if (*tp == 0) {
			/*
			 * A ``space''.
			 */
			if ((hold & HOLDPUPD) == 0)
				*tp = QUOTE;
			destcol++;
			return;
		}
		/*
		 * A ``space'' ontop of a part of a tab.
		 */
		if (*tp & QUOTE) {
			destcol++;
			return;
		}
		c = ' ' | QUOTE;
		/* fall into ... */

def:
	default:
		d = *tp & TRIM;
		/*
		 * Now get away with doing nothing if the characters
		 * are the same, provided we are not in insert mode
		 * and if we are in hardopen, that the terminal has overstrike.
		 */
		if (d == (c & TRIM) && !insmode && (state != HARDOPEN || OS)) {
			if ((hold & HOLDPUPD) == 0)
				*tp = c;
			destcol++;
			return;
		}
		/*
		 * Backwards looking optimization.
		 * The low level cursor motion routines will use
		 * a cursor motion right sequence to step 1 character
		 * right.  On, e.g., a DM3025A this is 2 characters
		 * and printing is noticeably slower at 300 baud.
		 * Since the low level routines are not allowed to use
		 * spaces for positioning, we discover the common
		 * case of a single space here and force a space
		 * to be printed.
		 */
		if (destcol == outcol + 1 && tp[-1] == ' ' && outline == destline) {
			vputc(' ');
			outcol++;
		}

		/*
		 * This is an inline expansion a call to vcsync() dictated
		 * by high frequency in a profile.
		 */
		if (outcol != destcol || outline != destline)
			vgoto(destline, destcol);

		/*
		 * Deal with terminals which have overstrike.
		 * We handle erasing general overstrikes, erasing
		 * underlines on terminals (such as CONCEPTS) which
		 * do underlining correctly automatically (e.g. on nroff
		 * output), and remembering, in hardcopy mode,
		 * that we have overstruct something.
		 */
		if (!insmode && d && d != ' ' && d != (c & TRIM)) {
			if (EO && (OS || UL && (c == '_' || d == '_'))) {
				vputc(' ');
				outcol++, destcol++;
				back1();
			} else
				rubble = 1;
		}

		/*
		 * Unless we are just bashing characters around for
		 * inner working of insert mode, update the display.
		 */
		if ((hold & HOLDPUPD) == 0)
			*tp = c;

		/*
		 * In insert mode, put out the IC sequence, padded
		 * based on the depth of the current line.
		 * A terminal which had no real insert mode, rather
		 * opening a character position at a time could do this.
		 * Actually should use depth to end of current line
		 * but this rarely matters.
		 */
		if (insmode)
			vputp(IC, DEPTH(vcline));
		vputc(c & TRIM);

		/*
		 * In insert mode, IP is a post insert pad.
		 */
		if (insmode)
			vputp(IP, DEPTH(vcline));
		destcol++, outcol++;

		/*
		 * CONCEPT braindamage in early models:  after a wraparound
		 * the next newline is eaten.  It's hungry so we just
		 * feed it now rather than worrying about it.
		 * Fixed to use	return linefeed to work right
		 * on vt100/tab132 as well as concept.
		 */
		if (XN && outcol % WCOLS == 0) {
			vputc('\r');
			vputc('\n');
		}
	}
}

/*
 * Delete display positions stcol through endcol.
 * Amount of use of special terminal features here is limited.
 */
physdc(stcol, endcol)
	int stcol, endcol;
{
	register char *tp, *up;
	char *tpe;
	register int i;
	register int nc = endcol - stcol;

#ifdef IDEBUG
	if (trace)
		tfixnl(), fprintf(trace, "physdc(%d, %d)\n", stcol, endcol);
#endif
	if (!DC || nc <= 0)
		return;
	if (IN) {
		/*
		 * CONCEPT-100 like terminal.
		 * If there are any ``spaces'' in the material to be
		 * deleted, then this is too hard, just retype.
		 */
		vprepins();
		up = vtube0 + stcol;
		i = nc;
		do
			if ((*up++ & (QUOTE|TRIM)) == QUOTE)
				return;
		while (--i);
		i = 2 * nc;
		do
			if (*up == 0 || (*up++ & QUOTE) == QUOTE)
				return;
		while (--i);
		vgotoCL(stcol);
	} else {
		/*
		 * HP like delete mode.
		 * Compute how much text we are moving over by deleting.
		 * If it appears to be faster to just retype
		 * the line, do nothing and that will be done later.
		 * We are assuming 2 output characters per deleted
		 * characters and that clear to end of line is available.
		 */
		i = stcol / WCOLS;
		if (i != endcol / WCOLS)
			return;
		i += LINE(vcline);
		stcol %= WCOLS;
		endcol %= WCOLS;
		up = vtube[i]; tp = up + endcol; tpe = up + WCOLS;
		while (tp < tpe && *tp)
			tp++;
		if (tp - (up + stcol) < 2 * nc)
			return;
		vgoto(i, stcol);
	}

	/*
	 * Go into delete mode and do the actual delete.
	 * Padding is on DC itself.
	 */
	godm();
	for (i = nc; i > 0; i--)
		vputp(DC, DEPTH(vcline));
	vputp(ED, 0);

	/*
	 * Straighten up.
	 * With CONCEPT like terminals, characters are pulled left
	 * from first following null.  HP like terminals shift rest of
	 * this (single physical) line rigidly.
	 */
	if (IN) {
		up = vtube0 + stcol;
		tp = vtube0 + endcol;
		while (i = *tp++) {
			if ((i & (QUOTE|TRIM)) == QUOTE)
				break;
			*up++ = i;
		}
		do
			*up++ = i;
		while (--nc);
	} else {
		copy(up + stcol, up + endcol, WCOLS - endcol);
		vclrbyte(tpe - nc, nc);
	}
}

#ifdef TRACE
tfixnl()
{

	if (trubble || techoin)
		fprintf(trace, "\n");
	trubble = 0, techoin = 0;
}

tvliny()
{
	register int i;

	if (!trace)
		return;
	tfixnl();
	fprintf(trace, "vcnt = %d, vcline = %d, vliny = ", vcnt, vcline);
	for (i = 0; i <= vcnt; i++) {
		fprintf(trace, "%d", LINE(i));
		if (FLAGS(i) & VDIRT)
			fprintf(trace, "*");
		if (DEPTH(i) != 1)
			fprintf(trace, "<%d>", DEPTH(i));
		if (i < vcnt)
			fprintf(trace, " ");
	}
	fprintf(trace, "\n");
}

tracec(c)
	int c;		/* mjm: char --> int */
{

	if (!techoin)
		trubble = 1;
	if (c == ESCAPE)
		fprintf(trace, "$");
	else if (c & QUOTE)	/* mjm: for 3B (no sign extension) */
		fprintf(trace, "~%c", ctlof(c&TRIM));
	else if (c < ' ' || c == DELETE)
		fprintf(trace, "^%c", ctlof(c));
	else
		fprintf(trace, "%c", c);
}
#endif

/*
 * Put a character with possible tracing.
 */
vputch(c)
	int c;
{

#ifdef TRACE
	if (trace)
		tracec(c);
#endif
	vputc(c);
}
