/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: scrn.c,v 2.5 85/08/22 16:07:10 timo Exp $";

/*
 * B editor -- Screen management package, higher level routines.
 */

#include "b.h"
#include "erro.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "gram.h"
#include "cell.h"


extern bool dflag;

cell *gettop();
extern int focy;
extern int focx;

Visible int winstart;

Visible int winheight;
Visible int indent;
Visible int llength;

Visible bool noscroll;
Visible bool nosense;

Hidden cell *tops;


/*
 * Actual screen update.
 */

Visible Procedure
actupdate(copybuffer, recording, lasttime)
	value copybuffer;
	bool recording;
	bool lasttime; /* Yes if called from final screen update */
{
	register cell *p;
	cell *top = tops;
	register int diff;
	register int curlno;
	register int delcnt = 0; /* Lines deleted during the process. */
		/* Used as offset for lines that are on the screen. */
	int totlines = 0;
	int topline = 0;
	int scrlines = 0;

	if (winstart > 0)
		growwin();
	if (winstart <= 0) {
		top = gettop(tops);
		for (p = tops; p && p != top; p = p->c_link)
			++topline;
		totlines = topline;
	}
	startactupdate(lasttime);
	focy = Nowhere;
	for (p = top, curlno = winstart; p && curlno < winheight;
		curlno += Space(p), p = p->c_link) {
		++scrlines;
		if (lasttime) {
			p->c_newfocus = No;
			p->c_newvhole = 0;
		}
		if (p->c_onscreen != Nowhere && Space(p) == Oldspace(p)) {
			/* Old comrade */
			diff = p->c_onscreen - (curlno+delcnt);
			/* diff can't be negative due to 'makeroom' below! */
			if (diff > 0) { /* Get him here */
				trmscrollup(curlno, winheight, diff);
				delcnt += diff;
			}
			if (p->c_oldfocus || p->c_newfocus
				|| p->c_oldindent != p->c_newindent
				|| p->c_onscreen + Space(p) >= winheight) {
				delcnt = make2room(p, curlno, delcnt);
				outline(p, curlno);
			}
		}
		else { /* New guy, make him toe the line */
			delcnt = makeroom(p, curlno, delcnt);
			delcnt = make2room(p, curlno, delcnt);
			outline(p, curlno);
		}
		p->c_onscreen = curlno;
		p->c_oldindent = p->c_newindent;
		p->c_oldvhole = p->c_newvhole;
		p->c_oldfocus = p->c_newfocus;
	}
	totlines += scrlines;
	for (; p; p = p->c_link) { /* Count rest and remove old memories */
		++totlines;
		/* This code should never find any garbage?! */
#ifndef NDEBUG
		if (p->c_onscreen != Nowhere)
			debug("[Garbage removed from screen list]");
#endif NDEBUG
		p->c_onscreen = Nowhere;
	}
	trmscrollup(curlno, winheight, -delcnt);
	curlno += delcnt;
	if (curlno < winheight) { /* Clear lines beyond end of unit */
		trmputdata(curlno, winheight-1, 0, "");
		scrlines += winheight-curlno;
	}
	if (!lasttime) {
		stsline(totlines, topline, scrlines, copybuffer, recording);
		if (focy != Nowhere)
			trmsync(focy, focx);
		else
			trmsync(winheight, 0);
	}
	endactupdate();
}


/*
 * Grow the window if not maximum size.
 */

Hidden Procedure
growwin()
{
	register int winsize;
	register int growth;
	register cell *p;

	winsize = 0;
	for (p = tops; p; p = p->c_link)
		winsize += Space(p);
	if (winsize <= winheight - winstart)
		return; /* No need to grow */
	if (winsize > winheight)
		winsize = winheight; /* Limit size to maximum available */

	growth = winsize - (winheight - winstart);
	trmscrollup(0, winheight - (winstart!=winheight), growth);
	winstart -= growth;
	for (p = tops; p; p = p->c_link) {
		if (p->c_onscreen != Nowhere)
			p->c_onscreen -= growth;
	}
}


/*
 * Make room for possible insertions.
 * (If a line is inserted, it may be necessary to delete lines
 * further on the screen.)
 */

Hidden Procedure
makeroom(p, curlno, delcnt)
	register cell *p;
	register int curlno;
	register int delcnt;
{
	register int here = 0;
	register int need = Space(p);
	register int amiss;
	int avail;
	int diff;

	Assert(p);
	do {
		p = p->c_link;
		if (!p)
			return delcnt;
	} while (p->c_onscreen == Nowhere);
	here = p->c_onscreen - delcnt;
	avail = here - curlno;
	amiss = need - avail;
#ifndef NDEBUG
	if (dflag)
		debug("[makeroom: curlno=%d, delcnt=%d, here=%d, avail=%d, amiss=%d]",
			curlno, delcnt, here, avail, amiss);
#endif NDEBUG
	if (amiss <= 0)
		return delcnt;
	if (amiss > delcnt) {
		for (; p; p = p->c_link) {
			if (p->c_onscreen != Nowhere) {
				diff = amiss-delcnt;
				if (p->c_onscreen - delcnt - here < diff)
					diff = p->c_onscreen - delcnt - here;
				if (diff > 0) {
					trmscrollup(here, winheight, diff);
					delcnt += diff;
				}
				p->c_onscreen += -delcnt + amiss;
				here = p->c_onscreen - amiss;
				if (p->c_onscreen >= winheight)
					p->c_onscreen = Nowhere;
			}
			here += Space(p);
		}
		/* Now for all p encountered whose p->c_onscreen != Nowhere,
		/* p->c_onscreen - amiss is its actual position. */
		if (amiss > delcnt) {
			trmscrollup(winheight - amiss, winheight, amiss-delcnt);
			delcnt = amiss;
		}
	}
	/* Now amiss <= delcnt */
	trmscrollup(curlno + avail, winheight, -amiss);
	return delcnt - amiss;
}


/*
 * Addition to makeroom - make sure the status line is not overwritten.
 * Returns new delcnt, like makeroom does.
 */

Hidden int
make2room(p, curlno, delcnt)
	cell *p;
	int curlno;
	int delcnt;
{
	int nextline = curlno + Space(p);
	int sline = winheight - delcnt;
	int diff;

	if (sline < curlno) {
#ifndef NDEBUG
		debug("[Status line overwritten]");
#endif NDEBUG
		return delcnt;
	}
	if (nextline > winheight)
		nextline = winheight;
	diff = nextline - sline;
	if (diff > 0) {
		trmscrollup(sline, winheight, -diff);
		delcnt -= diff;
	}
	return delcnt;
		
}


/*
 * Routine called for every change in the screen.
 */

Visible Procedure
virtupdate(oldep, newep, highest)
	environ *oldep;
	environ *newep;
	int highest;
{
	environ old;
	environ new;
	register int oldlno;
	register int newlno;
	register int oldlcnt;
	register int newlcnt;
	register int i;

	if (!oldep) {
		highest = 1;
		trmputdata(winstart, winheight, indent, "");
		discard(tops);
		tops = Cnil;
		Ecopy(*newep, old);
	}
	else {
		Ecopy(*oldep, old);
	}
	Ecopy(*newep, new);

	savefocus(&new);

	oldlcnt = fixlevels(&old, &new, highest);
	newlcnt = -width(tree(new.focus));
	if (newlcnt < 0)
		newlcnt = 0;
	i = -width(tree(old.focus));
	if (i < 0)
		i = 0;
	newlcnt -= i - oldlcnt;
		/* Offset newlcnt as much as oldcnt is offset */
	
	oldlno = Ycoord(old.focus);
	newlno = Ycoord(new.focus);
	if (!atlinestart(&old))
		++oldlcnt;
	else
		++oldlno;
	if (!atlinestart(&new))
		++newlcnt;
	else
		++newlno;
	Assert(oldlno == newlno);

	tops = replist(tops, build(new.focus, newlcnt), oldlno, oldlcnt);

	setfocus(tops); /* Incorporate the information saved by savefocus */

	Erelease(old);
	Erelease(new);
}


Hidden bool
atlinestart(ep)
	environ *ep;
{
	register string repr = noderepr(tree(ep->focus))[0];

	return Fw_negative(repr);
}


/*
 * Make the two levels the same, and make sure they both are line starters
 * if at all possible.  Return the OLD number of lines to be replaced.
 * (0 if the whole unit has no linefeeds.)
 */

Hidden int
fixlevels(oldep, newep, highest)
	register environ *oldep;
	register environ *newep;
	register int highest;
{
	register int oldpl = pathlength(oldep->focus);
	register int newpl = pathlength(newep->focus);
	register bool intraline = No;
	register int w;

	if (oldpl < highest)
		highest = oldpl;
	if (newpl < highest)
		highest = newpl;
	while (oldpl > highest) {
		up(&oldep->focus) || Abort();
		--oldpl;
	}
	while (newpl > highest) {
		up(&newep->focus) || Abort();
		--newpl;
	}
	if (Ycoord(newep->focus) != Ycoord(oldep->focus) ||
		Level(newep->focus) != Level(newep->focus)) {
		/* Inconsistency found.  */
		Assert(highest > 1); /* Inconsistency at top level. Stop. */
		return fixlevels(oldep, newep, 1); /* Try to recover. */
	}
	intraline = width(tree(oldep->focus)) >= 0
		&& width(tree(newep->focus)) >= 0;
	while (!atlinestart(oldep) || !atlinestart(newep)) {
		/* Find beginning of lines for both */
		if (!up(&newep->focus)) {
			Assert(!up(&newep->focus));
			break;
		}
		--oldpl;
		up(&oldep->focus) || Abort();
		--newpl;
	}
	if (intraline)
		return atlinestart(oldep);
	w = width(tree(oldep->focus));
	return w < 0 ? -w : 0;
}


/*
 * Initialization code.
 */

Visible Procedure
initshow()
{
	int flags = 0;
#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** initshow();\n\r");
#endif NDEBUG
	if (!trmstart(&winheight, &llength, &flags)) {
		endunix();
		exit(2);
	}
	noscroll = (flags&2) == 0;
	nosense = (flags&8) == 0;
	winstart = --winheight;
}


/*
 * Routine to move the cursor to the first line after the just edited
 * document.  (Called after each editing action.)
 */

Visible Procedure
endshow()
{
	register cell *p;
	register int last = winheight;

	for (p = tops; p; p = p->c_link) {
		if (p->c_onscreen != Nowhere)
			last = p->c_onscreen + Oldspace(p);
	}
	if (last > winheight)
		last = winheight;
	discard(tops);
	tops = Cnil;
	trmputdata(last, winheight, 0, "");
	trmsync(last, 0);
	trmend();
}


/*
 * Translate a cursor position in tree coordinates.
 *
 * ***** DOESN'T WORK IF SCREEN INDENT DIFFERS FROM TREE INDENT! *****
 * (I.e. for lines with >= 80 spaces indentation)
 */

Visible bool
backtranslate(py, px)
	int *py;
	int *px;
{
	cell *p;
	int y = *py;
	int x = *px;
	int i;

	for (i = 0, p = tops; p; ++i, p = p->c_link) {
		if (p->c_onscreen != Nowhere
			&& y >= p->c_onscreen && y < p->c_onscreen + Space(p)) {
			*px += (y - p->c_onscreen) * llength - indent;
			if (*px < 0)
				*px = 0;
			*py = i;
			if (p->c_oldvhole && (y > focy || y == focy && x > focx))
				--*px; /* Correction if beyond Vhole on same logical line */
			return Yes;
		}
	}
	error(GOTO_OUT);
	return No;
}


/*
 * Set the indent level and window start line.
 */

Visible Procedure
setindent(x)
	int x;
{
	winstart= winheight;
	indent= x;
}


/*
 * Show the command prompt.
 */

Visible Procedure cmdprompt(prompt)
	string prompt;
{
	setindent(strlen(prompt));
	trmputdata(winstart, winstart, 0, prompt);
}
