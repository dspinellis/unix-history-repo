/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_vops3.c	7.3 (Berkeley) %G%";
#endif not lint

#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * Routines to handle structure.
 * Operations supported are:
 *	( ) { } [ ]
 *
 * These cover:		LISP		TEXT
 *	( )		s-exprs		sentences
 *	{ }		list at same	paragraphs
 *	[ ]		defuns		sections
 *
 * { and } for C used to attempt to do something with matching {}'s, but
 * I couldn't find definitions which worked intuitively very well, so I
 * scrapped this.
 *
 * The code here is very hard to understand.
 */
line	*llimit;
int	(*lf)();

#ifdef LISPCODE
int	lindent();
#endif

bool	wasend;

/*
 * Find over structure, repeated count times.
 * Don't go past line limit.  F is the operation to
 * be performed eventually.  If pastatom then the user said {}
 * rather than (), implying past atoms in a list (or a paragraph
 * rather than a sentence.
 */
lfind(pastatom, cnt, f, limit)
	bool pastatom;
	int cnt, (*f)();
	line *limit;
{
	register int c;
	register int rc = 0;
	char save[LBSIZE];

	/*
	 * Initialize, saving the current line buffer state
	 * and computing the limit; a 0 argument means
	 * directional end of file.
	 */
	wasend = 0;
	lf = f;
	strcpy(save, linebuf);
	if (limit == 0)
		limit = dir < 0 ? one : dol;
	llimit = limit;
	wdot = dot;
	wcursor = cursor;

	if (pastatom >= 2) {
		while (cnt > 0 && word(f, cnt))
			cnt--;
		if (pastatom == 3)
			eend(f);
		if (dot == wdot) {
			wdot = 0;
			if (cursor == wcursor)
				rc = -1;
		}
	}
#ifdef LISPCODE
	else if (!value(LISP)) {
#else
	else {
#endif
		char *icurs;
		line *idot;

		if (linebuf[0] == 0) {
			do
				if (!lnext())
					goto ret;
			while (linebuf[0] == 0);
			if (dir > 0) {
				wdot--;
				linebuf[0] = 0;
				wcursor = linebuf;
				/*
				 * If looking for sentence, next line
				 * starts one.
				 */
				if (!pastatom) {
					icurs = wcursor;
					idot = wdot;
					goto begin;
				}
			}
		}
		icurs = wcursor;
		idot = wdot;

		/*
		 * Advance so as to not find same thing again.
		 */
		if (dir > 0) {
			if (!lnext()) {
				rc = -1;
				goto ret;
			}
		} else
			ignore(lskipa1(""));

		/*
		 * Count times find end of sentence/paragraph.
		 */
begin:
		for (;;) {
			while (!endsent(pastatom))
				if (!lnext())
					goto ret;
			if (!pastatom || wcursor == linebuf && endPS())
				if (--cnt <= 0)
					break;
			if (linebuf[0] == 0) {
				do
					if (!lnext())
						goto ret;
				while (linebuf[0] == 0);
			} else
				if (!lnext())
					goto ret;
		}

		/*
		 * If going backwards, and didn't hit the end of the buffer,
		 * then reverse direction.
		 */
		if (dir < 0 && (wdot != llimit || wcursor != linebuf)) {
			dir = 1;
			llimit = dot;
			/*
			 * Empty line needs special treatement.
			 * If moved to it from other than begining of next line,
			 * then a sentence starts on next line.
			 */
			if (linebuf[0] == 0 && !pastatom && 
			   (wdot != dot - 1 || cursor != linebuf)) {
				lnext();
				goto ret;
			}
		}

		/*
		 * If we are not at a section/paragraph division,
		 * advance to next.
		 */
		if (wcursor == icurs && wdot == idot || wcursor != linebuf || !endPS())
			ignore(lskipa1(""));
	}
#ifdef LISPCODE
	else {
		c = *wcursor;
		/*
		 * Startup by skipping if at a ( going left or a ) going
		 * right to keep from getting stuck immediately.
		 */
		if (dir < 0 && c == '(' || dir > 0 && c == ')') {
			if (!lnext()) {
				rc = -1;
				goto ret;
			}
		}
		/*
		 * Now chew up repitition count.  Each time around
		 * if at the beginning of an s-exp (going forwards)
		 * or the end of an s-exp (going backwards)
		 * skip the s-exp.  If not at beg/end resp, then stop
		 * if we hit a higher level paren, else skip an atom,
		 * counting it unless pastatom.
		 */
		while (cnt > 0) {
			c = *wcursor;
			if (dir < 0 && c == ')' || dir > 0 && c == '(') {
				if (!lskipbal("()"))
					goto ret;
				/*
 				 * Unless this is the last time going
				 * backwards, skip past the matching paren
				 * so we don't think it is a higher level paren.
				 */
				if (dir < 0 && cnt == 1)
					goto ret;
				if (!lnext() || !ltosolid())
					goto ret;
				--cnt;
			} else if (dir < 0 && c == '(' || dir > 0 && c == ')')
				/* Found a higher level paren */
				goto ret;
			else {
				if (!lskipatom())
					goto ret;
				if (!pastatom)
					--cnt;
			}
		}
	}
#endif
ret:
	strcLIN(save);
	return (rc);
}

/*
 * Is this the end of a sentence?
 */
endsent(pastatom)
	bool pastatom;
{
	register char *cp = wcursor;
	register int c, d;

	/*
	 * If this is the beginning of a line, then
	 * check for the end of a paragraph or section.
	 */
	if (cp == linebuf)
		return (endPS());

	/*
	 * Sentences end with . ! ? not at the beginning
	 * of the line, and must be either at the end of the line,
	 * or followed by 2 spaces.  Any number of intervening ) ] ' "
	 * characters are allowed.
	 */
	if (!any(c = *cp, ".!?"))
		goto tryps;
	do
		if ((d = *++cp) == 0)
			return (1);
	while (any(d, ")]'"));
	if (*cp == 0 || *cp++ == ' ' && *cp == ' ')
		return (1);
tryps:
	if (cp[1] == 0)
		return (endPS());
	return (0);
}

/*
 * End of paragraphs/sections are respective
 * macros as well as blank lines and form feeds.
 */
endPS()
{

	return (linebuf[0] == 0 ||
		isa(svalue(PARAGRAPHS)) || isa(svalue(SECTIONS)));
	    
}

#ifdef LISPCODE
lindent(addr)
	line *addr;
{
	register int i;
	char *swcurs = wcursor;
	line *swdot = wdot;

again:
	if (addr > one) {
		register char *cp;
		register int cnt = 0;

		addr--;
		getline(*addr);
		for (cp = linebuf; *cp; cp++)
			if (*cp == '(')
				cnt++;
			else if (*cp == ')')
				cnt--;
		cp = vpastwh(linebuf);
		if (*cp == 0)
			goto again;
		if (cnt == 0)
			return (whitecnt(linebuf));
		addr++;
	}
	wcursor = linebuf;
	linebuf[0] = 0;
	wdot = addr;
	dir = -1;
	llimit = one;
	lf = lindent;
	if (!lskipbal("()"))
		i = 0;
	else if (wcursor == linebuf)
		i = 2;
	else {
		register char *wp = wcursor;

		dir = 1;
		llimit = wdot;
		if (!lnext() || !ltosolid() || !lskipatom()) {
			wcursor = wp;
			i = 1;
		} else
			i = 0;
		i += column(wcursor) - 1;
		if (!inopen)
			i--;
	}
	wdot = swdot;
	wcursor = swcurs;
	return (i);
}
#endif

lmatchp(addr)
	line *addr;
{
	register int i;
	register char *parens, *cp;

	for (cp = cursor; !any(*cp, "({[)}]");)
		if (*cp++ == 0)
			return (0);
	lf = 0;
	parens = any(*cp, "()") ? "()" : any(*cp, "[]") ? "[]" : "{}";
	if (*cp == parens[1]) {
		dir = -1;
		llimit = one;
	} else {
		dir = 1;
		llimit = dol;
	}
	if (addr)
		llimit = addr;
	if (splitw)
		llimit = dot;
	wcursor = cp;
	wdot = dot;
	i = lskipbal(parens);
	return (i);
}

lsmatch(cp)
	char *cp;
{
	char save[LBSIZE];
	register char *sp = save;
	register char *scurs = cursor;

	wcursor = cp;
	strcpy(sp, linebuf);
	*wcursor = 0;
	strcpy(cursor, genbuf);
	cursor = strend(linebuf) - 1;
	if (lmatchp(dot - vcline)) {
		register int i = insmode;
		register int c = outcol;
		register int l = outline;

		if (!MI)
			endim();
		vgoto(splitw ? WECHO : LINE(wdot - llimit), column(wcursor) - 1);
		flush();
		sleep(1);
		vgoto(l, c);
		if (i)
			goim();
	}
	else {
		strcLIN(sp);
		strcpy(scurs, genbuf);
		if (!lmatchp((line *) 0))
			beep();
	}
	strcLIN(sp);
	wdot = 0;
	wcursor = 0;
	cursor = scurs;
}

ltosolid()
{

	return (ltosol1("()"));
}

ltosol1(parens)
	register char *parens;
{
	register char *cp;

	if (*parens && !*wcursor && !lnext())
		return (0);
	while (isspace(*wcursor) || (*wcursor == 0 && *parens))
		if (!lnext())
			return (0);
	if (any(*wcursor, parens) || dir > 0)
		return (1);
	for (cp = wcursor; cp > linebuf; cp--)
		if (isspace(cp[-1]) || any(cp[-1], parens))
			break;
	wcursor = cp;
	return (1);
}

lskipbal(parens)
	register char *parens;
{
	register int level = dir;
	register int c;

	do {
		if (!lnext()) {
			wdot = NOLINE;
			return (0);
		}
		c = *wcursor;
		if (c == parens[1])
			level--;
		else if (c == parens[0])
			level++;
	} while (level);
	return (1);
}

lskipatom()
{

	return (lskipa1("()"));
}

lskipa1(parens)
	register char *parens;
{
	register int c;

	for (;;) {
		if (dir < 0 && wcursor == linebuf) {
			if (!lnext())
				return (0);
			break;
		}
		c = *wcursor;
		if (c && (isspace(c) || any(c, parens)))
			break;
		if (!lnext())
			return (0);
		if (dir > 0 && wcursor == linebuf)
			break;
	}
	return (ltosol1(parens));
}

lnext()
{

	if (dir > 0) {
		if (*wcursor)
			wcursor++;
		if (*wcursor)
			return (1);
		if (wdot >= llimit) {
			if (lf == vmove && wcursor > linebuf)
				wcursor--;
			return (0);
		}
		wdot++;
		getline(*wdot);
		wcursor = linebuf;
		return (1);
	} else {
		--wcursor;
		if (wcursor >= linebuf)
			return (1);
#ifdef LISPCODE
		if (lf == lindent && linebuf[0] == '(')
			llimit = wdot;
#endif
		if (wdot <= llimit) {
			wcursor = linebuf;
			return (0);
		}
		wdot--;
		getline(*wdot);
		wcursor = linebuf[0] == 0 ? linebuf : strend(linebuf) - 1;
		return (1);
	}
}

lbrack(c, f)
	register int c;
	int (*f)();
{
	register line *addr;

	addr = dot;
	for (;;) {
		addr += dir;
		if (addr < one || addr > dol) {
			addr -= dir;
			break;
		}
		getline(*addr);
		if (linebuf[0] == '{' ||
#ifdef LISPCODE
		    value(LISP) && linebuf[0] == '(' ||
#endif
		    isa(svalue(SECTIONS))) {
			if (c == ']' && f != vmove) {
				addr--;
				getline(*addr);
			}
			break;
		}
		if (c == ']' && f != vmove && linebuf[0] == '}')
			break;
	}
	if (addr == dot)
		return (0);
	if (f != vmove)
		wcursor = c == ']' ? strend(linebuf) : linebuf;
	else
		wcursor = 0;
	wdot = addr;
	vmoving = 0;
	return (1);
}

isa(cp)
	register char *cp;
{

	if (linebuf[0] != '.')
		return (0);
	for (; cp[0] && cp[1]; cp += 2)
		if (linebuf[1] == cp[0]) {
			if (linebuf[2] == cp[1])
				return (1);
			if (linebuf[2] == 0 && cp[1] == ' ')
				return (1);
		}
	return (0);
}
