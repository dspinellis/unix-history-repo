/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"

#include "ctype.h"

/* Make a newline after AFTER in buffer BUF, UNLESS after is 0,
   in which case we insert the newline before after. */

Line *
listput(buf, after)
register Buffer	*buf;
register Line	*after;
{
	register Line	*newline = nbufline();

	if (after == 0) {	/* Before the first line */
		newline->l_next = buf->b_first;
		newline->l_prev = 0;
		buf->b_first = newline;
	} else {
		newline->l_prev = after;
		newline->l_next = after->l_next;
		after->l_next = newline;
	}
	if (newline->l_next)
		newline->l_next->l_prev = newline;
	else
		if (buf)
			buf->b_last = newline;
	if (buf && buf->b_dot == 0)
		buf->b_dot = newline;
	return newline;
}	

/* Divide the current line and move the current line to the next one */

LineInsert()
{
	register int	num = exp;
	char	newline[LBSIZE];
	register Line	*newdot,
			*olddot;
	int	oldchar;

	exp = 1;
	olddot = curline;
	oldchar = curchar;

	newdot = curline;
	while (--num >= 0) {
		newdot = listput(curbuf, newdot);
		SavLine(newdot, NullStr);
	}

	modify();
	if (curchar != 0) {
		strcpy(newline, &linebuf[curchar]);
		linebuf[curchar] = '\0';	/* Shorten this line */
		SavLine(curline, linebuf);
		strcpy(linebuf, newline);
	} else {	/* Redisplay optimization */
		newdot->l_dline = curline->l_dline;
		SavLine(curline, NullStr);
	}

	makedirty(curline);
	curline = newdot;
	curchar = 0;
	makedirty(curline);
	IFixMarks(olddot, oldchar, curline, curchar);
}	

n_indent(goal)
register int	goal;
{
	if (goal < 0)
		return;
	DoTimes(Insert('\t'), (goal / tabstop));
	if (goal % tabstop)
		DoTimes(Insert(' '), (goal % tabstop));
	exp_p = 0;
	exp = 1;
}

SelfInsert()
{
#ifdef ABBREV
	if (MinorMode(Abbrev) && !ismword(LastKeyStruck) &&
	    !bolp() && ismword(linebuf[curchar - 1]))
		AbbrevExpand();
#endif
	if (MinorMode(OverWrite)) {
		register int	num,
				i;

		for (i = 0, num = exp, exp = 1; i < num; i++) {
			int	pos = calc_pos(linebuf, curchar);

			if (!eolp()) {
				if (linebuf[curchar] == '\t') {
					if ((pos + 1) == ((pos + tabstop) - (pos % tabstop)))
						DelNChar();
				} else
					DelNChar();
			}
			Insert(LastKeyStruck);
		}
	} else
		Insert(LastKeyStruck);

	if (MinorMode(Fill) && (curchar >= RMargin ||
			       (calc_pos(linebuf, curchar) >= RMargin)))
		DoJustify(curline, 0, curline,
			  curchar + strlen(&linebuf[curchar]), 1, LMargin);
}

Insert(c)
{
	if (exp <= 0)
		return;
	modify();
	makedirty(curline);
	ins_c(c, linebuf, curchar, exp, LBSIZE);
	IFixMarks(curline, curchar, curline, curchar + exp);
	curchar += exp;
}	

/* Tab in to the right place for C mode */

Tab()
{
#ifdef LISP
	if (MajorMode(LISPMODE)) {
		Mark	*m = bolp() ? 0 : MakeMark(curline, curchar, FLOATER);

		Bol();
		DelWtSpace();
		(void) lisp_indent();
		if (m) {
			ToMark(m);
			DelMark(m);
		}
		if (bolp())
			ToIndent();
		return;
	}
#endif
	if (MajorMode(CMODE) && strlen(linebuf) == 0)
		(void) c_indent(CIndIncrmt);
	else
		SelfInsert();
}

QuotChar()
{
	int	c;
	extern int	alarmed;	/* If waitfor had to wait. */

	c = waitchar();
	if (alarmed)
		message(key_strokes);
	if (c == CTL(J))
		LineInsert();
	else if (c != CTL(@))
		Insert(c);
}

/* Insert the paren.  If in C mode and c is a '}' then insert the
   '}' in the "right" place for C indentation; that is indented 
   the same amount as the matching '{' is indented. */

int	PDelay = 5,	/* 1/2 a second */
	CIndIncrmt = 8;

DoParen()
{
	Bufpos	*bp = (Bufpos *) -1;
	int	nx,
		c = LastKeyStruck;

	if (!isclosep(c)) {
		SelfInsert();
		return;
	}

	if (MajorMode(CMODE) && c == '}' && blnkp(linebuf)) {
		DelWtSpace();
		bp = c_indent(0);
	}
#ifdef LISP
	if (MajorMode(LISPMODE) && c == ')' && blnkp(linebuf)) {
		DelWtSpace();
		bp = lisp_indent();
	}
#endif
	SelfInsert();
	if (MinorMode(ShowMatch) && !charp() && !in_macro()) {
		BackChar();	/* Back onto the ')' */
		if ((int) bp == -1)
			bp = m_paren(c, BACKWARD, NO, YES);
		ForChar();
		if (bp != 0) {
			nx = in_window(curwind, bp->p_line);
			if (nx != -1) {		/* is visible */
				Bufpos	b;

				DOTsave(&b);
				SetDot(bp);
				(void) SitFor(PDelay);
				SetDot(&b);
			} else
				s_mess("%s", lcontents(bp->p_line));
		}
		mp_error();	/* display error message */
	}
}

LineAI()
{
	DoNewline(TRUE);
}

Newline()
{
	DoNewline(MinorMode(Indent));
}	

DoNewline(indentp)
{
	Bufpos	save;
	int	indent;

	/* first we calculate the indent of the current line */
	DOTsave(&save);
	ToIndent();
	indent = calc_pos(linebuf, curchar);
	SetDot(&save);

	/* If there is more than 2 blank lines in a row then don't make
	   a newline, just move down one. */

#ifdef ABBREV
	if (MinorMode(Abbrev) && !ismword(LastKeyStruck) &&
	    !bolp() && ismword(linebuf[curchar - 1]))
		AbbrevExpand();
#endif
#ifdef LISP
	if (MajorMode(LISPMODE))
		DelWtSpace();
#endif
	else if (blnkp(linebuf))
		DelWtSpace();
		
	if (exp == 1 && eolp() && TwoBlank())
		SetLine(curline->l_next);
	else
		LineInsert();

	if (indentp)
#ifdef LISP
	    if (MajorMode(LISPMODE))
		(void) lisp_indent();
	    else
#endif
		n_indent((LMargin == 0) ? indent : LMargin);
}

ins_str(str, ok_nl)
register char	*str;
{
	register char	c;
	Bufpos	save;
	int	llen;

	DOTsave(&save);
	llen = strlen(linebuf);
	while (c = *str++) {
		if (c == '\n' || (ok_nl && llen >= LBSIZE - 2)) {
			IFixMarks(save.p_line, save.p_char, curline, curchar);
			modify();
			makedirty(curline);
			LineInsert();
			DOTsave(&save);
			llen = strlen(linebuf);
		}
		if (c != '\n') {
			ins_c(c, linebuf, curchar++, 1, LBSIZE);
			llen++;
		}
	}
	IFixMarks(save.p_line, save.p_char, curline, curchar);
	modify();
	makedirty(curline);
}

OpenLine()
{
	Bufpos	dot;

	DOTsave(&dot);
	LineInsert();	/* Open the lines... */
	SetDot(&dot);
}

/* Take the region FLINE/FCHAR to TLINE/TCHAR and insert it at
   ATLINE/ATCHAR in WHATBUF. */

Bufpos *
DoYank(fline, fchar, tline, tchar, atline, atchar, whatbuf)
Line	*fline,
	*tline,
	*atline;
Buffer	*whatbuf;
{
	register Line	*newline;
	static Bufpos	bp;
	char	save[LBSIZE],
		buf[LBSIZE];
	Line	*startline = atline;
	int	startchar = atchar;

	lsave();
	if (whatbuf)
		modify();
	(void) ltobuf(atline, genbuf);
	strcpy(save, &genbuf[atchar]);

	(void) ltobuf(fline, buf);
	if (fline == tline)
		buf[tchar] = '\0';

	linecopy(genbuf, atchar, &buf[fchar]);
	atline->l_dline = putline(genbuf);
	makedirty(atline);

	fline = fline->l_next;
	while (fline != tline->l_next) {
		newline = listput(whatbuf, atline);
		newline->l_dline = fline->l_dline;
		makedirty(newline);
		fline = fline->l_next;
		atline = newline;
		atchar = 0;
	}

	(void) getline(atline->l_dline, genbuf);
	atchar += tchar;
	linecopy(genbuf, atchar, save);
	atline->l_dline = putline(genbuf);
	makedirty(atline);
	IFixMarks(startline, startchar, atline, atchar);
	bp.p_line = atline;
	bp.p_char = atchar;
	this_cmd = YANKCMD;
	getDOT();			/* Whatever used to be in linebuf */
	return &bp;
}

YankPop()
{
	Line	*line,
		*last;
	Mark	*mp = CurMark();
	Bufpos	*dot;
	int	dir = -1;	/* Direction to rotate the ring */

	if (last_cmd != YANKCMD)
		complain("Yank something first!");

	lfreelist(reg_delete(mp->m_line, mp->m_char, curline, curchar));

	/* Now must find a recently killed region. */

	if (exp < 0)
		dir = 1;

	killptr += dir;
	for (;;) {
		if (killptr < 0)
			killptr = NUMKILLS - 1;
		else if (killptr >= NUMKILLS)
			killptr = 0;
		if (killbuf[killptr])
			break;
		killptr += dir;
	}

	this_cmd = YANKCMD;

	line = killbuf[killptr];
	last = lastline(line);
	dot = DoYank(line, 0, last, length(last), curline, curchar, curbuf);
	MarkSet(CurMark(), curline, curchar);
	SetDot(dot);
}

/* This is an attempt to reduce the amount of memory taken up by each line.
   Without this each malloc of a line uses sizeof (line) + sizeof(HEADER)
   where line is 3 words and HEADER is 1 word.
   This is going to allocate memory in chucks of CHUNKSIZE * sizeof (line)
   and divide each chuck into lineS.  A line is free in a chunk when its
   line->l_dline == 0, so freeline sets dline to 0. */

#define CHUNKSIZE	300

struct chunk {
	int	c_nlines;	/* Number of lines in this chunk (so they
				   don't all have to be CHUNKSIZE long). */
	Line	*c_block;	/* Chunk of memory */
	struct chunk	*c_nextfree;	/* Next chunk of lines */
};

static struct chunk	*fchunk = 0;
static Line	*ffline = 0;	/* First free line */

freeline(line)
register Line	*line;
{
	line->l_dline = 0;
	line->l_next = ffline;
	if (ffline)
		ffline->l_prev = line;
	line->l_prev = 0;
	ffline = line;
}

lfreelist(first)
register Line	*first;
{
	if (first)
		lfreereg(first, lastline(first));
}

/* Append region from line1 to line2 onto the free list of lines */

lfreereg(line1, line2)
register Line	*line1,
		*line2;
{
	register Line	*next,
			*last = line2->l_next;

	while (line1 != last) {
		next = line1->l_next;
		freeline(line1);
		line1 = next;
	}
}

static
newchunk()
{
	register Line	*newline;
	register int	i;
	struct chunk	*f;
	int	nlines = CHUNKSIZE;

	f = (struct chunk *) emalloc(sizeof (struct chunk));
	if (f == 0)
		return 0;

	if ((f->c_block = (Line *) malloc((unsigned) (sizeof (Line) * nlines))) == 0) {
		while (nlines > 0) {
			f->c_block = (Line *) malloc((unsigned) (sizeof (Line) * nlines));
			if (f->c_block != 0)
				break;
			nlines /= 2;
		}
	}

	if (nlines <= 0)
		return 0;

	f->c_nlines = nlines;
	for (i = 0, newline = f->c_block; i < nlines; newline++, i++)
		freeline(newline);
	f->c_nextfree = fchunk;
	fchunk = f;
	return 1;
}

/* New BUFfer LINE */

Line *
nbufline()
{
	register Line	*newline;

	if (ffline == 0)	/* No free list */
		if (newchunk() == 0)
			complain("[Out of lines] ");
	newline = ffline;
	ffline = ffline->l_next;
	if (ffline)
		ffline->l_prev = 0;
	return newline;
}

/* Remove the free lines, in chunk c, from the free list because they are
   no longer free. */

static
remfreelines(c)
register struct chunk	*c;
{
	register Line	*lp;
	register int	i;

	for (lp = c->c_block, i = 0; i < c->c_nlines; i++, lp++) {
		if (lp->l_prev)
			lp->l_prev->l_next = lp->l_next;
		else
			ffline = lp->l_next;
		if (lp->l_next)
			lp->l_next->l_prev = lp->l_prev;
	}
}

/* This is used to garbage collect the chunks of lines when malloc fails
   and we are NOT looking for a new buffer line.  This goes through each
   chunk, and if every line in a given chunk is not allocated, the entire
   chunk is `free'd by "free()". */

GCchunks()
{
	register struct chunk	*cp;
	struct chunk	*prev = 0,
			*next = 0;
	register int	i;
	register Line	*newline;

 	for (cp = fchunk; cp != 0; cp = next) {
		for (i = 0, newline = cp->c_block; i < cp->c_nlines; newline++, i++)
			if (newline->l_dline != 0)
				break;

 		next = cp->c_nextfree;

		if (i == cp->c_nlines) {		/* Unlink it!!! */
			if (prev)
				prev->c_nextfree = cp->c_nextfree;
			else
				fchunk = cp->c_nextfree;
			remfreelines(cp);
			free((char *) cp->c_block);
			free((char *) cp);
		} else
			prev = cp;
	}
}

#ifdef LISP

/* Grind S-Expr */

GSexpr()
{
	Bufpos	dot,
		end;

	if (linebuf[curchar] != '(')
		complain((char *) 0);
	DOTsave(&dot);
	FSexpr();
	DOTsave(&end);
	exp = 1;
	SetDot(&dot);
	for (;;) {
		if (curline == end.p_line)
			break;
		line_move(FORWARD, NO);
		if (!blnkp(linebuf)) {
			DelWtSpace();
			(void) lisp_indent();
		}
	}
	SetDot(&dot);
}

/* lisp_indent() indents a new line in Lisp Mode, according to where
   the matching close-paren would go if we typed that (sort of). */

Bufpos *
lisp_indent()
{
	Bufpos	*bp,
		savedot;
	int	goal;

	bp = m_paren(')', BACKWARD, NO, YES);

	if (bp == 0)
		return 0;

	/*
	 * Otherwise, we indent to the first argument of
	 * the current s-expression.  This is done by
	 * starting at the matching paren, skipping
	 * to a word (atom), skipping over it, and
	 * skipping to the next one.
	 *
	 * We want to end up
	 *
	 *	(atom atom atom ...
	 *	      ^ here.
	 */

	DOTsave(&savedot);
	SetDot(bp);
	DoTimes(ForChar(), 1);
	if (linebuf[curchar] != '(') {
		static char	*specials[] = {
			"def",
			"let",
			"lambda",
			"fluid-let",
			"macro",
			"lexpr",
			"nlambda",
			"dolist",
			"caseq",
			"selectq",
			"while",
			"prog",
			0
		};
		int	i = 0;

		while (specials[i]) {
			char	*cp1 = specials[i],
				*cp2 = &linebuf[curchar];
			int	n = strlen(cp1);

			while (--n >= 0)
				if (Upper(*cp1++) != Upper(*cp2++))
					break;
			if (n < 0)
				break;	/* Matched. */
			i++;
		}
		if (specials[i] == 0) {
			if (index(&linebuf[curchar], ' ') != 0) {
			    WITH_TABLE(curbuf->b_major)
				ForWord();
			    END_TABLE();
				while (linebuf[curchar] == ' ')
					curchar++;
			}
		} else
			curchar++;
	}
	goal = calc_pos(linebuf, curchar);
	SetDot(&savedot);
	n_indent(goal);

	return bp;
}
#endif LISP
