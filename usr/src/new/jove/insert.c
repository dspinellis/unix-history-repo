/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"
#include "table.h"

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private int
	newchunk(void);
private void	
	init_specials(void),
	remfreelines(struct chunk *);
#else
private int
	newchunk();
private void	
	init_specials(),
	remfreelines();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif

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

void
LineInsert(num)
register int	num;
{
	char	newline[LBSIZE];
	register Line	*newdot,
			*olddot;
	int	oldchar;

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

/* Makes the indent of the current line == goal.  If the current indent
   is greater than GOAL it deletes.  If more indent is needed, it uses
   tabs and spaces to get to where it's going. */

void
n_indent(goal)
register int	goal;
{
	int	dotcol,
		incrmt;

	ToIndent();
	dotcol = calc_pos(linebuf, curchar);
	if (goal < dotcol) {
		DelWtSpace();
		dotcol = 0;
	}

	for (;;) {
		incrmt = (tabstop - (dotcol % tabstop));
		if (dotcol + incrmt > goal)
			break;
		insert_c('\t', 1);
		dotcol += incrmt;
	}
	if (dotcol != goal)
		insert_c(' ', (goal - dotcol));
}

#ifdef ABBREV
void
MaybeAbbrevExpand()
{
	if (MinorMode(Abbrev) && !ismword(LastKeyStruck) &&
	    !bolp() && ismword(linebuf[curchar - 1]))
		AbbrevExpand();
}
#endif

void
SelfInsert()
{
#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
	if (LastKeyStruck != CTL('J') && MinorMode(OverWrite)) {
		register int	num,
				i;

		for (i = 0, num = arg_value(); i < num; i++) {
			int	pos = calc_pos(linebuf, curchar);

			if (!eolp()) {
				if (linebuf[curchar] == '\t') {
					if ((pos + 1) == ((pos + tabstop) - (pos % tabstop)))
						del_char(FORWARD, 1);
				} else
					del_char(FORWARD, 1);
			}
			insert_c(LastKeyStruck, 1);
		}
	} else
		Insert(LastKeyStruck);

	if (MinorMode(Fill) && (curchar >= RMargin ||
			       (calc_pos(linebuf, curchar) >= RMargin))) {
		int margin;
		Bufpos save;

		if (MinorMode(Indent)) {
			DOTsave(&save);
			ToIndent();
			margin = calc_pos(linebuf, curchar);
			SetDot(&save);
		} else
			margin = LMargin;
		DoJustify(curline, 0, curline,
			  curchar + strlen(&linebuf[curchar]), 1, margin);
	}
}

void
Insert(c)
{
	if (c == CTL('J'))
		LineInsert(arg_value());
	else
		insert_c(c, arg_value());
}

/* insert character C N times at point */
void
insert_c(c, n)
{
	if (n <= 0)
		return;
	modify();
	makedirty(curline);
	ins_c(c, linebuf, curchar, n, LBSIZE);
	IFixMarks(curline, curchar, curline, curchar + n);
	curchar += n;
}	

/* Tab in to the right place for C mode */

void
Tab()
{
#ifdef LISP
	if (MajorMode(LISPMODE) && (bolp() || !eolp())) {
		int	dotchar = curchar;
		Mark	*m = 0;

		ToIndent();
		if (dotchar > curchar)
			m = MakeMark(curline, dotchar, M_FLOATER);
		(void) lisp_indent();
		if (m) {
			ToMark(m);
			DelMark(m);
		} else
			ToIndent();
		return;
	}
#endif
	if (MajorMode(CMODE) && strlen(linebuf) == 0)
		(void) c_indent(CIndIncrmt);
	else
		SelfInsert();
}

void
QuotChar()
{
	int	c,
		slow;

	c = waitchar(&slow);
	if (slow)
		message(key_strokes);
	if (c != CTL('@'))
		Insert(c);
}

/* Insert the paren.  If in C mode and c is a '}' then insert the
   '}' in the "right" place for C indentation; that is indented 
   the same amount as the matching '{' is indented. */

int	PDelay = 5,	/* 1/2 a second */
	CIndIncrmt = 8;

void
DoParen()
{
	Bufpos	*bp = (Bufpos *) -1;
	int	nx,
		c = LastKeyStruck;

	if (!isclosep(c)) {
		SelfInsert();
		return;
	}

	if (MajorMode(CMODE) && c == '}' && blnkp(linebuf))
		bp = c_indent(0);
#ifdef LISP
	if (MajorMode(LISPMODE) && c == ')' && blnkp(linebuf))
		bp = lisp_indent();
#endif
	SelfInsert();
#ifdef MAC
	if (MinorMode(ShowMatch) && !in_macro()) {
#else
	if (MinorMode(ShowMatch) && !charp() && !in_macro()) {
#endif
		b_char(1);	/* Back onto the ')' */
		if ((int) bp == -1)
			bp = m_paren(c, BACKWARD, NO, YES);
		f_char(1);
		if (bp != 0) {
			nx = in_window(curwind, bp->p_line);
			if (nx != -1) {		/* is visible */
				Bufpos	b;

				DOTsave(&b);
				SetDot(bp);
				SitFor(PDelay);
				SetDot(&b);
			} else
				s_mess("%s", lcontents(bp->p_line));
		}
		mp_error();	/* display error message */
	}
}

void
LineAI()
{
	DoNewline(TRUE);
}

void
Newline()
{
	DoNewline(MinorMode(Indent));
}	

void
DoNewline(indentp)
{
	Bufpos	save;
	int	indent;

	/* first we calculate the indent of the current line */
	DOTsave(&save);
	ToIndent();
	indent = calc_pos(linebuf, curchar);
	SetDot(&save);

#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
#ifdef LISP
	if (MajorMode(LISPMODE))
		DelWtSpace();
	else
#endif
	    if (indentp || blnkp(linebuf))
		DelWtSpace();
		
	/* If there is more than 2 blank lines in a row then don't make
	   a newline, just move down one. */
	if (arg_value() == 1 && eolp() && TwoBlank())
		SetLine(curline->l_next);
	else
		LineInsert(arg_value());

	if (indentp)
#ifdef LISP
	    if (MajorMode(LISPMODE))
		(void) lisp_indent();
	    else
#endif
		n_indent((LMargin == 0) ? indent : LMargin);
}

void
ins_str(str, ok_nl)
register char	*str;
{
	register char	c;
	Bufpos	save;
	int	llen;

	if (*str == 0)
		return;		/* ain't nothing to insert! */
	DOTsave(&save);
	llen = strlen(linebuf);
	while (c = *str++) {
		if (c == '\n' || (ok_nl && llen >= LBSIZE - 2)) {
			IFixMarks(save.p_line, save.p_char, curline, curchar);
			modify();
			makedirty(curline);
			LineInsert(1);
			DOTsave(&save);
			llen = strlen(linebuf);
		}
		if (c != '\n') {
			ins_c(c, linebuf, curchar++, 1, LBSIZE);
			llen += 1;
		}
	}
	IFixMarks(save.p_line, save.p_char, curline, curchar);
	modify();
	makedirty(curline);
}

void
open_lines(n)
{
	Bufpos	dot;

	DOTsave(&dot);
	LineInsert(n);	/* Open the lines... */
	SetDot(&dot);
}

void
OpenLine()
{
	open_lines(arg_value());
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

	getline(atline->l_dline, genbuf);
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

void
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

	if (arg_value() < 0)
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

private struct chunk	*fchunk = 0;
private Line	*ffline = 0;	/* First free line */

void
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

void
lfreelist(first)
register Line	*first;
{
	if (first)
		lfreereg(first, lastline(first));
}

/* Append region from line1 to line2 onto the free list of lines */

void
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

private int
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

private void
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

void
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

void
GSexpr()
{
	Bufpos	dot,
		end;

	if (linebuf[curchar] != '(')
		complain((char *) 0);
	DOTsave(&dot);
	FSexpr();
	DOTsave(&end);
	SetDot(&dot);
	for (;;) {
		if (curline == end.p_line)
			break;
		line_move(FORWARD, 1, NO);
		if (!blnkp(linebuf))
			(void) lisp_indent();
	}
	SetDot(&dot);
}

/* lisp_indent() indents a new line in Lisp Mode, according to where
   the matching close-paren would go if we typed that (sort of). */

private Table	*specials = NIL;

private void
init_specials()
{
	static char *words[] = {
		"case",
		"def",
		"dolist",
		"fluid-let",
		"lambda",
		"let",
		"lexpr",
		"macro",
		"named-l",	/* named-let and named-lambda */
		"nlambda",
		"prog",
		"selectq",
		0
	};
	char	**wordp = words;

	specials = make_table();
	while (*wordp)
		add_word(*wordp++, specials);
}

void
AddSpecial()
{
	char	*word;

	word = ask((char *) 0, ProcFmt);
	if (specials == NIL)
		init_specials();
	add_word(copystr(word), specials);
}

Bufpos *
lisp_indent()
{
	Bufpos	*bp,
		savedot;
	int	goal;

	bp = m_paren(')', BACKWARD, NO, YES);

	if (bp == 0)
		return 0;

	/* We want to end up
	 
	 	(atom atom atom ...
	 	      ^ here.
	 */

	DOTsave(&savedot);
	SetDot(bp);
	f_char(1);
	if (linebuf[curchar] != '(') {
		register Word	*wp;

		if (specials == NIL)
			init_specials();
		for (wp = table_top(specials); wp != NIL; wp = next_word(wp))
			if (casencmp(word_text(wp), &linebuf[curchar], word_length(wp)) == 0)
				break;
		if (wp == NIL) {	/* not special */
			int	c_char = curchar;

			WITH_TABLE(curbuf->b_major)
				f_word(1);
			END_TABLE();
			if (LookingAt("[ \t]*;\\|[ \t]*$", linebuf, curchar))
				curchar = c_char;
			else while (linebuf[curchar] == ' ')
				curchar += 1;
		} else
			curchar += 1;
	}
	goal = calc_pos(linebuf, curchar);
	SetDot(&savedot);
	n_indent(goal);

	return bp;
}
#endif /* LISP */
