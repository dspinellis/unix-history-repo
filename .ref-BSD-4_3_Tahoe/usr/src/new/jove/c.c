/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Contains commands for C mode.  Paren matching routines are in here. */

#include "jove.h"
#include "re.h"
#include "ctype.h"

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private int
	backslashed(char *, int);
private void	
	do_expr(int, int),
	FindMatch(int),
	parse_cmt_fmt(char *),
	strip_c(char *, char *);
#else
private int
	backslashed();
private void	
	do_expr(),
	FindMatch(),
	parse_cmt_fmt(),
	strip_c();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif


private int
backslashed(lp, cpos)
register char	*lp;
register int	cpos;
{
	register int	cnt = 0;

	while (cpos > 0 && lp[--cpos] == '\\')
		cnt += 1;
	return (cnt % 2);
}

private char	*p_types = "(){}[]";
private int	mp_kind;
#define MP_OKAY		0
#define MP_MISMATCH	1
#define MP_UNBALANCED	2

void
mp_error()
{
	switch (mp_kind) {
	case MP_MISMATCH:
		message("[Mismatched parentheses]");
		break;

	case MP_UNBALANCED:
		message("[Unbalanced parenthesis]");
		break;

	case MP_OKAY:
	default:
		return;
	}
	rbell();
}

/* Search from the current position for the paren that matches p_type.
   Search in the direction dir.  If can_mismatch is YES then it is okay
   to have mismatched parens.  If stop_early is YES then when an open
   paren is found at the beginning of a line, it is assumed that there
   is no point in backing up further.  This is so when you hit tab or
   LineFeed outside, in-between procedure/function definitions, it won't
   sit there searching all the way to the beginning of the file for a
   match that doesn't exist.  {forward,backward}-s-expression are the
   only ones that insist on getting the "true" story. */

Bufpos *
m_paren(p_type, dir, can_mismatch, can_stop)
char	p_type;
register int	dir;
{
	static Bufpos	ret;
	Bufpos	savedot,
		*sp;
	char	re_buf[100],
		*re_alts[NALTS];
	int	count = 0;
	register char	*lp,
			c;
	char	p_match,
		re_str[128],
		*cp,
		quote_c = 0;
	register int	c_char;
	int	in_comment = -1,
		stopped = NO;

	sprintf(re_str, "[(){}[\\]%s]", (MajorMode(CMODE)) ? "/\"'" : "\"");
	REcompile(re_str, 1, re_buf, re_alts);
	if (cp = index(p_types, p_type))
		p_match = cp[dir];
	else
		complain("[Cannot match %c's]", p_type);
	DOTsave(&savedot);

	/* To make things a little faster I avoid copying lines into
	   linebuf by setting curline and curchar by hand.  Warning:
	   this is slightly to very risky.  When I did this there were
	   lots of problems with procedures that expect the contents of
	   curline to be in linebuf. */
	while (count >= 0) {
		sp = docompiled(dir, re_buf, re_alts);
		if (sp == 0)
			break;
		lp = lbptr(sp->p_line);

		if (sp->p_line != curline)
			/* let's assume that strings do NOT go over line
			   bounderies (for now don't check for wrapping
 			   strings) */
			quote_c = 0;
		curline = sp->p_line;
		curchar = sp->p_char;	/* here's where I cheat */
		c_char = curchar;
		if (dir == FORWARD)
			c_char -= 1;
		if (backslashed(lp, c_char))
			continue;
		c = lp[c_char];
		/* check if this is a comment (if we're not inside quotes) */
		if (quote_c == 0 && c == '/') {
			int	new_ic;

			if ((c_char != 0) && lp[c_char - 1] == '*') {
				new_ic = (dir == FORWARD) ? NO : YES;
				if (new_ic == NO && in_comment == -1) {
					count = 0;
					quote_c = 0;
				}
			} else if (lp[c_char + 1] == '*') {
				new_ic = (dir == FORWARD) ? YES : NO;
				if (new_ic == NO && in_comment == -1) {
					count = 0;
					quote_c = 0;
				}
			}
			in_comment = new_ic;
		}
		if (in_comment == YES)
			continue;
		if (c == '"' || c == '\'') {
			if (quote_c == c)
				quote_c = 0;
			else if (quote_c == 0)
				quote_c = c;
		}
		if (quote_c != 0)
			continue;
		if (isopenp(c)) {
			count += dir;
			if (c_char == 0 && can_stop == YES && count >= 0) {
				stopped = YES;
				break;
			}
		} else if (isclosep(c))
			count -= dir;
	}

	ret.p_line = curline;
	ret.p_char = curchar;

	curline = savedot.p_line;
	curchar = savedot.p_char;	/* here's where I undo it */

	if (count >= 0)
		mp_kind = MP_UNBALANCED;
	else if (c != p_match)
		mp_kind = MP_MISMATCH;
	else
		mp_kind = MP_OKAY;

	/* If we stopped (which means we were allowed to stop) and there
	   was an error, we clear the error so no error message is printed.
	   An error should be printed ONLY when we are sure about the fact,
	   namely we didn't stop prematurely HOPING that it was the right
	   answer. */
	if (stopped && mp_kind != MP_OKAY) {
		mp_kind = MP_OKAY;
		return 0;
	}
	if (mp_kind == MP_OKAY || (mp_kind == MP_MISMATCH && can_mismatch == YES))
		return &ret;
	return 0;
}

private void
do_expr(dir, skip_words)
register int	dir;
{
	register char	c,
			syntax = (dir == FORWARD) ? _Op : _Cl;

	if (dir == BACKWARD)
		b_char(1);
	c = linebuf[curchar];
	for (;;) {
		if (!skip_words && ismword(c)) {
		    WITH_TABLE(curbuf->b_major)
		    if (dir == FORWARD)
		    	f_word(1);
		    else
		    	b_word(1);	
		    END_TABLE();
		    break;
		} else if (has_syntax(c, syntax)) {
			FindMatch(dir);
			break;
		}
		f_char(dir);
		if (eobp() || bobp())
			return;
		c = linebuf[curchar];
	}
}

void
FSexpr()
{
	register int	num = arg_value();

	if (num < 0) {
		set_arg_value(-num);
		BSexpr();
	}
	while (--num >= 0)
		do_expr(FORWARD, NO);
}

void
FList()
{
	register int	num = arg_value();

	if (num < 0) {
		set_arg_value(-num);
		BList();
	}
	while (--num >= 0)
		do_expr(FORWARD, YES);
}

void
BSexpr()
{
	register int	num = arg_value();

	if (num < 0) {
		negate_arg_value();
		FSexpr();
	}
	while (--num >= 0)
		do_expr(BACKWARD, NO);
}

void
BList()
{
	register int	num = arg_value();

	if (num < 0) {
		negate_arg_value();
		FList();
	}
	while (--num >= 0)
		do_expr(BACKWARD, YES);
}

void
BUpList()
{
	Bufpos	*mp;
	char	c = (MajorMode(CMODE) ? '}' : ')');

	mp = m_paren(c, BACKWARD, NO, YES);
	if (mp == 0)
		mp_error();
	else
		SetDot(mp);
}

void
FDownList()
{
	Bufpos	*sp;
	char	*sstr = (MajorMode(CMODE) ? "[{([\\])}]" : "[()]"),
		*lp;

	sp = dosearch(sstr, FORWARD, YES);
	if (sp != 0)
		lp = lcontents(sp->p_line);
	if (sp == 0 || has_syntax(lp[sp->p_char - 1], _Cl))
		complain("[No contained expression]");
	SetDot(sp);
}

/* Move to the matching brace or paren depending on the current position
   in the buffer. */

private void
FindMatch(dir)
{
	register Bufpos	*bp;
	register char	c = linebuf[curchar];

	if ((index(p_types, c) == 0) ||
	    (backslashed(linebuf, curchar)))
		complain((char *) 0);
	if (dir == FORWARD)
		f_char(1);
	bp = m_paren(c, dir, YES, NO);
	if (dir == FORWARD)
		b_char(1);
	if (bp != 0)
		SetDot(bp);
	mp_error();	/* if there is an error the user wants to
			   know about it */
}

Bufpos *
c_indent(incrmt)
{
	Bufpos	*bp;
	int	indent = 0;

	if (bp = m_paren('}', BACKWARD, NO, YES)) {
		Bufpos	save;

		DOTsave(&save);
		SetDot(bp);
		ToIndent();
		indent = calc_pos(linebuf, curchar);
		SetDot(&save);
	}
	if (incrmt) {
		if (indent == 0)
			incrmt = tabstop;
		else
			incrmt = (tabstop - (indent%tabstop));
	}
	n_indent(indent + incrmt);
	return bp;
}

#ifdef CMT_FMT

char	CmtFmt[80] = "/*%n%! * %c%!%n */";

void
Comment()
{
	FillComment(CmtFmt);
}

/* Strip leading and trailing white space.  Skip over any imbedded '\r's. */

private void
strip_c(from, to)
char	*from,
	*to;
{
	register char	*fr_p = from,
			*to_p = to,
			c;

	while (c = *fr_p) {
		if (c == ' ' || c == '\t' || c == '\r')
			fr_p += 1;
		else
			break;
	}
	while (c = *fr_p) {
		if (c != '\r')
			*to_p++ = c;
		fr_p += 1;
	}
	while (--to_p >= to)
		if (*to_p != ' ' && *to_p != '\t')
			break;
	*++to_p = '\0';
}

private char	open_c[20],	/* the open comment format string */
		open_pat[20],	/* the search pattern for open comment */
		l_header[20],	/* the prefix for each comment line */
		l_trailer[20],	/* the suffix ... */
		close_c[20],
		close_pat[20];

private char	*comment_body[] = {
 	open_c,
	l_header,
	l_trailer,
	close_c
};
					
private int	nlflags;

/* Fill in the data structures above from the format string.  Don't return
   if there's trouble. */

private void
parse_cmt_fmt(str)
char	*str;
{
	register char	*fmtp = str;
	register char	**c_body = comment_body,
			*body_p = *c_body;
	int	c,
	 	newlines = 1;

	/* pick apart the comment string */
	while (c = *fmtp++) {
		if (c != '%') {
			*body_p++ = c;
			continue;
		}
		switch(c = *fmtp++) {
		case 'n':
			if (newlines == 2 || newlines == 3)
				complain("%n not allowed in line header or trailer: %s",
				  fmtp - 2);
			nlflags += newlines;
			*body_p++ = '\r';
			break;
		case 't':
			*body_p++ = '\t';
			break;
		case '%':
			*body_p++ = '%';
			break;
		case '!':
		case 'c':
			newlines += 1;
			*body_p++ = '\0';
			body_p = *++c_body;
			break;
		default:
			complain("[Unknown comment escape: %%%c]", c);
			/* VARARGS */
			break;
		}
	}
	*body_p = '\0';
	/* make search patterns */
	strip_c(open_c, open_pat);
	strip_c(close_c, close_pat);
}

#define NL_IN_OPEN_C  ((nlflags % 4) == 1)
#define NL_IN_CLOSE_C (nlflags >= 4)

void
FillComment(format)
char	*format;
{
	int	saveRMargin,
		indent_pos,
		close_at_dot = NO,
		slen,
		header_len,
		trailer_len;
	register char	*cp;
	static char	inside_err[] = "[Must be between %s and %s to re-format]";
	Bufpos	open_c_pt,
		close_c_pt,
		tmp_bp,
		*match_o,
		*match_c;
	Mark	*entry_mark,
		*open_c_mark,
		*savedot;

	parse_cmt_fmt(format);
	/* figure out if we're "inside" a comment */
 	if ((match_o = dosearch(open_pat, BACKWARD, 0)) == 0)
		/* VARARGS */
		complain("No opening %s to match to.", open_pat);
	open_c_pt = *match_o;
	if ((match_c = dosearch(close_pat, BACKWARD, NO)) != 0 &&
	    inorder(open_c_pt.p_line, open_c_pt.p_char,
		    match_c->p_line, match_c->p_char))
	  	complain(inside_err, open_pat, close_pat);
	if ((match_o = dosearch(open_pat, FORWARD, NO)) != 0) {
		tmp_bp = *match_o;
		match_o = &tmp_bp;
	} 
	if ((match_c = dosearch(close_pat, FORWARD, 0)) != (Bufpos *) 0)
		close_c_pt = *match_c;

	/* Here's where we figure out whether to format from dot or from
	   the close comment.  Note that we've already searched backwards to
	   find the open comment symbol for the comment we are formatting.
	   The open symbol mentioned below refers to the possible existence
	   of the next comment.  There are 5 cases:
		1) no open or close symbol		==> dot
		2) open, but no close symbol		==> dot
		3) close, but no open			==> close
		4) open, close are inorder		==> dot
		5) open, close are not inorder		==> close */


	if (match_o == (Bufpos *) 0) {
		if (match_c == (Bufpos *) 0)
			close_at_dot = YES;
	} else if (match_c == (Bufpos *) 0)
		close_at_dot = YES;
	else if (inorder(match_o->p_line, match_o->p_char,
		 match_c->p_line, match_c->p_char))
		close_at_dot = YES;
	if (close_at_dot) {
		close_c_pt.p_line = curline;
		close_c_pt.p_char = curchar;
	} else {
		SetDot(match_c);
	}
	SetDot(&open_c_pt);
	open_c_mark = MakeMark(curline, curchar, M_FLOATER);
	indent_pos = calc_pos(linebuf, curchar);
	/* search for a close comment; delete it if it exits */
	SetDot(&close_c_pt);
	if (close_at_dot == 0) {
		slen = strlen(close_pat);
		while (slen--)
			del_char(BACKWARD, 1);
	}
	entry_mark = MakeMark(curline, curchar, M_FLOATER);
	ToMark(open_c_mark);
	/* always separate the comment body from anything preceeding it */
	LineInsert(1);
	DelWtSpace();
	Bol();
	for (cp = open_c; *cp; cp++) {
		if (*cp == '\r') {
			if (!eolp())
				LineInsert(1);
			else
				line_move(FORWARD, 1, NO);
		} else if (*cp == ' ' || *cp == '\t') {
			if (linebuf[curchar] != *cp)
				insert_c(*cp, 1);
		} else
			/* Since we matched the open comment string on this
			   line, we don't need to worry about crossing line
			   boundaries. */
			curchar += 1;
	}
	savedot = MakeMark(curline, curchar, M_FLOATER);

	/* We need to strip the line header pattern of leading white space
	   since we need to match the line after all of its leading
	   whitespace is gone. */
	for (cp = l_header; *cp && (isspace(*cp)); cp++)
		;
	header_len = strlen(cp);
	trailer_len = strlen(l_trailer);

	/* Strip each comment line of the open and close comment strings
	   before reformatting it. */

	do {
		Bol();
		DelWtSpace();
		if (header_len && !strncmp(linebuf, cp, header_len))
			del_char(FORWARD, header_len);
		if (trailer_len) {
			Eol();
			if ((curchar > trailer_len) &&
			    (!strncmp(&linebuf[curchar - trailer_len],
				      l_trailer, trailer_len)))
				del_char(BACKWARD, trailer_len);
		}
		if (curline->l_next != 0)
			line_move(FORWARD, 1, NO);
		else
			break;
	} while (curline != entry_mark->m_line->l_next);

	do_set_mark(savedot->m_line, savedot->m_char);
	ToMark(entry_mark);
	saveRMargin = RMargin;
	RMargin = saveRMargin - strlen(l_header) -
		  strlen(l_trailer) - indent_pos + 2;
	do_rfill(NO);
	RMargin = saveRMargin;
	/* get back to the start of the comment */
	PopMark(); 
	do {
		if (curline == open_c_mark->m_line->l_next) {
			;
		} else {
			n_indent(indent_pos);
			ins_str(l_header, NO);
		}
		Eol();
		if (!NL_IN_CLOSE_C && (curline == entry_mark->m_line))
			;
		else
			ins_str(l_trailer, NO);
		if (curline->l_next != 0)
			line_move(FORWARD, 1, NO);
		else 
			break;
	} while (curline != entry_mark->m_line->l_next);
	/* handle the close comment symbol */
	if (curline == entry_mark->m_line->l_next) {
		line_move(BACKWARD, 1, NO);
		Eol();
	}
	DelWtSpace();
	/* if the addition of the close symbol would cause the line to be
	   too long, put the close symbol on the next line. */
	if (!(NL_IN_CLOSE_C) &&
	  strlen(close_c) + calc_pos(linebuf, curchar) > RMargin) {
		LineInsert(1);
		n_indent(indent_pos);
	}
	for (cp = close_c; *cp; cp++) {
		if (*cp == '\r') {
			LineInsert(1);
			n_indent(indent_pos);
		} else
			insert_c(*cp, 1);
	}
	ToMark(open_c_mark);
	Eol();
	del_char(FORWARD, 1);
}

#endif /* CMT_FMT */

