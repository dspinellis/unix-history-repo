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

static int	line_pos;

ForChar()
{
	register int	num = exp;

	if (exp < 0) {
		exp = -exp;
		BackChar();
		return;
	}
	exp = 1;
	while (--num >= 0) {
		if (eolp()) {			/* Go to the next Line */
			if (curline->l_next == 0)
				break;
			SetLine(curline->l_next);
		} else
			curchar++;
	}
}

BackChar()
{
	register int	num = exp;

	if (exp < 0) {
		exp = -exp;
		ForChar();
		return;
	}
	exp = 1;
	while (--num >= 0) {
		if (bolp()) {
			if (curline->l_prev == 0)
				break;
			SetLine(curline->l_prev);
			Eol();
		} else
			--curchar;
	}
}

NextLine()
{
	if ((curline == curbuf->b_last) && eolp())
		complain(NullStr);
	line_move(FORWARD, YES);
}

PrevLine()
{
	if ((curline == curbuf->b_first) && bolp())
		complain(NullStr);
	line_move(BACKWARD, YES);
}

/* moves to a different line in DIR; LINE_CMD says whether this is
   being called from NextLine() or PrevLine(), in which case it tries
   to line up the column with the column of the current line */

line_move(dir, line_cmd)
{
	Line	*(*proc)() = (dir == FORWARD) ? next_line : prev_line;
	Line	*line;

	line = (*proc)(curline, exp);
	if (line == curline) {
		(dir == FORWARD) ? Eol() : Bol();
		return;
	}

	if (line_cmd) {
		this_cmd = LINECMD;
		if (last_cmd != LINECMD)
			line_pos = calc_pos(linebuf, curchar);
	}
	SetLine(line);		/* curline is in linebuf now */
	if (line_cmd)
		curchar = how_far(curline, line_pos);
}

/* returns what cur_char should be for that position col */

how_far(line, col)
Line	*line;
{
	register char	*lp;
	register int	pos,
			c;
	char	*base;

	base = lp = lcontents(line);
	pos = 0;

	while (pos < col && (c = (*lp & 0177))) {
		if (c == '\t')
			pos += (tabstop - (pos % tabstop));
		else if (isctrl(c))
			pos += 2;
		else
			pos++;
		lp++;
	}

	return lp - base;
}

Bol()
{
	curchar = 0;
}

Eol()
{
	curchar = strlen(linebuf);
}

Eof()
{
	PushPntp(curbuf->b_last);
	ToLast();
}

Bof()
{
	PushPntp(curbuf->b_first);
	ToFirst();
}

/* Move forward (if dir > 0) or backward (if dir < 0) a sentence.  Deals
   with all the kludgery involved with paragraphs, and moving backwards
   is particularly yucky. */

to_sent(dir)
{
	Bufpos	*new,
		old;
	extern char	*ParaStr;

	DOTsave(&old);

	new = dosearch("^[ \t]*$\\|[?.!]", dir, 1);
	if (new == 0) {
		(dir < 0) ? ToFirst() : ToLast();
		return;
	}
	SetDot(new);
	if (dir < 0) {
		to_word(1);
		if ((old.p_line == curline && old.p_char <= curchar) ||
		    (inorder(new->p_line, new->p_char, old.p_line, old.p_char) &&
		     inorder(old.p_line, old.p_char, curline, curchar))) {
		     	SetDot(new);
		     	to_sent(dir);
		}
		return;		/* We're there? */
	}
	if (blnkp(linebuf)) {
		Bol();
		BackChar();
		if (old.p_line == curline && old.p_char >= curchar) {
			to_word(1);	/* Oh brother this is painful */
			to_sent(1);
		}
	} else {
		extern int	REbom;

		curchar = REbom + 1;	/* Just after the [?.!] */
		if (LookingAt("[\")]  *\\|[\")]$", linebuf, curchar))
			curchar++;
		else if (!eolp() && !LookingAt("  *", linebuf, curchar))
			to_sent(dir);
	}
}

Bos()
{
	int	num = exp;

	if (exp < 0) {
		exp = -exp;
		Eos();
		return;
	}

	exp = 1;

	while (--num >= 0) {
		to_sent(-1);
		if (bobp())
			break;
	}
}

Eos()
{
	int	num = exp;

	if (exp < 0) {
		exp = -exp;
		Bos();
		return;
	}

	exp = 1;

	while (--num >= 0) {
		to_sent(1);
		if (eobp())
			break;
	}
}

ForWord()
{
	register char	c;
	register int	num = exp;

	if (exp < 0) {
		exp = -exp;
		BackWord();
		return;
	}
	exp = 1;
	while (--num >= 0) {
		to_word(1);
		while ((c = linebuf[curchar]) != 0 && isword(c))
			curchar++;
		if (eobp())
			break;
	}
	this_cmd = 0;	/* Semi kludge to stop some unfavorable behavior */
}

BackWord()
{
	register int	num = exp;
	register char	c;

	if (exp < 0) {
		exp = -exp;
		ForWord();
		return;
	}
	exp = 1;
	while (--num >= 0) {
		to_word(-1);
		while (!bolp() && (c = linebuf[curchar - 1], isword(c)))
			--curchar;
		if (bobp())
			break;
	}
	this_cmd = 0;
}
