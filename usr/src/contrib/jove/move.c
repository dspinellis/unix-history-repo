/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "re.h"
#include "ctype.h"
#include "disp.h"
private void to_sent proto((int));

private int	line_pos;

void
f_char(n)
register int	n;
{
	if (n < 0) {
		b_char(-n);
		return;
	}
	while (--n >= 0) {
		if (eolp()) {			/* Go to the next Line */
			if (curline->l_next == 0)
				break;
			SetLine(curline->l_next);
		} else
			curchar += 1;
	}
}

void
b_char(n)
register int	n;
{
	if (n < 0) {
		f_char(-n);
		return;
	}
	while (--n >= 0) {
		if (bolp()) {
			if (curline->l_prev == 0)
				break;
			SetLine(curline->l_prev);
			Eol();
		} else
			curchar -= 1;
	}
}

void
ForChar()
{
	f_char(arg_value());
}

void
BackChar()
{
	b_char(arg_value());
}

void
NextLine()
{
	if ((curline == curbuf->b_last) && eolp())
		complain(NullStr);
	line_move(FORWARD, arg_value(), YES);
}

void
PrevLine()
{
	if ((curline == curbuf->b_first) && bolp())
		complain(NullStr);
	line_move(BACKWARD, arg_value(), YES);
}

/* moves to a different line in DIR; LINE_CMD says whether this is
   being called from NextLine() or PrevLine(), in which case it tries
   to line up the column with the column of the current line */

void
line_move(dir, n, line_cmd)
int	dir,
	n,
	line_cmd;
{
	Line	*(*proc) proto((Line *, int)) =
		(dir == FORWARD) ? next_line : prev_line;
	Line	*line;

	line = (*proc)(curline, n);
	if (line == curline) {
		if (dir == FORWARD)
			Eol();
		else
			Bol();
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

int
how_far(line, col)
Line	*line;
int	col;
{
	register char	*lp;
	register int	pos,
			c;
	char	*base;

	base = lp = lcontents(line);
	pos = 0;

	while (pos < col && (c = (*lp & CHARMASK)) != '\0') {
		if (c == '\t')
			pos += (tabstop - (pos % tabstop));
		else if (isctrl(c))
			pos += 2;
		else
			pos += 1;
		lp += 1;
	}

	return lp - base;
}

void
Bol()
{
	curchar = 0;
}

void
Eol()
{
	curchar = length(curline);
}

void
Eof()
{
	PushPntp(curbuf->b_last);
	ToLast();
}

void
Bof()
{
	PushPntp(curbuf->b_first);
	ToFirst();
}

/* Move forward (if dir > 0) or backward (if dir < 0) a sentence.  Deals
   with all the kludgery involved with paragraphs, and moving backwards
   is particularly yucky. */

private void
to_sent(dir)
int	dir;
{
	Bufpos	*new,
		old;

	DOTsave(&old);

	new = dosearch("^[ \t]*$\\|[?.!]", dir, YES);
	if (new == 0) {
		if (dir == BACKWARD)
			ToFirst();
		else
			ToLast();
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
		b_char(1);
		if (old.p_line == curline && old.p_char >= curchar) {
			to_word(1);	/* Oh brother this is painful */
			to_sent(1);
		}
	} else {
		curchar = REbom + 1;	/* Just after the [?.!] */
		if (LookingAt("[\")]  *\\|[\")]$", linebuf, curchar))
			curchar += 1;
		else if (!eolp() && !LookingAt("  *", linebuf, curchar))
			to_sent(dir);
	}
}

void
Bos()
{
	register int	num = arg_value();

	if (num < 0) {
		negate_arg_value();
		Eos();
		return;
	}

	while (--num >= 0) {
		to_sent(-1);
		if (bobp())
			break;
	}
}

void
Eos()
{
	register int	num = arg_value();

	if (num < 0) {
		negate_arg_value();
		Bos();
		return;
	}

	while (--num >= 0) {
		to_sent(1);
		if (eobp())
			break;
	}
}

void
f_word(num)
register int	num;
{
	register char	c;
	if (num < 0) {
		b_word(-num);
		return;
	}
	while (--num >= 0) {
		to_word(FORWARD);
		while ((c = linebuf[curchar]) != 0 && isword(c))
			curchar += 1;
		if (eobp())
			break;
	}
	this_cmd = 0;	/* Semi kludge to stop some unfavorable behavior */
}

void
b_word(num)
register int	num;
{
	register char	c;

	if (num < 0) {
		f_word(-num);
		return;
	}
	while (--num >= 0) {
		to_word(BACKWARD);
		while (!bolp() && (c = linebuf[curchar - 1], isword(c)))
			curchar -= 1;
		if (bobp())
			break;
	}
	this_cmd = 0;
}

void
ForWord()
{
	f_word(arg_value());
}

void
BackWord()
{
	b_word(arg_value());
}
