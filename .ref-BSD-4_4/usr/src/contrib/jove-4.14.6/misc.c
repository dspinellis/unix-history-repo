/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"
#include "disp.h"

#include <signal.h>

void
prCTIME()
{
	s_mess(": %f %s", get_time((time_t *)NULL, (char *)NULL, 0, -1));
}

void
ChrToOct()
{
	int	c,
		slow = NO;

	c = waitchar(&slow);
	ins_str(sprint("\\%03o", c), NO);
}

void
StrLength()
{
	static const char	inquotes[] = "Where are the quotes?";
	char	*first = StrIndex(BACKWARD, linebuf, curchar, '"'),
		*last = StrIndex(FORWARD, linebuf, curchar + 1, '"'),
		c;
	int	numchars = 0;

	if (first == NULL || last == NULL)
		complain(inquotes);
	first += 1;
	while (first < last) {
		c = *first++;
		if (c == '\\') {
			int	num;

			if (!jisdigit(*first)) {
				first += 1;
			} else {
				num = 3;
				do ; while (num-- && jisdigit(*first++) && first < last);
			}
		}
		numchars += 1;
	}
	s_mess("%d characters", numchars);
}

/* Transpos cur_char with cur_char - 1 */

void
TransChar()
{
	char	before;

	if (curchar == 0 || (eolp() && curchar == 1))
		complain((char *)NULL);	/* BEEP */
	if (eolp())
		b_char(1);
	before = linebuf[curchar - 1];
	del_char(BACKWARD, 1, NO);
	f_char(1);
	insert_c(before, 1);
}

/* Switch current line with previous one */

void
TransLines()
{
	daddr	old_prev;

	if (firstp(curline))
		return;
	lsave();
	old_prev = curline->l_prev->l_dline;
	curline->l_prev->l_dline = curline->l_dline;
	curline->l_dline = old_prev;
	getDOT();
	if (!lastp(curline))
		line_move(FORWARD, 1, NO);
	modify();
}

void
Leave()
{
	longjmp(mainjmp, QUIT);
}

/* If argument is specified, kill that many lines down.  Otherwise,
   if we "appear" to be at the end of a line, i.e. everything to the
   right of the cursor is white space, we delete the line separator
   as if we were at the end of the line. */

void
KillEOL()
{
	Line	*line2;
	int	char2;
	int	num = arg_value();

	if (is_an_arg()) {
		if (num == 0) {	/* Kill to beginning of line */
			line2 = curline;
			char2 = 0;
		} else {
			line2 = next_line(curline, num);
			if ((LineDist(curline, line2) < num) || (line2 == curline))
				char2 = length(line2);
			else
				char2 = 0;
		}
	} else if (blnkp(&linebuf[curchar])) {
		line2 = next_line(curline, 1);
		if (line2 == curline)
			char2 = length(curline);
		else
			char2 = 0;
	} else {
		line2 = curline;
		char2 = length(curline);
	}
	reg_kill(line2, char2, NO);
}

/* kill to beginning of sentence */

void
KillBos()
{
	negate_arg_value();
	KillEos();
}

/* Kill to end of sentence */

void
KillEos()
{
	Line	*line1;
	int	char1;

	line1 = curline;
	char1 = curchar;
	Eos();
	reg_kill(line1, char1, YES);
}

void
KillExpr()
{
	Line	*line1;
	int	char1;

	line1 = curline;
	char1 = curchar;
	FSexpr();
	reg_kill(line1, char1, YES);
}

void
Yank()
{
	Line	*line,
		*lp;
	Bufpos	*dot;

	if (killbuf[killptr] == NULL)
		complain("[Nothing to yank!]");
	lsave();
	this_cmd = YANKCMD;
	line = killbuf[killptr];
	lp = lastline(line);
	dot = DoYank(line, 0, lp, length(lp), curline, curchar, curbuf);
	set_mark();
	SetDot(dot);
}

void
WtModBuf()
{
	if (!ModBufs(NO))
		message("[No buffers need saving]");
	else
		put_bufs(is_an_arg());
}

void
put_bufs(askp)
bool	askp;
{
	register Buffer	*oldb = curbuf,
			*b;

	for (b = world; b != NULL; b = b->b_next) {
		if (!IsModified(b) || b->b_type != B_FILE)
			continue;
		SetBuf(b);	/* Make this current Buffer */
		if (curbuf->b_fname == NULL) {
			char	*newname;

			newname = ask(NullStr, "Buffer \"%s\" needs a file name; type Return to skip: ", b->b_name);
			if (*newname == '\0')
				continue;
			setfname(b, newname);
		}
		if (askp && (yes_or_no_p("Write %s? ", curbuf->b_fname) == NO))
			continue;
		filemunge(curbuf->b_fname);
		chk_mtime(curbuf, curbuf->b_fname, "save");
		file_write(curbuf->b_fname, NO);
	}
	SetBuf(oldb);
}

void
ToIndent()
{
	Bol();
	skip_wht_space();
}

void
skip_wht_space()
{
	register char	*cp,
			c;

	for (cp = linebuf + curchar; (c = *cp)!='\0'; cp++)
		if (c != ' ' && c != '\t')
			break;
	curchar = cp - linebuf;
}

/* GoLine -- go to a line, usually wired to goto-line, ESC g or ESC G.
   If no argument is specified it asks for a line number. */
void
GoLine()
{
	Line	*newline;

	if (!is_an_arg())
		set_arg_value(ask_int("Line: ",10));
	newline = next_line(curbuf->b_first, arg_value() - 1);
	PushPntp(newline);
	SetLine(newline);
}

void
NotModified()
{
	unmodify();
}

void
SetLMargin()
{
	int	lmarg = calc_pos(linebuf, curchar);

	if (lmarg >= RMargin)
		complain("[Left margin must be left of right margin]");
	LMargin = lmarg;
}

void
SetRMargin()
{
	int	rmarg = calc_pos(linebuf, curchar);

	if (rmarg <= LMargin)
		complain("[Right margin must be right of left margin]");
	RMargin = rmarg;
}

/*
 *	Mouse support for Xterm
 *	The Xterm program sends
 *	Esc [ M
 *		button + space
 *		col + space + 1
 *		row + space + 1
 */
static void MoveToCursor(/* int, int */);

void
XtermMouse()
{
	int mouse;
	int line;
	int col;
	int slow = NO;
	register struct scrimage *sp;
	char	*msg;
	
	mouse = waitchar(&slow) - ' ';
	col = waitchar(&slow) - ' ' - 1;
	line = waitchar(&slow) - ' ' - 1;

	sp = &PhysScreen[line];

	while (sp->s_id == NULL)
		sp = &PhysScreen[--line];
	if (curwind != sp->s_window)
		SetWind(sp->s_window);
	if (sp->s_flags & MODELINE)
	{
		switch (mouse)
		{
		case 0:
			WindSize(curwind, 1);
			msg = "[Grow window]";
			break;
		default:
			WindSize(curwind, -1);
			msg = "[Shrink window]";
			break;
		}
		s_mess(msg);

	}
	else
	{	SetLine(sp->s_lp);
		curchar = how_far(sp->s_lp, col);
		switch (mouse)
		{
		case 0:			/* left button */
			set_mark();
			break;
		case 1:			/* paste on middle button */
			Yank();
			s_mess("[Region pasted]");
			break;
		case 2:			/* end of region on right button */
			CopyRegion();
			s_mess("[Region copied]");
			break;
		}
	}
}
