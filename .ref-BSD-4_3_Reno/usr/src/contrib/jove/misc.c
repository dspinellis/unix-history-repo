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
	s_mess(": %f %s", get_time((time_t *) 0, (char *) 0, 0, -1));
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
	static char	inquotes[] = "Where are the quotes?";
	char	*first = StrIndex(-1, linebuf, curchar, '"'),
		*last = StrIndex(1, linebuf, curchar + 1, '"'),
		c;
	int	numchars = 0;

	if (first == 0 || last == 0)
		complain(inquotes);
	first += 1;
	while (first < last) {
		c = *first++;
		if (c == '\\') {
			int	num;

			if (!isdigit(*first))
				first += 1;
			else {
				num = 3;
				while (num-- && isdigit(*first++) && first < last)
					;
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
		complain((char *) 0);	/* BEEP */
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
	reg_kill(line2, char2, 0);
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
	reg_kill(line1, char1, 1);
}

void
KillExpr()
{
	Line	*line1;
	int	char1;

	line1 = curline;
	char1 = curchar;
	FSexpr();
	reg_kill(line1, char1, 1);
}

void
Yank()
{
	Line	*line,
		*lp;
	Bufpos	*dot;

	if (killbuf[killptr] == 0)
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
int	askp;
{
	register Buffer	*oldb = curbuf,
			*b;

	for (b = world; b != 0; b = b->b_next) {
		if (!IsModified(b) || b->b_type != B_FILE)
			continue;
		SetBuf(b);	/* Make this current Buffer */
		if (curbuf->b_fname == 0) {
			char	*newname;

			newname = ask(NullStr, "Buffer \"%s\" needs a file name; type Return to skip: ", b->b_name);
			if (*newname == 0)
				continue;
			setfname(b, newname);
		}
		if (askp && (yes_or_no_p("Write %s? ", curbuf->b_fname) == NO))
			continue;
		filemunge(curbuf->b_fname);
#if !(defined(MSDOS) || defined(MAC))
		chk_mtime(curbuf, curbuf->b_fname, "save");
#endif
		file_write(curbuf->b_fname, 0);
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

