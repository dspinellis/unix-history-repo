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
#include <signal.h>
#ifdef ANSICODES
#include "termcap.h"
#endif

Digit()
{
	GetExp(LastKeyStruck);
}

Digit0()
{
	GetExp('0');
}

Digit1()
{
	GetExp('1');
}

Digit2()
{
	GetExp('2');
}

Digit3()
{
	GetExp('3');
}

Digit4()
{
	GetExp('4');
}

Digit5()
{
	GetExp('5');
}

Digit6()
{
	GetExp('6');
}

Digit7()
{
	GetExp('7');
}

Digit8()
{
	GetExp('8');
}

Digit9()
{
	GetExp('9');
}

prCTIME()
{
	s_mess(": %f %s", get_time((long *) 0, (char *) 0, 0, -1));
}

extern int	alarmed;

FourTime()
{
	int	oldc = LastKeyStruck,
		newc;
	int	nexp;

	alarmed = 0;
	exp_p = 1;
	this_cmd = ARG_CMD;
	do {
		if ((nexp = exp * 4) != 0)
			exp = nexp;
		if (!alarmed)
			newc = waitchar();
		else
			newc = getch();
		if (alarmed)
			message(key_strokes);
	} while (newc == oldc);
	Ungetc(newc);
}

int	exp_p,
	exp;

GetExp(c)
{
	int	sign = 1,
		i,
		digited = 0;

	if (!isdigit(c) && c != '-')
		complain((char *) 0);
	if (exp_p)
		i = exp;
	else
		i = 0;
	if (c == '-') {
		sign = -1;
		goto goread;
	}
	for (;;) {
		if (alarmed)
			message(key_strokes);
		if (isdigit(c)) {
			i = i * 10 + (c - '0');
			digited++;
		} else {
			if (digited)
				exp_p = 1;
			else
				i = 1;
			exp = i * sign;
			this_cmd = ARG_CMD;
			Ungetc(c);
			return;
		}
goread:		if (!alarmed)
			c = waitchar();
		else {
			add_mess(NullStr);
			c = getch();
		}
	}
}

ChrToOct()
{
	int	c;

	c = waitchar();
	if (alarmed)
		message(key_strokes);
	ins_str(sprint("\\%03o", c), NO);
}

StrLength()
{
	static char	inquotes[] = "Where are the quotes?";
	char	*first = StrIndex(-1, linebuf, curchar, '"'),
		*last = StrIndex(1, linebuf, curchar + 1, '"'),
		c;
	int	numchars = 0;

	if (first == 0 || last == 0)
		complain(inquotes);
	first++;
	while (first < last) {
		c = *first++;
		if (c == '\\') {
			int	num;

			if (!isdigit(*first))
				++first;
			else {
				num = 3;
				while (num-- && isdigit(*first++) && first < last)
					;
			}
		}
		numchars++;
	}
	s_mess("%d characters", numchars);
}

/* Transpos cur_char with cur_char - 1 */

TransChar()
{
	char	before;

	if (curchar == 0 || (eolp() && curchar == 1))
		complain((char *) 0);	/* BEEP */
	exp = 1;
	if (eolp())
		BackChar();
	before = linebuf[curchar - 1];
	DelPChar();
	ForChar();
	Insert(before);
}

/* Switch current line with previous one */

TransLines()
{
	disk_line	old_prev;

	if (firstp(curline))
		return;
	exp = 1;
	lsave();
	old_prev = curline->l_prev->l_dline;
	curline->l_prev->l_dline = curline->l_dline;
	curline->l_dline = old_prev;
	getDOT();
	if (!lastp(curline))
		line_move(FORWARD, NO);
	modify();
}

Leave()
{
	longjmp(mainjmp, QUIT);
}

/* If argument is specified, kill that many lines down.  Otherwise,
   if we "appear" to be at the end of a line, i.e. everything to the
   right of the cursor is white space, we delete the line separator
   as if we were at the end of the line. */

KillEOL()
{
	Line	*line2;
	int	char2;

	if (exp_p) {
		if (exp == 0) {	/* Kill to beginning of line */
			line2 = curline;
			char2 = 0;
		} else {
			line2 = next_line(curline, exp);
			if ((LineDist(curline, line2) < exp) || (line2 == curline))
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

/* Kill to beginning of sentence */

KillBos()
{
	exp = -exp;
	KillEos();
}

/* Kill to end of sentence */

KillEos()
{
	Line	*line1;
	int	char1;

	line1 = curline;
	char1 = curchar;
	Eos();
	reg_kill(line1, char1, 1);
}

KillExpr()
{
	Line	*line1;
	int	char1;

	line1 = curline;
	char1 = curchar;
	FSexpr();
	reg_kill(line1, char1, 1);
}

EscPrefix()
{
	HandlePref(pref1map);
}

CtlxPrefix()
{
	HandlePref(pref2map);
}

MiscPrefix()
{
	HandlePref(miscmap);
}

HandlePref(map)
data_obj	**map;
{
	register data_obj	*cp;
	register int	c;

	c = waitchar();
	if (c == CTL(G)) {
		message("[Aborted]");
		rbell();
		return;
	}

	if (alarmed)
		message(key_strokes);

	cp = map[c];
	if (cp == 0) {
		s_mess("[%sunbound]", key_strokes);
		rbell();
	} else
		ExecCmd(cp);
}

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
	SetMark();
	SetDot(dot);
}

WtModBuf()
{
	if (!ModBufs(NO))
		message("[No buffers need saving]");
	else
		put_bufs(exp_p);
}

put_bufs(askp)
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
		if (SaveBuf(askp))
			unmodify();
	}
	SetBuf(oldb);
}

SaveBuf(askp)
{
	char	*yorn;

	if (askp) {
		yorn = ask((char *) 0, "Write %s? ", curbuf->b_fname);
		if (*yorn != 'Y' && *yorn != 'y')
			return 0;
	}
	file_write(curbuf->b_fname, 0);
	return 1;
}

DOTsave(buf)
Bufpos *buf;
{
	buf->p_line = curline;
	buf->p_char = curchar;
}

ToIndent()
{
	register char	*cp,
			c;

	for (cp = linebuf; c = *cp; cp++)
		if (c != ' ' && c != '\t')
			break;
	curchar = cp - linebuf;
}

GoLine()
{
	Line	*newline;

#ifndef ANSICODES
	if (exp_p == 0)
		return;
#else
	if (exp_p == 0 || exp <= 0) {
		if (SP)
			putpad(SP, 1);	/* Ask for cursor position */
		return;
	}
#endif
	newline = next_line(curbuf->b_first, exp - 1);
	PushPntp(newline);
	SetLine(newline);
}

#ifdef ANSICODES
MoveToCursor(line, col)
{
	register struct scrimage *sp = &PhysScreen[line];

	while (sp->s_id == NULL)
		sp = &PhysScreen[--line];
	if (sp->s_flags & MODELINE)
		complain((char *) 0);
	if (curwind != sp->s_window)
		SetWind(sp->s_window);
	SetLine(sp->s_lp);
	curchar = how_far(sp->s_lp, col);
}

AnsiCodes()
{
	int	c;
	int	num1 = 0;
	int	num2;
	static char *unsupported = "[Unsupported ANSI code received]";

	while (isdigit(c = getch()))
		num1 = (num1*10) + (c - '0');

	switch (c) {
	case ';':
		num2 = 0;
		while (isdigit(c = getch()))
			num2 = (num2*10) + (c - '0');
		switch (c) {
		case 'R':
			MoveToCursor(--num1, --num2);
			break;
		case 'H':
			Eow(); Bol();
			break;
		default:
			complain(unsupported);
		}
		break;
	case 'A':
		line_move(BACKWARD, NO);
		break;
	case 'B':
		line_move(FORWARD, NO);
		break;
	case 'C':
		ForChar();
		break;
	case 'D':
		BackChar();
		break;
	case 'H':
		Bow();
		break;
	case 'J':
		if (num1 == 2) {
			ClAndRedraw();
			break;
		}
		/* FALL THROUGH */
	default:
		complain(unsupported);
	}
}
#endif ANSICODES

NotModified()
{
	unmodify();
}

SetLMargin()
{
	LMargin = calc_pos(linebuf, curchar);
}

SetRMargin()
{
	RMargin = calc_pos(linebuf, curchar);
}
