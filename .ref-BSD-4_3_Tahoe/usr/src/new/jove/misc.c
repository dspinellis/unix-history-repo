/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"
#include <signal.h>
#ifdef ANSICODES
# include "termcap.h"
#endif

void
prCTIME()
{
	s_mess(": %f %s", get_time((time_t *) 0, (char *) 0, 0, -1));
}

void
ChrToOct()
{
	int	c,
		slow;

	c = waitchar(&slow);
	if (slow)
		message(key_strokes);
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
	del_char(BACKWARD, 1);
	f_char(1);
	insert_c(before, 1);
}

/* Switch current line with previous one */

void
TransLines()
{
	disk_line	old_prev;

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
EscPrefix()
{
	HandlePref(pref1map);
}

void
CtlxPrefix()
{
	HandlePref(pref2map);
}

void
MiscPrefix()
{
	HandlePref(miscmap);
}

void
HandlePref(map)
data_obj	**map;
{
	register data_obj	*cp;
	register int	c;
	int	slow;

	c = waitchar(&slow);
	if (c == AbortChar) {
		message("[Aborted]");
		rbell();
		return;
	}

	if (slow)
		message(key_strokes);

	cp = map[c];
	if (cp == 0) {
		s_mess("[%sunbound]", key_strokes);
		rbell();
	} else
		ExecCmd(cp);
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
		unmodify();
	}
	SetBuf(oldb);
}

void
ToIndent()
{
	register char	*cp,
			c;

	for (cp = linebuf; c = *cp; cp++)
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

#ifndef ANSICODES
 	if (!is_an_arg())
 		set_arg_value(ask_int("Line: ",10));
#else /* not ANSICODES */
 	if (!is_an_arg() || arg_value() <= 0) {
  		if (SP) {
  			putpad(SP, 1);	/* Ask for cursor position */
			return;
		}
 		set_arg_value(ask_int("Line: ", 10));
  	}
#endif /* ANSICODES */
 	newline = next_line(curbuf->b_first, arg_value() - 1);
  	PushPntp(newline);
  	SetLine(newline);
}

#ifdef ANSICODES
void
MoveToCursor(line, col)
{
	register struct scrimage *sp = &PhysScreen[line];

	while (sp->s_id == 0)
		sp = &PhysScreen[--line];
	if (sp->s_flags & MODELINE)
		complain((char *) 0);
	if (curwind != sp->s_window)
		SetWind(sp->s_window);
	SetLine(sp->s_lp);
	curchar = how_far(sp->s_lp, col);
}

void
AnsiCodes()
{
	int	c;
	int	num1 = 0;
	int	num2;
	static char *unsupported = "[Unsupported ANSI code received]";

	while (isdigit(c = getch()))
		num1 = (num1 * 10) + (c - '0');

	switch (c) {
	case ';':
		num2 = 0;
		while (isdigit(c = getch()))
			num2 = (num2 * 10) + (c - '0');
		switch (c) {
		case 'R':
			MoveToCursor(--num1, --num2);
			break;
		case 'H':
			Eow();
			Bol();
			break;
		default:
			complain(unsupported);
		}
		break;
	case 'A':
		PrevLine();
		break;
	case 'B':
		NextLine();
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
 	case 'M':		/* Enter */
 		PopMark();
 		break;
 	case 'P':		/* PF1 */
 		ExecCmd((data_obj *) FindCmd(IncFSearch));
 		break;
 	case 'Q':		/* PF2 */
 		ExecCmd((data_obj *) FindCmd(QRepSearch));
 		break;
 	case 'R':		/* PF3 */
 		WtModBuf();
 		Leave();
 		break;
 	case 'S':		/* PF4 */
 		KillEOL();
 		break;
 	case 'l':		/* , */
 		DelNChar();
 		break;
 	case 'm':		/* - */
 		DelNWord();
 		break;
 	case 'n':		/* . */
 		SetMark();
 		break;
 	case 'p':		/* 0 */
 		Bol();
 		NextLine();
 		break;
 	case 'q':		/* 1 */
 		ForWord();
 		break;
 	case 'r':		/* 2 */
 		ForChar();
 		Eol();
 		break;
 	case 's':		/* 3 */
 		Yank();
 		break;
 	case 't':		/* 4 */
 		ExecCmd((data_obj *) FindCmd(ForSearch));
 		break;
 	case 'u':		/* 5 */
 		ExecCmd((data_obj *) FindCmd(RevSearch));
 		break;
 	case 'v':		/* 6 */
 		DelReg();
 		break;
 	case 'w':		/* 7 */
 		PrevPage();
 		break;
 	case 'x':		/* 8 */
 		NextPage();
 		break;
 	case 'y':		/* 9 */
 		DelPWord();
 		break;
	case 'z':	/* Sun function keys send <esc>[Nz */
		switch(num1) {
			case 193:	/* L2 */
				SetMark();
				break;
			case 194:	/* L3 */
				PopMark();
				break;
			case 195:	/* L4 */
				DelReg();
				break;
			case 208:	/* R1 */
				ExecCmd((data_obj *) FindCmd(QRepSearch));
				break;
			case 209:	/* R2 */
				ExecCmd((data_obj *) FindCmd(IncFSearch));
				break;
			case 210:	/* R3 */
				WtModBuf();
				break;
			case 211:	/* R4 */
				ExecCmd((data_obj *) FindCmd(RepSearch));
				break;
			case 212:	/* R5 */
				ExecCmd((data_obj *) FindCmd(IncRSearch));
				break;
			case 213:	/* R6 */
				Leave();
				break;
			case 214:	/* R7 */
				BackWord();
				break;
			case 215:	/* R8 == UpArrow */
				break;
			case 216:	/* R9 */
				ForWord();
				break;
			case 217:	/* R10 == LeftArrow */
				break;
			case 218:	/* R11 */
				NextWindow();
				break;
			case 219:	/* R12 == RightArrow */
				break;
			case 220:	/* R13 */
			case 221:	/* R14 == DownArrow */
				break;
			case 222:	/* R15 */
			case 225:	/* F2 */
			case 226:	/* F3 */
			case 227:	/* F4 */
				break;
			case 228:	/* F5 */
				break;
			case 229:	/* F6 */
				break;
			case 230:	/* F7 */
				break;
			case 231:	/* F8 */
				break;
			case 232:	/* F9 */
				break;
			default:
				num1 = -1;	/* Hack flags failure */
				break;
		}
		if (num1 >= 0)
			break;
	default:
		complain(unsupported);
	}
}
#endif /* ANSICODES */

void
NotModified()
{
	unmodify();
}

void
SetLMargin()
{
	LMargin = calc_pos(linebuf, curchar);
}

void
SetRMargin()
{
	RMargin = calc_pos(linebuf, curchar);
}

