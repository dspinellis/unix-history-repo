/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "ctype.h"
#include "termcap.h"
#include "disp.h"
#include <signal.h>

#ifdef MSDOS
#define SIGHUP 99
#endif

int	AbortCnt,
	tabstop = 8;
bool
	CanScroll = NO;

#ifdef	TERMCAP
private void
	(*TTins_line) proto((int, int, int)),
	(*TTdel_line) proto((int, int, int));
#endif	/* TERMCAP */

struct scrimage
	*DesiredScreen = NULL,
	*PhysScreen = NULL;

struct screenline	*Screen = NULL,	/* the screen (a bunch of screenline) */
			*Curline = NULL;	/* current line */

private struct screenline   *Savelines = NULL;	/* another bunch (LI of them) */


private char	*cursor;			/* offset into current Line */

char	*cursend;

int	CapCol,
	CapLine,

	i_line,
	i_col;

#ifdef	IBMPC
extern unsigned char	CHPL;
extern void		near normfun(),
			near scr_win(),
			near clr_page(),
			near clr_eoln();

#endif

void
make_scr()
{
	register int	i;
	register struct screenline	*ns;
	register char	*nsp;

	/* In case we are RESHAPING the window! */
	if (DesiredScreen != NULL)
		free((UnivPtr) DesiredScreen);
	if (PhysScreen != NULL)
		free((UnivPtr) PhysScreen);
	if (Savelines != NULL)
		free((UnivPtr) Savelines);
	if (Screen != NULL) {
		free((UnivPtr) Screen->s_line);	/* free all the screen data */
		free((UnivPtr) Screen);
	}

	DesiredScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));
	PhysScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));

	Savelines = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));
	ns = Screen = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));

	nsp = (char *) malloc((unsigned)CO * LI);

	if (DesiredScreen == NULL
	|| PhysScreen == NULL
	|| Savelines == NULL
	|| ns == NULL
	|| nsp == NULL)
	{
		writef("\n\rCannot malloc screen!\n");
		finish(SIGHUP);
	}

	for (i = 0; i < LI; i++) {
		ns->s_line = nsp;
		nsp += CO;
		ns->s_length = nsp - 1;		/* End of Line */
		ns += 1;
	}
	cl_scr(0);
}

void
clrline(cp1, cp2)
register char	*cp1,
		*cp2;
{
	while (cp1 <= cp2)
		*cp1++ = ' ';
}


/* Output one character (if necessary) at the current position */

#ifdef	MAC

/* Character output to bit-mapped screen is very expensive. It makes
   much more sense to write the entire line at once. So, we print all
   the characters, whether already there or not, once the line is
   complete.  */

private char sput_buf[256];
private int sput_len = 0;

private void
sput_start()
{
/*	if (i_line != CapLine || i_col != CapCol) */
		NPlacur(i_line, i_col);
	sput_len = 0;
}

private void
sput_end()
{
	sput_buf[0] = (unsigned char) sput_len;
	writechr(sput_buf);
	sput_len = 0;
}

private void
sputc(c)
register int c;
{
	if (sput_len < sizeof(sput_buf)) {
		*cursor++ = c;
		sput_buf[++sput_len] = (c == '0')? 0xAF /* slashed zero */ : c;
		CapCol++;
		i_col++;
	}
}

#else	/* !MAC */
#ifdef	IBMPC

private bool force = NO;

private void
sputc(c)
register int	c;
{
	if (force || (*cursor != c)) {
		if (i_line != CapLine || i_col != CapCol)
			Placur(i_line, i_col);
		*cursor++ = c;
		normfun((char) c);
		AbortCnt -= 1;
		CapCol += 1;
	} else {
		cursor += 1;
	}
	i_col += 1;
}

#else	/* !IBMPC */

#  define sputc(c)	{ \
	if (*cursor != (char) (c)) { \
		do_sputc(c); \
	} else { \
		cursor++; \
		i_col++; \
	} \
}

private void
do_sputc(c)
register int	c;
{
	if (*cursor != c) {
# ifdef	ID_CHAR
		if (IN_INSmode)
			INSmode(OFF);
# endif
		if (i_line != CapLine || i_col != CapCol)
			Placur(i_line, i_col);
		if (UL && (c & CHARMASK) == '_' && (*cursor & CHARMASK) != ' ')
			putstr(" \b");		/* Erase so '_' looks right. */
		*cursor++ = c;
		jputchar(c & CHARMASK);
		AbortCnt -= 1;
		CapCol += 1;
	} else {
		cursor += 1;
	}
	i_col += 1;
}

#endif	/* !IBMPC */
#endif	/* !MAC */

void
cl_eol()
{
	if (cursor > cursend)
		return;

	if (cursor < Curline->s_length) {
#ifdef	TERMCAP
		if (CE) {
			Placur(i_line, i_col);
			putpad(CE, 1);
			clrline(cursor, Curline->s_length);
		} else {
			/* Ugh.  The slow way for dumb terminals. */
			register char *savecp = cursor;

			while (cursor <= Curline->s_length)
				sputc(' ');
			cursor = savecp;
		}
#else	/* !TERMCAP */
		Placur(i_line, i_col);
		clr_eoln();
		clrline(cursor, Curline->s_length);
#endif	/* !TERMCAP */
		Curline->s_length = cursor;
	}
}

void
cl_scr(doit)
bool doit;
{
	register int	i;
	register struct screenline	*sp = Screen;

	for (i = 0; i < LI; i++, sp++) {
		clrline(sp->s_line, sp->s_length);
		sp->s_length = sp->s_line;
		PhysScreen[i].s_id = 0;
	}
	if (doit) {
#ifdef	TERMCAP
		putpad(CL, LI);
#else	/* !TERMCAP */
		clr_page();
#endif	/* !TERMCAP */
		CapCol = CapLine = 0;
		UpdMesg = YES;
	}
}

/* Write `line' at the current position of `cursor'.  Stop when we
   reach the end of the screen.  Aborts if there is a character
   waiting.  */


bool
swrite(line, inversep, abortable)
register char	*line;
bool	inversep;
bool	abortable;
{
	register int	n = cursend - cursor;
	bool	aborted = NO;

	if (n > 0) {

		register int	c;
		int	col = i_col;
#ifdef	MAC
#		define	spit(c)	sputc(c)
#else	/* !MAC */
#ifdef	IBMPC
#		define	spit(c)	sputc(c)
#else	/* !IBMPC */
		int	or_byte = inversep ? 0200 : 0;
#		define	spit(c)	{ int temp = (c) | or_byte; sputc(temp); }
#endif	/* !IBMPC */
#endif	/* !MAC */

#ifdef	MAC
		sput_start();	/* Okay, because no interruption possible */
#endif	/* MAC */
#ifdef	IBMPC
		force = inversep;  /* to force a redraw of the modeline */
#endif
		while ((c = *line++) != '\0') {
#			define  spot(c) { if (--n <= 0) break; spit(c); col += 1; }

			if (abortable && AbortCnt < 0) {
				AbortCnt = BufSize;
				if ((InputPending = charp()) != NO) {
					aborted = YES;
					break;
				}
			}
			if (c == '\t') {
				int	nchars;

				nchars = (tabstop - (col % tabstop));
				while (--nchars > 0)
					spot(' ');
				c = ' ';
			} else if (jiscntrl(c)) {
				spot('^');
				c = (c == '\177') ? '?' : c + '@';
#ifdef	TERMCAP
			} else if (Hazeltine && c == '~') {
				c = '`';
#endif
#ifdef	IBMPC
			} else if (c == 255) {
				c = 1;
			} else if (c == ' ' && inversep) {
				c = 255;
#endif	/* IBMPC */
			}
			spot(c);
#			undef	spot
		}
		if (n <= 0)
			spit(((*line=='\0') && (c!='\t') && !jiscntrl(c))? c : '!');
		if (cursor > Curline->s_length)
			Curline->s_length = cursor;
#ifdef	MAC
		sput_end();
#endif	/* MAC */
#ifdef	IBMPC
		force = NO;
#endif
#		undef	spit
	}
	return !aborted;
}

/* This is for writing a buffer line to the screen.  This is to
   minimize the amount of copying from one buffer to another buffer.
   This gets the info directly from the disk buffers. */


bool
BufSwrite(linenum)
int linenum;
{
	register int	n = cursend - cursor,
			col = 0,
			c = -1;
	register char	*bp;
	int	StartCol = DesiredScreen[linenum].s_offset,
		visspace = DesiredScreen[linenum].s_window->w_flags & W_VISSPACE;
	bool	aborted = NO;

	bp = lcontents(DesiredScreen[linenum].s_lp);
	if (*bp) {
		for (;;) {
			if (col >= StartCol) {
				DesiredScreen[linenum].s_offset = col;
				break;
			}

			c = *bp++ & CHARMASK;
			if (c == '\0')
				break;
			if (c == '\t')
				col += (tabstop - (col % tabstop));
			else if (jiscntrl(c))
				col += 2;
			else
				col += 1;
		}
	}
#ifdef	MAC
	sput_start();	/* Okay because we can't be interrupted */
#endif
	if (c != '\0') {
		while ((c = *bp++) != '\0') {
#			define spot(c)  { if (--n <= 0) break; sputc(c); col += 1; }

			if (AbortCnt < 0) {
				AbortCnt = BufSize;
				if ((InputPending = charp()) != NO) {
					aborted = YES;
					break;
				}
			}
			if (c == '\t') {
				int	nchars = (tabstop - (col % tabstop));

				if (visspace) {
					spot('>');
					nchars -= 1;
				}
				while (--nchars > 0)
					spot(' ');
				c = ' ';
			} else if (jiscntrl(c)) {
				spot('^');
				c = (c == '\177') ? '?' : c + '@';
			} else if (c == ' ' && visspace) {
				c = '_';
#ifdef	TERMCAP
			} else if (Hazeltine && c == '~') {
				c = '`';
#endif
#ifdef	IBMPC
			} else if (c == 255) {
				   c = 1;
#endif	/* IBMPC */
			}
			spot(c);
#			undef	spot
		}
	}
	if (n <= 0)
		sputc(((*bp == '\0') && (c != '\t') && !jiscntrl(c))? c : '!');
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
#ifdef	MAC
	sput_end();
#endif
	return !aborted;		/* Didn't abort */
}

void
i_set(nline, ncol)
register int	nline,
		ncol;
{
	Curline = &Screen[nline];
	cursor = Curline->s_line + ncol;
	cursend = &Curline->s_line[CO - 1];
	i_line = nline;
	i_col = ncol;
}

#ifdef	TERMCAP
void
SO_on()
{
	/* If there are magic cookies, then WHERE the SO string is
	   printed decides where the SO actually starts on the screen.
	   So it's important to make sure the cursor is positioned there
	   anyway.  I think this is right. */
	if (SG != 0) {
		Placur(i_line, i_col);
		i_col += SG;
		CapCol += SG;
	}
	putpad(SO, 1);
}

void
SO_off()
{
	/* see comment in SO_on() */
	if (SG != 0) {
		Placur(i_line, i_col);
		i_col += SG;
		CapCol += SG;
	}
	putpad(SE, 1);
}
#endif	/* TERMCAP */

/* Insert `num' lines a top, but leave all the lines BELOW `bottom'
   alone (at least they won't look any different when we are done).
   This changes the screen array AND does the physical changes. */

void
v_ins_line(num, top, bottom)
int num,
    top,
    bottom;
{
	register int	i;

	/* Save the screen pointers. */

	for(i = 0; i < num && top + i <= bottom; i++)
		Savelines[i] = Screen[bottom - i];

	/* Num number of bottom lines will be lost.
	   Copy everything down num number of times. */

	for (i = bottom; i > top && i-num >= 0; i--)
		Screen[i] = Screen[i - num];

	/* Restore the saved ones, making them blank. */

	for (i = 0; i < num; i++) {
		Screen[top + i] = Savelines[i];
		clrline(Screen[top + i].s_line, Screen[top + i].s_length);
		Screen[top + i].s_length = Screen[top + i].s_line;
	}

#ifdef	IBMPC
	scr_win((int) -num, (unsigned char) top, 0, (unsigned char) bottom, CHPL-1);
#else	/* !IBMPC */
# ifdef	MAC
	i_lines(top, bottom, num);
# else	/* !MAC */
	(*TTins_line)(top, bottom, num);
# endif	/* !MAC */
#endif	/* !IBMPC */
}

/* Delete `num' lines starting at `top' leaving the lines below `bottom'
   alone.  This updates the internal image as well as the physical image.  */

void
v_del_line(num, top, bottom)
int num,
    top,
    bottom;
{
	register int	i,
			bot;

	bot = bottom;

	/* Save the lost lines. */

	for (i = 0; i < num && top + i <= bottom; i++)
		Savelines[i] = Screen[top + i];

	/* Copy everything up num number of lines. */

	for (i = top; num + i <= bottom; i++)
		Screen[i] = Screen[i + num];

	/* Restore the lost ones, clearing them. */

	for (i = 0; i < num; i++) {
		Screen[bottom - i] = Savelines[i];
		clrline(Screen[bot].s_line, Screen[bot].s_length);
		Screen[bot].s_length = Screen[bot].s_line;
		bot -= 1;
	}

#ifdef	IBMPC
	scr_win(num, (unsigned char) top, 0, (unsigned char) bottom, CHPL-1);
#else	/* !IBMPC */
# ifdef	MAC
	d_lines(top, bottom, num);
# else	/* !MAC */
	(*TTdel_line)(top, bottom, num);
# endif	/* !MAC */
#endif	/* !IBMPC */
}

#ifdef	TERMCAP	/* remainder of this file */

/* The cursor optimization happens here.  You may decide that this
   is going too far with cursor optimization, or perhaps it should
   limit the amount of checking to when the output speed is slow.
   What ever turns you on ...   */

struct cursaddr {
	int	cm_numchars;
	void	(*cm_proc) ();
};

private char	*Cmstr;
private struct cursaddr	*HorMin,
			*VertMin,
			*DirectMin;

private void
	GENi_lines proto((int, int, int)),
	GENd_lines proto((int, int, int)),
	ForMotion proto((int)),
	ForTab proto((int)),
	BackMotion proto((int)),
	RetTab proto((int)),
	DownMotion proto((int)),
	UpMotion proto((int)),
	GoDirect proto((int, int)),
	HomeGo proto((int, int)),
	BottomUp proto((int, int));


private struct cursaddr	WarpHor[] = {
	0,	ForMotion,
	0,	ForTab,
	0,	BackMotion,
	0,	RetTab
};

private struct cursaddr	WarpVert[] = {
	0,	DownMotion,
	0,	UpMotion
};

private struct cursaddr	WarpDirect[] = {
	0,	GoDirect,
	0,	HomeGo,
	0,	BottomUp
};

#undef	FORWARD
#define	FORWARD		0	/* Move forward */
#define FORTAB		1	/* Forward using tabs */
#undef	BACKWARD
#define	BACKWARD	2	/* Move backward */
#define RETFORWARD	3	/* Beginning of line and then tabs */
#define NUMHOR		4

#define DOWN		0	/* Move down */
#define UPMOVE		1	/* Move up */
#define NUMVERT		2

#define DIRECT		0	/* Using CM */
#define HOME		1	/* HOME	*/
#define LOWER		2	/* Lower Line */
#define NUMDIRECT	3

#define	home()		Placur(0, 0)
#define LowLine()	{ putpad(LL, 1); CapLine = ILI; CapCol = 0; }
#define PrintHo()	{ putpad(HO, 1); CapLine = CapCol = 0; }

int	phystab = 8;

private void
GoDirect(line, col)
register int	line,
		col;
{
	putpad(Cmstr, 1);
	CapLine = line;
	CapCol = col;
}

private void
RetTab(col)
register int	col;
{
	jputchar('\r');
	CapCol = 0;
	ForTab(col);
}

private void
HomeGo(line, col)
int line,
    col;
{
	PrintHo();
	DownMotion(line);
	ForTab(col);
}

private void
BottomUp(line, col)
register int	line,
		col;
{
	LowLine();
	UpMotion(line);
	ForTab(col);
}

/* Tries to move forward using tabs (if possible).  It tabs to the
   closest tabstop which means it may go past 'destcol' and backspace
   to it. */

private void
ForTab(destcol)
int	destcol;
{
	register int	tabgoal,
			ntabs,
			tabstp = phystab;

	if (TABS && (tabstp > 0)) {
		tabgoal = destcol + (tabstp / 2);
		tabgoal -= (tabgoal % tabstp);

		/* Don't tab to last place or else it is likely to screw up. */
		if (tabgoal >= CO)
			tabgoal -= tabstp;

		ntabs = (tabgoal / tabstp) - (CapCol / tabstp);
		while (--ntabs >= 0)
			jputchar('\t');
		CapCol = tabgoal;
	}
	if (CapCol > destcol)
		BackMotion(destcol);
	else if (CapCol < destcol)
		ForMotion(destcol);
}

private void
ForMotion(destcol)
register int	destcol;
{
	register int	nchars = destcol - CapCol;
	register char	*cp = &Screen[CapLine].s_line[CapCol];

	while (--nchars >= 0)
		jputchar(*cp++ & CHARMASK);
	CapCol = destcol;
}

private void
BackMotion(destcol)
register int	destcol;
{
	register int	nchars = CapCol - destcol;

	if (BC)
		while (--nchars >= 0)
			putpad(BC, 1);
	else
		while (--nchars >= 0)
			jputchar('\b');
	CapCol = destcol;
}

private void
DownMotion(destline)
register int	destline;
{
	register int	nlines = destline - CapLine;

	while (--nlines >= 0)
		putpad(DO, 1);
	CapLine = destline;
}

private void
UpMotion(destline)
register int	destline;
{
	register int	nchars = CapLine - destline;

	while (--nchars >= 0)
		putpad(UP, 1);
	CapLine = destline;
}

#ifdef	ID_CHAR
static int	EIlen;
#endif

void
InitCM()
{
	HOlen = HO ? strlen(HO) : 1000;
	LLlen = LL ? strlen(LL) : 1000;
	UPlen = UP ? strlen(UP) : 1000;
#ifdef	ID_CHAR
	if (EI)
		EIlen = strlen(EI);
#endif
}

private int ForNum proto((int from, int to));

void
Placur(line, col)
int line,
    col;
{
	int	dline,		/* Number of lines to move */
		dcol;		/* Number of columns to move */
	register int	best,
			i;
	register struct cursaddr	*cp;
	int	xtracost = 0;	/* Misc addition to cost. */

#define CursMin(which,addrs,max)	{ \
	for (best = 0, cp = &(addrs)[1], i = 1; i < (max); i++, cp++) \
		if (cp->cm_numchars < (addrs)[best].cm_numchars) \
			best = i; \
	(which) = &(addrs)[best]; \
}

	if (line == CapLine && col == CapCol)
		return;		/* We are already there. */

	dline = line - CapLine;
	dcol = col - CapCol;
#ifdef	ID_CHAR
	if (IN_INSmode && MI)
		xtracost = EIlen + IMlen;
	/* If we're already in insert mode, it is likely that we will
	   want to be in insert mode again, after the insert. */
#endif

	/* Number of characters to move horizontally for each case.
	   1: Just move forward by typing the right character on the screen.
	   2: Print the correct number of back spaces.
	   3: Try tabbing to the correct place.
	   4: Try going to the beginning of the line, and then tab. */

	if (dcol == 1 || dcol == 0) {		/* Most common case. */
		HorMin = &WarpHor[FORWARD];
		HorMin->cm_numchars = dcol + xtracost;
	} else {
		WarpHor[FORWARD].cm_numchars = dcol >= 0 ? dcol + xtracost : 1000;
		WarpHor[BACKWARD].cm_numchars = dcol < 0 ? -(dcol + xtracost) : 1000;
		WarpHor[FORTAB].cm_numchars = dcol >= 0 && TABS ?
				ForNum(CapCol, col) + xtracost : 1000;
		WarpHor[RETFORWARD].cm_numchars = (xtracost + 1 + (TABS ? ForNum(0, col) : col));

		/* Which is the shortest of the bunch */

		CursMin(HorMin, WarpHor, NUMHOR);
	}

	/* Moving vertically is more simple. */

	WarpVert[DOWN].cm_numchars = dline >= 0 ? dline : 1000;
	WarpVert[UPMOVE].cm_numchars = dline < 0 ? ((-dline) * UPlen) : 1000;

	/* Which of these is simpler */
	CursMin(VertMin, WarpVert, NUMVERT);

	/* Homing first and lowering first are considered
	   direct motions.
	   Homing first's total is the sum of the cost of homing
	   and the sum of tabbing (if possible) to the right. */

	if (VertMin->cm_numchars + HorMin->cm_numchars <= 3) {
		DirectMin = &WarpDirect[DIRECT];	/* A dummy ... */
		DirectMin->cm_numchars = 100;
	} else {
		WarpDirect[DIRECT].cm_numchars = CM ?
				strlen(Cmstr = tgoto(CM, col, line)) : 1000;
		WarpDirect[HOME].cm_numchars = HOlen + line +
				WarpHor[RETFORWARD].cm_numchars;
		WarpDirect[LOWER].cm_numchars = LLlen + ((ILI - line) * UPlen) +
				WarpHor[RETFORWARD].cm_numchars;
		CursMin(DirectMin, WarpDirect, NUMDIRECT);
	}

	if (HorMin->cm_numchars + VertMin->cm_numchars < DirectMin->cm_numchars) {
		if (line != CapLine)
			(*(void (*)ptrproto((int)))VertMin->cm_proc)(line);
		if (col != CapCol) {
#ifdef	ID_CHAR
			if (IN_INSmode)	/* We may use real characters ... */
				INSmode(OFF);
#endif
			(*(void (*)ptrproto((int)))HorMin->cm_proc)(col);
		}
	} else {
#ifdef	ID_CHAR
		if (IN_INSmode && !MI)
			INSmode(OFF);
#endif
		(*(void (*)ptrproto((int, int)))DirectMin->cm_proc)(line, col);
	}
}

#define abs(x)	((x) >= 0 ? (x) : -(x))

private int
ForNum(from, to)
register int	from;
int to;
{
	register int	tabgoal,
			tabstp = phystab;
	int		numchars = 0;

	if (from >= to)
		return from - to;
	if (TABS && (tabstp > 0)) {
		tabgoal = to + (tabstp / 2);
		tabgoal -= (tabgoal % tabstp);
		if (tabgoal >= CO)
			tabgoal -= tabstp;
		numchars = (tabgoal / tabstop) - (from / tabstp);
		from = tabgoal;
	}
	return numchars + abs(from - to);
}

#ifdef	WIRED_TERMS

private void
BGi_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	writef("\033[%d;%dr\033[%dL\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

private void
SUNi_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	Placur(bottom - num + 1, 0);
	writef("\033[%dM", num);
	Placur(top, 0);
	writef("\033[%dL", num);
}

private void
C100i_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	if (num <= 1) {
		GENi_lines(top, bottom, num);
		return;
	}
	writef("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(AL, ILI - CapLine);
	writef("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif	/* WIRED_TERMS */

private void
GENi_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	register int	i;

	if (CS) {
		putpad(tgoto(CS, bottom, top), 1);
		CapCol = CapLine = 0;
		Placur(top, 0);
		if (M_SR && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_SR, num);
			putpad(minibuf, bottom - top);
		} else {
			for (i = 0; i < num; i++)
				putpad(SR, bottom - top);
		}
		putpad(tgoto(CS, ILI, 0), 1);
		CapCol = CapLine = 0;
	} else {
		Placur(bottom - num + 1, 0);
		if (M_DL && (num > 1)) {
			putargpad(M_DL, num, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - CapLine);
		}
		Placur(top, 0);
		if (M_AL && (num > 1)) {
			putargpad(M_AL, num, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

#ifdef	WIRED_TERMS

private void
BGd_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	writef("\033[%d;%dr\033[%dM\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

private void
SUNd_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	Placur(top, 0);
	writef("\033[%dM", num);
	Placur(bottom + 1 - num, 0);
	writef("\033[%dL", num);
}

private void
C100d_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	if (num <= 1) {
		GENd_lines(top, bottom, num);
		return;
	}
	writef("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(DL, ILI - CapLine);
	writef("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif	/* WIRED_TERMS */

private void
GENd_lines(top, bottom, num)
int top,
    bottom,
    num;
{
	register int	i;

	if (CS) {
		putpad(tgoto(CS, bottom, top), 1);
		CapCol = CapLine = 0;
		Placur(bottom, 0);
		if (M_SF && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_SF, num);
			putpad(minibuf, bottom - top);
		} else {
			for (i = 0; i < num; i++)
				putpad(SF, bottom - top);
		}
		putpad(tgoto(CS, ILI, 0), 1);
		CapCol = CapLine = 0;
	} else {
		Placur(top, 0);
		if (M_DL && (num > 1)) {
			putargpad(M_DL, num, ILI - top);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - top);
		}
		Placur(bottom + 1 - num, 0);
		if (M_AL && (num > 1)) {
			putargpad(M_AL, num, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

private const struct ID_lookup {
	char	*ID_name;
	void	(*I_proc) proto((int, int, int));	/* proc to insert lines */
	void	(*D_proc) proto((int, int, int));	/* proc to delete lines */
} ID_trms[] = {
	"generic",	GENi_lines,	GENd_lines,	/* This should stay here */
#ifdef	WIRED_TERMS
	"sun",		SUNi_lines,	SUNd_lines,
	"bg",		BGi_lines,	BGd_lines,
	"c1",		C100i_lines,	C100d_lines,
#endif	/* WIRED_TERMS */
	NULL,		0,		0
};

void
IDline_setup(tname)
char	*tname;
{
	register const struct ID_lookup	*idp;

	for (idp = &ID_trms[1]; idp->ID_name; idp++)
		if (strncmp(idp->ID_name, tname, strlen(idp->ID_name)) == 0)
			break;
	if (idp->ID_name == NULL)
		idp = &ID_trms[0];
	TTins_line = idp->I_proc;
	TTdel_line = idp->D_proc;
}

#endif	/* TERMCAP */
