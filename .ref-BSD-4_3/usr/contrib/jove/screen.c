/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "io.h"
#include "ctype.h"
#include "temp.h"
#include "termcap.h"

extern int	BufSize;

int	OkayAbort,
	tabstop = 8;

int	(*TTins_line)(),
	(*TTdel_line)();

struct scrimage
	*DesiredScreen = 0,
	*PhysScreen = 0;

struct screenline	*Screen = 0,	/* the screen (a bunch of screenline) */
			*Savelines = 0,	/* another bunch (LI of them) */
			*Curline = 0;	/* current line */
char	*cursor,			/* offset into current Line */
	*cursend;

int	CapCol,
	CapLine,

	i_line,
	i_col;

make_scr()
{
	register int	i;
	register struct screenline	*ns;
	register char	*nsp;

#ifdef RESHAPING
	/* In case we are RESHAPING the window! */
	if (DesiredScreen)
		free((char *) DesiredScreen);
	if (PhysScreen)
		free((char *) PhysScreen);
	if (Savelines)
		free((char *) Savelines);
	if (Screen) {
		ns = Screen;
		for (i = 0; i < LI; i++)
			free((ns++)->s_line);
		free((char *) Screen);
	}
#endif RESHAPING

	DesiredScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));
	PhysScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));

	Savelines = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));
	ns = Screen = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));

	nsp = (char *) malloc((unsigned)CO * LI);
	if (nsp == 0) {
		printf("\n\rCannot malloc screen!\n");
		finish(1);
	}

	for (i = 0; i < LI; i++) {
		ns->s_line = nsp;
		nsp += CO;
		ns->s_length = nsp - 1;		/* End of Line */
		ns++;
	}
	cl_scr(0);
}

clrline(cp1, cp2)
register char	*cp1,
		*cp2;
{
	while (cp1 <= cp2)
		*cp1++ = ' ';
}

#define sputc(c)	((*cursor != (char) (c)) ? dosputc(c) : (cursor++, i_col++))
#define soutputc(c)	if (--n <= 0) break; else sputc(c)

cl_eol()
{
	if (cursor > cursend)
		return;

	if (cursor < Curline->s_length) {
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
		Curline->s_length = cursor;
	}
}

cl_scr(doit)
{
	register int	i;
	register struct screenline	*sp = Screen;

	for (i = 0; i < LI; i++, sp++) {
		clrline(sp->s_line, sp->s_length);
		sp->s_length = sp->s_line;
		PhysScreen[i].s_id = 0;
	}
	if (doit) {
		putpad(CL, LI);
		CapCol = CapLine = 0;
		UpdMesg++;
	}
}

#ifdef ID_CHAR
extern int	IN_INSmode;
#endif

/* Output one character (if necessary) at the current position */

dosputc(c)
register char	c;
{
	if (*cursor != c) {
#ifdef ID_CHAR
		if (IN_INSmode)
			INSmode(0);
#endif
		if (i_line != CapLine || i_col != CapCol)
			Placur(i_line, i_col);
		if (UL && (c & 0177) == '_' && (*cursor & 0177) != ' ')
			putstr(" \b");		/* Erase so '_' looks right. */
		*cursor++ = c;
		putchar(c & 0177);
		CapCol++;
		i_col++;
	} else {
		cursor++;
		i_col++;
	}
}

/* Write `line' at the current position of `cursor'.  Stop when we
   reach the end of the screen.  Aborts if there is a character
   waiting.  */

swrite(line, inversep, abortable)
register char	*line;
register int	abortable;
{
	register int	c;
	int	col = i_col,
		aborted = 0;
	register int	n = cursend - cursor;
	int	or_byte = inversep ? 0200 : 0,
		thebyte;

	if (n <= 0)
		return 1;

	OkayAbort = 0;
	while (c = *line++) {
		if (abortable && OkayAbort) {
			OkayAbort = NO;
			if (InputPending = charp()) {
				aborted = 1;
				break;
			}
		}
		if (c == '\t') {
			int	nchars;

			nchars = (tabstop - (col % tabstop));
			col += nchars;

			thebyte = (' ' | or_byte);
			while (nchars--)
				soutputc(thebyte);
			if (n <= 0)
				break;
		} else if (isctrl(c)) {
			thebyte = ('^' | or_byte);
			soutputc(thebyte);
			thebyte = (((c == '\177') ? '?' : c + '@') | or_byte);
			soutputc(thebyte);
			col += 2;
		} else {
			thebyte = (c | or_byte);
			soutputc(thebyte);
			col++;
		}
	}
	if (n <= 0) {
		thebyte = ('!' | or_byte);
		sputc(thebyte);
	}
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
	return !aborted;
}

/* This is for writing a buffer line to the screen.  This is to
   minimize the amount of copying from one buffer to another buffer.
   This gets the info directly from the disk buffers. */

BufSwrite(linenum)
{
	char	*bp;
	register int	n = cursend - cursor,
			col = 0,
			c;
	int	StartCol = DesiredScreen[linenum].s_offset,
		visspace = DesiredScreen[linenum].s_window->w_visspace,
		aborted = 0;

	bp = lcontents(DesiredScreen[linenum].s_lp);
	if (*bp) for (;;) {
		if (col >= StartCol) {
			DesiredScreen[linenum].s_offset = col;
			break;
		}

		c = *bp++ & 0177;
		if (c == '\t')
			col += (tabstop - (col % tabstop));
		else if (isctrl(c))
			col += 2;
		else
			col++;
	}

	OkayAbort = 0;
	while (c = (*bp++ & 0177)) {
		if (OkayAbort) {
			OkayAbort = NO;
			if (InputPending = charp()) {
				aborted = 1;
				break;
			}
		}
		if (c == '\t') {
			int	nchars = (tabstop - (col % tabstop));

			col += nchars;
			if (visspace) {
				soutputc('>');
				nchars--;
			}
			while (--nchars >= 0)
				soutputc(' ');
			if (n <= 0)
				break;
		} else if (isctrl(c)) {
			soutputc('^');
			soutputc((c == '\177') ? '?' : c + '@');
			col += 2;
		} else {
			if (visspace && c == ' ')
				c = '_';
			soutputc(c);
			col++;
		}
	}
	if (n <= 0)
		sputc('!');
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
	return !aborted;		/* Didn't abort */
}

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

/* Insert `num' lines a top, but leave all the lines BELOW `bottom'
   alone (at least they won't look any different when we are done).
   This changes the screen array AND does the physical changes. */

v_ins_line(num, top, bottom)
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

	(*TTins_line)(top, bottom, num);
}

/* Delete `num' lines starting at `top' leaving the lines below `bottom'
   alone.  This updates the internal image as well as the physical image.  */

v_del_line(num, top, bottom)
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
		bot--;
	}

	(*TTdel_line)(top, bottom, num);
}


/* The cursor optimization happens here.  You may decide that this
   is going too far with cursor optimization, or perhaps it should
   limit the amount of checking to when the output speed is slow.
   What ever turns you on ...   */

private struct cursaddr {
	int	c_numchars,
		(*c_proc)();
};

private char	*Cmstr;
private struct cursaddr	*HorMin,
			*VertMin,
			*DirectMin;

private ForMotion(),
	ForTab(),
	BackMotion(),
	RetTab(),
	DownMotion(),
	UpMotion(),
	GoDirect(),
	HomeGo(),
	BottomUp();
	

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
#define LowLine()	putpad(LL, 1), CapLine = ILI, CapCol = 0
#define PrintHo()	putpad(HO, 1), CapLine = CapCol = 0

int	phystab = 8;

private
GoDirect(line, col)
register int	line,
		col;
{
	putpad(Cmstr, 1);
	CapLine = line;
	CapCol = col;
}

private
RetTab(col)
register int	col;
{
	putchar('\r');
	CapCol = 0;
	ForTab(col);
}

private
HomeGo(line, col)
{
	PrintHo();
	DownMotion(line);
	ForTab(col);
}

private
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

private
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
			putchar('\t');
		CapCol = tabgoal;
	}
	if (CapCol > destcol)
		BackMotion(destcol);
	else if (CapCol < destcol)
		ForMotion(destcol);
}

private
ForMotion(destcol)
register int	destcol;
{
	register int	nchars = destcol - CapCol;
	register char	*cp = &Screen[CapLine].s_line[CapCol];

	while (--nchars >= 0)
		putchar(*cp++ & 0177);
	CapCol = destcol;
}

private
BackMotion(destcol)
register int	destcol;
{
	register int	nchars = CapCol - destcol;

	if (BC)
		while (--nchars >= 0)
			putpad(BC, 1);
	else
		while (--nchars >= 0)
			putchar('\b');
	CapCol = destcol;
}

private
DownMotion(destline)
register int	destline;
{
	register int	nlines = destline - CapLine;

	while (--nlines >= 0)
		putchar('\n');
	CapLine = destline;
}

private
UpMotion(destline)
register int	destline;
{
	register int	nchars = CapLine - destline;

	while (--nchars >= 0)
		putpad(UP, 1);
	CapLine = destline;
}

#ifdef ID_CHAR
static int	EIlen;
#endif
extern int	IMlen;

InitCM()
{
	HOlen = HO ? strlen(HO) : 1000;
	LLlen = LL ? strlen(LL) : 1000;
	UPlen = UP ? strlen(UP) : 1000;
#ifdef ID_CHAR
	if (EI)
		EIlen = strlen(EI);
#endif
}

Placur(line, col)
{
	int	dline,		/* Number of lines to move */
		dcol;		/* Number of columns to move */
	register int	best,
			i;
	register struct cursaddr	*cp;
	int	xtracost = 0;	/* Misc addition to cost. */

#define CursMin(which,addrs,max) \
	for (best = 0, cp = &addrs[1], i = 1; i < max; i++, cp++) \
		if (cp->c_numchars < addrs[best].c_numchars) \
			best = i; \
	which = &addrs[best];

	if (line == CapLine && col == CapCol)
		return;		/* We are already there. */

	dline = line - CapLine;
	dcol = col - CapCol;
#ifdef ID_CHAR
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
		HorMin->c_numchars = dcol + xtracost;
	} else {
		WarpHor[FORWARD].c_numchars = dcol >= 0 ? dcol + xtracost : 1000;
		WarpHor[BACKWARD].c_numchars = dcol < 0 ? -(dcol + xtracost) : 1000;
		WarpHor[FORTAB].c_numchars = dcol >= 0 && TABS ?
				ForNum(CapCol, col) + xtracost : 1000;
		WarpHor[RETFORWARD].c_numchars = (xtracost + 1 + (TABS ? ForNum(0, col) : col));

		/* Which is the shortest of the bunch */

		CursMin(HorMin, WarpHor, NUMHOR);
	}

	/* Moving vertically is more simple. */

	WarpVert[DOWN].c_numchars = dline >= 0 ? dline : 1000;
	WarpVert[UPMOVE].c_numchars = dline < 0 ? ((-dline) * UPlen) : 1000;

	/* Which of these is simpler */
	CursMin(VertMin, WarpVert, NUMVERT);

	/* Homing first and lowering first are considered 
	   direct motions.
	   Homing first's total is the sum of the cost of homing
	   and the sum of tabbing (if possible) to the right. */
	
	if (VertMin->c_numchars + HorMin->c_numchars <= 3) {
		DirectMin = &WarpDirect[DIRECT];	/* A dummy ... */
		DirectMin->c_numchars = 100;
	} else {
		WarpDirect[DIRECT].c_numchars = CM ?
				strlen(Cmstr = tgoto(CM, col, line)) : 1000;
		WarpDirect[HOME].c_numchars = HOlen + line +
				WarpHor[RETFORWARD].c_numchars;
		WarpDirect[LOWER].c_numchars = LLlen + ((ILI - line) * UPlen) +
				WarpHor[RETFORWARD].c_numchars;
		CursMin(DirectMin, WarpDirect, NUMDIRECT);
	}

	if (HorMin->c_numchars + VertMin->c_numchars < DirectMin->c_numchars) {
		if (line != CapLine)
			(*VertMin->c_proc)(line);
		if (col != CapCol) {
#ifdef ID_CHAR
			if (IN_INSmode)	/* We may use real characters ... */
				INSmode(0);
#endif
			(*HorMin->c_proc)(col);
		}
	} else {
#ifdef ID_CHAR
		if (IN_INSmode && !MI)
			INSmode(0);
#endif
		(*DirectMin->c_proc)(line, col);
	}
}

#define abs(x)	((x) >= 0 ? (x) : -(x))

ForNum(from, to)
register int	from;
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

#ifdef WIRED_TERMS

BGi_lines(top, bottom, num)
{
	printf("\033[%d;%dr\033[%dL\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

SUNi_lines(top, bottom, num)
{
	Placur(bottom - num + 1, 0);
	printf("\033[%dM", num);
	Placur(top, 0);
	printf("\033[%dL", num);
}

C100i_lines(top, bottom, num)
{
	if (num <= 1) {
		GENi_lines(top, bottom, num);
		return;
	}
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(AL, ILI - CapLine);
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif WIRED_TERMS

GENi_lines(top, bottom, num)
{
	register int	i;

	if (CS) {
		printf(tgoto(CS, bottom, top));
		CapCol = CapLine = 0;
		Placur(top, 0);
		for (i = 0; i < num; i++)
			putpad(SR, bottom - top);
		printf(tgoto(CS, ILI, 0));
		CapCol = CapLine = 0;
	} else {
		Placur(bottom - num + 1, 0);
		if (M_DL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_DL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - CapLine);
		}
		Placur(top, 0);
		if (M_AL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_AL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

#ifdef WIRED_TERMS

BGd_lines(top, bottom, num)
{
	printf("\033[%d;%dr\033[%dM\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

SUNd_lines(top, bottom, num)
{
	Placur(top, 0);
	printf("\033[%dM", num);
	Placur(bottom + 1 - num, 0);
	printf("\033[%dL", num);
}

C100d_lines(top, bottom, num)
{
	if (num <= 1) {
		GENd_lines(top, bottom, num);
		return;
	}
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(DL, ILI - CapLine);
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif WIRED_TERMS

GENd_lines(top, bottom, num)
{
	register int	i;

	if (CS) {
		printf(tgoto(CS, bottom, top));
		CapCol = CapLine = 0;
		Placur(bottom, 0);
		for (i = 0; i < num; i++)
			putpad(SF, bottom - top);
		printf(tgoto(CS, ILI, 0));
		CapCol = CapLine = 0;
	} else {
		Placur(top, 0);
		if (M_DL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_DL, num);
			putpad(minibuf, ILI - top);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - top);
		}
		Placur(bottom + 1 - num, 0);
		if (M_AL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_AL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

struct ID_lookup {
	char	*ID_name;
	int	(*I_proc)();	/* proc to insert lines */
	int	(*D_proc)();	/* proc to delete lines */
} ID_trms[] = {
	"generic",	GENi_lines,	GENd_lines,	/* This should stay here */
#ifdef WIRED_TERMS
	"sun",		SUNi_lines,	SUNd_lines,
	"bg",		BGi_lines,	BGd_lines,
	"c1",		C100i_lines,	C100d_lines,
#endif WIRED_TERMS
	0,		0,		0
};

IDline_setup(tname)
char	*tname;
{
	register struct ID_lookup	*idp;

	for (idp = &ID_trms[1]; idp->ID_name; idp++)
		if (strncmp(idp->ID_name, tname, strlen(idp->ID_name)) == 0)
			break;
	if (idp->ID_name == 0)
		idp = &ID_trms[0];
	TTins_line = idp->I_proc;
	TTdel_line = idp->D_proc;
}
