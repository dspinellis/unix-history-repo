/*
 *	$Source: /u1/X/xterm/RCS/Tekproc.c,v $
 *	$Header: Tekproc.c,v 10.104 86/12/02 11:35:38 swick Exp $
 */

#include <X/mit-copyright.h>

/* Copyright (c) 1985 Massachusetts Institute of Technology		*/
/* Copyright (c) 1985	Digital Equipment Corporation			*/

/* Tekproc.c */

#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "Tekparse.h"
#include <stdio.h>
#include <sgtty.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/time.h>
#include <sys/file.h>
#include <pwd.h>
#include "data.h"
#include "error.h"
#ifdef MODEMENU
#include "menu.h"
#endif MODEMENU

/* Tek defines */

#define	BEL		07
#define	CANCEL		030
#define	DOTDASHEDLINE	2
#define	DOTTEDLINE	1
#define	EAST		01
#define	ETX		03
#define ICONFONT	4
#define	LARGEFONT	0
#define	LARGEFONTNAME	"9x15"
#define	LINEMASK	07
#define	LONGDASHEDLINE	4
#define	MARGIN1		0
#define	MARGIN2		1
#define MAX_PTS		150
#define MAX_VTX		300
#define	NAK		025
#define	NORTH		04
#define	PENDOWN		1
#define	PENUP		0
#define	SHORTDASHEDLINE	3
#define	SMALLFONT	3
#define	SMALLFONTNAME	"6x10"
#define	SOLIDLINE	0
#define	SOUTH		010
#define	TEKBOTTOMPAD	23
#define	TEKDEFHEIGHT	565
#define	TEKDEFWIDTH	750
#define	TEKHEIGHT	3072
#define	TEKHOME		((TekChar[screen->page.fontsize].nlines - 1)\
			 * TekChar[screen->page.fontsize].vsize)
#define	TEKMINHEIGHT	452
#define	TEKMINWIDTH	600
#define	TEKPAD		57
#define	TEKTOPPAD	34
#define	TEKWIDTH	4096
#define	TEXT_BUF_SIZE	256
#define	THREEFONT	2
#define	THREEFONTNAME	"8x13"
#define	TWOFONT		1
#define	TWOFONTNAME	"6x13"
#define	WEST		02

#define	TekMove(x,y)	screen->cur_X = x; screen->cur_Y = y
#define	input()		Tinput()
#define	unput(c)	*Tpushback++ = c

#ifndef lint
static char sccs_id[] = "@(#)Tekproc.c\tX10/6.6B\t12/26/86";
#endif lint

static Vertex *T_box[TEKNUMFONTS] = {
	T_boxlarge,
	T_box2,
	T_box3,
	T_boxsmall,
	T_boxicon,
};
static struct Tek_Char {
	int hsize;	/* in Tek units */
	int vsize;	/* in Tek units */
	int charsperline;
	int nlines;
} TekChar[TEKNUMFONTS] = {
	{56, 88, 74, 35},	/* large */
	{51, 82, 81, 38},	/* #2 */
	{34, 53, 121, 58},	/* #3 */
	{31, 48, 133, 64},	/* small */
	{56, 88, 74,  35},	/* icon is same as large */
};

static int *curstate;
static Cursor GINcursor;
static Vertex *line_pt;
static int nplot;
static TekLink Tek0;
static jmp_buf Tekjump;
static TekLink *TekRecord;
static Vertex *Tline;
static int *Tparsestate;

extern int Talptable[];
extern int Tbestable[];
extern int Tbyptable[];
extern int Tesctable[];
extern int Tipltable[];
extern int Tplttable[];
extern int Tpttable[];
extern int Tspttable[];

Tekparse()
{
	register Screen *screen = &term.screen;
	register int c, x, y;
	register char *cp;
	char ch;
	int arg;
	int Tinput();

	for( ; ; )
		switch(Tparsestate[c = input()]) {
		 case CASE_REPORT:
			/* report address */
			if(screen->TekGIN) {
				TekGINoff();
				TekEnqMouse(0);
			} else {
				c = 064;	/* has hard copy unit */
				if(screen->margin == MARGIN2)
					c |= 02;
				TekEnq(c, screen->cur_X, screen->cur_Y);
			}
			TekRecord->ptr[-1] = NAK; /* remove from recording */
			Tparsestate = curstate;
			break;

		 case CASE_VT_MODE:
			/* special return to vt102 mode */
			Tparsestate = curstate;
			TekRecord->ptr[-1] = NAK; /* remove from recording */
			if(screen->logging) {
				FlushLog(screen);
				screen->logstart = buffer;
			}
			return;

		 case CASE_SPT_STATE:
			/* Enter Special Point Plot mode */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate = Tspttable;
			break;

		 case CASE_GIN:
			/* Do Tek GIN mode */
			screen->TekGIN = &TekRecord->ptr[-1];
				/* Set cross-hair cursor raster array */
			if(GINcursor = make_tcross(screen->mousecolor,
			 screen->background, GXcopy))
				XDefineCursor(TWindow(screen), GINcursor);
			Tparsestate = Tbyptable;	/* Bypass mode */
			break;

		 case CASE_BEL:
			/* BEL */
			if(screen->TekGIN)
				TekGINoff();
			if(!TekRefresh)
				Bell();
			Tparsestate = curstate;	/* clear bypass condition */
			break;

		 case CASE_BS:
			/* BS */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate;	/* clear bypass condition */
			TCursorBack();
			break;

		 case CASE_PT_STATE:
			/* Enter Tek Point Plot mode */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate = Tpttable;
			break;

		 case CASE_PLT_STATE:
			/* Enter Tek Plot mode */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate = Tplttable;
			if((c = input()) == BEL)
				screen->pen = PENDOWN;
			else {
				unput(c);
				screen->pen = PENUP;
			}
			break;

		 case CASE_TAB:
			/* HT */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate;	/* clear bypass condition */
			TCursorForward();
			break;

		 case CASE_IPL_STATE:
			/* Enter Tek Incremental Plot mode */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate = Tipltable;
			break;

		 case CASE_ALP_STATE:
			/* Enter Tek Alpha mode from any other mode */
			if(screen->TekGIN)
				TekGINoff();
			/* if in one of graphics states, move alpha cursor */
			if(nplot > 0)	/* flush line Tbuffer */
				TekFlush();
			Tparsestate = curstate = Talptable;
			break;

		 case CASE_UP:
			/* cursor up */
			if(screen->TekGIN)
				TekGINoff();
			Tparsestate = curstate;	/* clear bypass condition */
			TCursorUp();
			break;

		 case CASE_COPY:
			/* make copy */
			if(screen->TekGIN)
				TekGINoff();
			TekCopy();
			TekRecord->ptr[-1] = NAK; /* remove from recording */
			Tparsestate = curstate;	/* clear bypass condition */
			break;

		 case CASE_PAGE:
			/* Page Function */
			if(screen->TekGIN)
				TekGINoff();
			TekPage();	/* clear bypass condition */
			break;

		 case CASE_BES_STATE:
			/* Byp: an escape char */
			Tparsestate = Tbestable;
			break;

		 case CASE_BYP_STATE:
			/* set bypass condition */
			Tparsestate = Tbyptable;
			break;

		 case CASE_IGNORE:
			/* Esc: totally ignore CR, ESC, LF, ~ */
			break;

		 case CASE_ASCII:
			/* Select ASCII char set */
			/* ignore for now */
			Tparsestate = curstate;
			break;

		 case CASE_APL:
			/* Select APL char set */
			/* ignore for now */
			Tparsestate = curstate;
			break;

		 case CASE_CHAR_SIZE:
			/* character size selector */
			screen->cur.fontsize = c & 03;
			Tparsestate = curstate;
			break;

		 case CASE_BEAM_VEC:
			/* beam and vector selector */
			/* only line types */
			if((c &= LINEMASK) != screen->cur.linetype) {
				if(nplot > 0)
					TekFlush();
				screen->cur.linetype = c;
			}
			Tparsestate = curstate;
			break;

		 case CASE_CURSTATE:
			Tparsestate = curstate;
			break;

		 case CASE_PENUP:
			/* Ipl: penup */
			screen->pen = PENUP;
			break;

		 case CASE_PENDOWN:
			/* Ipl: pendown */
			screen->pen = PENDOWN;
			break;

		 case CASE_IPL_POINT:
			/* Ipl: point */
			x = screen->cur_X;
			y = screen->cur_Y;
			if(c & NORTH)
				y++;
			else if(c & SOUTH)
				y--;
			if(c & EAST)
				x++;
			else if(c & WEST)
				x--;
			if(screen->pen == PENDOWN)
				TekDraw(x, y);
			else
				TekMove(x, y);
			break;

		 case CASE_PLT_VEC:
			/* Plt: vector */
			unput(c);
			if(getpoint()) {
				if(screen->pen == PENDOWN)
					TekDraw(screen->cur.x, screen->cur.y);
				else
					TekMove(screen->cur.x, screen->cur.y);
				screen->pen = PENDOWN;
			}
			break;

		 case CASE_PT_POINT:
			/* Pt: point */
			unput(c);
			if(getpoint()) {
				TekMove(screen->cur.x, screen->cur.y);
				TekDraw(screen->cur.x, screen->cur.y);
			}
			break;

		 case CASE_SPT_POINT:
			/* Spt: point */
			/* ignore intensity character in c */
			if(getpoint()) {
				TekMove(screen->cur.x, screen->cur.y);
				TekDraw(screen->cur.x, screen->cur.y);
			}
			break;

		 case CASE_CR:
			/* CR */
			if(screen->TekGIN)
				TekGINoff();
			if(nplot > 0)	/* flush line Tbuffer */
				TekFlush();
			screen->cur_X = screen->margin == MARGIN1 ? 0 :
			 TEKWIDTH / 2;
			Tparsestate = curstate = Talptable;
			break;

		 case CASE_ESC_STATE:
			/* ESC */
			Tparsestate = Tesctable;
			break;

		 case CASE_LF:
			/* LF */
			if(screen->TekGIN)
				TekGINoff();
			TCursorDown();
			if(!TekRefresh && (screen->display->qlen > 0 ||
			 (ioctl(screen->display->fd, FIONREAD, &arg), arg) > 0))
				xevents();
			break;

		 case CASE_SP:
			/* SP */
			TCursorForward();
			break;

		 case CASE_PRINT:
			/* printable character */
			ch = c;
			c = TActiveIcon(screen)
			    ? ICONFONT
			    : screen->cur.fontsize;

			XTextMask(TWindow(screen), (int)(screen->cur_X *
			 TekScale(screen)) + screen->border,
			 (int)((TEKHEIGHT + TEKTOPPAD - screen->cur_Y) *
			 TekScale(screen)) + screen->border + TTitlebar(screen)
			 - screen->tobaseline[c], &ch, 1, screen->Tfont[c],
			 screen->Tforeground);
			TCursorForward();
			break;
		 case CASE_OSC:
			/* do osc escape */
			do_osc(Tinput);
			Tparsestate = curstate;
			break;
		}
}

static int rcnt;
static char *rptr;
static int Tselect_mask;

Tinput()
{
	register Screen *screen = &term.screen;
	register char *cp;
	register int i;
	register TekLink *tek;
	extern char *malloc();

	if(Tpushback > Tpushb)
		return(*--Tpushback);
	if(TekRefresh) {
		if(rcnt-- > 0)
			return(*rptr++);
		if(tek = TekRefresh->next) {
			TekRefresh = tek;
			rcnt = tek->count - 1;
			rptr = tek->data;
			return(*rptr++);
		}
		TekRefresh = (TekLink *)0;
		longjmp(Tekjump, 1);
	}
again:
	if(Tbcnt-- <= 0) {
		if(nplot > 0)	/* flush line Tbuffer */
			TekFlush();
		Tselect_mask = pty_mask;	/* force a read */
		for( ; ; ) {
			if(Tselect_mask & pty_mask) {
				if(screen->logging)
					FlushLog(screen);
				if((Tbcnt = read(screen->respond,
				 Tbptr = Tbuffer, BUF_SIZE)) < 0) {
					if(errno == EIO && am_slave)
						exit(0);
					else if(errno != EWOULDBLOCK)
						Panic(
				 "Tinput:read returned unexpected error (%d)\n",
						 errno);
				} else if(Tbcnt == 0)
					Panic("input: read returned zero\n");
				else {
					if(screen->icon_show
					   && !screen->iconinput) {
						screen->iconinput = TRUE;
						IconBox(screen);
					}
					/* strip parity bit */
					for(i = Tbcnt, cp = Tbptr ; i > 0 ; i--)
						*cp++ &= CHAR;
					break;
				}
			}
			if (Ttoggled && curstate == Talptable) {
				TCursorToggle(TOGGLE);
				Ttoggled = FALSE;
			}
			if(QLength())
				Tselect_mask = X_mask;
			else {
				XFlush();
				Tselect_mask = Select_mask;
				if((i = select(max_plus1, &Tselect_mask, NULL,
				 NULL, screen->timeout)) < 0){
					if (errno != EINTR)
						SysError(ERROR_TSELECT);
					continue;
				} else if(i == 0) {
					if(GetButtonState(screen->sb) & HILITED)
						WindowScroll(screen,
						 ButtonRegion(screen->sb));
					screen->timeout->tv_usec = STEPTIME;
					continue;
				}
			}
			if(Tselect_mask & X_mask) {
				xevents();
				if(Tbcnt > 0)
					goto again;
			}
		}
		Tbcnt--;
		if (!Ttoggled && curstate == Talptable) {
			TCursorToggle(TOGGLE);
			Ttoggled = TRUE;
		}
	}
	if((tek = TekRecord)->count >= TEK_LINK_BLOCK_SIZE) {
		if((TekRecord = tek->next = (TekLink *)malloc(sizeof(TekLink)))
		 == (TekLink *)0)
			Panic("Tinput: malloc error (%d)\n", errno);
		tek = tek->next;
		tek->next = (TekLink *)0;
		tek->count = 0;
		tek->ptr = tek->data;
	}
	tek->count++;
	return(*tek->ptr++ = *Tbptr++);
}

TekExpose(rep)
register XExposeWindowEvent *rep;
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;
	register double d;

	if(rep && (screen->mappedTwin == &screen->fullTwin)) {
		if (rep->width != (TWidth(screen) + border) || rep->height !=
		 (THeight(screen) + border + TTitlebar(screen))) {
			XClear (TWindow(screen));
			TWidth(screen) = rep->width - border;
			THeight(screen) = rep->height - TTitlebar(screen)
			 - border;
			TekScale(screen) = (double)TWidth(screen) /
			 (TEKWIDTH + TEKPAD);
			if((d = (double)THeight(screen) / (TEKHEIGHT +
			 TEKTOPPAD + TEKBOTTOMPAD)) < TekScale(screen))
				TekScale(screen) = d;
			if(TTitlebar(screen) && TFullWidth(screen) !=
			 rep->width)
				TekTitleResize(rep->width);
			TFullWidth(screen) = rep->width;
			TFullHeight(screen) = rep->height;
			if (screen->active_icon)
			    TSetIconSize( screen );

		} else if(TFullHeight(screen) != rep->height)
			TFullHeight(screen) = rep->height;
		else if(!Ttoggled)
			TCursorToggle(CLEAR);
	}
	Ttoggled = TRUE;
	Tpushback = Tpushb;
	screen->cur_X = 0;
	screen->cur_Y = TEKHOME;
	screen->cur = screen->page;
	screen->margin = MARGIN1;
	if(screen->TekGIN) {
		screen->TekGIN = NULL;
		TekGINoff();
	}
	TekRefresh = &Tek0;
	rptr = TekRefresh->data;
	rcnt = TekRefresh->count;
	Tparsestate = curstate = Talptable;
	if(!screen->waitrefresh)
		dorefresh();
}

dorefresh()
{
	register Screen *screen = &term.screen;
	register Cursor cur;

	XDefineCursor(TWindow(screen), cur = make_wait(screen->mousecolor,
	 screen->background, GXcopy));
	XFlush();
	if(!setjmp(Tekjump))
		Tekparse();
	XDefineCursor(TWindow(screen), (screen->TekGIN && GINcursor) ?
	 GINcursor : screen->arrow);
	XFreeCursor(cur);
}

TekPage()
{
	register Screen *screen = &term.screen;
	register TekLink *tek, *tek2;

	XClear(TWindow(screen));
	screen->cur_X = 0;
	screen->cur_Y = TEKHOME;
	screen->margin = MARGIN1;
	screen->page = screen->cur;
	if(screen->TekGIN)
		TekGINoff();
	tek = TekRecord = &Tek0;
	tek->count = 0;
	tek->ptr = tek->data;
	if(tek = tek->next)
		do {
			tek2 = tek->next;
			free((char *)tek);
		} while(tek = tek2);
	TekRecord->next = (TekLink *)0;
	TekRefresh = (TekLink *)0;
	Ttoggled = TRUE;
	Tparsestate = curstate = Talptable;	/* Tek Alpha mode */
}

#define	EXTRABITS	017
#define	FIVEBITS	037
#define	HIBITS		(FIVEBITS << SHIFTHI)
#define	LOBITS		(FIVEBITS << SHIFTLO)
#define	SHIFTHI		7
#define	SHIFTLO		2
#define	TWOBITS		03

getpoint()
{
	register int c, x, y, e, lo_y = 0;
	register Screen *screen = &term.screen;

	x = screen->cur.x;
	y = screen->cur.y;
	for( ; ; ) {
		if((c = input()) < ' ') {	/* control character */
			unput(c);
			return(0);
		}
		if(c < '@') {	/* Hi X or Hi Y */
			if(lo_y) {	/* seen a Lo Y, so this must be Hi X */
				x &= ~HIBITS;
				x |= (c & FIVEBITS) << SHIFTHI;
				continue;
			}
			/* else Hi Y */
			y &= ~HIBITS;
			y |= (c & FIVEBITS) << SHIFTHI;
			continue;
		}
		if(c < '`') {	/* Lo X */
			x &= ~LOBITS;
			x |= (c & FIVEBITS) << SHIFTLO;
			screen->cur.x = x;
			screen->cur.y = y;
			return(1);	/* OK */
		}
		/* else Lo Y */
		if(lo_y) {	/* seen a Lo Y, so other must be extra bits */
			e = (y >> SHIFTLO) & EXTRABITS;
			x &= ~TWOBITS;
			x |= e & TWOBITS;
			y &= ~TWOBITS;
			y |= (e >> SHIFTLO) & TWOBITS;
		}
		y &= ~LOBITS;
		y |= (c & FIVEBITS) << SHIFTLO;
		lo_y++;
	}
}

TCursorBack()
{
	register Screen *screen = &term.screen;
	register struct Tek_Char *t;
	register int x, l;

	x = ( screen->cur_X -=
		( t = &TekChar[TActiveIcon(screen)
			       ? ICONFONT
			       : screen->cur.fontsize]
		)->hsize
	    );

	if(screen->margin == MARGIN1 && x < 0 || screen->margin == MARGIN2
	 && x < TEKWIDTH / 2) {
		if((l = (screen->cur_Y + (t->vsize - 1)) / t->vsize + 1) >=
		 t->nlines) {
			screen->margin = !screen->margin;
			l = 0;
		}
		screen->cur_Y = l * t->vsize;
		screen->cur_X = (t->charsperline - 1) * t->hsize;
	}
}

TCursorForward()
{
	register Screen *screen = &term.screen;
	register struct Tek_Char *t;
	register int l;

	if( ( screen->cur_X +=
		( t = &TekChar[TActiveIcon(screen)
			       ? ICONFONT
			       : screen->cur.fontsize]
		)->hsize
	    ) > TEKWIDTH
	  ) {
		if((l = screen->cur_Y / t->vsize - 1) < 0) {
			screen->margin = !screen->margin;
			l = t->nlines - 1;
		}
		screen->cur_Y = l * t->vsize;
		screen->cur_X = screen->margin == MARGIN1 ? 0 : TEKWIDTH / 2;
	}
}

TCursorUp()
{
	register Screen *screen = &term.screen;
	register struct Tek_Char *t;
	register int l;

	t = &TekChar[TActiveIcon(screen)
		     ? ICONFONT
		     : screen->cur.fontsize];

	if((l = (screen->cur_Y + (t->vsize - 1)) / t->vsize + 1) >= t->nlines) {
		l = 0;
		if((screen->margin = !screen->margin) != MARGIN1) {
			if(screen->cur_X < TEKWIDTH / 2)
				screen->cur_X += TEKWIDTH / 2;
		} else if(screen->cur_X >= TEKWIDTH / 2)
			screen->cur_X -= TEKWIDTH / 2;
	}
	screen->cur_Y = l * t->vsize;
}

TCursorDown()
{
	register Screen *screen = &term.screen;
	register struct Tek_Char *t;
	register int l;

	t = &TekChar[TActiveIcon(screen)
		     ? ICONFONT
		     : screen->cur.fontsize];

	if((l = screen->cur_Y / t->vsize - 1) < 0) {
		l = t->nlines - 1;
		if((screen->margin = !screen->margin) != MARGIN1) {
			if(screen->cur_X < TEKWIDTH / 2)
				screen->cur_X += TEKWIDTH / 2;
		} else if(screen->cur_X >= TEKWIDTH / 2)
			screen->cur_X -= TEKWIDTH / 2;
	}
	screen->cur_Y = l * t->vsize;
}

TekDraw (x, y)
int x, y;
{
	register Screen *screen = &term.screen;

	if(nplot == 0 || T_lastx != screen->cur_X || T_lasty != screen->cur_Y) {
		/*
		 * We flush on each unconnected line segment if the line
		 * type is not solid.  This solves a bug in X when drawing
		 * points while the line type is not solid.
		 */
		if(nplot > 0 && screen->cur.linetype != SOLIDLINE)
			TekFlush();
		AddToDraw(VertexDontDraw, screen->cur_X, screen->cur_Y);
	}
	T_lastx = screen->cur_X = x;
	T_lasty = screen->cur_Y = y;
	AddToDraw(VertexDrawLastPoint, x, y);
}

AddToDraw(type, x, y)
int type, x, y;
{
	register Screen *screen = &term.screen;
	register int last;
	register Vertex *lp;

	if(nplot >= MAX_PTS) {
		if(Tline[last = nplot - 1].flags == VertexDontDraw)
			nplot--;
		TekFlush();
		if(type != VertexDontDraw) {
			*line_pt = Tline[last];
			(line_pt++)->flags = VertexDontDraw;
			nplot++;
		}
	}
	lp = line_pt++;
	lp->flags = type;
	lp->x = x = x * TekScale(screen) + screen->border;
	lp->y = y = (TEKHEIGHT + TEKTOPPAD - y) * TekScale(screen) +
	 screen->border + TTitlebar(screen);
	nplot++;
}

TekFlush ()
{
	register Screen *screen = &term.screen;

	if(screen->cur.linetype == SOLIDLINE)
		XDraw (TWindow(screen), Tline, nplot, 1, 1, screen->Tforeground,
		 GXcopy, AllPlanes);
	else
		XDrawDashed (TWindow(screen), Tline, nplot, 1, 1,
		 screen->Tforeground, screen->linepat[screen->cur.linetype - 1],
		 GXcopy, AllPlanes);
	nplot = 0;
	line_pt = Tline;
}

TekGINoff()
{
	register Screen *screen = &term.screen;
	
	XDefineCursor(TWindow(screen), screen->arrow);
	if(GINcursor)
		XFreeCursor(GINcursor);
	if(screen->TekGIN) {
		*screen->TekGIN = CANCEL;	/* modify recording */
		screen->TekGIN = NULL;
	}
}

TekEnqMouse(c)
int c;
{
	register Screen *screen = &term.screen;
	int mousex, mousey;
	Window subw;

	XUpdateMouse(TWindow(screen), &mousex, &mousey, &subw);
	if((mousex = (mousex - screen->border) / TekScale(screen)) < 0)
		mousex = 0;
	else if(mousex >= TEKWIDTH)
		mousex = TEKWIDTH - 1;
	if((mousey = TEKHEIGHT + TEKTOPPAD - (mousey - screen->border -
	 TTitlebar(screen)) / TekScale(screen)) < 0)
		mousey = 0;
	else if(mousey >= TEKHEIGHT)
		mousey = TEKHEIGHT - 1;
	TekEnq(c, mousex, mousey);
}

TekEnq (status, x, y)
int status;
register int x, y;
{
	register Screen *screen = &term.screen;
	int pty = screen->respond;
	char cplot [5];

	/* Translate x and y to Tektronix code */
	cplot[1] = 040 | ((x >> SHIFTHI) & FIVEBITS);
	cplot[2] = 040 | ((x >> SHIFTLO) & FIVEBITS);
	cplot[3] = 040 | ((y >> SHIFTHI) & FIVEBITS);
	cplot[4] = 040 | ((y >> SHIFTLO) & FIVEBITS);
	if(cplot[0] = status)
		write (pty, cplot, 5);
	else
		write (pty, &cplot[1], 4);
}

TekRun()
{
	register Screen *screen = &term.screen;
	register int i;
	
	if(!TWindow(screen) && !TekInit()) {
		if(VWindow(screen)) {
			screen->TekEmu = FALSE;
			return;
		}
		Exit(ERROR_TINIT);
	}
	if(screen->icon_show) {
		if(screen->icon_show < 0) {
			screen->mappedTwin = &screen->iconTwin;
			screen->icon_show = TRUE;
			XMapWindow(TWindow(screen));
		}
	} else if(!screen->Tshow) {
		screen->mappedTwin = &screen->fullTwin;
		screen->Tshow = TRUE;
		XMapWindow(TWindow(screen));
	} else
		XRaiseWindow(TWindow(screen));
	if(screen->select)
		TekSelect();
	if (L_flag > 0) {
		XWarpMouse (TWindow(screen),
			    TFullWidth(screen) >> 1, TFullHeight(screen) >> 1);
		L_flag = -1;
	}
	Tpushback = Tpushb;
	Tbptr = Tbuffer;
	for(i = Tbcnt = bcnt ; i > 0 ; i--)
		*Tbptr++ = *bptr++;
	Tbptr = Tbuffer;
	Ttoggled = TRUE;
	if(!setjmp(Tekend))
		Tekparse();
	if(!Ttoggled) {
		TCursorToggle(TOGGLE);
		Ttoggled = TRUE;
	}
	screen->TekEmu = FALSE;
	TekUnselect();
}

static int Tpattern[TEKNUMLINES] = {
	XMakePattern(0x1, 4, 1),	/* dotted */
	XMakePattern(0xf1, 11, 1),	/* dot-dashed */
	XMakePattern(0xf, 8, 1),	/* short-dashed */
	XMakePattern(0x7f, 11, 1),	/* long-dashed */
};

TekInit()
{
	register Screen *screen = &term.screen;
	register int i, j;
	register TekLink *tek;
	register int width, height;
	register double d;
	register int border = 2 * screen->border;
	OpaqueFrame twindow;
	FontInfo finfo, ifinfo;
	char Tdefault[32];
	char iconname[128];
	WindowInfo wininfo;
	Color cdef;
	int pixels[2];
	static short Tfailed;
	int x, y;
	Window win;
	extern char *malloc();

	screen->mappedTwin = &screen->fullTwin;

	if(Tfailed)
		return(0);
	if (!(screen->Tfont[SMALLFONT] = XGetFont(SMALLFONTNAME))) {
		fprintf(stderr, "%s: Could not get font %s!\n",
			xterm_name, SMALLFONTNAME);
		goto fontfailed;
	}
	if((Tbuffer = (char *)malloc(BUF_SIZE)) == NULL ||
	 (Tpushb = (char *)malloc(10)) == NULL ||
	 (Tline = (Vertex *)malloc(MAX_VTX * sizeof(Vertex))) == NULL) {
		fprintf(stderr, "%s: Not enough core for Tek mode\n",
		 xterm_name);
		goto mallocfailed;
	}

	screen->xorplane = 1;
	screen->Tbackground = W_Pixel;
	screen->Tforeground = B_Pixel;
	screen->Tcursorcolor = B_Pixel;

	if (DisplayCells() > 2 && (fore_color || back_color ||
	 curs_color)) {
		if (curs_color && XParseColor(curs_color, &cdef)) {
			if(XGetColorCells(0, 2, 1, &screen->xorplane, pixels)) {
				screen->cellsused = TRUE;
				screen->colorcells[2] = cdef;
				screen->Tbackground = pixels[0];
				screen->Tforeground = pixels[1];
				screen->Tcursorcolor = screen->Tbackground |
				 screen->xorplane;
				screen->Tcolor |= C_CURSOR;
				screen->planeused = TRUE;
			}
		} else if (XGetColorCells(0, 1, 1, &screen->xorplane,
		 &screen->Tbackground)) {
			screen->Tforeground = screen->Tbackground |
			 screen->xorplane;
			screen->Tcursorcolor = screen->Tforeground;
			screen->planeused = TRUE;
		}
		if (screen->Tbackground != W_Pixel) {
			if (back_color == NULL ||
				!XParseColor(back_color, &cdef)) {
				cdef.pixel = W_Pixel;
				XQueryColor(&cdef);
				screen->Tcolor |= C_BACKGROUND;
			}
			cdef.pixel = screen->Tbackground;
			XStoreColor(&cdef);
			if(screen->cellsused) {
				screen->colorcells[0] = cdef;
				cdef.pixel = screen->Tforeground |
					screen->xorplane;
				XStoreColor(&cdef);
			}
			if (fore_color == NULL ||
				!XParseColor(fore_color, &cdef)) {
				cdef.pixel = B_Pixel;
				XQueryColor(&cdef);
				screen->Tcolor |= C_FOREGROUND;
			}
			cdef.pixel = screen->Tforeground;
			XStoreColor(&cdef);
			if(screen->cellsused) {
				screen->colorcells[1] = cdef;
				cdef.pixel = screen->Tcursorcolor;
				XStoreColor(&cdef);
			}
		}
	}
	if(term.flags & REVERSE_VIDEO) {
		screen->Tbgndtile = NULL;
		TekReverseVideo(screen);
	} else
		TekBackground(screen);

	twindow.bdrwidth = screen->borderwidth;
	if(grayborder)
		twindow.border = screen->graybordertile;
	else
		twindow.border = screen->bordertile;
	twindow.background = screen->Tbgndtile;

	sprintf(Tdefault, "=%dx%d+1+1", TEKDEFWIDTH + border, TEKDEFHEIGHT +
	 TTitlebar(screen) + border);
	if((TWindow(screen) = XCreate ("Tektronix Emulator", xterm_name,
	 T_geometry, Tdefault, &twindow, TEKMINWIDTH + border, TEKMINHEIGHT +
	 TTitlebar(screen) + border)) == NULL) {
		fprintf(stderr, "%s: Can't create Tek window\n", xterm_name);
		free((char *)Tline);
mallocfailed:
		if(Tpushb)
			free((char *)Tpushb);
		if(Tbuffer)
			free((char *)Tbuffer);
		XFreeFont(screen->Tfont[SMALLFONT]);
fontfailed:
		Tfailed = TRUE;
		return(FALSE);
	}
	XSelectInput(TWindow(screen), TWINDOWEVENTS);
	screen->Tbox = T_box;
	/*
	 * XCreate flushes all events, which might include an EnterWindow
	 * or LeaveWindow.  So if the cursor is not where it is supposed to
	 * be, we set select to the appropriate thing.
	 */
	if(VWindow(screen) && XQueryMouse(RootWindow, &x, &y, &win)) {
		if(screen->timer) {
			Timer(0L);
			screen->timer = 0;
		}
		if(win == VWindow(screen))
			screen->select |= INWINDOW;
		else
			screen->select &= ~INWINDOW;
	}

	TFullWidth(screen) = twindow.width;
	TFullHeight(screen) = twindow.height;
	TWidth(screen) = twindow.width - border;
	THeight(screen) = twindow.height - TTitlebar(screen) - border;
	TekScale(screen) = (double)TWidth(screen)
				    / (TEKWIDTH + TEKPAD);
	if((d = (double)THeight(screen) / (TEKHEIGHT + TEKTOPPAD +
	 TEKBOTTOMPAD)) < TekScale(screen))
		TekScale(screen) = d;


	XQueryFont(screen->Tfont[SMALLFONT], &finfo);
	screen->tobaseline[SMALLFONT] = finfo.height - finfo.baseline;

	if (!(screen->Tfont[THREEFONT] = XGetFont(THREEFONTNAME)))
		screen->Tfont[THREEFONT] = screen->Tfont[SMALLFONT];
	else
		XQueryFont(screen->Tfont[THREEFONT], &finfo);
	screen->tobaseline[THREEFONT] = finfo.height - finfo.baseline;

	if (!screen->fnt_icon)
	    screen->fnt_icon = XGetFont( f_i );

	screen->Tfont[ICONFONT] = screen->fnt_icon;
	XQueryFont( screen->fnt_icon, &ifinfo );
	Tfontsize[ICONFONT].Twidth = ifinfo.width;
	Tfontsize[ICONFONT].Theight = ifinfo.height;
	screen->tobaseline[ICONFONT] = ifinfo.height - ifinfo.baseline;
	T_boxicon[1].x = ifinfo.width - 1;
	T_boxicon[2].y = ifinfo.height - 1;
	T_boxicon[3].x = -ifinfo.width + 1;
	T_boxicon[4].y = -ifinfo.height + 1;

	if (!(screen->Tfont[TWOFONT] = XGetFont(TWOFONTNAME)))
		screen->Tfont[TWOFONT] = screen->Tfont[THREEFONT];
	else
		XQueryFont(screen->Tfont[TWOFONT], &finfo);
	screen->tobaseline[TWOFONT] = finfo.height - finfo.baseline;

	if (!(screen->Tfont[LARGEFONT] = XGetFont(LARGEFONTNAME)))
		screen->Tfont[LARGEFONT] = screen->Tfont[TWOFONT];
	else
		XQueryFont(screen->Tfont[LARGEFONT], &finfo);
	screen->tobaseline[LARGEFONT] = finfo.height - finfo.baseline;

	for(i = 0 ; i < TEKNUMLINES ; i++)
		screen->linepat[i] = Tpattern[i];
	screen->margin = MARGIN1;		/* Margin 1		*/
	screen->cur.fontsize = LARGEFONT;	/* set large font	*/
	screen->TekGIN = FALSE;			/* GIN off		*/

	if(screen->iconVwin.window) {
		XQueryWindow(screen->iconVwin.window, &wininfo);
		x = wininfo.x;
		y = wininfo.y;
	} else {
		x = twindow.x + (twindow.width - screen->iconTwin.width) / 2;
		y = twindow.y + (twindow.height - screen->iconTwin.height) / 2;
		IconGeometry(screen, &x, &y);
	}
	screen->iconTwin.window =
		XCreateWindow( RootWindow, x, y, 1, 1, screen->borderwidth,
			       screen->bordertile, screen->bgndtile );

	TSetIconSize( screen );
	XSetIconWindow( screen->fullTwin.window, screen->iconTwin.window );

	XDefineCursor( screen->iconTwin.window, screen->arrow );
	XSelectInput( screen->iconTwin.window,
		      screen->active_icon && (term.flags & ICONINPUT)
			? TICONWINDOWEVENTS | ICONINPUTEVENTS
			: TICONWINDOWEVENTS );

	XDefineCursor( TWindow(screen), screen->curs );
	if((screen->Twinname = malloc(screen->winnamelen + 6)) == NULL)
		Error(ERROR_TWINNAME);
	strcpy(screen->Twinname, screen->winname);
	strcat(screen->Twinname, " (Tek)");
	screen->Twinnamelen = strlen(screen->Twinname);
	XStoreName (TWindow(screen), screen->Twinname);
	strcpy(iconname, screen->winname);
	strcat(iconname, " (icon)");
	XStoreName (screen->iconTwin.window, iconname);
	XSetResizeHint (TWindow(screen), TEKMINWIDTH + border, TEKMINHEIGHT
	 + border + TTitlebar(screen), 1, 1);

	tek = TekRecord = &Tek0;
	tek->next = (TekLink *)0;
	tek->count = 0;
	tek->ptr = tek->data;
	Tpushback = Tpushb;
	screen->cur_X = 0;
	screen->cur_Y = TEKHOME;
	line_pt = Tline;
	Ttoggled = TRUE;
	if(TTitlebar(screen))
		TekTitleShow(TRUE);
	Tparsestate = curstate = Talptable;
	return(TRUE);
}

TekReverseVideo(screen)
register Screen *screen;
{
	register int flag, i;

	if(screen->Tbgndtile && ((screen->Tcolor & C_BACKGROUND) ||
	 screen->planeused))
		XFreePixmap(screen->Tbgndtile);
	i = screen->Tbackground;
	screen->Tbackground = screen->Tforeground;
	screen->Tforeground = i;
	screen->Tcolor = (screen->Tcolor & ~C_FBMASK) | switchfb[screen->Tcolor
	 & C_FBMASK];

	if(screen->cellsused) {
		flag = (term.flags & REVERSE_VIDEO) != 0;
		screen->Tcursorcolor = screen->Tbackground | screen->xorplane;
		i = screen->select ? 2 : !flag;
		screen->colorcells[i].pixel = screen->Tcursorcolor;
		XStoreColor(&screen->colorcells[i]);
		screen->colorcells[flag].pixel = screen->Tforeground |
		 screen->xorplane;
		XStoreColor(&screen->colorcells[flag]);
	} else
		screen->Tcursorcolor = screen->Tforeground;
	TekBackground(screen);
}

TekBackground(screen)
register Screen *screen;
{
	if((screen->Tcolor & C_BACKGROUND) || screen->planeused) {
		if(!(screen->Tbgndtile = XMakeTile(screen->Tbackground)))
			Error(ERROR_TBACK);
	} else
		screen->Tbgndtile = (screen->Tbackground == W_Pixel) ? W_Pixmap
		 : B_Pixmap;
	if(TWindow(screen))
		XChangeBackground(TWindow(screen), screen->Tbgndtile);
}

/*
 * Toggles cursor on or off at cursor position in screen.
 */
TCursorToggle(toggle)
int toggle;
{
	register Screen *screen = &term.screen;
	register int c, x, y;
	register T_fontsize *Tf;
	register int pixel, func, planes;

	if (screen->icon_show && !screen->active_icon) return;

	if(toggle) {
		pixel = screen->Tcursorcolor;
		func = GXinvert;
		planes = screen->xorplane;
	} else {
		pixel = screen->Tbackground;
		func = GXcopy;
		planes = AllPlanes;
	}

	c = TActiveIcon(screen)
	    ? ICONFONT
	    : screen->cur.fontsize;

	x = (screen->cur_X * TekScale(screen)) + screen->border;
	y = ((TEKHEIGHT + TEKTOPPAD - screen->cur_Y) * TekScale(screen)) +
	 screen->border - screen->tobaseline[c] + TTitlebar(screen);
	if(!toggle || screen->select) {
		Tf = &Tfontsize[c];
		XPixFill(TWindow(screen), x, y, Tf->Twidth, Tf->Theight, pixel,
		 (Bitmap)0, func, planes);
	} else {
		screen->Tbox[c]->x = x;
		screen->Tbox[c]->y = y;
		XDraw(TWindow(screen), screen->Tbox[c], NBOX, 1, 1, pixel,
		 func, planes);
	}
}

TekSelect()
{
	register Screen *screen = &term.screen;

	if(grayborder && screen->borderwidth > 0)
		XChangeBorder(TWindow(screen), screen->bordertile);
	if(TTitlebar(screen))
		TekTitleHilite();
}

TekUnselect()
{
	register Screen *screen = &term.screen;

	if(grayborder && screen->borderwidth > 0)
		XChangeBorder(TWindow(screen), screen->graybordertile);
	if(TTitlebar(screen))
		TekTitleUnhilite();
}

TekCopy()
{
	register TekLink *Tp;
	register int fd;
	register Screen *screen = &term.screen;
	register char *cp;
	register struct tm *tp;
	long l;
	char buf[32];

	/* for login windows, check that a user has logged in */
	if(L_flag && !checklogin()) {
		Bell();
		return;
	}
	time(&l);
	tp = localtime(&l);
	sprintf(buf, "COPY%02d-%02d-%02d.%02d:%02d:%02d", tp->tm_year,
	 tp->tm_mon, tp->tm_mday, tp->tm_hour, tp->tm_min, tp->tm_sec);
	if(access(buf, F_OK) >= 0) {	/* file exists */
		if(access(buf, W_OK) < 0) {
			Bell();
			return;
		}
	} else if(access(".", W_OK) < 0) {	/* can't write in directory */
		Bell();
		return;
	}
	if((fd = open(buf, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0) {
		Bell();
		return;
	}
	chown(buf, screen->uid, screen->gid);
	sprintf(buf, "\033%c\033%c", screen->page.fontsize + '8',
	 screen->page.linetype + '`');
	write(fd, buf, 4);
	Tp = &Tek0; 
	do
		write(fd, (char *)Tp->data, Tp->count);
	while(Tp = Tp->next);
	close(fd);
}

TekTitleShow(init)
int init;
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;

	if(!screen->Ttitle.tbar)
		TekTitleInit();
	if(!init) {
		XSetResizeHint (TWindow(screen), TEKMINWIDTH + border,
		 TEKMINHEIGHT + border + TTitlebar(screen), 1, 1);
		XChangeWindow (TWindow(screen), TWidth(screen) + border,
		 THeight(screen) + TTitlebar(screen) + border);
	}
	if(screen->select && screen->TekEmu)
		TekTitleHilite();
	else
		TekTitleUnhilite();
	XMapWindow(screen->Ttitle.tbar);
}

TekTitleHide()
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;

	XUnmapWindow(screen->Ttitle.tbar);
	XSetResizeHint (TWindow(screen), TEKMINWIDTH + border, TEKMINHEIGHT
	 + border, 1, 1);
	XChangeWindow (TWindow(screen), TWidth(screen) + border,
	 THeight(screen) + border);
}

TekTitleHilite()
{
	register Screen *screen = &term.screen;

	if(screen->Ttitle.hilited)
		return;
	XMapWindow(screen->Ttitle.left);
	XMapWindow(screen->Ttitle.right);
	screen->Ttitle.hilited = TRUE;
}

TekTitleUnhilite()
{
	register Screen *screen = &term.screen;

	if(!screen->Ttitle.hilited)
		return;
	XUnmapWindow(screen->Ttitle.left);
	XUnmapWindow(screen->Ttitle.right);
	screen->Ttitle.hilited = FALSE;
}

TekTitleResize(width)
register int width;
{
	register Screen *screen = &term.screen;
	register int i, j;

	if((screen->Ttitle.width = i = screen->Ttitle.fullwidth) >
	 (j = width - 2 * (MINHILITE + screen->title_n_size + 1)))
		screen->Ttitle.width = (i = j) + screen->title_n_size;
	j = width - i - 2 * (screen->title_n_size + 1);
	i = j / 2;
	j -= i;
	screen->Ttitle.x = i + 1 + screen->title_n_size;
	XChangeWindow(screen->Ttitle.tbar, width, screen->titleheight - 1);
	XChangeWindow(screen->Ttitle.left, i, screen->titlefont->height);
	XConfigureWindow(screen->Ttitle.right, width - j - 1, TITLEPAD, j,
	 screen->titlefont->height);
}

TekTitleExpose(rep)
register XExposeWindowEvent *rep;
{
	register Screen *screen = &term.screen;

	if(rep && (rep->x > (screen->Ttitle.x + screen->Ttitle.width) ||
	 (rep->x + rep->width) < screen->Ttitle.x ||
	 rep->y > (screen->Ttitle.y + screen->titlefont->height) ||
	 (rep->y + rep->height) < screen->Ttitle.y))
		return;
	XText(screen->Ttitle.tbar, screen->Ttitle.x, screen->Ttitle.y,
	 screen->Twinname, screen->Twinnamelen, screen->titlefont->id,
	 screen->foreground, screen->background);
}

TekTitleInit()
{
	register Screen *screen = &term.screen;
	register int w, i, j;
	OpaqueFrame hilite[2];
	extern Pixmap make_hilite();

	if((screen->Ttitle.tbar = XCreateWindow(TWindow(screen), -1, -1,
	 w = TFullWidth(screen), screen->titleheight - 1, 1, screen->bordertile,
	 screen->bgndtile)) == NULL)
		Error(ERROR_TCRTITLE);
	XSelectInput(screen->Ttitle.tbar, ButtonPressed | ButtonReleased |
	 ExposeWindow | EnterWindow | LeaveWindow | UnmapWindow);
	if(!screen->hilitetile && (screen->hilitetile =
	 make_hilite(screen->foreground, screen->background)) == NULL)
		Error(ERROR_THILITE);
	screen->Ttitle.fullwidth = XQueryWidth(screen->Twinname,
	 screen->titlefont->id);
	if((screen->Ttitle.width = i = screen->Ttitle.fullwidth) >
	 (j = w - 2 * (MINHILITE + screen->title_n_size + 1)))
		screen->Ttitle.width = (i = j) + screen->title_n_size;
	j = w - i - 2 * (screen->title_n_size + 1);
	i = j / 2;
	j -= i;
	screen->Ttitle.x = i + 1 + screen->title_n_size;
	screen->Ttitle.y = TITLEPAD;
	hilite[0].x = 1;
	hilite[1].x = w - j - 1;
	hilite[0].y = hilite[1].y = TITLEPAD;
	hilite[0].width = i;
	hilite[1].width = j;
	hilite[0].height = hilite[1].height = screen->titlefont->height;
	hilite[0].bdrwidth = hilite[1].bdrwidth = 0;
	hilite[0].border = hilite[1].border = NULL;
	hilite[0].background = hilite[1].background = screen->hilitetile;
	if(XCreateWindows(screen->Ttitle.tbar, hilite, 2) != 2)
		Error(ERROR_TCRLFRG);
	screen->Ttitle.left = hilite[0].self;
	screen->Ttitle.right = hilite[1].self;
}

#ifdef MODEMENU
/*
 * TMENU_LARGE through TMENU_SMALL must be first, as they must be the same
 * as the font size values LARGEFONT through SMALLFONT
 */
#define	TMENU_LARGE	0
#define	TMENU_NUM2	(TMENU_LARGE+1)
#define	TMENU_NUM3	(TMENU_NUM2+1)
#define	TMENU_SMALL	(TMENU_NUM3+1)
#define	TMENU_VTWIN	(TMENU_SMALL+1)
#define	TMENU_LINE	(TMENU_VTWIN+1)
#define	TMENU_PAGE	(TMENU_LINE+1)
#define	TMENU_RESET	(TMENU_PAGE+1)
#define	TMENU_COPY	(TMENU_RESET+1)
#define	TMENU_VTMODE	(TMENU_COPY+1)
#define	TMENU_HIDETEK	(TMENU_VTMODE+1)

static char *Ttext[] = {
	"Large Characters",
	"#2 Size Characters",
	"#3 Size Characters",
	"Small Characters",
	"VT Window Showing",
	"-",
	"PAGE",
	"RESET",
	"COPY",
	"Select VT Mode",
	"Hide Tek Window",
	0,
};

static Tmodes curmodes;
static int Tsize;
static int vshow;

Menu *Tsetupmenu(menu)
register Menu **menu;
{
	register Screen *screen = &term.screen;
	register char **cp;
	register int size = TActiveIcon(screen)
			    ? ICONFONT
			    : screen->cur.fontsize;
	register int kflags = term.keyboard.flags;

	curmodes = screen->cur;
	if (*menu == NULL) {
		if ((*menu = NewMenu("Tektronix", re_verse)) == NULL)
			return(NULL);
		for(cp = Ttext ; *cp ; cp++)
			AddMenuItem(*menu, *cp);
		CheckItem(*menu, size);
		if(vshow = screen->show)
			CheckItem(*menu, TMENU_VTWIN);
		else
			DisableItem(*menu, TMENU_HIDETEK);
		DisableItem(*menu, TMENU_LINE);
		Tsize = size;
		return(*menu);
	}
	if (Tsize != size) {
		UncheckItem(*menu, Tsize);
		CheckItem(*menu, Tsize = size);
	}
	if(vshow != screen->show) {
		SetItemCheck(*menu, TMENU_VTWIN, (vshow = screen->show));
		SetItemDisable(*menu, TMENU_HIDETEK, !vshow);
	}
	return(*menu);
}

static char *changesize[] = {
	"\0338",
	"\0339",
	"\033:",
	"\033;",
};

Tdomenufunc(item)
int item;
{
	register Screen *screen = &term.screen;
	register char *tp;
	register char *fp;
	Window win;
	int x, y;

	switch (item) {
	case TMENU_LARGE:
	case TMENU_NUM2:
	case TMENU_NUM3:
	case TMENU_SMALL:
		if(!Ttoggled) {
			TCursorToggle(TOGGLE);
			Ttoggled = TRUE;
		}
		if(Tbcnt < 0)
			Tbcnt = 0;
		for(fp = changesize[item], tp = &Tbptr[Tbcnt] ; *fp ; ) {
			*tp++ = *fp++;
			Tbcnt++;
		}
		break;

	case TMENU_RESET:
		bzero((char *)&curmodes, sizeof(Tmodes));
			/* drop through */
	case TMENU_PAGE:
		TekRefresh = (TekLink *)0;
		screen->cur = curmodes;
		TekPage();
		screen->cur_X = 0;
		screen->cur_Y = TEKHOME;
		break;

	case TMENU_COPY:
		TekCopy();
		break;

	case TMENU_HIDETEK:
		screen->Tshow = FALSE;
		XUnmapWindow(TWindow(screen));
		SyncUnmap(TWindow(screen), TWINDOWEVENTS);
		reselectwindow(screen);
		TekRefresh = (TekLink *)0;
			/* drop through */
	case TMENU_VTMODE:
		if(TekRefresh)
			dorefresh();
		if(screen->TekEmu) {
			if(screen->logging) {
				FlushLog(screen);
				screen->logstart = buffer;
			}
			longjmp(Tekend, 1);
		} else
			XRaiseWindow(VWindow(screen));
		break;

	case TMENU_VTWIN:
		if(screen->show = !screen->show) {
			if(VWindow(screen) || VTInit()) {
				XMapWindow(VWindow(screen));
				screen->show = TRUE;
			}
		} else {
			screen->show = FALSE;
			XUnmapWindow(VWindow(screen));
			SyncUnmap(VWindow(screen), WINDOWEVENTS);
			if(!screen->TekEmu) {
				if(TekRefresh)
					dorefresh();
				if(screen->logging) {
					FlushLog(screen);
					screen->logstart = Tbuffer;
				}
				screen->TekEmu = TRUE;
				longjmp(VTend, 1);
			}
		}
		reselectwindow(screen);
		break;
	}
	if(TekRefresh)
		dorefresh();
}
#endif MODEMENU


TSetIconSize( screen )
  Screen *screen;
{
	double d;

	if (screen->active_icon) {
	    screen->iconTwin.width = TWidth(screen)
				      * Tfontsize[ICONFONT].Twidth
				      / Tfontsize[THREEFONT].Twidth;
	    screen->iconTwin.height = THeight(screen)
				      * Tfontsize[ICONFONT].Theight
				      / Tfontsize[THREEFONT].Theight;
	    XChangeWindow( screen->iconTwin.window,
			   screen->iconTwin.width,
			   screen->iconTwin.height );
	} else
	    IconRecalc( screen );

	screen->iconTwin.fullwidth = screen->iconTwin.width;
	screen->iconTwin.fullheight = screen->iconTwin.height;
	screen->iconTwin.tekscale = (double) screen->iconTwin.width
				    / (TEKWIDTH + TEKPAD);

	if ( (d = (double) screen->iconTwin.height
	         / (TEKHEIGHT + TEKTOPPAD + TEKBOTTOMPAD)
	     ) < screen->iconTwin.tekscale )
	    screen->iconTwin.tekscale = d;
}
