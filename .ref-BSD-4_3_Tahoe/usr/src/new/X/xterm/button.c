/*
 *	$Source: /u1/X/xterm/RCS/button.c,v $
 *	$Header: button.c,v 10.103 86/12/02 09:49:20 swick Exp $
 */

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/
/*
button.c	Handles button events in the terminal emulator.
		does cut/paste operations, change modes via menu,
		passes button events through to some applications.
				J. Gettys.
*/
#ifndef lint
static char csrg_id[] = "@(#)button.c	1.6\t(Berkeley/CSRG)\t9/18/87";
static char sccs_id[] = "@(#)button.c\tX10/6.6B\t12/26/86";
#endif	lint
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "data.h"
#include "error.h"
#ifdef MODEMENU
#include "menu.h"
#endif MODEMENU

#define NBUTS 3
#define DIRS 2
#define UP 1
#define DOWN 0
#define SHIFTS 8		/* three keys, so eight combinations */
#define	Coordinate(r,c)		((r) * ncols + (c))

char *GetRestOfLine();
char *SaveText();
extern UnSaltText();
extern StartCut();
extern ReExecute();
extern EditorDown();

extern ButtonUp();
extern DownButtonDown();
extern MiddleButtonDown();
extern UpButtonDown();
extern ModeMenu();
extern char *xterm_name;
extern Bogus(), Silence();
extern GINbutton();
/* due to LK201 limitations, not all of the below are actually possible */
static int (*bfunc[SHIFTS][DIRS][NBUTS])() = {
/*	left		middle		right	*/
	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	ReExecute,	StartCut,	Silence,	/* down |	  */
	Silence,	Silence,	UnSaltText,	/* up	|shift	  */

	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta	  */

	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta shift */

	ModeMenu,	ModeMenu,	ModeMenu,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|control  */

	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|ctl shift */

	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	EditorDown,	EditorDown,	EditorDown,	/* down	| control  */
	Silence,	Silence,	Silence		/* up	|meta shift*/

};	/* button, shift keys, and direction */
static int (*tfunc[SHIFTS][DIRS][NBUTS])() = {
/*	left		middle		right	*/
	ModeMenu,	ModeMenu,	ModeMenu,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	ModeMenu,	ModeMenu,	ModeMenu,	/* down |	  */
	Silence,	Silence,	Silence,	/* up	|shift	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta shift */

	ModeMenu,	ModeMenu,	ModeMenu,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|control  */

	ModeMenu,	ModeMenu,	ModeMenu,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|ctl shift */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	Bogus,		Bogus,		Bogus,		/* down	| control  */
	Silence,	Silence,	Silence		/* up	|meta shift*/

};	/* button, shift keys, and direction */
static int (*scrollfunc[SHIFTS][DIRS][NBUTS])() = {
/*	left		middle		right	*/
	DownButtonDown,	ModeMenu,	UpButtonDown,	/* down	|	  */
	ButtonUp,	Silence,	ButtonUp,	/* up	|no shift */

	DownButtonDown,	ModeMenu,	UpButtonDown,	/* down |	  */
	ButtonUp,	Silence,	ButtonUp,	/* up	|shift	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta shift */

	DownButtonDown,	ModeMenu,	UpButtonDown,	/* down	|	  */
	ButtonUp,	Silence,	ButtonUp,	/* up	|control  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|ctl shift */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	Bogus,		Bogus,		Bogus,		/* down	| control  */
	Silence,	Silence,	Silence		/* up	|meta shift*/

};	/* button, shift keys, and direction */
static int (*Tbfunc[SHIFTS][DIRS][NBUTS])() = {
/*	left		middle		right	*/
	GINbutton,	GINbutton,	GINbutton,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	GINbutton,	GINbutton,	GINbutton,	/* down |	  */
	Silence,	Silence,	Silence,	/* up	|shift	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta shift */

	ModeMenu,	ModeMenu,	ModeMenu,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|control  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|ctl shift */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	Bogus,		Bogus,		Bogus,		/* down	| control  */
	Silence,	Silence,	Silence		/* up	|meta shift*/

};	/* button, shift keys, and direction */

extern Terminal term;

static int crow, ccol;	/* cut row and column */
static int ccoord;
static int ncols;

HandleButtons(term, reply, pty)
register Terminal *term;
register XEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	int (*bp)();
	int dir = DOWN;
	/* so table above will be nice, we change left to right */
	int button = 2 - ((XKeyOrButtonEvent *)reply)->detail & 0177; 
	int shift = KeyState(((XKeyOrButtonEvent *)reply)->detail);

	switch (reply->type) {
		case ButtonPressed:
			dir = DOWN;
			break;
		case ButtonReleased:
			dir = UP;
			break;
		default:
			break;
	}
	if(L_flag && !checklogin()) {	/* if login window, check for login */
		if(dir == DOWN)
			Bell();
		return;
	}
	bp = (screen->sb && (reply->window == screen->sb->bar ||
	 GetButtonState(screen->sb) != BUTTON_NORMAL)) ?
	 scrollfunc[shift][dir][button] :
	 ((Titlebar(screen) && (reply->window == screen->title.tbar ||
	 reply->window == screen->Ttitle.tbar)) ? tfunc[shift][dir][button] :
	 ((reply->window == TWindow(screen)) ? Tbfunc[shift][dir][button] :
	 bfunc[shift][dir][button]));
	if (bp != NULL)
		(*bp)(term, reply, pty);
	else
		Bell();
}

UnSaltText(term, reply, pty)
register Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	char *line;
	int nbytes;
	register char *lag, *cp, *end;

	line = XFetchBytes(&nbytes);
	end = &line[nbytes];
	lag = line;
	for (cp = line; cp != end; cp++)
	{
		if (*cp != '\n') continue;
		*cp = '\r';
		write(pty, lag, cp - lag + 1);
		lag = cp + 1;
	}
	if (lag != end)
		write(pty, lag, end - lag);
	free (line);	/* free text from fetch */
}

ReExecute(term, reply, pty)
register XKeyOrButtonEvent *reply;
Terminal *term;
int pty;
{
	XKeyOrButtonEvent xevent;
	register XKeyOrButtonEvent *ev = &xevent;
	register Screen *screen = &term->screen;
	register int xrow, xcol, mask, cursor, ignore;
	register char *line;
	int row, col;

	cursor = screen->curs;
	if(!XGrabMouse(VWindow(screen), cursor,
	 mask = ButtonReleased | EnterWindow | LeaveWindow | MouseMoved)) {
		Bell();
		return;
	}
	ncols = screen->max_col + 1;	/* needed by Coordinate() define */
	if(NearestRowCol(reply->y, reply->x, &crow, &ccol) ||
	 crow > screen->max_row) {
		Bell();
		XUngrabMouse();
		return;
	}
	xrow = crow;
	xcol = ccol;
	crow++;
	ccol = 0;
	ccoord = Coordinate(crow, ccol);
	if (screen->cursor_state)
	    HideCursor ();
	HiliteText(xrow, xcol, crow, 0, TRUE);
	ignore = FALSE;
	for( ; ; ) {
		XMaskEvent(mask, ev);
		switch(ev->type) {
		 case ButtonReleased:
			if(xrow == crow - 1) {
				line = GetRestOfLine(screen, xrow, xcol);
				row = strlen(line);
				write(pty, line, row);
				line[row] = '\n';
				XStoreBytes(line, row);
				free (line);	/* free text from fetch */
				HiliteText(xrow, xcol, crow, 0, FALSE);
			}
			XUngrabMouse();
			if (screen->cursor_set && !screen->cursor_state)
			    ShowCursor ();
			return;
		 case LeaveWindow:
			if(ev->window != VWindow(screen))
				break;
			if(xrow == crow - 1)
				HiliteText(xrow, xcol, crow, 0, FALSE);
			xrow = crow;
			xcol = 0;
			XGrabMouse(VWindow(screen), cursor, mask & ~MouseMoved);
			ignore = TRUE;
			break;
		 case EnterWindow:
			if(ev->window != VWindow(screen))
				break;
			XGrabMouse(VWindow(screen), cursor, mask);
			ignore = FALSE;
				/* drop through */
		 case MouseMoved:
			if(ignore)
				break;
			NearestRowCol(ev->y, ev->x, &row, &col);
			if(row != crow - 1) {
				if(xrow == crow - 1)
					HiliteText(xrow, xcol, crow, 0, FALSE);
			} else if(xrow == row)
				TrackText(xrow, xcol, row, col);
			else
				HiliteText(row, col, crow, 0, TRUE);
			xrow = row;
			xcol = col;
			break;
		}
	}
}
	
char *GetRestOfLine(screen, row, col)
register Screen *screen;
register int row, col;
{
	char *line;
	int i;

	i = Length(screen, row, col, screen->max_col);
	if((line = (char *)malloc(i + 2)) == NULL)
		SysError(ERROR_BMALLOC);
	SaveText(screen, row, col, screen->max_col, line);
	line[i] = '\r';
	line[i+1] = '\0';
	return(line);
}

StartCut(term, reply, pty)
register XKeyOrButtonEvent *reply;
Terminal *term;
int pty;
{
	XKeyOrButtonEvent xevent;
	register XKeyOrButtonEvent *ev = &xevent;
	register Screen *screen = &term->screen;
	register int xrow, xcol, mask, cursor, ignore;
	int row, col;

	cursor = screen->curs;
	if(!XGrabMouse(VWindow(screen), cursor,
	 mask = ButtonReleased | EnterWindow | LeaveWindow | MouseMoved)) {
		Bell();
		return;
	}
	ncols = screen->max_col + 1;	/* needed by Coordinate() define */
	NearestRowCol(reply->y, reply->x, &crow, &ccol);
	ccoord = Coordinate(crow, ccol);
	xrow = crow;
	xcol = ccol;
	ignore = FALSE;
	if (screen->cursor_state)
	    HideCursor ();
	for( ; ; ) {
		XMaskEvent(mask, ev);
		switch(ev->type) {
		 case ButtonReleased:
			if(!ignore) {
				row = crow;
				col = ccol; /* SaltTextAway may alter these */
				SaltTextAway(term, xrow, xcol, pty);
			}
			HiliteText(row, col, xrow, xcol, FALSE);
			XUngrabMouse();
			if (screen->cursor_set && !screen->cursor_state)
			    ShowCursor ();
			return;
		 case LeaveWindow:
			if(ev->window != VWindow(screen))
				break;
			HiliteText(crow, ccol, xrow, xcol, FALSE);
			xrow = crow;
			xcol = ccol;
			XGrabMouse(VWindow(screen), cursor, mask & ~MouseMoved);
			ignore = TRUE;
			break;
		 case EnterWindow:
			if(ev->window != VWindow(screen))
				break;
			XGrabMouse(VWindow(screen), cursor, mask);
			ignore = FALSE;
				/* drop through */
		 case MouseMoved:
			if(ignore)
				break;
			NearestRowCol(ev->y, ev->x, &row, &col);
			if(row != xrow || col != xcol) {
				TrackText(xrow, xcol, row, col);
				xrow = row;
				xcol = col;
			}
			break;
		}
	}
}

int NearestRowCol(y, x, r, c)
register int y, x;
int *r, *c;
{
	register Screen *screen = &term.screen;
	register row, col, i;
	register char *ch;
	register int passed_eol = FALSE;

	if((row = (y - screen->border - Titlebar(screen)) / FontHeight(screen))
	 < 0)
		row = 0;
	else if(row > screen->max_row)
		row = screen->max_row;
	i = FontWidth(screen);
	if((col = (x - screen->border + (i / 3)) / i) < 0)
		col = 0;
	else if(col > screen->max_col) {
		col = 0;
		row++;	/* this can be screen->max_row + 1 */
		passed_eol = TRUE;
	}
	if(col > 0) {
		for(i = screen->max_col,
		 ch = screen->buf[2 * (row + screen->topline)] + i ;
		 i > 0 && *ch == 0 ; ch--, i--);
		if(col > i + 1) {
			col = 0;
			row++;
			passed_eol = TRUE;
		}
	}
	*r = row;
	*c = col;
	return(passed_eol);
}

TrackText(frow, fcol, trow, tcol)
register int frow, fcol, trow, tcol;
{
	register int f, t;

	f = Coordinate(frow, fcol);
	t = Coordinate(trow, tcol);
	if(f == t)
		return;
	if(f > ccoord) {
		if(t < ccoord) {
			HiliteText(crow, ccol, frow, fcol, FALSE);
			HiliteText(trow, tcol, crow, ccol, TRUE);
		} else if(t > f)
			HiliteText(frow, fcol, trow, tcol, TRUE);
		else
			HiliteText(trow, tcol, frow, fcol, FALSE);
	} else {
		if(t > ccoord) {
			HiliteText(frow, fcol, crow, ccol, FALSE);
			HiliteText(crow, ccol, trow, tcol, TRUE);
		} else if(t < f)
			HiliteText(trow, tcol, frow, fcol, TRUE);
		else
			HiliteText(frow, fcol, trow, tcol, FALSE);
	}
}

HiliteText(frow, fcol, trow, tcol, hilite)
register int frow, fcol, trow, tcol;
int hilite;
{
	register Screen *screen = &term.screen;
	register int i, j;

	if((i = Coordinate(frow, fcol)) == (j = Coordinate(trow, tcol)))
		return;
	else if(i > j) {
		i = frow;
		j = fcol;
		frow = trow;
		fcol = tcol;
		trow = i;
		tcol = j;
	}
	if(hilite) {
		i = screen->foreground;
		screen->foreground = screen->background;
		screen->background = i;
	}
	if(frow != trow) {	/* do multiple rows */
		if((i = screen->max_col - fcol + 1) > 0) {	/* first row */
			XPixSet(VWindow(screen), fcol * FontWidth(screen) +
			 screen->border, frow * FontHeight(screen) +
			 screen->border + Titlebar(screen), i * FontWidth(screen),
			 FontHeight(screen), screen->background);
			ScrnRefresh(screen, frow, fcol, 1, i);
		}
		if((i = trow - frow - 1) > 0) {			/* middle rows*/
			j = screen->max_col + 1;
			XPixSet(VWindow(screen), screen->border, (frow + 1) *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 j * FontWidth(screen), i * FontHeight(screen),
			 screen->background);
			ScrnRefresh(screen, frow + 1, 0, i, j);
		}
		if(tcol > 0 && trow <= screen->max_row) {	/* last row */
			XPixSet(VWindow(screen), screen->border, trow *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 tcol * FontWidth(screen), FontHeight(screen),
			 screen->background);
			ScrnRefresh(screen, trow, 0, 1, tcol);
		}
	} else {		/* do single row */
		i = tcol - fcol;
		XPixSet(VWindow(screen), fcol* FontWidth(screen) + screen->border,
		 frow * FontHeight(screen) + screen->border + Titlebar(screen),
		 i * FontWidth(screen), FontHeight(screen), screen->background);
		ScrnRefresh(screen, frow, fcol, 1, tcol - fcol);
	}
	if(hilite) {
		i = screen->foreground;
		screen->foreground = screen->background;
		screen->background = i;
	}
}

SaltTextAway(term, row, col, pty)
Terminal *term;
register row, col;
int pty;
{
	register Screen *screen = &term->screen;
	register int i, j = 0;
	register char *str;		/* string to be saved */
	char *line, *lp;

	/* first order of business is to guarantee that crow,ccol is before */
	/* row,col. */
	if ( row == crow ) {		/* must exchange as pairs */
		if ( ccol > col ) { 	/* may have to exchange columns */
			register int tmp;
			tmp = ccol; ccol = col; col = tmp;
		}
	}
	else {
		if ( crow > row ) {	/* may have to exchange row and col */
			register int tmp;
			tmp = ccol; ccol = col; col = tmp;
			tmp = crow; crow = row; row = tmp;
		}
	}
		
	if (ccol < 0) ccol = 0;
	else if (ccol > screen->max_col) { crow++; ccol = 0; }
	if (crow < 0) crow = ccol = 0;
	else if (crow > screen->max_row) { crow = screen->max_row; ccol = 0; }

	if (row > screen->max_row) { row = screen->max_row + 1 ; col = 0; }
	else if (--col > screen->max_col) col = screen->max_col;

	/* first we need to know how long the string is before we can save it*/

	if ( row == crow ) j = Length(screen, crow, ccol, col);
	else {	/* two cases, cut is on same line, cut spans multiple lines */
		j += Length(screen, crow, ccol, screen->max_col) + 1;
		for(i = crow + 1; i < row; i++) 
			j += Length(screen, i, 0, screen->max_col) + 1;
		if (col >= 0)
			j += Length(screen, row, 0, col);
	}
	
	/* now get some memory to save it in */

	if((line = (char *)malloc(j + 1)) == NULL)
		SysError(ERROR_BMALLOC2);
	line[j] = '\0';		/* make sure it is null terminated */
	lp = line;		/* lp points to where to save the text */
	if ( row == crow ) lp = SaveText(screen, row, ccol, col, lp);
	else {
		lp = SaveText(screen, crow, ccol, screen->max_col, lp);
		*lp ++ = '\n';	/* put in newline at end of line */
		for(i = crow +1; i < row; i++) {
			lp = SaveText(screen, i, 0, screen->max_col, lp);
			*lp ++ = '\n';
			}
		if (col >= 0)
			lp = SaveText(screen, row, 0, col, lp);
	}
	*lp = '\0';		/* make sure we have end marked */
	
	XStoreBytes(line, j);
	free(line);
}

/* returns number of chars in line from scol to ecol out */
int Length(screen, row, scol, ecol)
register int row, scol, ecol;
register Screen *screen;
{
	register char *ch;
	int end = 0;

	ch = screen->buf[2 * (row + screen->topline)];
	while (ecol >= scol && ch[ecol] == 0)
	    ecol--;
	return (ecol - scol + 1);
}

/* copies text into line, preallocated */
char *SaveText(screen, row, scol, ecol, lp)
int row;
int scol, ecol;
Screen *screen;
register char *lp;		/* pointer to where to put the text */
{
	register int i = 0;
	register char *ch = screen->buf[2 * (row + screen->topline)];
	register int c;

	if ((i = Length(screen, row, scol, ecol)) == 0) return(lp);
	ecol = scol + i;
	for (i = scol; i < ecol; i++) {
		if ((c = ch[i]) == 0)
			c = ' ';
		else if(c < ' ') {
			if(c == '\036')
				c = '#';
			else
				c += 0x5f;
		} else if(c == 0x7f)
			c = 0x5f;
		*lp++ = c;
	}
	return(lp);
}

EditorDown (term, reply, pty)
Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	char line[6];
	register unsigned row, col;
	int button; 

	if (!screen->send_mouse_pos) {
		Bell();
		return;
	}
	button = 2 - reply->detail & 0177; 
	row = (reply->y - screen->border - Titlebar(screen)) / FontHeight(screen);
	col = (reply->x - screen->border) / FontWidth(screen);
	strcpy(line, "\033[M");
	line[3] = ' ' + button;
	line[4] = ' ' + col + 1;
	line[5] = ' ' + row + 1;
	write(pty, line, 6);
}

#ifdef ALLOWUNSHIFTEDSELECTION
UnshiftedSelectionInit()
{
	register int i, j;

	/* copy table for shifted actions to table for unshifted
	 * actions.  We assume the shifted actions are in bfunc[1]
	 * and the unshifted ones go in bfunc[0].
	 */
	for (i = 0; i < DIRS; i++)
		for (j = 0; j < NBUTS; j++)
			bfunc[0][i][j] = bfunc[1][i][j];
}
#endif

#ifdef MODEMENU
#define	MAXWINDOWMENU	64
#define	XTERMMENU	0
#define	VTMENU		1
#define	TEKMENU		2
#define	SCROLLBARMENU	3
#ifndef NOWINDOWMENU
#define	WINDOWMENU	4
#define	NMENUS		5
#else NOWINDOWMENU
#define	NMENUS		4
#endif NOWINDOWMENU

static Menu *menus[NMENUS];
#ifndef NOWINDOWMENU
static char *namebuf[MAXWINDOWMENU + 1];
static char *wname;
static Window windows[MAXWINDOWMENU];
#endif NOWINDOWMENU

ModeMenu(term, reply, pty)
Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register Menu *menu;
	register int type, item;
	static int inited;
	extern TekLink *TekRefresh;
	extern int xeventpass();
	extern Menu *setupmenu(), *Tsetupmenu(), *xsetupmenu();
#ifndef NOWINDOWMENU
	extern Menu *wsetupmenu();
#endif NOWINDOWMENU
	extern Menu *ssetupmenu();

	if((reply->window == screen->title.tbar || reply->window ==
	 screen->Ttitle.tbar) && InTitle(screen, reply->window,
	 reply->x))
		return;
	if(!inited) {
		extern Pixmap Gray_Tile;
		extern Cursor Menu_DefaultCursor;
		extern char *Menu_DefaultFont;
		extern FontInfo *Menu_DefaultFontInfo;

		inited++;
		Gray_Tile = screen->graybordertile;
		InitMenu(xterm_name);
		Menu_DefaultCursor = screen->arrow;
		if(strcmp(Menu_DefaultFont, f_t) == 0)
			Menu_DefaultFontInfo = screen->titlefont;
	}
	if((reply->detail & 0177) == LeftButton)
		type = XTERMMENU;
	else if((reply->detail & 0177) == RightButton)
#ifndef NOWINDOWMENU
		type = WINDOWMENU;
#else NOWINDOWMENU
		{
		    Bell();
		    return;
		}
#endif NOWINDOWMENU
	else if(reply->window == VWindow(screen) || reply->window ==
	 screen->title.tbar)
		type = VTMENU;
	else if(reply->window == TWindow(screen) || reply->window ==
	 screen->Ttitle.tbar)
		type = TEKMENU;
	else
		type = SCROLLBARMENU;
	switch(type) {
	 case XTERMMENU:
		if((menu = xsetupmenu(&menus[XTERMMENU])) == NULL)
			return;
		break;
	 case VTMENU:
		if((menu = setupmenu(&menus[VTMENU])) == NULL)
			return;
		break;
#ifndef NOWINDOWMENU
	 case WINDOWMENU:
		wname = (reply->window == screen->title.tbar || reply->window
		 == VWindow(screen)) ? screen->winname : screen->Twinname;
		if((menu = wsetupmenu(&menus[WINDOWMENU])) == NULL)
			return;
		break;
#endif NOWINDOWMENU
	 case TEKMENU:
		if((menu = Tsetupmenu(&menus[TEKMENU])) == NULL)
			return;
		screen->waitrefresh = TRUE;
		break;
	 case SCROLLBARMENU:
		if((menu = ssetupmenu(&menus[SCROLLBARMENU])) == NULL)
			return;
		break;
	}
	/*
	 * Make the window entry and leaving routines holdoff on setting
	 * the timer and on selecting or unselecting any windows.  Then
	 * set the select mode manually.
	 */
	screen->holdoff = TRUE;
	SetMenuEventHandler(menu, xeventpass);
	item = TrackMenu(menu, reply);
	menusync();
	screen->waitrefresh = FALSE;
	screen->timer = 0;
	screen->holdoff = FALSE;
	reselectwindow(screen);

	if (item < 0) {
		if(type == TEKMENU && TekRefresh)
			dorefresh();
#ifndef NOWINDOWMENU
		else if(type == WINDOWMENU)
			wfree(menu);
#endif NOWINDOWMENU
		return;
	}
	switch(type) {
	 case XTERMMENU:
		xdomenufunc(item);
		break;
	 case VTMENU:
		domenufunc(item);
		break;
#ifndef NOWINDOWMENU
	 case WINDOWMENU:
		wdomenufunc(item);
		wfree(menu);
		break;
#endif NOWINDOWMENU
	 case TEKMENU:
		Tdomenufunc(item);
		break;
	 case SCROLLBARMENU:
		sdomenufunc(item);
		break;
	}
}

menusync()
{
	XEvent ev;

	XSync(0);
	while(QLength() > 0) {
		XNextEvent(&ev);
		xeventpass(&ev);
	}
}

#define	XMENU_TITLE	0
#define XMENU_ACTIVEICON (XMENU_TITLE+1)
#define XMENU_ALLOWICONINPUT (XMENU_ACTIVEICON+1)
#define	XMENU_AUTORAISE	(XMENU_ALLOWICONINPUT+1)
#define	XMENU_AUDIBLEBELL (XMENU_AUTORAISE+1)
#define	XMENU_VISUALBELL (XMENU_AUDIBLEBELL+1)
#define	XMENU_DEICONWARP (XMENU_VISUALBELL+1)
#define	XMENU_SENDMOUSE	(XMENU_DEICONWARP+1)
#define	XMENU_LOG	(XMENU_SENDMOUSE+1)
#define	XMENU_LINE	(XMENU_LOG+1)
#define	XMENU_REDRAW	(XMENU_LINE+1)
#define	XMENU_RESUME	(XMENU_REDRAW+1)
#define	XMENU_SUSPEND	(XMENU_RESUME+1)
#define	XMENU_INTR	(XMENU_SUSPEND+1)
#define	XMENU_HANGUP	(XMENU_INTR+1)
#define	XMENU_TERM	(XMENU_HANGUP+1)
#define	XMENU_KILL	(XMENU_TERM+1)

static char *xtext[] = {
	"Title Bar(s)",
	"Active Icon",
	"Allow Icon Input",
	"Auto Raise",
	"Audible Bell",
	"Visual Bell",
	"Deiconify Warp",
	"Send Mouse Pos",
	"Logging",
	"-",
	"Redraw",
	"Continue",
	"Suspend",
	"Interrupt",
	"Hangup",
	"Terminate",
	"Kill",
	0,
};

static int xauto;
static int xabell;
static int xvbell;
static int xdeiconwarp;
static int xsendmouse;
static int xlog;
static int xtbar;
static int xactive_icon;
static int xallow_iconinput;

Menu *xsetupmenu(menu)
register Menu **menu;
{
	register Screen *screen = &term.screen;
	register char **cp;
	register int i;

	if (*menu == NULL) {
		if ((*menu = NewMenu("xterm X10/6.6B", re_verse)) == NULL)
			return(NULL);
		for(cp = xtext ; *cp ; cp++)
			AddMenuItem(*menu, *cp);
		if(xtbar = (Titlebar(screen) > 0))
			CheckItem(*menu, XMENU_TITLE);
		if(xactive_icon = screen->active_icon)
			CheckItem(*menu, XMENU_ACTIVEICON);
		if(xallow_iconinput = (term.flags & ICONINPUT))
			CheckItem(*menu, XMENU_ALLOWICONINPUT);
		SetItemDisable(*menu, XMENU_ALLOWICONINPUT, !xactive_icon);
		if(xauto = screen->autoraise)
			CheckItem(*menu, XMENU_AUTORAISE);
		if(xabell = screen->audiblebell)
			CheckItem(*menu, XMENU_AUDIBLEBELL);
		if(xvbell = screen->visualbell)
			CheckItem(*menu, XMENU_VISUALBELL);
		if(xdeiconwarp = screen->deiconwarp)
			CheckItem(*menu, XMENU_DEICONWARP);
		if(xlog = screen->logging)
			CheckItem(*menu, XMENU_LOG);
		if(xsendmouse = screen->send_mouse_pos)
			CheckItem(*menu, XMENU_SENDMOUSE);
		DisableItem(*menu, XMENU_LINE);
		if(screen->inhibit & I_LOG)
			DisableItem(*menu, XMENU_LOG);
		if(screen->inhibit & I_SIGNAL)
			for(i = XMENU_SUSPEND ; i <= XMENU_KILL ; i++)
				DisableItem(*menu, i);
		return(*menu);
	}
	if (xtbar != (Titlebar(screen) > 0))
		SetItemCheck(*menu, XMENU_TITLE, (xtbar =
		 (Titlebar(screen) > 0)));
	if (xactive_icon != screen->active_icon) {
		SetItemCheck(*menu, XMENU_ACTIVEICON,
			     (xactive_icon = screen->active_icon));
		SetItemDisable(*menu, XMENU_ALLOWICONINPUT, !xactive_icon);
	}
	if (xallow_iconinput != (term.flags & ICONINPUT))
		SetItemCheck(*menu, XMENU_ALLOWICONINPUT,
			     (xallow_iconinput = (term.flags & ICONINPUT)));
	if (xauto != screen->autoraise)
		SetItemCheck(*menu, XMENU_AUTORAISE, (xauto =
		 screen->autoraise));
	if (xabell != screen->audiblebell)
		SetItemCheck(*menu, XMENU_AUDIBLEBELL, (xabell =
		 screen->audiblebell));
	if (xvbell != screen->visualbell)
		SetItemCheck(*menu, XMENU_VISUALBELL, (xvbell =
		 screen->visualbell));
	if (xdeiconwarp != screen->deiconwarp)
		SetItemCheck(*menu, XMENU_DEICONWARP, (xdeiconwarp =
		 screen->deiconwarp));
	if (xsendmouse != screen->send_mouse_pos)
		SetItemCheck(*menu, XMENU_SENDMOUSE,
			(xsendmouse = screen->send_mouse_pos));
	if (xlog != screen->logging)
		SetItemCheck(*menu, XMENU_LOG, (xlog = screen->logging));
	return(*menu);
}

xdomenufunc(item)
int item;
{
	register Screen *screen = &term.screen;

	switch (item) {
	case XMENU_TITLE:
		if(Titlebar(screen)) {
		        screen->fullVwin.titlebar =
			    screen->fullTwin.titlebar = 0;
			if(VWindow(screen))
				VTTitleHide();
			if(TWindow(screen))
				TekTitleHide();
		} else {
		        screen->fullVwin.titlebar =
			    screen->fullTwin.titlebar = screen->titleheight;
			if(VWindow(screen))
				VTTitleShow(FALSE);
			if(TWindow(screen))
				TekTitleShow(FALSE);
		}
		break;

	case XMENU_ACTIVEICON:
		screen->active_icon = !screen->active_icon;
		if (screen->active_icon && !screen->fnt_icon) {
		    FontInfo *fInfo = XOpenFont( f_i );
		    screen->fnt_icon = fInfo->id;
		    screen->iconVwin.f_width = fInfo->width;
		    screen->iconVwin.f_height = fInfo->height;
		}

		if (screen->iconVwin.window) {
		    SetIconSize( screen );
		    XSelectInput( screen->iconVwin.window,
		    		  screen->active_icon
				  && (term.flags & ICONINPUT)
				    ? ICONWINDOWEVENTS | ICONINPUTEVENTS
				    : ICONWINDOWEVENTS );
		}

		if (screen->iconTwin.window) {
		    TSetIconSize( screen );
		    XSelectInput( screen->iconTwin.window,
				  screen->active_icon
				  && (term.flags & ICONINPUT)
				    ? TICONWINDOWEVENTS | ICONINPUTEVENTS
				    : TICONWINDOWEVENTS );
		}

		break;

	case XMENU_ALLOWICONINPUT:
		term.flags ^= ICONINPUT;
		if (screen->iconVwin.window)
		    XSelectInput( screen->iconVwin.window,
				  screen->active_icon
				  && (term.flags & ICONINPUT)
				    ? ICONWINDOWEVENTS | ICONINPUTEVENTS
				    : ICONWINDOWEVENTS );

		if (screen->iconTwin.window)
		    XSelectInput( screen->iconTwin.window,
				  screen->active_icon
				  && (term.flags & ICONINPUT)
				    ? TICONWINDOWEVENTS | ICONINPUTEVENTS
				    : TICONWINDOWEVENTS );

		break;

	case XMENU_AUTORAISE:
		screen->autoraise = !screen->autoraise;
		break;

	case XMENU_DEICONWARP:
		screen->deiconwarp = !screen->deiconwarp;
		break;

	case XMENU_AUDIBLEBELL:
		screen->audiblebell = !screen->audiblebell;
		break;

	case XMENU_VISUALBELL:
		screen->visualbell = !screen->visualbell;
		break;

	case XMENU_SENDMOUSE:
		screen->send_mouse_pos = !screen->send_mouse_pos;
		break;

	case XMENU_LOG:
		if(screen->logging)
			CloseLog(screen);
		else
			StartLog(screen);
		break;

	case XMENU_REDRAW:
		Redraw();
		break;

	case XMENU_RESUME:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGCONT);
		break;

	case XMENU_SUSPEND:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGTSTP);
		break;

	case XMENU_INTR:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGINT);
		break;

	case XMENU_HANGUP:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGHUP);
		break;

	case XMENU_TERM:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGTERM);
		break;

	case XMENU_KILL:
		if(screen->pid > 1)
			killpg(getpgrp(screen->pid), SIGKILL);
		break;
	}
}

#ifndef NOWINDOWMENU
Menu *wsetupmenu(menu)
register Menu **menu;
{
	register Window *cp, *wp;
	register char **np;
	register int i, j;
	Window win, *children;
	int nchildren;
	char *name;
	WindowInfo winfo;

	if(!XQueryTree(RootWindow, &win, &nchildren, &children))
		return(NULL);
	if(nchildren > MAXWINDOWMENU)
		nchildren = MAXWINDOWMENU;
	if ((*menu = NewMenu("Windows", re_verse)) == NULL) {
		free((char *)children);
		return(NULL);
	}
	np = namebuf;
	wp = windows;
	for(i = nchildren, j = 0, cp = children ; i > 0 ; cp++, i--) {
		if(!XQueryWindow(*cp, &winfo))
			goto failed;
		if(winfo.mapped != IsMapped)
			continue;
		if(!XFetchName(*cp, &name)) {
failed:
			free((char *)children);
			*np = NULL;
			wfree(*menu);
			return(NULL);
		}
		if(name == NULL)
			continue;
		AddMenuItem(*menu, *np++ = name);
		*wp++ = *cp;
		if(strcmp(wname, name) == 0)
			CheckItem(*menu, j);
		j++;
	}
	*np = NULL;
	free((char *)children);
	if(np > namebuf)
		return(*menu);
	DisposeMenu(*menu);
	return(NULL);
}

wdomenufunc(item)
int item;
{
	register Window w;

	if((w = windows[item]) != NULL)
		XRaiseWindow(w);
}

wfree(menu)
Menu *menu;
{
	register char **np;

	for(np = namebuf ; *np ; np++)
		free(*np);
	DisposeMenu(menu);
}
#endif NOWINDOWMENU

MenuNewCursor(cur)
register Cursor cur;
{
	register Menu **menu;
	register int i;
	extern Cursor Menu_DefaultCursor;

	Menu_DefaultCursor = cur;
	for(i = XTERMMENU, menu = menus ; i <= TEKMENU ; menu++, i++) {
		if(!*menu)
			continue;
		(*menu)->menuCursor = cur;
		if((*menu)->menuWindow)
			XDefineCursor((*menu)->menuWindow, cur);
	}
}
#else MODEMENU

ModeMenu(term, reply, pty)
Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;

	if((reply->window == screen->title.tbar || reply->window ==
	 screen->Ttitle.tbar) && InTitle(screen, reply->window,
	 reply->x))
		return;
	Bell();
}
#endif MODEMENU

GINbutton(term, reply, pty)
Terminal *term;
XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register int i;

	if(screen->TekGIN) {
		i = "rml"[reply->detail & 0xff];
		if(reply->detail & ShiftMask)
			i = toupper(i);
		TekEnqMouse(i | 0x80);	/* set high bit */
		TekGINoff();
	} else
		Bell();
}

Bogus(term, reply, pty)
Terminal *term;
XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	Bell();
}

Silence(term, reply, pty)
Terminal *term;
XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
}
