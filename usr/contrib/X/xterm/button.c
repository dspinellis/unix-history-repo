#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/
/*
button.c	Handles button events in the terminal emulator.
		does cut/paste operations, change modes via menu,
		passes button events through to some applications.
				J. Gettys.
*/
#ifndef lint
static char *rcsid_window_c = "$Header: button.c,v 10.11 86/02/01 16:05:41 tony Rel $";
#endif	lint
#include <stdio.h>
#include <X/Xlib.h>
#include "ptyx.h"
#ifdef MODEMENU
#include <X/XMenu.h>
#endif

#define NBUTS 3
#define DIRS 2
#define UP 1
#define DOWN 0
#define SHIFTS 8		/* three keys, so eight combinations */
char *GetRestOfLine();
char *SaveText();
extern UnSaltText();
extern SaltTextAway();
extern StartCut();
extern ReExecute();
extern EditorDown();
#ifdef MODEMENU
extern ModeMenu();
extern char *xterm_name;
#else
#define ModeMenu Bogus
#endif MODEMENU
extern Bogus(), Silence();
/* due to LK201 limitations, not all of the below are actually possible */
int (*bfunc[SHIFTS][DIRS][NBUTS])() = {
/*	left		middle		right	*/
	EditorDown,	EditorDown,	EditorDown,	/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	Silence,	StartCut,	Silence,	/* down |	  */
	ReExecute,	SaltTextAway,	UnSaltText,	/* up	|shift	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta	  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|meta shift */

	Bogus,		ModeMenu,	Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|control  */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|ctl shift */

	Bogus,		Bogus,		Bogus,		/* down	|	  */
	Silence,	Silence,	Silence,	/* up	|no shift */

	Bogus,		Bogus,		Bogus,		/* down	| control  */
	Silence,	Silence,	Silence		/* up	|meta shift*/

};	/* button, shift keys, and direction */

static crow, ccol;	/* cut row and column */

HandleButtons(term, reply, pty)
register Terminal *term;
register XEvent *reply;
int pty;			/* file descriptor of pty */
{
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
	bp = bfunc[shift][dir][button];
	if (bp != NULL) return ((*bp)(term, reply, pty));
	XFeep(0);
	return(0);
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
	return(0);
}

ReExecute(term, reply, pty)
register Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	char *line = GetRestOfLine(&term->screen, reply);
	register int nbytes = strlen(line);

	write(pty, line, nbytes);
	line[nbytes] = '\n';
	XStoreBytes(line, nbytes);
	free (line);	/* free text from fetch */
	return(0);
}
	
char *GetRestOfLine(screen, reply)
register XKeyOrButtonEvent *reply;
register Screen *screen;
{
	char *line;
	int i;
	register int row, col;
	row = (reply->y - screen->border) / screen->f_height;
	col = (reply->x - screen->border) / screen->f_width;
	if ( row < 0 ) row = 0;
	else if ( row > screen->max_row ) row = screen->max_row;
	if ( col < 0 ) col = 0;
	else if ( col > screen->max_col ) col = screen->max_col;
	i = Length(screen, row, col, screen->max_col);
	if((line = (char *)malloc(i + 2)) == NULL) Error();
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
	register Screen *screen = &term->screen;
	crow = (reply->y - screen->border) / screen->f_height;
	ccol = (reply->x - screen->border) / screen->f_width;
	return(0);
}

SaltTextAway(term, reply, pty)
register XKeyOrButtonEvent *reply;
Terminal *term;
int pty;
{
	register Screen *screen = &term->screen;
	register int i, j = 0;
	register row, col;
	register char *str;		/* string to be saved */
	char *line, *lp;
	row = (reply->y - screen->border) / screen->f_height;
	col = (reply->x - screen->border) / screen->f_width;

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

	if((line = (char *)malloc(j + 1)) == NULL) Error();
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
	return(0);
}

/* returns number of chars in line from scol to ecol out */
int Length(screen, row, scol, ecol)
register int row, scol, ecol;
register Screen *screen;
{
	register short *ch;
	int end = 0;
	ch = screen->buf[row];
	while (ecol >= scol &&
		((ch[ecol] & CHAR) == 0 || (ch[ecol] & CHAR) == ' '))
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
	register short *ch = screen->buf[row];
	register int c;
	if ((i = Length(screen, row, scol, ecol)) == 0) return(lp);
	ecol = scol + i;
	for (i = scol; i < ecol; i++)
	{
		c = ch[i] & CHAR;
		if (c == 0) c = ' ';
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
	int button = 2 - reply->detail & 0177; 
	if (!screen->send_mouse_pos) {
		XFeep(0);
		return(0);
	}
	row = (reply->y - screen->border) / screen->f_height;
	col = (reply->x - screen->border) / screen->f_width;
	strcpy(line, "\033[M");
	line[3] = ' ' + button;
	line[4] = ' '+col+1;
	line[5] = ' '+row+1;
	write(pty, line, 6);
	return(0);
}

#ifdef MODEMENU

static int menutoggled;
static Terminal *menuterm;

HandleMenuEvent (rep)
	XEvent *rep;
{
	register Screen *screen = &menuterm->screen;

	switch (rep->type)
	{
	    case ExposeWindow:
		    menutoggled = 1;
		    if (ScreenResize (screen,
			    ((XExposeWindowEvent *)rep)->width,
			    ((XExposeWindowEvent *)rep)->height,
			    menuterm->flags) != -1)
		    {
			TabReset (menuterm->tabs);
			XClear (screen->window);
			ScrnRefresh (screen, 0, 0,
				     screen->max_row + 1,
				     screen->max_col + 1);

			if (screen->TekEmu) TekRefresh (menuterm);
		    }
		    break;

	    case ExposeRegion:
		    if (HandleExposure (screen, rep))
			    menutoggled = 1;
		    break;
	    }
}

#define MMENU_SCROLL 1
#define MMENU_VIDEO 2
#define MMENU_WRAP 3
#define MMENU_NLM 4
#define MMENU_CURSOR 5
#define MMENU_PAD 6
#define MMENU_RESET 7
#define MMENU_FULLRESET 8

static XMenu *menu = NULL;
static int menutermflags = 0;
static int menukbdflags = 0;
static int lastmenupane = 0;
static int lastmenusel = 0;

ModeMenu(term, reply, pty)
Terminal *term;
register XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	register Screen *screen = &term->screen;
	register char **ptr;
	int x, y;
	int ulx, uly;
	int menuw, menuh;
	int pnum = lastmenupane, snum = lastmenusel;
	int status, val;

	if (menu == NULL) {
	    if ((menu = (XMenu *)XMenuCreate(RootWindow, xterm_name)) == NULL)
		return(0);
	    XMenuAddPane(menu, "Modes", 1);
#ifdef JUMPSCROLL    
	    XMenuAddSelection(menu, 0, MMENU_SCROLL,
		    (term->flags & SMOOTHSCROLL) ? "jump scroll" : "smooth scroll",
		    1);
#endif
	    XMenuAddSelection(menu, 0, MMENU_VIDEO,
		    (term->flags & REVERSE_VIDEO) ? "normal video" : "reverse video",
		    1);
	    XMenuAddSelection(menu, 0, MMENU_WRAP,
		    (term->flags & WRAPAROUND) ? "no wrap" : "auto wrap",
		    1);
	    XMenuAddSelection(menu, 0, MMENU_NLM,
		    (term->flags & LINEFEED) ? "normal linefeed" : "auto linefeed",
		    1);
	    XMenuAddSelection(menu, 0, MMENU_CURSOR,
		    (term->keyboard.flags & CURSOR_APL) ? "normal cursors" : "application cursors",
		    1);
	    XMenuAddSelection(menu, 0, MMENU_PAD,
		    (term->keyboard.flags & KYPD_APL) ? "numeric pad" : "application pad",
		    1);
	    XMenuAddSelection(menu, 0, MMENU_RESET, "soft reset", 1);
	    XMenuAddSelection(menu, 0, MMENU_FULLRESET, "full reset", 1);
	    menuterm = term;
	    menutermflags = term->flags;
	    menukbdflags = term->keyboard.flags;
	}
#ifdef JUMPSCROLL
	if ((menutermflags ^ term->flags) & SMOOTHSCROLL) {
	    XMenuChangeSelection(menu, 0, MMENU_SCROLL-1, MMENU_SCROLL, 1,
		    (term->flags & SMOOTHSCROLL) ? "jump scroll" : "smooth scroll",
		    1);
	}
#endif
	if ((menutermflags ^ term->flags) & REVERSE_VIDEO) {
	    XMenuChangeSelection(menu, 0, MMENU_VIDEO-1, MMENU_VIDEO, 1,
		    (term->flags & REVERSE_VIDEO) ? "normal video" : "reverse video",
		    1);
	}
	if ((menutermflags ^ term->flags) & WRAPAROUND) {
	    XMenuChangeSelection(menu, 0, MMENU_WRAP-1, MMENU_WRAP, 1,
		    (term->flags & WRAPAROUND) ? "no wrap" : "auto wrap",
		    1);
	}
	if ((menutermflags ^ term->flags) & LINEFEED) {
	    XMenuChangeSelection(menu, 0, MMENU_NLM-1, MMENU_NLM, 1,
		    (term->flags & LINEFEED) ? "normal linefeed" : "auto linefeed",
		    1);
	}
	if ((menukbdflags ^ term->keyboard.flags) & CURSOR_APL) {
	    XMenuChangeSelection(menu, 0, MMENU_CURSOR-1, MMENU_CURSOR, 1,
		    (term->keyboard.flags & CURSOR_APL) ? "normal cursors" : "application cursors",
		    1);
	}
	if ((menukbdflags ^ term->keyboard.flags) & KYPD_APL) {
	    XMenuChangeSelection(menu, 0, MMENU_PAD-1, MMENU_PAD, 1,
		    (term->keyboard.flags & KYPD_APL) ? "numeric pad" : "application pad",
		    1);
	}
	XMenuRecompute(menu);
	menutermflags = term->flags;
	menukbdflags = term->keyboard.flags;
	x = (reply->location >> 16) & 0xffff;
	y = reply->location & 0xffff;
	XMenuLocate(menu, pnum, snum, x, y, &ulx, &uly, &menuw, &menuh);
	if ((ulx + menuw) > DisplayWidth())
	    x -= ((ulx + menuw) - DisplayWidth());
	if (ulx < 0)
	    x -= ulx;
	if ((uly + menuh) > DisplayHeight())
	    y -= ((uly + menuh) - DisplayHeight());
	if (uly < 0)
	    y -= uly;
	XMenuEventHandler(HandleMenuEvent);
	menutoggled = 0;
	status = XMenuActivate(
	    menu,
	    &pnum, &snum,
	    x, y,
	    MiddleMask|ButtonReleased,
	    (char **)&val
	);
	if (status == XM_FAILURE) return(menutoggled);
	if (status == XM_NO_SELECT) return(menutoggled);
	else {
	    lastmenupane = pnum;
	    lastmenusel = snum;
	}
	switch (val) {
#ifdef JUMPSCROLL
	case MMENU_SCROLL:
	    term->flags ^= SMOOTHSCROLL;
	    if (term->flags & SMOOTHSCROLL) {
		screen->jumpscroll = 0;
		if (screen->scroll_amt) FlushScroll(screen);
	    } else if (!screen->TekEmu) screen->jumpscroll = 1;
	    break;
#endif
	case MMENU_VIDEO:
	    term->flags ^= REVERSE_VIDEO;
	    ReverseVideo(term);
	    menutoggled = 1;
	    break;
	case MMENU_WRAP:
	    term->flags ^= WRAPAROUND;
	case MMENU_NLM:
	    term->flags ^= LINEFEED;
	case MMENU_CURSOR:
	    term->keyboard.flags ^= CURSOR_APL;
	    break;
	case MMENU_PAD:
	    term->keyboard.flags ^= KYPD_APL;
	    break;
	case MMENU_FULLRESET:
	    TabReset (term->tabs);
	    term->keyboard.flags = NULL;
	    screen->mode = ANSInormal;
	    /* Reset Tektronix alpha mode */
	    screen->TekGMode = 0;
	    screen->TekAMode = 0;
	    screen->gsets[0] = 'B';
	    screen->gsets[1] = 'B';
	    screen->gsets[2] = '<';
	    screen->gsets[3] = '<';
	    screen->curgl = 0;
	    screen->curgr = 2;
	    screen->cur_x = screen->cur_y = 0;
	    screen->cur_X = screen->cur_Y = 0;
	    if (term->flags & REVERSE_VIDEO) ReverseVideo(term);
	    term->flags &= ~REVERSE_VIDEO;
	    menutoggled = 1;
	    CursorSet(screen, 0, 0, term->flags);
	    ClearScreen(screen);
	    term->flags = WRAPAROUND|SMOOTHSCROLL;
	case MMENU_RESET:
	    /* reset scrolling region */
	    screen->top_marg = 0;
	    screen->bot_marg = screen->max_row;
	    term->flags &= ~ORIGIN;
	    break;
	}
	return(menutoggled);
}
#endif MODEMENU

Bogus(term, reply, pty)
Terminal *term;
XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	XFeep(0);
	return(0);
}

Silence(term, reply, pty)
Terminal *term;
XKeyOrButtonEvent *reply;
int pty;			/* file descriptor of pty */
{
	return(0);
}
