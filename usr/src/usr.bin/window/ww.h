/*	@(#)ww.h	1.4 83/07/19		*/

#include <stdio.h>
#include <sgtty.h>
#include "window.h"

struct ww {
	char ww_state;		/* state of window creation */
	char ww_mode;		/* mode used to open this window */
	char ww_wstate;		/* state for printing charcters */
	int ww_insert :1;	/* insert mode, for printing */
	int ww_refresh :1;	/* force refresh after \n and others */
	char ww_ident;
	Win *ww_win;
	int ww_row;		/* outside dimensions */
	int ww_col;
	int ww_nrow;
	int ww_ncol;
	int ww_irow;		/* inside dimensions */
	int ww_icol;
	int ww_inrow;
	int ww_incol;
	int ww_pty;		/* pty or socket pair */
	int ww_tty;
	int ww_pid;
	struct ww *ww_next;
};

struct ww_tty {
	struct sgttyb ww_sgttyb;
	struct tchars ww_tchars;
	struct ltchars ww_ltchars;
	int ww_lmode;
	int ww_ldisc;
	int ww_pgrp;
};

	/* ww_state values */
#define WW_INITIAL	0
#define WW_HASPROC	1
#define WW_INCHILD	2
#define WW_DEAD		3

	/* ww_mode values */
#define WW_PTY		0		/* has pty */
#define WW_SOCKET	1		/* has socket pair */
#define WW_NONE		2		/* has nothing */

#ifndef CTRL
#define CTRL(c)	('c'&0x1f)
#endif

extern struct ww *wwhead, *curwin;
extern struct ww_tty wwoldtty, wwnewtty;
extern int wwnwrite;

#define wwputchar(c)	wwputc((c), curwin)
#define wwputstr(s)	wwputs((s), curwin)
#define wwsetcursor(r,c) (WRCurRow = (r), WRCurCol = (c))
#define wwflush()	Wrefresh(1)

struct ww *wwopen();
struct ww *wwfind();
int wwchild();
