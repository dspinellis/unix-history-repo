/*	@(#)ww.h	1.5 83/07/22		*/

#include <stdio.h>
#include <sgtty.h>
#include "window.h"

struct ww_dim {
	short col;
	short row;
	short ncol;
	short nrow;
};

struct ww {
	char ww_state;		/* state of window creation */
	char ww_mode;		/* mode used to open this window */
	char ww_wstate;		/* state for printing charcters */
	int ww_insert :1;	/* insert mode, for printing */
	int ww_refresh :1;	/* force refresh after \n and others */
	char ww_ident;
	Win *ww_win;
	struct ww_dim ww_o;	/* outside dimemsions */
	struct ww_dim ww_i;	/* inside dimemsions */
	struct ww_dim ww_w;	/* window dimemsions */
	int ww_pty;		/* pty or socket pair */
	int ww_tty;
	int ww_pid;
	struct ww *ww_next;
	char ww_ttyname[11];
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
extern int wwnrow, wwncol;		/* the screen size */

#define wwputchar(c)	wwputc((c), curwin)
#define wwputstr(s)	wwputs((s), curwin)
#define wwsetcursor(r,c) (WRCurRow = (r), WRCurCol = (c))
#define wwflush()	Wrefresh(1)

struct ww *wwopen();
struct ww *wwfind();
int wwchild();
