/*	@(#)ww.h	1.2 83/07/17		*/

#include <stdio.h>
#include <sgtty.h>
#include "window.h"

struct ww {
	int ww_state :4;
	int ww_wstate :4;
	int ww_insert :1;		/* insert mode */
	int ww_refresh:1;		/* force refresh after \n and others */
	Win *ww_win;
	int ww_row;
	int ww_col;
	int ww_nrow;
	int ww_ncol;
	int ww_pty;
	int ww_tty;
	int ww_pid;
	int ww_newrow;			/* for cursor addressing */
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

#define WW_INITIAL	0
#define WW_HASPROC	1
#define WW_INCHILD	2
#define WW_DEAD		3

#ifndef CTRL
#define CTRL(c)	('c'&0x1f)
#endif

struct ww *wwopen();

extern struct ww *wwhead, *curwin;
extern struct ww_tty wwoldtty, wwnewtty;
extern int wwnwrite;

#define wwputchar(c)	wwputc((c), curwin)
#define wwputstr(s)	wwputs((s), curwin)
