/*	@(#)ww.h	1.1 83/07/12		*/

#include <sgtty.h>
#include <curses.h>

struct ww {
	int ww_state :4;
	int ww_wstate :4;
	int ww_touched :1;
	WINDOW *ww_win;
	int ww_row;
	int ww_col;
	int ww_nrow;
	int ww_ncol;
	int ww_x;
	int ww_y;
	struct sgttyb ww_sgttyb;
	struct tchars ww_tchars;
	struct ltchars ww_ltchars;
	int ww_lmode;
	int ww_ldisc;
	int ww_pgrp;
	int ww_pty;
	int ww_tty;
	int ww_pid;
	struct ww *ww_next;
};

#define WW_INITIAL	0
#define WW_HASPROC	1
#define WW_INCHILD	2
#define WW_DEAD		3

#ifndef CTRL
#define CTRL(c)	('c'&0x1f)
#endif

struct ww *wwopen();

extern struct ww *_wwhead, *_wwcurrent;

#define wwputchar(c)	wwputc((c), _wwcurrent)
#define wwputstr(s)	wwputs((s), _wwcurrent)
