/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)talk.h	5.1 (Berkeley) 6/6/85
 */

#include <sys/types.h>
#include <curses.h>
#include <utmp.h>

#define forever		for(;;)

#define BUF_SIZE	512

FILE	*popen();
int	quit();
int	sleeper();

extern	int sockt;
extern	int curses_initialized;
extern	int invitation_waiting;

extern	char *current_state;
extern	int current_line;

typedef struct xwin {
	WINDOW	*x_win;
	int	x_nlines;
	int	x_ncols;
	int	x_line;
	int	x_col;
	char	kill;
	char	cerase;
	char	werase;
} xwin_t;

extern	xwin_t my_win;
extern	xwin_t his_win;
extern	WINDOW *line_win;
