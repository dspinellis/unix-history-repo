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

void	announce_invite __P((void));
int	check_local __P((void));
void	display __P((xwin_t *, char *, int));
void	end_msgs __P((void));
void	get_addrs __P((char *, char *));
void	get_names __P((int, char **));
void	init_display __P((void));
void	invite_remote __P((void));
void	message __P((const char *));
void	open_ctl __P((void));
void	open_sockt __P((void));
__dead void p_error __P((const char *)) __attribute__((volatile));
__dead void quit __P((void)) __attribute__((volatile));
void	send_delete __P((void));
void	set_edit_chars __P((void));
void	start_msgs __P((void));
void	talk __P((void));
