/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	"hunt.h"

# ifdef MONITOR
FLAG	Am_monitor = FALSE;		/* current process is a monitor */
# endif MONITOR

char	Buf[BUFSIZ];			/* general scribbling buffer */
char	Maze[HEIGHT][WIDTH2];		/* the maze */
char	Orig_maze[HEIGHT][WIDTH2];	/* the original maze */

long	Fds_mask;			/* mask for the file descriptors */
int	Have_inp;			/* which file descriptors have input */
int	Nplayer = 0;			/* number of players */
int	Num_fds;			/* number of maximum file descriptor */
int	Socket;				/* main socket */
long	Sock_mask;			/* select mask for main socket */
int	See_over[NASCII];		/* lookup table for determining whether
					 * character represents "transparent"
					 * item */

BULLET	*Bullets = NULL;		/* linked list of bullets */

EXPL	*Expl[EXPLEN];			/* explosion lists */

PLAYER	Player[MAXPL];			/* all the players */
PLAYER	*End_player = Player;		/* last active player slot */
IDENT	*Scores;			/* score cache */
# ifdef MONITOR
PLAYER	Monitor[MAXMON];		/* all the monitors */
PLAYER	*End_monitor = Monitor;		/* last active monitor slot */
# endif MONITOR

# ifdef VOLCANO
int	volcano = 0;			/* Explosion size */
# endif VOLCANO
