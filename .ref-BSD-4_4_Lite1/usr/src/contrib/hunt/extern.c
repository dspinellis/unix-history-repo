/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	"hunt.h"

# ifdef MONITOR
FLAG	Am_monitor = FALSE;		/* current process is a monitor */
# endif

char	Buf[BUFSIZ];			/* general scribbling buffer */
char	Maze[HEIGHT][WIDTH2];		/* the maze */
char	Orig_maze[HEIGHT][WIDTH2];	/* the original maze */

long	Fds_mask;			/* mask for the file descriptors */
int	Have_inp;			/* which file descriptors have input */
int	Nplayer = 0;			/* number of players */
int	Num_fds;			/* number of maximum file descriptor */
int	Socket;				/* main socket */
long	Sock_mask;			/* select mask for main socket */
int	Status;				/* stat socket */
long	Stat_mask;			/* select mask for stat socket */
int	See_over[NASCII];		/* lookup table for determining whether
					 * character represents "transparent"
					 * item */

BULLET	*Bullets = NULL;		/* linked list of bullets */

EXPL	*Expl[EXPLEN];			/* explosion lists */
EXPL	*Last_expl;			/* last explosion on Expl[0] */

PLAYER	Player[MAXPL];			/* all the players */
PLAYER	*End_player = Player;		/* last active player slot */
# ifdef BOOTS
PLAYER	Boot[NBOOTS];			/* all the boots */
# endif
IDENT	*Scores;			/* score cache */
# ifdef MONITOR
PLAYER	Monitor[MAXMON];		/* all the monitors */
PLAYER	*End_monitor = Monitor;		/* last active monitor slot */
# endif

# ifdef VOLCANO
int	volcano = 0;			/* Explosion size */
# endif

int	shot_req[MAXBOMB]	= {
				BULREQ, GRENREQ, SATREQ,
				BOMB7REQ, BOMB9REQ, BOMB11REQ,
				BOMB13REQ, BOMB15REQ, BOMB17REQ,
				BOMB19REQ, BOMB21REQ,
			};
int	shot_type[MAXBOMB]	= {
				SHOT, GRENADE, SATCHEL,
				BOMB, BOMB, BOMB,
				BOMB, BOMB, BOMB,
				BOMB, BOMB,
			};

int	slime_req[MAXSLIME]	= {
				SLIMEREQ, SSLIMEREQ, SLIME2REQ, SLIME3REQ,
			};
