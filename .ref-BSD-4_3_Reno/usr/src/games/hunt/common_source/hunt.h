/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)hunt.h	5.2 (Berkeley) 6/27/88
 */

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	<stdio.h>
# ifndef OLDIPC
# include	<sgtty.h>
# include	<sys/types.h>
# include	<sys/uio.h>
# else OLDIPC
# include	<sys/localopts.h>
# include	<sys/types.h>
# include	<sys/netltoshort.h>
# endif OLDIPC
# include	<sys/socket.h>
# ifdef	INTERNET
# include	<netinet/in.h>
# include	<netdb.h>
# ifndef OLDIPC
# include	<arpa/inet.h>
# endif !OLDIPC
# ifdef BROADCAST
# include	<net/if.h>
# endif BROADCAST
# else	INTERNET
# include	<sys/un.h>
# endif	INTERNET

# ifdef	INTERNET
# define	SOCK_FAMILY	AF_INET
# else	INTERNET
# define	SOCK_FAMILY	AF_UNIX
# define	AF_UNIX_HACK		/* 4.2 hack; leaves files around */
# endif	INTERNET

# define	ADDCH		('a' | 0200)
# define	MOVE		('m' | 0200)
# define	REFRESH		('r' | 0200)
# define	CLRTOEOL	('c' | 0200)
# define	ENDWIN		('e' | 0200)
# define	CLEAR		('C' | 0200)
# define	REDRAW		('R' | 0200)
# define	LAST_PLAYER	('l' | 0200)
# define	BELL		('b' | 0200)
# define	READY		('g' | 0200)

/*
 * Choose MAXPL and MAXMON carefully.  The screen is assumed to be
 * 23 lines high and will only tolerate (MAXPL == 12 && MAXMON == 0)
 * or (MAXPL + MAXMON <= 10).
 */
# define	MAXPL		9
# ifdef MONITOR
# define	MAXMON		1
# endif MONITOR
# define	NAMELEN		20
# define	MSGLEN		80
# define	DECAY		50.0

# define	NASCII		128

# ifndef REFLECT
# ifndef RANDOM
# define RANDOM
# endif RANDOM
# endif REFLECT

# define	WIDTH	59
# define	WIDTH2	64	/* Next power of 2 >= WIDTH (for fast access) */
# define	HEIGHT	23
# define	UBOUND	1
# define	DBOUND	22
# define	LBOUND	1
# define	RBOUND	(WIDTH - 1)

# define	STAT_LABEL_COL	60
# define	STAT_VALUE_COL	74
# define	STAT_NAME_COL	61
# define	STAT_SCAN_COL	(STAT_NAME_COL + 5)
# define	STAT_NAME_ROW	0
# define	STAT_AMMO_ROW	2
# define	STAT_SCAN_ROW	3
# define	STAT_CLOAK_ROW	4
# define	STAT_GUN_ROW	5
# define	STAT_DAM_ROW	7
# define	STAT_KILL_ROW	8
# define	STAT_PLAY_ROW	10
# ifdef MONITOR
# define	STAT_MON_ROW	(STAT_PLAY_ROW + MAXPL + 1)
# endif MONITOR
# define	STAT_NAME_LEN	16

# define	DOOR	'#'
# define	WALL1	'-'
# define	WALL2	'|'
# define	WALL3	'+'
# ifdef REFLECT
# define	WALL4	'/'
# define	WALL5	'\\'
# endif REFLECT
# define	KNIFE	'K'
# define	SHOT	':'
# define	GRENADE	'o'
# define	SATCHEL	'O'
# define	BOMB	'@'
# define	MINE	';'
# define	GMINE	'g'
# ifdef	OOZE
# define	SLIME	'$'
# endif	OOZE
# ifdef	VOLCANO
# define	LAVA	'~'
# endif	VOLCANO
# ifdef FLY
# define	FALL	'F'
# endif FLY
# define	SPACE	' '

# define	ABOVE	'i'
# define	BELOW	'!'
# define	RIGHT	'}'
# define	LEFTS	'{'
# ifdef FLY
# define	FLYER	'&'
# endif FLY

# define	NORTH	01
# define	SOUTH	02
# define	EAST	010
# define	WEST	020

# ifndef TRUE
# define	TRUE	1
# define	FALSE	0
# endif TRUE
# ifndef CTRL
# define	CTRL(x)	('x' & 037)
# endif CTRL

# define	BULSPD		5		/* bullets movement speed */
# define	ISHOTS		15
# define	NSHOTS		5
# define	MAXNCSHOT	2
# define	MAXDAM		10
# define	MINDAM		5
# define	STABDAM		2

# define	BULREQ		1
# define	GRENREQ		9
# define	SATREQ		25
# define	BOMBREQ		49
# ifdef	OOZE
# define	SLIMEREQ	15
# define	SSLIMEREQ	30
# define	SLIMESPEED	5
# endif	OOZE
# ifdef	VOLCANO
# define	LAVASPEED	2
# endif VOLCANO

# define	CLOAKLEN	20
# define	SCANLEN		(Nplayer * 20)
# define	EXPLEN		4

# ifdef FLY
# define	_cloak_char(pp)	(((pp)->p_cloak < 0) ? ' ' : '+')
# define	_scan_char(pp)	(((pp)->p_scan < 0) ? _cloak_char(pp) : '*')
# define	stat_char(pp)	(((pp)->p_flying < 0) ? _scan_char(pp) : FLYER)
# else FLY
# define	_cloak_char(pp)	(((pp)->p_cloak < 0) ? ' ' : '+')
# define	stat_char(pp)	(((pp)->p_scan < 0) ? _cloak_char(pp) : '*')
# endif FLY

typedef int			FLAG;
typedef struct bullet_def	BULLET;
typedef struct expl_def		EXPL;
typedef struct player_def	PLAYER;
typedef struct ident_def	IDENT;
typedef struct regen_def	REGEN;
# ifdef	INTERNET
typedef struct sockaddr_in	SOCKET;
# else	INTERNET
typedef struct sockaddr_un	SOCKET;
# endif	INTERNET
typedef struct sgttyb		TTYB;

struct ident_def {
	char	i_name[NAMELEN];
	long	i_machine;
	long	i_uid;
	int	i_kills;
	int	i_entries;
	float	i_score;
	IDENT	*i_next;
};

struct player_def {
	IDENT	*p_ident;
	int	p_face;
	char	p_over;
	int	p_undershot;
# ifdef	FLY
	int	p_flying;
	int	p_flyx, p_flyy;
# endif FLY
	FILE	*p_output;
	int	p_fd;
	int	p_mask;
	int	p_damage;
	int	p_damcap;
	int	p_ammo;
	int	p_ncshot;
	int	p_scan;
	int	p_cloak;
	int	p_x, p_y;
	int	p_ncount;
	int	p_nexec;
	long	p_nchar;
	char	p_death[MSGLEN];
	char	p_maze[HEIGHT][WIDTH2];
	int	p_curx, p_cury;
	int	p_lastx, p_lasty;
	int	p_changed;
	char	p_cbuf[BUFSIZ];
};

struct bullet_def {
	int	b_x, b_y;
	int	b_face;
	int	b_charge;
	char	b_type;
	char	b_over;
	PLAYER	*b_owner;
	IDENT	*b_score;
	FLAG	b_expl;
	BULLET	*b_next;
};

struct expl_def {
	int	e_x, e_y;
	char	e_char;
	EXPL	*e_next;
};

struct regen_def {
	int	r_x, r_y;
	REGEN	*r_next;
};

/*
 * external variables
 */

extern FLAG	Last_player;

extern char	Buf[BUFSIZ], Maze[HEIGHT][WIDTH2], Orig_maze[HEIGHT][WIDTH2];

extern char	*Sock_name, *Driver;

extern int	errno, Have_inp, Nplayer, Num_fds, Socket;
extern long	Fds_mask, Sock_mask;

# ifdef INTERNET
extern int	Test_port;
extern int	Sock_port;
# else INTERNET
extern char	*Sock_name;
# endif INTERNET

# ifdef VOLCANO
extern int	volcano;
# endif	VOLCANO

extern int	See_over[NASCII];

extern BULLET	*Bullets;

extern EXPL	*Expl[EXPLEN];

extern IDENT	*Scores;

extern PLAYER	Player[MAXPL], *End_player;

# ifdef MONITOR
extern FLAG	Am_monitor;
extern PLAYER	Monitor[MAXMON], *End_monitor;
# endif MONITOR

/*
 * function types
 */

char	*getenv(), *malloc(), *strcpy(), *strncpy();

IDENT	*get_ident();

int	moveshots();

BULLET	*is_bullet(), *create_shot();

PLAYER	*play_at();
