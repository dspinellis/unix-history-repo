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
 */

#ifndef lint
static char sccsid[] = "@(#)answer.c	5.2 (Berkeley) 6/27/88";
#endif /* not lint */

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	"hunt.h"
# include	<errno.h>

# define	MAXPERMACH	3	/* Max player/monitor per machine */

static char	Ttyname[NAMELEN];

answer()
{
	register PLAYER		*pp;
	register int		newsock;
	register FILE		*tmpfd;
# ifdef MONITOR
	static FLAG		monitor;
# endif MONITOR
	static char		name[NAMELEN];
	static int		socklen;
	static u_long		machine;
	static u_long		uid;
	static SOCKET		sockstruct;
# ifdef OLDIPC
	extern SOCKET		Daemon;
# endif OLDIPC

# ifdef INTERNET
	socklen = sizeof sockstruct;
# else
	socklen = sizeof sockstruct - 1;
# endif INTERNET
	errno = 0;
# ifndef OLDIPC
	if ((newsock = accept(Socket, &sockstruct, &socklen)) < 0)
# else OLDIPC
	if (accept(Socket, &sockstruct) < 0)
# endif OLDIPC
	{
		if (errno == EINTR)
			return;
		perror("accept");
		cleanup(1);
	}
# ifdef OLDIPC
	newsock = Socket;
	Socket = socket(SOCK_STREAM, 0, (struct sockaddr *) &Daemon,
		SO_ACCEPTCONN);
	if (Socket < 0) {
		perror("new accept socket");
		cleanup(1);
	}
	Sock_mask = (1 << Socket);
	Fds_mask |= Sock_mask;
	if (Socket >= Num_fds)
		Num_fds = Socket + 1;
# endif OLDIPC

	tmpfd = fdopen(newsock, "w");

# ifdef INTERNET
	machine = ntohl(((struct sockaddr_in *) &sockstruct)->sin_addr.s_addr);
# else INTERNET
	if (machine == 0)
		machine = gethostid();
# endif INTERNET
	(void) putw(getpid(), tmpfd);
	(void) read(newsock, (char *) &uid, sizeof uid);
	uid = ntohl(uid);
	(void) read(newsock, name, NAMELEN);
	(void) read(newsock, Ttyname, NAMELEN);
# ifdef MONITOR
	(void) read(newsock, (char *) &monitor, sizeof monitor);
# endif MONITOR

	if (reached_limit(machine)) {
		socklen = 0;
		(void) write(newsock, (char *) &socklen, sizeof socklen);
		(void) close(newsock);
# ifdef OLDIPC
		Fds_mask &= ~(1 << newsock);
# endif OLDIPC
		return;
	}

# ifdef MONITOR
	if (monitor)
		if (End_monitor < &Monitor[MAXMON])
			pp = End_monitor++;
		else {
			socklen = 0;
			(void) write(newsock, (char *) &socklen,
				sizeof socklen);
			(void) close(newsock);
			return;
		}
	else
# endif MONITOR
		if (End_player < &Player[MAXPL])
			pp = End_player++;
		else {
			socklen = 0;
			(void) write(newsock, (char *) &socklen,
				sizeof socklen);
			(void) close(newsock);
			return;
		}

	pp->p_ident = get_ident(machine, uid, name);
	pp->p_output = tmpfd;
	pp->p_death[0] = '\0';
	pp->p_fd = newsock;
	pp->p_mask = (1 << pp->p_fd);
# ifndef OLDIPC
	Fds_mask |= pp->p_mask;
	if (pp->p_fd >= Num_fds)
		Num_fds = pp->p_fd + 1;
# endif OLDIPC

	pp->p_y = 0;
	pp->p_x = 0;

# ifdef MONITOR
	if (monitor)
		stmonitor(pp);
	else
# endif MONITOR
		stplayer(pp);
}

# ifdef MONITOR
stmonitor(pp)
register PLAYER	*pp;
{
	register int	line;
	register PLAYER	*npp;

	bcopy((char *) Maze, (char *) pp->p_maze, sizeof Maze);

	drawmaze(pp);

	(void) sprintf(Buf, "%5.5s%c%-10.10s", " ", stat_char(pp),
		pp->p_ident->i_name);
	line = STAT_MON_ROW + 1 + (pp - Monitor);
	for (npp = Player; npp < End_player; npp++) {
		cgoto(npp, line, STAT_NAME_COL);
		outstr(npp, Buf, STAT_NAME_LEN);
	}
	for (npp = Monitor; npp < End_monitor; npp++) {
		cgoto(npp, line, STAT_NAME_COL);
		outstr(npp, Buf, STAT_NAME_LEN);
	}

	sendcom(pp, REFRESH);
	sendcom(pp, READY, 0);
	(void) fflush(pp->p_output);
}
# endif MONITOR

stplayer(newpp)
register PLAYER	*newpp;
{
	register int	x, y;
	register PLAYER	*pp;

	Nplayer++;

	for (y = 0; y < UBOUND; y++)
		for (x = 0; x < WIDTH; x++)
			newpp->p_maze[y][x] = Maze[y][x];
	for (     ; y < DBOUND; y++) {
		for (x = 0; x < LBOUND; x++)
			newpp->p_maze[y][x] = Maze[y][x];
		for (     ; x < RBOUND; x++)
			newpp->p_maze[y][x] = SPACE;
		for (     ; x < WIDTH;  x++)
			newpp->p_maze[y][x] = Maze[y][x];
	}
	for (     ; y < HEIGHT; y++)
		for (x = 0; x < WIDTH; x++)
			newpp->p_maze[y][x] = Maze[y][x];

	do {
		x = rand_num(WIDTH - 1) + 1;
		y = rand_num(HEIGHT - 1) + 1;
	} while (Maze[y][x] != SPACE);
	newpp->p_over = SPACE;
	newpp->p_x = x;
	newpp->p_y = y;
	newpp->p_undershot = FALSE;

# ifdef	START_FLYING
	/* This is only for debugging */
	newpp->p_flying = rand_num(20);
	newpp->p_flyx = 2 * rand_num(6) - 5;
	newpp->p_flyy = 2 * rand_num(6) - 5;
	newpp->p_face = FLYER;
# else START_FLYING
	newpp->p_flying = -1;
	rand_face(newpp);
# endif START_FLYING
	newpp->p_damage = 0;
	newpp->p_damcap = MAXDAM;
	newpp->p_nchar = 0;
	newpp->p_ncount = 0;
	newpp->p_nexec = 0;
	newpp->p_ammo = ISHOTS;
	newpp->p_scan = -1;
	newpp->p_cloak = CLOAKLEN;
	newpp->p_ncshot = 0;

	do {
		x = rand_num(WIDTH - 1) + 1;
		y = rand_num(HEIGHT - 1) + 1;
	} while (Maze[y][x] != SPACE);
	Maze[y][x] = GMINE;
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++)
		check(pp, y, x);
# endif MONITOR

	do {
		x = rand_num(WIDTH - 1) + 1;
		y = rand_num(HEIGHT - 1) + 1;
	} while (Maze[y][x] != SPACE);
	Maze[y][x] = MINE;
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++)
		check(pp, y, x);
# endif MONITOR

	(void) sprintf(Buf, "%5.2f%c%-10.10s", newpp->p_ident->i_score,
		stat_char(newpp), newpp->p_ident->i_name);
	y = STAT_PLAY_ROW + 1 + (newpp - Player);
	for (pp = Player; pp < End_player; pp++) {
		if (pp != newpp) {
			char	smallbuf[10];

			pp->p_ammo += NSHOTS;
			newpp->p_ammo += NSHOTS;
			cgoto(pp, y, STAT_NAME_COL);
			outstr(pp, Buf, STAT_NAME_LEN);
			(void) sprintf(smallbuf, "%3d", pp->p_ammo);
			cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
			outstr(pp, smallbuf, 3);
		}
	}
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++) {
		cgoto(pp, y, STAT_NAME_COL);
		outstr(pp, Buf, STAT_NAME_LEN);
	}
# endif MONITOR

	drawmaze(newpp);
	drawplayer(newpp, TRUE);
	look(newpp);
# ifdef START_FLYING
	/* Make sure that the position you enter in will be erased */
	showexpl(newpp->p_y, newpp->p_x, FLYER);
# endif START_FLYING
	sendcom(newpp, REFRESH);
	sendcom(newpp, READY, 0);
	(void) fflush(newpp->p_output);
}

/*
 * rand_face:
 *	Give the player a random facing direction
 */
rand_face(pp)
register PLAYER	*pp;
{
	switch (rand_num(4)) {
	  case 0:
		pp->p_face = LEFTS;
		break;
	  case 1:
		pp->p_face = RIGHT;
		break;
	  case 2:
		pp->p_face = BELOW;
		break;
	  case 3:
		pp->p_face = ABOVE;
		break;
	}
}

/*
 * get_ident:
 *	Get the score structure of a player
 */
IDENT *
get_ident(machine, uid, name)
u_long	machine;
u_long	uid;
char	*name;
{
	register IDENT	*ip;
	static IDENT	punt;

	for (ip = Scores; ip != NULL; ip = ip->i_next)
		if (ip->i_machine == machine && ip->i_uid == uid &&
		    strncmp(ip->i_name, name, NAMELEN) == 0)
			break;

	if (ip != NULL) {
		ip->i_entries++;
		ip->i_score = ip->i_kills / (double) ip->i_entries;
	}
	else {
		ip = (IDENT *) malloc(sizeof (IDENT));
		if (ip == NULL) {
			/* Fourth down, time to punt */
			ip = &punt;
		}
		ip->i_machine = machine;
		ip->i_uid = uid;
		strncpy(ip->i_name, name, NAMELEN);
		ip->i_kills = 0;
		ip->i_entries = 1;
		ip->i_score = 0;
		ip->i_next = Scores;
		Scores = ip;
	}

	return ip;
}

/*
 * reached_limit:
 *	Returns whether the limit of x persons per machine has been reached
 */
reached_limit(machine)
u_long	machine;
{
	register PLAYER	*pp;
	register int	count;

	count = 0;
	for (pp = Player; pp < End_player; pp++)
		if (pp->p_ident->i_machine == machine)
			count++;
	for (pp = Monitor; pp < End_monitor; pp++)
		if (pp->p_ident->i_machine == machine)
			count++;
	return count >= MAXPERMACH;
}
