/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	"hunt.h"
# include	<signal.h>
# include	<errno.h>
# include	<sys/ioctl.h>
# include	<sys/time.h>

# ifndef pdp11
# define	RN	(((Seed = Seed * 11109 + 13849) >> 16) & 0xffff)
# else
# define	RN	((Seed = Seed * 11109 + 13849) & 0x7fff)
# endif

int	Seed = 0;


SOCKET	Daemon;
char	*First_arg;		/* pointer to argv[0] */
char	*Last_arg;		/* pointer to end of argv/environ */
# ifdef	INTERNET
int	Test_socket;		/* test socket to answer datagrams */
FLAG	inetd_spawned;		/* invoked via inetd */
FLAG	standard_port = TRUE;	/* true if listening on standard port */
u_short	sock_port;		/* port # of tcp listen socket */
u_short	stat_port;		/* port # of statistics tcp socket */
# define	DAEMON_SIZE	(sizeof Daemon)
# else
# define	DAEMON_SIZE	(sizeof Daemon - 1)
# endif

extern SIGNAL_TYPE	cleanup();

/*
 * main:
 *	The main program.
 */
main(ac, av, ep)
int	ac;
char	**av, **ep;
{
	register PLAYER	*pp;
	register int	had_char;
# ifdef INTERNET
	register long	test_mask;
	u_short		msg;
	short		port_num, reply;
	int		namelen;
	SOCKET		test;
# endif
	static long	read_fds;
	static FLAG	first = TRUE;
	static FLAG	server = FALSE;
	extern int	optind;
	extern char	*optarg;
	int		c;
	static struct timeval	linger = {	90, 0	};

	First_arg = av[0];
	if (ep == NULL || *ep == NULL)
		ep = av + ac;
	while (*ep)
		ep++;
	Last_arg = ep[-1] + strlen(ep[-1]);

	while ((c = getopt(ac, av, "sp:")) != EOF) {
		switch (c) {
		  case 's':
			server = TRUE;
			break;
# ifdef INTERNET
		  case 'p':
			standard_port = FALSE;
			Test_port = atoi(optarg);
			break;
# endif
		  default:
erred:
			fprintf(stderr, "Usage: %s [-s] [-p port]\n", av[0]);
			exit(1);
		}
	}
	if (optind < ac)
		goto erred;

	init();
	Sock_mask = (1 << Socket);
	Stat_mask = (1 << Status);
# ifdef INTERNET
	test_mask = (1 << Test_socket);
# endif


again:
	do {
		read_fds = Fds_mask;
		errno = 0;
		while (select(Num_fds, &read_fds, (int *) NULL,
		    (int *) NULL, (struct timeval *) NULL) < 0)
		{
			if (errno != EINTR)
# ifdef LOG
				syslog(LOG_WARNING, "select: %m");
# else
				perror("select");
# endif
			errno = 0;
		}
		Have_inp = read_fds;
# ifdef INTERNET
		if (read_fds & test_mask) {
			namelen = DAEMON_SIZE;
			port_num = htons(sock_port);
			(void) recvfrom(Test_socket, (char *) &msg, sizeof msg,
				0, (struct sockaddr *) &test, &namelen);
			switch (ntohs(msg)) {
			  case C_MESSAGE:
				if (Nplayer <= 0)
					break;
				reply = htons((u_short) Nplayer);
				(void) sendto(Test_socket, (char *) &reply,
					sizeof reply, 0,
					(struct sockaddr *) &test, DAEMON_SIZE);
				break;
			  case C_SCORES:
				reply = htons(stat_port);
				(void) sendto(Test_socket, (char *) &reply,
					sizeof reply, 0,
					(struct sockaddr *) &test, DAEMON_SIZE);
				break;
			  case C_PLAYER:
			  case C_MONITOR:
				if (msg == C_MONITOR && Nplayer <= 0)
					break;
				reply = htons(sock_port);
				(void) sendto(Test_socket, (char *) &reply,
					sizeof reply, 0,
					(struct sockaddr *) &test, DAEMON_SIZE);
				break;
			}
		}
# endif
		for (;;) {
			had_char = FALSE;
			for (pp = Player; pp < End_player; pp++)
				if (havechar(pp)) {
					execute(pp);
					pp->p_nexec++;
					had_char++;
				}
# ifdef MONITOR
			for (pp = Monitor; pp < End_monitor; pp++)
				if (havechar(pp)) {
					mon_execute(pp);
					pp->p_nexec++;
					had_char++;
				}
# endif
			if (!had_char)
				break;
			moveshots();
			for (pp = Player; pp < End_player; )
				if (pp->p_death[0] != '\0')
					zap(pp, TRUE);
				else
					pp++;
# ifdef MONITOR
			for (pp = Monitor; pp < End_monitor; )
				if (pp->p_death[0] != '\0')
					zap(pp, FALSE);
				else
					pp++;
# endif
		}
		if (read_fds & Sock_mask)
			if (answer()) {
# ifdef INTERNET
				if (first && standard_port)
					faketalk();
# endif
				first = FALSE;
			}
		if (read_fds & Stat_mask)
			send_stats();
		for (pp = Player; pp < End_player; pp++) {
			if (read_fds & pp->p_mask)
				sendcom(pp, READY, pp->p_nexec);
			pp->p_nexec = 0;
			(void) fflush(pp->p_output);
		}
# ifdef MONITOR
		for (pp = Monitor; pp < End_monitor; pp++) {
			if (read_fds & pp->p_mask)
				sendcom(pp, READY, pp->p_nexec);
			pp->p_nexec = 0;
			(void) fflush(pp->p_output);
		}
# endif
	} while (Nplayer > 0);

	read_fds = Fds_mask;
	if (select(Num_fds, &read_fds, (int *) NULL, (int *) NULL,
		   &linger) > 0) {
		goto again;
	}
	if (server) {
		clear_scores();
		makemaze();
		clearwalls();
# ifdef BOOTS
		makeboots();
# endif
		first = TRUE;
		goto again;
	}

# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; )
		zap(pp, FALSE);
# endif
	cleanup(0);
}

/*
 * init:
 *	Initialize the global parameters.
 */
init()
{
	register int	i;
# ifdef	INTERNET
	auto SOCKET	test_port;
	auto int	msg;
	auto int	len;
# endif

# ifndef DEBUG
# ifdef TIOCNOTTY
	(void) ioctl(fileno(stdout), TIOCNOTTY, NULL);
# endif
	(void) setpgrp(getpid(), getpid());
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTERM, cleanup);
# endif

	(void) chdir("/usr/tmp");	/* just in case it core dumps */
	(void) umask(0);		/* No privacy at all! */
	(void) signal(SIGPIPE, SIG_IGN);

# ifdef LOG
# ifdef	SYSLOG_43
	openlog("HUNT", LOG_PID, LOG_DAEMON);
# endif
# ifdef	SYSLOG_42
	openlog("HUNT", LOG_PID);
# endif
# endif

	/*
	 * Initialize statistics socket
	 */
# ifdef	INTERNET
	Daemon.sin_family = SOCK_FAMILY;
	Daemon.sin_addr.s_addr = INADDR_ANY;
	Daemon.sin_port = 0;
# else
	Daemon.sun_family = SOCK_FAMILY;
	(void) strcpy(Daemon.sun_path, Stat_name);
# endif

	Status = socket(SOCK_FAMILY, SOCK_STREAM, 0);
	if (bind(Status, (struct sockaddr *) &Daemon, DAEMON_SIZE) < 0) {
		if (errno == EADDRINUSE)
			exit(0);
		else {
# ifdef LOG
			syslog(LOG_ERR, "bind: %m");
# else
			perror("bind");
# endif
			cleanup(1);
		}
	}
	(void) listen(Status, 5);

# ifdef INTERNET
	len = sizeof (SOCKET);
	if (getsockname(Status, (struct sockaddr *) &Daemon, &len) < 0)  {
# ifdef LOG
		syslog(LOG_ERR, "getsockname: %m");
# else
		perror("getsockname");
# endif
		exit(1);
	}
	stat_port = ntohs(Daemon.sin_port);
# endif

	/*
	 * Initialize main socket
	 */
# ifdef	INTERNET
	Daemon.sin_family = SOCK_FAMILY;
	Daemon.sin_addr.s_addr = INADDR_ANY;
	Daemon.sin_port = 0;
# else
	Daemon.sun_family = SOCK_FAMILY;
	(void) strcpy(Daemon.sun_path, Sock_name);
# endif

	Socket = socket(SOCK_FAMILY, SOCK_STREAM, 0);
# if defined(INTERNET)
	msg = 1;
	if (setsockopt(Socket, SOL_SOCKET, SO_USELOOPBACK, &msg, sizeof msg)<0)
# ifdef LOG
		syslog(LOG_WARNING, "setsockopt loopback %m");
# else
		perror("setsockopt loopback");
# endif
# endif
	if (bind(Socket, (struct sockaddr *) &Daemon, DAEMON_SIZE) < 0) {
		if (errno == EADDRINUSE)
			exit(0);
		else {
# ifdef LOG
			syslog(LOG_ERR, "bind: %m");
# else
			perror("bind");
# endif
			cleanup(1);
		}
	}
	(void) listen(Socket, 5);

# ifdef INTERNET
	len = sizeof (SOCKET);
	if (getsockname(Socket, (struct sockaddr *) &Daemon, &len) < 0)  {
# ifdef LOG
		syslog(LOG_ERR, "getsockname: %m");
# else
		perror("getsockname");
# endif
		exit(1);
	}
	sock_port = ntohs(Daemon.sin_port);
# endif

	/*
	 * Initialize minimal select mask
	 */
	Fds_mask = (1 << Socket) | (1 << Status);
	Num_fds = ((Socket > Status) ? Socket : Status) + 1;

# ifdef INTERNET
	len = sizeof (SOCKET);
	if (getsockname(0, (struct sockaddr *) &test_port, &len) >= 0
	&& test_port.sin_family == AF_INET) {
		inetd_spawned = TRUE;
		Test_socket = 0;
		if (test_port.sin_port != htons((u_short) Test_port)) {
			standard_port = FALSE;
			Test_port = ntohs(test_port.sin_port);
		}
	} else {
		test_port = Daemon;
		test_port.sin_port = htons((u_short) Test_port);

		Test_socket = socket(SOCK_FAMILY, SOCK_DGRAM, 0);
		if (bind(Test_socket, (struct sockaddr *) &test_port,
		    DAEMON_SIZE) < 0) {
# ifdef LOG
			syslog(LOG_ERR, "bind: %m");
# else
			perror("bind");
# endif
			exit(1);
		}
		(void) listen(Test_socket, 5);
	}

	Fds_mask |= (1 << Test_socket);
	if (Test_socket + 1 > Num_fds)
		Num_fds = Test_socket + 1;
# endif

	Seed = getpid() + time((time_t *) NULL);
	makemaze();
# ifdef BOOTS
	makeboots();
# endif

	for (i = 0; i < NASCII; i++)
		See_over[i] = TRUE;
	See_over[DOOR] = FALSE;
	See_over[WALL1] = FALSE;
	See_over[WALL2] = FALSE;
	See_over[WALL3] = FALSE;
# ifdef REFLECT
	See_over[WALL4] = FALSE;
	See_over[WALL5] = FALSE;
# endif

}

# ifdef BOOTS
/*
 * makeboots:
 *	Put the boots in the maze
 */
makeboots()
{
	register int	x, y;
	register PLAYER	*pp;

	do {
		x = rand_num(WIDTH - 1) + 1;
		y = rand_num(HEIGHT - 1) + 1;
	} while (Maze[y][x] != SPACE);
	Maze[y][x] = BOOT_PAIR;
	for (pp = Boot; pp < &Boot[NBOOTS]; pp++)
		pp->p_flying = -1;
}
# endif


/*
 * checkdam:
 *	Check the damage to the given player, and see if s/he is killed
 */
checkdam(ouch, gotcha, credit, amt, shot_type)
register PLAYER	*ouch, *gotcha;
register IDENT	*credit;
int		amt;
char		shot_type;
{
	register char	*cp;

	if (ouch->p_death[0] != '\0')
		return;
# ifdef BOOTS
	if (shot_type == SLIME)
		switch (ouch->p_nboots) {
		  default:
			break;
		  case 1:
			amt = (amt + 1) / 2;
			break;
		  case 2:
			if (gotcha != NULL)
				message(gotcha, "He has boots on!");
			return;
		}
# endif
	ouch->p_damage += amt;
	if (ouch->p_damage <= ouch->p_damcap) {
		(void) sprintf(Buf, "%2d", ouch->p_damage);
		cgoto(ouch, STAT_DAM_ROW, STAT_VALUE_COL);
		outstr(ouch, Buf, 2);
		return;
	}

	/* Someone DIED */
	switch (shot_type) {
	  default:
		cp = "Killed";
		break;
# ifdef FLY
	  case FALL:
		cp = "Killed on impact";
		break;
# endif
	  case KNIFE:
		cp = "Stabbed to death";
		ouch->p_ammo = 0;		/* No exploding */
		break;
	  case SHOT:
		cp = "Shot to death";
		break;
	  case GRENADE:
	  case SATCHEL:
	  case BOMB:
		cp = "Bombed";
		break;
	  case MINE:
	  case GMINE:
		cp = "Blown apart";
		break;
# ifdef	OOZE
	  case SLIME:
		cp = "Slimed";
		if (credit != NULL)
			credit->i_slime++;
		break;
# endif
# ifdef	VOLCANO
	  case LAVA:
		cp = "Baked";
		break;
# endif
# ifdef DRONE
	  case DSHOT:
		cp = "Eliminated";
		break;
# endif
	}
	if (credit == NULL) {
		(void) sprintf(ouch->p_death, "| %s by %s |", cp,
			(shot_type == MINE || shot_type == GMINE) ?
			"a mine" : "act of God");
		return;
	}

	(void) sprintf(ouch->p_death, "| %s by %s |", cp, credit->i_name);

	if (ouch == gotcha) {		/* No use killing yourself */
		credit->i_kills--;
		credit->i_bkills++;
	}
	else if (ouch->p_ident->i_team == ' '
	|| ouch->p_ident->i_team != credit->i_team) {
		credit->i_kills++;
		credit->i_gkills++;
	}
	else {
		credit->i_kills--;
		credit->i_bkills++;
	}
	credit->i_score = credit->i_kills / (double) credit->i_entries;
	ouch->p_ident->i_deaths++;
	if (ouch->p_nchar == 0)
		ouch->p_ident->i_stillb++;
	if (gotcha == NULL)
		return;
	gotcha->p_damcap += STABDAM;
	gotcha->p_damage -= STABDAM;
	if (gotcha->p_damage < 0)
		gotcha->p_damage = 0;
	(void) sprintf(Buf, "%2d/%2d", gotcha->p_damage, gotcha->p_damcap);
	cgoto(gotcha, STAT_DAM_ROW, STAT_VALUE_COL);
	outstr(gotcha, Buf, 5);
	(void) sprintf(Buf, "%3d", (gotcha->p_damcap - MAXDAM) / 2);
	cgoto(gotcha, STAT_KILL_ROW, STAT_VALUE_COL);
	outstr(gotcha, Buf, 3);
	(void) sprintf(Buf, "%5.2f", gotcha->p_ident->i_score);
	for (ouch = Player; ouch < End_player; ouch++) {
		cgoto(ouch, STAT_PLAY_ROW + 1 + (gotcha - Player),
			STAT_NAME_COL);
		outstr(ouch, Buf, 5);
	}
# ifdef MONITOR
	for (ouch = Monitor; ouch < End_monitor; ouch++) {
		cgoto(ouch, STAT_PLAY_ROW + 1 + (gotcha - Player),
			STAT_NAME_COL);
		outstr(ouch, Buf, 5);
	}
# endif
}

/*
 * zap:
 *	Kill off a player and take him out of the game.
 */
zap(pp, was_player)
register PLAYER	*pp;
FLAG		was_player;
{
	register int	i, len;
	register BULLET	*bp;
	register PLAYER	*np;
	register int	x, y;
	int		savefd, savemask;

	if (was_player) {
		if (pp->p_undershot)
			fixshots(pp->p_y, pp->p_x, pp->p_over);
		drawplayer(pp, FALSE);
		Nplayer--;
	}

	len = strlen(pp->p_death);	/* Display the cause of death */
	x = (WIDTH - len) / 2;
	cgoto(pp, HEIGHT / 2, x);
	outstr(pp, pp->p_death, len);
	for (i = 1; i < len; i++)
		pp->p_death[i] = '-';
	pp->p_death[0] = '+';
	pp->p_death[len - 1] = '+';
	cgoto(pp, HEIGHT / 2 - 1, x);
	outstr(pp, pp->p_death, len);
	cgoto(pp, HEIGHT / 2 + 1, x);
	outstr(pp, pp->p_death, len);
	cgoto(pp, HEIGHT, 0);

	savefd = pp->p_fd;
	savemask = pp->p_mask;

# ifdef MONITOR
	if (was_player) {
# endif
		for (bp = Bullets; bp != NULL; bp = bp->b_next) {
			if (bp->b_owner == pp)
				bp->b_owner = NULL;
			if (bp->b_x == pp->p_x && bp->b_y == pp->p_y)
				bp->b_over = SPACE;
		}

		i = rand_num(pp->p_ammo);
		x = rand_num(pp->p_ammo);
		if (x > i)
			i = x;
		if (pp->p_ammo == 0)
			x = 0;
		else if (i == pp->p_ammo - 1) {
			x = pp->p_ammo;
			len = SLIME;
		}
		else {
			for (x = MAXBOMB - 1; x > 0; x--)
				if (i >= shot_req[x])
					break;
			for (y = MAXSLIME - 1; y > 0; y--)
				if (i >= slime_req[y])
					break;
			if (y >= 0 && slime_req[y] > shot_req[x]) {
				x = slime_req[y];
				len = SLIME;
			}
			else if (x != 0) {
				len = shot_type[x];
				x = shot_req[x];
			}
		}
		if (x > 0) {
			(void) add_shot(len, pp->p_y, pp->p_x, pp->p_face, x,
				(PLAYER *) NULL, TRUE, SPACE);
			(void) sprintf(Buf, "%s detonated.",
				pp->p_ident->i_name);
			for (np = Player; np < End_player; np++)
				message(np, Buf);
# ifdef MONITOR
			for (np = Monitor; np < End_monitor; np++)
				message(np, Buf);
# endif
# ifdef BOOTS
			while (pp->p_nboots-- > 0) {
				for (np = Boot; np < &Boot[NBOOTS]; np++)
					if (np->p_flying < 0)
						break;
				if (np >= &Boot[NBOOTS])
					abort(1, "Too many boots");
				np->p_undershot = FALSE;
				np->p_x = pp->p_x;
				np->p_y = pp->p_y;
				np->p_flying = rand_num(20);
				np->p_flyx = 2 * rand_num(6) - 5;
				np->p_flyy = 2 * rand_num(6) - 5;
				np->p_over = SPACE;
				np->p_face = BOOT;
				showexpl(np->p_y, np->p_x, BOOT);
			}
# endif
		}
# ifdef BOOTS
		else if (pp->p_nboots > 0) {
			if (pp->p_nboots == 2)
				Maze[pp->p_y][pp->p_x] = BOOT_PAIR;
			else
				Maze[pp->p_y][pp->p_x] = BOOT;
			if (pp->p_undershot)
				fixshots(pp->p_y, pp->p_x,
					Maze[pp->p_y][pp->p_x]);
		}
# endif

# ifdef VOLCANO
		volcano += pp->p_ammo - x;
		if (rand_num(100) < volcano / 50) {
			do {
				x = rand_num(WIDTH / 2) + WIDTH / 4;
				y = rand_num(HEIGHT / 2) + HEIGHT / 4;
			} while (Maze[y][x] != SPACE);
			(void) add_shot(LAVA, y, x, LEFTS, volcano,
				(PLAYER *) NULL, TRUE, SPACE);
			for (np = Player; np < End_player; np++)
				message(np, "Volcano eruption.");
			volcano = 0;
		}
# endif

# ifdef	DRONE
		if (rand_num(100) < 2) {
			do {
				x = rand_num(WIDTH / 2) + WIDTH / 4;
				y = rand_num(HEIGHT / 2) + HEIGHT / 4;
			} while (Maze[y][x] != SPACE);
			add_shot(DSHOT, y, x, rand_dir(),
				shot_req[MINDSHOT +
				rand_num(MAXBOMB - MINDSHOT)],
				(PLAYER *) NULL, FALSE, SPACE);
		}
# endif

		sendcom(pp, ENDWIN);
		(void) putc(' ', pp->p_output);
		(void) fclose(pp->p_output);

		End_player--;
		if (pp != End_player) {
			memcpy(pp, End_player, sizeof (PLAYER));
			(void) sprintf(Buf, "%5.2f%c%-10.10s %c",
				pp->p_ident->i_score, stat_char(pp),
				pp->p_ident->i_name, pp->p_ident->i_team);
			i = STAT_PLAY_ROW + 1 + (pp - Player);
			for (np = Player; np < End_player; np++) {
				cgoto(np, i, STAT_NAME_COL);
				outstr(np, Buf, STAT_NAME_LEN);
			}
# ifdef MONITOR
			for (np = Monitor; np < End_monitor; np++) {
				cgoto(np, i, STAT_NAME_COL);
				outstr(np, Buf, STAT_NAME_LEN);
			}
# endif
		}

		/* Erase the last player */
		i = STAT_PLAY_ROW + 1 + Nplayer;
		for (np = Player; np < End_player; np++) {
			cgoto(np, i, STAT_NAME_COL);
			ce(np);
		}
# ifdef MONITOR
		for (np = Monitor; np < End_monitor; np++) {
			cgoto(np, i, STAT_NAME_COL);
			ce(np);
		}
	}
	else {
		sendcom(pp, ENDWIN);
		(void) putc(LAST_PLAYER, pp->p_output);
		(void) fclose(pp->p_output);

		End_monitor--;
		if (pp != End_monitor) {
			memcpy(pp, End_monitor, sizeof (PLAYER));
			(void) sprintf(Buf, "%5.5s %-10.10s %c", " ",
				pp->p_ident->i_name, pp->p_ident->i_team);
			i = STAT_MON_ROW + 1 + (pp - Player);
			for (np = Player; np < End_player; np++) {
				cgoto(np, i, STAT_NAME_COL);
				outstr(np, Buf, STAT_NAME_LEN);
			}
			for (np = Monitor; np < End_monitor; np++) {
				cgoto(np, i, STAT_NAME_COL);
				outstr(np, Buf, STAT_NAME_LEN);
			}
		}

		/* Erase the last monitor */
		i = STAT_MON_ROW + 1 + (End_monitor - Monitor);
		for (np = Player; np < End_player; np++) {
			cgoto(np, i, STAT_NAME_COL);
			ce(np);
		}
		for (np = Monitor; np < End_monitor; np++) {
			cgoto(np, i, STAT_NAME_COL);
			ce(np);
		}

	}
# endif

	Fds_mask &= ~savemask;
	if (Num_fds == savefd + 1) {
		Num_fds = Socket;
# ifdef INTERNET
		if (Test_socket > Socket)
			Num_fds = Test_socket;
# endif
		for (np = Player; np < End_player; np++)
			if (np->p_fd > Num_fds)
				Num_fds = np->p_fd;
# ifdef MONITOR
		for (np = Monitor; np < End_monitor; np++)
			if (np->p_fd > Num_fds)
				Num_fds = np->p_fd;
# endif
		Num_fds++;
	}
}

/*
 * rand_num:
 *	Return a random number in a given range.
 */
rand_num(range)
int	range;
{
	return (range == 0 ? 0 : RN % range);
}

/*
 * havechar:
 *	Check to see if we have any characters in the input queue; if
 *	we do, read them, stash them away, and return TRUE; else return
 *	FALSE.
 */
havechar(pp)
register PLAYER	*pp;
{
	extern int	errno;

	if (pp->p_ncount < pp->p_nchar)
		return TRUE;
	if (!(Have_inp & pp->p_mask))
		return FALSE;
	Have_inp &= ~pp->p_mask;
check_again:
	errno = 0;
	if ((pp->p_nchar = read(pp->p_fd, pp->p_cbuf, sizeof pp->p_cbuf)) <= 0)
	{
		if (errno == EINTR)
			goto check_again;
		pp->p_cbuf[0] = 'q';
	}
	pp->p_ncount = 0;
	return TRUE;
}

/*
 * cleanup:
 *	Exit with the given value, cleaning up any droppings lying around
 */
SIGNAL_TYPE
cleanup(eval)
int	eval;
{
	register PLAYER	*pp;

	for (pp = Player; pp < End_player; pp++) {
		cgoto(pp, HEIGHT, 0);
		sendcom(pp, ENDWIN);
		(void) putc(LAST_PLAYER, pp->p_output);
		(void) fclose(pp->p_output);
	}
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++) {
		cgoto(pp, HEIGHT, 0);
		sendcom(pp, ENDWIN);
		(void) putc(LAST_PLAYER, pp->p_output);
		(void) fclose(pp->p_output);
	}
# endif
	(void) close(Socket);
# ifdef AF_UNIX_HACK
	(void) unlink(Sock_name);
# endif

	exit(eval);
}

/*
 * send_stats:
 *	Print stats to requestor
 */
send_stats()
{
	register IDENT	*ip;
	register FILE	*fp;
	int		s;
	SOCKET		sockstruct;
	int		socklen;

	/*
	 * Get the output stream ready
	 */
# ifdef INTERNET
	socklen = sizeof sockstruct;
# else
	socklen = sizeof sockstruct - 1;
# endif
	s = accept(Status, (struct sockaddr *) &sockstruct, &socklen);
	if (s < 0) {
		if (errno == EINTR)
			return;
# ifdef LOG
		syslog(LOG_ERR, "accept: %m");
# else
		perror("accept");
# endif
		return;
	}
	fp = fdopen(s, "w");
	if (fp == NULL) {
# ifdef LOG
		syslog(LOG_ERR, "fdopen: %m");
# else
		perror("fdopen");
# endif
		(void) close(s);
		return;
	}

	/*
	 * Send output to requestor
	 */
	fputs("Name\t\tScore\tDucked\tAbsorb\tFaced\tShot\tRobbed\tMissed\tSlimeK\n", fp);
	for (ip = Scores; ip != NULL; ip = ip->i_next) {
		fprintf(fp, "%s\t", ip->i_name);
		if (strlen(ip->i_name) < 8)
			putc('\t', fp);
		fprintf(fp, "%.2f\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
			ip->i_score, ip->i_ducked, ip->i_absorbed,
			ip->i_faced, ip->i_shot, ip->i_robbed,
			ip->i_missed, ip->i_slime);
	}
	fputs("\n\nName\t\tEnemy\tFriend\tDeaths\tStill\tSaved\n", fp);
	for (ip = Scores; ip != NULL; ip = ip->i_next) {
		if (ip->i_team == ' ') {
			fprintf(fp, "%s\t", ip->i_name);
			if (strlen(ip->i_name) < 8)
				putc('\t', fp);
		}
		else {
			fprintf(fp, "%s[%c]\t", ip->i_name, ip->i_team);
			if (strlen(ip->i_name) + 3 < 8)
				putc('\t', fp);
		}
		fprintf(fp, "%d\t%d\t%d\t%d\t%d\n",
			ip->i_gkills, ip->i_bkills, ip->i_deaths,
			ip->i_stillb, ip->i_saved);
	}

	(void) fclose(fp);
}

/*
 * clear_scores:
 *	Clear out the scores so the next session start clean
 */
clear_scores()
{
	register IDENT	*ip, *nextip;

	for (ip = Scores; ip != NULL; ip = nextip) {
		nextip = ip->i_next;
		(void) free((char *) ip);
	}
	Scores = NULL;
}
