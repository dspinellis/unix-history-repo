/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	<errno.h>
# include	<curses.h>
# include	"hunt.h"
# include	<signal.h>
# include	<ctype.h>
# include	<sys/stat.h>
# ifndef HPUX
# include	<sys/time.h>
# else
# include	<time.h>
# endif
# ifdef TERMINFO
# include	<term.h>

# define	CM		cursor_address

# else

/*
 * Some old versions of curses don't have these defined
 */
# ifndef cbreak
# define	cbreak()	crmode()
# endif
# endif

/*
 *	these numbers are contrived to allow 3 users on a VAX 11/750
 *	i.e. an spin loop of 10000 iterations in 30 milliseconds.
 */
# define	LOOP_COUNT	10000
# define	FUDGE_FACTOR	30

FLAG	Last_player = FALSE;
# ifdef MONITOR
FLAG	Am_monitor = FALSE;
# endif MONITOR

char	Buf[BUFSIZ];

int	Socket;
# ifdef INTERNET
char	*Sock_host;
char	*use_port;
FLAG	Query_driver = FALSE;
char	*Send_message = NULL;
FLAG	Show_scores = FALSE;
# endif INTERNET

SOCKET	Daemon;
# ifdef	INTERNET
# define	DAEMON_SIZE	(sizeof Daemon)
# else	INTERNET
# define	DAEMON_SIZE	(sizeof Daemon - 1)
# endif	INTERNET

char	map_key[256];			/* what to map keys to */
FLAG	no_beep;

static char	name[NAMELEN];
static char	team = ' ';

static int	in_visual;

extern int	cur_row, cur_col, _putchar();
extern char	*tgoto();

# ifdef INTERNET
extern	SOCKET	*list_drivers();
# endif

/*
 * main:
 *	Main program for local process
 */
main(ac, av)
int	ac;
char	**av;
{
	char		*term;
	int		c;
	extern int	errno;
	extern int	Otto_mode;
	extern int	optind;
	extern char	*optarg;
	long		enter_status;
	int		intr(), sigterm(), sigemt(), tstp();
	long		env_init(), quit();

	enter_status = env_init((long) Q_CLOAK);
	while ((c = getopt(ac, av, "Sbcfh:l:mn:op:qst:w:")) != EOF) {
		switch (c) {

# ifdef INTERNET
		case 'S':
			Show_scores = TRUE;
			break;
# endif

		case 'l':	/* rsh compatibility */
		case 'n':
			(void) strncpy(name, optarg, NAMELEN);
			break;
		case 't':
			team = *optarg;
			if (!isdigit(team)) {
				fprintf(stderr, "Team names must be numeric\n");
				team = ' ';
			}
			break;
		case 'o':
# ifndef OTTO
			fputs("The -o flag is reserved for future use.\n",
				stderr);
			goto usage;
# else OTTO
			Otto_mode = TRUE;
			break;
# endif OTTO
# ifdef MONITOR
		case 'm':
			Am_monitor = TRUE;
			break;
# endif MONITOR
# ifdef INTERNET
		case 'q':	/* query whether hunt is running */
			Query_driver = TRUE;
			break;
		case 'w':
			Send_message = optarg;
			break;
		case 'h':
			Sock_host = optarg;
			break;
		case 'p':
			use_port = optarg;
			Test_port = atoi(use_port);
			break;
# endif INTERNET
		case 'c':
			enter_status = Q_CLOAK;
			break;
# ifdef FLY
		case 'f':
			enter_status = Q_FLY;
			break;
# endif FLY
		case 's':
			enter_status = Q_SCAN;
			break;
		case 'b':
			no_beep = !no_beep;
			break;
		default:
		usage:
# ifdef INTERNET
#  ifdef MONITOR
#   define	USAGE	"usage:\thunt [-qmcsfS] [-n name] [-t team]\n\t[-p port] [-w message] [host]\n"
#  else MONITOR
#   define	USAGE	"usage:\thunt [-qcsfS] [-n name] [-t team]\n\t[-p port] [-w message] [host]\n"
#  endif MONITOR
# else INTERNET
#  ifdef MONITOR
#   define	USAGE	"usage:\thunt [-mcsf] [-n name] [-t team]\n"
#  else MONITOR
#   define	USAGE	"usage:\thunt [-csf] [-n name] [-t team]\n"
#  endif MONITOR
# endif INTERNET
			fputs(USAGE, stderr);
# undef USAGE
			exit(1);
		}
	}
# ifdef INTERNET
	if (optind + 1 < ac)
		goto usage;
	else if (optind + 1 == ac)
		Sock_host = av[ac - 1];
# else INTERNET
	if (optind > ac)
		goto usage;
# endif INTERNET	

# ifdef INTERNET
	if (Show_scores) {
		SOCKET	*hosts;

		for (hosts = list_drivers(); hosts->sin_port != 0; hosts += 1)
			dump_scores(*hosts);
		exit(0);
	}
	if (Query_driver) {
		SOCKET	*hosts;

		for (hosts = list_drivers(); hosts->sin_port != 0; hosts += 1) {
			struct	hostent	*hp;
			int	num_players;

			hp = gethostbyaddr((char *) &hosts->sin_addr,
					sizeof hosts->sin_addr, AF_INET);
			num_players = ntohs(hosts->sin_port);
			printf("%d player%s hunting on %s!\n",
				num_players, (num_players == 1) ? "" : "s",
				hp != NULL ? hp->h_name :
				inet_ntoa(hosts->sin_addr));
		}
		exit(0);
	}
# endif INTERNET
# ifdef OTTO
	if (Otto_mode)
		(void) strncpy(name, "otto", NAMELEN);
	else
# endif OTTO
	fill_in_blanks();

	(void) fflush(stdout);
	if (!isatty(0) || (term = getenv("TERM")) == NULL) {
		fprintf(stderr, "no terminal type\n");
		exit(1);
	}
# ifdef TERMINFO
	initscr();
	(void) noecho();
	(void) cbreak();
# else
	_tty_ch = 0;
	gettmode();
	(void) setterm(term);
	(void) noecho();
	(void) cbreak();
	_puts(TI);
	_puts(VS);
# endif
	in_visual = TRUE;
	if (LINES < SCREEN_HEIGHT || COLS < SCREEN_WIDTH)
		leave(1, "Need a larger window");
	clear_the_screen();
	(void) signal(SIGINT, intr);
	(void) signal(SIGTERM, sigterm);
	(void) signal(SIGEMT, sigemt);
	(void) signal(SIGPIPE, SIG_IGN);
#ifdef	SIGTSTP
	(void) signal(SIGTSTP, tstp);
#endif

	for (;;) {
		{
			register int	loop;
			struct	timeval	start, stop;
			int		elapsed_time;

			(void) gettimeofday(&start, (struct timezone *) NULL);
			for (loop = 0; loop < LOOP_COUNT; loop++)
				continue;
			(void) gettimeofday(&stop, (struct timezone *) NULL);
			elapsed_time = (stop.tv_sec - start.tv_sec) * 1000000
						+ stop.tv_usec - start.tv_usec;
			if (elapsed_time > LOOP_COUNT * FUDGE_FACTOR)
				leave(1, "Response time too slow");
		}

# ifdef	INTERNET
		find_driver(TRUE);

		if (Daemon.sin_port == 0)
			leave(1, "Game not found, try again");

	jump_in:
		do {
			int	option;

			Socket = socket(SOCK_FAMILY, SOCK_STREAM, 0);
			if (Socket < 0) {
				perror("socket");
				exit(1);
			}
			option = 1;
			if (setsockopt(Socket, SOL_SOCKET, SO_USELOOPBACK,
			    &option, sizeof option) < 0)
				perror("setsockopt loopback");
			errno = 0;
			if (connect(Socket, (struct sockaddr *) &Daemon,
			    DAEMON_SIZE) < 0) {
				if (errno != ECONNREFUSED) {
					perror("connect");
					leave(1, "connect");
				}
			}
			else
				break;
			sleep(1);
		} while (close(Socket) == 0);
# else	INTERNET
		/*
		 * set up a socket
		 */

		if ((Socket = socket(SOCK_FAMILY, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			exit(1);
		}

		/*
		 * attempt to connect the socket to a name; if it fails that
		 * usually means that the driver isn't running, so we start
		 * up the driver.
		 */

		Daemon.sun_family = SOCK_FAMILY;
		(void) strcpy(Daemon.sun_path, Sock_name);
		if (connect(Socket, &Daemon, DAEMON_SIZE) < 0) {
			if (errno != ENOENT) {
				perror("connect");
				leave(1, "connect2");
			}
			start_driver();

			do {
				(void) close(Socket);
				if ((Socket = socket(SOCK_FAMILY, SOCK_STREAM, 0)) < 0) {
					perror("socket");
					exit(1);
				}
				sleep(2);
			} while (connect(Socket, &Daemon, DAEMON_SIZE) < 0);
		}
# endif INTERNET

		do_connect(name, team, enter_status);
# ifdef INTERNET
		if (Send_message != NULL) {
			do_message();
			if (enter_status == Q_MESSAGE)
				break;
			Send_message = NULL;
			/* don't continue as that will call find_driver */
			goto jump_in;
		}
# endif
		playit();
		if ((enter_status = quit(enter_status)) == Q_QUIT)
			break;
	}
	leave(0, (char *) NULL);
	/* NOTREACHED */
}

# ifdef INTERNET
# ifdef BROADCAST
broadcast_vec(s, vector)
	int			s;		/* socket */
	struct	sockaddr	**vector;
{
	char			if_buf[BUFSIZ];
	struct	ifconf		ifc;
	struct	ifreq		*ifr;
	unsigned int		n;
	int			vec_cnt;

	*vector = NULL;
	ifc.ifc_len = sizeof if_buf;
	ifc.ifc_buf = if_buf;
	if (ioctl(s, SIOCGIFCONF, (char *) &ifc) < 0)
		return 0;
	vec_cnt = 0;
	n = ifc.ifc_len / sizeof (struct ifreq);
	*vector = (struct sockaddr *) malloc(n * sizeof (struct sockaddr));
	for (ifr = ifc.ifc_req; n != 0; n--, ifr++)
		if (ioctl(s, SIOCGIFBRDADDR, ifr) >= 0)
			bcopy((char *) &ifr->ifr_addr,
				(char *) &(*vector)[vec_cnt++],
				sizeof (struct sockaddr));
	return vec_cnt;
}
# endif BROADCAST

SOCKET	*
list_drivers()
{
	int			option;
	u_short			msg;
	u_short			port_num;
	static SOCKET		test;
	int			test_socket;
	int			namelen;
	char			local_name[256];
	static			initial = TRUE;
	static struct in_addr	local_address;
	register struct hostent	*hp;
	extern int		errno;
# ifdef BROADCAST
	static	int		brdc;
	static	SOCKET		*brdv;
# else
	u_long			local_net;
# endif BROADCAST
	int			i;
	static	SOCKET		*listv;
	static	unsigned int	listmax;
	unsigned int		listc;
	int			mask;
	struct timeval		wait;

	if (initial) {			/* do one time initialization */
# ifndef BROADCAST
		sethostent(1);		/* don't bother to close host file */
# endif BROADCAST
		if (gethostname(local_name, sizeof local_name) < 0) {
			leave(1, "Sorry, I have no name.");
			/* NOTREACHED */
		}
		if ((hp = gethostbyname(local_name)) == NULL) {
			leave(1, "Can't find myself.");
			/* NOTREACHED */
		}
		local_address = * ((struct in_addr *) hp->h_addr);

		listmax = 20;
		listv = (SOCKET *) malloc(listmax * sizeof (SOCKET));
	} else if (Sock_host != NULL)
		return listv;		/* address already valid */

	test_socket = socket(SOCK_FAMILY, SOCK_DGRAM, 0);
	if (test_socket < 0) {
		perror("socket");
		leave(1, "socket system call failed");
		/* NOTREACHED */
	}
	test.sin_family = SOCK_FAMILY;
	test.sin_port = htons(Test_port);
	listc = 0;

	if (Sock_host != NULL) {	/* explicit host given */
		if ((hp = gethostbyname(Sock_host)) == NULL) {
			leave(1, "Unknown host");
			/* NOTREACHED */
		}
		test.sin_addr = *((struct in_addr *) hp->h_addr);
		goto test_one_host;
	}

	if (!initial) {
		/* favor host of previous session by broadcasting to it first */
		test.sin_addr = Daemon.sin_addr;
		msg = htons(C_PLAYER);		/* Must be playing! */
		(void) sendto(test_socket, (char *) &msg, sizeof msg, 0,
		    (struct sockaddr *) &test, DAEMON_SIZE);
	}

# ifdef BROADCAST
	if (initial)
		brdc = broadcast_vec(test_socket, (struct sockaddr **) &brdv);

	if (brdc <= 0) {
		initial = FALSE;
		test.sin_addr = local_address;
		goto test_one_host;
	}

# ifdef SO_BROADCAST
	/* Sun's will broadcast even though this option can't be set */
	option = 1;
	if (setsockopt(test_socket, SOL_SOCKET, SO_BROADCAST,
	    (int) &option, sizeof option) < 0) {
		perror("setsockopt broadcast");
		leave(1, "setsockopt broadcast");
		/* NOTREACHED */
	}
# endif

	/* send broadcast packets on all interfaces */
	msg = htons(C_TESTMSG());
	for (i = 0; i < brdc; i++) {
		test.sin_addr = brdv[i].sin_addr;
		if (sendto(test_socket, (char *) &msg, sizeof msg, 0,
		    (struct sockaddr *) &test, DAEMON_SIZE) < 0) {
			perror("sendto");
			leave(1, "sendto");
			/* NOTREACHED */
		}
	}
# else BROADCAST
	/* loop thru all hosts on local net and send msg to them. */
	msg = htons(C_TESTMSG());
	local_net = inet_netof(local_address);
	sethostent(0);		/* rewind host file */
	while (hp = gethostent()) {
		if (local_net == inet_netof(* ((struct in_addr *) hp->h_addr))){
			test.sin_addr = * ((struct in_addr *) hp->h_addr);
			(void) sendto(test_socket, (char *) &msg, sizeof msg, 0,
			    (struct sockaddr *) &test, DAEMON_SIZE);
		}
	}
# endif BROADCAST

get_response:
	namelen = DAEMON_SIZE;
	errno = 0;
	wait.tv_sec = 1;
	wait.tv_usec = 0;
	for (;;) {
		if (listc + 1 >= listmax) {
			listmax += 20;
			listv = (SOCKET *) realloc((char *) listv,
						listmax * sizeof(SOCKET));
		}

		mask = 1 << test_socket;
		if (select(test_socket + 1, &mask, NULL, NULL, &wait) == 1
		&& recvfrom(test_socket, (char *) &port_num, sizeof port_num,
			0, (struct sockaddr *) &listv[listc], &namelen) > 0) {
			/*
			 * Note that we do *not* convert from network to host
			 * order since the port number *should* be in network
			 * order:
			 */
			for (i = 0; i < listc; i += 1)
				if (listv[listc].sin_addr.s_addr
				== listv[i].sin_addr.s_addr)
					break;
			if (i == listc)
				listv[listc++].sin_port = port_num;
			continue;
		}

		if (errno != 0 && errno != EINTR) {
			perror("select/recvfrom");
			leave(1, "select/recvfrom");
			/* NOTREACHED */
		}

		/* terminate list with local address */
		listv[listc].sin_family = SOCK_FAMILY;
		listv[listc].sin_addr = local_address;
		listv[listc].sin_port = htons(0);

		(void) close(test_socket);
		initial = FALSE;
		return listv;
	}

test_one_host:
	msg = htons(C_TESTMSG());
	(void) sendto(test_socket, (char *) &msg, sizeof msg, 0,
	    (struct sockaddr *) &test, DAEMON_SIZE);
	goto get_response;
}

find_driver(do_startup)
FLAG	do_startup;
{
	SOCKET	*hosts;

	hosts = list_drivers();
	if (hosts[0].sin_port != htons(0)) {
		int	i, c;

		if (hosts[1].sin_port == htons(0)) {
			Daemon = hosts[0];
			return;
		}
		/* go thru list and return host that matches daemon */
		clear_the_screen();
		mvcur(cur_row, cur_col, 1, 0);
		cur_row = 1;
		cur_col = 0;
		put_str("Pick one:");
		for (i = 0; i < HEIGHT - 4 && hosts[i].sin_port != htons(0);
								i += 1) {
			struct	hostent	*hp;
			char	buf[80];

			mvcur(cur_row, cur_col, 3 + i, 0);
			cur_row = 3 + i;
			cur_col = 0;
			hp = gethostbyaddr((char *) &hosts[i].sin_addr,
					sizeof hosts[i].sin_addr, AF_INET);
			(void) sprintf(buf, "%8c    %.64s", 'a' + i,
				hp != NULL ? hp->h_name
				: inet_ntoa(hosts->sin_addr));
			put_str(buf);
		}
		mvcur(cur_row, cur_col, 4 + i, 0);
		cur_row = 4 + i;
		cur_col = 0;
		put_str("Enter letter: ");
		(void) fflush(stdout);
		while (!islower(c = getchar()) || (c -= 'a') >= i) {
			(void) putchar(CTRL(G));
			(void) fflush(stdout);
		}
		Daemon = hosts[c];
		clear_the_screen();
		return;
	}
	if (!do_startup)
		return;

	start_driver();
	sleep(2);
	find_driver(FALSE);
}

dump_scores(host)
	SOCKET	host;
{
	struct	hostent	*hp;
	int	s;
	char	buf[BUFSIZ];
	int	cnt;

	hp = gethostbyaddr((char *) &host.sin_addr, sizeof host.sin_addr,
								AF_INET);
	printf("\n%s:\n", hp != NULL ? hp->h_name : inet_ntoa(host.sin_addr));
	fflush(stdout);

	s = socket(SOCK_FAMILY, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	if (connect(s, (struct sockaddr *) &host, sizeof host) < 0) {
		perror("connect");
		exit(1);
	}
	while ((cnt = read(s, buf, BUFSIZ)) > 0)
		write(fileno(stdout), buf, cnt);
	(void) close(s);
}

# endif INTERNET

start_driver()
{
	register int	procid;

# ifdef MONITOR
	if (Am_monitor) {
		leave(1, "No one playing.");
		/* NOTREACHED */
	}
# endif MONITOR

# ifdef INTERNET
	if (Sock_host != NULL) {
		sleep(3);
		return;
	}
# endif INTERNET

	mvcur(cur_row, cur_col, 23, 0);
	cur_row = 23;
	cur_col = 0;
	put_str("Starting...");
	(void) fflush(stdout);
# ifndef BSD_RELEASE
	procid = fork();
# else
	procid = vfork();
# endif
	if (procid == -1) {
		perror("fork");
		leave(1, "fork failed.");
	}
	if (procid == 0) {
		(void) signal(SIGINT, SIG_IGN);
# ifndef INTERNET
		(void) close(Socket);
# else
		if (use_port == NULL)
# endif
			execl(Driver, "HUNT", (char *) NULL);
# ifdef INTERNET
		else 
			execl(Driver, "HUNT", "-p", use_port, (char *) NULL);
# endif
		/* only get here if exec failed */
		(void) kill(getppid(), SIGEMT);	/* tell mom */
		_exit(1);
	}
	mvcur(cur_row, cur_col, 23, 0);
	cur_row = 23;
	cur_col = 0;
	put_str("Connecting...");
	(void) fflush(stdout);
}

/*
 * bad_con:
 *	We had a bad connection.  For the moment we assume that this
 *	means the game is full.
 */
bad_con()
{
	leave(1, "The game is full.  Sorry.");
	/* NOTREACHED */
}

/*
 * bad_ver:
 *	version number mismatch.
 */
bad_ver()
{
	leave(1, "Version number mismatch. No go.");
	/* NOTREACHED */
}

/*
 * sigterm:
 *	Handle a terminate signal
 */
sigterm()
{
	leave(0, (char *) NULL);
	/* NOTREACHED */
}


/*
 * sigemt:
 *	Handle a emt signal - shouldn't happen on vaxes(?)
 */
sigemt()
{
	leave(1, "Unable to start driver.  Try again.");
	/* NOTREACHED */
}

# ifdef INTERNET
/*
 * sigalrm:
 *	Handle an alarm signal
 */
sigalrm()
{
	return;
}
# endif INTERNET

/*
 * rmnl:
 *	Remove a '\n' at the end of a string if there is one
 */
rmnl(s)
char	*s;
{
	register char	*cp;
	char		*rindex();

	cp = rindex(s, '\n');
	if (cp != NULL)
		*cp = '\0';
}

/*
 * intr:
 *	Handle a interrupt signal
 */
intr()
{
	register int	ch;
	register int	explained;
	register int	y, x;

	(void) signal(SIGINT, SIG_IGN);
	y = cur_row;
	x = cur_col;
	mvcur(cur_row, cur_col, 23, 0);
	cur_row = 23;
	cur_col = 0;
	put_str("Really quit? ");
	clear_eol();
	(void) fflush(stdout);
	explained = FALSE;
	for (;;) {
		ch = getchar();
		if (isupper(ch))
			ch = tolower(ch);
		if (ch == 'y') {
			if (Socket != 0) {
				(void) write(Socket, "q", 1);
				(void) close(Socket);
			}
			leave(0, (char *) NULL);
		}
		else if (ch == 'n') {
			(void) signal(SIGINT, intr);
			mvcur(cur_row, cur_col, y, x);
			cur_row = y;
			cur_col = x;
			(void) fflush(stdout);
			return;
		}
		if (!explained) {
			put_str("(Yes or No) ");
			(void) fflush(stdout);
			explained = TRUE;
		}
		(void) putchar(CTRL(G));
		(void) fflush(stdout);
	}
}

/*
 * leave:
 *	Leave the game somewhat gracefully, restoring all current
 *	tty stats.
 */
leave(eval, mesg)
int	eval;
char	*mesg;
{
	if (in_visual) {
		mvcur(cur_row, cur_col, 23, 0);
		(void) fflush(stdout);	/* flush in case VE changes pages */
# ifdef TERMINFO
		putp(cursor_normal);
		putp(exit_ca_mode);
		reset_shell_mode();
# else
		resetty();
		_puts(VE);
		_puts(TE);
# endif
	}
	if (mesg != NULL)
		puts(mesg);
	exit(eval);
}

#ifdef	SIGTSTP
/*
 * tstp:
 *	Handle stop and start signals
 */
tstp()
{
# ifndef TERMINFO
	static struct sgttyb	tty;
# endif
	int	y, x;

	y = cur_row;
	x = cur_col;
	mvcur(cur_row, cur_col, 23, 0);
	cur_row = 23;
	cur_col = 0;
# ifdef TERMINFO
	putp(cursor_normal);
	putp(exit_ca_mode);
	reset_shell_mode();
# else
	tty = _tty;
	_puts(VE);
	_puts(TE);
	(void) fflush(stdout);
	resetty();
# endif
	(void) kill(getpid(), SIGSTOP);
	(void) signal(SIGTSTP, tstp);
# ifdef TERMINFO
	reset_prog_mode();
	putp(enter_ca_mode);
	putp(cursor_visible);
# else
	_tty = tty;
	ioctl(_tty_ch, TIOCSETP, &_tty);
	_puts(TI);
	_puts(VS);
# endif
	cur_row = y;
	cur_col = x;
# ifdef TERMINFO
	putp(tgoto(CM, cur_row, cur_col));
# else
	_puts(tgoto(CM, cur_row, cur_col));
# endif
	redraw_screen();
	(void) fflush(stdout);
}
#endif

long
env_init(enter_status)
	long	enter_status;
{
	register int	i;
	char	*envp, *envname, *s, *index(), *strpbrk();

	for (i = 0; i < 256; i++)
		map_key[i] = (char) i;

	envname = NULL;
	if ((envp = getenv("HUNT")) != NULL) {
		while ((s = strpbrk(envp, "=,")) != NULL) {
			if (strncmp(envp, "cloak,", s - envp + 1) == 0) {
				enter_status = Q_CLOAK;
				envp = s + 1;
			}
			else if (strncmp(envp, "scan,", s - envp + 1) == 0) {
				enter_status = Q_SCAN;
				envp = s + 1;
			}
			else if (strncmp(envp, "fly,", s - envp + 1) == 0) {
				enter_status = Q_FLY;
				envp = s + 1;
			}
			else if (strncmp(envp, "nobeep,", s - envp + 1) == 0) {
				no_beep = TRUE;
				envp = s + 1;
			}
			else if (strncmp(envp, "name=", s - envp + 1) == 0) {
				envname = s + 1;
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					strncpy(name, envname, NAMELEN);
					break;
				}
				*s = '\0';
				strncpy(name, envname, NAMELEN);
				envp = s + 1;
			}
# ifdef INTERNET
			else if (strncmp(envp, "port=", s - envp + 1) == 0) {
				use_port = s + 1;
				Test_port = atoi(use_port);
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					break;
				}
				*s = '\0';
				envp = s + 1;
			}
			else if (strncmp(envp, "host=", s - envp + 1) == 0) {
				Sock_host = s + 1;
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					break;
				}
				*s = '\0';
				envp = s + 1;
			}
			else if (strncmp(envp, "message=", s - envp + 1) == 0) {
				Send_message = s + 1;
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					break;
				}
				*s = '\0';
				envp = s + 1;
			}
# endif
			else if (strncmp(envp, "team=", s - envp + 1) == 0) {
				team = *(s + 1);
				if (!isdigit(team))
					team = ' ';
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					break;
				}
				*s = '\0';
				envp = s + 1;
			}			/* must be last option */
			else if (strncmp(envp, "mapkey=", s - envp + 1) == 0) {
				for (s = s + 1; *s != '\0'; s += 2) {
					map_key[(unsigned int) *s] = *(s + 1);
					if (*(s + 1) == '\0') {
						break;
					}
				}
				*envp = '\0';
				break;
			} else {
				*s = '\0';
				printf("unknown option %s\n", envp);
				if ((s = index(envp, ',')) == NULL) {
					*envp = '\0';
					break;
				}
				envp = s + 1;
			}
		}
		if (*envp != '\0')
			if (envname == NULL)
				strncpy(name, envp, NAMELEN);
			else
				printf("unknown option %s\n", envp);
	}
	return enter_status;
}

fill_in_blanks()
{
	register int	i;
	register char	*cp;

again:
	if (name[0] != '\0') {
		printf("Entering as '%s'", name);
		if (team != ' ')
			printf(" on team %c.\n", team);
		else
			putchar('\n');
	} else {
		printf("Enter your code name: ");
		if (fgets(name, NAMELEN, stdin) == NULL)
			exit(1);
	}
	rmnl(name);
	if (name[0] == '\0') {
		name[0] = '\0';
		printf("You have to have a code name!\n");
		goto again;
	}
	for (cp = name; *cp != '\0'; cp++)
		if (!isprint(*cp)) {
			name[0] = '\0';
			printf("Illegal character in your code name.\n");
			goto again;
		}
	if (team == ' ') {
		printf("Enter your team (0-9 or nothing): ");
		i = getchar();
		if (isdigit(i))
			team = i;
		while (i != '\n' && i != EOF)
			i = getchar();
	}
}

# ifdef BSD_RELEASE
# if BSD_RELEASE < 43
char *
strpbrk(s, brk)
	register char *s, *brk;
{
	register char *p;
	register c;

	while (c = *s) {
		for (p = brk; *p; p++)
			if (c == *p)
				return (s);
		s++;
	}
	return (0);
}
# endif
# endif
