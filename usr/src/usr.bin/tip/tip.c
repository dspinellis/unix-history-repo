/*	tip.c	4.4	81/06/16	*/
/*
 * tip - Unix link to other systems
 *  tip [-v] [-speed] system-name
 *
 * Uses remote file for system descriptions.
 * Current commands (escapes):
 *
 *	~!	fork a shell on the local machine
 *	~c	change working directory on local machine
 *	~^D	exit tip
 *	~<	fetch file from remote system
 *	~>	send file to remote system
 *	~t	take a file from a remote UNIX (uses cat & echo)
 *	~p	send a file to a remote UNIX (uses cat)
 *	~|	fetch file from remote system and pipe it to
 *		 a local process
 *	~%	fork and wait for a program which inherits file
 *		 descriptors 3 & 4 attached to the remote machine
 *		 (optional by CONNECT define)
 *	~s	set or show variable
 *	~?	give a help summary
 *
 * Samuel J. Leffler	1-18-81
 *
 * sjl			2-11-81
 * add auto-dial stuff for the BIZCOMP
 *
 * sjl			2-14-81
 * cleaned up auto-dialer stuff and added variables
 *
 * sjl			2-19-81
 * handle quit and interrupt during calls
 *
 * sjl			3-8-81
 * made to pass lint
 *
 * sjl			4-11-81
 * mods to handle both FIOCAPACITY and FIONREAD in biz.c
 *
 * sjl			4-17-81
 * added take and put, made piping stuff work
 * honor uucp locks
 * rewrite remote file stuff for DN-11 like acu's and just to clean
 *   it up
 *
 * sjl			6-16-81
 * real working setup for DN-11's
 */

#include "tip.h"

/*
 * Baud rate mapping table
 */
int bauds[] = {
	0, 50, 75, 110, 134, 150, 200, 300, 600,
	1200, 1800, 2400, 4800, 9600, 19200, -1
};

int	intprompt();
int	timeout();
static int cleanup();

main(argc, argv)
char *argv[];
{
	char *system = NOSTR;
	register int i;
#ifdef VMUNIX
	int disc;
#endif
	char *p;

	if (argc > 4) {
		fprintf(stderr, "usage: tip [-v] [-speed] [system-name]\n");
		exit(1);
	}
	if (!isatty(0)) {
		fprintf(stderr, "tip: must be interactive\n");
		exit(1);
	}
	if (argc > 1 && argv[argc-1][0] != '-')
		system = argv[argc-1];		/* always last item */
	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGTERM, cleanup);

	if ((i = hunt(system)) == 0) {
		printf("all ports busy\n");
		exit(3);
	}
	if (i == -1) {
		printf("link down\n");
		exit(3);
	}
	setbuf(stdout, NULL);
	loginit();
	/*
	 * Now that we have the logfile and the ACU open
	 *  return to the real uid and gid.  These things will
	 *  be closed on exit.  Note that we can't run as root,
	 *  because locking mechanism on the tty and the accounting
	 *  will be bypassed.
	 */
	setuid(getuid());
	setgid(getgid());
	for (i = 1; i < argc-1; i++)
		if (equal(argv[i], "-v"))
			vflag++;
	/*
	 * Kludge, their's no easy way to get the initialization
	 *   in the right order, so force it here
	 */
	if ((PH = getenv("PHONES")) == NOSTR)
		PH = "/etc/phones";
	vinit();				/* init variables */
	for (i = 1; i < argc-1; i++)
		if (argv[i][0] == '-' && argv[i][1] != 'v') {
			if (isnum(argv[i][1]))
				number(value(BAUDRATE)) = atoi(&argv[i][1]);
			else
				printf("%s: unknown option\n", argv[i]);
		}
	if ((arg.sg_ispeed = speed(number(value(BAUDRATE)))) == NULL) {
		printf("tip: bad baud rate %d\n", number(value(BAUDRATE)));
		delock(uucplock);
		exit(3);
	}

	if (p = connect()) {
		printf("\07%s\n[EOT]\n", p);
		delock(uucplock);
		exit(1);
	}
	arg.sg_ospeed = arg.sg_ispeed;
	/*
	 * NOTE that remote side runs in TANDEM mode,
	 *  if the host doesn't honor X-ON/X-OFF with default
	 *  start/stop chars, the remote description must be
	 *  extended and tchars will have to be set up here.
	 * If the host doesn't honor TANDEM mode, then watch
	 *  out, as you'll get garbage.
	 */
	arg.sg_flags = RAW | TANDEM;
	ioctl(FD, TIOCSETP, &arg);

	ioctl(0, TIOCGETP, &defarg);	/* store initial status */
	ioctl(0, TIOCGETC, &defchars);
	arg = defarg;
	arg.sg_flags = ANYP | CBREAK;
	tchars = defchars;
	tchars.t_intrc = tchars.t_quitc = -1;
	raw();
#ifdef VMUNIX
	ioctl(0, TIOCGETD, (char *)&odisc);
	disc = OTTYDISC;
	ioctl(0, TIOCSETD, (char *)&disc);
#endif
	pipe(fildes); pipe(repdes);
	signal(SIGALRM, timeout);

	/*
	 * Everything's set up now:
	 *	connection established (hardwired or diaulup)
	 *	line conditioned (baud rate, mode, etc.)
	 *	internal data structures (variables)
	 * so, fork one process for local side and one for remote.
	 */
	write(1, "\07connected\r\n", 12);
	if (pid = fork())
		tipin();
	else
		tipout();
	/*NOTREACHED*/
}

static
cleanup()
{
	delock(uucplock);
#ifdef VMUNIX
	if (odisc)
		ioctl(0, TIOCSETD, (char *)&odisc);
#endif
	exit(0);
}

/*
 * put the controlling keyboard into raw mode
 */
raw()
{
	ioctl(0, TIOCSETP, &arg);
	ioctl(0, TIOCSETC, &tchars);
}


/*
 * return keyboard to normal mode
 */
unraw()
{
	ioctl(0, TIOCSETP, &defarg);
	ioctl(0, TIOCSETC, &defchars);
}

/*
 * Print string ``s'', then read a string
 *  in from the terminal.  Handles signals & allows use of
 *  normal erase and kill characters.
 */
prompt(s, p)
	char *s;
	register char *p;
{
	register char *b = p;

	stoprompt = 0;
	signal(SIGINT, intprompt);
	signal(SIGQUIT, SIG_IGN);
	unraw();
	printf("%s", s);
	while ((*p = getchar()) != EOF && *p != '\n') {
		if (stoprompt)
			goto pbreak;
		p++;
	}
	*p = '\0';
pbreak:
	raw();
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT,SIG_DFL);
	return(stoprompt || p == b);
}

/*
 * Interrupt service routine during prompting
 */
intprompt()
{
	signal(SIGINT, SIG_IGN);
	stoprompt = 1;
	printf("\r\n");
}

/*
 * ****TIPIN   TIPIN****
 */
tipin()
{
	char gch, bol = 1;

	/*
	 * Kinda klugey here...
	 *   check for scripting being turned on from the .tiprc file,
	 *   but be careful about just using setscript(), as we may
	 *   send a SIGEMT before tipout has a chance to set up catching
	 *   it; so wait a second, then setscript()
	 */
	if (boolean(value(SCRIPT))) {
		sleep(1);
		setscript();
	}

	while (1) {
		gch = getchar()&0177;
		if ((gch == character(value(ESCAPE))) && bol) {
			if (!(gch = escape()))
				continue;
		} else if (gch == character(value(RAISECHAR))) {
			boolean(value(RAISE)) = !boolean(value(RAISE));
			printf("%s", ctrl(character(value(RAISECHAR))));
			continue;
		} else if (gch == '\r') {
			bol = 1;
			write(FD, &gch, 1);
			continue;
		} else if (gch == character(value(FORCE))) {
			printf("%s", ctrl(character(value(FORCE))));
			gch = getchar()&0177;
		}
		bol = any(gch, value(EOL));
		if (boolean(value(RAISE)) && islower(gch))
			toupper(gch);
		write(FD, &gch, 1);
	}
}

/*
 * Escape handler --
 *  called on recognition of ``escapec'' at the beginning of a line
 */
escape()
{
	register char gch;
	register esctable_t *p;
	char c = character(value(ESCAPE));
	extern esctable_t etable[];

	gch = (getchar()&0177);
	for (p = etable; p->e_char; p++)
		if (p->e_char == gch) {
			if ((p->e_flags&PRIV) && getuid())
				continue;
			printf("%s", ctrl(c));
			(*p->e_func)(gch);
			return(0);
		}

	write(FD, &c, 1);
	return(gch);
}

speed(n)
{
	register int *p;

	for (p = bauds; *p != -1;  p++)
		if (*p == n)
			return(p-bauds);
	return(NULL);
}

any(c, p)
	register char c, *p;
{
	while (*p)
		if (*p++ == c)
			return(1);
	return(0);
}

size(s)
	register char	*s;
{
	register int	i = 0;

	while (*s++) i++;
	return(i);
}

char *
interp(s)
	register char *s;
{
	static char buf[256];
	register char *p = buf, c, *q;

	while (c = *s++) {
		for (q = "\nn\rr\tt\ff\033E\bb"; *q; q++)
			if (*q++ == c) {
				*p++ = '\\'; *p++ = *q;
				goto next;
			}
		if (c < 040) {
			*p++ = '^'; *p++ = c + 'A'-1;
		} else if (c == 0177) {
			*p++ = '^'; *p++ = '?';
		} else
			*p++ = c;
	next:
		;
	}
	*p = '\0';
	return(buf);
}

char *
ctrl(c)
	char c;
{
	static char s[3];

	if (c < 040 || c == 0177) {
		s[0] = '^';
		s[1] = c == 0177 ? '?' : c+'A'-1;
		s[2] = '\0';
	} else {
		s[0] = c;
		s[1] = '\0';
	}
	return(s);
}

/*
 * Help command
 */
help(c)
	char c;
{
	register esctable_t *p;
	extern esctable_t etable[];

	printf("%c\r\n", c);
	for (p = etable; p->e_char; p++) {
		if ((p->e_flags&PRIV) && getuid())
			continue;
		printf("%2s", ctrl(character(value(ESCAPE))));
		printf("%-2s %c   %s\r\n", ctrl(p->e_char),
			p->e_flags&EXP ? '*': ' ', p->e_help);
	}
}
