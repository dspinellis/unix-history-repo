/*	lpd.c	4.1	83/04/29	*/
/*
 * lpd -- line printer daemon.
 *
 * Listen for a connection and perform the requested operation.
 * Operations are:
 *	\1printer\n
 *		check the queue for jobs and print any found.
 *	\2printer\n
 *		receive a job from another machine and queue it.
 *	\3printer [users ...] [jobs ...]\n
 *		return the current state of the queue (short form).
 *	\4printer [users ...] [jobs ...]\n
 *		return the current state of the queue (long form).
 *	\5printer person [users ...] [jobs ...]\n
 *		remove jobs from the queue.
 *
 * Strategy to maintain protected spooling area:
 *	1. Spooling area is writable only by daemon and spooling group
 *	2. lpr runs setuid root and setgrp spooling group; it uses
 *	   root to access any file it wants (verifying things before
 *	   with an access call) and group id to know how it should
 *	   set up ownership of files in the spooling area.
 *	3. Files in spooling area are owned by the owner, group spooling
 *	   group, with mode 660.
 *	4. lpd, lpq and lprm run setuid daemon and setgrp spooling group to
 *	   access files and printer.  Users can't get to anything
 *	   w/o help of lpq and lprm programs.
 */

#include "lp.h"

char	*logfile = DEFLOGF;
struct	sockaddr_in sin = { AF_INET };
int	reapchild();
char	*ntoa();

main(argc, argv)
	int argc;
	char **argv;
{
	int f, options;
	struct sockaddr_in from;
	struct servent *sp;

	gethostname(host, sizeof(host));
	name = argv[0];

	sp = getservbyname("printer", "tcp");
	if (sp == NULL) {
		log("printer/tcp: unknown service");
		exit(1);
	}
	sin.sin_port = sp->s_port;

	while (--argc > 0) {
		argv++;
		if (argv[0][0] == '-')
			switch (argv[0][1]) {
			case 'd':
				options |= SO_DEBUG;
				break;
			case 'l':
				argc--;
				logfile = *++argv;
				break;
			}
		else {
			int port = atoi(argv[0]);

			if (port < 0) {
				fprintf(stderr, "%s: bad port #\n", argv[0]);
				exit(1);
			}
			sin.sin_port = htons((u_short) port);
		}
	}
#ifndef DEBUG
	/*
	 * Set up standard environment by detaching from the parent.
	 */
	if (fork())
		exit(0);
	for (f = 0; f < 3; f++)
		(void) close(f);
	(void) open("/dev/null", FRDONLY, 0);
	(void) open("/dev/null", FWRONLY, 0);
	(void) open(logfile, FWRONLY|FAPPEND, 0);
	f = open("/dev/tty", FRDWR, 0);
	if (f > 0) {
		ioctl(f, TIOCNOTTY, 0);
		(void) close(f);
	}
#endif
	(void) umask(0);
	f = socket(AF_INET, SOCK_STREAM, 0);
	if (f < 0) {
		logerror("socket");
		exit(1);
	}
	if (options & SO_DEBUG)
		if (setsockopt(f, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
			logerror("setsockopt (SO_DEBUG)");
	if (bind(f, &sin, sizeof(sin), 0) < 0) {
		logerror("bind");
		exit(1);
	}
	/*
	 * Restart all the printers and tell everyone that we are
	 * up and running.
	 */
	startup();
	/*
	 * Main loop: listen, accept, do a request, continue.
	 */
	sigset(SIGCHLD, reapchild);
	listen(f, 10);
	for (;;) {
		int s, len = sizeof(from);

		s = accept(f, &from, &len, 0);
		if (s < 0) {
			if (errno == EINTR)
				continue;
			logerror("accept");
			continue;
		}
		if (fork() == 0) {
			sigset(SIGCHLD, SIG_IGN);
			(void) close(f);
			doit(s, &from);
			exit(0);
		}
		(void) close(s);
	}
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

/*
 * Stuff for handling job specifications
 */
char	*user[MAXUSERS];	/* users to process */
int	users;			/* # of users in user array */
int	requ[MAXREQUESTS];	/* job number of spool entries */
int	requests;		/* # of spool requests */

char	cbuf[BUFSIZ];		/* command line buffer */
char	fromb[32];		/* buffer for client's machine name */

doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	register char *cp;
	register struct hostent *hp;
	register int n;
	extern char *person;
	char c;

	dup2(f, 1);
	(void) close(f);
	f = 1;
	fromp->sin_port = ntohs(fromp->sin_port);
	if (fromp->sin_family != AF_INET || fromp->sin_port >= IPPORT_RESERVED)
		fatal("Malformed from address");
	hp = gethostbyaddr(&fromp->sin_addr, sizeof(struct in_addr),
		fromp->sin_family);
	if (hp == 0)
		fatal("Host name for your address (%s) unknown",
			ntoa(fromp->sin_addr));
	strcpy(fromb, hp->h_name);
	from = fromb;
	for (;;) {
		cp = cbuf;
		do {
			if ((n = read(f, &c, 1)) != 1) {
				if (n < 0)
					fatal("Lost connection");
				return;
			}
			if (cp >= &cbuf[sizeof(cbuf)])
				fatal("Command line too long");
			*cp++ = c;
		} while (c != '\n');
		*--cp = '\0';
		cp = cbuf;
		switch (*cp++) {
		case '\1':	/* check the queue and print any jobs there */
			printer = cp;
			printjob();
			break;
		case '\2':	/* receive files to be queued */
			printer = cp;
			recvjob();
			break;
		case '\3':	/* send back the short form queue status */
		case '\4':	/* send back the long form queue status */
			printer = cp;
			while (*cp) {
				if (*cp != ' ') {
					cp++;
					continue;
				}
				*cp++ = '\0';
				while (isspace(*cp))
					cp++;
				if (*cp == '\0')
					break;
				if (isdigit(*cp)) {
					if (requests >= MAXREQUESTS)
						fatal("Too many requests");
					requ[requests++] = atoi(cp);
				} else {
					if (users >= MAXUSERS)
						fatal("Too many users");
					user[users++] = cp;
				}
			}
			displayq(cbuf[0] - '\3');
			exit(0);
		case '\5':	/* remove a job from the queue */
			printer = cp;
			while (*cp && *cp != ' ')
				cp++;
			if (!*cp)
				break;
			*cp++ = '\0';
			person = cp;
			while (*cp) {
				if (*cp != ' ') {
					cp++;
					continue;
				}
				*cp++ = '\0';
				while (isspace(*cp))
					cp++;
				if (*cp == '\0')
					break;
				if (isdigit(*cp)) {
					if (requests >= MAXREQUESTS)
						fatal("Too many requests");
					requ[requests++] = atoi(cp);
				} else {
					if (users >= MAXUSERS)
						fatal("Too many users");
					user[users++] = cp;
				}
			}
			rmjob();
			break;
		}
		fatal("Illegal service request");
		exit(1);
	}
}

/*
 * Convert network-format internet address
 * to base 256 d.d.d.d representation.
 */
char *
ntoa(in)
	struct in_addr in;
{
	static char b[18];
	register char *p;

	p = (char *)&in;
#define	UC(b)	(((int)b)&0xff)
	sprintf(b, "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
	return (b);
}

/*VARARGS1*/
log(msg, a1, a2, a3)
	char *msg;
{
	short console = isatty(fileno(stderr));

	fprintf(stderr, console ? "\r\n%s: " : "%s: ", name);
	if (printer)
		fprintf(stderr, "%s: ", printer);
	fprintf(stderr, msg, a1, a2, a3);
	if (console)
		putc('\r', stderr);
	putc('\n', stderr);
	fflush(stderr);
}

logerror(msg)
	char *msg;
{
	register int err = errno;
	short console = isatty(fileno(stderr));
	extern int sys_nerr;
	extern char *sys_errlist[];

	fprintf(stderr, console ? "\r\n%s: " : "%s: ", name);
	if (*msg)
		fprintf(stderr, "%s: ", msg);
	fputs(err < sys_nerr ? sys_errlist[err] : "Unknown error" , stderr);
	if (console)
		putc('\r', stderr);
	putc('\n', stderr);
	fflush(stderr);
}
