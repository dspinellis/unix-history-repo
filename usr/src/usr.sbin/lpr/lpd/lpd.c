/*	lpd.c	4.2	83/05/13	*/
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
 *	\6printer\n
 *		enable queuing on the specified printer queue.
 *	\7printer\n
 *		disable queuing on the specified printer queue.
 *	\8printer\n
 *		return the queue status (queuing enabled or disabled).
 *
 * Strategy to maintain protected spooling area:
 *	1. Spooling area is writable only by daemon and spooling group
 *	2. lpr runs setuid root and setgrp spooling group; it uses
 *	   root to access any file it wants (verifying things before
 *	   with an access call) and group id to know how it should
 *	   set up ownership of files in the spooling area.
 *	3. Files in spooling area are owned by root, group spooling
 *	   group, with mode 660.
 *	4. lpd, lpq and lprm run setuid daemon and setgrp spooling group to
 *	   access files and printer.  Users can't get to anything
 *	   w/o help of lpq and lprm programs.
 */

#include "lp.h"

int	lflag;					/* log requests flag */
char	*logfile = DEFLOGF;
struct	sockaddr_in sin = { AF_INET };
int	reapchild();
char	*ntoa();

main(argc, argv)
	int argc;
	char **argv;
{
	int f, options;
	struct sockaddr_in fromaddr;
	struct servent *sp;

	gethostname(host, sizeof(host));
	name = argv[0];

	sp = getservbyname("printer", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "printer/tcp: unknown service");
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
				lflag++;
				break;
			case 'L':
				argc--;
				logfile = *++argv;
				break;
			}
		else {
			int port = atoi(argv[0]);
			int c = argv[0][0];

			if (c < '0' || c > '9' || port < 0) {
				fprintf(stderr, "lpd: %s: bad port number\n", argv[0]);
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
		if (setsockopt(f, SOL_SOCKET, SO_DEBUG, 0, 0) < 0) {
			logerror("setsockopt (SO_DEBUG)");
			exit(1);
		}
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
		int s, len = sizeof(fromaddr);

		s = accept(f, &fromaddr, &len, 0);
		if (s < 0) {
			if (errno == EINTR)
				continue;
			logerror("accept");
			exit(1);
		}
		if (fork() == 0) {
			sigset(SIGCHLD, SIG_IGN);
			(void) close(f);
			doit(s, &fromaddr);
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

char	fromb[32];		/* buffer for client's machine name */
char	cbuf[BUFSIZ];		/* command line buffer */
char	*cmdnames[] = {
	"null",
	"printjob",
	"recvjob",
	"displayq short",
	"displayq long",
	"rmjob"
};

doit(f, fromaddr)
	int f;
	struct sockaddr_in *fromaddr;
{
	register char *cp;
	register struct hostent *hp;
	register int n;
	extern char *person;
	char c;

	dup2(f, 1);
	(void) close(f);
	f = 1;
	fromaddr->sin_port = ntohs(fromaddr->sin_port);
	if (fromaddr->sin_family != AF_INET || fromaddr->sin_port >= IPPORT_RESERVED)
		fatal("Malformed from address");
	hp = gethostbyaddr(&fromaddr->sin_addr, sizeof(struct in_addr),
		fromaddr->sin_family);
	if (hp == 0)
		fatal("Host name for your address (%s) unknown",
			ntoa(fromaddr->sin_addr));
	strcpy(fromb, hp->h_name);
	from = fromb;
	if (chkhost())
		fatal("Your host does not have line printer access");
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
		if (lflag && *cp >= '\1' && *cp <= '\5') {
			printer = NULL;
			log("%s requests %s %s", from, cmdnames[*cp], cp+1);
		}
		switch (*cp++) {
		case '\1':	/* check the queue and print any jobs there */
			printer = cp;
			printjob();
			break;
		case '\2':	/* receive files to be queued */
			printer = cp;
			recvjob();
			break;
		case '\3':	/* display the queue (short form) */
		case '\4':	/* display the queue (long form) */
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
 * Make a pass through the printcap database and start printing any
 * files left from the last time the machine went down.
 */
startup()
{
	char buf[BUFSIZ];
	register char *cp;
	int pid;

	printer = buf;

	/*
	 * Restart the daemons.
	 */
	while (getprent(buf) > 0) {
		for (cp = buf; *cp; cp++)
			if (*cp == '|' || *cp == ':') {
				*cp = '\0';
				break;
			}
		if ((pid = fork()) < 0) {
			log("startup: cannot fork");
			exit(1);
		}
		if (!pid) {
			endprent();
			printjob();
		}
	}
}

/*
 * Check to see if the from host has access to the line printer.
 */
chkhost()
{
	register FILE *hostf;
	register char *cp;
	char ahost[50];

	hostf = fopen("/etc/hosts.equiv", "r");
	while (fgets(ahost, sizeof(ahost), hostf)) {
		if (cp = index(ahost, '\n'))
			*cp = '\0';
		cp = index(ahost, ' ');
		if (!strcmp(from, ahost) && cp == NULL) {
			(void) fclose(hostf);
			return(0);
		}
	}
	(void) fclose(hostf);
	return(-1);
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
	if (msg)
		fprintf(stderr, "%s: ", msg);
	fputs(err < sys_nerr ? sys_errlist[err] : "Unknown error" , stderr);
	if (console)
		putc('\r', stderr);
	putc('\n', stderr);
	fflush(stderr);
}
