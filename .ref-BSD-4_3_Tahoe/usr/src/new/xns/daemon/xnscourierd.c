/* $Header: xnscourierd.c,v 2.2 86/11/22 07:39:06 jqj Exp $ */

/*
 * daemon for XNS Courier.  Listens on SPP socket 5 for requests for
 * Courier connections.  Forks one process per SPP connection to service
 * the Courier requests
 */

/*
 * $Log:	xnscourierd.c,v $
 * Revision 2.2  86/11/22  07:39:06  jqj
 * Better error recovery/retry mechanism, I hope.
 * 
 * Revision 2.1  86/06/30  12:52:26  jqj
 * added check for bad return on listen() per request from Bill Jackson at
 * Xerox PARCVAX.
 * 
 * Revision 2.0  85/11/21  07:21:56  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/21  06:06:52  jqj
 * Initial revision
 * 
 */

#include <stdio.h>		/* for lots of things */
#include <errno.h>		/* for EINTR */
#include <signal.h>		/* for signal() */
#include <sys/wait.h>		/* for struct wait and WNOHANG */
#include <sgtty.h>
#include <sys/types.h>		/* for lots of things, e.g. xn.h */
#include <sys/socket.h>		/* for SOCK_STREAM, AF_NS, etc. */
#include <netns/ns.h>		/* for sockaddr_ns, etc. */
#include <netns/sp.h> 
#include <xnscourier/courier.h>	/* for lots of things */
#include <xnscourier/realcourierconnection.h> /* for CourierConnection */

struct sockaddr_ns here, dest;

int CourierServerDebuggingFlag = 0;


/*
 * Message stream handle.
 */
CourierConnection *_serverConnection = 0;
Unspecified tid;				/* transaction ID */


static void
reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int s;
#ifndef DEBUGDBX
	if (fork())
		exit(0);
#endif /* DEBUGDBX */
	for (;;)
		poller(argc,argv);
}

static
poller(argc,argv)
	int argc;
	char *argv[];
{
	int s, pid;
	extern int errno;

	here.sns_family = AF_NS;
	here.sns_addr.x_port = htons(IDPPORT_COURIER);

#ifndef DEBUGDBX
	for (s = 0; s < 20; s++)
		(void) close(s);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	s = open("/dev/tty", 2);
	if (s > 0) {
		ioctl(s, TIOCNOTTY, 0);
		close(s);
	}
#endif /* DEBUGDBX */
	while ((s = socket(AF_NS, SOCK_SEQPACKET, 0)) < 0) {
		perror("xnscourierd: socket");
		sleep(5);
	}
	while (bind(s, &here, sizeof here) < 0) {
		perror("xnscourierd: bind");
		sleep(5);
	}
	signal(SIGCHLD, reapchild);
	while (listen(s, 10) < 0) {
		perror("xnscourierd: listen");
		sleep(5);
	}
	for (;;) {
		int s2, fromlen = sizeof(struct sockaddr_ns);
		/* int padbefore[100]; */
		struct sockaddr_ns from;
		/* int padafter[100]; */

		s2 = accept(s, (caddr_t)&from, &fromlen);
		if (s2 < 0) {
			if (errno == EINTR)
				continue;
			perror("xnscourierd: accept");
			(void) close(s);
			return;	/* reset the world */
		}
#ifndef DEBUGDBX
		if ((pid = fork()) < 0) {
			perror("xnscourierd: Out of processes");
			sleep(5);
		}
		else if (pid == 0) {
			/* child */
			signal(SIGCHLD, SIG_DFL);
			close(s);	/* don't keep accepting */
			doit(s2, &from);
			exit(1);	/* can't get here? */
			/*NOTREACHED*/
		}
#else
		signal(SIGCHLD, SIG_DFL);
		doit(s2, &from);
#endif
		close(s2);
	}
	/*NOTREACHED*/
}

static CourierConnection connblock;

/*
 * f is the socket on which we have gotten an SPP connection.
 * who is the sockaddr_ns for the other end.
 */
doit(f, who)
	int f;
	struct sockaddr_ns *who;
{
	LongCardinal programnum;
	Cardinal versionnum;
	int skipcount;
	Unspecified skippedwords[8];
	Unspecified *bp;
	static Cardinal ourversion = COURIERVERSION;

	/* set up the CourierConnection data */
	_serverConnection = &connblock;
	_serverConnection->fd = f;
	_serverConnection->state = wantversion;
	_serverConnection->bdtstate = wantdata;
	/* send our version number */
	bp = skippedwords;
	bp += externalize_Cardinal(&ourversion, bp);
	bp += externalize_Cardinal(&ourversion, bp);
	CourierWrite(_serverConnection, (bp-skippedwords), skippedwords,
		     0, (Unspecified*) NULL);
	/* read and process a connection message */
	for (;;) {
		skipcount = LookAheadCallMsg(&programnum, &versionnum,
					     skippedwords);
		if (skipcount < 0) fatal("connection timed out");
#ifdef DEBUG
		fprintf(stderr,"Chaining to %d(%d).  Skipcount =%d\n",
			programnum, versionnum, skipcount);
#endif
		ExecCourierProgram(programnum, versionnum, skipcount,
				   skippedwords);
	}
}


fatal(msg)
	char *msg;
{
	(void) fprintf(stderr, "xnscourierd: %s.\n", msg);
	exit(1);
}
