#ifndef lint
static char	*sccsid = "@(#)main.c	1.8	(Berkeley) 7/17/87";
#endif

/*
 *	Network News Transfer Protocol server
 *
 *	Phil Lapsley
 *	University of California, Berkeley
 *	(Internet: phil@berkeley.edu; UUCP: ...!ucbvax!phil)
 */

#include "common.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>

main()
{

#ifdef ALONE	/* If no inetd */

	int			sockt, client, length;
	struct sockaddr_in	from;
	extern int 		reaper();

	disassoc();

	/* fd 0-2 should be open and point to / now. */

#ifdef SYSLOG
#ifdef BSD_42
	openlog("nntpd", LOG_PID);			/* fd 3 */
#else
	openlog("nntpd", LOG_PID, SYSLOG);		/* fd 3 */
#endif
#endif

#ifdef FASTFORK
	num_groups = read_groups();	/* Read active file now (fd 4) */
					/* and then do it every */
	set_timer();			/* so often later */
#endif

	sockt = get_socket();		/* should be fd 4 or 5 */

	(void) signal(SIGCHLD, reaper);

	if (listen(sockt, SOMAXCONN) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "main: listen: %m");
#endif
		exit(1);
	}

	for (;;) {
		length = sizeof (from);
		client = accept(sockt, &from, &length);
		if (client < 0) {
#ifdef SYSLOG
			if (errno != EINTR)
				syslog(LOG_ERR, "accept: %m\n");
#endif
			continue;
		}

		switch (fork()) {
		case	-1:
#ifdef SYSLOG
				syslog(LOG_ERR, "fork: %m\n");
#endif
				(void) close(client);
				break;

		case	0:	(void) close(sockt);
				make_stdio(client);
				serve();
				break;

		default:	(void) close(client);
				break;
		}
	}

#else		/* We have inetd */

	serve();

#endif
}
