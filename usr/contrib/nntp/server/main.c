#ifndef lint
static char	*sccsid = "@(#)main.c	1.2	(Berkeley) 2/8/86";
#endif

/*
 *	Network News Transfer Protocol server
 *
 *	Phil Lapsley
 *	College of Engineering
 *	University of California, Berkeley
 *	(ARPA: phil@berkeley.edu; UUCP: ...!ucbvax!phil)
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

#ifdef FASTFORK
	num_groups = read_groups();	/* Read active file now */
					/* and then do it every */
	set_timer();			/* so often later */
#endif

	sockt = get_socket();

	signal(SIGCHLD, reaper);

	listen(sockt, SOMAXCONN);

	for (;;) {
		client = accept(sockt, &from, &length);
		if (client < 0) {
			if (errno != EINTR)
				syslog(LOG_ERR, "nntpd: accept: %m\n");
			continue;
		}

		switch (fork()) {
		case	-1:	syslog(LOG_ERR, "nntpd: fork: %m\n");
				close(client);
				break;

		case	0:	close(sockt);
				make_stdio(client);
				serve();
				break;

		default:	close(client);
				break;
		}
	}

#else		/* We have inetd */

	serve();

#endif
}
