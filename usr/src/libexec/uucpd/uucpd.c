#ifndef lint
static char sccsid[] = "@(#)uucpd.c	5.1 (BERKELEY) %G%";
#endif

/*
 * UUCP server daemon
 * 	Looks for attempts to connect on our uucp socket.  When it
 *	finds one it execs uucico to handle it.
 *
 * Invoked by inetd.
 */

#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define	UUCICO	"/usr/lib/uucp/uucico"

extern int errno;

main(argc, argv)
	int argc;
	char **argv;
{
	int status, fromlen;
	struct sockaddr_in from;

	fromlen = sizeof(from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}

	execl(UUCICO, "UUCICO", "-r0", "-v", 0);
	openlog("uucpd", 0, 0);
	syslog(LOG_ERR, "%s: %m", UUCICO);
	exit(1);
}
