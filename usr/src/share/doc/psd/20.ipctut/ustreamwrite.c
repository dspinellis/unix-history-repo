.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)ustreamwrite.c	6.5 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#define DATA "Half a league, half a league . . ."

/*
 * This program connects to the socket named in the command line and sends a
 * one line message to that socket. The form of the command line is
 * ustreamwrite pathname 
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int sock;
	struct sockaddr_un server;
	char buf[1024];

	/* Create socket */
	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(1);
	}
	/* Connect socket using name specified by command line. */
	server.sun_family = AF_UNIX;
	strcpy(server.sun_path, argv[1]);

	if (connect(sock, &server, sizeof(struct sockaddr_un)) < 0) {
		close(sock);
		perror("connecting stream socket");
		exit(1);
	}
	if (write(sock, DATA, sizeof(DATA)) < 0)
		perror("writing on stream socket");
}
