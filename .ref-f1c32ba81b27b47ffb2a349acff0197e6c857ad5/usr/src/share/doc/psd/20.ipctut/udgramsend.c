.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)udgramsend.c	6.4 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#define DATA "The sea is calm tonight, the tide is full . . ."

/*
 * Here I send a datagram to a receiver whose name I get from the command
 * line arguments.  The form of the command line is udgramsend pathname 
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	int sock;
	struct sockaddr_un name;

	/* Create socket on which to send. */
	sock = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (sock < 0) {
		perror("opening datagram socket");
		exit(1);
	}
	/* Construct name of socket to send to. */
	name.sun_family = AF_UNIX;
	strcpy(name.sun_path, argv[1]);
	/* Send message. */
	if (sendto(sock, DATA, sizeof(DATA), 0,
	    &name, sizeof(struct sockaddr_un)) < 0) {
		perror("sending datagram message");
	}
	close(sock);
}
