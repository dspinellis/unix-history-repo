.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)streamwrite.c	6.4 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#define DATA "Half a league, half a league . . ."

/*
 * This program creates a socket and initiates a connection with the socket
 * given in the command line.  One message is sent over the connection and
 * then the socket is closed, ending the connection. The form of the command
 * line is streamwrite hostname portnumber 
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	int sock;
	struct sockaddr_in server;
	struct hostent *hp, *gethostbyname();
	char buf[1024];

	/* Create socket */
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(1);
	}
	/* Connect socket using name specified by command line. */
	server.sin_family = AF_INET;
	hp = gethostbyname(argv[1]);
	if (hp == 0) {
		fprintf(stderr, "%s: unknown host\n", argv[1]);
		exit(2);
	}
	bcopy(hp->h_addr, &server.sin_addr, hp->h_length);
	server.sin_port = htons(atoi(argv[2]));

	if (connect(sock, &server, sizeof(server)) < 0) {
		perror("connecting stream socket");
		exit(1);
	}
	if (write(sock, DATA, sizeof(DATA)) < 0)
		perror("writing on stream socket");
	close(sock);
}
