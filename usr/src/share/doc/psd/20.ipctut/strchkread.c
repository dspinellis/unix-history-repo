.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)strchkread.c	6.1 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#define TRUE 1

/*
 * This program uses select() to check that someone is trying to connect
 * before calling accept(). 
 */

main()
{
	int             sock, length;
	struct sockaddr_in server;
	int             msgsock;
	char            buf[1024];
	int             rval;
	int             i;
	int             ready;
	struct timeval  to;

	/* Create socket */
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(0);
	}
	/* Name socket using wildcards */
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = INADDR_ANY;
	server.sin_port = 0;
	if (bind(sock, &server, sizeof(server))) {
		perror("binding stream socket");
	}
	/* Find out assigned port number and print it out */
	length = sizeof(server);
	if (getsockname(sock, &server, &length)) {
		perror("getting socket name");
		exit(0);
	}
	printf("Socket has port #%d\en", ntohs(server.sin_port));

	/* Start accepting connections */
	listen(sock, 5);
	do {
		ready = 1 << sock;
		to.tv_sec = 5;
		select(20, &ready, 0, 0, &to);
		if (ready) {
			msgsock = accept(sock, 0, 0);
			do {
				for (i = 0; i < 1024; i++)
					buf[i] = '\e0';
				if ((rval = read(msgsock, buf, 1024)) < 0)
					perror("reading stream message");
				i = 0;
				if (rval == 0)
					printf("Ending connection\en");
				else
					printf("-->%s\en", buf);
			} while (rval != 0);
			close(msgsock);
		} else
			printf("Do something else\en");
	} while (TRUE);
}
