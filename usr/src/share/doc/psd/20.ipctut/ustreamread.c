.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)ustreamread.c	6.1 (Berkeley) %G%
.\"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#define TRUE 1
#define NAME "socket"

/*
 * This program creates a socket in the UNIX domain and binds a name to it. 
 * After printing the socket's name it begins a loop. Each time through the
 * loop it accepts a connection and prints out messages from it.  When the
 * connection breaks, or a termination message comes through, the program
 * accepts a new connection. 
 */

main()
{
	int             sock;
	struct sockaddr_un server;
	int             msgsock;
	char            buf[1024];
	int             rval;
	int             i;

	/* Create socket */
	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(0);
	}
	/* Name socket using file system name */
	server.sun_family = AF_UNIX;
	strcpy(server.sun_path, NAME);
	if (bind(sock, &server, sizeof(struct sockaddr_un))) {
		perror("binding stream socket");
	}
	printf("Socket has name %s\en", server.sun_path);

	/* Start accepting connections */
	listen(sock, 5);
	do {
		msgsock = accept(sock, 0, 0);
		do {
			for (i = 0; i < 1024; i++)
				buf[i] = '\e0';
			if ((rval = read(msgsock, buf, 1024)) < 0)
				perror("reading stream message");
			if (rval == 0) {
				printf("Ending connection\en");
			} else {
				printf("-->%s\en", buf);
			};
		} while (rval != 0);
		close(msgsock);
	} while (TRUE);
	/*
	 * The following statements are not executed, because they follow an
	 * infinite loop.  However, most ordinary programs will not run
	 * forever.  In the UNIX domain it is necessary to tell the file
	 * system that one is through using NAME. in most programs one uses
	 * the call unlink() as below. Since the user will have to kill this
	 * program, it will be necessary to remove the name by a command from
	 * the shell. 
	 */
	unlink(NAME);
	close(sock);
}
