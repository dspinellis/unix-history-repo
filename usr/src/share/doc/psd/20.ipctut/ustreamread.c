/*
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ustreamread.c	6.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

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
	int sock, msgsock, rval;
	struct sockaddr_un server;
	char buf[1024];

	/* Create socket */
	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(1);
	}
	/* Name socket using file system name */
	server.sun_family = AF_UNIX;
	strcpy(server.sun_path, NAME);
	if (bind(sock, &server, sizeof(struct sockaddr_un))) {
		perror("binding stream socket");
		exit(1);
	}
	printf("Socket has name %s\en", server.sun_path);
	/* Start accepting connections */
	listen(sock, 5);
	for (;;) {
		msgsock = accept(sock, 0, 0);
		if (msgsock == -1)
			perror("accept");
		else do {
			bzero(buf, sizeof(buf));
			if ((rval = read(msgsock, buf, 1024)) < 0)
				perror("reading stream message");
			else if (rval == 0)
				printf("Ending connection\en");
			else
				printf("-->%s\en", buf);
		} while (rval > 0);
		close(msgsock);
	}
	/*
	 * The following statements are not executed, because they follow an
	 * infinite loop.  However, most ordinary programs will not run
	 * forever.  In the UNIX domain it is necessary to tell the file
	 * system that one is through using NAME.  In most programs one uses
	 * the call unlink() as below. Since the user will have to kill this
	 * program, it will be necessary to remove the name by a command from
	 * the shell. 
	 */
	close(sock);
	unlink(NAME);
}
