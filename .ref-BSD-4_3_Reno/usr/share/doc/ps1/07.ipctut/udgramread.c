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
static char sccsid[] = "@(#)udgramread.c	6.4 (Berkeley) 3/7/89";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

/*
 * In the included file <sys/un.h> a sockaddr_un is defined as follows
 * struct sockaddr_un {
 *	short	sun_family;
 *	char	sun_path[108];
 * }; 
 */

#include <stdio.h>

#define NAME "socket"

/*
 * This program creates a UNIX domain datagram socket, binds a name to it,
 * then reads from the socket. 
 */
main()
{
	int sock, length;
	struct sockaddr_un name;
	char buf[1024];

	/* Create socket from which to read. */
	sock = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (sock < 0) {
		perror("opening datagram socket");
		exit(1);
	}
	/* Create name. */
	name.sun_family = AF_UNIX;
	strcpy(name.sun_path, NAME);
	if (bind(sock, &name, sizeof(struct sockaddr_un))) {
		perror("binding name to datagram socket");
		exit(1);
	}
	printf("socket -->%s\en", NAME);
	/* Read from the socket */
	if (read(sock, buf, 1024) < 0)
		perror("receiving datagram packet");
	printf("-->%s\en", buf);
	close(sock);
	unlink(NAME);
}
