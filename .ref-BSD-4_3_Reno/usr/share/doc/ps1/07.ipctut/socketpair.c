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
static char sccsid[] = "@(#)socketpair.c	6.3 (Berkeley) 3/7/89";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>

#define DATA1 "In Xanadu, did Kublai Khan . . ."
#define DATA2 "A stately pleasure dome decree . . ."

/*
 * This program creates a pair of connected sockets then forks and
 * communicates over them.  This is very similar to communication with pipes,
 * however, socketpairs are two-way communications objects. Therefore I can
 * send messages in both directions. 
 */

main()
{
	int sockets[2], child;
	char buf[1024];

	if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0) {
		perror("opening stream socket pair");
		exit(1);
	}

	if ((child = fork()) == -1)
		perror("fork");
	else if (child) {	/* This is the parent. */
		close(sockets[0]);
		if (read(sockets[1], buf, 1024, 0) < 0)
			perror("reading stream message");
		printf("-->%s\en", buf);
		if (write(sockets[1], DATA2, sizeof(DATA2)) < 0)
			perror("writing stream message");
		close(sockets[1]);
	} else {		/* This is the child. */
		close(sockets[1]);
		if (write(sockets[0], DATA1, sizeof(DATA1)) < 0)
			perror("writing stream message");
		if (read(sockets[0], buf, 1024, 0) < 0)
			perror("reading stream message");
		printf("-->%s\en", buf);
		close(sockets[0]);
	}
}
