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
static char sccsid[] = "@(#)pipe.c	6.3 (Berkeley) 3/7/89";
#endif /* not lint */

#include <stdio.h>

#define DATA "Bright star, would I were steadfast as thou art . . ."

/*
 * This program creates a pipe, then forks.  The child communicates to the
 * parent over the pipe. Notice that a pipe is a one-way communications
 * device.  I can write to the output socket (sockets[1], the second socket
 * of the array returned by pipe()) and read from the input socket
 * (sockets[0]), but not vice versa. 
 */

main()
{
	int sockets[2], child;

	/* Create a pipe */
	if (pipe(sockets) < 0) {
		perror("opening stream socket pair");
		exit(10);
	}

	if ((child = fork()) == -1)
		perror("fork");
	else if (child) {
		char buf[1024];

		/* This is still the parent.  It reads the child's message. */
		close(sockets[1]);
		if (read(sockets[0], buf, 1024) < 0)
			perror("reading message");
		printf("-->%s\en", buf);
		close(sockets[0]);
	} else {
		/* This is the child.  It writes a message to its parent. */
		close(sockets[0]);
		if (write(sockets[1], DATA, sizeof(DATA)) < 0)
			perror("writing message");
		close(sockets[1]);
	}
}
