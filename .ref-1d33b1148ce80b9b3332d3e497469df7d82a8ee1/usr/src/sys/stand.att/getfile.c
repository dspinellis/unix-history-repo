/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)getfile.c	8.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/time.h>
#include <stand.att/saio.h>

getfile(prompt, mode)
	char *prompt;
	int mode;
{
	int fd;
	char buf[100];

	do {
		printf("%s: ", prompt);
		gets(buf);
	} while ((fd = open(buf, mode)) <= 0);
	return(fd);
}
