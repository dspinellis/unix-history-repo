/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)getfile.c	7.7 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/time.h>
#include <stand/saio.h>

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
