/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)freeze.c	4.3 (Berkeley) 4/17/91";
#endif /* not lint */

#include "stdio.h"
freeze(s) char *s;
{	int fd;
	unsigned int *len;
	len = (unsigned int *)sbrk(0);
	if((fd = creat(s, 0666)) < 0) {
		perror(s);
		return(1);
	}
	write(fd, &len, sizeof(len));
	write(fd, (char *)0, len);
	close(fd);
	return(0);
}

thaw(s) char *s;
{	int fd;
	unsigned int *len;
	if(*s == 0) {
		fprintf(stderr, "empty restore file\n");
		return(1);
	}
	if((fd = open(s, 0)) < 0) {
		perror(s);
		return(1);
	}
	read(fd, &len, sizeof(len));
	(void) brk(len);
	read(fd, (char *)0, len);
	close(fd);
	return(0);
}
