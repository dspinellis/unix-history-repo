/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tempnam.c	4.2 (Berkeley) %G%";
#endif not lint

char *tmpnam(s)
char *s;
{
	static seed;

	sprintf(s, "temp.%d.%d", getpid(), seed++);
	return(s);
}
