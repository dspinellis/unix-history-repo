/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)sh.print.c	5.4 (Berkeley) 6/6/88";
#endif

#include "sh.h"
#include <sys/ioctl.h>

/*
 * C Shell
 */

psecs(l)
	long l;
{
	register int i;

	i = l / 3600;
	if (i) {
		printf("%d:", i);
		i = l % 3600;
		p2dig(i / 60);
		goto minsec;
	}
	i = l;
	printf("%d", i / 60);
minsec:
	i %= 60;
	printf(":");
	p2dig(i);
}

p2dig(i)
	register int i;
{

	printf("%d%d", i / 10, i % 10);
}

char linbuf[LINELEN];
char *linp = linbuf;

cshputchar(ch)
	register int ch;
{
	CSHPUTCHAR;
}

draino()
{
	linp = linbuf;
}

flush()
{
	register int unit;
	int lmode;

	if (linp == linbuf)
		return;
	if (haderr)
		unit = didfds ? 2 : SHDIAG;
	else
		unit = didfds ? 1 : SHOUT;
#ifdef TIOCLGET
	if (didfds == 0 && ioctl(unit, TIOCLGET, (char *)&lmode) == 0 &&
	    lmode&LFLUSHO) {
		lmode = LFLUSHO;
		(void) ioctl(unit, TIOCLBIC, (char *)&lmode);
		(void) write(unit, "\n", 1);
	}
#endif
	(void) write(unit, linbuf, linp - linbuf);
	linp = linbuf;
}
