/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)necf.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <sgtty.h>

#define PAGESIZE	66

main()
{
	extern char _sobuf[BUFSIZ];
	extern char *rindex();
	char line[256];
	register char c, *cp;
	register lnumber;

	setbuf(stdout, _sobuf);
#ifdef SHEETFEEDER
	printf("\033=\033\033\033O\f");
#else
	printf("\033=");
#endif
	lnumber = 0;
	while (fgets(line, sizeof(line), stdin) != NULL) {
#ifdef SHEETFEEDER
		if (lnumber == PAGESIZE-1) {
			putchar('\f');
			lnumber = 0;
		}
		if (lnumber >= 2) {
#endif
#ifdef TTY
			if ((cp = rindex(line, '\n')) != NULL)
				*cp = '\r';
#endif
			printf("%s", line);
#ifdef SHEETFEEDER
		}
		lnumber++;
#endif
	}
	fflush (stdout);
}
