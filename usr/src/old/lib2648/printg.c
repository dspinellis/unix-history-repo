/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printg.c	5.1 (Berkeley) 4/26/85";
#endif not lint

#include "2648.h"

printg()
{
	int oldvid = _video;
	int c, c2;

	if (oldvid==INVERSE)
		togvid();
	sync();
	escseq(NONE);
	outstr("\33&p4d5u0C");
	outchar('\21');	/* test handshaking fix */

	/*
	 * The terminal sometimes sends back S<cr> or F<cr>.
	 * Ignore them.
	 */
	fflush(stdout);
	c = getchar();
	if (c=='F' || c=='S') {
		c2 = getchar();
		if (c2 != '\r' && c2 != '\n')
			ungetc(c2, stdin);
	} else {
		ungetc(c, stdin);
	}

	if (oldvid==INVERSE)
		togvid();
}
