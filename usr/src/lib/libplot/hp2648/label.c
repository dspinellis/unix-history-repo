/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "hp2648.h"

label(s)
char *s;
{
	handshake();
	putchar(ESC);
	putchar(GRAPHIC);
	putchar('l');
	for(;*s!='\0';s++)
		putchar(*s);
	putchar(ESC);
	putchar(GRAPHIC);
	putchar('d');
	putchar('T');
	handshake();
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(PLOT);
	putchar(BINARY);
	buffcount = 4;
}
