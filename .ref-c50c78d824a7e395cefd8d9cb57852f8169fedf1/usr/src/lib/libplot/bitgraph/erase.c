/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.2 (Berkeley) %G%";
#endif not lint


#include "bg.h"

erase()
{
	putchar( ESC );
	printf("[H");
	putchar( ESC );
	printf("[J");
}
