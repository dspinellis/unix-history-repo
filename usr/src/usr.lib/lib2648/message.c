/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)message.c	5.1 (Berkeley) 4/30/85";
#endif not lint

/*
 * message: print str on the screen in the message area.
 */

#include "2648.h"

message(str)
char *str;
{
	dispmsg(str, 4, 4, 100);
}
