/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)setjmperr.c	5.1 (Berkeley) %G%";
#endif not lint

#define ERRMSG	"longjmp botch\n"

/*
 * This routine is called from longjmp() when an error occurs.
 * Programs that wish to exit gracefully from this error may
 * write their own versions.
 * If this routine returns, the program is aborted.
 */
longjmperror()
{

	write(2, ERRMSG, sizeof(ERRMSG));
}
