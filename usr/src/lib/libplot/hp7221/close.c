/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <signal.h>
#include "hp7221.h"

void
closepl()
{
	/* receive interupts */
	signal(SIGINT, SIG_IGN);
	printf( "v@}" );			/* Put pen away. */
	fflush( stdout );
}
