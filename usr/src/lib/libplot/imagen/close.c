/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "imp.h"
#include "imPcodes.h"

closepl(){
	putch(imP_ENDPAGE);
	fflush(stdout);
}
