/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "imp.h"
#include "imPcodes.h"

closepl(){
	putch(imP_ENDPAGE);
	fflush(stdout);
}
