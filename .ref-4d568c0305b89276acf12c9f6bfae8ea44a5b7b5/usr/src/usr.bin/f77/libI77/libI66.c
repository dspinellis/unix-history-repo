/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)libI66.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * set flag to initialize fortran-66 mods
 *
 * usage: f77 ... -lI66 ...
 */

#include	"fiodefs.h"

struct ioiflg	ioiflg_ = {
	 0,		/* open files at beginning */
	 1,		/* carriage control on all units */
	 1,		/* blanks are zero on input; 0 => 0.0 on output */
};
