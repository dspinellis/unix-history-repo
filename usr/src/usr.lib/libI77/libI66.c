/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)libI66.c	5.1	6/7/85
 */

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
