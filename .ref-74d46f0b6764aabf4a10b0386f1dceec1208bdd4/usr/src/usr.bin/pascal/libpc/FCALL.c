/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)FCALL.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

FCALL(save, frtn)
	char *save;
	register struct formalrtn *frtn;
{
	blkcpy(&_disply[1], save, frtn->fbn * sizeof(struct display));
	blkcpy(&frtn->fdisp[0], &_disply[1],
		frtn->fbn * sizeof(struct display));
}
