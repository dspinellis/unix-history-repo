/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PFLUSH.c 1.1 10/29/80";

#include "h00vars.h"

/*
 * insure that a usable image is in the buffer window
 */
PFLUSH()
{
	register struct iorec	*next;

	next = _fchain.fchain;
	while(next != FILNIL) {
		if ((next->funit & (FDEF | FREAD)) == 0) {
			fflush(next->fbuf);
		}
		next = next->fchain;
	}
}
