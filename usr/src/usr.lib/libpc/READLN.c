/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READLN.c 1.4 6/10/81";

#include "h00vars.h"

READLN(curfile)

	register struct iorec	*curfile;
{
	do	{
		IOSYNC(curfile);
		curfile->funit |= SYNC;
	} while ((curfile->funit & EOLN) == 0);
}
