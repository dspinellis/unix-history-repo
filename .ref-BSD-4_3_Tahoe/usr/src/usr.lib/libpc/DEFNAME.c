/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)DEFNAME.c 1.2 3/7/81";

#include "h00vars.h"

DEFNAME(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	filep = GETNAME(filep, name, maxnamlen, datasize);
	filep->funit |= FDEF;
}
