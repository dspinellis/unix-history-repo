/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)DEFNAME.c 1.1 10/29/80";


#include "h00vars.h"

DEFNAME(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	int			maxnamlen;
	int			datasize;
{
	filep = GETNAME(filep, name, maxnamlen, datasize);
	filep->funit |= FDEF;
}
