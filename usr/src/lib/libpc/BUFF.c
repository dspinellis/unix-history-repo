/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)BUFF.c 1.1 10/29/80";

#include "h00vars.h"

extern char	_sobuf[];

BUFF(amount)

	int		amount;
{
	struct iorec	*curfile;

	curfile = OUTPUT;
	if (amount == 0)
		setbuf(0, ACTFILE(curfile));
	else if (amount == 2)
		setbuf(_sobuf, ACTFILE(curfile));
}
