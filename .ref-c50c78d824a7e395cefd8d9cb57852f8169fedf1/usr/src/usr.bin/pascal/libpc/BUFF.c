/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)BUFF.c 1.4 %G%";

#include "h00vars.h"

BUFF(amount)

	long		amount;
{
	struct iorec	*curfile;
	static char	sobuf[BUFSIZ];

	curfile = OUTPUT;
	if (amount == 0)
		setbuf(ACTFILE(curfile), 0);
	else if (amount == 2)
		setbuf(ACTFILE(curfile), sobuf);
}
