/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NIL.c 1.4 %G%";

#include "h00vars.h"

char ENIL[] = "Pointer value out of legal range\n";

char *
NIL(ptr)
	char	*ptr;		/* pointer to struct */
{
	if (ptr > _maxptr || ptr < _minptr) {
		ERROR(ENIL, 0);
	}
	return ptr;
}
