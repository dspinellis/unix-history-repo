/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)NIL.c 1.2 6/10/81";

#include "h00vars.h"

char *
NIL(ptr)

	char	*ptr;		/* pointer to struct */
{
	if (ptr > _maxptr || ptr < _minptr) {
		ERROR("Pointer value out of legal range\n", 0);
		return;
	}
	return ptr;
}
