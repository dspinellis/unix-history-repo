/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)HALT.c 1.3 6/10/81";

#include "h00vars.h"

HALT()
{
		PFLUSH();
		fputs("Call to procedure halt\n", stderr);
		PCEXIT(0);
}
