/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)opc.c 1.1 8/27/80";

#include "OPnames.h"

main()  {
	register int i;

	for (i = 0;  i < 256;  i++)
		if (otext[i])
			printf("#define O_%s %04o\n", otext[i]+1, i);
	exit(0);
}
