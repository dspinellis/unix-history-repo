/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)opc.c 4.1 10/10/80";

#include "OPnames.h"

main()  {
	register int i;

	printf("%s\n\n%s\n\n",
	    "/* Copyright (c) 1979 Regents of the University of California */",
	    "/* static	char sccsid[] = \"@(#)opc.c 4.1 10/10/80\"; */");
	for (i = 0;  i < 256;  i++)
		if (otext[i])
			printf("#define O_%s %04o\n", otext[i]+1, i);
	exit(0);
}
