/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)opoff.c 4.1 10/10/80";

#include "OPnames.h"

main()  {
	register int i;

	for (i = 0;  i < 256;  i++)
		if (otext[i] && *otext[i] != '*')
			printf("\t.word\t_%s-optab\n", otext[i]+1);
		else
			printf("\t.word\tbadop-optab\n");
	printf("badop:\n");
	printf("\tincl\tr10\n");
	printf("\tmovw\t$EBADOP,_perrno\n");
	printf("\tjbr\terror\n");
	exit(0);
}
