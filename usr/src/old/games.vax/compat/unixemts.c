static char sccsid[] = "	unixemts.c	1.1	82/05/12	";

#include <stdio.h>
doemt(code) int code; {
	/* just print a message if not caught */
	fprintf(stderr,"EMT 0%o caught\n",code);
}
