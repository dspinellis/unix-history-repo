static char *sccsid = "@(#)echo.c	4.2 (Berkeley) 5/27/85";
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	register int i, nflg;

	nflg = 0;
	if(argc > 1 && argv[1][0] == '-' && argv[1][1] == 'n' && !argv[1][2]) {
		nflg++;
		argc--;
		argv++;
	}
	for(i=1; i<argc; i++) {
		fputs(argv[i], stdout);
		if (i < argc-1)
			putchar(' ');
	}
	if(nflg == 0)
		putchar('\n');
	exit(0);
}
