#ifndef lint
static char sccsid[] = "@(#)close.c	4.1 (Berkeley) %G%";
#endif

#include <signal.h>
#include "dumb.h"

closepl()
{
	int i, j;

	for(j=0; j<LINES; j++){
		for(i=0; i<COLS; i++){
			printf("%c", screenmat[i][j]);
		}
		printf("\n");
	}
	signal(SIGINT, SIG_IGN);
	exit(0);
}
