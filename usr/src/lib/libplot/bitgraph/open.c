#ifndef lint
static char sccsid[] = "@(#)open.c	4.1 (Berkeley) %G%";
#endif

/*
 * Displays plot files on a BBN bitgraph terminal.
 */

#include <signal.h>
#include "bg.h"

int currentx = 0;
int currenty = 0;
double lowx = 0.0;
double lowy = 0.0;
double scale = 1.0;

openpl()
{
	int closepl();

	/* catch interupts */
	signal(SIGINT, closepl);
	currentx = 0;
	currenty = 0;

	space(0, 0, XMAX, YMAX);
}
