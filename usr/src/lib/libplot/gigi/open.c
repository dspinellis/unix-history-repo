#ifndef lint
static char sccsid[] = "@(#)open.c	4.1 (Berkeley) %G%";
#endif

/*
 * Displays plot files on a gigi "graphics" terminal.
 */

#include <signal.h>
#include "gigi.h"

int currentx = 0;
int currenty = 0;
double lowx = 0.0;
double lowy = 0.0;
double scalex = 1.0;
double scaley = 1.0;

openpl()
{
	int closepl();

	/* catch interupts */
	signal(SIGINT, closepl);
	currentx = 0;
	currenty = 0;
	/* enter grapics mode */
	putchar(ESC); putchar('P'); putchar('p');

	/* set some parameters */
	printf("S(I0 T0 [0,0])");

	space(0, 0, XMAX, YMAX);
}
