#ifndef lint
static char sccsid[] = "@(#)erase.c	4.1 (Berkeley) %G%";
#endif

#include "hp2648.h"

erase()
{
	buffready(8);
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(DISPLAY);
	putchar('a');
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(PLOT);
	putchar(BINARY);
}
