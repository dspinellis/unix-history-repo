#ifndef lint
static char sccsid[] = "@(#)line.c	4.1 (Berkeley) 6/27/83";
#endif

#include <stdio.h>
line(x0,y0,x1,y1){
	putc('l',stdout);
	putsi(x0);
	putsi(y0);
	putsi(x1);
	putsi(y1);
}
