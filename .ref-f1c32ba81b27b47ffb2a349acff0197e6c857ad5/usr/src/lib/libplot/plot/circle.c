#ifndef lint
static char sccsid[] = "@(#)circle.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
circle(x,y,r){
	putc('c',stdout);
	putsi(x);
	putsi(y);
	putsi(r);
}
