#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
point(xi,yi){
	putc('p',stdout);
	putsi(xi);
	putsi(yi);
}
