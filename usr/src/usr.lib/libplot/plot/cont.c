#ifndef lint
static char sccsid[] = "@(#)cont.c	4.1 (Berkeley) 6/27/83";
#endif

#include <stdio.h>
cont(xi,yi){
	putc('n',stdout);
	putsi(xi);
	putsi(yi);
}
