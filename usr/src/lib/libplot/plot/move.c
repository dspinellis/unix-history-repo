#ifndef lint
static char sccsid[] = "@(#)move.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
move(xi,yi){
	putc('m',stdout);
	putsi(xi);
	putsi(yi);
}
