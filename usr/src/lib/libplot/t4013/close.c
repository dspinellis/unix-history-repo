#ifndef lint
static char sccsid[] = "@(#)close.c	1.1 (Berkeley) %G%";
#endif

#include <stdio.h>
closevt(){
	putch(037);
	fflush(stdout);
}
closepl(){
	putch(037);
	fflush(stdout);
}
