#ifndef lint
static char sccsid[] = "@(#)close.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
closevt(){
	fflush(stdout);
}
closepl(){
	fflush(stdout);
}
