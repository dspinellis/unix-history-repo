#ifndef lint
static char sccsid[] = "@(#)getchar.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * A subroutine version of the macro getchar.
 */
#include <stdio.h>

#undef getchar

getchar()
{
	return(getc(stdin));
}
