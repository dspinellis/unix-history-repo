/* @(#)getchar.c	4.1 (Berkeley) 12/21/80 */
/*
 * A subroutine version of the macro getchar.
 */
#include <stdio.h>

#undef getchar

getchar()
{
	return(getc(stdin));
}
