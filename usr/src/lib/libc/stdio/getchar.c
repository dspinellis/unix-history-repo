/* @(#)getchar.c	4.1 (Berkeley) %G% */
/*
 * A subroutine version of the macro getchar.
 */
#include <stdio.h>

#undef getchar

getchar()
{
	return(getc(stdin));
}
