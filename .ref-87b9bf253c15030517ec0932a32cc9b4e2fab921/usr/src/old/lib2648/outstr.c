/*	outstr.c	4.1	83/03/09	*/
/*
 * Low level output routines
 */

#include "2648.h"

outstr(str)
char *str;
{
	while (*str)
		outchar(*str++);
}
