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
