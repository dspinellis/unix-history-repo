/*	@(#)alldigits.c	4.1	(Melbourne)	82/02/21	*/

#include <ctype.h>

alldigits(s)
register char *s;
{
	register c;

	c = *s++;
	do {
		if (!isdigit(c))
			return(0);
	} while (c = *s++);
	return(1);
}
