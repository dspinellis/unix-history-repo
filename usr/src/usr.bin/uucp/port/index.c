#ifndef lint
static char sccsid[] = "@(#)index.c	5.2 (Berkeley) %G%";
#endif

#include <stdio.h>

/*
 *	return pointer to character c
 *
 *	return codes:
 *		NULL  -  character not found
 *		pointer  -  pointer to character
 */

char *
index(str, c)
register char c, *str;
{
	for (; *str != '\0'; str++) {
		if (*str == c)
			return str;
	}

	return NULL;
}
