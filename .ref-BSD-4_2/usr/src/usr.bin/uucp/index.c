#ifndef lint
static char sccsid[] = "@(#)index.c	5.1 (Berkeley) 7/2/83";
#endif

#include <stdio.h>

/*******
 *	char *
 *	index(str, c)	return pointer to character c
 *	char c, *str;
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
			return(str);
	}

	return(NULL);
}
