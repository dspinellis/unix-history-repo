/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * yes() returns 1 if the user replies with a word starting with `y',
 * otherwise zero. yes() eats up the rest of the line.
 */
#include <stdio.h>

yes()
{
	register int c;			/* current character */
	int status = 0;			/* return status */

	while ((c = getchar()) == ' ' || c == '\t')
		continue;
	if (c == 'y')
		status = 1;
	while (c != '\n')
		c = getchar();
	return(status);
}
