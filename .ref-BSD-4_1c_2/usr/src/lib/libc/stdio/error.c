/* @(#)error.c	4.1 (Berkeley) 12/21/80 */
#include	<stdio.h>

_error(s)
register char *s;
{
	static reentered;

	if (reentered++)
		_exit(0177);
	write(2, s, strlen(s));
	exit(0176);
}
