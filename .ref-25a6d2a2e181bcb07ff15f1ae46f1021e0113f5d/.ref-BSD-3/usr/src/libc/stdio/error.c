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
