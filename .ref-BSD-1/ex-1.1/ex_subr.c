#
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

#include "ex.h"

skipwh()
{
	register int wh;

	wh = 0;
	while (white(peekchar())) {
		wh++;
		getchar();
	}
	return (wh);
}


strcmp(left, right)
	register char *left, *right;
{

	while (*left == *right++)
		if (*left++ == 0)
			return (0);
	return (*left - *--right);
}

letter(c)
	register int c;
{

	if (c >= 'a' && c <= 'z')
		return (c);
	if (c >= 'A' && c <= 'Z')
		return (c + 'a' - 'A');
	return (0);
}

strlen(cp)
	register char *cp;
{
	register int i;

	i = 0;
	while (*cp++)
		i++;
	return (i);
}

white(c)
	char c;
{

	return (c == ' ' || c == '\t');
}

digit(c)
	char c;
{
	return (c >= '0' && c <= '9');
}

ucletter(c)
	char c;
{
	return (c >= 'A' && c <= 'Z');
}
