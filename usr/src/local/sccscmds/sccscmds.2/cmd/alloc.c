# include <sccs.h>

SCCSID(@(#)alloc.c	4.1);

alloc(i)
	int i;
{
	register char *cp = malloc(i);

	if (cp == 0)
		return (-1);
	return (cp);
}
