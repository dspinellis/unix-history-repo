#ifndef lint
static char sccsid[] = "@(#)calloc.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Calloc - allocate and clear memory block
 */
char *
calloc(num, size)
	register unsigned num, size;
{
	extern char *malloc();
	register char *p;

	size *= num;
	if (p = malloc(size))
		bzero(p, size);
	return (p);
}

cfree(p, num, size)
	char *p;
	unsigned num;
	unsigned size;
{
	free(p);
}
