#ifndef lint
static char sccsid[] = "@(#)lastpart.c	5.3 (Berkeley) %G%";
#endif

/*******
 *	char *
 *	lastpart(file)	find last part of file name
 *	char *file;
 *
 *	return - pointer to last part
 */

char *
lastpart(file)
register char *file;
{
	register char *c;
	char *rindex();

	c = rindex(file, '/');
	if (c)
		return c + 1;
	else
		return file;
}
