#ifndef lint
static char sccsid[] = "@(#)lastpart.c	5.1 (Berkeley) %G%";
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

	c = file + strlen(file);
	while (c >= file)
		if (*(--c) == '/')
			break;
	return(++c);
}
