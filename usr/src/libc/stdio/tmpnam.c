/* @(#)tmpnam.c	4.1 (Berkeley) 12/21/80 */
char *tmpnam(s)
char *s;
{
	static seed;

	sprintf(s, "temp.%d.%d", getpid(), seed++);
	return(s);
}
