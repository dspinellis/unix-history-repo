/* @(#)tempnam.c	4.1 (Berkeley) %G% */
char *tmpnam(s)
char *s;
{
	static seed;

	sprintf(s, "temp.%d.%d", getpid(), seed++);
	return(s);
}
