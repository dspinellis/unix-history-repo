/* @(#)tell.c	4.1 (Berkeley) %G% */
/*
 * return offset in file.
 */

long	lseek();

long tell(f)
{
	return(lseek(f, 0L, 1));
}
