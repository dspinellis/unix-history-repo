/* @(#)tell.c	4.1 (Berkeley) 12/21/80 */
/*
 * return offset in file.
 */

long	lseek();

long tell(f)
{
	return(lseek(f, 0L, 1));
}
