#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fakcu.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * Null cleanup routine to resolve reference in exit() 
 * if not using stdio.
 */
_cleanup()
{
}
