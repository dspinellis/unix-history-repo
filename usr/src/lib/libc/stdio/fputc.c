#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fputc.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <stdio.h>

fputc(c, fp)
register FILE *fp;
{
	return(putc(c, fp));
}
