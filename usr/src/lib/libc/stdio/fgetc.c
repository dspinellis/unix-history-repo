#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fgetc.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <stdio.h>

fgetc(fp)
FILE *fp;
{
	return(getc(fp));
}
