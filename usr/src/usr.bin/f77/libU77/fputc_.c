/*
char id_fputc[] = @(#)fputc_.c	1.1";
 *
 * write a character to a logical unit bypassing formatted I/O
 *
 * calling sequence:
 *	integer fputc
 *	ierror = fputc (unit, char)
 * where:
 *	char will be sent to the logical unit
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include	"../libI77/fiodefs.h"
#include	"../libI77/f_errno.h"

extern unit units[];	/* logical units table from iolib */

long fputc_(u, c, clen)
long *u; char *c; long clen;
{
	int i;

	if (*u < 0 || *u >= MXUNIT)
		return((long)(errno=F_ERARG));
	if (!units[*u].ufd)
		return((long)(errno=F_ERNOPEN));
	putc (*c, units[*u].ufd);
	if (ferror(units[*u].ufd))
	{
		i = errno;
		clearerr(units[*u].ufd);
		return((long)i);
	}
	return(0L);
}
