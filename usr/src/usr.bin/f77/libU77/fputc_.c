/*
char id_fputc[] = @(#)fputc_.c	1.3";
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
	int	i;
	unit	*lu;

	if (*u < 0 || *u >= MXUNIT)
		return((long)(errno=F_ERUNIT));
	lu = &units[*u];
	if (!lu->ufd)
		return((long)(errno=F_ERNOPEN));
	if (!lu->uwrt)
		nowwriting(lu);
	putc (*c, lu->ufd);
	if (ferror(lu->ufd))
	{
		i = errno;
		clearerr(lu->ufd);
		return((long)i);
	}
	return(0L);
}
