/*
char id_fgetc[] = "@(#)fgetc_.c	1.2";
 *
 * get a character from a logical unit bypassing formatted I/O
 *
 * calling sequence:
 *	integer fgetc
 *	ierror = fgetc (unit, char)
 * where:
 *	char will return a character from logical unit
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include	"../libI77/fiodefs.h"
#include	"../libI77/f_errno.h"

extern unit units[];	/* logical units table from iolib */

long fgetc_(u, c, clen)
long *u; char *c; long clen;
{
	int i;

	if (*u < 0 || *u >= MXUNIT)
		return((long)(errno=F_ERUNIT));
	if (!units[*u].ufd)
		return((long)(errno=F_ERNOPEN));
	if ((i = getc (units[*u].ufd)) < 0)
	{
		if (feof(units[*u].ufd))
			return(-1L);
		i = errno;
		clearerr(units[*u].ufd);
		return((long)i);
	}
	*c = i & 0177;
	return(0L);
}
