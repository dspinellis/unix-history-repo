/*
char id_putc[] = "@(#)putc_.c	1.1";
 *
 * write a character to the standard output
 *
 * calling sequence:
 *	integer putc
 *	ierror =  putc (char)
 * where:
 *	char will be sent to the standard output, usually the terminal
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include	"../libI77/f_errno.h"
#include	"../libI77/fiodefs.h"

extern unit units[];	/* logical units table from iolib */

long putc_(c, clen)
char *c; long clen;
{
	int i;

	if (!units[STDOUT].ufd)
		return((long)(errno=F_ERNOPEN));
	putc (*c, units[STDOUT].ufd);
	if (ferror(units[STDOUT].ufd))
	{
		i = errno;
		clearerr(units[STDOUT].ufd);
		return((long)i);
	}
	return(0L);
}
