/*
char id_putc[] = "@(#)putc_.c	1.2";
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
	int	i;
	unit	*lu;

	lu = &units[STDOUT];
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
