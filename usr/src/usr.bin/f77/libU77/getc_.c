/*
char id_getc[] = "@(#)getc_.c	1.1";
 *
 * get a character from the standard input
 *
 * calling sequence:
 *	integer getc
 *	ierror = getc (char)
 * where:
 *	char will be read from the standard input, usually the terminal
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include	"../libI77/f_errno.h"
#include	"../libI77/fiodefs.h"

extern unit units[];	/* logical units table from iolib */

long getc_(c, clen)
char *c; long clen;
{
	int i;

	if (!units[STDIN].ufd)
		return((long)(errno=F_ERNOPEN));
	if ((i = getc (units[STDIN].ufd)) < 0)
	{
		if (feof(units[STDIN].ufd))
			return(-1L);
		i = errno;
		clearerr(units[STDIN].ufd);
		return((long)i);
	}
	*c = i & 0177;
	return(0L);
}
