/*
char id_fseek[] = "@(#)fseek_.c	1.1";
 *
 * position a file associated with a fortran logical unit
 *
 * calling sequence:
 *	ierror = fseek(lunit, ioff, ifrom)
 * where:
 *	lunit is an open logical unit
 *	ioff is an offset in bytes relative to the position specified by ifrom
 *	ifrom	- 0 means 'beginning of the file'
 *		- 1 means 'the current position'
 *		- 2 means 'the end of the file'
 *	ierror will be 0 if successful, a system error code otherwise.
 */

#include	<stdio.h>
#include	"../libI77/f_errno.h"
#include	"../libI77/fiodefs.h"

extern unit units[];

long fseek_(lu, off, from)
long *lu, *off, *from;
{
	if (*lu < 0 || *lu >= MXUNIT || *from < 0 || *from > 2)
		return((long)(errno=F_ERARG));
	if (!units[*lu].ufd)
		return((long)(errno=F_ERNOPEN));
	if (fseek(units[*lu].ufd, *off, (int)*from) < 0)
		return((long)errno);
	return(0L);
}
