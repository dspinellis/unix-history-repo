/*
char id_ftell[] = "@(#)ftell_.c	1.1";
 *
 * return current file position
 *
 * calling sequence:
 *	integer curpos, ftell
 *	curpos = ftell(lunit)
 * where:
 *	lunit is an open logical unit
 *	curpos will be the current offset in bytes from the start of the
 *		file associated with that logical unit
 *		or a (negative) system error code.
 */

#include	"../libI77/fiodefs.h"
#include	"../libI77/f_errno.h"

extern unit units[];

long ftell_(lu)
long *lu;
{
	if (*lu < 0 || *lu >= MXUNIT)
		return(-(long)(errno=F_ERARG));
	if (!units[*lu].ufd)
		return(-(long)(errno=F_ERNOPEN));
	return(ftell(units[*lu].ufd));
}
