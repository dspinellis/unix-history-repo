/*
char id_unlink[] = "@(#)unlink_.c	1.1";
 *
 * unlink (remove) a file
 *
 * calling sequence:
 *	integer unlink
 *	ierror = unlink(filename)
 * where:
 *	ierror will be a returned status (0 == OK)
 *	filename is the file to be unlinked
 */

#include "../libI77/f_errno.h"

long
unlink_(fname, namlen)
char *fname;
long namlen;
{
	char buf[128];

	if (namlen >= sizeof buf)
		return((long)(errno=F_ERARG));
	g_char(fname, namlen, buf);
	if (unlink(buf) != 0)
		return((long)errno);
	return(0L);
}
