/*
char id_chdir[] = "@(#)chdir_.c	1.1";
 *
 * change default directory
 *
 * calling sequence:
 *	integer chdir
 *	ierror = chdir(dirname)
 * where:
 *	ierror will receive a returned status (0 == OK)
 *	dirname is the directory name
 */

#include "../libI77/f_errno.h"

long chdir_(dname, dnamlen)
char *dname;
long dnamlen;
{
	char buf[128];

	g_char(dname, dnamlen, buf);
	if (buf[0] == '\0')
		return((long)(errno=F_ERARG));
	if (chdir(buf) != 0)
		return((long)errno);
	return(0L);
}
