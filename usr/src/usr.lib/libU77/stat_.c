/*
char id_stat[] = "@(#)stat_.c	1.1";
 *
 * get file status
 *
 * calling sequence:
 *	integer stat, statb(11)
 *	call stat (name, statb)
 * where:
 *	'statb' will receive the stat structure for file 'name'.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "../libI77/f_errno.h"

long stat_(name, stbuf, namlen)
char *name; long *stbuf, namlen;
{
	char buf[128];
	struct stat statb;

	if (namlen >= sizeof buf)
		return((long)(errno=F_ERARG));
	g_char(name, namlen, buf);
	if (stat(buf, &statb) == 0)
	{
		*stbuf++ = statb.st_dev;
		*stbuf++ = statb.st_ino;
		*stbuf++ = statb.st_mode;
		*stbuf++ = statb.st_nlink;
		*stbuf++ = statb.st_uid;
		*stbuf++ = statb.st_gid;
		*stbuf++ = statb.st_rdev;
		*stbuf++ = statb.st_size;
		*stbuf++ = statb.st_atime;
		*stbuf++ = statb.st_mtime;
		*stbuf++ = statb.st_ctime;
		return(0L);
	}
	return ((long)errno);
}
