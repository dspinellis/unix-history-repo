# include "../hdr/macros.h"
# include "errno.h"
# include "../hdr/fatal.h"
SCCSID(@(#)rename	2.1);

/*
	rename (unlink/link)
	Calls xlink() and xunlink().
*/

rename(oldname,newname)
char *oldname, *newname;
{
	extern int errno;

	if (unlink(newname) < 0 && errno != ENOENT)
		return(xunlink(newname));

	if (xlink(oldname,newname) == Fvalue)
		return(-1);
	return(xunlink(oldname));
}
