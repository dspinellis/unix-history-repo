/*  $Revision: 1.6 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include "configdata.h"


#if	defined(FDCOUNT_GETDTAB)
int
getfdcount()
{
    static int		size;

    if (size <= 0) {
	if ((size = getdtablesize()) < 0)
	    return -1;
    }
    return size;
}
#endif	/* defined(FDCOUNT_GETDTAB) */


#if	defined(FDCOUNT_GETRLIMIT)
#include <sys/time.h>
#include <sys/resource.h>

int
getfdcount()
{
    static int		size;
    struct rlimit	rl;

    if (size <= 0) {
	if (getrlimit(RLIMIT_NOFILE, &rl) < 0)
	    return -1;
	size = rl.rlim_cur;
    }
    return size;
}
#endif	/* defined(FDCOUNT_GETRLIMIT) */


#if	defined(FDCOUNT_SYSCONF)
#include <unistd.h>
#include <limits.h>

int
getfdcount()
{
    static int		size;

    if (size <= 0) {
	if ((size = sysconf(_SC_OPEN_MAX)) < 0)
	    return -1;
    }
    return size;
}
#endif	/* defined(FDCOUNT_SYSCONF) */


#if	defined(FDCOUNT_ULIMIT)
int
getfdcount()
{
    static int		size;

    if (size <= 0) {
	if ((size = ulimit(4, 0L)) < 0)
	    return -1;
    }
    return size;
}
#endif	/* defined(FDCOUNT_ULIMIT) */


#if	defined(FDCOUNT_CONSTANT)
int
getfdcount()
{
#if	defined(NOFILE)
    return NOFILE;
#else
    return 20;
#endif	/* defined(NOFILE) */
}
#endif	/* defined(FDCOUNT_CONSTANT) */
