/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#include <sys/wait.h>

#if	defined(DO_USE_UNION_WAIT)
typedef union wait	WAITER;
#if	defined(WEXITSTATUS)
#define WAITVAL(x)	(WEXITSTATUS(x))
#else
#define WAITVAL(x)	((x).w_retcode)
#endif	/* defined(WEXITSTATUS) */
#else
typedef int		WAITER;
#define WAITVAL(x)	(((x) >> 8) & 0xFF)
#endif	/* defined(DO_USE_UNION_WAIT) */

int
waitnb(statusp)
    int		*statusp;
{
    WAITER	w;
    int		pid;

#if	defined(DO_HAVE_WAITPID)
    pid = waitpid(-1, &w, WNOHANG);
#endif	/* defined(DO_HAVE_WAITPID) */

#if	defined(DONT_HAVE_WAITPID)
    pid = wait3(&w, WNOHANG, (struct rusage *)NULL);
#endif	/* defined(DONT_HAVE_WAITPID) */

    if (pid > 0)
	*statusp = WAITVAL(w);
    return pid;
}
