/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#include "clibrary.h"
#include "macros.h"


#if	defined(RES_RUSAGE)
#include <sys/time.h>
#include <sys/resource.h>

#define TIMEVALasDOUBLE(t)	\
    ((double)(t).tv_sec + ((double)(t).tv_usec) / 1000000.0)

int
GetResourceUsage(usertime, systime)
    double	*usertime;
    double	*systime;
{
    struct rusage	R;

    if (getrusage(RUSAGE_SELF, &R) < 0)
	return -1;
    *usertime = TIMEVALasDOUBLE(R.ru_utime);
    *systime = TIMEVALasDOUBLE(R.ru_stime);
    return 0;
}
#endif	/* defined(RES_RUSAGE) */


#if	defined(RES_TIMES)
#include <sys/param.h>
#include <sys/times.h>

#if	!defined(HZ)
#define HZ	60
#endif	/* !defined(HZ) */

#define CPUTIMEasDOUBLE(t1, t2)		((double)(t1 + t2) / (double)HZ)

int
GetResourceUsage(usertime, systime)
    double	*usertime;
    double	*systime;
{
    struct tms	T;

    if (times(&T) == -1)
	return -1;
    *usertime = CPUTIMEasDOUBLE(T.tms_utime, T.tms_cutime);
    *systime = CPUTIMEasDOUBLE(T.tms_stime, T.tms_cstime);
    return 0;
}
#endif	/* defined(RES_TIMES) */
