#include "ntp-config.h"
#ifdef	REFCLOCK
/*
 *  A dummy clock reading routine that reads the current system time.
 *  from the local host.  Its possible that this could be actually used
 *  if the system was in fact a very accurate time keeper (a true real-time
 *  system with good crystal clock or better).
 */
#include "ntp.h"
#include <sys/types.h>
#include <sys/time.h>

extern LLog *pgm_log;

/* ARGSUSED */
int init_clock_local(file)
char *file;
{
	struct intf *ap;
	int	acount;

	ap = getintf (&acount);
	ap -> name = "LOCAL";
	ap -> addr.type = 0;
	ap -> inum = acount;
	return acount;	/* invalid if we ever use it */
}

/* ARGSUSED */
read_clock_local(cfd, tvp, mtvp)
int cfd;
struct timeval **tvp, **mtvp;
{
	static struct timeval realtime, mytime;

	TRACE (2, ("read_local_clock"));
	(void) gettimeofday(&realtime, (struct timezone *)0);
	mytime = realtime;
	*tvp = &realtime;
	*mtvp = &mytime;
	return(0);
}
#endif
