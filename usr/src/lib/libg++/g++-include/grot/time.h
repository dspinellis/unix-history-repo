#ifndef time_h
#define time_h 1

// this kludge will probably work on most systems

#include <stddef.h>  /* sorry, but needed for DGUX */
#include <stdio.h>   /* sorry, but needed for USG */

#include <sys/types.h>

extern "C" {

#define gmtime c_proto_gmtime
#define localtime c_proto_localtime
#define asctime c_proto_asctime
#define ctime c_proto_ctime
#define tzset c_proto_tzset
#define tzsetwall c_proto_tzsetwall
#define  timezone c_proto_timezone
#define getitimer c_proto_getitimer
#define setitimer c_proto_setitimer
#define gettimeofday c_proto_gettimeofday
#define settimeofday c_proto_settimeofday

#define KERNEL

#ifdef VMS
	struct  unix_time
	{
		long int	tv_sec;
		long int	tv_usec;
	};

	struct rusage
	{
		struct unix_time	ru_utime;
	};

#define RUSAGE_SELF 0		//define it, it will be unused
#else
#if !defined(USG) || defined(hpux)
#ifdef hpux
#define _INCLUDE_POSIX_SOURCE
#endif
#include "//usr/include/sys/time.h"
#endif
#include "//usr/include/sys/times.h"
#ifndef __NeXT__
#include "//usr/include/time.h"
#endif
#endif
#undef KERNEL

#undef gmtime 
#undef localtime 
#undef asctime 
#undef ctime 
#undef tzset 
#undef tzsetwall 
#undef timezone 
#undef getitimer
#undef setitimer
#undef gettimeofday
#undef settimeofday

extern struct tm* localtime(long*);
extern struct tm* gmtime(long*);
extern char* ctime(long*);
extern char* asctime(struct tm*);
extern void tzset();
extern void tzsetwall();

#ifdef convex
extern clock_t times(struct tms*);
#elif defined(hpux)
extern unsigned long times(struct tms*);
#else
extern long times(struct tms*); 
#endif

#if defined(USG)
extern long timezone;
#ifdef hpux
extern int getitimer(int, struct itimerval*);
extern int setitimer(int, struct itimerval*, struct itimerval*);
extern int gettimeofday(struct timeval*, struct timezone*);
extern int settimeofday(struct timeval*, struct timezone*);
#endif
extern int daylight;
extern char* tzname[];
#else
typedef struct c_proto_timezone timezone;
extern char* timezone(int, int);
extern int getitimer(int, struct itimerval*);
extern int setitimer(int, struct itimerval*, struct itimerval*);
extern int gettimeofday(struct timeval*, struct timezone*);
extern int settimeofday(struct timeval*, struct timezone*);

#endif

}
#else
#ifdef ctime
/* What to do if "//usr/include/sys/time.h" tries to include us.  */
#include "//usr/include/time.h"
#endif
#endif


