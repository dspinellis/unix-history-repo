/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)time.h	5.8 (Berkeley) %G%
 */

#include <machine/types.h>

#ifndef	NULL
#define	NULL	0
#endif

#ifdef	_CLOCK_T_
typedef	_CLOCK_T_	clock_t;
#undef	_CLOCK_T_
#endif

#ifdef	_TIME_T_
typedef	_TIME_T_	time_t;
#undef	_TIME_T_
#endif

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

struct tm {
	int	tm_sec;		/* seconds after the minute [0-60] */
	int	tm_min;		/* minutes after the hour [0-59] */
	int	tm_hour;	/* hours since midnight [0-23] */
	int	tm_mday;	/* day of the month [1-31] */
	int	tm_mon;		/* months since January [0-11] */
	int	tm_year;	/* years since 1900 */
	int	tm_wday;	/* days since Sunday [0-6] */
	int	tm_yday;	/* days since January 1 [0-365] */
	int	tm_isdst;	/* Daylight Savings Time flag */
	long	tm_gmtoff;	/* offset from CUT in seconds */
	char	*tm_zone;	/* timezone abbreviation */
};

#if __STDC__ || c_plusplus
extern struct tm *gmtime(const time_t *);
extern struct tm *localtime(const time_t *);
extern time_t mktime(const struct tm *);
extern time_t time(time_t *);
extern clock_t clock(void);
extern double difftime(const time_t, const time_t);
extern char *asctime(const struct tm *);
extern char *ctime(const time_t *);
extern char *timezone(int , int);
extern void tzset(void);
extern void tzsetwall(void);
#else
extern struct tm *gmtime();
extern struct tm *localtime();
extern clock_t clock();
extern time_t mktime();
extern time_t time();
extern double difftime();
extern char *asctime();
extern char *ctime();
extern char *timezone();
extern void tzset();
extern void tzsetwall();
#endif 
