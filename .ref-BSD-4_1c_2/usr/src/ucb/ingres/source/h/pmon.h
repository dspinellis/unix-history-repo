/*
**  MONITOR.H -- structures dealing with performance monitoring
**
**	Version:
**		@(#)pmon.h	7.1	2/5/81
*/

struct monitor
{
	long	mon_utime;	/* user time in milliseconds */
	long	mon_stime;	/* system time in milliseconds */
	long	mon_pread;	/* pages read */
	long	mon_pwrit;	/* pages written */
	long	mon_cread;	/* catalog pages read */
	long	mon_cwrit;	/* catalog pages written */
	long	mon_xread;	/* ISAM/BTREE index pages read */
};
