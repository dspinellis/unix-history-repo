/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)syslog.h	7.9 (Berkeley) %G%
 */

/*
 *  Facility codes
 */

#define LOG_KERN	(0<<3)	/* kernel messages */
#define LOG_USER	(1<<3)	/* random user-level messages */
#define LOG_MAIL	(2<<3)	/* mail system */
#define LOG_DAEMON	(3<<3)	/* system daemons */
#define LOG_AUTH	(4<<3)	/* security/authorization messages */
#define LOG_SYSLOG	(5<<3)	/* messages generated internally by syslogd */
#define LOG_LPR		(6<<3)	/* line printer subsystem */
#define LOG_NEWS	(7<<3)	/* network news subsystem */
#define LOG_UUCP	(8<<3)	/* UUCP subsystem */
	/* other codes through 15 reserved for system use */
#define LOG_LOCAL0	(16<<3)	/* reserved for local use */
#define LOG_LOCAL1	(17<<3)	/* reserved for local use */
#define LOG_LOCAL2	(18<<3)	/* reserved for local use */
#define LOG_LOCAL3	(19<<3)	/* reserved for local use */
#define LOG_LOCAL4	(20<<3)	/* reserved for local use */
#define LOG_LOCAL5	(21<<3)	/* reserved for local use */
#define LOG_LOCAL6	(22<<3)	/* reserved for local use */
#define LOG_LOCAL7	(23<<3)	/* reserved for local use */

#define LOG_NFACILITIES	24	/* maximum number of facilities */
#define LOG_FACMASK	0x03f8	/* mask to extract facility part */

#define LOG_FAC(p)	(((p) & LOG_FACMASK) >> 3)	/* facility of pri */

/*
 *  Priorities (these are ordered)
 */

#define LOG_EMERG	0	/* system is unusable */
#define LOG_ALERT	1	/* action must be taken immediately */
#define LOG_CRIT	2	/* critical conditions */
#define LOG_ERR		3	/* error conditions */
#define LOG_WARNING	4	/* warning conditions */
#define LOG_NOTICE	5	/* normal but signification condition */
#define LOG_INFO	6	/* informational */
#define LOG_DEBUG	7	/* debug-level messages */

#define LOG_PRIMASK	0x0007	/* mask to extract priority part (internal) */
#define LOG_PRI(p)	((p) & LOG_PRIMASK)	/* extract priority */

#define	LOG_MAKEPRI(fac, pri)	(((fac) << 3) | (pri))

#ifdef KERNEL
#define LOG_PRINTF	-1	/* pseudo-priority to indicate use of printf */
#endif

/*
 * arguments to setlogmask.
 */
#define	LOG_MASK(pri)	(1 << (pri))		/* mask for one priority */
#define	LOG_UPTO(pri)	((1 << ((pri)+1)) - 1)	/* all priorities through pri */

/*
 *  Option flags for openlog.
 *
 *	LOG_ODELAY no longer does anything; LOG_NDELAY is the
 *	inverse of what it used to be.
 */
#define	LOG_PID		0x01	/* log the pid with each message */
#define	LOG_CONS	0x02	/* log on the console if errors in sending */
#define	LOG_ODELAY	0x04	/* delay open until first syslog() (default) */
#define LOG_NDELAY	0x08	/* don't delay open */
#define LOG_NOWAIT	0x10	/* if forking to log on console, don't wait() */
