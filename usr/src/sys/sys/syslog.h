/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)syslog.h	4.5 (Berkeley) %G%
 */

/*
 *  Declarations for system logging program
 *
 *	These are used as the first parameter to syslog().
 */

	/* kernel priorities */
#define	KERN_EMERG	1	/* emergency -- send to all users (wall) */
#define	KERN_ALERT	2	/* alert -- system failure */
#define	KERN_ERR	3	/* hard errors */
#define	KERN_FAIL	4	/* table full/overflow */
#define	KERN_RECOV	5	/* recoverable errors (softecc) */
#define	KERN_INFO	6	/* inconsistency/configuration error */

	/* user abnormal conditions priorities */
#define	LOG_EMERG	7	/* system unusable -- send to all users */
#define	LOG_ALERT	8	/* missing files (e.g., /etc/utmp) */
#define	LOG_CRIT	9	/* critical conditions */
#define	LOG_ERR		10	/* init open faliures/fatal daemon errors */
#define	LOG_FAIL	11	/* getty failing, interface dropped */
#define	LOG_WARNING	12	/* non-fatal daemon errs */

	/* user priorities */
#define	LOG_SALERT	13	/* important information */
#define	LOG_SECURITY	14	/* root/su logins */
#define	LOG_FIXED	15	/* abnormal condition fixed (recovery action) */
#define	LOG_MAIL	16	/* mail failures */
#define	LOG_REJECT	17	/* login/daemon rejections */
#define	LOG_NOTICE	18	/* important info */

	/* user information priorities */
#define	LOG_INFO	19	/* informational message */
#define	LOG_INFO1	20	/* informational message */
#define	LOG_INFO2	21	/* informational message */
#define	LOG_INFO3	22	/* informational message */
#define	LOG_INFO4	23	/* informational message */
#define	LOG_INFO5	24	/* informational message */

	/* user debug/local priorities */
#define	LOG_DEBUG	25	/* debugging info */
#define	LOG_LOCAL1	26	/* reserved for local use */
#define	LOG_LOCAL2	27	/* reserved for local use */
#define	LOG_LOCAL3	28	/* reserved for local use */
#define	LOG_LOCAL4	29	/* reserved for local use */
#define	LOG_LOCAL5	30	/* reserved for local use */
#define	LOG_LOCAL6	31	/* reserved for local use */

/*
 *  Option flags for openlog.
 */
#define	LOG_PID		01	/* log the pid with each message */
#define	LOG_CONS	02	/* log on the console if errors in sending */
#define	LOG_ODELAY	04	/* delay open until syslog() is called */
