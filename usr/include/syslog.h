/*	syslog.h	4.1	83/05/27	*/

/*
 *  SYSLOG.H -- declarations for system logging program
 *
 *	These are used as the first parameter to logmsg().
 *	Their meanings are approximately as follows:
 *
 *	LOG_ALERT -- this priority should essentially never
 *		be used.  It applies only to messages that
 *		are so important that every user should be
 *		aware of them, e.g., a serious hardware failure.
 *	LOG_SALERT -- messages of this priority should be
 *		issued only when immediate attention is needed
 *		by a qualified system person, e.g., when some
 *		valuable system resource dissappears.  They get
 *		sent to a list of system people.
 *	LOG_EMERG -- Emergency messages are not sent to users,
 *		but represent major conditions.  An example
 *		might be hard disk failures.  These could be
 *		logged in a separate file so that critical
 *		conditions could be easily scanned.
 *	LOG_ERR -- these represent error conditions, such as soft
 *		disk failures, etc.
 *	LOG_CRIT -- such messages contain critical information,
 *		but which can not be classed as errors, for example,
 *		'su' attempts.
 *	LOG_WARNING -- issued when an abnormal condition has been
 *		detected, but recovery can take place.
 *	LOG_NOTICE -- something that falls in the class of
 *		"important information"; this class is informational
 *		but important enough that you don't want to throw
 *		it away casually.
 *	LOG_INFO -- information level messages.  These messages
 *		could be thrown away without problems, but should
 *		be included if you want to keep a close watch on
 *		your system.
 *	LOG_DEBUG -- it may be useful to log certain debugging
 *		information.  Normally this will be thrown away.
 */

/* some configuration parameters..... */
#define LOG_IPC				/* set if using 4.2 IPC, else mpx */
#define LOG_HOST 	"localhost"	/* name of host to log on */

/* defines for priorities */
#define	LOG_ALERT	1	/* alert -- send to all users */
#define	LOG_SALERT	2	/* subalert -- send to special users */
#define	LOG_EMERG	3	/* emergency conditions */
#define	LOG_ERROR	4	/* error */
#define	LOG_ERR		4	/* synonym of LOG_ERROR */
#define	LOG_CRIT	5	/* critical information */
#define	LOG_WARNING	6	/* warning */
#define	LOG_NOTICE	7	/* important information */
#define	LOG_INFO	8	/* informational message */
#define	LOG_DEBUG	9	/* debug level info */

/*
 *  Mode parameters to initlog.
 */
#define	LOG_NULL	0	/* don't touch log */
#define	LOG_SOUT	1	/* log standard & diag output */
#define	LOG_DIAG	2	/* log diagnostic output */
#define	LOG_INDEP	3	/* log independently */
#define	LOG_CLOSE	4	/* close the log */

/*
 *  Status parameters to initlog.
 */
#define	LOG_PID		0001	/* log the pid with each message */
#define	LOG_TIME	0002	/* log the time with each message */
#define	LOG_COOLIT	0004	/* suppress priority stuff */
#define	LOG_DGRAM	0010	/* running over a datagram socket */
