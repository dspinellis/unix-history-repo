/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)syslog.c	5.15 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */


/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it
 * outputs to the log file instead of the standard output.
 * Also:
 *	adds a timestamp,
 *	prints the module name in front of the message,
 *	has some other formatting types (or will sometime),
 *	adds a newline on the end of the message.
 *
 * The output of this routine is intended to be read by /etc/syslogd.
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/syslog.h>
#include <netdb.h>
#include <strings.h>

#define	MAXLINE	1024			/* max message size */
#define NULL	0			/* manifest */

#define IMPORTANT 	LOG_ERR

static char	logname[] = "/dev/log";
static char	ctty[] = "/dev/console";

static int	LogFile = -1;		/* fd for log */
static int	connected;		/* have done connect */
static int	LogStat	= 0;		/* status bits, set by openlog() */
static char	*LogTag = "syslog";	/* string to tag the entry with */
static int	LogMask = 0xff;		/* mask of priorities to be logged */
static int	LogFacility = LOG_USER;	/* default facility code */

static struct sockaddr SyslogAddr;	/* AF_UNIX address of local logger */

extern	int errno, sys_nerr;
extern	char *sys_errlist[];

syslog(pri, fmt, p0, p1, p2, p3, p4)
	int pri;
	char *fmt;
{
	char buf[MAXLINE + 1], outline[MAXLINE + 1];
	register char *b, *f, *o;
	register int c;
	long now;
	int pid, olderrno = errno;

	/* see if we should just throw out this message */
	if ((unsigned) LOG_FAC(pri) >= LOG_NFACILITIES ||
	    LOG_MASK(LOG_PRI(pri)) == 0 ||
	    (pri &~ (LOG_PRIMASK|LOG_FACMASK)) != 0)
		return;
	if (LogFile < 0 || !connected)
		openlog(LogTag, LogStat | LOG_NDELAY, 0);

	/* set default facility if none specified */
	if ((pri & LOG_FACMASK) == 0)
		pri |= LogFacility;

	/* build the message */
	o = outline;
	(void)sprintf(o, "<%d>", pri);
	o += strlen(o);
	time(&now);
	(void)sprintf(o, "%.15s ", ctime(&now) + 4);
	o += strlen(o);
	if (LogTag) {
		strcpy(o, LogTag);
		o += strlen(o);
	}
	if (LogStat & LOG_PID) {
		(void)sprintf(o, "[%d]", getpid());
		o += strlen(o);
	}
	if (LogTag) {
		strcpy(o, ": ");
		o += 2;
	}

	b = buf;
	f = fmt;
	while ((c = *f++) != '\0' && c != '\n' && b < &buf[MAXLINE]) {
		if (c != '%') {
			*b++ = c;
			continue;
		}
		if ((c = *f++) != 'm') {
			*b++ = '%';
			*b++ = c;
			continue;
		}
		if ((unsigned)olderrno > sys_nerr)
			(void)sprintf(b, "error %d", olderrno);
		else
			strcpy(b, sys_errlist[olderrno]);
		b += strlen(b);
	}
	*b++ = '\n';
	*b = '\0';
	(void)sprintf(o, buf, p0, p1, p2, p3, p4);
	c = strlen(outline);
	if (c > MAXLINE)
		c = MAXLINE;

	/* output the message to the local logger */
	if (send(LogFile, outline, c, 0) >= 0)
		return;
	if (!(LogStat & LOG_CONS))
		return;

	/* output the message to the console */
	pid = vfork();
	if (pid == -1)
		return;
	if (pid == 0) {
		int fd;

		signal(SIGALRM, SIG_DFL);
		sigsetmask(sigblock(0L) & ~sigmask(SIGALRM));
		alarm(5);
		fd = open(ctty, O_WRONLY);
		alarm(0);
		strcat(o, "\r");
		o = index(outline, '>') + 1;
		write(fd, o, c + 1 - (o - outline));
		close(fd);
		_exit(0);
	}
	if (!(LogStat & LOG_NOWAIT))
		while ((c = wait((int *)0)) > 0 && c != pid)
			;
}

/*
 * OPENLOG -- open system log
 */

openlog(ident, logstat, logfac)
	char *ident;
	int logstat, logfac;
{
	if (ident != NULL)
		LogTag = ident;
	LogStat = logstat;
	if (logfac != 0 && (logfac &~ LOG_FACMASK) == 0)
		LogFacility = logfac;
	if (LogFile == -1) {
		SyslogAddr.sa_family = AF_UNIX;
		strncpy(SyslogAddr.sa_data, logname, sizeof SyslogAddr.sa_data);
		if (LogStat & LOG_NDELAY) {
			LogFile = socket(AF_UNIX, SOCK_DGRAM, 0);
			fcntl(LogFile, F_SETFD, 1);
		}
	}
	if (LogFile != -1 && !connected &&
	    connect(LogFile, &SyslogAddr, sizeof(SyslogAddr)) != -1)
		connected = 1;
}

/*
 * CLOSELOG -- close the system log
 */

closelog()
{

	(void) close(LogFile);
	LogFile = -1;
	connected = 0;
}

/*
 * SETLOGMASK -- set the log mask level
 */
setlogmask(pmask)
	int pmask;
{
	int omask;

	omask = LogMask;
	if (pmask != 0)
		LogMask = pmask;
	return (omask);
}
