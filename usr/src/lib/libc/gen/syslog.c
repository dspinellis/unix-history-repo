/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)syslog.c	5.32 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/syslog.h>
#include <sys/uio.h>
#include <sys/errno.h>
#include <netdb.h>
#include <string.h>
#include <varargs.h>
#include <paths.h>
#include <stdio.h>

static int	LogFile = -1;		/* fd for log */
static int	connected;		/* have done connect */
static int	LogStat = 0;		/* status bits, set by openlog() */
static char	*LogTag = "syslog";	/* string to tag the entry with */
static int	LogFacility = LOG_USER;	/* default facility code */

/*
 * syslog, vsyslog --
 *	print message on log file; output is intended for syslogd(8).
 */
syslog(pri, fmt, args)
	int pri, args;
	char *fmt;
{
	vsyslog(pri, fmt, &args);
}

vsyslog(pri, fmt, ap)
	int pri;
	register char *fmt;
	va_list ap;
{
	register int cnt;
	register char *p;
	time_t now, time();
	int fd, saved_errno;
	char tbuf[2048], fmt_cpy[1024], *stdp, *ctime();

	/* check for invalid bits or no priority set */
	if (!LOG_PRI(pri) || (pri &~ (LOG_PRIMASK|LOG_FACMASK)))
		return;

	saved_errno = errno;

	/* set default facility if none specified */
	if ((pri & LOG_FACMASK) == 0)
		pri |= LogFacility;

	/* build the message */
	(void)time(&now);
	(void)sprintf(tbuf, "<%d>%.15s ", pri, ctime(&now) + 4);
	for (p = tbuf; *p; ++p);
	if (LogStat & LOG_PERROR)
		stdp = p;
	if (LogTag) {
		(void)strcpy(p, LogTag);
		for (; *p; ++p);
	}
	if (LogStat & LOG_PID) {
		(void)sprintf(p, "[%d]", getpid());
		for (; *p; ++p);
	}
	if (LogTag) {
		*p++ = ':';
		*p++ = ' ';
	}

	/* substitute error message for %m */
	{
		register char ch, *t1, *t2;
		char *strerror();

		for (t1 = fmt_cpy; ch = *fmt; ++fmt)
			if (ch == '%' && fmt[1] == 'm') {
				++fmt;
				for (t2 = strerror(saved_errno);
				    *t1 = *t2++; ++t1);
			}
			else
				*t1++ = ch;
		*t1 = '\0';
	}

	(void)vsprintf(p, fmt_cpy, ap);

	cnt = strlen(tbuf);

	/* output to stderr if requested */
	if (LogStat & LOG_PERROR) {
		struct iovec iov[2];
		register struct iovec *v = iov;

		v->iov_base = stdp;
		v->iov_len = cnt - (stdp - tbuf);
		++v;
		v->iov_base = "\n";
		v->iov_len = 1;
		(void)writev(STDERR_FILENO, iov, 2);
	}

	/* get connected, output the message to the local logger */
	if ((connected || !openlog(LogTag, LogStat | LOG_NDELAY, 0)) &&
	    send(LogFile, tbuf, cnt, 0) >= 0)
		return;

	/* see if should attempt the console */
	if (!(LogStat&LOG_CONS))
		return;

	/*
	 * Output the message to the console; don't worry about blocking,
	 * if console blocks everything will.  Make sure the error reported
	 * is the one from the syslogd failure.
	 */
	if ((fd = open(_PATH_CONSOLE, O_WRONLY, 0)) >= 0) {
		(void)strcat(tbuf, "\r\n");
		cnt += 2;
		p = index(tbuf, '>') + 1;
		(void)write(fd, p, cnt - (p - tbuf));
		(void)close(fd);
	}
}

static struct sockaddr SyslogAddr;	/* AF_UNIX address of local logger */

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
		(void)strncpy(SyslogAddr.sa_data, _PATH_LOG,
		    sizeof(SyslogAddr.sa_data));
		if (LogStat & LOG_NDELAY) {
			if ((LogFile = socket(AF_UNIX, SOCK_DGRAM, 0)) == -1)
				return;
			(void)fcntl(LogFile, F_SETFD, 1);
		}
	}
	if (LogFile != -1 && !connected)
		if (connect(LogFile, &SyslogAddr, sizeof(SyslogAddr)) == -1) {
			(void)close(LogFile);
			LogFile = -1;
		} else
			connected = 1;
}

closelog()
{
	(void)close(LogFile);
	LogFile = -1;
	connected = 0;
}

static int	LogMask = 0xff;		/* mask of priorities to be logged */

/* setlogmask -- set the log mask level */
setlogmask(pmask)
	int pmask;
{
	int omask;

	omask = LogMask;
	if (pmask != 0)
		LogMask = pmask;
	return (omask);
}
