/*  $Revision: 1.10 $
**  Modified by Rich $alz <rsalz@osf.org> to be more portable to older
**  systems.
*/
/*#define INET_SYSLOG */
/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)syslog.c	5.28 (Berkeley) 6/27/90";
#endif /* LIBC_SCCS and not lint */

/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it outputs to the
 * log file instead of the standard output.  Also:
 *	adds a timestamp,
 *	prints the module name in front of the message,
 *	has some other formatting types (or will sometime),
 *	adds a newline on the end of the message.
 *
 * The output of this routine is intended to be read by syslogd(8).
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include "syslog.h"
#if	!defined(INET_SYSLOG)
#include <sys/uio.h>
#else
#include <uio.h>
#include <fcntl.h>
#endif	/* !defined(INET_SYSLOG) */
#include <varargs.h>
#include <stdio.h>
#if	defined(INET_SYSLOG)
#include <netinet/in.h>
#endif	/* defined(INET_SYSLOG) */

#ifndef	_PATH_LOGNAME
    /* if you are running an old syslog, change /dev/log to the same as it
     * is in syslogd.c */
#define	_PATH_LOGNAME	"/dev/log"
#endif
#define index		strchr

#define	_PATH_CONSOLE	"/dev/console"

static int	LogFile = -1;		/* fd for log */
static int	connected;		/* have done connect */
static int	LogStat = 0;		/* status bits, set by openlog() */
static char	*LogTag = "syslog";	/* string to tag the entry with */
static int	LogFacility = LOG_USER;	/* default facility code */

extern char *index(), *strcpy(), *strncpy(), *strcat();
/* =()<extern @<SIZE_T>@	strlen();>()= */
extern size_t	strlen();

/*
**  Return a string representation of errno.
*/
static char *
xstrerror(e)
    int		e;
{
    extern int	sys_nerr;
    extern char	*sys_errlist[];
    static char	buff[30];

    if (e >= 0 && e < sys_nerr)
	return sys_errlist[e];
    (void)sprintf(buff, "Error code %d\n", e);
    return buff;
}

/* VARARGS0 */
syslog(va_alist)
    va_dcl
{
	int pri;
	char *fmt;
	va_list ap;

	va_start(ap);
	pri = va_arg(ap, int);
	fmt = va_arg(ap, char*);
	vsyslog(pri, fmt, ap);
	va_end(ap);
}

vsyslog(pri, fmt, ap)
	int pri;
	register char *fmt;
	va_list ap;
{
	extern int errno;
	register int cnt;
	register char *p;
	time_t now, time();
	int fd, saved_errno;
	char tbuf[2048], fmt_cpy[1024], *stdp, *ctime();

	saved_errno = errno;

	/* see if we should just throw out this message */
	if (!LOG_MASK(LOG_PRI(pri)) || (pri &~ (LOG_PRIMASK|LOG_FACMASK)))
		return;
	if (LogFile < 0 || !connected)
		openlog(LogTag, LogStat | LOG_NDELAY, 0);

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
		char *xstrerror();

		for (t1 = fmt_cpy; ch = *fmt; ++fmt)
			if (ch == '%' && fmt[1] == 'm') {
				++fmt;
				for (t2 = xstrerror(saved_errno);
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
		(void)writev(2, iov, 2);
	}

	/* output the message to the local logger */
	if (send(LogFile, tbuf, cnt, 0) >= 0 || !(LogStat&LOG_CONS))
		return;

	/*
	 * output the message to the console; don't worry about
	 * blocking, if console blocks everything will.
	 */
	if ((fd = open(_PATH_CONSOLE, O_WRONLY, 0)) < 0)
		return;
	(void)strcat(tbuf, "\r\n");
	cnt += 2;
	p = index(tbuf, '>') + 1;
	(void)write(fd, p, cnt - (p - tbuf));
	(void)close(fd);
}

#if	!defined(INET_SYSLOG)
static struct sockaddr SyslogAddr;	/* AF_UNIX address of local logger */
#else
static struct sockaddr_in SyslogAddr;	/* AF_INET address of local logger */
#endif	/* !defined(INET_SYSLOG) */
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
#if	!defined(INET_SYSLOG)
		SyslogAddr.sa_family = AF_UNIX;
		strncpy(SyslogAddr.sa_data, _PATH_LOGNAME,
		    sizeof(SyslogAddr.sa_data));
		if (LogStat & LOG_NDELAY) {
			LogFile = socket(AF_UNIX, SOCK_DGRAM, 0);
			fcntl(LogFile, F_SETFD, 1);
		}
#else
		SyslogAddr.sin_family = AF_INET;
		SyslogAddr.sin_port = htons(514);
		SyslogAddr.sin_addr.s_addr = INADDR_ANY;
  		if (LogStat & LOG_NDELAY) {
			LogFile = socket(AF_INET, SOCK_DGRAM, 0);
  			fcntl(LogFile, F_SETFD, 1);
  		}
#endif	/* !defined(INET_SYSLOG) */
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

static int	LogMask = 0xff;		/* mask of priorities to be logged */
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
