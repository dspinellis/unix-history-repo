#if !defined (BSD43) && !defined(hpux)
#ifndef lint
static char SccsId[] =	"@(#)syslog.c	4.1 (Berkeley) 5/27/83";
#endif
#ifndef	lint
static char ident[] = "@(#)$Id: syslog.c,v 1.9 1993/08/25 17:23:42 jromine Exp $";
#endif	/* lint */

/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it
 * outputs to the log file instead of the standard output.
 * Also, it prints the module name in front of lines,
 * and has some other formatting types (or will sometime).
 * Also, it adds a newline on the end of messages.
 *
 * The output of this routine is intended to be read by
 * /etc/syslog, which will add timestamps.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "syslog.h"
#include <netdb.h>

#define	MAXLINE	1024		/* max message size */
#define BUFSLOP	20		/* space to allow for "extra stuff" */
#ifndef NULL
#define NULL	0		/* manifest */
#endif

#define LOG_COOLIT	LOG_LOCAL0	/* local syslog code */
#define LOG_DGRAM	LOG_LOCAL1	/* idem */

#ifndef	LOG_HOST
#define LOG_HOST	"localhost"	/* host where syslogd is running */
#endif	/* LOG_HOST */

int	LogFile = -1;		/* fd for log */
int	LogStat	= 0;		/* status bits, set by initlog */
char	*LogTag = (char *)NULL;	/* string to tag the entry with */
int	LogMask = LOG_DEBUG;	/* lowest priority to be logged */

struct sockaddr_in SyslogAddr;
static char *SyslogHost = LOG_HOST;

extern	int errno;
#ifndef	BSD44
extern	int	sys_nerr;
extern	char *sys_errlist[];
#endif

syslog(pri, fmt, p0, p1, p2, p3, p4)
	int pri;
	char *fmt;
{
	char buf[MAXLINE+BUFSLOP], outline[MAXLINE + 1];
	register char *b, *f;

	if (LogFile < 0)
		openlog(0, 0);
	/* see if we should just throw out this message */
	if (pri > LogMask)
		return;
	for (b = buf, f = fmt; f && *f; b = buf) {
		register char c;

		if (pri > 0 && (LogStat & LOG_COOLIT) == 0) {
			sprintf(b, "<%d>", pri);
			b += strlen(b);
		}
		if (LogStat & LOG_PID) {
			sprintf(b, "%d ", getpid());
			b += strlen(b);
		}
		if (LogTag) {
			sprintf(b, "%s: ", LogTag);
			b += strlen(b);
		}
		while ((c = *f++) != '\0' && c != '\n' && b < buf + MAXLINE) {
			if (c != '%') {
				*b++ = c;
				continue;
			}
			c = *f++;
			if (c != 'm') {
#ifndef	notdef
				*b++ = '%', *b++ = c, *b++ = '\0';
#else
				*b++ = '%', *b++ = c;
#endif
				continue;
			}
			if ((unsigned)errno > sys_nerr)
				sprintf(b, "error %d", errno);
			else
				strcat(b, sys_errlist[errno]);
			b += strlen(b);
		}
		if (c == '\0')
			f--;
		*b++ = '\n', *b = '\0';
		sprintf(outline, buf, p0, p1, p2, p3, p4);
		errno = 0;
		if (LogStat & LOG_DGRAM)
			(void) sendto(LogFile, outline, strlen(outline), 0,
				   &SyslogAddr, sizeof SyslogAddr);
		else
			(void) write(LogFile, outline, strlen(outline));
		if (errno)
			perror("syslog: sendto");
	}
}

/*
 * OPENLOG -- open system log
 */
openlog(ident, logstat)
	char *ident;
	int logstat;
{
	struct servent *sp;
	struct hostent *hp;

	LogTag = ident;
	LogStat = logstat;
	if (LogFile >= 0)
		return;
	sp = getservbyname("syslog", "udp");
	hp = gethostbyname(SyslogHost);
	if (sp == NULL || hp == NULL)
		goto bad;
	LogFile = socket(AF_INET, SOCK_DGRAM, 0);
	if (LogFile < 0) {
		perror("syslog: socket");
		goto bad;
	}
	bzero(&SyslogAddr, sizeof SyslogAddr);
	SyslogAddr.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, (char *)&SyslogAddr.sin_addr, hp->h_length);
	SyslogAddr.sin_port = sp->s_port;
	LogStat |= LOG_DGRAM;
	return;
bad:
	LogStat |= LOG_COOLIT;
	LogStat &= ~LOG_DGRAM;
	LogMask = LOG_CRIT;
	LogFile = open("/dev/console", 1);
	if (LogFile < 0) {
		perror("syslog: /dev/console");
		LogFile = 2;
	}
}

/*
 * CLOSELOG -- close the system log
 */
closelog()
{

	(void) close(LogFile);
	LogFile = -1;
}
#endif	/* not BSD43 */
