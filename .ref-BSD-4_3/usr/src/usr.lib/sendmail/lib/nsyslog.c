#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sgtty.h>
#ifdef LOG_IPC
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif LOG_IPC

static char	SccsId[] =	"%W%	%Y%	%G%";


/*
**  SYSLOG -- print message on log file
**
**	This routine looks a lot like printf, except that it
**	outputs to the log file instead of the standard output.
**	Also, it prints the module name in front of lines,
**	and has some other formatting types (or will sometime).
**	Also, it adds a newline on the end of messages.
**
**	The output of this routine is intended to be read by
**	/etc/syslog, which will add timestamps.
**
**	Parameters:
**		pri -- the message priority.
**		fmt -- the format string.
**		p0 -- the first of many parameters.
**
**	Returns:
**		none
**
**	Side Effects:
**		output to log.
*/

#define MAXLINE		1000		/* maximum line length */
#define BUFSLOP		20		/* space to allow for "extra stuff" */
#define NULL		0		/* manifest */

static int	SyslogFile	= -1;		/* fd for log */
static int	SyslogStat	= 0;		/* status bits; see below */
static char	*SyslogTag	= NULL;		/* tag for each entry */
static int	SyslogMask	= LOG_DEBUG;	/* lowest priority logged */
#ifdef LOG_IPC
static struct sockaddr_in	SyslogAddr;	/* address of syslog daemon */
static char	*SyslogHost	= LOG_HOST;	/* name of this host */
#endif LOG_IPC

syslog(pri, fmt, p0, p1, p2, p3, p4)
	int pri;
	char *fmt;
{
	char buf[MAXLINE+BUFSLOP];
	register char *b;
	char *f;
	int prec;
	int len;
	register char c;
	register char *p;
	int i;
	extern int errno;
	extern int sys_nerr;
	extern char *sys_errlist[];
	extern char *logcvt();
	char outline[MAXLINE + 1];

	/* if we have no log, open it */
	if (SyslogFile < 0)
		openlog(0, 0);

	/* see if we should just throw out this message */
	if (pri > SyslogMask)
		return;

	f = fmt;

	while (*f != '\0')
	{
		/* beginning of line */
		b = buf;

		/* insert priority code */
		if (pri > 0 && (SyslogStat & LOG_COOLIT) == 0)
		{
			*b++ = '<';
			*b++ = pri + '0';
			*b++ = '>';
		}

		/* output current process ID */
		if ((SyslogStat & LOG_PID) != 0)
		{
			sprintf(b, "%d ", getpid());
			b += strlen(b);
		}

		/* and module name */
		if (SyslogTag != 0)
		{
			for (p = SyslogTag; *p != '\0'; )
				*b++ = *p++;
			*b++ = ':';
			*b++ = ' ';
		}
		while ((c = *f++) != '\0' && c != '\n')
		{
			/* output character directly if not interpolated */
			if (c != '%')
			{
				*b++ = c;
				continue;
			}
			c = *f++;
			switch (c)
			{
			  case 'm':	/* output error code */
				if (errno < 0 || errno > sys_nerr)
					sprintf(b, "error %d", errno);
				else
					sprintf(b, "%s", sys_errlist[errno]);
				break;

			  default:
				*b++ = '%';
				*b++ = c;
				*b = '\0';
				break;
			}
			b += strlen(b);
			if (b >= &buf[MAXLINE])
				break;
		}
		if (c == '\0')
			f--;

		/* add trailing newline */
		*b++ = '\n';
		*b = '\0';
		
		/* output string */
		sprintf(outline, buf, p0, p1, p2, p3, p4);
#ifdef LOG_IPC
		if (SyslogStat & LOG_DGRAM)
		{
			register int r;

			r = sendto(SyslogFile, outline, strlen(outline), 0,
				   &SyslogAddr, sizeof SyslogAddr);
#ifdef DEBUG
			if (r < 0)
				perror("syslog: send");
#endif DEBUG
		}
		else
#endif LOG_IPC
			write(SyslogFile, outline, strlen(outline));
	}
}
/*
**  OPENLOG -- open system log
**
**	This happens automatically with reasonable defaults if you
**	do nothing.
**
**	Parameters:
**		ident -- the name to be printed as a header for
**			all messages.
**		logstat -- a status word, interpreted as follows:
**			LOG_PID -- log the pid with each message.
**
**	Returns:
**		0 -- success.
**		-1 -- failure; logging on /dev/console instead.
**
**	Side Effects:
**		Several global variables get set.
*/

openlog(ident, logstat)
	char *ident;
	int logstat;
{
	register int i;
	register int fd;
	struct stat st;
#ifdef LOG_IPC
	struct servent *sp;
	struct hostent *hp;
#endif LOG_IPC

	SyslogTag = ident;
	SyslogStat = logstat;

	if (SyslogFile >= 0)
		return;
#ifdef LOG_IPC
	sp = getservbyname("syslog", "udp");
	hp = gethostbyname(SyslogHost);
	if (sp != NULL && hp != NULL)
	{
		bzero(&SyslogAddr, sizeof SyslogAddr);
		SyslogAddr.sin_family = AF_INET;
		SyslogFile = socket(AF_INET, SOCK_DGRAM, 0, 0);
		if (SyslogFile >= 0 && bind(SyslogFile, &SyslogAddr, sizeof SyslogAddr, 0) < 0)
		{
			close(SyslogFile);
			SyslogFile = -1;
		}
#ifdef DEBUG
		if (SyslogFile < 0)
			perror("syslog: socket");
#endif DEBUG
		SyslogAddr.sin_port = sp->s_port;
		bcopy(hp->h_addr, (char *) &SyslogAddr.sin_addr, hp->h_length);
		SyslogStat |= LOG_DGRAM;
	}
#else LOG_IPC
	SyslogFile = open("/dev/log", 1);
#endif LOG_IPC
	if (SyslogFile < 0)
	{
	  nolog:
		SyslogStat |= LOG_COOLIT;
		SyslogStat &= ~LOG_DGRAM;
		SyslogMask = LOG_CRIT;
		SyslogFile = open("/dev/console", 1);
		if (SyslogFile < 0)
		{
			perror("syslog: cannot open /dev/console");
			SyslogFile = 2;
		}
	}
#ifndef LOG_IPC
	if (fstat(SyslogFile, &st) < 0)
		goto nolog;
	switch (st.st_mode & S_IFMT)
	{
	  case S_IFREG:
	  case S_IFDIR:
		(void) close(SyslogFile);
		goto nolog;
	}

#ifdef FIOCLEX
	/* have it close automatically on exec */
	ioctl(SyslogFile, FIOCLEX, NULL);
#endif FIOCLEX
#endif LOG_IPC
}
/*
**  CLOSELOG -- close the system log
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The system log is closed.
*/

closelog()
{
	(void) close(SyslogFile);
	SyslogFile = -1;
}
