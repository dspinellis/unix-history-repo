# include	<syslog.h>
# include	<sys/types.h>
# include	<sys/stat.h>
# include	<sgtty.h>
# ifdef LOG_IPC
# include	<sys/socket.h>
#ifdef LOG_OLDIPC
# include	<net/in.h>
#else LOG_OLDIPC
# include	<netinet/in.h>
# include	<netdb.h>
#endif LOG_OLDIPC
# endif LOG_IPC

static char	SccsId[] =	"@(#)syslog.c	4.1		7/25/83";


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

# define MAXLINE	1000		/* maximum line length */
# define BUFSLOP	20		/* space to allow for "extra stuff" */
# define NULL		0		/* manifest */

int	LogFile =	-1;		/* fd for log */
int	LogStat	=	0;		/* status bits, set by initlog */
char	*LogTag =	NULL;		/* string to tag the entry with */
int	LogMask =	LOG_DEBUG;	/* lowest priority to be logged */

# ifdef LOG_IPC
#ifndef LOG_OLDIPC
struct sockaddr_in	SyslogAddr;
#else LOG_OLDIPC
struct sockaddr_in	SyslogAddr =	{ AF_INET, LOG_PORT };
struct sockproto	SyslogProto =	{ PF_INET, IPPROTO_UDP };
#endif LOG_OLDIPC
static char		*SyslogHost =	LOG_HOST;
# endif LOG_IPC

syslog(pri, fmt, p0, p1, p2, p3, p4)
	int	pri;
	char	*fmt;
{
	char		buf[MAXLINE+BUFSLOP];
	register char	*b;
	char		*f;
	int		prec;
	int		len;
	register char	c;
	register char	*p;
	int		i;
	extern int	errno;
	extern int	sys_nerr;
	extern char	*sys_errlist[];
	extern char	*logcvt();
	char		outline[MAXLINE + 1];

	/* if we have no log, open it */
	if (LogFile < 0)
		openlog(0, 0);

	/* see if we should just throw out this message */
	if (pri > LogMask)
		return;

	f = fmt;

	while (*f != '\0')
	{
		/* beginning of line */
		b = buf;

		/* insert priority code */
		if (pri > 0 && (LogStat & LOG_COOLIT) == 0)
		{
			*b++ = '<';
			*b++ = pri + '0';
			*b++ = '>';
		}

		/* output current process ID */
		if ((LogStat & LOG_PID) != 0)
		{
			sprintf(b, "%d ", getpid());
			b += strlen(b);
		}

		/* and module name */
		if (LogTag != 0)
		{
			for (p = LogTag; *p != '\0'; )
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
# ifdef LOG_IPC
		if (LogStat & LOG_DGRAM)
		{
			register int r;

#ifndef LOG_OLDIPC
			r = sendto(LogFile, outline, strlen(outline), 0,
				   &SyslogAddr, sizeof SyslogAddr);
#else LOG_OLDIPC
			r = send(LogFile, &SyslogAddr, outline, strlen(outline));
#endif LOG_OLDIPC
# ifdef EBUG
			if (r < 0)
				perror("syslog: send");
# endif EBUG
		}
		else
# endif LOG_IPC
			write(LogFile, outline, strlen(outline));
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
#ifndef LOG_OLDIPC
# ifdef LOG_IPC
	struct servent *sp;
	struct hostent *hp;
# endif LOG_IPC
#endif LOG_OLDIPC

	LogTag = ident;
	LogStat = logstat;

	if (LogFile >= 0)
		return;
# ifdef LOG_IPC
#ifndef LOG_OLDIPC
	sp = getservbyname("syslog", "udp");
	hp = gethostbyname(SyslogHost);
	if (sp != NULL && hp != NULL)
	{
		bzero(&SyslogAddr, sizeof SyslogAddr);
		SyslogAddr.sin_family = AF_INET;
		LogFile = socket(AF_INET, SOCK_DGRAM, 0, 0);
		if (LogFile >= 0 && bind(LogFile, &SyslogAddr, sizeof SyslogAddr, 0) < 0)
		{
			close(LogFile);
			LogFile = -1;
		}
# ifdef EBUG
		if (LogFile < 0)
			perror("syslog: socket");
# endif EBUG
		SyslogAddr.sin_port = sp->s_port;
		bcopy(hp->h_addr, (char *) &SyslogAddr.sin_addr, hp->h_length);
		LogStat |= LOG_DGRAM;
	}
#else LOG_OLDIPC
	SyslogAddr.sin_addr.s_addr = rhost(&SyslogHost);
	LogFile = socket(SOCK_DGRAM, &SyslogProto, 0, 0);
# ifdef EBUG
	if (LogFile < 0)
		perror("syslog: socket");
# endif EBUG
	LogStat |= LOG_DGRAM;
#endif LOG_OLDIPC
# else LOG_IPC
	LogFile = open("/dev/log", 1);
# endif LOG_IPC
	if (LogFile < 0)
	{
	  nolog:
		LogStat |= LOG_COOLIT;
		LogStat &= ~LOG_DGRAM;
		LogMask = LOG_CRIT;
# ifdef EBUG
		LogFile = -1;
# else EBUG
		LogFile = open("/dev/console", 1);
# endif EBUG
		if (LogFile < 0)
		{
			perror("Cannot open /dev/console");
			LogFile = 2;
		}
	}
# ifndef LOG_IPC
	if (fstat(LogFile, &st) < 0)
		goto nolog;
	switch (st.st_mode & S_IFMT)
	{
	  case S_IFREG:
	  case S_IFDIR:
		(void) close(LogFile);
		goto nolog;
	}

# ifdef FIOCLEX
	/* have it close automatically on exec */
	ioctl(LogFile, FIOCLEX, NULL);
# endif FIOCLEX
# endif LOG_IPC
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
	(void) close(LogFile);
	LogFile = -1;
}
