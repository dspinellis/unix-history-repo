#include <stdio.h>
#include <sysexits.h>

/* string extraction/restoration routines */

/*
  STR_FILE must be defined as a quoted string on the cc command line,
  for example:

  	-DSTR_FILE=\\\"/usr/local/lib/kermit5.sr\\\"

  This is the file where the strings go, and where C-Kermit looks for them
  at runtime.
*/
#ifdef STR_FILE
char	*StringFile = STR_FILE;
#else
char	*StringFile = "/usr/local/lib/kermit5.sr";
#endif /* STR_FILE */

#ifdef STR_CTIMED
char	*Ctimed = STR_CTIMED;
#else
char	*Ctimed = "/usr/lib/ctimed";
#endif

extern int errno;
static int strfile = -1, ourpid = 0;

#define BUFLEN 256

errprep(offset, buf)
unsigned short offset;
char *buf;
{
register int pid = getpid();

	if (pid != ourpid) {
		ourpid = pid;
		if (strfile >= 0) {
			close(strfile);
			strfile = -1;
		}
	}
	if (strfile < 0) {
	        char *p, *getenv();
		if (p = getenv("KSTR"))
		  if (strlen(p))
		    StringFile = p;
		strfile = open(StringFile, 0);
		if (strfile < 0) {
oops:
			fprintf(stderr, "Cannot find %s\r\n", StringFile);
			exit(EX_OSFILE);
		}
	}
	if (lseek(strfile, (long) offset, 0) < 0
			|| read(strfile, buf, BUFLEN) <= 0)
		goto oops;
}

/* extracted string front end for printf() */
/*VARARGS1*/
strprerror(fmt, a, b, c, d, e, f, g, h, i, j, k, l)
	int fmt;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	printf(buf, a, b, c, d, e, f, g, h, i ,j, k, l);
}

/* extracted string front end for sprintf() */
/*VARARGS1*/
strsrerror(fmt, obuf, a, b, c, d, e, f, g, h, i, j, k, l)
	int fmt;
	char *obuf;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	sprintf(obuf, buf, a, b, c, d, e, f, g, h, i, j, k, l);
}

/* extracted string front end for fprintf() */
/*VARARGS1*/
strfrerror(fmt, fd, a, b, c, d, e, f, g, h, i, j, k, l)
	int fmt, fd;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	fprintf(fd, buf, a, b, c, d, e, f, g, h, i, j, k, l);
}

/* extracted string front end for perror() */
strperror(fmt)
	int fmt;
{
	char buf[BUFLEN];
	register int saverr = errno;

	errprep(fmt, buf);
	errno = saverr;
	perror(buf);
}

perror(str)
	char	*str;
	{

	printf("%s: errno %d\n", str, errno);
	}

#include	<sys/types.h>
#include	<sys/time.h>

#define	SEND_FD	W[1]
#define	RECV_FD	R[0]

#define	CTIME	1
#define	ASCTIME	2
#define	TZSET	3
#define	LOCALTIME 4
#define	GMTIME	5
#define	OFFTIME	6

	static	int	R[2], W[2], inited;
	static	char	result[26];
	static	struct	tm	tmtmp;

char	*
ctime(t)
	time_t	*t;
	{
	u_char	fnc = CTIME;

	sewer();
	write(SEND_FD, fnc, sizeof fnc);
	write(SEND_FD, t, sizeof (*t));
	getb(RECV_FD, result, 26);
	return(result);
	}

char	*
asctime(tp)
	struct	tm	*tp;
	{
	u_char	fnc = ASCTIME;

	sewer();
	write(SEND_FD, &fnc, sizeof fnc);
	write(SEND_FD, tp, sizeof (*tp));
	getb(RECV_FD, result, 26);
	return(result);
	}

void
tzset()
	{
	u_char	fnc = TZSET;

	sewer();
	write(SEND_FD, &fnc, sizeof fnc);
	}

struct	tm *
localtime(tp)
	time_t	*tp;
	{
	u_char	fnc = LOCALTIME;

	sewer();
	write(SEND_FD, &fnc, sizeof fnc);
	write(SEND_FD, tp, sizeof (*tp));
	getb(RECV_FD, &tmtmp, sizeof tmtmp);
	getb(RECV_FD, result, 24);
	tmtmp.tm_zone = result;
	return(&tmtmp);
	}

struct	tm *
gmtime(tp)
	time_t	*tp;
	{
	u_char	fnc = GMTIME;

	sewer();
	write(SEND_FD, &fnc, sizeof fnc);
	write(SEND_FD, tp, sizeof (*tp));
	getb(RECV_FD, &tmtmp, sizeof tmtmp);
	getb(RECV_FD, result, 24);
	tmtmp.tm_zone = result;
	return(&tmtmp);
	}

struct	tm *
offtime(clock, offset)
	time_t	*clock;
	long	offset;
	{
	u_char	fnc = OFFTIME;

	sewer();
	write(SEND_FD, &fnc, sizeof fnc);
	write(SEND_FD, clock, sizeof (*clock));
	write(SEND_FD, &offset, sizeof offset);
	getb(RECV_FD, &tmtmp, sizeof tmtmp);
	tmtmp.tm_zone = "";
	return(&tmtmp);
	}

getb(f, p, n)
	register int f, n;
	register char *p;
	{
	int	i;

	while	(n)
		{
		i = read(f, p, n);
		if	(i <= 0)
			return;
		p += i;
		n -= i;
		}
	}

sewer()
	{
	register int	pid, ourpid = getpid();

	if	(inited == ourpid)
		return;
	if	(inited)
		{
		close(SEND_FD);
		close(RECV_FD);
		}
	pipe(W);
	pipe(R);
	pid = vfork();
	if	(pid == 0)
		{			/* child */
		alarm(0);		/* cancel alarms */
		dup2(W[0], 0);		/* parent write side to our stdin */
		dup2(R[1], 1);		/* parent read side to our stdout */
		close(SEND_FD);		/* copies made, close the... */
		close(RECV_FD);		/* originals now */
		execl(Ctimed, "ctimed", 0);
		_exit(EX_OSFILE);
		}
	if	(pid == -1)
		abort();		/* nothing else really to do */
	close(W[0]);			/* close read side of SEND channel */
	close(R[1]);			/* close write side of RECV channel */
	inited = ourpid;		/* don't do this again in this proc */
	}

XXctime()
	{

	if	(SEND_FD)
		close(SEND_FD);
	if	(RECV_FD)
		close(RECV_FD);
	SEND_FD = RECV_FD = 0;
	inited = 0;
	}
