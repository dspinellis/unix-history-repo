/*  $Revision: 1.9 $
**  This file has been modified to get it to compile more easily
**  on pre-4.4BSD (e.g., SysVr4 :-) systems.  Rich $alz, June 1991.
*/

    /* change these three if you must keep running an old syslog. */
#define	_PATH_LOGFILE	"/dev/log"
#define	_PATH_LOGCONF	"/etc/syslog.conf"
#define	_PATH_LOGPID	"/etc/syslog.pid"

    /* #define this to enable verbose debugging of all messages. */
#undef VERBOSE_DEBUG
    /* #define this to line to listen on the INET port. */
#undef DO_INET_SOCKET
    /* #undef this to not read kernel syslog messages. */
#define	_PATH_KLOG	"/dev/klog"
#undef _PATH_KLOG
    /* set this depending on what your sighandler is. */
    /* =()<#define SIGHANDLER	@<SIGHANDLER>@>()= */
#define SIGHANDLER	void
    /* if you need various BSD functions, set  this. */
#define NEED_BZERO_ETC

    /* Use "union wait" instead of int? */
    /* =()<#define @<USE_UNION_WAIT>@_USE_UNION_WAIT>()= */
#define DONT_USE_UNION_WAIT
#if	defined(DO_USE_UNION_WAIT)
#define WAITVALUE	union wait
#else
#define WAITVALUE	int
#endif	/* defined(DO_USE_UNION_WAIT) */

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

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)syslogd.c	5.42 (Berkeley) 6/29/90";
#endif /* not lint */

/*
 *  syslogd -- log system messages
 *
 * This program implements a system log. It takes a series of lines.
 * Each line may have a priority, signified as "<n>" as
 * the first characters of the line.  If this is
 * not present, a default priority is used.
 *
 * To kill syslogd, send a signal 15 (terminate).  A signal 1 (hup) will
 * cause it to reread its configuration file.
 *
 * Defined Constants:
 *
 * MAXLINE -- the maximimum line length that can be handled.
 * DEFUPRI -- the default priority for user messages
 * DEFSPRI -- the default priority for kernel messages
 *
 * Author: Eric Allman
 * extensive changes by Ralph Campbell
 * more extensive changes by Eric Allman (again)
 */

#define	MAXLINE		1024		/* maximum line length */
#define	MAXSVLINE	120		/* maximum saved line length */
#define DEFUPRI		(LOG_USER|LOG_NOTICE)
#define DEFSPRI		(LOG_KERN|LOG_CRIT)
#define TIMERINTVL	30		/* interval for checking flush, mark */

#include <sys/param.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/signal.h>

#ifdef	MSG_BSIZE
#undef	MSG_BSIZE
#endif
#define MSG_BSIZE	4096

#include <netinet/in.h>
#include <netdb.h>

#include <utmp.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define SYSLOG_NAMES
#include "syslog.h"

#define UT_NAMESIZE	8
#define	_PATH_UTMP	"/etc/utmp"
#define	_PATH_DEV	"/dev/"
#define	_PATH_CONSOLE	"/dev/console"

#ifndef sigmask
#define sigmask(m)      (1 << ((m)-1))
#endif
char	*LogName = _PATH_LOGFILE;
char	*ConfFile = _PATH_LOGCONF;
char	*PidFile = _PATH_LOGPID;
char	ctty[] = _PATH_CONSOLE;

#define FDMASK(fd)	(1 << (fd))

#define	dprintf		if (Debug) printf

#define MAXUNAMES	20	/* maximum number of user names */

#ifdef NEED_BZERO_ETC
#include <fcntl.h>
bzero(b, length)
    char *b;
    int length;
{
    while (--length >= 0)
	*b++ = '\0';
}
bcopy(b1, b2, length)
    char *b1;
    char *b2;
    int length;
{
    while (--length >= 0)
	*b2++ = *b1++;
}
setlinebuf(f)
    FILE *f;
{
    setbuf(f, (char *)NULL);
}
wait3(status, flags, np)
    WAITVALUE *status;
    int flags;
    struct rusage *np;
{
    return waitpid(-1, status, WNOHANG);
}

/* Not a general mask handler -- it know what we call, below. */
SIGHANDLER (*oldhup)(), (*oldalrm)();
int
sigsetmask(omask)
    int omask;
{
    if (omask & sigmask(SIGHUP))
	(void) signal(SIGHUP, oldhup);
    if (omask & sigmask(SIGALRM))
	(void) signal(SIGALRM, oldalrm);
}
int
sigblock(mask)
    int mask;
{
    if (mask & sigmask(SIGHUP))
	oldhup = signal(SIGHUP, SIG_IGN);
    if (mask & sigmask(SIGALRM))
	oldhup = signal(SIGALRM, SIG_IGN);
}
#endif

/*
 * Flags to logmsg().
 */

#define IGN_CONS	0x001	/* don't print on console */
#define SYNC_FILE	0x002	/* do fsync on file after printing */
#define ADDDATE		0x004	/* add a date to the message */
#define MARK		0x008	/* this message is a mark */

/*
 * This structure represents the files that will have log
 * copies printed.
 */

struct filed {
	struct	filed *f_nextone;	/* next in linked list */
	short	f_type;			/* entry type, see below */
	short	f_file;			/* file descriptor */
	time_t	f_time;			/* time this was last written */
	u_char	f_pmask[LOG_NFACILITIES+1];	/* priority mask */
	union {
		char	f_uname[MAXUNAMES][UT_NAMESIZE+1];
		struct {
			char	f_hname[MAXHOSTNAMELEN+1];
			struct sockaddr_in	f_addr;
		} f_forw;		/* forwarding address */
		char	f_fname[MAXPATHLEN];
	} f_un;
	char	f_prevline[MAXSVLINE];		/* last message logged */
	char	f_lasttime[16];			/* time of last occurrence */
	char	f_prevhost[MAXHOSTNAMELEN+1];	/* host from which recd. */
	int	f_prevpri;			/* pri of f_prevline */
	int	f_prevlen;			/* length of f_prevline */
	int	f_prevcount;			/* repetition cnt of prevline */
	int	f_repeatcount;			/* number of "repeated" msgs */
};

/*
 * Intervals at which we flush out "message repeated" messages,
 * in seconds after previous message is logged.  After each flush,
 * we move to the next interval until we reach the largest.
 */
int	repeatinterval[] = { 30, 120, 600 };	/* # of secs before flush */
#define	MAXREPEAT ((sizeof(repeatinterval) / sizeof(repeatinterval[0])) - 1)
#define	REPEATTIME(f)	((f)->f_time + repeatinterval[(f)->f_repeatcount])
#define	BACKOFF(f)	{ if (++(f)->f_repeatcount > MAXREPEAT) \
				 (f)->f_repeatcount = MAXREPEAT; \
			}

/* values for f_type */
#define F_UNUSED	0		/* unused entry */
#define F_FILE		1		/* regular file */
#define F_TTY		2		/* terminal */
#define F_CONSOLE	3		/* console terminal */
#define F_FORW		4		/* remote machine */
#define F_USERS		5		/* list of users */
#define F_WALL		6		/* everyone logged on */

char	*TypeNames[7] = {
	"UNUSED",	"FILE",		"TTY",		"CONSOLE",
	"FORW",		"USERS",	"WALL"
};

struct	filed *Files;
struct	filed consfile;

int	Debug;			/* debug flag */
char	LocalHostName[MAXHOSTNAMELEN+1];	/* our hostname */
char	*LocalDomain;		/* our local domain name */
int	InetInuse = 0;		/* non-zero if INET sockets are being used */
int	finet;			/* Internet datagram socket */
int	LogPort;		/* port number for INET connections */
int	Initialized = 0;	/* set when we have initialized ourselves */
int	MarkInterval = 20 * 60;	/* interval between marks in seconds */
int	MarkSeq = 0;		/* mark sequence number */

extern	int errno;
extern	char *ctime(), *strchr(), *malloc();
SIGHANDLER die(), domark(), init(), reapchild();

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *p;
	int funix, inetm, fklog, klogm, len;
	struct sockaddr_un sunx, fromunix;
	struct sockaddr_in sin, frominet;
	FILE *fp;
	int ch;
	char line[MSG_BSIZE + 1];
	extern int optind;
	extern char *optarg;

	while ((ch = getopt(argc, argv, "df:m:p:")) != EOF)
		switch((char)ch) {
		case 'd':		/* debug */
			Debug++;
			break;
		case 'f':		/* configuration file */
			ConfFile = optarg;
			break;
		case 'm':		/* mark interval */
			MarkInterval = atoi(optarg) * 60;
			break;
		case 'p':		/* path */
			LogName = optarg;
			break;
		case '?':
		default:
			usage();
		}
	if (argc -= optind)
		usage();

	if (!Debug)
		daemon(0, 0);
	else
		setlinebuf(stdout);

	consfile.f_type = F_CONSOLE;
	(void) strcpy(consfile.f_un.f_fname, ctty);
	(void) gethostname(LocalHostName, sizeof LocalHostName);
	if (p = strchr(LocalHostName, '.')) {
		*p++ = '\0';
		LocalDomain = p;
	}
	else
		LocalDomain = "";
	(void) signal(SIGTERM, die);
	if (Debug) {
	    (void) signal(SIGINT, die);
	    (void) signal(SIGQUIT, die);
	}
	else {
	    (void) signal(SIGINT, SIG_IGN);
	    (void) signal(SIGQUIT, SIG_IGN);
	}
	(void) signal(SIGCHLD, reapchild);
	(void) signal(SIGALRM, domark);
	(void) alarm(TIMERINTVL);
	(void) unlink(LogName);

	bzero((char *)&sunx, sizeof(sunx));
	sunx.sun_family = AF_UNIX;
	(void) strncpy(sunx.sun_path, LogName, sizeof sunx.sun_path);
	funix = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (funix < 0 || bind(funix, (struct sockaddr *) &sunx,
	    sizeof(sunx)) < 0 ||
	    chmod(LogName, 0666) < 0) {
		(void) sprintf(line, "cannot create %s", LogName);
		logerror(line);
		dprintf("cannot create %s (%d)\n", LogName, errno);
		die(0);
	}
#ifdef	DO_INET_SOCKET
	finet = socket(AF_INET, SOCK_DGRAM, 0);
	if (finet >= 0) {
		struct servent *sp;

		sp = getservbyname("syslog", "udp");
		if (sp == NULL) {
			errno = 0;
			logerror("syslog/udp: unknown service");
			die(0);
		}
		bzero((char *)&sin, sizeof(sin)); /* added uunet!rbj */
		sin.sin_family = AF_INET;
		sin.sin_addr.s_addr = INADDR_ANY;
		sin.sin_port = LogPort = htons(sp->s_port);
		if (bind(finet, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
			logerror("bind");
			if (!Debug)
				die(0);
		} else {
			inetm = FDMASK(finet);
			InetInuse = 1;
		}
	}
#else
	finet = -1;
	inetm = 0;
#endif	/* DO_INET_SOCKET */
#ifdef	_PATH_KLOG
	if ((fklog = open(_PATH_KLOG, O_RDONLY, 0)) >= 0)
		klogm = FDMASK(fklog);
	else {
		dprintf("can't open %s (%d)\n", _PATH_KLOG, errno);
		klogm = 0;
	}
#else
	fklog = -1;
	klogm = 0;
#endif	/* _PATH_KLOG */

	/* tuck my process id away */
	fp = fopen(PidFile, "w");
	if (fp != NULL) {
		fprintf(fp, "%d\n", getpid());
		(void) fclose(fp);
	}

	dprintf("off & running....\n");

	init();
	(void) signal(SIGHUP, init);

	for (;;) {
		int nfds, readfds = FDMASK(funix) | inetm | klogm;

		errno = 0;
		dprintf("readfds = %#x\n", readfds);
		nfds = select(20, (fd_set *) &readfds, (fd_set *) NULL,
		    (fd_set *) NULL, (struct timeval *) NULL);
		if (nfds == 0)
			continue;
		if (nfds < 0) {
			if (errno != EINTR)
				logerror("select");
			continue;
		}
		dprintf("got a message (%d, %#x)\n", nfds, readfds);
		if (readfds & klogm) {
			i = read(fklog, line, sizeof(line) - 1);
			if (i > 0) {
				line[i] = '\0';
				printsys(line);
			} else if (i < 0 && errno != EINTR) {
				logerror("klog");
				fklog = -1;
				klogm = 0;
			}
		}
		if (readfds & FDMASK(funix)) {
			len = sizeof fromunix;
			i = recvfrom(funix, line, MAXLINE, 0,
			    (struct sockaddr *) &fromunix, &len);
			if (i > 0) {
				line[i] = '\0';
				printline(LocalHostName, line);
			} else if (i < 0 && errno != EINTR)
				logerror("recvfrom unix");
		}
		if (readfds & inetm) {
			len = sizeof frominet;
			i = recvfrom(finet, line, MAXLINE, 0,
			    (struct sockaddr *) &frominet, &len);
			if (i > 0) {
				extern char *cvthname();

				line[i] = '\0';
				printline(cvthname(&frominet), line);
			} else if (i < 0 && errno != EINTR)
				logerror("recvfrom inet");
		}
	}
}

usage()
{
	(void) fprintf(stderr,
	    "usage: syslogd [-d] [-f conffile] [-m markinterval] [-p path]\n");
	exit(1);
}

/*
 * Take a raw input line, decode the message, and print the message
 * on the appropriate log files.
 */

printline(hname, msg)
	char *hname;
	char *msg;
{
	register char *p, *q;
	register int c;
	char line[MAXLINE + 1];
	int pri;

	/* test for special codes */
	pri = DEFUPRI;
	p = msg;
	if (*p == '<') {
		pri = 0;
		while (isdigit(*++p))
			pri = 10 * pri + (*p - '0');
		if (*p == '>')
			++p;
	}
	if (pri &~ (LOG_FACMASK|LOG_PRIMASK))
		pri = DEFUPRI;

	/* don't allow users to log kernel messages */
	if (LOG_FAC(pri) == LOG_KERN)
		pri = LOG_MAKEPRI(LOG_USER, LOG_PRI(pri));

	q = line;

	while ((c = *p++ & 0177) != '\0' &&
	    q < &line[sizeof(line) - 1])
		if (iscntrl(c))
			if (c == '\n')
				*q++ = ' ';
			else if (c == '\t')
				*q++ = '\t';
			else {
				*q++ = '^';
				*q++ = c ^ 0100;
			}
		else
			*q++ = c;
	*q = '\0';

	logmsg(pri, line, hname, 0);
}

/*
 * Take a raw input line from /dev/klog, split and format similar to syslog().
 */

printsys(msg)
	char *msg;
{
	register char *p, *q;
	register int c;
	char line[MAXLINE + 1];
	int pri, flags;
	char *lp;

	(void) strcpy(line, "vmunix: ");
	lp = line + strlen(line);
	for (p = msg; *p != '\0'; ) {
		flags = SYNC_FILE | ADDDATE;	/* fsync file after write */
		pri = DEFSPRI;
		if (*p == '<') {
			pri = 0;
			while (isdigit(*++p))
				pri = 10 * pri + (*p - '0');
			if (*p == '>')
				++p;
		} else {
			/* kernel printf's come out on console */
			flags |= IGN_CONS;
		}
		if (pri &~ (LOG_FACMASK|LOG_PRIMASK))
			pri = DEFSPRI;
		q = lp;
		while (*p != '\0' && (c = *p++) != '\n' &&
		    q < &line[MAXLINE])
			*q++ = c;
		*q = '\0';
		logmsg(pri, line, LocalHostName, flags);
	}
}


#ifdef	VERBOSE_DEBUG
/*
 *  Decode a numeric value to a symbolic name
 */
char *
edoced(val, codetab)
	int val;
	CODE *codetab;
{
	register CODE *c;

	if (val >= 0)
		for (c = codetab; c->c_val != -1; c++)
			if (c->c_val == val)
				return (c->c_name);
	return ("???");
}
#endif

time_t	now;

/*
 * Log a message to the appropriate log files, users, etc. based on
 * the priority.
 */

logmsg(pri, msg, from, flags)
	int pri;
	char *msg, *from;
	int flags;
{
	register struct filed *f;
	int fac, prilev;
	int omask, msglen;
	char *timestamp;
	time_t time();

#ifdef	VERBOSE_DEBUG
	if (Debug) {
		int pfac = (flags & MARK) ? INTERNAL_MARK : (pri & LOG_FACMASK);
		printf("logmsg: {%s.%s} pri %o, flags %x, from %s, msg %s\n",
		    edoced(pfac, facilitynames),
		    edoced(LOG_PRI(pri), prioritynames),
		    pri, flags, from, msg);
 	}
#else
	dprintf("logmsg: pri %o, flags %x, from %s, msg %s\n",
	    pri, flags, from, msg);
#endif

	omask = sigblock(sigmask(SIGHUP)|sigmask(SIGALRM));

	/*
	 * Check to see if msg looks non-standard.
	 */
	msglen = strlen(msg);
	if (msglen < 16 || msg[3] != ' ' || msg[6] != ' ' ||
	    msg[9] != ':' || msg[12] != ':' || msg[15] != ' ')
		flags |= ADDDATE;

	(void) time(&now);
	if (flags & ADDDATE)
		timestamp = ctime(&now) + 4;
	else {
		timestamp = msg;
		msg += 16;
		msglen -= 16;
	}

	/* extract facility and priority level */
	if (flags & MARK)
		fac = LOG_NFACILITIES;
	else
		fac = LOG_FAC(pri);
	prilev = LOG_PRI(pri);

	/* log the message to the particular outputs */
	if (!Initialized) {
		f = &consfile;
		f->f_file = open(ctty, O_WRONLY, 0);

		if (f->f_file >= 0) {
			fprintlog(f, flags, msg);
			(void) close(f->f_file);
		}
		(void) sigsetmask(omask);
		return;
	}
	for (f = Files; f; f = f->f_nextone) {
		/* skip messages that are incorrect priority */
		if ((int)f->f_pmask[fac] < prilev ||
		    f->f_pmask[fac] == INTERNAL_NOPRI)
			continue;

		if (f->f_type == F_CONSOLE && (flags & IGN_CONS))
			continue;

		/* don't output marks to recently written files */
		if ((flags & MARK) && (now - f->f_time) < MarkInterval / 2)
			continue;

		/*
		 * suppress duplicate lines to this file
		 */
		if ((flags & MARK) == 0 && msglen == f->f_prevlen &&
		    !strcmp(msg, f->f_prevline) &&
		    !strcmp(from, f->f_prevhost)) {
			(void) strncpy(f->f_lasttime, timestamp, 15);
			f->f_prevcount++;
			dprintf("msg repeated %d times, %ld sec of %d\n",
			    f->f_prevcount, now - f->f_time,
			    repeatinterval[f->f_repeatcount]);
			/*
			 * If domark would have logged this by now,
			 * flush it now (so we don't hold isolated messages),
			 * but back off so we'll flush less often
			 * in the future.
			 */
			if (now > REPEATTIME(f)) {
				fprintlog(f, flags, (char *)NULL);
				BACKOFF(f);
			}
		} else {
			/* new line, save it */
			if (f->f_prevcount)
				fprintlog(f, 0, (char *)NULL);
			f->f_repeatcount = 0;
			(void) strncpy(f->f_lasttime, timestamp, 15);
			(void) strncpy(f->f_prevhost, from,
					sizeof(f->f_prevhost));
			if (msglen < MAXSVLINE) {
				f->f_prevlen = msglen;
				f->f_prevpri = pri;
				(void) strcpy(f->f_prevline, msg);
				fprintlog(f, flags, (char *)NULL);
			} else {
				f->f_prevline[0] = 0;
				f->f_prevlen = 0;
				fprintlog(f, flags, msg);
			}
		}
	}
	(void) sigsetmask(omask);
}

fprintlog(f, flags, msg)
	register struct filed *f;
	int flags;
	char *msg;
{
	struct iovec iov[6];
	register struct iovec *v;
	register int l;
	char line[MAXLINE + 1], repbuf[80], greetings[200];

	v = iov;
	if (f->f_type == F_WALL) {
		v->iov_base = greetings;
		sprintf(greetings,
		    "\r\n\7Message from syslogd@%s at %.24s ...\r\n",
		    f->f_prevhost, ctime(&now));
		v->iov_len = strlen(greetings);
		v++;
		v->iov_base = "";
		v->iov_len = 0;
		v++;
	} else {
		v->iov_base = f->f_lasttime;
		v->iov_len = 15;
		v++;
		v->iov_base = " ";
		v->iov_len = 1;
		v++;
	}
	v->iov_base = f->f_prevhost;
	v->iov_len = strlen(v->iov_base);
	v++;
	v->iov_base = " ";
	v->iov_len = 1;
	v++;

	if (msg) {
		v->iov_base = msg;
		v->iov_len = strlen(msg);
	} else if (f->f_prevcount > 1) {
		v->iov_base = repbuf;
		sprintf(repbuf, "last message repeated %d times",
		    f->f_prevcount);
		v->iov_len = strlen(repbuf);
	} else {
		v->iov_base = f->f_prevline;
		v->iov_len = f->f_prevlen;
	}
	v++;

	dprintf("Logging to %s", TypeNames[f->f_type]);
	f->f_time = now;

	switch (f->f_type) {
	case F_UNUSED:
		dprintf("\n");
		break;

	case F_FORW:
		dprintf(" %s\n", f->f_un.f_forw.f_hname);
		sprintf(line, "<%d>%.15s %s", f->f_prevpri,
		    iov[0].iov_base, iov[4].iov_base);
		l = strlen(line);
		if (l > MAXLINE)
			l = MAXLINE;
		if (sendto(finet, line, l, 0, &f->f_un.f_forw.f_addr,
		    sizeof f->f_un.f_forw.f_addr) != l) {
			int e = errno;
			(void) close(f->f_file);
			f->f_type = F_UNUSED;
			errno = e;
			logerror("sendto");
		}
		break;

	case F_CONSOLE:
		if (flags & IGN_CONS) {
			dprintf(" (ignored)\n");
			break;
		}
		/* FALLTHROUGH */

	case F_TTY:
	case F_FILE:
		dprintf(" %s\n", f->f_un.f_fname);
		if (f->f_type != F_FILE) {
			v->iov_base = "\r\n";
			v->iov_len = 2;
		} else {
			v->iov_base = "\n";
			v->iov_len = 1;
		}
	again:
		if (writev(f->f_file, iov, 6) < 0) {
			int e = errno;
			(void) close(f->f_file);
			/*
			 * Check for errors on TTY's due to loss of tty
			 */
			if ((e == EIO || e == EBADF) && f->f_type != F_FILE) {
				f->f_file = open(f->f_un.f_fname,
				    O_WRONLY|O_APPEND, 0);
				if (f->f_file < 0) {
					f->f_type = F_UNUSED;
					logerror(f->f_un.f_fname);
				} else
					goto again;
			} else {
				f->f_type = F_UNUSED;
				errno = e;
				logerror(f->f_un.f_fname);
			}
		} else if (flags & SYNC_FILE)
			(void) fsync(f->f_file);
		break;

	case F_USERS:
	case F_WALL:
		dprintf("\n");
		v->iov_base = "\r\n";
		v->iov_len = 2;
		wallmsg(f, iov);
		break;
	}
	f->f_prevcount = 0;
}

/*
 *  WALLMSG -- Write a message to the world at large
 *
 *	Write the specified message to either the entire
 *	world, or a list of approved users.
 */

wallmsg(f, iov)
	register struct filed *f;
	struct iovec *iov;
{
	static int reenter;			/* avoid calling ourselves */
	register FILE *uf;
	register int i;
	struct utmp ut;
	char *p, *ttymsg();

	if (reenter++)
		return;
	if ((uf = fopen(_PATH_UTMP, "r")) == NULL) {
		logerror(_PATH_UTMP);
		reenter = 0;
		return;
	}
	/* NOSTRICT */
	while (fread((char *) &ut, sizeof ut, 1, uf) == 1) {
		if (ut.ut_name[0] == '\0')
			continue;
		if (f->f_type == F_WALL) {
			if (p = ttymsg(iov, 6, ut.ut_line, 1)) {
				errno = 0;	/* already in msg */
				logerror(p);
			}
			continue;
		}
		/* should we send the message to this user? */
		for (i = 0; i < MAXUNAMES; i++) {
			if (!f->f_un.f_uname[i][0])
				break;
			if (!strncmp(f->f_un.f_uname[i], ut.ut_name,
			    UT_NAMESIZE)) {
				if (p = ttymsg(iov, 6, ut.ut_line, 1)) {
					errno = 0;	/* already in msg */
					logerror(p);
				}
				break;
			}
		}
	}
	(void) fclose(uf);
	reenter = 0;
}

SIGHANDLER
reapchild()
{
	WAITVALUE status;

	while (wait3(&status, WNOHANG, (struct rusage *) NULL) > 0)
		continue;
}

/*
 * Return a printable representation of a host address.
 */
char *
cvthname(f)
	struct sockaddr_in *f;
{
	struct hostent *hp;
	register char *p;
	extern char *inet_ntoa();

	dprintf("cvthname(%s)\n", inet_ntoa(f->sin_addr));

	if (f->sin_family != AF_INET) {
		dprintf("Malformed from address\n");
		return ("???");
	}
	hp = gethostbyaddr(&f->sin_addr, sizeof(struct in_addr), f->sin_family);
	if (hp == 0) {
		dprintf("Host name for your address (%s) unknown\n",
			inet_ntoa(f->sin_addr));
		return (inet_ntoa(f->sin_addr));
	}
	if ((p = strchr(hp->h_name, '.')) && strcmp(p + 1, LocalDomain) == 0)
		*p = '\0';
	return (hp->h_name);
}

SIGHANDLER
domark()
{
	register struct filed *f;
	time_t time();

	now = time((time_t *)NULL);
	MarkSeq += TIMERINTVL;
	if (MarkSeq >= MarkInterval) {
		logmsg(LOG_INFO, "-- MARK --", LocalHostName, ADDDATE|MARK);
		MarkSeq = 0;
	}

	for (f = Files; f; f = f->f_nextone) {
		if (f->f_prevcount && now >= REPEATTIME(f)) {
			dprintf("flush %s: repeated %d times, %d sec.\n",
			    TypeNames[f->f_type], f->f_prevcount,
			    repeatinterval[f->f_repeatcount]);
			fprintlog(f, 0, (char *)NULL);
			BACKOFF(f);
		}
	}
	(void) alarm(TIMERINTVL);
}

static char *
xstrerror()
{
    extern int	sys_nerr;
    extern char	*sys_errlist[];
    extern int	errno;
    static char	buff[30];

    if (errno >= 0 && errno < sys_nerr)
	return sys_errlist[errno];
    (void)sprintf(buff, "Error code %d\n", errno);
    return buff;
}

/*
 * Print syslogd errors some place.
 */
logerror(type)
	char *type;
{
	char buf[100];

	if (errno)
		(void) sprintf(buf, "syslogd: %s: %s", type, xstrerror(errno));
	else
		(void) sprintf(buf, "syslogd: %s", type);
	errno = 0;
	dprintf("%s\n", buf);
	logmsg(LOG_SYSLOG|LOG_ERR, buf, LocalHostName, ADDDATE);
}

SIGHANDLER
die(sig)
{
	register struct filed *f;
	char buf[100];

	for (f = Files; f != NULL; f = f->f_nextone) {
		/* flush any pending output */
		if (f->f_prevcount)
			fprintlog(f, 0, (char *)NULL);
	}
	if (sig) {
		dprintf("syslogd: exiting on signal %d\n", sig);
		(void) sprintf(buf, "exiting on signal %d", sig);
		errno = 0;
		logerror(buf);
	}
	(void) unlink(LogName);
	exit(0);
}

/*
 *  INIT -- Initialize syslogd from configuration table
 */

SIGHANDLER
init()
{
	register int i;
	register FILE *cf;
	register struct filed *f, *next, **nextp;
	register char *p;
	char cline[BUFSIZ];

	dprintf("init\n");

	/*
	 *  Close all open log files.
	 */
	Initialized = 0;
	for (f = Files; f != NULL; f = next) {
		/* flush any pending output */
		if (f->f_prevcount)
			fprintlog(f, 0, (char *)NULL);

		switch (f->f_type) {
		  case F_FILE:
		  case F_TTY:
		  case F_CONSOLE:
		  case F_FORW:
			(void) close(f->f_file);
			break;
		}
		next = f->f_nextone;
		free((char *) f);
	}
	Files = NULL;
	nextp = &Files;

	/* open the configuration file */
	if ((cf = fopen(ConfFile, "r")) == NULL) {
		dprintf("cannot open %s\n", ConfFile);
		*nextp = (struct filed *)malloc(sizeof(*f));
		cfline("*.ERR\t/dev/console", *nextp);
		(*nextp)->f_nextone = (struct filed *)malloc(sizeof(*f));
		cfline("*.PANIC\t*", (*nextp)->f_nextone);
		Initialized = 1;
		return;
	}

	/*
	 *  Foreach line in the conf table, open that file.
	 */
	f = NULL;
	while (fgets(cline, sizeof cline, cf) != NULL) {
		/*
		 * check for end-of-section, comments, strip off trailing
		 * spaces and newline character.
		 */
		for (p = cline; isspace(*p); ++p);
		if (*p == NULL || *p == '#')
			continue;
		for (p = strchr(cline, '\0'); isspace(*--p););
		*++p = '\0';
		f = (struct filed *)malloc(sizeof(*f));
		*nextp = f;
		nextp = &f->f_nextone;
		cfline(cline, f);
	}

	/* close the configuration file */
	(void) fclose(cf);

	Initialized = 1;

	if (Debug) {
		for (f = Files; f; f = f->f_nextone) {
			for (i = 0; i <= LOG_NFACILITIES; i++)
				if (f->f_pmask[i] == INTERNAL_NOPRI)
					printf("X ");
				else
					printf("%d ", f->f_pmask[i]);
			printf("%s: ", TypeNames[f->f_type]);
			switch (f->f_type) {
			case F_FILE:
			case F_TTY:
			case F_CONSOLE:
				printf("%s", f->f_un.f_fname);
				break;

			case F_FORW:
				printf("%s", f->f_un.f_forw.f_hname);
				break;

			case F_USERS:
				for (i = 0; i < MAXUNAMES && *f->f_un.f_uname[i]; i++)
					printf("%s, ", f->f_un.f_uname[i]);
				break;
			}
			printf("\n");
		}
	}

	logmsg(LOG_SYSLOG|LOG_INFO, "syslogd: restart", LocalHostName, ADDDATE);
	dprintf("syslogd: restarted\n");
}

/*
 * Crack a configuration file line
 */

cfline(line, f)
	char *line;
	register struct filed *f;
{
	register char *p;
	register char *q;
	register int i;
	char *bp;
	int pri;
	struct hostent *hp;
	char buf[MAXLINE];

	dprintf("cfline(%s)\n", line);

	errno = 0;	/* keep strerror() stuff out of logerror messages */

	/* clear out file entry */
	bzero((char *) f, sizeof *f);
	for (i = 0; i <= LOG_NFACILITIES; i++)
		f->f_pmask[i] = INTERNAL_NOPRI;

	/* scan through the list of selectors */
	for (p = line; *p && *p != '\t';) {

		/* find the end of this facility name list */
		for (q = p; *q && *q != '\t' && *q++ != '.'; )
			continue;

		/* collect priority name */
		for (bp = buf; *q && !strchr("\t,;", *q); )
			*bp++ = *q++;
		*bp = '\0';

		/* skip cruft */
		while (strchr(", ;", *q))
			q++;

		/* decode priority name */
		pri = decode(buf, prioritynames);
		if (pri < 0) {
			char xbuf[200];

			(void) sprintf(xbuf, "unknown priority name \"%s\"",
			    buf);
			logerror(xbuf);
			return;
		}

		/* scan facilities */
		while (*p && !strchr("\t.;", *p)) {
			for (bp = buf; *p && !strchr("\t,;.", *p); )
				*bp++ = *p++;
			*bp = '\0';
			if (*buf == '*')
				for (i = 0; i < LOG_NFACILITIES; i++)
					f->f_pmask[i] = pri;
			else {
				i = decode(buf, facilitynames);
				if (i < 0) {
					char xbuf[200];

					(void) sprintf(xbuf,
					    "unknown facility name \"%s\"",
					    buf);
					logerror(xbuf);
					return;
				}
				f->f_pmask[i >> 3] = pri;
			}
			while (*p == ',' || *p == ' ')
				p++;
		}

		p = q;
	}

	/* skip to action part */
	while (*p == '\t')
		p++;

	switch (*p)
	{
	case '@':
		if (!InetInuse)
			break;
		(void) strcpy(f->f_un.f_forw.f_hname, ++p);
		hp = gethostbyname(p);
		if (hp == NULL) {
			logerror("hostname lookup error");
			break;
		}
		bzero((char *) &f->f_un.f_forw.f_addr,
			 sizeof f->f_un.f_forw.f_addr);
		f->f_un.f_forw.f_addr.sin_family = AF_INET;
		f->f_un.f_forw.f_addr.sin_port = LogPort;
		bcopy(hp->h_addr, (char *) &f->f_un.f_forw.f_addr.sin_addr, hp->h_length);
		f->f_type = F_FORW;
		break;

	case '/':
		(void) strcpy(f->f_un.f_fname, p);
		if ((f->f_file = open(p, O_WRONLY|O_APPEND, 0)) < 0) {
			f->f_file = F_UNUSED;
			logerror(p);
			break;
		}
		if (isatty(f->f_file))
			f->f_type = F_TTY;
		else
			f->f_type = F_FILE;
		if (strcmp(p, ctty) == 0)
			f->f_type = F_CONSOLE;
		break;

	case '*':
		f->f_type = F_WALL;
		break;

	default:
		for (i = 0; i < MAXUNAMES && *p; i++) {
			for (q = p; *q && *q != ','; )
				q++;
			(void) strncpy(f->f_un.f_uname[i], p, UT_NAMESIZE);
			if ((q - p) > UT_NAMESIZE)
				f->f_un.f_uname[i][UT_NAMESIZE] = '\0';
			else
				f->f_un.f_uname[i][q - p] = '\0';
			while (*q == ',' || *q == ' ')
				q++;
			p = q;
		}
		f->f_type = F_USERS;
		break;
	}
}


/*
 *  Decode a symbolic name to a numeric value
 */

decode(name, codetab)
	char *name;
	CODE *codetab;
{
	register CODE *c;
	register char *p;
	char buf[40];

	if (isdigit(*name))
		return (atoi(name));

	(void) strcpy(buf, name);
	for (p = buf; *p; p++)
		if (isupper(*p))
			*p = tolower(*p);
	for (c = codetab; c->c_name; c++)
		if (!strcmp(buf, c->c_name))
			return (c->c_val);

	return (-1);
}

daemon(nochdir, noclose)
        int nochdir, noclose;
{
        int cpid;

        if ((cpid = fork()) == -1)
                return (-1);
        if (cpid)
                exit(0);
        (void) setsid();
        if (!nochdir)
                (void) chdir("/");
        if (!noclose) {
                int devnull = open("/dev/null", O_RDWR, 0);

                if (devnull != -1) {
                        (void) dup2(devnull, 0);
                        (void) dup2(devnull, 1);
                        (void) dup2(devnull, 2);
                        if (devnull > 2)
                                (void) close(devnull);
                }
        }
}


/*
 * Display the contents of a uio structure on a terminal.  Used by wall(1)
 * and syslogd(8).  Forks and finishes in child if write would block, waiting
 * at most five minutes.  Returns pointer to error string on unexpected error;
 * string is not newline-terminated.  Various "normal" errors are ignored
 * (exclusive-use, lack of permission, etc.).
 */
char *
ttymsg(iov, iovcnt, line)
	struct iovec *iov;
	int iovcnt;
	char *line;
{
	static char device[64] = _PATH_DEV;
	static char errbuf[1024];
	register int cnt, fd, left, wret;
	struct iovec localiov[6];
	int forked = 0;

	if (iovcnt > 6)
		return ("too many iov's");
	/*
	 * open will fail on slip lines or exclusive-use lines
	 * if not running as root; not an error.
	 */
	(void) strcpy(device + sizeof(_PATH_DEV) - 1, line);
	if ((fd = open(device, O_WRONLY|O_NONBLOCK, 0)) < 0) {
		if (errno == EBUSY || errno == EACCES)
			return (NULL);
		(void) sprintf(errbuf, "%s: error %d", device, errno);
		return (errbuf);
	}

	for (cnt = left = 0; cnt < iovcnt; ++cnt)
		left += iov[cnt].iov_len;

	for (;;) {
		wret = writev(fd, iov, iovcnt);
		if (wret >= left)
			break;
		if (wret >= 0) {
			left -= wret;
			if (iov != localiov) {
				bcopy(iov, localiov,
				    iovcnt * sizeof(struct iovec));
				iov = localiov;
			}
			for (cnt = 0; wret >= iov->iov_len; ++cnt) {
				wret -= iov->iov_len;
				++iov;
				--iovcnt;
			}
			if (wret) {
				iov->iov_base += wret;
				iov->iov_len -= wret;
			}
			continue;
		}
		if (errno == EWOULDBLOCK) {
			int cpid, off = 0;

			if (forked) {
				(void) close(fd);
				_exit(1);
			}
			cpid = fork();
			if (cpid < 0) {
				(void) sprintf(errbuf,
				    "fork: error %d", errno);
				(void) close(fd);
				return (errbuf);
			}
			if (cpid) {	/* parent */
				(void) close(fd);
				return (NULL);
			}
			forked++;
			/* wait at most 5 minutes */
			(void) signal(SIGALRM, SIG_DFL);
			(void) signal(SIGTERM, SIG_DFL); /* XXX */
			(void) sigsetmask(0);
			(void) alarm((u_int)(60 * 5));
			(void) fcntl(fd, FNDELAY, &off);
			continue;
		}
		/*
		 * We get ENODEV on a slip line if we're running as root,
		 * and EIO if the line just went away.
		 */
		if (errno == ENODEV || errno == EIO)
			break;
		(void) close(fd);
		if (forked)
			_exit(1);
		(void) sprintf(errbuf, "%s: error %d", errno);
		return (errbuf);
	}

	(void) close(fd);
	if (forked)
		_exit(0);
	return (NULL);
}
