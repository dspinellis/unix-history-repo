/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)syslogd.c	5.1 (Berkeley) %G%";
#endif not lint

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
 * NLOGS   -- the maximum number of simultaneous log files.
 * NUSERS  -- the maximum number of people that can
 *		be designated as "superusers" on your system.
 * DEFUPRI -- the default priority for user messages
 * DEFSPRI -- the default priority for kernel messages
 *
 * Author: Eric Allman
 * extensive changes by Ralph Campbell
 */

#define	NLOGS		10		/* max number of log files */
#define	NSUSERS		10		/* max number of special users */
#define	MAXLINE		1024		/* maximum line length */
#define DEFUPRI		LOG_NOTICE
#define DEFSPRI		KERN_ERR

#include <syslog.h>
#include <errno.h>
#include <stdio.h>
#include <utmp.h>
#include <ctype.h>
#include <signal.h>
#include <sysexits.h>
#include <strings.h>

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/msgbuf.h>
#include <sys/uio.h>
#include <sys/un.h>

#include <netinet/in.h>
#include <netdb.h>

char	logname[] = "/dev/log";
char	defconf[] = "/etc/syslog.conf";
char	defpid[] = "/etc/syslog.pid";
char	ctty[] = "/dev/console";

#define	dprintf		if (Debug) printf

#define UNAMESZ	8	/* length of a login name */

#define mask(x)	(1 << (x))

/*
 * Flags to logmsg().
 */
#define IGN_CONS	0x1
#define SYNC_FILE	0x2
#define NOCOPY		0x4
#define ISMARK		0x10

/*
 * This structure represents the files that will have log
 * copies printed.
 */

struct filed {
	int	f_file;			/* file descriptor */
	u_int	f_pmask;		/* priority mask */
	u_int	f_flags;		/* see #defines below */
	struct	sockaddr_in f_addr;	/* forwarding address */
	char	f_name[248];		/* filename */
};

#define F_TTY	001		/* file is a tty */
#define F_MARK	002		/* write to the file periodically */
#define F_FORW	004		/* forward message to another host */
#define F_CONS	010		/* file is the console */

struct filed	Files[NLOGS];

/* list of superusers */
struct susers {
	u_int	s_pmask;		/* priority mask */
	char	s_name[UNAMESZ+1];
};

struct	susers Susers[NSUSERS];

int	Debug;			/* debug flag */
int	LogFile;		/* log file descriptor */
u_int	Sumask;			/* priorities written to super-users */
int	MarkIntvl = 15;		/* mark interval in minutes */
char	*ConfFile = defconf;	/* configuration file */
char	host[32];		/* our hostname */
char	rhost[32];		/* hostname of sender (forwarded messages) */
int	inet = 0;		/* non-zero if INET sockets are being used */
int	port;			/* port number for INET connections */
u_int	Copymask = 0xffffffff;	/* priorities to supress multiple copies */
char	prevline[MAXLINE + 1];	/* copy of last line to supress repeats */
char	*prevdate;		/* pointer to the date in prevline */
char	prevhost[32];		/* previous host */
int	prevflags;
int	prevpri;
int	count = 0;		/* number of times seen */

extern	int errno, sys_nerr;
extern	char *sys_errlist[];
extern	char *ctime(), *index();

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *p;
	int funix, finet, inetm, fklog, klogm, len;
	struct sockaddr_un sun, fromunix;
	struct sockaddr_in sin, frominet;
	FILE *fp;
	char line[MSG_BSIZE + 1];
	extern int die(), domark(), reapchild();

	sun.sun_family = AF_UNIX;
	strncpy(sun.sun_path, logname, sizeof sun.sun_path);
	gethostname(host, sizeof host);

	while (--argc > 0) {
		p = *++argv;
		if (p[0] != '-')
			usage();
		switch (p[1]) {
		case 'm':		/* set mark interval */
			MarkIntvl = atoi(&p[2]);
			if (MarkIntvl <= 0)
				MarkIntvl = 1;
			break;

		case 'f':		/* configuration file */
			if (p[2] != '\0')
				ConfFile = &p[2];
			break;

		case 'd':		/* debug */
			Debug++;
			break;

		case 'p':		/* path */
			if (p[2] != '\0')
				strncpy(sun.sun_path, &p[2], 
					sizeof sun.sun_path);
			break;

		default:
			usage();
		}
	}

	if (!Debug) {
		if (fork())
			exit(0);
		for (i = 0; i < 10; i++)
			(void) close(i);
		(void) open("/", 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		untty();
	} else
		setlinebuf(stdout);

	signal(SIGTERM, die);
	signal(SIGINT, die);
	signal(SIGCHLD, reapchild);
	funix = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (funix < 0 || bind(funix, &sun,
	    sizeof(sun.sun_family)+strlen(sun.sun_path)) < 0 ||
	    chmod(sun.sun_path, 0666) < 0) {
		fp = fopen(ctty, "w");
		fprintf(fp, "\r\nsyslogd: cannot create %s (%d)\r\n", logname, errno);
		dprintf("cannot create %s (%d)\n", logname, errno);
		exit(1);
	}
	finet = socket(AF_INET, SOCK_DGRAM, 0);
	if (finet >= 0) {
		struct servent *sp;

		sp = getservbyname("syslog", "udp");
		if (sp == NULL) {
			errno = 0;
			logerror("syslog/udp: unknown service");
			die();
		}
		sin.sin_family = AF_INET;
		sin.sin_port = port = sp->s_port;
		if (bind(finet, &sin, sizeof(sin), 0) < 0) {
			logerror("bind");
			die();
		}
		inetm = mask(finet);
		inet = 1;
	}
	if ((fklog = open("/dev/klog", O_RDONLY)) >= 0)
		klogm = mask(fklog);
	else {
		dprintf("can't open /dev/klog (%d)\n", errno);
		klogm = 0;
	}

	/* tuck my process id away */
	fp = fopen(defpid, "w");
	if (fp != NULL) {
		fprintf(fp, "%d\n", getpid());
		fclose(fp);
	}

	dprintf("off & running....\n");

	for (i = 0; i < NLOGS; i++)
		Files[i].f_file = -1;
	init();
	signal(SIGHUP, init);
	signal(SIGALRM, domark);
	alarm(MarkIntvl * 60);

	for (;;) {
		int nfds, readfds = mask(funix) | inetm | klogm;

		dprintf("readfds = %#x\n", readfds, funix, finet, fklog);
		nfds = select(20, &readfds, 0, 0, 0);
		dprintf("got a message (%d, %#x)\n", nfds, readfds);
		if (nfds == 0)
			continue;
		if (nfds < 0) {
			if (errno == EINTR)
				continue;
			logerror("select");
			continue;
		}
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
		if (readfds & mask(funix)) {
			len = sizeof fromunix;
			i = recvfrom(funix, line, MAXLINE, 0, &fromunix, &len);
			if (i > 0) {
				line[i] = '\0';
				printline(1, line);
			} else if (i < 0 && errno != EINTR)
				logerror("recvfrom");
		}
		if (readfds & inetm) {
			len = sizeof frominet;
			i = recvfrom(finet, line, MAXLINE, 0, &frominet, &len);
			if (i > 0 && chkhost(&frominet)) {
				line[i] = '\0';
				printline(0, line);
			} else if (i < 0 && errno != EINTR)
				logerror("recvfrom");
		} 
	}
}

usage()
{
	fprintf(stderr, "usage: syslogd [-m#] [-d] [-ppath] [-fconffile]\n");
	exit(1);
}

untty()
{
	int i;

	if (!Debug) {
		i = open("/dev/tty", O_RDWR);
		if (i >= 0) {
			ioctl(i, TIOCNOTTY, (char *)0);
			(void) close(i);
		}
	}
}

/*
 * Take a raw input line, decode the message, and print the message
 * on the appropriate log files.
 */

printline(local, msg)
	int local;
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
		if (pri <= 0 || pri >= 32)
			pri = DEFUPRI;
	}

	q = line;
	while ((c = *p++ & 0177) != '\0' && c != '\n' &&
	    q < &line[sizeof(line) - 1]) {
		if (iscntrl(c)) {
			*q++ = '^';
			*q++ = c ^ 0100;
		} else
			*q++ = c;
	}
	*q = '\0';

	logmsg(pri, line, local ? host : rhost, 0);
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
	long now;

	time(&now);
	sprintf(line, "vmunix: %.15s-- ", ctime(&now) + 4);
	lp = line + strlen(line);
	for (p = msg; *p != '\0'; ) {
		flags = SYNC_FILE;	/* fsync file after write */
		pri = DEFSPRI;
		if (*p == '<') {
			pri = 0;
			while (isdigit(*++p))
				pri = 10 * pri + (*p - '0');
			if (*p == '>')
				++p;
			if (pri <= 0 || pri >= 32)
				pri = DEFSPRI;
		} else {
			/* kernel printf's come out on console */
			flags |= IGN_CONS;
		}
		q = lp;
		while (*p != '\0' && (c = *p++) != '\n' &&
		    q < &line[MAXLINE])
			*q++ = c;
		*q = '\0';
		logmsg(pri, line, host, flags);
	}
}

/*
 * Log a message to the appropriate log files, users, etc. based on
 * the priority.
 */

logmsg(pri, msg, from, flags)
	int	pri;
	char	*msg, *from;
	int	flags;
{
	char line[MAXLINE + 1];
	register struct filed *f;
	register int l;
	struct iovec iov[4];
	register struct iovec *v = iov;
	int omask;

	omask = sigblock(sigmask(SIGALRM)|sigmask(SIGHUP));

	if ((flags & NOCOPY) == 0) {
		register char *cp;

		/*
		 * Check to see if copies should be supressed or
		 * msg looks non-standard (e.g., 'prog: Feb 16 13:23:56-- ').
		 */
		if ((Copymask & mask(pri)) == 0 ||
		    (cp = index(msg, ':')) == NULL || strlen(cp) < 20 ||
		    cp[5] != ' ' || cp[8] != ' ' || cp[11] != ':' ||
		    cp[14] != ':' || cp[17] != '-' || cp[18] != '-' ||
		    cp[19] != ' ')
			flushmsg();
		else if (!strncmp(msg, prevline, cp-msg) &&
		    !strcmp(cp+20, prevdate+18)) {
			/* we found a match, update the time */
			strncpy(prevdate, cp+2, 15);
			count++;
			(void) sigsetmask(omask);
			return;
		} else {
			/* new line, save it */
			flushmsg();
			strcpy(prevline, msg);
			strcpy(prevhost, from);
			prevdate = prevline + (cp - msg) + 2;
			prevflags = flags;
			prevpri = pri;
		}
	}

	v->iov_base = from;
	v->iov_len = strlen(v->iov_base);
	v++;
	v->iov_base = " ";
	v->iov_len = 1;
	v++;
	v->iov_base = msg;
	v->iov_len = strlen(v->iov_base);
	v++;
	/* log the message to the particular outputs */
	for (f = Files; f < &Files[NLOGS]; f++) {
		if (f->f_file < 0)
			continue;
		if (flags & ISMARK) {	/* mark message */
			if (!(f->f_flags & F_MARK))
				continue;
			if (!(f->f_flags & F_CONS)) {
				struct stat stb;
				long now;

				if (fstat(f->f_file, &stb) < 0)
					continue;
				time(&now);
				if (stb.st_mtime > now - MarkIntvl * 60)
					continue;
			}
		} else if ((f->f_pmask & mask(pri)) == 0 ||
		    (flags & IGN_CONS) && (f->f_flags & F_CONS))
			continue;
		if (f->f_flags & F_FORW) {
			sprintf(line, "<%d>%s", pri, msg);
			l = strlen(line);
			if (l > MAXLINE)
				l = MAXLINE;
			if (sendto(f->f_file, line, l, 0,
			    &f->f_addr, sizeof f->f_addr) != l) {
				int e = errno;
				(void) close(f->f_file);
				f->f_file = -1;
				errno = e;
				logerror("sendto");
			}
			continue;
		}
		if (f->f_flags & F_TTY) {
			v->iov_base = "\r\n";
			v->iov_len = 2;
		} else {
			v->iov_base = "\n";
			v->iov_len = 1;
		}
		if (writev(f->f_file, iov, 4) < 0) {
			int e = errno;
			(void) close(f->f_file);
			/*
			 * Check for EBADF on the console due to vhangup() XXX
			 */
			if (e == EBADF && (f->f_flags & F_TTY)) {
				f->f_file = open(f->f_name, O_WRONLY|O_APPEND);
				if (f->f_file < 0)
					logerror(f->f_name);
			} else {
				f->f_file = -1;
				errno = e;
				logerror(f->f_name);
			}
		} else if (flags & SYNC_FILE)
			(void) fsync(f->f_file);
	}

	/*
	 * Output high priority messages to terminals.
	 */
	if (!(flags & ISMARK) && (mask(pri) & Sumask))
		wallmsg(pri, msg, from);

	(void) sigsetmask(omask);
}

/*
 *  INIT -- Initialize syslogd from configuration table
 *
 *	The configuration table consists of a series of lines broken
 *	into two sections by a blank line.  The first section gives a
 *	list of files to log on.  Each line begins with a
 *	comma-separated list of digits or ranges of digits (pairs of
 *	digits separated by a dash); if the priority of a message falls
 *	in the set of digits defined by this list, then the message is
 *	logged in the file corresponding to this line.  If the
 *	following character is an asterisk, then syslogd arranges for
 *	something to be printed every fifteen minutes (even if only a
 *	null line), so that crashes and other events can be localized.
 *	The rest of the line is the pathname of the log file.  The
 *	second section is a list of user names; these people are all
 *	notified when subalert messages occur (if they are logged on).
 *	These lines may also have associated priority lists.
 *
 *	The configuration table will be reread by this routine if a
 *	SIGHUP signal occurs; for that reason, it is tricky about not
 *	re-opening files and closing files it will not be using.
 */

init()
{
	register int i;
	register FILE *cf;
	register struct filed *f;
	register char *p;
	char cline[BUFSIZ];
	struct hostent *hp;
	int pmask, flags;
	long now;
	char *getpmask();

	dprintf("init\n");

	/* flush any pending output */
	flushmsg();

	/*
	 *  Close all open log files.
	 */
	for (f = Files; f < &Files[NLOGS]; f++) {
		if (f->f_file >= 0)
			(void) close(f->f_file);
		f->f_file = -1;
	}

	/* open the configuration file */
	if ((cf = fopen(ConfFile, "r")) == NULL) {
		dprintf("cannot open %s\n", ConfFile);
		f = Files;
		if ((f->f_file = open(ctty, O_WRONLY)) >= 0) {
			strncpy(f->f_name, ctty, sizeof(f->f_name)-1);
			f->f_pmask = mask(LOG_CRIT);
			f->f_flags = F_TTY|F_MARK|F_CONS;
			untty();
		}
		return;
	}

	/*
	 *  Foreach line in the conf table, open that file.
	 */
	f = Files;
	while (fgets(cline, sizeof cline, cf) != NULL) {
		/* check for end-of-section */
		if (cline[0] == '\n')
			break;

		/* strip off newline character */
		p = index(cline, '\n');
		if (p)
			*p = '\0';

		dprintf("F: got line '%s'\n", cline);

		/* extract priority mask and mark flag */
		p = cline;
		flags = 0;
		p = getpmask(p, &pmask);
		if (*p == '*') {
			p++;
			flags |= F_MARK;
		}

		if (f >= &Files[NLOGS])
			continue;

		/* mark entry as used and update flags */
		if (*p == '@') {
			if (!inet)
				continue;
			hp = gethostbyname(++p);
			if (hp == NULL) {
				char buf[100];

				sprintf(buf, "unknown host %s", p);
				errno = 0;
				logerror(buf);
				continue;
			}
			bzero(&f->f_addr, sizeof f->f_addr);
			f->f_addr.sin_family = AF_INET;
			f->f_addr.sin_port = port;
			bcopy(hp->h_addr, (char *) &f->f_addr.sin_addr, hp->h_length);
			f->f_file = socket(AF_INET, SOCK_DGRAM, 0);
			if (f->f_file < 0) {
				logerror("socket");
				continue;
			}
			flags |= F_FORW;
			f->f_pmask = pmask;
			f->f_flags = flags;
			dprintf("Host %s pmask %#x flags %#x\n", p, pmask, flags);
			f++;
			continue;
		}
		strncpy(f->f_name, p, sizeof(f->f_name)-1);
		if ((f->f_file = open(p, O_WRONLY|O_APPEND)) < 0) {
			logerror(p);
			continue;
		}
		if (isatty(f->f_file)) {
			flags |= F_TTY;
			untty();
		}
		if (strcmp(p, ctty) == 0)
			flags |= F_CONS;
		f->f_pmask = pmask;
		f->f_flags = flags;
		dprintf("File %s pmask %#x flags %#x\n", p, pmask, flags);
		f++;
	}

	/*
	 *  Read the list of users.
	 *
	 *	Anyone in this list is informed directly if s/he
	 *	is logged in when a high priority message comes through.
	 */
	Sumask = mask(KERN_EMERG)|mask(LOG_EMERG);
	for (i = 0; i < NSUSERS && fgets(cline, sizeof cline, cf) != NULL; i++) {
		/* strip off newline */
		p = index(cline, '\n');
		if (p)
			*p = '\0';
		dprintf("U: got line '%s'\n", cline);
		p = cline;
		if (isdigit(*p))
			p = getpmask(p, &pmask);
		else
			pmask = mask(LOG_SALERT);
		Susers[i].s_pmask = pmask;
		Sumask |= pmask;
		strncpy(Susers[i].s_name, p, UNAMESZ);
		dprintf("Suser %s pmask %#x\n", p, pmask);
	}

	/* zero the rest of the old superusers */
	while (i < NSUSERS)
		Susers[i++].s_name[0] = '\0';

	/* close the configuration file */
	(void) fclose(cf);

	dprintf("syslogd: restarted\n");
}

/*
 *  WALLMSG -- Write a message to the world at large
 *
 *	Write the specified message to either the entire
 *	world, or a list of approved users.
 */

wallmsg(pri, msg, from)
	int pri;
	char *msg, *from;
{
	register char *p;
	register int i;
	int f, flags, len, e;
	FILE *uf;
	struct utmp ut;
	long now;
	char line[MAXLINE + 100];

	/* open the user login file */
	if ((uf = fopen("/etc/utmp", "r")) == NULL) {
		i = Sumask;
		Sumask = 0;
		logerror("/etc/utmp");
		Sumask = i;
		return;
	}

	time(&now);
	sprintf(line,
	    "\r\n\7Message from syslogd@%s at %.24s ...\r\n%s\r\n",
		from, ctime(&now), msg);
	len = strlen(line);

	/* scan the user login file */
	while (fread(&ut, sizeof ut, 1, uf) == 1) {
		/* is this slot used? */
		if (ut.ut_name[0] == '\0')
			continue;

		/* should we send the message to this user? */
		if (pri != LOG_ALERT) {
			for (i = 0; i < NSUSERS; i++) {
				if ((mask(pri) & Susers[i].s_pmask) == 0)
					continue;
				if (strncmp(Susers[i].s_name, ut.ut_name,
				    UNAMESZ) == 0)
					goto prmsg;
			}
			continue;
		}
	prmsg:

		/* compute the device name */
		p = "/dev/12345678";
		strcpyn(&p[5], ut.ut_line, UNAMESZ);

		/*
		 * Might as well fork instead of using nonblocking I/O
		 * and doing notty().
		 */
		if (fork() == 0) {
			signal(SIGALRM, SIG_DFL);
			alarm(30);
			/* open the terminal */
			f = open(p, O_WRONLY);
			if (f >= 0)
				(void) write(f, line, len);
			exit(0);
		}
	}
	/* close the user login file */
	(void) fclose(uf);
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

/*
 * Make sure every marked file gets written to periodically.
 * Reset the alarm clock to call itself after MarkIntvl minutes.
 */
domark()
{
	char buf[50];
	long now;

	dprintf("domark\n");

	time(&now);
	sprintf(buf, "syslogd: %.24s-- MARK", ctime(&now));
	flushmsg();
	logmsg(0, buf, host, NOCOPY|ISMARK);
	alarm(MarkIntvl * 60);
}

/*
 * Check to see if we should log this message.
 */
chkhost(f)
	struct sockaddr_in *f;
{
	struct hostent *hp;
	extern char *inet_ntoa();

	dprintf("chkhost\n");

	if (f->sin_family != AF_INET) {
		dprintf("Malformed from address\n");
		return (0);
	}
	hp = gethostbyaddr(&f->sin_addr, sizeof(struct in_addr), f->sin_family);
	if (hp == 0) {
		dprintf("Host name for your address (%s) unknown\n",
			inet_ntoa(f->sin_addr));
		return (0);
	}
	strncpy(rhost, hp->h_name, sizeof rhost);
	return (1);
}

flushmsg()
{
	if (count == 0)
		return;
	if (count > 1)
		sprintf(prevdate+18, "last message repeated %d times", count);
	count = 0;
	logmsg(prevpri, prevline, prevhost, prevflags|NOCOPY);
	prevline[0] = '\0';
}

/*
 * Print syslogd errors some place.
 */
logerror(type)
	char *type;
{
	char buf[100];
	long now;

	time(&now);
	if (errno == 0)
		sprintf(buf, "syslogd: %.24s-- %s", ctime(&now), type);
	else if ((unsigned) errno > sys_nerr)
		sprintf(buf, "syslogd: %.24s-- %s: error %d",
			ctime(&now), type, errno);
	else
		sprintf(buf, "syslogd: %.24s-- %s: %s",
			ctime(&now), type, sys_errlist[errno]);
	errno = 0;
	dprintf("%s\n", buf);
	logmsg(LOG_ERR, buf, host, 0);
}

die()
{

	flushmsg();
	dprintf("syslogd: going down\n");
	(void) unlink(logname);
	exit(0);
}

/*
 * getpmask() parses a string cp looking for a set of numbers like
 * '1-5,8,16' and returns in *ppmask the set of bits represented by
 * these numbers.  A notation '1-5' is interpreted to mean 'turn on
 * bits 1 through 5 inclusive'.  getpmask() returns the address of
 * first byte after the number set.
 */
char *
getpmask(cp, ppmask)
	register char *cp;
	unsigned *ppmask;
{
	int count1, count2;
	register int i;

	*ppmask = 0;
	while (isdigit(*cp)) {
		count1 = count2 = 0;
		do {
			count1 = 10 * count1 + (*cp++ - '0');
		} while (isdigit(*cp));
		switch (*cp) {
		case ',':
			++cp;
			/* FALL THRU */
		default:
			*ppmask |= mask(count1);
			continue;

		case '-':
			while (isdigit(*++cp))
				count2 = 10 * count2 + (*cp - '0');
			for (i = count1; i <= count2; ++i)
				*ppmask |= mask(i);
			if (*cp == ',')
				++cp;
			continue;
		}
	}

	return (cp);
}
