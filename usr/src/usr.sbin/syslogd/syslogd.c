#ifndef lint
static char sccsid[] = "@(#)syslogd.c	4.4 (Berkeley) %G%";
#endif

/*
 *  syslogd -- log system messages
 *
 * This program implements a system log. It takes a series of lines.
 * Each line may have a priority, signified as "<n>" as
 * the first three characters of the line.  If this is
 * not present, a default priority (DefPri) is used, which
 * starts out as LOG_ERR.  The default priority can get
 * changed using "<*>n".
 *
 * To kill syslogd, send a signal 15 (terminate).  A signal 1 (hup) will
 * cause it to reread its configuration file.
 *
 * Defined Constants:
 *
 * DAEMON -- Userid number to setuid to after setup.
 * MAXLINE -- the maximimum line length that can be handled.
 * NLOGS -- the maximum number of simultaneous log files.
 * NUSERS -- the maximum number of people that can
 *	be designated as "superusers" on your system.
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 */

#define DAEMON		1	/* Daemon user-id */
#define	NLOGS		10	/* max number of log files */
#define	NSUSERS		10	/* max number of special users */
#define	MAXLINE		1024	/* maximum line length */

#include <syslog.h>
#include <errno.h>
#include <stdio.h>
#include <utmp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <signal.h>
#include <sysexits.h>
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

/*
 * This structure represents the files that will have log
 * copies printed.
 */

struct filed {
	int	f_file;			/* file descriptor */
	short	f_pmask;		/* priority mask */
	short	f_flags;		/* see #defines below */
	struct	sockaddr_in f_addr;	/* forwarding address */
	char	f_name[248];		/* filename */
};

#define F_TTY	01		/* file is a tty */
#define F_MARK	02		/* write to the file periodically */
#define F_FORW	04		/* forward message to another host */

struct filed	Files[NLOGS];

/* list of superusers */
struct susers {
	short	s_pmask;		/* priority mask */
	char	s_name[UNAMESZ+1];
};

struct	susers Susers[NSUSERS];

int	Debug;			/* debug flag */
int	LogFile;		/* log file descriptor */
int	DefPri = LOG_ERR;	/* the default priority for untagged msgs */
int	Sumask;			/* lowest priority written to super-users */
int	MarkIntvl = 15;		/* mark interval in minutes */
char	*ConfFile = defconf;	/* configuration file */
char	host[32];		/* our hostname */
char	rhost[32];		/* hostname of sender (forwarded messages) */
int	inet = 0;		/* non-zero if INET sockets are being used */
int	port;			/* port number for INET connections */

extern	int errno, sys_nerr;
extern	char *sys_errlist[];
extern	char *ctime();

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *p;
	int klog, funix, finet, defreadfds, len;
	struct sockaddr_un sun, fromunix;
	struct sockaddr_in sin, frominet;
	FILE *fp;
	char line[MSG_BSIZE + 1];
	extern int die(), domark();

	sun.sun_family = AF_UNIX;
	strncpy(sun.sun_path, logname, sizeof sun.sun_path);
	gethostname(host, sizeof host);

	while (--argc > 0) {
		p = *++argv;
		if (p[0] == '-') {
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
			}
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
		i = open("/dev/tty", O_RDWR);
	  	if (i >= 0) {
			ioctl(i, TIOCNOTTY, (char *)0);
			(void) close(i);
	  	}
	}
	signal(SIGTERM, die);
	funix = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (funix >= 0 && bind(funix, &sun,
	    sizeof(sun.sun_family)+strlen(sun.sun_path)) < 0) {
		close(funix);
		funix = -1;
	}
	if (funix < 0) {
		fp = fopen(ctty, "w");
		fprintf(fp, "\r\nsyslog: cannot create %s (%d)\r\n", logname, errno);
		dprintf("cannot create %s (%d)\n", logname, errno);
		exit(1);
	}
	defreadfds = 1 << funix;
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
		defreadfds |= 1 << finet;
		inet = 1;
	}
	if ((klog = open("/dev/klog", O_RDONLY)) >= 0)
		defreadfds |= 1 << klog;
	else
		dprintf("can't open /dev/klog (%d)\n", errno);

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
	signal(SIGALRM, domark);
	alarm(MarkIntvl * 60);

	for (;;) {
		int domain, nfds, readfds = defreadfds;

		nfds = select(20, &readfds, 0, 0, 0);
		if (nfds == 0)
			continue;
		if (nfds < 0) {
			if (errno == EINTR)
				continue;
			logerror("select");
			continue;
		}
		if (readfds & (1 << funix)) {
			domain = AF_UNIX;
			len = sizeof fromunix;
			i = recvfrom(funix, line, MAXLINE, 0, &fromunix, &len);
		} else if (readfds & (1 << finet)) {
			domain = AF_INET;
			len = sizeof frominet;
			i = recvfrom(finet, line, MAXLINE, 0, &frominet, &len);
		} else {
			i = read(klog, line, sizeof(line) - 1);
			if (i < 0) {
				logerror("read");
				continue;
			}
			line[i] = '\0';
			printsys(line);
			continue;
		}
		if (i < 0) {
			if (errno == EINTR)
				continue;
			logerror("recvfrom");
			continue;
		}
		if (domain == AF_INET && !chkhost(&frominet))
			continue;
		line[i] = '\0';
		printline(domain != AF_INET, line);
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
	pri = DefPri;
	p = msg;
	if (p[0] == '<' && p[2] == '>') {
		switch (p[1]) {
		case '*':	/* reset default message priority */
			dprintf("default priority = %c\n", p[3]);
			c = p[3] - '0';
			if ((unsigned) c <= 9)
				DefPri = c;
			break;

		case '$':	/* reconfigure */
			dprintf("reconfigure\n");
			init();
		}
		p++;
		pri = *p++ - '0';
		p++;
		if ((unsigned) pri > LOG_DEBUG)
			pri = DefPri;
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

	logmsg(pri, line, local ? host : rhost);
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
	int pri;
	long now;

	time(&now);
	for (p = msg; *p != '\0'; ) {
		/* test for special codes */
		pri = DefPri;
		if (p[0] == '<' && p[2] == '>') {
			switch (p[1]) {
			case '*':	/* reset default message priority */
				dprintf("default priority = %c\n", p[3]);
				c = p[3] - '0';
				if ((unsigned) c <= 9)
					DefPri = c;
				break;

			case '$':	/* reconfigure */
				dprintf("reconfigure\n");
				init();
			}
			p++;
			pri = *p++ - '0';
			p++;
			if ((unsigned) pri > LOG_DEBUG)
				pri = DefPri;
		}

		q = line;
		sprintf(q, "vmunix: %.15s-- ", ctime(&now) + 4);
		q += strlen(q);
		while ((c = *p++) != '\0' && c != '\n' &&
		    q < &line[sizeof(line) - 1])
			*q++ = c;
		*q = '\0';
		logmsg(pri, line, host);
	}
}

/*
 * Log a message to the appropriate log files, users, etc. based on
 * the priority.
 */

logmsg(pri, msg, from)
	int	pri;
	char	*msg, *from;
{
	char line[MAXLINE + 1];
	register struct filed *f;
	register int l;
	struct iovec iov[4];
	register struct iovec *v = iov;

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
		if (f->f_file < 0 || f->f_pmask < pri)
			continue;
		if (f->f_flags & F_FORW) {
			sprintf(line, "<%d>%s", pri, msg);
			l = strlen(line);
			if (l > MAXLINE)
				l = MAXLINE;
			if (sendto(f->f_file, line, l, 0,
			    &f->f_addr, sizeof f->f_addr) != l)
				logerror("sendto");
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
			logerror(f->f_name);
			(void) close(f->f_file);
			f->f_file = -1;
		}
	}

	/*
	 * Output high priority messages to terminals.
	 */
	if (pri <= Sumask)
		wallmsg(pri, msg, from);
}

/*
 *  INIT -- Initialize syslog from configuration table
 *
 *	The configuration table consists of a series of lines
 *	broken into two sections by a blank line.  The first
 *	section gives a list of files to log on.  The first
 *	character is a digit which is the priority mask for
 *	that file.  If the second character is an asterisk, then
 *	syslog arranges for something to be printed every fifteen
 *	minutes (even if only a null line), so that crashes and
 *	other events can be localized.  The rest of the line is
 *	the pathname of the log file.  The second section is
 *	a list of user names; these people are all notified
 *	when subalert messages occur (if they are logged on).
 *
 *	The configuration table will be reread by this routine
 *	if a signal 1 occurs; for that reason, it is tricky
 *	about not re-opening files and closing files it will
 *	not be using.
 */

init()
{
	register int i;
	register FILE *cf;
	register struct filed *f;
	register char *p;
	char cline[BUFSIZ];
	struct servent *sp;
	struct hostent *hp;
	int pmask, flags;
	long now;

	dprintf("init\n");

	/* ignore interrupts during this routine */
	signal(SIGHUP, SIG_IGN);

	/*
	 *  Close all open log files.
	 */
	for (f = Files; f < &Files[NLOGS]; f++) {
		if (f->f_file < 0)
			(void) close(f->f_file);
		f->f_file = -1;
	}

	/* open the configuration file */
	if ((cf = fopen(ConfFile, "r")) == NULL) {
		dprintf("cannot open %s\n", ConfFile);
		f = Files;
		if ((f->f_file = open(ctty, O_WRONLY)) >= 0) {
			strncpy(f->f_name, ctty, sizeof(f->f_name)-1);
			f->f_pmask = LOG_CRIT;
			f->f_flags = F_TTY|F_MARK;
		}
		return;
	}

	/*
	 *  Foreach line in the conf table, open that file.
	 */
	f = Files;
	sp = getservbyname("syslogd", "udp");
	while (fgets(cline, sizeof cline, cf) != NULL) {
		/* check for end-of-section */
		if (cline[0] == '\n')
			break;

		/* strip off newline character */
		for (p = cline; *p != '\0'; p++)
			if (*p == '\n') {
				*p = '\0';
				break;
			}

		dprintf("F: got line '%s'\n", cline);

		/* extract priority mask and mark flag */
		p = cline;
		flags = 0;
		pmask = *p++ - '0';
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
			dprintf("Host %s pmask %d flags %o\n", p, pmask, flags);
			f++;
			continue;
		}
		strncpy(f->f_name, p, sizeof(f->f_name)-1);
		if ((f->f_file = open(p, O_WRONLY|O_APPEND)) < 0) {
			logerror(p);
			continue;
		}
		if (isatty(f->f_file))
			flags |= F_TTY;
		f->f_pmask = pmask;
		f->f_flags = flags;
		dprintf("File %s pmask %d flags %o\n", p, pmask, flags);
		f++;
	}

	/*
	 *  Read the list of users.
	 *
	 *	Anyone in this list is informed directly if s/he
	 *	is logged in when a high priority message comes through.
	 */
	Sumask = LOG_SALERT;
	for (i = 0; i < NSUSERS && fgets(cline, sizeof cline, cf) != NULL; i++) {
		/* strip off newline */
		for (p = cline; *p != '\0'; p++)
			if (*p == '\n') {
				*p = '\0';
				break;
			}
		dprintf("U: got line '%s'\n", cline);
		p = cline;
		if (isdigit(*p)) {
			Susers[i].s_pmask = pmask = *p++ - '0';
			if (pmask > Sumask)
				Sumask = pmask;
		} else
			Susers[i].s_pmask = pmask = LOG_SALERT;
		strncpy(Susers[i].s_name, p, UNAMESZ);
		dprintf("Suser %s pmask %d\n", p, pmask);
	}

	/* zero the rest of the old superusers */
	while (i < NSUSERS)
		Susers[i++].s_name[0] = '\0';

	/* close the configuration file */
	(void) fclose(cf);

	dprintf("syslogd: restarted\n");

	/* arrange for signal 1 to reconfigure */
	signal(SIGHUP, init);
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
		logerror("/etc/utmp");
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
				if (pri > Susers[i].s_pmask)
					continue;
				if (strncmp(Susers[i].s_name, ut.ut_name,
				    sizeof ut.ut_name) == 0)
					goto prmsg;
			}
			continue;
		}
		prmsg:

		/* compute the device name */
		p = "/dev/12345678";
		strcpyn(&p[5], ut.ut_line, UNAMESZ);

		/* open the terminal */
		f = open(p, O_WRONLY|O_NDELAY);
		if (f < 0)
			continue;
		if ((flags = fcntl(f, F_GETFL, 0)) == -1)
			continue;
		if (fcntl(f, F_SETFL, flags | FNDELAY) == -1)
			goto oldway;
		i = write(f, line, len);
		e = errno;
		(void) fcntl(f, F_SETFL, flags);
		if (i == len || e != EWOULDBLOCK) {
			(void) close(f);
			continue;
		}
	oldway:
		if (fork() == 0) {
			(void) write(f, line, len);
			exit(0);
		}
		(void) close(f);
	}

	/* close the user login file */
	(void) close(uf);
}

/*
 * Make sure every marked file gets written to periodically.
 * Reset the alarm clock to call itself after MarkIntvl minutes.
 */
domark()
{
	register struct filed *f;
	struct stat stb;
	char buf[40];
	long now;

	dprintf("domark\n");

	time(&now);
	for (f = Files; f < &Files[NLOGS]; f++) {
		if (!(f->f_flags & F_MARK))
			continue;
		if (f->f_file < 0 || fstat(f->f_file, &stb) < 0)
			continue;
		if (stb.st_mtime >= now - MarkIntvl * 60)
			continue;
		sprintf(buf, "syslogd: %.24s-- MARK", ctime(&now));
		logmsg(-1, buf, host);
	}
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
	logmsg(LOG_ERR, buf, host);
}

die()
{

	dprintf("syslogd: going down\n");
	(void) unlink(logname);
	exit(0);
}
