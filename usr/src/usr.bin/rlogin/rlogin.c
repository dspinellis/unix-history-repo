/*-
 * Copyright (c) 1983, 1990 The Regents of the University of California.
 * All rights reserved.
 *
%sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rlogin.c	5.24 (Berkeley) %G%";
#endif /* not lint */

/*
 * $Source: mit/rlogin/RCS/rlogin.c,v $
 * $Header: mit/rlogin/RCS/rlogin.c,v 5.2 89/07/26 12:11:21 kfall Exp Locker: kfall $
 */

/*
 * rlogin - remote login
 */
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

#include <netinet/in.h>
#include <netdb.h>

#include <sgtty.h>
#include <setjmp.h>
#include <errno.h>
#include <varargs.h>
#include <pwd.h>
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <setjmp.h>

#ifdef KERBEROS
#include <kerberosIV/krb.h>

#ifndef TIOCPKT_WINDOW
#define	TIOCPKT_WINDOW	0x80
#endif

#endif
struct	winsize winsize;
int	sigwinch();

#ifndef sun
#define	get_window_size(fd, wp)	ioctl(fd, TIOCGWINSZ, wp)
#endif

void exit();

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	struct passwd *pw;
	struct servent *sp;
	int uid, options = 0;
	struct sgttyb ttyb;
	long omask;
	int argoff, ch, dflag, one, uid;
	char *host, *p, *user, term[1024];
	void lostpeer();
	char *getenv();

	argoff = dflag = 0;
	one = 1;
	host = user = NULL;
	cmdchar = '~';

	if (p = rindex(argv[0], '/'))
		++p;
	else
		p = argv[0];

	if (strcmp(p, "rlogin"))
		host = p;

	/* handle "rlogin host flags" */
	if (!host && argc > 2 && argv[1][0] != '-') {
		host = argv[1];
		argoff = 1;
	}

#ifdef KERBEROS
#define	OPTIONS	"8KLde:k:l:x"
#else
#define	OPTIONS	"8KLde:l:"
#endif
	while ((ch = getopt(argc - argoff, argv + argoff, OPTIONS)) != EOF)
		switch(ch) {
		case '8':
			eight = 1;
			break;
		case 'K':
#ifdef KERBEROS
			use_kerberos = 0;
#endif
			break;
		case 'L':
			litout = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'e':
			cmdchar = optarg[0];
			break;
#ifdef KERBEROS
		case 'k':
			dest_realm = dst_realm_buf;
			(void)strncpy(dest_realm, optarg, REALM_SZ);
			break;
#endif
		case 'l':
			user = optarg;
			break;
#ifdef KERBEROS
		case 'x':
			encrypt = 1;
			des_set_key(cred.session, schedule);
			break;
#endif
		case '?':
		default:
			usage();
		}
	optind += argoff;
	argc -= optind;
	argv += optind;

	/* if haven't gotten a host yet, do so */
	if (!host && !(host = *argv++))
		usage();

	if (*argv)
		usage();

	if (!(pw = getpwuid(uid = getuid()))) {
		(void)fprintf(stderr, "rlogin: unknown user id.\n");
		exit(1);
	}
	if (!user)
		user = pw->pw_name;

	sp = NULL;
#ifdef KERBEROS
	if (use_kerberos) {
		sp = getservbyname((encrypt ? "eklogin" : "klogin"), "tcp");
		if (sp == NULL) {
			use_kerberos = 0;
			warning("can't get entry for %s/tcp service",
			    encrypt ? "eklogin" : "klogin");
		}
	}
#endif
	if (sp == NULL)
		sp = getservbyname("login", "tcp");
	if (sp == NULL) {
		(void)fprintf(stderr, "rlogin: login/tcp: unknown service.\n");
		exit(1);
	}

	(void)strcpy(term, (p = getenv("TERM")) ? p : "network");
	if (ioctl(0, TIOCGETP, &ttyb) == 0) {
		(void)strcat(term, "/");
		(void)strcat(term, speeds[ttyb.sg_ospeed]);
	}
	if (rem < 0)
		exit(1);

	/*NOTREACHED*/
}

int child, defflags, deflflags, tabflag;
char deferase, defkill;
struct tchars deftc;
struct ltchars defltc;
struct tchars notc = { -1, -1, -1, -1, -1, -1 };
struct ltchars noltc = { -1, -1, -1, -1, -1, -1 };

{
	struct sgttyb sb;
	void catch_child(), copytochild(), exit(), writeroob();

	(void)ioctl(0, TIOCGETP, (char *)&sb);
	defflags = sb.sg_flags;
	tabflag = defflags & TBDELAY;
	defflags &= ECHO | CRMOD;
	deferase = sb.sg_erase;
	defkill = sb.sg_kill;
	(void)ioctl(0, TIOCLGET, (char *)&deflflags);
	(void)ioctl(0, TIOCGETC, (char *)&deftc);
	notc.t_startc = deftc.t_startc;
	notc.t_stopc = deftc.t_stopc;
	(void)ioctl(0, TIOCGLTC, (char *)&defltc);
	(void)signal(SIGINT, SIG_IGN);
	setsignal(SIGHUP, exit);
	setsignal(SIGQUIT, exit);
	child = fork();
	if (child == -1) {
		(void)fprintf(stderr, "rlogin: fork: %s.\n", strerror(errno));
		done(1);
	}
	signal(SIGINT, SIG_IGN);
	mode(1);
	if (child == 0) {
		if (reader(omask) == 0) {
			msg("connection closed.");
			exit(0);
		}
		sleep(1);
		msg("\007connection closed.");
		exit(1);
	}

	/*
	 * We may still own the socket, and may have a pending SIGURG (or might
	 * receive one soon) that we really want to send to the reader.  Set a
	 * trap that simply copies such signals to the child.
	 */
	(void)signal(SIGURG, copytochild);
	(void)signal(SIGUSR1, writeroob);
	(void)sigsetmask(omask);
	(void)signal(SIGCHLD, catch_child);
	writer();
	msg("closed connection.");
	done(0);
}

/* trap a signal, unless it is being ignored. */
setsignal(sig, act)
	int sig;
	void (*act)();
{
	int omask = sigblock(sigmask(sig));

	if (signal(sig, act) == SIG_IGN)
		(void)signal(sig, SIG_IGN);
	(void)sigsetmask(omask);
}

done(status)
	int status;
{
	int w;

	mode(0);
	if (child > 0) {
		/* make sure catch_child does not snap it up */
		(void)signal(SIGCHLD, SIG_DFL);
		if (kill(child, SIGKILL) >= 0)
			while ((w = wait((union wait *)0)) > 0 && w != child);
	}
	exit(status);
}

void
catch_child()
{
	union wait status;
	int pid;

	for (;;) {
		pid = wait3(&status, WNOHANG|WUNTRACED, (struct rusage *)0);
		if (pid == 0)
			return;
		/* if the child (reader) dies, just quit */
		if (pid < 0 || pid == child && !WIFSTOPPED(status))
			done((int)(status.w_termsig | status.w_retcode));
	}
	/* NOTREACHED */
}

/*
 * writer: write to remote: 0 -> line.
 * ~.	terminate
 * ~^Z	suspend rlogin process.
 * ~^Y  suspend rlogin process, but leave reader alone.
 */
writer()
{
	char c;
	register int bol, local, n;

	bol = 1;			/* beginning of line */
	local = 0;
	/*
	 * Handle SIGWINCH's with in-band signaling of new
	 * window size.  It seems reasonable that we flush
	 * pending input and not force out of band signal
	 * as this most likely will occur from an input device
	 * other than the keyboard (e.g. a mouse).
	 *
	 * The hack of using 0377 to signal an in-band signal
	 * is pretty bad, but otherwise we'd be forced to
	 * either get complicated (use MSG_OOB) or go to a
	 * serious (telnet-style) protocol.
	 */
	if (setjmp(winsizechanged)) {
		char obuf[4 + sizeof (struct winsize)];
		struct winsize *wp = (struct winsize *)(obuf+4);

		obuf[0] = 0377;			/* XXX */
		obuf[1] = 0377;			/* XXX */
		obuf[2] = 's';			/* XXX */
		obuf[3] = 's';			/* XXX */
		wp->ws_row = htons(winsize.ws_row);
		wp->ws_col = htons(winsize.ws_col);
		wp->ws_xpixel = htons(winsize.ws_xpixel);
		wp->ws_ypixel = htons(winsize.ws_ypixel);
		(void) write(rem, obuf, 4+sizeof (*wp));
	}

	for (;;) {
		n = read(STDIN_FILENO, &c, 1);
		if (n <= 0) {
			if (n < 0 && errno == EINTR)
				continue;
			break;
		}
		if (!eight)
			c &= 0177;
		/*
		 * If we're at the beginning of the line and recognize a
		 * command character, then we echo locally.  Otherwise,
		 * characters are echo'd remotely.  If the command character
		 * is doubled, this acts as a force and local echo is
		 * suppressed.
		 */
		if (bol) {
			bol = 0;
			if (c == cmdchar) {
				bol = 0;
				local = 1;
				continue;
			}
		} else if (local) {
			local = 0;
			if (c == '.' || c == deftc.t_eofc) {
				echo(c);
				break;
			}
			if (c == defltc.t_suspc || c == defltc.t_dsuspc) {
				bol = 1;
				echo(c);
				stop(c);
				continue;
			}
			if (c != cmdchar) {
#ifdef KERBEROS
				if (encrypt) {
					(void)des_write(rem, &cmdchar, 1);
				} else
#endif
					(void)write(rem, &cmdchar, 1);
			}
		}

#ifdef KERBEROS
		if (encrypt) {
			if (des_write(rem, &c, 1) == 0) {
				msg("line gone");
				break;
			}
		} else
#endif
			if (write(rem, &c, 1) == 0) {
				msg("line gone");
				break;
			}
		bol = c == defkill || c == deftc.t_eofc ||
		    c == deftc.t_intrc || c == defltc.t_suspc ||
		    c == '\r' || c == '\n';
	}
}

echo(c)
register char c;
{
	register char *p;
	char buf[8];

	p = buf;
	c &= 0177;
	*p++ = cmdchar;
	if (c < ' ') {
		*p++ = '^';
		*p++ = c + '@';
	} else if (c == 0177) {
		*p++ = '^';
		*p++ = '?';
	} else
		*p++ = c;
	*p++ = '\r';
	*p++ = '\n';
	(void)write(1, buf, p - buf);
}

stop(cmdc)
	char cmdc;
{
	struct winsize ws;

	mode(0);
	(void)signal(SIGCHLD, SIG_IGN);
	(void)kill(cmdc == defltc.t_suspc ? 0 : getpid(), SIGTSTP);
	(void)signal(SIGCHLD, catch_child);
	mode(1);
	sigwinch();			/* check for size changes */
}

void
sigwinch()
{
	struct winsize ws;

	if (!nosigwin && ioctl(0, TIOCGWINSZ, &ws) == 0 &&
	    bcmp(&ws, &winsize, sizeof(ws))) {
		winsize = ws;
		longjmp(winsizechanged, 1);
	}
}

oob()
{

	out = FWRITE;
	rcvd = 0;
	while (recv(rem, &mark, 1, MSG_OOB) < 0)
		switch (errno) {
		case EWOULDBLOCK:
			/*
			 * Urgent data not here yet.  It may not be possible
			 * to send it yet if we are blocked for output and
			 * our input buffer is full.
			 */
			if (rcvcnt < sizeof(rcvbuf)) {
				n = read(rem, rcvbuf + rcvcnt,
				    sizeof(rcvbuf) - rcvcnt);
				if (n <= 0)
					return;
				rcvd += n;
			} else {
				n = read(rem, waste, sizeof(waste));
				if (n <= 0)
					return;
			}
			continue;
		default:
			return;
	}
	if (!eight && (mark & TIOCPKT_NOSTOP)) {
		notc.t_stopc = -1;
		notc.t_startc = -1;
		(void)ioctl(0, TIOCSETC, (char *)&notc);
	}
	if (!eight && (mark & TIOCPKT_DOSTOP)) {
		notc.t_stopc = deftc.t_stopc;
		notc.t_startc = deftc.t_startc;
		(void)ioctl(0, TIOCSETC, (char *)&notc);
	}
	if (mark & TIOCPKT_FLUSHWRITE) {
		(void)ioctl(1, TIOCFLUSH, (char *)&out);
		for (;;) {
			if (ioctl(rem, SIOCATMARK, &atmark) < 0) {
				(void)fprintf(stderr, "rlogin: ioctl: %s.\n",
				    strerror(errno));
				break;
			}
			if (atmark)
				break;
			n = read(rem, waste, sizeof (waste));
			if (n <= 0)
				break;
		}
		/*
		 * Don't want any pending data to be output, so clear the recv
		 * buffer.  If we were hanging on a write when interrupted,
		 * don't want it to restart.  If we were reading, restart
		 * anyway.
		 */
		rcvcnt = 0;
		longjmp(rcvtop, 1);
	}

	/* oob does not do FLUSHREAD (alas!) */

	/*
	 * If we filled the receive buffer while a read was pending, longjmp
	 * to the top to restart appropriately.  Don't abort a pending write,
	 * however, or we won't know how much was written.
	 */
	if (rcvd && rcvstate == READING)
		longjmp(rcvtop, 1);
}

/* reader: read from remote: line -> 1 */
reader(omask)
	int omask;
{
	void oob();

#if !defined(BSD) || BSD < 43
	int pid = -getpid();
#else
	int pid = getpid();
#endif
	int n, remaining;
	char *bufp = rcvbuf;

	signal(SIGURG, oob);
	(void)signal(SIGTTOU, SIG_IGN);
	(void)signal(SIGURG, oob);
	ppid = getppid();
	(void)fcntl(rem, F_SETOWN, pid);
	(void)setjmp(rcvtop);
	(void)sigsetmask(omask);
	for (;;) {
		while ((remaining = rcvcnt - (bufp - rcvbuf)) > 0) {
			rcvstate = WRITING;
			n = write(1, bufp, remaining);
			if (n < 0) {
				if (errno != EINTR)
					return(-1);
				continue;
			}
			bufp += n;
		}
		bufp = rcvbuf;
		rcvcnt = 0;
		rcvstate = READING;

#ifdef KERBEROS
		if (encrypt)
			rcvcnt = des_read(rem, rcvbuf, sizeof(rcvbuf));
		else
#endif
			rcvcnt = read(rem, rcvbuf, sizeof (rcvbuf));
		if (rcvcnt == 0)
			return (0);
		if (rcvcnt < 0) {
			if (errno == EINTR)
				continue;
			(void)fprintf(stderr, "rlogin: read: %s.\n",
			    strerror(errno));
			return(-1);
		}
	}
}

mode(f)
{
	struct ltchars *ltc;
	struct sgttyb sb;
	struct tchars *tc;
	int lflags;

	(void)ioctl(0, TIOCGETP, (char *)&sb);
	(void)ioctl(0, TIOCLGET, (char *)&lflags);
	switch(f) {
	case 0:
		sb.sg_flags &= ~(CBREAK|RAW|TBDELAY);
		sb.sg_flags |= defflags|tabflag;
		tc = &deftc;
		ltc = &defltc;
		sb.sg_kill = defkill;
		sb.sg_erase = deferase;
		lflags = deflflags;
		break;
	case 1:
		sb.sg_flags |= (eight ? RAW : CBREAK);
		sb.sg_flags &= ~defflags;
		/* preserve tab delays, but turn off XTABS */
		if ((sb.sg_flags & TBDELAY) == XTABS)
			sb.sg_flags &= ~TBDELAY;
		tc = &notc;
		ltc = &noltc;
		sb.sg_kill = sb.sg_erase = -1;
		if (litout)
			lflags |= LLITOUT;
		break;
	default:
		return;
	}
	(void)ioctl(0, TIOCSLTC, (char *)ltc);
	(void)ioctl(0, TIOCSETC, (char *)tc);
	(void)ioctl(0, TIOCSETN, (char *)&sb);
	(void)ioctl(0, TIOCLSET, (char *)&lflags);
}

void
lostpeer()
{
	(void)signal(SIGPIPE, SIG_IGN);
	msg("\007connection closed.");
	done(1);
}

	fprintf(stderr, f, a1, a2, a3);
/* copy SIGURGs to the child process. */
void
copytochild()
{
	(void)kill(child, SIGURG);
}

msg(str)
	char *str;
{
	(void)fprintf(stderr, "rlogin: %s\r\n", str);
}

#ifdef KERBEROS
/* VARARGS */
warning(va_alist)
va_dcl
{
	va_list ap;
	char *fmt;

	(void)fprintf(stderr, "rlogin: warning, using standard rlogin: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, ".\n");
}
#endif

usage()
{
	(void)fprintf(stderr,
	    "usage: rlogin [ -%s]%s[-e char] [ -l username ] host\n",
#ifdef KERBEROS
	    "8Lx", " [-k realm] ");
#else
	    "8L", " ");
#endif
	exit(1);
}

/*
 * The following routine provides compatibility (such as it is) between 4.2BSD
 * Suns and others.  Suns have only a `ttysize', so we convert it to a winsize.
 */
#ifdef sun
int
get_window_size(fd, wp)
	int fd;
	struct winsize *wp;
{
	struct ttysize ts;
	int error;

	if ((error = ioctl(0, TIOCGSIZE, &ts)) != 0)
		return(error);
	wp->ws_row = ts.ts_lines;
	wp->ws_col = ts.ts_cols;
	wp->ws_xpixel = 0;
	wp->ws_ypixel = 0;
	return(0);
}
#endif
