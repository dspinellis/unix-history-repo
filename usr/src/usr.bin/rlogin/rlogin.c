/*
 * Copyright (c) 1983, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1983, 1990, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rlogin.c	8.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * rlogin - remote login
 */
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <pwd.h>
#include <setjmp.h>
#include <termios.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <setjmp.h>

#ifdef KERBEROS
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>

#include "krb.h"

#ifndef TIOCPKT_WINDOW
#define	TIOCPKT_WINDOW	0x80
#endif

#endif
struct	winsize winsize;
int	sigwinch();

void		catch_child __P((int));
void		copytochild __P((int));
__dead void	doit __P((sigset_t *));
__dead void	done __P((int));
void		echo __P((char));
u_int		getescape __P((char *));
void		lostpeer __P((int));
void		mode __P((int));
void		msg __P((char *));
void		oob __P((int));
int		reader __P((sigset_t *));
void		sendwindow __P((void));
void		setsignal __P((int));
void		sigwinch __P((int));
void		stop __P((char));
__dead void	usage __P((void));
void		writer __P((void));
void		writeroob __P((int));

#ifdef	KERBEROS
void		warning __P((const char *, ...));
#endif
#ifdef OLDSUN
int		get_window_size __P((int, struct winsize *));
#endif

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct passwd *pw;
	struct servent *sp;
	int uid, options = 0;
	sigset_t smask;
	uid_t uid;
	int argoff, ch, dflag, one;
	char *host, *p, *user, term[1024];
	struct sigaction sa;

	argoff = dflag = 0;
	one = 1;
	host = user = NULL;

	if (p = strrchr(argv[0], '/'))
		++p;
	else
		p = argv[0];

	if (strcmp(p, "rlogin") != 0)
		host = p;

	/* handle "rlogin host flags" */
	if (!host && argc > 2 && argv[1][0] != '-') {
		host = argv[1];
		argoff = 1;
	}

#ifdef KERBEROS
#define	OPTIONS	"8EKLde:k:l:x"
#else
#define	OPTIONS	"8EKLde:l:"
#endif
	while ((ch = getopt(argc - argoff, argv + argoff, OPTIONS)) != EOF)
		switch(ch) {
		case '8':
			eight = 1;
			break;
		case 'E':
			noescape = 1;
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
			noescape = 0;
			escapechar = getescape(optarg);
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
#ifdef CRYPT
#ifdef KERBEROS
		case 'x':
			doencrypt = 1;
			des_set_key(cred.session, schedule);
			break;
#endif
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

	if (!(pw = getpwuid(uid = getuid())))
		errx(1, "unknown user id.");

	if (!user)
		user = pw->pw_name;

	sp = NULL;
#ifdef KERBEROS
	if (use_kerberos) {
		sp = getservbyname((doencrypt ? "eklogin" : "klogin"), "tcp");
		if (sp == NULL) {
			use_kerberos = 0;
			warning("can't get entry for %s/tcp service",
			    doencrypt ? "eklogin" : "klogin");
		}
	}
#endif
	if (sp == NULL)
		sp = getservbyname("login", "tcp");
	if (sp == NULL)
		errx(1, "login/tcp: unknown service.");

	(void)snprintf(term, sizeof(term), "%s/%d",
			((p = getenv("TERM")) ? p : "network"),
			cfgetispeed(0));
	if (rem < 0)
		exit(1);

	/*NOTREACHED*/
}

pid_t child;
struct termios deftt;
struct termios nott;

void
{
	int i;
	struct sigaction sa;

	for (i = 0; i < NCCS; i++)
		nott.c_cc[i] = _POSIX_VDISABLE;
	tcgetattr(0, &deftt);
	nott.c_cc[VSTART] = deftt.c_cc[VSTART];
	nott.c_cc[VSTOP] = deftt.c_cc[VSTOP];
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	sa.sa_handler = SIG_IGN;
	(void)sigaction(SIGINT, &sa, (struct sigaction *) 0);
	setsignal(SIGHUP);
	setsignal(SIGQUIT);
	child = fork();
	if (child == -1) {
		warn("fork");
		done(1);
	}
	signal(SIGINT, SIG_IGN);
	mode(1);
	if (child == 0) {
		if (reader(smask) == 0) {
			msg("connection closed.");
			exit(0);
		}
		sleep(1);
		msg("\007connection closed.");
		exit(1);
	}

	/*
	 * We may still own the socket, and may have a pending SIGURG (or might
	 * receive one soon) that we really want to send to the reader.  When
	 * one of these comes in, the trap copytochild simply copies such
	 * signals to the child. We can now unblock SIGURG and SIGUSR1
	 * that were set above.
	 */
	(void)sigprocmask(SIG_SETMASK, smask, (sigset_t *) 0);
	sa.sa_handler = catch_child;
	(void)sigaction(SIGCHLD, &sa, (struct sigaction *) 0);
	writer();
	msg("closed connection.");
	done(0);
}

/* trap a signal, unless it is being ignored. */
void
setsignal(sig)
	int sig;
{
	struct sigaction sa;
	sigset_t sigs;

	sigemptyset(&sigs);
	sigaddset(&sigs, sig);
	sigprocmask(SIG_BLOCK, &sigs, &sigs);

	sigemptyset(&sa.sa_mask);
	sa.sa_handler = exit;
	sa.sa_flags = SA_RESTART;
	(void)sigaction(sig, &sa, &sa);
	if (sa.sa_handler == SIG_IGN)
		(void)sigaction(sig, &sa, (struct sigaction *) 0);

	(void)sigprocmask(SIG_SETMASK, &sigs, (sigset_t *) 0);
}

__dead void
done(status)
	int status;
{
	pid_t w;
	int wstatus;
	struct sigaction sa;

	mode(0);
	if (child > 0) {
		/* make sure catch_child does not snap it up */
		sigemptyset(&sa.sa_mask);
		sa.sa_handler = SIG_DFL;
		sa.sa_flags = 0;
		(void)sigaction(SIGCHLD, &sa, (struct sigaction *) 0);
		if (kill(child, SIGKILL) >= 0)
			while ((w = wait(&wstatus)) > 0 && w != child)
				continue;
	}
	exit(status);
}

void
catch_child(signo)
	int signo;
{
	int status;
	pid_t pid;

	for (;;) {
		pid = waitpid(-1, &status, WNOHANG|WUNTRACED);
		if (pid == 0)
			return;
		/* if the child (reader) dies, just quit */
		if (pid < 0 || (pid == child && !WIFSTOPPED(status)))
			done(WEXITSTATUS(status) | WTERMSIG(status));
	}
	/* NOTREACHED */
}

/*
 * writer: write to remote: 0 -> line.
 * ~.				terminate
 * ~^Z				suspend rlogin process.
 * ~<delayed-suspend char>	suspend rlogin process, but leave reader alone.
 */
void
writer()
{
	register int bol, local, n;
	char c;

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
			if (!noescape && c == escapechar) {
				local = 1;
				continue;
			}
		} else if (local) {
			local = 0;
			if (c == '.' || c == deftt.c_cc[VEOF]) {
				echo(c);
				break;
			}
			if (c == deftt.c_cc[VSUSP] || c == deftt.c_cc[VDSUSP]) {
				bol = 1;
				echo(c);
				stop(c);
				continue;
			}
			if (c != escapechar)
#ifdef CRYPT
#ifdef KERBEROS
				if (doencrypt)
					(void)des_write(rem,
					    (char *)&escapechar, 1);
				else
#endif
#endif
					(void)write(rem, &escapechar, 1);
		}

#ifdef CRYPT
#ifdef KERBEROS
		if (doencrypt) {
			if (des_write(rem, &c, 1) == 0) {
				msg("line gone");
				break;
			}
		} else
#endif
#endif
			if (write(rem, &c, 1) == 0) {
				msg("line gone");
				break;
			}
		bol = c == deftt.c_cc[VKILL] || c == deftt.c_cc[VEOF] ||
		    c == deftt.c_cc[VINTR] || c == deftt.c_cc[VSUSP] ||
		    c == '\r' || c == '\n';
	}
}

void
#if __STDC__
echo(register char c)
#else
echo(c)
	register char c;
#endif
{
	register char *p;
	char buf[8];

	p = buf;
	c &= 0177;
	*p++ = escapechar;
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
	(void)write(STDOUT_FILENO, buf, p - buf);
}

void
#if __STDC__
stop(char cmdc)
#else
stop(cmdc)
	char cmdc;
#endif
{
	struct sigaction sa;

	struct winsize ws;

	mode(0);
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = SIG_IGN;
	sa.sa_flags = SA_RESTART;
	(void)sigaction(SIGCHLD, &sa, (struct sigaction *) 0);
	(void)kill(cmdc == deftt.c_cc[VSUSP] ? 0 : getpid(), SIGTSTP);
	sa.sa_handler = catch_child;
	(void)sigaction(SIGCHLD, &sa, (struct sigaction *) 0);
	mode(1);
	sigwinch(0);			/* check for size changes */
}

void
sigwinch(signo)
	int signo;
{
	struct winsize ws;

	if (!nosigwin && ioctl(0, TIOCGWINSZ, &ws) == 0 &&
	    memcmp(&ws, &winsize, sizeof(ws))) {
		winsize = ws;
		longjmp(winsizechanged, 1);
	}
}

oob(signo)
	int signo;
{

	out = O_RDWR;
	rcvd = 0;
	while (recv(rem, &mark, 1, MSG_OOB) < 0) {
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
	}
	if (!eight && (mark & TIOCPKT_NOSTOP)) {
		tcgetattr(0, &tt);
		tt.c_iflag &= ~(IXON|IXOFF);
		tt.c_cc[VSTOP] = _POSIX_VDISABLE;
		tt.c_cc[VSTART] = _POSIX_VDISABLE;
		tcsetattr(0, TCSANOW, &tt);
	}
	if (!eight && (mark & TIOCPKT_DOSTOP)) {
		tcgetattr(0, &tt);
		tt.c_iflag |= (IXON|IXOFF);
		tt.c_cc[VSTOP] = deftt.c_cc[VSTOP];
		tt.c_cc[VSTART] = deftt.c_cc[VSTART];
		tcsetattr(0, TCSANOW, &tt);
	}
	if (mark & TIOCPKT_FLUSHWRITE) {
		(void)ioctl(1, TIOCFLUSH, (char *)&out);
		for (;;) {
			if (ioctl(rem, SIOCATMARK, &atmark) < 0) {
				warn("ioctl SIOCATMARK (ignored)");
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
int
reader(smask)
	sigset_t *smask;
{
	pid_t pid;
	int n, remaining;
	char *bufp;
	struct sigaction sa;

#if BSD >= 43 || defined(SUNOS4)
	pid = getpid();		/* modern systems use positives for pid */
#else
	pid = -getpid();	/* old broken systems use negatives */
#endif
	signal(SIGURG, oob);
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	sa.sa_handler = SIG_IGN;
	(void)sigaction(SIGTTOU, &sa, (struct sigaction *) 0);
	sa.sa_handler = oob;
	(void)sigaction(SIGURG, &sa, (struct sigaction *) 0);
	ppid = getppid();
	(void)fcntl(rem, F_SETOWN, pid);
	(void)setjmp(rcvtop);
	(void)sigprocmask(SIG_SETMASK, smask, (sigset_t *) 0);
	bufp = rcvbuf;
	for (;;) {
		while ((remaining = rcvcnt - (bufp - rcvbuf)) > 0) {
			rcvstate = WRITING;
			n = write(STDOUT_FILENO, bufp, remaining);
			if (n < 0) {
				if (errno != EINTR)
					return (-1);
				continue;
			}
			bufp += n;
		}
		bufp = rcvbuf;
		rcvcnt = 0;
		rcvstate = READING;

#ifdef CRYPT
#ifdef KERBEROS
		if (doencrypt)
			rcvcnt = des_read(rem, rcvbuf, sizeof(rcvbuf));
		else
#endif
#endif
			rcvcnt = read(rem, rcvbuf, sizeof (rcvbuf));
		if (rcvcnt == 0)
			return (0);
		if (rcvcnt < 0) {
			if (errno == EINTR)
				continue;
			warn("read");
			return (-1);
		}
	}
}

void
mode(f)
	int f;
{
	struct termios tt;

	switch (f) {
	case 0:
		tcsetattr(0, TCSADRAIN, &deftt);
		break;
	case 1:
		tt = deftt;
		tt.c_oflag &= ~OPOST;
		tt.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
		tt.c_cc[VMIN] = 1;
		tt.c_cc[VTIME] = 0;
		if (eight) {
			tt.c_iflag &= ~(IXON | IXOFF);
			tt.c_cc[VSTOP] = _POSIX_VDISABLE;
			tt.c_cc[VSTART] = _POSIX_VDISABLE;
		}
		/*if (litout)
			lflags |= LLITOUT;*/
		tcsetattr(0, TCSADRAIN, &tt);
		break;

	default:
		return;
	}
}

void
lostpeer(signo)
	int signo;
{
	struct sigaction sa;

	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	sa.sa_handler = SIG_IGN;
	(void)sigaction(SIGPIPE, &sa, (struct sigaction *) 0);
	msg("\007connection closed.");
	done(1);
}

	fprintf(stderr, f, a1, a2, a3);
/* copy SIGURGs to the child process. */
void
copytochild(signo)
	int signo;
{

	(void)kill(child, SIGURG);
}

void
msg(str)
	char *str;
{

	(void)fprintf(stderr, "rlogin: %s\r\n", str);
}

#ifdef KERBEROS
/* VARARGS */
void
#if __STDC__
warning(const char *fmt, ...)
#else
warning(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;

	(void)fprintf(stderr, "rlogin: warning, using standard rlogin: ");
#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, ".\n");
}
#endif

__dead void
usage()
{
	(void)fprintf(stderr,
	    "usage: rlogin [ -%s]%s[-e char] [ -l username ] host\n",
#ifdef KERBEROS
#ifdef CRYPT
	    "8EKLx", " [-k realm] ");
#else
	    "8EKL", " [-k realm] ");
#endif
#else
	    "8EL", " ");
#endif
	exit(1);
}

/*
 * The following routine provides compatibility (such as it is) between older
 * Suns and others.  Suns have only a `ttysize', so we convert it to a winsize.
 */
#ifdef OLDSUN
int
get_window_size(fd, wp)
	int fd;
	struct winsize *wp;
{
	struct ttysize ts;
	int error;

	if ((error = ioctl(0, TIOCGSIZE, &ts)) != 0)
		return (error);
	wp->ws_row = ts.ts_lines;
	wp->ws_col = ts.ts_cols;
	wp->ws_xpixel = 0;
	wp->ws_ypixel = 0;
	return (0);
}
#endif

u_int
getescape(p)
	register char *p;
{
	long val;
	int len;

	if ((len = strlen(p)) == 1)	/* use any single char, including '\' */
		return ((u_int)*p);
					/* otherwise, \nnn */
	if (*p == '\\' && len >= 2 && len <= 4) {
		val = strtol(++p, NULL, 8);
		for (;;) {
			if (!*++p)
				return ((u_int)val);
			if (*p < '0' || *p > '8')
				break;
		}
	}
	msg("illegal option value -- e");
	usage();
	/* NOTREACHED */
}
