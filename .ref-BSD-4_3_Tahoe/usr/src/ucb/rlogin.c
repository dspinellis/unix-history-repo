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
static char sccsid[] = "@(#)rlogin.c	5.11 (Berkeley) 8/7/86";
#endif not lint

/*
 * rlogin - remote login
 */
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <stdio.h>
#include <sgtty.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <setjmp.h>
#include <netdb.h>

# ifndef TIOCPKT_WINDOW
# define TIOCPKT_WINDOW 0x80
# endif TIOCPKT_WINDOW

/* concession to sun */
# ifndef SIGUSR1
# define SIGUSR1 30
# endif SIGUSR1

char	*index(), *rindex(), *malloc(), *getenv(), *strcat(), *strcpy();
struct	passwd *getpwuid();
char	*name;
int	rem;
char	cmdchar = '~';
int	eight;
int	litout;
char	*speeds[] =
    { "0", "50", "75", "110", "134", "150", "200", "300",
      "600", "1200", "1800", "2400", "4800", "9600", "19200", "38400" };
char	term[256] = "network";
extern	int errno;
int	lostpeer();
int	dosigwinch = 0;
#ifndef sigmask
#define sigmask(m)	(1 << ((m)-1))
#endif
#ifdef sun
struct winsize {
	unsigned short ws_row, ws_col;
	unsigned short ws_xpixel, ws_ypixel;
};
#endif sun
struct	winsize winsize;
int	sigwinch(), oob();

/*
 * The following routine provides compatibility (such as it is)
 * between 4.2BSD Suns and others.  Suns have only a `ttysize',
 * so we convert it to a winsize.
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
		return (error);
	wp->ws_row = ts.ts_lines;
	wp->ws_col = ts.ts_cols;
	wp->ws_xpixel = 0;
	wp->ws_ypixel = 0;
	return (0);
}
#else sun
#define get_window_size(fd, wp)	ioctl(fd, TIOCGWINSZ, wp)
#endif sun

main(argc, argv)
	int argc;
	char **argv;
{
	char *host, *cp;
	struct sgttyb ttyb;
	struct passwd *pwd;
	struct servent *sp;
	int uid, options = 0, oldmask;
	int on = 1;

	host = rindex(argv[0], '/');
	if (host)
		host++;
	else
		host = argv[0];
	argv++, --argc;
	if (!strcmp(host, "rlogin"))
		host = *argv++, --argc;
another:
	if (argc > 0 && !strcmp(*argv, "-d")) {
		argv++, argc--;
		options |= SO_DEBUG;
		goto another;
	}
	if (argc > 0 && !strcmp(*argv, "-l")) {
		argv++, argc--;
		if (argc == 0)
			goto usage;
		name = *argv++; argc--;
		goto another;
	}
	if (argc > 0 && !strncmp(*argv, "-e", 2)) {
		cmdchar = argv[0][2];
		argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strcmp(*argv, "-8")) {
		eight = 1;
		argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strcmp(*argv, "-L")) {
		litout = 1;
		argv++, argc--;
		goto another;
	}
	if (host == 0)
		goto usage;
	if (argc > 0)
		goto usage;
	pwd = getpwuid(getuid());
	if (pwd == 0) {
		fprintf(stderr, "Who are you?\n");
		exit(1);
	}
	sp = getservbyname("login", "tcp");
	if (sp == 0) {
		fprintf(stderr, "rlogin: login/tcp: unknown service\n");
		exit(2);
	}
	cp = getenv("TERM");
	if (cp)
		(void) strcpy(term, cp);
	if (ioctl(0, TIOCGETP, &ttyb) == 0) {
		(void) strcat(term, "/");
		(void) strcat(term, speeds[ttyb.sg_ospeed]);
	}
	(void) get_window_size(0, &winsize);
	(void) signal(SIGPIPE, lostpeer);
	/* will use SIGUSR1 for window size hack, so hold it off */
	oldmask = sigblock(sigmask(SIGURG) | sigmask(SIGUSR1));
        rem = rcmd(&host, sp->s_port, pwd->pw_name,
	    name ? name : pwd->pw_name, term, 0);
        if (rem < 0)
                exit(1);
	if (options & SO_DEBUG &&
	    setsockopt(rem, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)) < 0)
		perror("rlogin: setsockopt (SO_DEBUG)");
	uid = getuid();
	if (setuid(uid) < 0) {
		perror("rlogin: setuid");
		exit(1);
	}
	doit(oldmask);
	/*NOTREACHED*/
usage:
	fprintf(stderr,
	    "usage: rlogin host [ -ex ] [ -l username ] [ -8 ] [ -L ]\n");
	exit(1);
}

#define CRLF "\r\n"

int	child;
int	catchild();
int	copytochild(), writeroob();

int	defflags, tabflag;
int	deflflags;
char	deferase, defkill;
struct	tchars deftc;
struct	ltchars defltc;
struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };

doit(oldmask)
{
	int exit();
	struct sgttyb sb;

	(void) ioctl(0, TIOCGETP, (char *)&sb);
	defflags = sb.sg_flags;
	tabflag = defflags & TBDELAY;
	defflags &= ECHO | CRMOD;
	deferase = sb.sg_erase;
	defkill = sb.sg_kill;
	(void) ioctl(0, TIOCLGET, (char *)&deflflags);
	(void) ioctl(0, TIOCGETC, (char *)&deftc);
	notc.t_startc = deftc.t_startc;
	notc.t_stopc = deftc.t_stopc;
	(void) ioctl(0, TIOCGLTC, (char *)&defltc);
	(void) signal(SIGINT, SIG_IGN);
	setsignal(SIGHUP, exit);
	setsignal(SIGQUIT, exit);
	child = fork();
	if (child == -1) {
		perror("rlogin: fork");
		done(1);
	}
	if (child == 0) {
		mode(1);
		if (reader(oldmask) == 0) {
			prf("Connection closed.");
			exit(0);
		}
		sleep(1);
		prf("\007Connection closed.");
		exit(3);
	}

	/*
	 * We may still own the socket, and may have a pending SIGURG
	 * (or might receive one soon) that we really want to send to
	 * the reader.  Set a trap that simply copies such signals to
	 * the child.
	 */
	(void) signal(SIGURG, copytochild);
	(void) signal(SIGUSR1, writeroob);
	(void) sigsetmask(oldmask);
	(void) signal(SIGCHLD, catchild);
	writer();
	prf("Closed connection.");
	done(0);
}

/*
 * Trap a signal, unless it is being ignored.
 */
setsignal(sig, act)
	int sig, (*act)();
{
	int omask = sigblock(sigmask(sig));

	if (signal(sig, act) == SIG_IGN)
		(void) signal(sig, SIG_IGN);
	(void) sigsetmask(omask);
}

done(status)
	int status;
{
	int w;

	mode(0);
	if (child > 0) {
		/* make sure catchild does not snap it up */
		(void) signal(SIGCHLD, SIG_DFL);
		if (kill(child, SIGKILL) >= 0)
			while ((w = wait((union wait *)0)) > 0 && w != child)
				/*void*/;
	}
	exit(status);
}

/*
 * Copy SIGURGs to the child process.
 */
copytochild()
{

	(void) kill(child, SIGURG);
}

/*
 * This is called when the reader process gets the out-of-band (urgent)
 * request to turn on the window-changing protocol.
 */
writeroob()
{

	if (dosigwinch == 0) {
		sendwindow();
		(void) signal(SIGWINCH, sigwinch);
	}
	dosigwinch = 1;
}

catchild()
{
	union wait status;
	int pid;

again:
	pid = wait3(&status, WNOHANG|WUNTRACED, (struct rusage *)0);
	if (pid == 0)
		return;
	/*
	 * if the child (reader) dies, just quit
	 */
	if (pid < 0 || pid == child && !WIFSTOPPED(status))
		done((int)(status.w_termsig | status.w_retcode));
	goto again;
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
	register n;
	register bol = 1;               /* beginning of line */
	register local = 0;

	for (;;) {
		n = read(0, &c, 1);
		if (n <= 0) {
			if (n < 0 && errno == EINTR)
				continue;
			break;
		}
		/*
		 * If we're at the beginning of the line
		 * and recognize a command character, then
		 * we echo locally.  Otherwise, characters
		 * are echo'd remotely.  If the command
		 * character is doubled, this acts as a 
		 * force and local echo is suppressed.
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
			if (c != cmdchar)
				(void) write(rem, &cmdchar, 1);
		}
		if (write(rem, &c, 1) == 0) {
			prf("line gone");
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
	char buf[8];
	register char *p = buf;

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
	(void) write(1, buf, p - buf);
}

stop(cmdc)
	char cmdc;
{
	mode(0);
	(void) signal(SIGCHLD, SIG_IGN);
	(void) kill(cmdc == defltc.t_suspc ? 0 : getpid(), SIGTSTP);
	(void) signal(SIGCHLD, catchild);
	mode(1);
	sigwinch();			/* check for size changes */
}

sigwinch()
{
	struct winsize ws;

	if (dosigwinch && get_window_size(0, &ws) == 0 &&
	    bcmp(&ws, &winsize, sizeof (ws))) {
		winsize = ws;
		sendwindow();
	}
}

/*
 * Send the window size to the server via the magic escape
 */
sendwindow()
{
	char obuf[4 + sizeof (struct winsize)];
	struct winsize *wp = (struct winsize *)(obuf+4);

	obuf[0] = 0377;
	obuf[1] = 0377;
	obuf[2] = 's';
	obuf[3] = 's';
	wp->ws_row = htons(winsize.ws_row);
	wp->ws_col = htons(winsize.ws_col);
	wp->ws_xpixel = htons(winsize.ws_xpixel);
	wp->ws_ypixel = htons(winsize.ws_ypixel);
	(void) write(rem, obuf, sizeof(obuf));
}

/*
 * reader: read from remote: line -> 1
 */
#define	READING	1
#define	WRITING	2

char	rcvbuf[8 * 1024];
int	rcvcnt;
int	rcvstate;
int	ppid;
jmp_buf	rcvtop;

oob()
{
	int out = FWRITE, atmark, n;
	int rcvd = 0;
	char waste[BUFSIZ], mark;
	struct sgttyb sb;

	while (recv(rem, &mark, 1, MSG_OOB) < 0)
		switch (errno) {
		
		case EWOULDBLOCK:
			/*
			 * Urgent data not here yet.
			 * It may not be possible to send it yet
			 * if we are blocked for output
			 * and our input buffer is full.
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
	if (mark & TIOCPKT_WINDOW) {
		/*
		 * Let server know about window size changes
		 */
		(void) kill(ppid, SIGUSR1);
	}
	if (!eight && (mark & TIOCPKT_NOSTOP)) {
		(void) ioctl(0, TIOCGETP, (char *)&sb);
		sb.sg_flags &= ~CBREAK;
		sb.sg_flags |= RAW;
		(void) ioctl(0, TIOCSETN, (char *)&sb);
		notc.t_stopc = -1;
		notc.t_startc = -1;
		(void) ioctl(0, TIOCSETC, (char *)&notc);
	}
	if (!eight && (mark & TIOCPKT_DOSTOP)) {
		(void) ioctl(0, TIOCGETP, (char *)&sb);
		sb.sg_flags &= ~RAW;
		sb.sg_flags |= CBREAK;
		(void) ioctl(0, TIOCSETN, (char *)&sb);
		notc.t_stopc = deftc.t_stopc;
		notc.t_startc = deftc.t_startc;
		(void) ioctl(0, TIOCSETC, (char *)&notc);
	}
	if (mark & TIOCPKT_FLUSHWRITE) {
		(void) ioctl(1, TIOCFLUSH, (char *)&out);
		for (;;) {
			if (ioctl(rem, SIOCATMARK, &atmark) < 0) {
				perror("ioctl");
				break;
			}
			if (atmark)
				break;
			n = read(rem, waste, sizeof (waste));
			if (n <= 0)
				break;
		}
		/*
		 * Don't want any pending data to be output,
		 * so clear the recv buffer.
		 * If we were hanging on a write when interrupted,
		 * don't want it to restart.  If we were reading,
		 * restart anyway.
		 */
		rcvcnt = 0;
		longjmp(rcvtop, 1);
	}

	/*
	 * oob does not do FLUSHREAD (alas!)
	 */

	/*
	 * If we filled the receive buffer while a read was pending,
	 * longjmp to the top to restart appropriately.  Don't abort
	 * a pending write, however, or we won't know how much was written.
	 */
	if (rcvd && rcvstate == READING)
		longjmp(rcvtop, 1);
}

/*
 * reader: read from remote: line -> 1
 */
reader(oldmask)
	int oldmask;
{
#if !defined(BSD) || BSD < 43
	int pid = -getpid();
#else
	int pid = getpid();
#endif
	int n, remaining;
	char *bufp = rcvbuf;

	(void) signal(SIGTTOU, SIG_IGN);
	(void) signal(SIGURG, oob);
	ppid = getppid();
	(void) fcntl(rem, F_SETOWN, pid);
	(void) setjmp(rcvtop);
	(void) sigsetmask(oldmask);
	for (;;) {
		while ((remaining = rcvcnt - (bufp - rcvbuf)) > 0) {
			rcvstate = WRITING;
			n = write(1, bufp, remaining);
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
		rcvcnt = read(rem, rcvbuf, sizeof (rcvbuf));
		if (rcvcnt == 0)
			return (0);
		if (rcvcnt < 0) {
			if (errno == EINTR)
				continue;
			perror("read");
			return (-1);
		}
	}
}

mode(f)
{
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;
	int	lflags;

	(void) ioctl(0, TIOCGETP, (char *)&sb);
	(void) ioctl(0, TIOCLGET, (char *)&lflags);
	switch (f) {

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
	(void) ioctl(0, TIOCSLTC, (char *)ltc);
	(void) ioctl(0, TIOCSETC, (char *)tc);
	(void) ioctl(0, TIOCSETN, (char *)&sb);
	(void) ioctl(0, TIOCLSET, (char *)&lflags);
}

/*VARARGS*/
prf(f, a1, a2, a3, a4, a5)
	char *f;
{

	fprintf(stderr, f, a1, a2, a3, a4, a5);
	fprintf(stderr, CRLF);
}

lostpeer()
{

	(void) signal(SIGPIPE, SIG_IGN);
	prf("\007Connection closed.");
	done(1);
}

