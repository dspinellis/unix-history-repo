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
static char sccsid[] = "@(#)rlogind.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * remote login server:
 *	remuser\0
 *	locuser\0
 *	terminal info\0
 *	data
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <sgtty.h>
#include <stdio.h>
#include <netdb.h>
#include <syslog.h>
#include <strings.h>

extern	errno;
int	reapchild();
struct	passwd *getpwnam();
char	*crypt(), *malloc();

main(argc, argv)
	int argc;
	char **argv;
{
	int on = 1, options = 0, fromlen;
	struct sockaddr_in from;

	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
		openlog(argv[0], LOG_PID, 0);
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}
	doit(0, &from);
}

int	child;
int	cleanup();
int	netf;
extern	errno;
char	*line;

doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	int i, p, t, pid, on = 1;
	register struct hostent *hp;
	char c;

	alarm(60);
	read(f, &c, 1);
	if (c != 0)
		exit(1);
	alarm(0);
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	hp = gethostbyaddr(&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp == 0) {
		char buf[BUFSIZ];

		fatal(f, sprintf(buf, "Host name for your address (%s) unknown",
			inet_ntoa(fromp->sin_addr)));
	}
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED)
		fatal(f, "Permission denied");
	write(f, "", 1);
	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;
		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
			p = open(line, 2);
			if (p > 0)
				goto gotpty;
		}
	}
	fatal(f, "All network ports in use");
	/*NOTREACHED*/
gotpty:
	netf = f;
	line[strlen("/dev/")] = 't';
#ifdef DEBUG
	{ int tt = open("/dev/tty", 2);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}
#endif
	t = open(line, 2);
	if (t < 0)
		fatalperror(f, line, errno);
	{ struct sgttyb b;
	  gtty(t, &b); b.sg_flags = RAW|ANYP; stty(t, &b);
	}
	pid = fork();
	if (pid < 0)
		fatalperror(f, "", errno);
	if (pid == 0) {
		close(f), close(p);
		dup2(t, 0), dup2(t, 1), dup2(t, 2);
		close(t);
		execl("/bin/login", "login", "-r", hp->h_name, 0);
		fatalperror(2, "/bin/login", errno);
		/*NOTREACHED*/
	}
	close(t);
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	ioctl(p, TIOCPKT, &on);
	signal(SIGTSTP, SIG_IGN);
	signal(SIGCHLD, cleanup);
	protocol(f, p);
	cleanup();
}

char	magic[2] = { 0377, 0377 };

/*
 * Handle a "control" request (signaled by magic being present)
 * in the data stream.  For now, we are only willing to handle
 * window size changes.
 */
control(pty, cp, n)
	int pty;
	char *cp;
	int n;
{
	struct winsize *wp;

	if (n < 4+sizeof (*wp) || cp[2] != 's' || cp[3] != 's')
		return (0);
	wp = (struct winsize *)(cp+4);
	wp->ws_row = ntohs(wp->ws_row);
	wp->ws_col = ntohs(wp->ws_col);
	wp->ws_xpixel = ntohs(wp->ws_xpixel);
	wp->ws_ypixel = ntohs(wp->ws_ypixel);
	(void)ioctl(pty, TIOCSWINSZ, wp);
	return (4+sizeof (*wp));
}

/*
 * rlogin "protocol" machine.
 */
protocol(f, p)
	int f, p;
{
	char pibuf[1024], fibuf[1024], *pbp, *fbp;
	register pcc = 0, fcc = 0;
	int cc, stop = TIOCPKT_DOSTOP;

	/*
	 * Must ignore SIGTTOU, otherwise we'll stop
	 * when we try and set slave pty's window shape
	 * (our pgrp is that of the master pty).
	 */
	(void) signal(SIGTTOU, SIG_IGN);
	for (;;) {
		int ibits = 0, obits = 0;

		if (fcc)
			obits |= (1<<p);
		else
			ibits |= (1<<f);
		if (pcc >= 0)
			if (pcc)
				obits |= (1<<f);
			else
				ibits |= (1<<p);
		if (select(16, &ibits, &obits, 0, 0) < 0) {
			if (errno == EINTR)
				continue;
			fatalperror(f, "select", errno);
		}
		if (ibits == 0 && obits == 0) {
			/* shouldn't happen... */
			sleep(5);
			continue;
		}
		if (ibits & (1<<f)) {
			fcc = read(f, fibuf, sizeof (fibuf));
			if (fcc < 0 && errno == EWOULDBLOCK)
				fcc = 0;
			else {
				register char *cp;
				int left, n;

				if (fcc <= 0)
					break;
				fbp = fibuf;
			top:
				for (cp = fibuf; cp < fibuf+fcc; cp++)
					if (cp[0] == magic[0] &&
					    cp[1] == magic[1]) {
						left = fcc - (cp-fibuf);
						n = control(p, cp, left);
						if (n) {
							left -= n;
							if (left > 0)
								bcopy(cp, cp+n, left);
							fcc -= n;
							goto top; /* n^2 */
						}
					}
			}
		}
		if (ibits & (1<<p)) {
			pcc = read(p, pibuf, sizeof (pibuf));
			pbp = pibuf;
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else if (pcc <= 0)
				break;
			else if (pibuf[0] == 0)
				pbp++, pcc--;
			else {
#define	pkcontrol(c)	((c)&(TIOCPKT_FLUSHWRITE|TIOCPKT_NOSTOP|TIOCPKT_DOSTOP))
				if (pkcontrol(pibuf[0])) {
				/* The following 3 lines do nothing. */
					int nstop = pibuf[0] &
					    (TIOCPKT_NOSTOP|TIOCPKT_DOSTOP);

					if (nstop)
						stop = nstop;
					pibuf[0] |= nstop;
					send(f, &pibuf[0], 1, MSG_OOB);
				}
				pcc = 0;
			}
		}
		if ((obits & (1<<f)) && pcc > 0) {
			cc = write(f, pbp, pcc);
			if (cc < 0 && errno == EWOULDBLOCK) {
				/* also shouldn't happen */
				sleep(5);
				continue;
			}
			if (cc > 0) {
				pcc -= cc;
				pbp += cc;
			}
		}
		if ((obits & (1<<p)) && fcc > 0) {
			cc = write(p, fbp, fcc);
			if (cc > 0) {
				fcc -= cc;
				fbp += cc;
			}
		}
	}
}

cleanup()
{

	rmut();
	vhangup();		/* XXX */
	shutdown(netf, 2);
	kill(0, SIGKILL);
	exit(1);
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	buf[0] = '\01';		/* error indicator */
	(void) sprintf(buf + 1, "rlogind: %s.\r\n", msg);
	(void) write(f, buf, strlen(buf));
	exit(1);
}

fatalperror(f, msg, errno)
	int f;
	char *msg;
	int errno;
{
	char buf[BUFSIZ];
	extern int sys_nerr;
	extern char *sys_errlist[];

	if ((unsigned)errno < sys_nerr)
		(void) sprintf(buf, "%s: %s", msg, sys_errlist[errno]);
	else
		(void) sprintf(buf, "%s: Error %d", msg, errno);
	fatal(f, buf);
}

#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmpf[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

rmut()
{
	register f;
	int found = 0;
	struct utmp *u, *utmp;
	int nutmp;
	struct stat statbf;

	f = open(utmpf, O_RDWR);
	if (f >= 0) {
		fstat(f, &statbf);
		utmp = (struct utmp *)malloc(statbf.st_size);
		if (!utmp)
			syslog(LOG_ERR, "utmp malloc failed");
		if (statbf.st_size && utmp) {
			nutmp = read(f, utmp, statbf.st_size);
			nutmp /= sizeof(struct utmp);
		
			for (u = utmp ; u < &utmp[nutmp] ; u++) {
				if (SCMPN(u->ut_line, line+5) ||
				    u->ut_name[0]==0)
					continue;
				lseek(f, ((long)u)-((long)utmp), L_SET);
				SCPYN(u->ut_name, "");
				SCPYN(u->ut_host, "");
				time(&u->ut_time);
				write(f, (char *)u, sizeof(wtmp));
				found++;
			}
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, O_WRONLY|O_APPEND);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}
