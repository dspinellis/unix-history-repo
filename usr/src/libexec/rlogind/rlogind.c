/*
 * Copyright (c) 1983, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rlogind.c	5.21 (Berkeley) %G%";
#endif /* not lint */

/*
 * remote login server:
 *	\0
 *	remuser\0
 *	locuser\0
 *	terminal_type/speed\0
 *	data (not used currently)
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
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <stdio.h>
#include <netdb.h>
#include <syslog.h>
#include <strings.h>
#include <utmp.h>

#ifdef	KERBEROS
#include <sys/param.h>
#include <kerberos/krb.h>
#define	SECURE_MESSAGE "This rlogin session is using DES encryption for all transmissions.\r\n"

AUTH_DAT	*kdata;
KTEXT		ticket;
u_char		auth_buf[sizeof(AUTH_DAT)];
u_char		tick_buf[sizeof(KTEXT_ST)];
Key_schedule	schedule;
int		encrypt = 0, retval;
int		do_krb_login();

#define		OLD_RCMD		0x00
#define		KERB_RCMD		0x01
#define		KERB_RCMD_MUTUAL	0x02
#endif	/* KERBEROS */

char	*envinit[2];
struct	utmp utmp;
#define	NMAX sizeof(utmp.ut_name)
char	lusername[NMAX+1], rusername[NMAX+1];
static	char term[64] = "TERM=";
#define	ENVSIZE	(sizeof("TERM=")-1)	/* skip null for concatenation */
int	keepalive = 1;

#define	SUPERUSER(pwd)	((pwd)->pw_uid == 0)

#ifndef TIOCPKT_WINDOW
#define TIOCPKT_WINDOW 0x80
#endif

extern	int errno;
int	reapchild();
struct	passwd *getpwnam(), *pwd;
char	*malloc();

main(argc, argv)
	int argc;
	char **argv;
{
	extern int opterr, optind, _check_rhosts_file;
	int ch;
	int on = 1, fromlen;
	struct sockaddr_in from;

	openlog("rlogind", LOG_PID | LOG_AUTH, LOG_AUTH);

	opterr = 0;
	while ((ch = getopt(argc, argv, "ln")) != EOF)
		switch (ch) {
		case 'l':
			_check_rhosts_file = 0;
			break;
		case 'n':
			keepalive = 0;
			break;
		case '?':
		default:
			syslog(LOG_ERR, "usage: rlogind [-l] [-n]");
			break;
		}
	argc -= optind;
	argv += optind;

	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		exit(1);
	}
	if (keepalive &&
	    setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0)
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	doit(0, &from);
}

int	child;
int	cleanup();
int	netf;
char	*line;
extern	char	*inet_ntoa();

struct winsize win = { 0, 0, 0, 0 };


doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	int i, p, t, pid, on = 1;
	int	authenticated = 0;
	register struct hostent *hp;
	struct hostent hostent;
	char c;

	alarm(60);
	read(f, &c, 1);

#ifdef	KERBEROS
	/*
	 * XXX 1st char tells us which client we're talking to
	 */
	switch (c) {

	case KERB_RCMD:
		break;

	case KERB_RCMD_MUTUAL:
		encrypt = 1;
		break;


	case OLD_RCMD:
	default:
		fatal(f, "Remote host requires Kerberos authentication");
	}
#else
	if (c != 0)
		exit(1);
#endif

	alarm(0);
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	hp = gethostbyaddr(&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp == 0) {
		/*
		 * Only the name is used below.
		 */
		hp = &hostent;
		hp->h_name = inet_ntoa(fromp->sin_addr);
	}

#ifdef	KERBEROS
	retval = do_krb_login(hp->h_name, fromp, encrypt);
	write(f, &c, 1);
	if (retval == 0)
		authenticated++;
	else
		if (retval > 0)
			fatal(f, krb_err_txt[retval]);
#else
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED ||
	    fromp->sin_port < IPPORT_RESERVED/2)
		fatal(f, "Permission denied");
	write(f, "", 1);
	if (do_rlogin(hp->h_name) == 0)
		authenticated++;
#endif

	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;
		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[sizeof("/dev/ptyp") - 1] = "0123456789abcdef"[i];
			p = open(line, O_RDWR);
			if (p > 0)
				goto gotpty;
		}
	}
	fatal(f, "Out of ptys");
	/*NOTREACHED*/
gotpty:
	(void) ioctl(p, TIOCSWINSZ, &win);
	netf = f;
	line[strlen("/dev/")] = 't';
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
	if (fchmod(t, 0))
		fatalperror(f, line);
	(void)signal(SIGHUP, SIG_IGN);
	vhangup();
	(void)signal(SIGHUP, SIG_DFL);
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
	setup_term(t);
#ifdef DEBUG
	{
		int tt = open("/dev/tty", O_RDWR);
		if (tt > 0) {
			(void)ioctl(tt, TIOCNOTTY, 0);
			(void)close(tt);
		}
	}
#endif
	pid = fork();
	if (pid < 0)
		fatalperror(f, "");
	if (pid == 0) {
		if (setsid() < 0)
			fatalperror(f, "setsid");
		if (ioctl(t, TIOCSCTTY, 0) < 0)
			fatalperror(f, "ioctl(sctty)");
		close(f), close(p);
		dup2(t, 0), dup2(t, 1), dup2(t, 2);
		close(t);
		if (authenticated)
			execl("/bin/login", "login", "-p", "-f",
			    "-h", hp->h_name, pwd->pw_name, 0);
		else
			execl("/bin/login", "login", "-p", "-h", hp->h_name,
			    pwd->pw_name, 0);
		fatalperror(2, "/bin/login");
		/*NOTREACHED*/
	}
	close(t);

#ifdef	KERBEROS
	/*
	 * If encrypted, don't turn on NBIO or the des read/write
	 * routines will croak.
	 */

	if (encrypt)
		(void) des_write(f, SECURE_MESSAGE, sizeof(SECURE_MESSAGE));
	else
#endif
		ioctl(f, FIONBIO, &on);

	ioctl(p, FIONBIO, &on);
	ioctl(p, TIOCPKT, &on);
	signal(SIGTSTP, SIG_IGN);
	signal(SIGCHLD, cleanup);
	setpgrp(0, 0);
	protocol(f, p);
	signal(SIGCHLD, SIG_IGN);
	cleanup();
}

char	magic[2] = { 0377, 0377 };
char	oobdata[] = {TIOCPKT_WINDOW};

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
	struct winsize w;

	if (n < 4+sizeof (w) || cp[2] != 's' || cp[3] != 's')
		return (0);
	oobdata[0] &= ~TIOCPKT_WINDOW;	/* we know he heard */
	bcopy(cp+4, (char *)&w, sizeof(w));
	w.ws_row = ntohs(w.ws_row);
	w.ws_col = ntohs(w.ws_col);
	w.ws_xpixel = ntohs(w.ws_xpixel);
	w.ws_ypixel = ntohs(w.ws_ypixel);
	(void)ioctl(pty, TIOCSWINSZ, &w);
	return (4+sizeof (w));
}

/*
 * rlogin "protocol" machine.
 */
protocol(f, p)
	int f, p;
{
	char pibuf[1024], fibuf[1024], *pbp, *fbp;
	register pcc = 0, fcc = 0;
	int cc, nfd, pmask, fmask;
	char cntl;

	/*
	 * Must ignore SIGTTOU, otherwise we'll stop
	 * when we try and set slave pty's window shape
	 * (our controlling tty is the master pty).
	 */
	(void) signal(SIGTTOU, SIG_IGN);
	send(f, oobdata, 1, MSG_OOB);	/* indicate new rlogin */
	if (f > p)
		nfd = f + 1;
	else
		nfd = p + 1;
	fmask = 1 << f;
	pmask = 1 << p;
	for (;;) {
		int ibits, obits, ebits;

		ibits = 0;
		obits = 0;
		if (fcc)
			obits |= pmask;
		else
			ibits |= fmask;
		if (pcc >= 0)
			if (pcc)
				obits |= fmask;
			else
				ibits |= pmask;
		ebits = pmask;
		if (select(nfd, &ibits, obits ? &obits : (int *)NULL,
		    &ebits, 0) < 0) {
			if (errno == EINTR)
				continue;
			fatalperror(f, "select");
		}
		if (ibits == 0 && obits == 0 && ebits == 0) {
			/* shouldn't happen... */
			sleep(5);
			continue;
		}
#define	pkcontrol(c)	((c)&(TIOCPKT_FLUSHWRITE|TIOCPKT_NOSTOP|TIOCPKT_DOSTOP))
		if (ebits & pmask) {
			cc = read(p, &cntl, 1);
			if (cc == 1 && pkcontrol(cntl)) {
				cntl |= oobdata[0];
				send(f, &cntl, 1, MSG_OOB);
				if (cntl & TIOCPKT_FLUSHWRITE) {
					pcc = 0;
					ibits &= ~pmask;
				}
			}
		}
		if (ibits & fmask) {
#ifdef	KERBEROS
			if (encrypt)
				fcc = des_read(f, fibuf, sizeof(fibuf));
			else
#endif
				fcc = read(f, fibuf, sizeof(fibuf));
			if (fcc < 0 && errno == EWOULDBLOCK)
				fcc = 0;
			else {
				register char *cp;
				int left, n;

				if (fcc <= 0)
					break;
				fbp = fibuf;

			top:
				for (cp = fibuf; cp < fibuf+fcc-1; cp++)
					if (cp[0] == magic[0] &&
					    cp[1] == magic[1]) {
						left = fcc - (cp-fibuf);
						n = control(p, cp, left);
						if (n) {
							left -= n;
							if (left > 0)
								bcopy(cp+n, cp, left);
							fcc -= n;
							goto top; /* n^2 */
						}
					}
				obits |= pmask;		/* try write */
			}
		}

		if ((obits & pmask) && fcc > 0) {
			cc = write(p, fbp, fcc);
			if (cc > 0) {
				fcc -= cc;
				fbp += cc;
			}
		}

		if (ibits & pmask) {
			pcc = read(p, pibuf, sizeof (pibuf));
			pbp = pibuf;
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else if (pcc <= 0)
				break;
			else if (pibuf[0] == 0) {
				pbp++, pcc--;
#ifdef	KERBEROS
				if (!encrypt)
#endif
					obits |= fmask;	/* try a write */
			} else {
				if (pkcontrol(pibuf[0])) {
					pibuf[0] |= oobdata[0];
					send(f, &pibuf[0], 1, MSG_OOB);
				}
				pcc = 0;
			}
		}
		if ((obits & fmask) && pcc > 0) {
#ifdef	KERBEROS
			if (encrypt)
				cc = des_write(f, pbp, pcc);
			else
#endif
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
	}
}

cleanup()
{
	char *p;

	p = line + sizeof("/dev/") - 1;
	if (logout(p))
		logwtmp(p, "", "");
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	*p = 'p';
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	shutdown(netf, 2);
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

fatalperror(f, msg)
	int f;
	char *msg;
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


int
do_rlogin(host)
	char	*host;
{
	getstr(rusername, sizeof(rusername), "remuser");
	getstr(lusername, sizeof(lusername), "locuser");
	getstr(term+ENVSIZE, sizeof(term)-ENVSIZE, "Terminal type");

	if(getuid()) {
		pwd = &nouser;
		return(-1);
	}
	pwd = getpwnam(lusername);
	if(pwd == NULL) {
		pwd = &nouser;
		pwd->pw_name = lusername;	/* pass on to login */
		return(-1);
	}
	return(ruserok(host, SUPERUSER(pwd), rusername, lusername));
}


getstr(buf, cnt, err)
	char	*buf;
	int	cnt;
	char	*err;
{
	char	c;
	do {
		if(read(0, &c, 1) != 1)
			exit(1);
		if(--cnt < 0) {
			printf("%s too long\r\n", err);
			exit(1);
		}
		*buf++ = c;
	} while(c != 0);
}

extern	char	**environ;

setup_term(fd)
	int	fd;
{
	struct	termios	tt;
	struct	sgttyb	tp;
	register char	*cp = index(term+ENVSIZE, '/'), **cpp;
	char		*speed;

	tcgetattr(fd, &tt);
	if(cp) {
		*cp++ = '\0';
		speed = cp;
		cp = index(speed, '/');
		if(cp)
			*cp++ = '\0';
		cfsetspeed(&tt, atoi(speed));
	}
	tt.c_iflag = BRKINT|ICRNL|IXON|ISTRIP|IEXTEN|IMAXBEL;
	tt.c_oflag = OPOST|ONLCR|OXTABS;
	tt.c_lflag = ISIG|ICANON|ECHO;
	tcsetattr(fd, TCSADFLUSH, &tt);

	envinit[0] = term;
	envinit[1] = 0;
	environ = envinit;
}

#ifdef	KERBEROS
#define	VERSION_SIZE	9

/*
 * Do the remote kerberos login to the named host with the
 * given inet address
 *
 * Return 0 on valid authorization
 * Return -1 on valid authentication, no authorization
 * Return >0 for error conditions
 */

int
do_krb_login(host, dest, encrypt)
	char			*host;
	struct	sockaddr_in	*dest;
	int			encrypt;
{
	int	rc;
	char	instance[INST_SZ], version[VERSION_SIZE];
	long	authopts = 0L;	/* !mutual */
	struct	sockaddr_in	faddr;

	if(getuid()) {
		pwd = &nouser;
		return(KFAILURE);
	}

	kdata = (AUTH_DAT *) auth_buf;
	ticket = (KTEXT) tick_buf;
	strcpy(instance, "*");

	if(encrypt) {
		rc = sizeof(faddr);
		if(getsockname(0, &faddr, &rc)) {
			pwd = &nouser;
			return(-1);
		}
		authopts = KOPT_DO_MUTUAL;
		rc = krb_recvauth(
			authopts, 0,
			ticket, "rcmd",
			instance, dest, &faddr,
			kdata, "", schedule, version);
		 des_set_key(kdata->session, schedule);

	} else {
		rc = krb_recvauth(
			authopts, 0,
			ticket, "rcmd",
			instance, dest, (struct sockaddr_in *) 0,
			kdata, "", (bit_64 *) 0, version);
	}

	if(rc != KSUCCESS) {
		pwd = &nouser;
		return(rc);
	}

	if((rc = krb_kntoln(kdata, rusername)) != KSUCCESS) {
		pwd = &nouser;
		return(rc);
	}

	getstr(lusername, sizeof(lusername), "locuser");
	/* get the "cmd" in the rcmd protocol */
	getstr(term+ENVSIZE, sizeof(term)-ENVSIZE, "Terminal type");

	pwd = getpwnam(lusername);
	if(pwd == NULL) {
		pwd = &nouser;
		pwd->pw_name = lusername;
		return(-1);
	}

	/* XXX need to use something other than ruserok */
	/* returns -1 for invalid authentication */
	return(ruserok(host, SUPERUSER(pwd), rusername, lusername));
}
#endif
