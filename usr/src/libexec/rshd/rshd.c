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
static char sccsid[] = "@(#)rshd.c	5.11 (Berkeley) %G%";
#endif not lint

/*
 * remote shell server:
 *	remuser\0
 *	locuser\0
 *	command\0
 *	data
 */
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/time.h>

#include <netinet/in.h>

#include <arpa/inet.h>

#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <netdb.h>
#include <syslog.h>

int	errno;
char	*index(), *rindex(), *strncat();
/*VARARGS1*/
int	error();

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	struct linger linger;
	int on = 1, fromlen;
	struct sockaddr_in from;

	openlog("rsh", LOG_PID | LOG_ODELAY, LOG_DAEMON);
	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, (char *)&on,
	    sizeof (on)) < 0)
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	linger.l_onoff = 1;
	linger.l_linger = 60;			/* XXX */
	if (setsockopt(0, SOL_SOCKET, SO_LINGER, (char *)&linger,
	    sizeof (linger)) < 0)
		syslog(LOG_WARNING, "setsockopt (SO_LINGER): %m");
	doit(dup(0), &from);
}

char	username[20] = "USER=";
char	homedir[64] = "HOME=";
char	shell[64] = "SHELL=";
char	*envinit[] =
	    {homedir, shell, "PATH=:/usr/ucb:/bin:/usr/bin", username, 0};
char	**environ;

doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	char cmdbuf[NCARGS+1], *cp;
	char locuser[16], remuser[16];
	struct passwd *pwd;
	int s;
	struct hostent *hp;
	char *hostname;
	short port;
	int pv[2], pid, ready, readfrom, cc;
	char buf[BUFSIZ], sig;
	int one = 1;

	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
	(void) signal(SIGTERM, SIG_DFL);
#ifdef DEBUG
	{ int t = open("/dev/tty", 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	if (fromp->sin_family != AF_INET) {
		syslog(LOG_ERR, "malformed from address\n");
		exit(1);
	}
	if (fromp->sin_port >= IPPORT_RESERVED ||
	    fromp->sin_port < IPPORT_RESERVED/2) {
		syslog(LOG_NOTICE, "connection from bad port\n");
		exit(1);
	}
	(void) alarm(60);
	port = 0;
	for (;;) {
		char c;
		if ((cc = read(f, &c, 1)) != 1) {
			if (cc < 0)
				syslog(LOG_NOTICE, "read: %m");
			shutdown(f, 1+1);
			exit(1);
		}
		if (c == 0)
			break;
		port = port * 10 + c - '0';
	}
	(void) alarm(0);
	if (port != 0) {
		int lport = IPPORT_RESERVED - 1;
		s = rresvport(&lport);
		if (s < 0) {
			syslog(LOG_ERR, "can't get stderr port: %m");
			exit(1);
		}
		if (port >= IPPORT_RESERVED) {
			syslog(LOG_ERR, "2nd port not reserved\n");
			exit(1);
		}
		fromp->sin_port = htons((u_short)port);
		if (connect(s, fromp, sizeof (*fromp)) < 0) {
			syslog(LOG_INFO, "connect second port: %m");
			exit(1);
		}
	}
	dup2(f, 0);
	dup2(f, 1);
	dup2(f, 2);
	hp = gethostbyaddr((char *)&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp)
		hostname = hp->h_name;
	else
		hostname = inet_ntoa(fromp->sin_addr);
	getstr(remuser, sizeof(remuser), "remuser");
	getstr(locuser, sizeof(locuser), "locuser");
	getstr(cmdbuf, sizeof(cmdbuf), "command");
	setpwent();
	pwd = getpwnam(locuser);
	if (pwd == NULL) {
		error("Login incorrect.\n");
		exit(1);
	}
	endpwent();
	if (chdir(pwd->pw_dir) < 0) {
		(void) chdir("/");
#ifdef notdef
		error("No remote directory.\n");
		exit(1);
#endif
	}
	if (pwd->pw_passwd != 0 && *pwd->pw_passwd != '\0' &&
	    ruserok(hostname, pwd->pw_uid == 0, remuser, locuser) < 0) {
		error("Permission denied.\n");
		exit(1);
	}
	if (pwd->pw_uid && !access("/etc/nologin", F_OK)) {
		error("Logins currently disabled.\n");
		exit(1);
	}
	(void) write(2, "\0", 1);
	if (port) {
		if (pipe(pv) < 0) {
			error("Can't make pipe.\n");
			exit(1);
		}
		pid = fork();
		if (pid == -1)  {
			error("Try again.\n");
			exit(1);
		}
		if (pid) {
			(void) close(0); (void) close(1); (void) close(2);
			(void) close(f); (void) close(pv[1]);
			readfrom = (1<<s) | (1<<pv[0]);
			ioctl(pv[0], FIONBIO, (char *)&one);
			/* should set s nbio! */
			do {
				ready = readfrom;
				if (select(16, &ready, (fd_set *)0,
				    (fd_set *)0, (struct timeval *)0) < 0)
					break;
				if (ready & (1<<s)) {
					if (read(s, &sig, 1) <= 0)
						readfrom &= ~(1<<s);
					else
						killpg(pid, sig);
				}
				if (ready & (1<<pv[0])) {
					errno = 0;
					cc = read(pv[0], buf, sizeof (buf));
					if (cc <= 0) {
						shutdown(s, 1+1);
						readfrom &= ~(1<<pv[0]);
					} else
						(void) write(s, buf, cc);
				}
			} while (readfrom);
			exit(0);
		}
		setpgrp(0, getpid());
		(void) close(s); (void) close(pv[0]);
		dup2(pv[1], 2);
	}
	if (*pwd->pw_shell == '\0')
		pwd->pw_shell = "/bin/sh";
	(void) close(f);
	(void) setgid((gid_t)pwd->pw_gid);
	initgroups(pwd->pw_name, pwd->pw_gid);
	(void) setuid((uid_t)pwd->pw_uid);
	environ = envinit;
	strncat(homedir, pwd->pw_dir, sizeof(homedir)-6);
	strncat(shell, pwd->pw_shell, sizeof(shell)-7);
	strncat(username, pwd->pw_name, sizeof(username)-6);
	cp = rindex(pwd->pw_shell, '/');
	if (cp)
		cp++;
	else
		cp = pwd->pw_shell;
	execl(pwd->pw_shell, cp, "-c", cmdbuf, 0);
	perror(pwd->pw_shell);
	exit(1);
}

/*VARARGS1*/
error(fmt, a1, a2, a3)
	char *fmt;
	int a1, a2, a3;
{
	char buf[BUFSIZ];

	buf[0] = 1;
	(void) sprintf(buf+1, fmt, a1, a2, a3);
	(void) write(2, buf, strlen(buf));
}

getstr(buf, cnt, err)
	char *buf;
	int cnt;
	char *err;
{
	char c;

	do {
		if (read(0, &c, 1) != 1)
			exit(1);
		*buf++ = c;
		if (--cnt == 0) {
			error("%s too long\n", err);
			exit(1);
		}
	} while (c != 0);
}
