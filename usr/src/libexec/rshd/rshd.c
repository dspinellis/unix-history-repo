/*
 * Copyright (c) 1983, 1988, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988, 1089 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rshd.c	5.36.1.1 (Berkeley) %G%";
#endif /* not lint */

/* From:
 *	$Source: /mit/kerberos/ucb/mit/rshd/RCS/rshd.c,v $
 *	$Header: /mit/kerberos/ucb/mit/rshd/RCS/rshd.c,v 5.2 89/07/31 19:30:04 kfall Exp $
 */


/*
 * remote shell server:
 *	[port]\0
 *	remuser\0
 *	locuser\0
 *	command\0
 *	data
 */
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/signal.h>
#include <sys/time.h>

#include <netinet/in.h>

#include <arpa/inet.h>

#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <netdb.h>
#include <syslog.h>
#include "pathnames.h"
#include "pathnames.h"

int	errno;
int	keepalive = 1;
int	check_all = 0;
int	check_all = 0;
char	*index(), *rindex(), *strncat();
/*VARARGS1*/
int	error();
int	sent_null;
int	sent_null;

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	extern int opterr, optind;
	extern int _check_rhosts_file;
	struct linger linger;
	int ch, on = 1, fromlen;
	struct sockaddr_in from;

	openlog("rshd", LOG_PID | LOG_ODELAY, LOG_DAEMON);

	opterr = 0;
		switch (ch) {
		case 'a':
			check_all = 1;
			break;
		case 'l':
			_check_rhosts_file = 0;
			break;
		case 'n':
			keepalive = 0;
			break;
		case '?':
		default:
			usage();
			exit(2);
		}

	argc -= optind;
	argv += optind;

	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		syslog(LOG_ERR, "getpeername: %m");
		_exit(1);
	}
	if (keepalive &&
	    setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, (char *)&on,
	    sizeof(on)) < 0)
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	linger.l_onoff = 1;
	linger.l_linger = 60;			/* XXX */
	if (setsockopt(0, SOL_SOCKET, SO_LINGER, (char *)&linger,
	    sizeof (linger)) < 0)
		syslog(LOG_WARNING, "setsockopt (SO_LINGER): %m");
	doit(&from);
}

char	username[20] = "USER=";
char	homedir[64] = "HOME=";
char	shell[64] = "SHELL=";
char	*envinit[] =
	    {homedir, shell, _PATH_DEFPATH, username, 0};
char	**environ;

doit(fromp)
	struct sockaddr_in *fromp;
{
	char cmdbuf[NCARGS+1], *cp;
	char locuser[16], remuser[16];
	struct passwd *pwd;
	int s;
	struct hostent *hp;
	char *hostname, *errorstr = NULL, *errorhost;
	u_short port;
	int pv[2], pid, cc;
	int nfd;
	fd_set ready, readfrom;
	char buf[BUFSIZ], sig;
	int one = 1;
	char remotehost[2 * MAXHOSTNAMELEN + 1];

	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
	(void) signal(SIGTERM, SIG_DFL);
#ifdef DEBUG
	{ int t = open(_PATH_TTY, 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	if (fromp->sin_family != AF_INET) {
		syslog(LOG_ERR, "malformed \"from\" address (af %d)\n",
		    fromp->sin_family);
		exit(1);
	}
#ifdef IP_OPTIONS
      {
	u_char optbuf[BUFSIZ/3], *cp;
	char lbuf[BUFSIZ], *lp;
	int optsize = sizeof(optbuf), ipproto;
	struct protoent *ip;
#ifdef IP_OPTIONS
      {
	u_char optbuf[BUFSIZ/3], *cp;
	char lbuf[BUFSIZ], *lp;
	int optsize = sizeof(optbuf), ipproto;
	struct protoent *ip;

	if ((ip = getprotobyname("ip")) != NULL)
		ipproto = ip->p_proto;
	else
		ipproto = IPPROTO_IP;
	if (getsockopt(0, ipproto, IP_OPTIONS, (char *)optbuf, &optsize) == 0 &&
	    optsize != 0) {
		lp = lbuf;
		for (cp = optbuf; optsize > 0; cp++, optsize--, lp += 3)
			sprintf(lp, " %2.2x", *cp);
		syslog(LOG_NOTICE,
		    "Connection received using IP options (ignored):%s", lbuf);
		if (setsockopt(0, ipproto, IP_OPTIONS,
		    (char *)NULL, &optsize) != 0) {
			syslog(LOG_ERR, "setsockopt IP_OPTIONS NULL: %m");
			exit(1);
		}
	}
      }
#endif

	if ((ip = getprotobyname("ip")) != NULL)
		ipproto = ip->p_proto;
	else
		ipproto = IPPROTO_IP;
	if (!getsockopt(0, ipproto, IP_OPTIONS, (char *)optbuf, &optsize) &&
	    optsize != 0) {
		lp = lbuf;
		for (cp = optbuf; optsize > 0; cp++, optsize--, lp += 3)
			sprintf(lp, " %2.2x", *cp);
		syslog(LOG_NOTICE,
		    "Connection received from %s using IP options (ignored):%s",
		    inet_ntoa(fromp->sin_addr), lbuf);
		if (setsockopt(0, ipproto, IP_OPTIONS,
		    (char *)NULL, &optsize) != 0) {
			syslog(LOG_ERR, "setsockopt IP_OPTIONS NULL: %m");
			exit(1);
		}
	}
      }
#endif

#ifdef	KERBEROS
	if (!use_kerberos)

	(void) alarm(60);
	port = 0;
	for (;;) {
		char c;
		if ((cc = read(0, &c, 1)) != 1) {
			if (cc < 0)
				syslog(LOG_NOTICE, "read: %m");
			shutdown(0, 1+1);
			exit(1);
		}
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
#ifdef	KERBEROS
		if (!use_kerberos)
		fromp->sin_port = htons(port);
		if (connect(s, fromp, sizeof (*fromp)) < 0) {
			syslog(LOG_INFO, "connect second port: %m");
			exit(1);
		}
	}

#ifdef notdef
	/* from inetd, socket is already on 0, 1, 2 */
	dup2(f, 0);
	dup2(f, 1);
	dup2(f, 2);
#endif
	hp = gethostbyaddr((char *)&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp) {
		/*
		 * If name returned by gethostbyaddr is in our domain,
		 * attempt to verify that we haven't been fooled by someone
		 * in a remote net; look up the name and check that this
		 * address corresponds to the name.
		 */
		hostname = hp->h_name;
#ifdef	KERBEROS
		if (!use_kerberos)
#endif
		if (check_all || local_domain(hp->h_name)) {
			strncpy(remotehost, hp->h_name, sizeof(remotehost) - 1);
			remotehost[sizeof(remotehost) - 1] = 0;
			errorhost = remotehost;
			hp = gethostbyname(remotehost);
			if (hp == NULL) {
				syslog(LOG_INFO,
				    "Couldn't look up address for %s",
				    remotehost);
				errorstr =
				"Couldn't look up address for your host (%s)\n";
				hostname = inet_ntoa(fromp->sin_addr);
			}
#ifdef h_addr	/* 4.2 hack */
			for (; ; hp->h_addr_list++) {
				if (hp->h_addr_list[0] == NULL) {
					syslog(LOG_NOTICE,
					  "Host addr %s not listed for host %s",
					    inet_ntoa(fromp->sin_addr),
					    hp->h_name);
					errorstr =
					    "Host address mismatch for %s\n";
					hostname = inet_ntoa(fromp->sin_addr);
					break;
				}
				if (!bcmp(hp->h_addr_list[0],
				    (caddr_t)&fromp->sin_addr,
				    sizeof(fromp->sin_addr))) {
					hostname = hp->h_name;
					break;
				}
			}
#else
			if (bcmp(hp->h_addr, (caddr_t)&fromp->sin_addr,
			    sizeof(fromp->sin_addr))) {
				syslog(LOG_NOTICE,
				  "Host addr %s not listed for host %s",
				    inet_ntoa(fromp->sin_addr),
				    hp->h_name);
				error("Host address mismatch\n");
				exit(1);
			}
#endif
		}
	} else
		errorhost = hostname = inet_ntoa(fromp->sin_addr);

	getstr(remuser, sizeof(remuser), "remuser");
	getstr(locuser, sizeof(locuser), "locuser");
	getstr(cmdbuf, sizeof(cmdbuf), "command");
	setpwent();
	pwd = getpwnam(locuser);
	if (pwd == NULL) {
		if (errorstr == NULL)
			errorstr = "Login incorrect.\n";
		goto fail;
	}
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

	if (pwd->pw_uid && !access(_PATH_NOLOGIN, F_OK)) {
		error("Logins currently disabled.\n");
		exit(1);
	}

	(void) write(2, "\0", 1);
	sent_null = 1;
	sent_null = 1;

	if (port) {
		if (pipe(pv) < 0) {
			error("Can't make pipe.\n");
			exit(1);
		}
		pid = fork();
		if (pid == -1)  {
			error("Can't fork; try again.\n");
			exit(1);
		}
		if (pid) {
			{
				(void) close(0); (void) close(1);
			}
			(void) close(2); (void) close(pv[1]);

			FD_ZERO(&readfrom);
			FD_SET(s, &readfrom);
			FD_SET(pv[0], &readfrom);
			if (pv[0] > s)
				nfd = pv[0];
			else
				nfd = s;
				ioctl(pv[0], FIONBIO, (char *)&one);

			/* should set s nbio! */
			nfd++;
			do {
				ready = readfrom;
					if (select(nfd, &ready, (fd_set *)0,
					  (fd_set *)0, (struct timeval *)0) < 0)
						break;
				if (FD_ISSET(s, &ready)) {
					int	ret;
						ret = read(s, &sig, 1);
					if (ret <= 0)
						FD_CLR(s, &readfrom);
					else
						killpg(pid, sig);
				}
				if (FD_ISSET(pv[0], &ready)) {
					errno = 0;
					cc = read(pv[0], buf, sizeof(buf));
					if (cc <= 0) {
						shutdown(s, 1+1);
						FD_CLR(pv[0], &readfrom);
					} else {
							(void)
							  write(s, buf, cc);
					}
				}

			} while (FD_ISSET(s, &readfrom) ||
			    FD_ISSET(pv[0], &readfrom));
			exit(0);
		}
		setpgrp(0, getpid());
		(void) close(s); (void) close(pv[0]);
		dup2(pv[1], 2);
		close(pv[1]);
		close(pv[1]);
	}
	if (*pwd->pw_shell == '\0')
		pwd->pw_shell = _PATH_BSHELL;
#if	BSD > 43
	if (setlogin(pwd->pw_name) < 0)
		syslog(LOG_ERR, "setlogin() failed: %m");
#endif
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
	endpwent();
	if (pwd->pw_uid == 0)
		syslog(LOG_INFO|LOG_AUTH, "ROOT shell from %s@%s, comm: %s\n",
		    remuser, hostname, cmdbuf);
	endpwent();
	if (pwd->pw_uid == 0) {
#ifdef	KERBEROS
		if (use_kerberos)
			syslog(LOG_INFO|LOG_AUTH,
				"ROOT Kerberos shell from %s.%s@%s on %s, comm: %s\n",
				kdata->pname, kdata->pinst, kdata->prealm,
				hostname, cmdbuf);
		else
#endif
			syslog(LOG_INFO|LOG_AUTH,
				"ROOT shell from %s@%s, comm: %s\n",
				remuser, hostname, cmdbuf);
	}
	execl(pwd->pw_shell, cp, "-c", cmdbuf, 0);
	perror(pwd->pw_shell);
	exit(1);
}

/*
 * Report error to client.
 * Note: can't be used until second socket has connected
 * to client, or older clients will hang waiting
 * for that connection first.
 */
/*
 * Report error to client.
 * Note: can't be used until second socket has connected
 * to client, or older clients will hang waiting
 * for that connection first.
 */
/*VARARGS1*/
error(fmt, a1, a2, a3)
	char *fmt;
	int a1, a2, a3;
{
	char buf[BUFSIZ], *bp = buf;

	if (sent_null == 0)
		*bp++ = 1;
	(void) sprintf(bp, fmt, a1, a2, a3);
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

/*
 * Check whether host h is in our local domain,
 * defined as sharing the last two components of the domain part,
 * or the entire domain part if the local domain has only one component.
 * If either name is unqualified (contains no '.'),
 * assume that the host is local, as it will be
 * interpreted as such.
 */
local_domain(h)
	char *h;
{
	char localhost[MAXHOSTNAMELEN];
	char *p1, *p2, *topdomain();

	localhost[0] = 0;
	localhost[0] = 0;
	(void) gethostname(localhost, sizeof(localhost));
	p1 = topdomain(localhost);
	p2 = topdomain(h);
	if (p1 == NULL || p2 == NULL || !strcasecmp(p1, p2))
		return(1);
	return(0);
}

char *
topdomain(h)
	char *h;
{
	register char *p;
	char *maybe = NULL;
	int dots = 0;

	for (p = h + strlen(h); p >= h; p--) {
		if (*p == '.') {
			if (++dots == 2)
				return (p);
			maybe = p;
		}
	}
	return (maybe);
}

char *
topdomain(h)
	char *h;
{
	register char *p;
	char *maybe = NULL;
	int dots = 0;

	for (p = h + strlen(h); p >= h; p--) {
		if (*p == '.') {
			if (++dots == 2)
				return (p);
			maybe = p;
		}
	}
	return (maybe);
}

usage()
{
	syslog(LOG_ERR, "usage: rshd [-%s]", OPTIONS);
}
