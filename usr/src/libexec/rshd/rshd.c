/*
 * Copyright (c) 1983 The Regents of the University of California.
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
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rshd.c	5.14 (Berkeley) 1/23/89";
#endif /* not lint */

/*
 * remote shell server:
 *	\0
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
int	keepalive = 1;
char	*index(), *rindex(), *strncat();
/*VARARGS1*/
int	error();

#ifdef	KERBEROS
#include <kerberos/krb.h>
#define	VERSION_SIZE	9
#define	OPTIONS		"lnkv"
char	*strsave();
char	authbuf[sizeof(AUTH_DAT)];
char	tickbuf[sizeof(KTEXT_ST)];
int	use_kerberos = 0;

#define	OLD_RCMD		0x00
#define	KERB_RCMD		0x00
#define	KERB_RCMD_MUTUAL	0x03

int	encrypt = 0;
#else
#define	OPTIONS	"ln"
#endif

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	extern int opterr, optind, _check_rhosts_file;
	struct linger linger;
	int ch, on = 1, fromlen;
	struct sockaddr_in from;

	openlog("rsh", LOG_PID | LOG_ODELAY, LOG_DAEMON);

	opterr = 0;
	while ((ch = getopt(argc, argv, OPTIONS)) != EOF)
		switch((char)ch) {
		case 'l':
			_check_rhosts_file = 0;
			break;
		case 'n':
			keepalive = 0;
			break;
#ifdef	KERBEROS
		case 'k':
			use_kerberos = 1;
			break;

		case 'v':
			vacuous = 1;
			break;
#endif
		case '?':
		default:
			syslog(LOG_ERR, "usage: rshd [-l]");
			break;
		}

	argc -= optind;
	argv += optind;

#ifdef	KERBEROS
	if(use_kerberos && vacuous) {
		syslog(LOG_ERR, "only one of -k and -v allowed");
		exit(1);
	}
#endif

	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
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
	int pv[2], pid, cc;
	long ready, readfrom;
	char buf[BUFSIZ], sig;
	int one = 1;
	char	localhost[MAXHOSTNAMELEN], remotehost[MAXHOSTNAMELEN];
	char	*raddr;

#ifdef	KERBEROS
	AUTH_DAT	*kdata = (AUTH_DAT *) NULL;
	KTEXT		ticket = (KTEXT) NULL;
	char		instance[INST_SZ], version[VERSION_SIZE];
	char		*h_name;
	struct		sockaddr_in	fromaddr = *fromp;
	int		rc;
	long		authopts;
#endif

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

#ifndef	KERBEROS
	if (fromp->sin_port >= IPPORT_RESERVED ||
	    fromp->sin_port < IPPORT_RESERVED/2) {
		syslog(LOG_NOTICE, "Connection from %s on illegal port",
			inet_ntoa(fromp->sin_addr));
		exit(1);
	}
#endif

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
#ifdef	KERBEROS
		if(c == OLD_RCMD || c == KERB_RCMD)
			break;

		if(c == KERB_RCMD_MUTUAL) {
			encrypt = 1;
			break;
		}
#else
		if (c == 0)
			break;
#endif
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
#ifndef	KERBEROS
		if (port >= IPPORT_RESERVED) {
			syslog(LOG_ERR, "2nd port not reserved\n");
			exit(1);
		}
#endif
		fromp->sin_port = htons((u_short)port);
		if (connect(s, fromp, sizeof (*fromp)) < 0) {
			syslog(LOG_INFO, "connect second port: %m");
			exit(1);
		}
	}

#ifdef	KERBEROS
	if(vacuous) {
		error("rshd: remote host requires Kerberos authentication\n");
		exit(1);
	}
#endif

	dup2(f, 0);
	dup2(f, 1);
	dup2(f, 2);
	hp = gethostbyaddr((char *)&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp) {
		(void) gethostname(localhost, sizeof(localhost));
		if(same_domain(hp->h_name, localhost)) {
			bcopy(hp->h_name, remotehost, sizeof(remotehost));
			hp = gethostbyname(remotehost);
			if(hp == NULL) {
				syslog(LOG_NOTICE, "Couldn't get host entry for %s",
					remotehost);
				error("Couldn't get host entry for your host");
				exit(1);
			} else {
			    for(;;) {
				if(!(raddr = hp->h_addr_list[0])) {
					syslog(LOG_NOTICE, "Host addr not listed for host %s",
						hp->h_name);
					error("Host address mismatch");
					exit(1);
				}
				if(!bcmp(raddr, (caddr_t)&fromp->sin_addr,
					sizeof(struct in_addr))) {
						break;
				}
				hp->h_addr_list++;
			    }
			}
		}
		hostname = hp->h_name;
	} else {
		hostname = inet_ntoa(fromp->sin_addr);
	}

#ifdef	KERBEROS
	if(use_kerberos) {
		h_name = strsave(hp->h_name);
		kdata = (AUTH_DAT *) authbuf;
		ticket = (KTEXT) tickbuf;
		authopts = 0L;
		strcpy(instance, "*");
		version[VERSION_SIZE - 1] = '\0';
		if(rc = krb_recvauth(
			authopts, f, ticket, "rcmd",
			instance, &fromaddr,
			(struct sockaddr_in *) 0,
			kdata, "", (bit_64 *) 0, version)
		) {
			fprintf(stderr,
				"Kerberos authentication failure: %s\r\n",
				  krb_err_txt[rc]);
			exit(1);
		}
		free(h_name);
		h_name = NULL;
	} else
#endif
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

#ifdef	KERBEROS
	if(use_kerberos) {
		if (pwd->pw_passwd != 0 && *pwd->pw_passwd != '\0') {
			if(krb_kntoln(kdata, remuser) != KSUCCESS) {
				error("Permission denied.\n");
				exit(1);
			}
			if(kuserok(kdata, locuser) != 0) {
				syslog(LOG_NOTICE, "Kerberos rlogin denied to %s.%s@%s",
					kdata->pname, kdata->pinst, kdata->prealm);
				error("Permission denied.\n");
				exit(1);
			}
	} else
#endif

		if (pwd->pw_passwd != 0 && *pwd->pw_passwd != '\0' &&
		    ruserok(hostname, pwd->pw_uid == 0, remuser, locuser) < 0) {
			error("Permission denied.\n");
			exit(1);
		}

	if (pwd->pw_uid && !access("/etc/nologin", F_OK)) {
		error("Logins currently disabled.\n");
		exit(1);
	}
#ifdef	KERBEROS
	if(encrypt) {
		char c = KERB_RCMD_MUTUAL;
		(void) write(2, &c, 1);
	} else {
		char c = KERB_RCMD;
		(void) write(2, &c, 1);
	}
#else
	(void) write(2, "\0", 1);
#endif

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
			readfrom = (1L<<s) | (1L<<pv[0]);
			ioctl(pv[0], FIONBIO, (char *)&one);
			/* should set s nbio! */
			do {
				ready = readfrom;
				if (select(16, &ready, (fd_set *)0,
				    (fd_set *)0, (struct timeval *)0) < 0)
					break;
				if (ready & (1L<<s)) {
					if (read(s, &sig, 1) <= 0)
						readfrom &= ~(1L<<s);
					else
						killpg(pid, sig);
				}
				if (ready & (1L<<pv[0])) {
					errno = 0;
					cc = read(pv[0], buf, sizeof (buf));
					if (cc <= 0) {
						shutdown(s, 1+1);
						readfrom &= ~(1L<<pv[0]);
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

int
same_domain(h1, h2)
	register char	*h1, *h2;
{
	register char *p1 = index(h1, '.');
	register char *p2 = index(h2, '.');
	if(!strcasecmp(p1, p2))
		return(1);
	return(0);
}
