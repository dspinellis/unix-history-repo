#ifndef lint
static char sccsid[] = "@(#)rshd.c	4.1 82/04/02";
#endif

#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>
#include <pwd.h>
#include <wait.h>
#include <signal.h>

int	errno;
struct	sockaddr_in sin = { AF_INET, IPPORT_CMDSERVER };
struct	passwd *getpwnam();
char	*index(), *rindex(), *raddr(), *sprintf();
int	options = SO_ACCEPTCONN|SO_KEEPALIVE;
/* VARARGS 1 */
int	error();
/*
 * remote execute server:
 *	remuser\0
 *	locuser\0
 *	command\0
 *	data
 */
main(argc, argv)
	int argc;
	char **argv;
{
	union wait status;
	int f;
	struct sockaddr_in from;

#ifndef DEBUG
	if (fork())
		exit(0);
	for (f = 0; f < 10; f++)
		(void) close(f);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int t = open("/dev/tty", 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
#if vax
	sin.sin_port = htons(sin.sin_port);
#endif
	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d"))
		options |= SO_DEBUG;
	for (;;) {
		errno = 0;
		f = socket(SOCK_STREAM, 0, &sin, options);
		if (f < 0) {
			perror("socket");
			sleep(5);
			continue;
		}
		if (accept(f, &from) < 0) {
			perror("accept");
			(void) close(f);
			sleep(1);
			continue;
		}
		if (fork() == 0)
			doit(f, &from);
		(void) close(f);
		while(wait3(status, WNOHANG, 0) > 0)
			continue;
	}
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
	char *rhost;
	int s;
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
	dup2(f, 0);
	dup2(f, 1);
	dup2(f, 2);
#if vax
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
#endif
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED)
		exit(1);
	(void) alarm(60);
	port = 0;
	for (;;) {
		char c;
		if (read(f, &c, 1) != 1)
			exit(1);
		if (c == 0)
			break;
		port = port * 10 + c - '0';
	}
	(void) alarm(0);
	if (port != 0) {
		s = rresvport(0);
		if (s < 0)
			exit(1);
		if (port >= IPPORT_RESERVED)
			goto protofail;
		(void) alarm(60);
		fromp->sin_port = port;
#if vax
		fromp->sin_port = ntohs(fromp->sin_port);
#endif
		if (connect(s, fromp) < 0)
			exit(1);
		(void) alarm(0);
	}
	rhost = raddr(fromp->sin_addr.s_addr);
	if (rhost == 0) {
		error("Host name for your address unknown\n");
		exit(1);
	}
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
		error("No remote directory.\n");
		exit(1);
	}
	if (ruserok(rhost, remuser, locuser) < 0) {
		error("Permission denied.\n");
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
			ioctl(pv[1], FIONBIO, (char *)&one);
			/* should set s nbio! */
			do {
				ready = readfrom;
				(void) select(32, &ready, 0, 1000000);
				if (ready & (1<<s)) {
					if (read(s, &sig, 1) <= 0)
						readfrom &= ~(1<<s);
					else
						killpg(pid, sig);
				}
				if (ready & (1<<pv[0])) {
					cc = read(pv[0], buf, sizeof (buf));
					if (cc <= 0) {
						int done = 1+1;
						ioctl(s, SIOCDONE, (char *)&done);
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
	inigrp(pwd->pw_name, pwd->pw_gid);
	(void) setuid(pwd->pw_uid);
	(void) setgid(pwd->pw_gid);
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
protofail:
	error("rsh: protocol failure detected by remote\n");
	exit(1);
}

/* VARARGS 1 */
error(fmt)
	char *fmt;
{
	char buf[BUFSIZ];

	buf[0] = 1;
	(void) sprintf(buf+1, fmt);
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
