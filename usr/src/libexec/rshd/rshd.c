#ifndef lint
static char sccsid[] = "@(#)rshd.c	4.14 83/02/23";
#endif

#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <wait.h>
#include <signal.h>
#include <netdb.h>

int	errno;
int	reapchild();
struct	sockaddr_in sin = { AF_INET };
struct	passwd *getpwnam();
char	*index(), *rindex(), *sprintf();
int	options;
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
	int f;
	struct sockaddr_in from;
	struct servent *sp;

	sp = getservbyname("shell", "tcp");
	if (sp == 0) {
		fprintf(stderr, "rshd: tcp/shell: unknown service\n");
		exit(1);
	}
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
	sin.sin_port = sp->s_port;
	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d")) {
		options |= SO_DEBUG;
		argc--, argv++;
	}
	if (argc > 0) {
		int port = atoi(argv[0]);

		if (port < 0) {
			fprintf(stderr, "%s: bad port #\n", argv[0]);
			exit(1);
		}
		sin.sin_port = htons((u_short)port);
		argc--, argv++;
	}
	f = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (f < 0) {
		perror("rshd: socket");
		exit(1);
	}
	if (options & SO_DEBUG && setsockopt(f, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
		perror("rshd: setsockopt (SO_DEBUG)");
#ifdef notdef
	if (setsockopt(f, SOL_SOCKET, SO_KEEPALIVE, 0, 0) < 0)
		perror("rshd: setsockopt (SO_KEEPALIVE)");
#endif
	if (bind(f, (caddr_t)&sin, sizeof (sin), 0) < 0) {
		perror("rshd: bind");
		exit(1);
	}
	sigset(SIGCHLD, reapchild);
	listen(f, 10);
	for (;;) {
		int g, len = sizeof (from);

		g = accept(f, &from, &len, 0);
		if (g < 0) {
			if (errno == EINTR)
				continue;
			perror("rshd: accept");
			continue;
		}
		if (fork() == 0) {
			signal(SIGCHLD, SIG_IGN);
			close(f);
			doit(g, &from);
		}
		close(g);
	}
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
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
	int s, backoff;
	struct hostent *hp;
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
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED) {
		fprintf(stderr, "rshd: malformed from address\n");
		exit(1);
	}
	(void) alarm(60);
	port = 0;
	for (;;) {
		char c;
		if (read(f, &c, 1) != 1) {
			perror("rshd: read");
			shutdown(f, 1+1);
			exit(1);
		}
		if (c == 0)
			break;
		port = port * 10 + c - '0';
	}
	(void) alarm(0);
	if (port != 0) {
		int lport = IPPORT_RESERVED - 1, retryshift;
		s = rresvport(&lport);
		if (s < 0) {
			perror("rshd: can't get stderr port");
			exit(1);
		}
		if (port >= IPPORT_RESERVED) {
			fprintf(stderr, "rshd: 2nd port not reserved\n");
			exit(1);
		}
		fromp->sin_port = htons((u_short)port);
		if (connect(s, fromp, sizeof (*fromp), 0) < 0) {
			perror("rshd: connect");
			exit(1);
		}
	}
	dup2(f, 0);
	dup2(f, 1);
	dup2(f, 2);
	hp = gethostbyaddr(&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp == 0) {
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
	if (ruserok(hp->h_name, pwd->pw_uid == 0, remuser, locuser) < 0) {
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
				if (select(16, &ready, 0, 0, 0) < 0)
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
	initgroups(pwd->pw_name, pwd->pw_gid);
	(void) setgid(pwd->pw_gid);
	(void) setuid(pwd->pw_uid);
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
