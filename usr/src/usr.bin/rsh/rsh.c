#ifndef lint
static char sccsid[] = "@(#)rsh.c	4.1 82/04/02";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <sys/file.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <pwd.h>

/*
 * rsh - remote shell
 */
/* VARARGS */
int	error();
char	*index(), *rindex(), *malloc(), *getpass(), *sprintf(), *strcpy();

struct	passwd *getpwuid();

int	errno;
int	options;
int	rfd2;
int	sendsig();

main(argc, argv0)
	int argc;
	char **argv0;
{
	int rem, pid;
	char *host, *cp, **ap, buf[BUFSIZ], *args, **argv = argv0, *user = 0;
	register int cc;
	int asrsh = 0;
	struct passwd *pwd;
	int readfrom, ready;
	int one = 1;

	host = rindex(argv[0], '/');
	if (host)
		host++;
	else
		host = argv[0];
	argv++, --argc;
	if (!strcmp(host, "rsh")) {
		host = *argv++, --argc;
		asrsh = 1;
	}
another:
	if (!strcmp(*argv, "-l")) {
		argv++, argc--;
		if (argc > 0)
			user = *argv++, argc--;
		goto another;
	}
	if (!strcmp(*argv, "-n")) {
		argv++, argc--;
		(void) close(0);
		(void) open("/dev/null", 0);
		goto another;
	}
	if (!strcmp(*argv, "-d")) {
		argv++, argc--;
		options |= SO_DEBUG;
		goto another;
	}
	if (host == 0)
		goto usage;
	if (argv[0] == 0) {
		if (asrsh)
			*argv0 = "rlogin";
		execv("/usr/ucb/rlogin", argv0);
		perror("/usr/ucb/rlogin");
		exit(1);
	}
	pwd = getpwuid(getuid());
	if (pwd == 0) {
		fprintf(stderr, "who are you?\n");
		exit(1);
	}
	cc = 0;
	for (ap = argv; *ap; ap++)
		cc += strlen(*ap) + 1;
	cp = args = malloc(cc);
	for (ap = argv; *ap; ap++) {
		(void) strcpy(cp, *ap);
		while (*cp)
			cp++;
		if (ap[1])
			*cp++ = ' ';
	}
        rem = rcmd(&host, IPPORT_CMDSERVER, pwd->pw_name,
	    user ? user : pwd->pw_name, args, &rfd2);
        if (rem < 0)
                exit(1);
	(void) setuid(getuid());
	sigacts(SIG_HOLD);
        pid = fork();
        if (pid < 0) {
		perror("fork");
                exit(1);
        }
	ioctl(rfd2, FIONBIO, &one);
	ioctl(rem, FIONBIO, &one);
        if (pid == 0) {
		char *bp; int rembits, wc;
		(void) close(rfd2);
	reread:
		cc = read(0, buf, sizeof buf);
		if (cc <= 0)
			goto done;
		bp = buf;
	rewrite:
		rembits = 1<<rem;
		(void) select(20, 0, &rembits, 100000);
		if ((rembits & (1<<rem)) == 0)
			goto rewrite;
		wc = write(rem, bp, cc);
		if (wc < 0) {
			if (errno == EWOULDBLOCK)
				goto rewrite;
			goto done;
		}
		cc -= wc; bp += wc;
		if (cc == 0)
			goto reread;
		goto rewrite;
	done:
		{ int flags = 1; ioctl(rem, SIOCDONE, &flags); }
		exit(0);
	}
	sigacts(sendsig);
	readfrom = (1<<rfd2) | (1<<rem);
	do {
		ready = readfrom;
		(void) select(32, &ready, 0, 10000000);
		if (ready & (1<<rfd2)) {
			errno = 0;
			cc = read(rfd2, buf, sizeof buf);
			if (cc <= 0) {
				if (errno != EWOULDBLOCK)
					readfrom &= ~(1<<rfd2);
			} else
				(void) write(2, buf, cc);
		}
		if (ready & (1<<rem)) {
			errno = 0;
			cc = read(rem, buf, sizeof buf);
			if (cc <= 0) {
				if (errno != EWOULDBLOCK)
					readfrom &= ~(1<<rem);
			} else
				(void) write(1, buf, cc);
		}
        } while (readfrom);
        (void) kill(pid, SIGKILL);
	exit(0);
usage:
	fprintf(stderr,
	    "usage: rsh host [ -l login ] [ -p passwd ] command\n");
	exit(1);
}

sigacts(state)
	int (*state)();
{

	sigset(SIGINT, state);
	sigset(SIGQUIT, state);
	sigset(SIGTERM, state);
}

sendsig(signo)
	int signo;
{

	(void) write(rfd2, (char *)&signo, 1);
}
