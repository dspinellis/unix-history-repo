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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rsh.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <pwd.h>
#include <netdb.h>
#include "pathnames.h"

#ifdef	KERBEROS
#include <kerberos/krb.h>
char	krb_realm[REALM_SZ];
int	use_kerberos = 1, encrypt = 0;
CREDENTIALS	cred;
Key_schedule	schedule;
#endif	/* KERBEROS */

/*
 * rsh - remote shell
 */
/* VARARGS */
int	error();
char	*index(), *rindex(), *malloc(), *getpass(), *strcpy();

struct	passwd *getpwuid();

int	errno;
int	options;
int	rfd2;
int	nflag;
int	sendsig();

#define	mask(s)	(1 << ((s) - 1))

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
	struct servent *sp;
	int omask;

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
	if (argc > 0 && !strcmp(*argv, "-l")) {
		argv++, argc--;
		if (argc > 0)
			user = *argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strcmp(*argv, "-n")) {
		argv++, argc--;
		nflag++;
		goto another;
	}
	if (argc > 0 && !strcmp(*argv, "-d")) {
		argv++, argc--;
		options |= SO_DEBUG;
		goto another;
	}
	/*
	 * Ignore the -L, -w, -e and -8 flags to allow aliases with rlogin
	 * to work
	 *
	 * There must be a better way to do this! -jmb
	 */
	if (argc > 0 && !strncmp(*argv, "-L", 2)) {
		argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strncmp(*argv, "-w", 2)) {
		argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strncmp(*argv, "-e", 2)) {
		argv++, argc--;
		goto another;
	}
	if (argc > 0 && !strncmp(*argv, "-8", 2)) {
		argv++, argc--;
		goto another;
	}

#ifdef	KERBEROS
	if(argc > 0 && !strncmp(*argv, "-x", 2)) {
		encrypt = 1;
		des_set_key(cred.session, schedule);
		argv++, argc--;
		goto another;
	}

	if(argc > 0 && !strcmp(*argv, "-k")) {
		argv++, argc--;
		if(argc <= 0 || (**argv == '-')) {
			fprintf(stderr, "-k option requires an argument\n");
			exit(1);
		}
		strncpy(krb_realm, *argv, REALM_SZ);
		argv++, argc--;
		goto another;
	}
#endif

	if (host == 0)
		goto usage;
	if (argv[0] == 0) {
		if (asrsh)
			*argv0 = "rlogin";
		execv(_PATH_RLOGIN, argv0);
		perror(_PATH_RLOGIN);
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
#ifdef	KERBEROS
	sp = getservbyname("kshell", "tcp");
	if (sp == NULL) {
		use_kerberos = 0;
		old_warning("kshell service unknown");
		sp = getservbyname("shell", "tcp");
	}
#else
	sp = getservbyname("shell", "tcp");
#endif

	if (sp == 0) {
		fprintf(stderr, "rsh: shell/tcp: unknown service\n");
		exit(1);
	}

#ifdef	KERBEROS
try_connect:
	if(use_kerberos) {
		rem = KSUCCESS;
		if(krb_realm[0] == '\0') {
			rem = krb_get_lrealm(krb_realm, 1);
		}

		if(rem == KSUCCESS) {
			if(encrypt) {
				rem = krcmd_mutual(
					&host, sp->s_port,
					user ? user : pwd->pw_name,
					args,
					&rfd2,
					krb_realm,
					&cred, schedule);
			} else {
				rem = krcmd(
					&host,
					sp->s_port,
					user ? user : pwd->pw_name,
					args,
					&rfd2,
					krb_realm
				);
			}
		} else {
			fprintf(stderr,
			    "%s: error getting local realm\n",
			    argv0[0]);
			exit(1);
		}
		if((rem < 0) && errno == ECONNREFUSED) {
			use_kerberos = 0;
			sp = getservbyname("shell", "tcp");
			if(sp == NULL) {
				fprintf(stderr, "unknown service shell/tcp\n");
				exit(1);
			}
			old_warning("remote host doesn't support Kerberos");
			goto try_connect;
		}
	} else {
		if(encrypt) {
			fprintf(stderr,"The -x flag requires Kerberos authentication\n");
			exit(1);
		}
        	rem = rcmd(&host, sp->s_port, pwd->pw_name,
	    		user ? user : pwd->pw_name, args, &rfd2);
	}

#else
			   
        rem = rcmd(&host, sp->s_port, pwd->pw_name,
		user ? user : pwd->pw_name, args, &rfd2);
#endif

        if (rem < 0)
                exit(1);

	if (rfd2 < 0) {
		fprintf(stderr, "rsh: can't establish stderr\n");
		exit(2);
	}
	if (options & SO_DEBUG) {
		if (setsockopt(rem, SOL_SOCKET, SO_DEBUG, &one, sizeof (one)) < 0)
			perror("setsockopt (stdin)");
		if (setsockopt(rfd2, SOL_SOCKET, SO_DEBUG, &one, sizeof (one)) < 0)
			perror("setsockopt (stderr)");
	}
	(void) setuid(getuid());
	omask = sigblock(mask(SIGINT)|mask(SIGQUIT)|mask(SIGTERM));
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, sendsig);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, sendsig);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, sendsig);
	if (nflag == 0) {
		pid = fork();
		if (pid < 0) {
			perror("fork");
			exit(1);
		}
	}
	ioctl(rfd2, FIONBIO, &one);
	ioctl(rem, FIONBIO, &one);
        if (nflag == 0 && pid == 0) {
		char *bp; int rembits, wc;
		(void) close(rfd2);
	reread:
		errno = 0;
		cc = read(0, buf, sizeof buf);
		if (cc <= 0)
			goto done;
		bp = buf;
	rewrite:
		rembits = 1<<rem;
		if (select(16, 0, &rembits, 0, 0) < 0) {
			if (errno != EINTR) {
				perror("select");
				exit(1);
			}
			goto rewrite;
		}
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
		(void) shutdown(rem, 1);
		exit(0);
	}
	sigsetmask(omask);
	readfrom = (1<<rfd2) | (1<<rem);
	do {
		ready = readfrom;
		if (select(16, &ready, 0, 0, 0) < 0) {
			if (errno != EINTR) {
				perror("select");
				exit(1);
			}
			continue;
		}
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
	if (nflag == 0)
		(void) kill(pid, SIGKILL);
	exit(0);
usage:
	fprintf(stderr,
#ifdef	KERBEROS
	    "usage: rsh host [ -l login ] [ -n ] [ -k realm ] command\n");
#else
	    "usage: rsh host [ -l login ] [ -n ] command\n");
#endif
	exit(1);
}

sendsig(signo)
	char signo;
{

	(void) write(rfd2, &signo, 1);
}

#ifdef	KERBEROS
int
old_warning(str)
	char	*str;
{
	fprintf(stderr,"Warning: %s, using standard rsh\n", str);
}
#endif
