/*-
 * Copyright (c) 1983, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rsh.c	5.21 (Berkeley) 5/15/90";
#endif /* not lint */

/*
 * $Source: mit/rsh/RCS/rsh.c,v $
 * $Header: mit/rsh/RCS/rsh.c,v 5.1 89/07/31 19:28:59 kfall Exp Locker: kfall $
 */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include <netinet/in.h>
#include <netdb.h>

#include <pwd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <varargs.h>
#include "pathnames.h"

#ifdef KERBEROS
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>

CREDENTIALS cred;
Key_schedule schedule;
int use_kerberos = 1, encrypt;
char dst_realm_buf[REALM_SZ], *dest_realm;
extern char *krb_realmofhost();
#endif

/*
 * rsh - remote shell
 */
extern int errno;
int rfd2;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	struct passwd *pw;
	struct servent *sp;
	long omask;
	int argoff, asrsh, ch, dflag, nflag, one, pid, rem, uid;
	register char *p;
	char *args, *host, *user, *copyargs();
	void sendsig();

	argoff = asrsh = dflag = nflag = 0;
	one = 1;
	host = user = NULL;

	/* if called as something other than "rsh", use it as the host name */
	if (p = rindex(argv[0], '/'))
		++p;
	else
		p = argv[0];
	if (strcmp(p, "rsh"))
		host = p;
	else
		asrsh = 1;

	/* handle "rsh host flags" */
	if (!host && argc > 2 && argv[1][0] != '-') {
		host = argv[1];
		argoff = 1;
	}

#ifdef KERBEROS
#define	OPTIONS	"8KLdek:l:nwx"
#else
#define	OPTIONS	"8KLdel:nw"
#endif
	while ((ch = getopt(argc - argoff, argv + argoff, OPTIONS)) != EOF)
		switch(ch) {
		case 'K':
#ifdef KERBEROS
			use_kerberos = 0;
#endif
			break;
		case 'L':	/* -8Lew are ignored to allow rlogin aliases */
		case 'e':
		case 'w':
		case '8':
			break;
		case 'd':
			dflag = 1;
			break;
		case 'l':
			user = optarg;
			break;
#ifdef KERBEROS
		case 'k':
			dest_realm = dst_realm_buf;
			strncpy(dest_realm, optarg, REALM_SZ);
			break;
#endif
		case 'n':
			nflag = 1;
			break;
#ifdef KERBEROS
		case 'x':
			encrypt = 1;
			des_set_key(cred.session, schedule);
			break;
#endif
		case '?':
		default:
			usage();
		}
	optind += argoff;

	/* if haven't gotten a host yet, do so */
	if (!host && !(host = argv[optind++]))
		usage();

	/* if no further arguments, must have been called as rlogin. */
	if (!argv[optind]) {
		if (asrsh)
			*argv = "rlogin";
		execv(_PATH_RLOGIN, argv);
		(void)fprintf(stderr, "rsh: can't exec %s.\n", _PATH_RLOGIN);
		exit(1);
	}

	argc -= optind;
	argv += optind;

	if (!(pw = getpwuid(uid = getuid()))) {
		(void)fprintf(stderr, "rsh: unknown user id.\n");
		exit(1);
	}
	if (!user)
		user = pw->pw_name;

#ifdef KERBEROS
	/* -x turns off -n */
	if (encrypt)
		nflag = 0;
#endif

	args = copyargs(argv);

	sp = NULL;
#ifdef KERBEROS
	if (use_kerberos) {
		sp = getservbyname((encrypt ? "ekshell" : "kshell"), "tcp");
		if (sp == NULL) {
			use_kerberos = 0;
			warning("can't get entry for %s/tcp service",
			    encrypt ? "ekshell" : "kshell");
		}
	}
#endif
	if (sp == NULL)
		sp = getservbyname("shell", "tcp");
	if (sp == NULL) {
		(void)fprintf(stderr, "rsh: shell/tcp: unknown service.\n");
		exit(1);
	}

#ifdef KERBEROS
try_connect:
	if (use_kerberos) {
		rem = KSUCCESS;
		errno = 0;
		if (dest_realm == NULL)
			dest_realm = krb_realmofhost(host);

		if (encrypt)
			rem = krcmd_mutual(&host, sp->s_port, user, args,
			    &rfd2, dest_realm, &cred, schedule);
		else
			rem = krcmd(&host, sp->s_port, user, args, &rfd2,
			    dest_realm);
		if (rem < 0) {
			use_kerberos = 0;
			sp = getservbyname("shell", "tcp");
			if (sp == NULL) {
				(void)fprintf(stderr,
				    "rsh: unknown service shell/tcp.\n");
				exit(1);
			}
			if (errno == ECONNREFUSED)
				warning("remote host doesn't support Kerberos");
			if (errno == ENOENT)
				warning("can't provide Kerberos auth data");
			goto try_connect;
		}
	} else {
		if (encrypt) {
			(void)fprintf(stderr,
			    "rsh: the -x flag requires Kerberos authentication.\n");
			exit(1);
		}
		rem = rcmd(&host, sp->s_port, pw->pw_name, user, args, &rfd2);
	}
#else
	rem = rcmd(&host, sp->s_port, pw->pw_name, user, args, &rfd2);
#endif

	if (rem < 0)
		exit(1);

	if (rfd2 < 0) {
		(void)fprintf(stderr, "rsh: can't establish stderr.\n");
		exit(1);
	}
	if (dflag) {
		if (setsockopt(rem, SOL_SOCKET, SO_DEBUG, &one,
		    sizeof(one)) < 0)
			(void)fprintf(stderr, "rsh: setsockopt: %s.\n",
			    strerror(errno));
		if (setsockopt(rfd2, SOL_SOCKET, SO_DEBUG, &one,
		    sizeof(one)) < 0)
			(void)fprintf(stderr, "rsh: setsockopt: %s.\n",
			    strerror(errno));
	}

	(void)setuid(uid);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGTERM));
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void)signal(SIGINT, sendsig);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		(void)signal(SIGQUIT, sendsig);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		(void)signal(SIGTERM, sendsig);

	if (!nflag) {
		pid = fork();
		if (pid < 0) {
			(void)fprintf(stderr,
			    "rsh: fork: %s.\n", strerror(errno));
			exit(1);
		}
	}

#ifdef KERBEROS
	if (!encrypt)
#endif
	{
		(void)ioctl(rfd2, FIONBIO, &one);
		(void)ioctl(rem, FIONBIO, &one);
	}

	talk(nflag, omask, pid, rem);

	if (!nflag)
		(void)kill(pid, SIGKILL);
	exit(0);
}

talk(nflag, omask, pid, rem)
	int nflag, pid;
	long omask;
	register int rem;
{
	register int cc, wc;
	register char *bp;
	int readfrom, ready, rembits;
	char buf[BUFSIZ];

	if (!nflag && pid == 0) {
		(void)close(rfd2);

reread:		errno = 0;
		if ((cc = read(0, buf, sizeof buf)) <= 0)
			goto done;
		bp = buf;

rewrite:	rembits = 1 << rem;
		if (select(16, 0, &rembits, 0, 0) < 0) {
			if (errno != EINTR) {
				(void)fprintf(stderr,
				    "rsh: select: %s.\n", strerror(errno));
				exit(1);
			}
			goto rewrite;
		}
		if ((rembits & (1 << rem)) == 0)
			goto rewrite;
#ifdef KERBEROS
		if (encrypt)
			wc = des_write(rem, bp, cc);
		else
#endif
			wc = write(rem, bp, cc);
		if (wc < 0) {
			if (errno == EWOULDBLOCK)
				goto rewrite;
			goto done;
		}
		bp += wc;
		cc -= wc;
		if (cc == 0)
			goto reread;
		goto rewrite;
done:
		(void)shutdown(rem, 1);
		exit(0);
	}

	(void)sigsetmask(omask);
	readfrom = (1 << rfd2) | (1 << rem);
	do {
		ready = readfrom;
		if (select(16, &ready, 0, 0, 0) < 0) {
			if (errno != EINTR) {
				(void)fprintf(stderr,
				    "rsh: select: %s.\n", strerror(errno));
				exit(1);
			}
			continue;
		}
		if (ready & (1 << rfd2)) {
			errno = 0;
#ifdef KERBEROS
			if (encrypt)
				cc = des_read(rfd2, buf, sizeof buf);
			else
#endif
				cc = read(rfd2, buf, sizeof buf);
			if (cc <= 0) {
				if (errno != EWOULDBLOCK)
					readfrom &= ~(1 << rfd2);
			} else
				(void)write(2, buf, cc);
		}
		if (ready & (1 << rem)) {
			errno = 0;
#ifdef KERBEROS
			if (encrypt)
				cc = des_read(rem, buf, sizeof buf);
			else
#endif
				cc = read(rem, buf, sizeof buf);
			if (cc <= 0) {
				if (errno != EWOULDBLOCK)
					readfrom &= ~(1 << rem);
			} else
				(void)write(1, buf, cc);
		}
	} while (readfrom);
}

void
sendsig(signo)
	char signo;
{
#ifdef KERBEROS
	if (encrypt)
		(void)des_write(rfd2, &signo, 1);
	else
#endif
		(void)write(rfd2, &signo, 1);
}

#ifdef KERBEROS
/* VARARGS */
warning(va_alist)
va_dcl
{
	va_list ap;
	char *fmt;

	(void)fprintf(stderr, "rsh: warning, using standard rsh: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, ".\n");
}
#endif

char *
copyargs(argv)
	char **argv;
{
	register int cc;
	register char **ap, *p;
	char *args, *malloc();

	cc = 0;
	for (ap = argv; *ap; ++ap)
		cc += strlen(*ap) + 1;
	if (!(args = malloc((u_int)cc))) {
		(void)fprintf(stderr, "rsh: %s.\n", strerror(ENOMEM));
		exit(1);
	}
	for (p = args, ap = argv; *ap; ++ap) {
		(void)strcpy(p, *ap);
		for (p = strcpy(p, *ap); *p; ++p);
		if (ap[1])
			*p++ = ' ';
	}
	return(args);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: rsh [-ndx]%s[-l login] host [command]\n",
#ifdef KERBEROS
	    " [-k realm] ");
#else
	    " ");
#endif
	exit(1);
}
