/*
 * Copyright (c) 1988 The Regents of the University of California.
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
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)su.c	5.14 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <syslog.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include "pathnames.h"

#ifdef	KERBEROS
#include <krb.h>	
#include <netdb.h>
extern	char	*krb_err_txt[];
int	use_kerberos = 1;
int	kerno = 0;
char	lrealm[REALM_SZ], krbtkfile[MAXPATHLEN];
#define	MAXPWSIZE	128
#define	ARGSTR	"-flmn"
char	pw_buf[MAXPWSIZE];
#else
#define	ARGSTR	"-flm"
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	extern char **environ;
	extern int errno, optind;
	register struct passwd *pwd;
	register char *p, **g;
	struct group *gr;
	uid_t ruid, getuid();
	int asme, ch, fulllogin, fastlogin, prio;
	enum { UNSET, YES, NO } iscsh = UNSET;
	char *user, *shell, *username, *cleanenv[2], *nargv[4], **np;
	char namebuf[50], shellbuf[MAXPATHLEN];
	char *crypt(), *getpass(), *getenv(), *getlogin(), *rindex(), *strcpy();

	np = &nargv[3];
	*np-- = NULL;
	asme = fulllogin = fastlogin = 0;
	while ((ch = getopt(argc, argv, ARGSTR)) != EOF)
		switch((char)ch) {
		case 'f':
			fastlogin = 1;
			break;
		case '-':
		case 'l':
			fulllogin = 1;
			break;
		case 'm':
			asme = 1;
			break;
#ifdef	KERBEROS
		case 'n':
			use_kerberos = 0;
			break;
#endif
		case '?':
		default:
			fprintf(stderr, "usage: su [-flm] [login]\n");
			exit(1);
		}
	argv += optind;

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (errno)
		prio = 0;
	(void)setpriority(PRIO_PROCESS, 0, -2);

	/* get current login name and shell */
	if ((pwd = getpwuid(ruid = getuid())) == NULL) {
		fprintf(stderr, "su: who are you?\n");
		exit(1);
	}
	username = strcpy(namebuf, pwd->pw_name);
	if (asme)
		if (pwd->pw_shell && *pwd->pw_shell)
			shell = strcpy(shellbuf,  pwd->pw_shell);
		else {
			shell = _PATH_BSHELL;
			iscsh = NO;
		}

	/* get target login information, default to root */
	user = *argv ? *argv : "root";
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "su: unknown login %s\n", user);
		exit(1);
	}

	/* only allow those in group zero to su to root. */
	if (pwd->pw_uid == 0 && (gr = getgrgid((gid_t)0)))
		for (g = gr->gr_mem;; ++g) {
			if (!*g) {
				(void)fprintf(stderr,
				    "su: you are not in the correct group to su %s.\n", user);
				exit(1);
			}
			if (!strcmp(username, *g))
				break;
		}
	openlog("su", LOG_CONS, 0);

#ifdef	KERBEROS
again:
	if (use_kerberos && ruid) {
		KTEXT_ST	ticket;
		AUTH_DAT	authdata;
		char		hostname[MAXHOSTNAMELEN];
		char		savehost[MAXHOSTNAMELEN];
		u_long		faddr;
		struct hostent	*hp;

		if (krb_get_lrealm(lrealm, 1) != KSUCCESS) {
			(void)fprintf(stderr, "Couldn't get local realm\n");
			use_kerberos = 0;
			goto again;
		}

		if (koktologin(username, lrealm, user) && !pwd->pw_uid) {
			(void)fprintf(stderr,
				"kerberos su: not in %s's ACL\n", user);
			use_kerberos = 0;
			goto again;
		}

		(void)sprintf(krbtkfile, "%s_%s_%d", TKT_ROOT, user, getuid());
		/* setuid(pwd->pw_uid); */

		if (read_pw_string(pw_buf, sizeof(pw_buf)-1,
		   "Kerberos password: ", 0)) {
			(void)fprintf(stderr, "error reading password\n");
			use_kerberos = 0;
			goto again;
		}

		p = pw_buf;
		setenv("KRBTKFILE", krbtkfile, 1);
		/* short lifetime for root tickets */
		if (setuid(0) < 0) {
			perror("setuid");
			use_kerberos = 0;
			goto again;
		}
		(void)unlink(krbtkfile);
		/* POLICY: short ticket lifetime for root */
		kerno = krb_get_pw_in_tkt(username,
			(pwd->pw_uid == 0 ? "root" : ""),
			lrealm, "krbtgt", lrealm,
			(pwd->pw_uid == 0 ? 2 : DEFAULT_TKT_LIFE),
			p);

		(void)bzero(p, strlen(p));
		if (kerno != KSUCCESS) {
			if (kerno == KDC_PR_UNKNOWN) {
				use_kerberos = 0;
				goto again;
			}
			printf("su: unable to su: %s\n",
				krb_err_txt[kerno]);
			syslog(LOG_NOTICE|LOG_AUTH,
				"su: BAD Kerberos SU: %s on %s: %s",
					username, ttyname(2), krb_err_txt[kerno]);
			use_kerberos = 0;
			goto again;
		} else if (chown(krbtkfile, pwd->pw_uid, -1) < 0) {
				perror("su: chown:");
				(void)unlink(krbtkfile);
				use_kerberos = 0;
				goto again;
		}

		setpriority(PRIO_PROCESS, 0, -2);

		if (gethostname(hostname, sizeof(hostname)) == -1) {
			fprintf(stderr, "couldn't get local hostname\n");
			dest_tkt();
			use_kerberos = 0;
			goto again;
		}

		(void) strncpy(savehost, krb_get_phost(hostname),
			sizeof(savehost));
		savehost[sizeof(savehost)-1] = 0;

		kerno = krb_mk_req(&ticket, "rcmd", savehost, lrealm, 33);

		if (kerno == KDC_PR_UNKNOWN) {
			printf("Warning: tgt not verified\n");
			syslog(LOG_NOTICE|LOG_AUTH,
				"su: %s on %s, tgt not verified",
				username, ttyname(2));
		} else if (kerno != KSUCCESS) {
			printf("Unable to use tgt: %s\n", krb_err_txt[kerno]);
			syslog(LOG_NOTICE|LOG_AUTH,
				"su: failed su: %s on %s: %s",
				username, ttyname(2), krb_err_txt[kerno]);
			dest_tkt();
			use_kerberos = 0;
			goto again;
		} else {
			if (!(hp = gethostbyname(hostname))) {
				printf("can't get addr of %s\n", hostname);
				dest_tkt();
				use_kerberos = 0;
				goto again;
			}
			(void) bcopy((char *)hp->h_addr, (char *) &faddr,
				sizeof(faddr));

			if ((kerno = krb_rd_req(&ticket, "rcmd", savehost,
				faddr, &authdata, "")) != KSUCCESS) {
				printf("unable to verify rcmd ticket: %s\n",
					krb_err_txt[kerno]);
				syslog(LOG_NOTICE|LOG_AUTH,
					"su: failed su: %s on %s: %s",
					    username, ttyname(2), krb_err_txt[kerno]);
				dest_tkt();
				use_kerberos = 0;
				goto again;
			}
		}

	} else
#endif

	/* if target requires a password, verify it */
	if (ruid && *pwd->pw_passwd) {
		p = getpass("Password:");
		if (strcmp(pwd->pw_passwd, crypt(p, pwd->pw_passwd))) {
			fprintf(stderr, "Sorry\n");
			if (pwd->pw_uid == 0)
				syslog(LOG_AUTH|LOG_CRIT,
				    "BAD SU %s on %s", username, ttyname(2));
			exit(1);
		}
	}

	if (asme) {
		/* if asme and non-standard target shell, must be root */
		if (!chshell(pwd->pw_shell) && ruid) {
			fprintf(stderr, "su: Permission denied.\n");
			exit(1);
		}
	}
	else if (pwd->pw_shell && *pwd->pw_shell) {
		shell = pwd->pw_shell;
		iscsh = UNSET;
	} else {
		shell = _PATH_BSHELL;
		iscsh = NO;
	}

	/* if we're forking a csh, we want to slightly muck the args */
	if (iscsh == UNSET) {
		if (p = rindex(shell, '/'))
			++p;
		else
			p = shell;
		iscsh = strcmp(p, "csh") ? NO : YES;
	}

	/* set permissions */
	if (setgid(pwd->pw_gid) < 0) {
		perror("su: setgid");
		exit(1);
	}
	if (initgroups(user, pwd->pw_gid)) {
		fprintf(stderr, "su: initgroups failed\n");
		exit(1);
	}
	if (setuid(pwd->pw_uid) < 0) {
		perror("su: setuid");
		exit(1);
	}

	if (!asme) {
		if (fulllogin) {
			p = getenv("TERM");
			cleanenv[0] = _PATH_SEARCHPATH;
			cleanenv[1] = NULL;
			environ = cleanenv;
			(void)setenv("TERM", p, 1);
			if (chdir(pwd->pw_dir) < 0) {
				fprintf(stderr, "su: no directory\n");
				exit(1);
			}
		}
		if (fulllogin || pwd->pw_uid)
			(void)setenv("USER", pwd->pw_name, 1);
		(void)setenv("HOME", pwd->pw_dir, 1);
		(void)setenv("SHELL", shell, 1);
	}

	if (iscsh == YES) {
		if (fastlogin)
			*np-- = "-f";
		if (asme)
			*np-- = "-m";
	}

	/* csh strips the first character... */
	*np = fulllogin ? "-su" : iscsh == YES ? "_su" : "su";

	if (pwd->pw_uid == 0)
		syslog(LOG_NOTICE|LOG_AUTH, "%s on %s",
		    username, ttyname(2));

	(void)setpriority(PRIO_PROCESS, 0, prio);

	execv(shell, np);
	fprintf(stderr, "su: no shell.\n");
	exit(1);
}

chshell(sh)
	char *sh;
{
	char *cp, *getusershell();

	while ((cp = getusershell()) != NULL)
		if (!strcmp(cp, sh))
			return(1);
	return(0);
}

#ifdef	KERBEROS
int
koktologin(name, realm, toname)
	char	*name, *realm, *toname;
{
	AUTH_DAT	kdata_st;
	AUTH_DAT	*kdata = &kdata_st;

	bzero((caddr_t) kdata, sizeof(*kdata));
	strcpy(kdata->pname, name);
	strcpy(kdata->pinst, ((strcmp(toname, "root") == 0) ? "root" : ""));
	strcpy(kdata->prealm, realm);
	return(kuserok(kdata, toname));
}
#endif
