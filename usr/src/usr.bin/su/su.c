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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)su.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <syslog.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>

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
	int ch, fulllogin, fastlogin, prio;
	enum { UNSET, YES, NO } iscsh = UNSET;
	char *user, *shell, *username, *cleanenv[2];
	char namebuf[50], shellbuf[MAXPATHLEN];
	char *crypt(), *getpass(), *getenv(), *getlogin(), *rindex(), *strcpy();

	fulllogin = fastlogin = 0;
	while ((ch = getopt(argc, argv, "-fl")) != EOF)
		switch((char)ch) {
		case 'f':
			fastlogin = 1;
			break;
		case '-':
		case 'l':
			fulllogin = 1;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: su [-fl] [login]\n");
			exit(1);
		}
	argv += optind;

	if ((pwd = getpwuid(ruid = getuid())) == NULL) {
		fprintf(stderr, "su: who are you?\n");
		exit(1);
	}
	username = strcpy(namebuf, pwd->pw_name);
	if (!fulllogin)
		if (pwd->pw_shell && *pwd->pw_shell)
			shell = strcpy(shellbuf,  pwd->pw_shell);
		else {
			shell = "/bin/sh";
			iscsh = NO;
		}

	user = *argv ? *argv++ : "root";
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "su: unknown login %s\n", user);
		exit(1);
	}

	/* only allow those in group zero to su to root. */
	if (pwd->pw_uid == 0 && (gr = getgrgid((gid_t)0)))
		for (g = gr->gr_mem;; ++g) {
			if (!*g) {
				fprintf(stderr, "su: you are not in the correct group to su %s.\n", user);
				exit(1);
			}
			if (!strcmp(username, *g))
				break;
		}

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (errno)
		prio = 0;
	(void)setpriority(PRIO_PROCESS, 0, -2);

	if ((p = getlogin()) && *p)
		username = p;

	if (*pwd->pw_passwd && ruid) {
		p = getpass("Password:");
		if (strcmp(pwd->pw_passwd, crypt(p, pwd->pw_passwd))) {
			fprintf(stderr, "Sorry\n");
			if (pwd->pw_uid == 0)
				syslog(LOG_CRIT|LOG_AUTH, "su: BAD SU %s on %s",
				    username, ttyname(2));
			exit(1);
		}
	}

	/* if target user has no password, or fulllogin, use their shell */
	if (!*pwd->pw_passwd || fulllogin)
		if (pwd->pw_shell && *pwd->pw_shell) {
			shell = pwd->pw_shell;
			iscsh = UNSET;
		} else {
			shell = "/bin/sh";
			iscsh = NO;
		}

	if (iscsh == UNSET) {
		if (p = rindex(shell, '/'))
			++p;
		else
			p = shell;
		iscsh = strcmp(p, "csh") ? NO : YES;
	}

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
	if (fulllogin) {
		p = getenv("TERM");
		cleanenv[0] = "PATH=:/usr/ucb:/bin:/usr/bin";
		cleanenv[1] = NULL;
		environ = cleanenv;
		(void)setenv("HOME", pwd->pw_dir, 1);
		(void)setenv("SHELL", shell, 1);
		(void)setenv("TERM", p, 1);
		(void)setenv("USER", pwd->pw_name, 1);
		if (chdir(pwd->pw_dir) < 0) {
			fprintf(stderr, "su: no directory\n");
			exit(1);
		}
	}

	if (fastlogin && iscsh == YES)
		*--argv = "-f";

	/* csh strips the first character... */
	*--argv = fulllogin ? "-su" : iscsh == YES ? "_su" : "su";

	if (pwd->pw_uid == 0)
		syslog(LOG_NOTICE|LOG_AUTH, "su: %s on %s",
		    username, ttyname(2));

	(void)setpriority(PRIO_PROCESS, 0, prio);

	execv(shell, argv);
	fprintf(stderr, "su: no shell.\n");
	exit(1);
}
