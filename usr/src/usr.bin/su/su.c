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
static char sccsid[] = "@(#)su.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
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
	char *cleanenv[2];
	register struct passwd *pwd;
	register char *p, **g;
	struct group *gr;
	int ch, fulllogin, fastlogin, prio;
	char *user, *shell, *username, nbuf[50];
	char *crypt(), *getpass(), *getenv(), *getlogin(), *strcpy();

	user = "root";
	shell = "/bin/sh";
	fulllogin = fastlogin = 0;

	while ((ch = getopt(argc, argv, "-fl")) != EOF)
		switch((char)ch) {
		case '-':
		case 'l':
			fulllogin = 1;
			break;
		case 'f':
			fastlogin = 1;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: su [-fl] [login]\n");
			exit(1);
		}
	argv += optind;

	if ((pwd = getpwuid(getuid())) == NULL) {
		fprintf(stderr, "su: who are you?\n");
		exit(1);
	}
	username = strcpy(nbuf, pwd->pw_name);

	if (*argv)
		user = *argv++;
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "su: unknown login %s\n", user);
		exit(1);
	}

	/* only allow those in group zero to su to root. */
	if (pwd->pw_uid == 0 && (gr = getgrgid(0)))
		for (g = gr->gr_mem;; ++g) {
			if (!*g) {
				fprintf(stderr, "su: you are not in the correct group to su %s.\n", user);
				exit(1);
			}
			if (!strcmp(username, *g))
				break;
		}

	openlog("su", LOG_ODELAY, LOG_AUTH);

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (errno)
		prio = 0;
	(void)setpriority(PRIO_PROCESS, 0, -2);

	if ((p = getlogin()) && *p)
		username = p;

	if (pwd->pw_passwd[0] && getuid()) {
		p = getpass("Password:");
		if (strcmp(pwd->pw_passwd, crypt(p, pwd->pw_passwd))) {
			fprintf(stderr, "Sorry\n");
			if (pwd->pw_uid == 0)
				syslog(LOG_CRIT, "BAD SU %s on %s",
					username, ttyname(2));
			exit(1);
		}
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
	if (pwd->pw_shell && *pwd->pw_shell)
		shell = pwd->pw_shell;

	if (fulllogin) {
		p = getenv("TERM");
		cleanenv[0] = "PATH=:/usr/ucb:/bin:/usr/bin";
		cleanenv[1] = NULL;
		environ = cleanenv;
		(void)setenv("TERM", p, 1);
		if (chdir(pwd->pw_dir) < 0) {
			fprintf(stderr, "su: no directory\n");
			exit(1);
		}
		(void)setenv("USER", pwd->pw_name, 1);
	}
	else if (strcmp(user, "root"))
		(void)setenv("USER", pwd->pw_name, 1);
	(void)setenv("SHELL", shell, 1);
	(void)setenv("HOME", pwd->pw_dir, 1);

	--argv;
	if (fastlogin)
		*argv-- = "-f";
	*argv = fulllogin ? "-su" : "su";

	if (pwd->pw_uid == 0)
		syslog(LOG_NOTICE, "%s on %s", username, ttyname(2));

	(void)setpriority(PRIO_PROCESS, 0, prio);

	execv(shell, argv);
	fprintf(stderr, "su: no shell\n");
	exit(1);
}
