/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)renice.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <pwd.h>

/*
 * Change the priority (nice) of processes
 * or groups of processes which are already
 * running.
 */
main(argc, argv)
	char **argv;
{
	int which = PRIO_PROCESS;
	int who = 0, prio, errs = 0;

	argc--, argv++;
	if (argc < 2) {
		fprintf(stderr, "usage: renice priority [ [ -p ] pids ] ");
		fprintf(stderr, "[ [ -g ] pgrps ] [ [ -u ] users ]\n");
		exit(1);
	}
	prio = atoi(*argv);
	argc--, argv++;
	if (prio > PRIO_MAX)
		prio = PRIO_MAX;
	if (prio < PRIO_MIN)
		prio = PRIO_MIN;
	for (; argc > 0; argc--, argv++) {
		if (strcmp(*argv, "-g") == 0) {
			which = PRIO_PGRP;
			continue;
		}
		if (strcmp(*argv, "-u") == 0) {
			which = PRIO_USER;
			continue;
		}
		if (strcmp(*argv, "-p") == 0) {
			which = PRIO_PROCESS;
			continue;
		}
		if (which == PRIO_USER) {
			register struct passwd *pwd = getpwnam(*argv);
			
			if (pwd == NULL) {
				fprintf(stderr, "renice: %s: unknown user\n",
					*argv);
				continue;
			}
			who = pwd->pw_uid;
		} else {
			who = atoi(*argv);
			if (who < 0) {
				fprintf(stderr, "renice: %s: bad value\n",
					*argv);
				continue;
			}
		}
		errs += donice(which, who, prio);
	}
	exit(errs != 0);
}

donice(which, who, prio)
	int which, who, prio;
{
	int oldprio;
	extern int errno;

	errno = 0, oldprio = getpriority(which, who);
	if (oldprio == -1 && errno) {
		fprintf(stderr, "renice: %d: ", who);
		perror("getpriority");
		return (1);
	}
	if (setpriority(which, who, prio) < 0) {
		fprintf(stderr, "renice: %d: ", who);
		perror("setpriority");
		return (1);
	}
	printf("%d: old priority %d, new priority %d\n", who, oldprio, prio);
	return (0);
}
