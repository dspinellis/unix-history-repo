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
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)renice.c	5.2 (Berkeley) %G%";
#endif /* not lint */

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
