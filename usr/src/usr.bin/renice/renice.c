#ifndef lint
static	char *sccsid = "@(#)renice.c	4.1 (Berkeley) 83/03/19";
#endif

#include <time.h>
#include <resource.h>
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
	int who, id, prio, oldprio, errs = 0;
	extern int errno;

	argc--, argv++;
	if (argc < 2)
		usage();
	if (strcmp(*argv, "-g") == 0) {
		which = PRIO_PGRP;
		argv++, argc--;
	} else if (strcmp(*argv, "-u") == 0) {
		which = PRIO_USER;
		argv++, argc--;
	}
	prio = atoi(*argv);
	argc--, argv++;
	if (prio > PRIO_MAX)
		prio = PRIO_MAX;
	if (prio < PRIO_MIN)
		prio = PRIO_MIN;
	if (argc == 0)
		usage();
	for (; argc > 0; argc--, argv++) {
		if (which == PRIO_USER) {
			register struct passwd *pwd = getpwnam(*argv);
			
			if (pwd == NULL) {
				fprintf(stderr, "renice: %s: unknown user\n",
					*argv);
				continue;
			}
			id = pwd->pw_uid;
		} else {
			id = atoi(*argv);
			if (id < 0) {
				fprintf(stderr, "renice: %s: bad value\n",
					*argv);
				continue;
			}
		}
		oldprio = getpriority(which, who);
		if (oldprio == -1 && errno) {
			fprintf(stderr, "renice: ");
			perror(*argv);
			errs++;
			continue;
		}
		if (setpriority(which, who, prio) < 0) {
			fprintf(stderr, "renice: ");
			perror(*argv);
			errs++;
			continue;
		}
		printf("%s: old priority %d, new priority %d\n", *argv,
			oldprio, prio);
	}
	exit(errs != 0);
}

usage()
{
	fprintf(stderr, "usage: renice priority pid ....\n");
	fprintf(stderr, "or, renice -g priority pgrp ....\n");
	fprintf(stderr, "or, renice -u priority user ....\n");
	exit(1);
}
