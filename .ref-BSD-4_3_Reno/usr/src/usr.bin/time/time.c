/*
 * Copyright (c) 1987, 1988 The Regents of the University of California.
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
"@(#) Copyright (c) 1987, 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)time.c	4.9 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/signal.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int pid;
	int ch, status, lflag;
	struct timeval before, after;
	struct rusage ru;

	lflag = 0;
	while ((ch = getopt(argc, argv, "l")) != EOF)
		switch((char)ch) {
		case 'l':
			lflag = 1;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: time [-l] command.\n");
			exit(1);
		}

	if (!(argc -= optind))
		exit(0);
	argv += optind;

	gettimeofday(&before, (struct timezone *)NULL);
	switch(pid = vfork()) {
	case -1:			/* error */
		perror("time");
		exit(1);
		/* NOTREACHED */
	case 0:				/* child */
		execvp(*argv, argv);
		perror(*argv);
		_exit(1);
		/* NOTREACHED */
	}
	/* parent */
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	while (wait3(&status, 0, &ru) != pid);		/* XXX use waitpid */
	gettimeofday(&after, (struct timezone *)NULL);
	if (status&0377)
		fprintf(stderr, "Command terminated abnormally.\n");
	after.tv_sec -= before.tv_sec;
	after.tv_usec -= before.tv_usec;
	if (after.tv_usec < 0)
		after.tv_sec--, after.tv_usec += 1000000;
	fprintf(stderr, "%9ld.%02ld real ", after.tv_sec, after.tv_usec/10000);
	fprintf(stderr, "%9ld.%02ld user ",
	    ru.ru_utime.tv_sec, ru.ru_utime.tv_usec/10000);
	fprintf(stderr, "%9ld.%02ld sys\n",
	    ru.ru_stime.tv_sec, ru.ru_stime.tv_usec/10000);
	if (lflag) {
		int hz = 100;			/* XXX */
		long ticks;

		ticks = hz * (ru.ru_utime.tv_sec + ru.ru_stime.tv_sec) +
		     hz * (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) / 1000000;
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_maxrss, "maximum resident set size");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_ixrss / ticks, "average shared memory size");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_idrss / ticks, "average unshared data size");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_isrss / ticks, "average unshared stack size");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_minflt, "page reclaims");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_majflt, "page faults");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_nswap, "swaps");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_inblock, "block input operations");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_oublock, "block output operations");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_msgsnd, "messages sent");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_msgrcv, "messages received");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_nsignals, "signals received");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_nvcsw, "voluntary context switches");
		fprintf(stderr, "%10ld  %s\n",
			ru.ru_nivcsw, "involuntary context switches");
	}
	exit (status>>8);
}
