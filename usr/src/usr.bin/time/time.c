#ifndef lint
static	char *sccsid = "@(#)time.c	4.7 (Berkeley) %G%";
#endif

/*
 * time
 */
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int status, lflag = 0;
	register int p;
	struct timeval before, after;
	struct rusage ru;

	if (argc<=1)
		exit(0);
	if (strcmp(argv[1], "-l") == 0) {
		lflag++;
		argc--;
		argv++;
	}
	gettimeofday(&before, (struct timezone *)NULL);
	p = fork();
	if (p < 0) {
		perror("time");
		exit(1);
	}
	if (p == 0) {
		execvp(argv[1], &argv[1]);
		perror(argv[1]);
		exit(1);
	}
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	while (wait3(&status, 0, &ru) != p)
		;
	gettimeofday(&after, (struct timezone *)NULL);
	if ((status&0377) != 0)
		fprintf(stderr, "Command terminated abnormally.\n");
	after.tv_sec -= before.tv_sec;
	after.tv_usec -= before.tv_usec;
	if (after.tv_usec < 0)
		after.tv_sec--, after.tv_usec += 1000000;
	printt("real", &after);
	printt("user", &ru.ru_utime);
	printt("sys ", &ru.ru_stime);
	fprintf(stderr, "\n");
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

printt(s, tv)
	char *s;
	struct timeval *tv;
{

	fprintf(stderr, "%9ld.%02ld %s ", tv->tv_sec, tv->tv_usec/10000, s);
}
