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
static char sccsid[] = "@(#)halt.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Halt
 */
#include <stdio.h>
#include <sys/reboot.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <signal.h>
#include <syslog.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int howto;
	char *ttyn = (char *)ttyname(2);
	register i;
	register qflag = 0;

	openlog("halt", 0, LOG_AUTH);
	howto = RB_HALT;
	argc--, argv++;
	while (argc > 0) {
		if (!strcmp(*argv, "-n"))
			howto |= RB_NOSYNC;
		else if (!strcmp(*argv, "-y"))
			ttyn = 0;
		else if (!strcmp(*argv, "-q"))
			qflag++;
		else {
			fprintf(stderr, "usage: halt [ -n ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (ttyn && *(ttyn+strlen("/dev/tty")) == 'd') {
		fprintf(stderr, "halt: dangerous on a dialup; use ``halt -y'' if you are really sure\n");
		exit(1);
	}

	signal(SIGHUP, SIG_IGN);		/* for network connections */
	if (kill(1, SIGTSTP) == -1) {
		fprintf(stderr, "reboot: can't idle init\n");
		exit(1);
	}

	if (!qflag) for (i = 1; ; i++) {
		if (kill(-1, SIGKILL) == -1) {
			extern int errno;

			if (errno == ESRCH)
				break;

			perror("reboot: kill");
			kill(1, SIGHUP);
			exit(1);
		}
		if (i > 5) {
	fprintf(stderr, "CAUTION: some process(es) wouldn't die\n");
			break;
		}
		setalarm(2 * i);
		pause();
	}

	if ((howto & RB_NOSYNC) == 0)
		syslog(LOG_CRIT, "halted");
	if (!qflag) {
		if ((howto & RB_NOSYNC)==0) {
			markdown();
			sync();
			sync();
		}
		setalarm(5);
		pause();
	}
	syscall(55, howto);
	perror("reboot");
}

dingdong()
{
	/* RRRIIINNNGGG RRRIIINNNGGG */
}

setalarm(n)
{
	signal(SIGALRM, dingdong);
	alarm(n);
}

#include <utmp.h>
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
char	wtmpf[]	= "/usr/adm/wtmp";
struct utmp wtmp;

markdown()
{
	register f = open(wtmpf, 1);
	if (f >= 0) {
		lseek(f, 0L, 2);
		SCPYN(wtmp.ut_line, "~");
		SCPYN(wtmp.ut_name, "shutdown");
		SCPYN(wtmp.ut_host, "");
		time(&wtmp.ut_time);
		write(f, (char *)&wtmp, sizeof(wtmp));
		close(f);
	}
}
