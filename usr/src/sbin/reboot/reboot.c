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
static char sccsid[] = "@(#)reboot.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Reboot
 */
#include <stdio.h>
#include <sys/reboot.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>

#define SHUTDOWNLOG "/usr/adm/shutdownlog"

main(argc, argv)
	int argc;
	char **argv;
{
	int howto;
	register char *argp;
	register i;
	register ok = 0;
	register qflag = 0;

	argc--, argv++;
	howto = 0;
	while (argc > 0) {
		if (!strcmp(*argv, "-q"))
			qflag++;
		else if (!strcmp(*argv, "-n"))
			howto |= RB_NOSYNC;
		else {
			fprintf(stderr,
			    "usage: reboot [ -n ][ -q ]\n");
			exit(1);
		}
		argc--, argv++;
	}

	signal(SIGHUP, SIG_IGN);	/* for remote connections */
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
		log_entry();
	if (!qflag) {
		if (!(howto & RB_NOSYNC)) {
			markdown();
			sync();
			sync();
		}
		setalarm(5);
		pause();
	}
	syscall(55, howto);
	perror("reboot");
	kill(1, SIGHUP);
	exit(1);
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

char *days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	"Oct", "Nov", "Dec"
};

log_entry()
{
	FILE *fp;
	struct tm *tm, *localtime();
	time_t now;

	time(&now);
	tm = localtime(&now);
	fp = fopen(SHUTDOWNLOG, "a");
	if (fp == 0)
		return;
	fseek(fp, 0L, 2);
	fprintf(fp, "%02d:%02d  %s %s %2d, %4d.  Halted for reboot.\n", tm->tm_hour,
		tm->tm_min, days[tm->tm_wday], months[tm->tm_mon],
		tm->tm_mday, tm->tm_year + 1900);
	fclose(fp);
}
