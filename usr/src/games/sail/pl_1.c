/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pl_1.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include "player.h"
#include <sys/types.h>
#include <sys/wait.h>

/*
 * If we get here before a ship is chosen, then ms == 0 and
 * we don't want to update the score file, or do any Write's either.
 * We can assume the sync file is already created and may need
 * to be removed.
 * Of course, we don't do any more Sync()'s if we got here
 * because of a Sync() failure.
 */
leave(conditions)
int conditions;
{
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGALRM, SIG_IGN);
	(void) signal(SIGCHLD, SIG_IGN);

	if (done_curses) {
		Signal("It looks like you've had it!",
			(struct ship *)0);
		switch (conditions) {
		case LEAVE_QUIT:
			break;
		case LEAVE_CAPTURED:
			Signal("Your ship was captured.",
				(struct ship *)0);
			break;
		case LEAVE_HURRICAN:
			Signal("Hurricane!  All ships destroyed.",
				(struct ship *)0);
			break;
		case LEAVE_DRIVER:
			Signal("The driver died.", (struct ship *)0);
			break;
		case LEAVE_SYNC:
			Signal("Synchronization error.", (struct ship *)0);
			break;
		default:
			Signal("A funny thing happened (%d).",
				(struct ship *)0, conditions);
		}
	} else {
		switch (conditions) {
		case LEAVE_QUIT:
			break;
		case LEAVE_DRIVER:
			printf("The driver died.\n");
			break;
		case LEAVE_FORK:
			perror("fork");
			break;
		case LEAVE_SYNC:
			printf("Synchronization error\n.");
			break;
		default:
			printf("A funny thing happened (%d).\n",
				conditions);
		}
	}

	if (ms != 0) {
		log(ms);
		if (conditions != LEAVE_SYNC) {
			makesignal(ms, "Captain %s relinquishing.",
				(struct ship *)0, mf->captain);
			Write(W_END, ms, 0, 0, 0, 0, 0);
			(void) Sync();
		}
	}
	sync_close(!hasdriver);
	cleanupscreen();
	exit(0);
}

choke()
{
	leave(LEAVE_QUIT);
}

child()
{
	union wait status;
	int pid;

	(void) signal(SIGCHLD, SIG_IGN);
	do {
		pid = wait3(&status, WNOHANG, (struct rusage *)0);
		if (pid < 0 || pid > 0 && !WIFSTOPPED(status))
			hasdriver = 0;
	} while (pid > 0);
	(void) signal(SIGCHLD, child);
}
