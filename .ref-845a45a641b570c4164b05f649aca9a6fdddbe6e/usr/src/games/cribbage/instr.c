/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)instr.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/errno.h>
#include <sys/stat.h>

#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "deck.h"
#include "cribbage.h"
#include "pathnames.h"

void
instructions()
{
	extern int errno;
	struct stat sb;
	union wait pstat;
	pid_t pid;
	char *pager, *path;

	if (stat(_PATH_INSTR, &sb)) {
		(void)fprintf(stderr, "cribbage: %s: %s.\n", _PATH_INSTR,
		    strerror(errno));
		exit(1);
	}
	switch (pid = vfork()) {
	case -1:
		(void)fprintf(stderr, "cribbage: %s.\n", strerror(errno));
		exit(1);
	case 0:
		if (!(path = getenv("PAGER")))
			path = _PATH_MORE;
		if (pager = rindex(path, '/'))
			++pager;
		pager = path;
		execlp(path, pager, _PATH_INSTR, (char *)NULL);
		(void)fprintf(stderr, "cribbage: %s.\n", strerror(errno));
		_exit(1);
	default:
		do {
			pid = waitpid(pid, (int *)&pstat, 0);
		} while (pid == -1 && errno == EINTR);
		if (pid == -1 || pstat.w_status)
			exit(1);
	}
}
