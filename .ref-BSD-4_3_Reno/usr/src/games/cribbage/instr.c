/*-
 * Copyright (c) 1990 The Regents of the University of California.
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
static char sccsid[] = "@(#)instr.c	5.1 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include "pathnames.h"

instructions()
{
	extern int errno;
	struct stat sb;
	union wait pstat;
	pid_t pid, waitpid();
	char *pager, *path, *getenv(), *rindex(), *strerror();

	if (stat(_PATH_INSTR, &sb)) {
		(void)fprintf(stderr, "cribbage: %s: %s.\n", _PATH_INSTR,
		    strerror(errno));
		exit(1);
	}
	switch(pid = vfork()) {
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
			pid = waitpid(pid, &pstat, 0);
		} while (pid == -1 && errno == EINTR);
		if (pid == -1 || pstat.w_status)
			exit(1);
	}
}
