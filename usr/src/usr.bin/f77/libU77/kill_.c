/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kill_.c	5.1	%G%
 */

/*
 * send a signal to a process
 *
 * calling sequence:
 *	ierror = kill(pid, signum)
 * where:
 *	pid must be the process id of one of the user's processes
 *	signum must be a valid signal number (see signal(2))
 *	ierror will be 0 if successful; an error code otherwise.
 */

#include "../libI77/f_errno.h"

long kill_(pid, signum)
long *pid, *signum;
{
	if (*pid < 0 || *pid > 32767L || *signum < 1 || *signum > 16)
		return((long)(errno=F_ERARG));
	if (kill((int)*pid, (int)*signum) != 0)
		return((long)errno);
	return(0L);
}
