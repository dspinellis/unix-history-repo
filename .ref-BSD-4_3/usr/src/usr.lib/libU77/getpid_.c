/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)getpid_.c	5.1	6/7/85
 */

/*
 * get process id
 *
 * calling sequence:
 *	integer getpid, pid
 *	pid = getpid()
 * where:
 *	pid will be the current process id
 */

long getpid_()
{
	return((long)getpid());
}
