/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)getgid_.c	5.1	6/7/85
 */

/*
 * get group id
 *
 * calling sequence:
 *	integer getgid, gid
 *	gid = getgid()
 * where:
 *	gid will be the real group id
 */

long getgid_()
{
	return((long)getgid());
}
