/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)getuid_.c	5.1	%G%
 */

/*
 * get user id
 *
 * calling sequence:
 *	integer getuid, uid
 *	uid = getuid()
 * where:
 *	uid will be the real user id
 */

long getuid_()
{
	return((long)getuid());
}
