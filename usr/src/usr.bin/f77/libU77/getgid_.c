/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)getgid_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

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
