/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)subr_rmap.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/map.h>
#include <sys/proc.h>

void
rminit(a1, a2, a3, a4, a5)
	struct map *a1;
	long a2, a3;
	char *a4;
	int a5;
{

	/*
	 * Body deleted.
	 */
	return;
}

long
rmalloc(a1, a2)
	struct map *a1;
	long a2;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

void
rmfree(a1, a2, a3)
	struct map *a1;
	long a2, a3;
{

	/*
	 * Body deleted.
	 */
	return;
}
