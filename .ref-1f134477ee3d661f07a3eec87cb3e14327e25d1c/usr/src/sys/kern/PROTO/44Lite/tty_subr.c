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
 *	from: @(#)tty_subr.c	8.2 (Berkeley) 9/5/93
 */

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/tty.h>

char cwaiting;
struct cblock *cfree, *cfreelist;
int cfreecount, nclist;

void
clist_init()
{

	/*
	 * Body deleted.
	 */
	return;
}

getc(a1)
	struct clist *a1;
{

	/*
	 * Body deleted.
	 */
	return ((char)0);
}

q_to_b(a1, a2, a3)
	struct clist *a1;
	char *a2;
	int a3;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

ndqb(a1, a2)
	struct clist *a1;
	int a2;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

void
ndflush(a1, a2)
	struct clist *a1;
	int a2;
{

	/*
	 * Body deleted.
	 */
	return;
}

putc(a1, a2)
	char a1;
	struct clist *a2;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

b_to_q(a1, a2, a3)
	char *a1;
	int a2;
	struct clist *a3;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

char *
nextc(a1, a2, a3)
	struct clist *a1;
	char *a2;
	int *a3;
{

	/*
	 * Body deleted.
	 */
	return ((char *)0);
}

unputc(a1)
	struct clist *a1;
{

	/*
	 * Body deleted.
	 */
	return ((char)0);
}

void
catq(a1, a2)
	struct clist *a1, *a2;
{

	/*
	 * Body deleted.
	 */
	return;
}
