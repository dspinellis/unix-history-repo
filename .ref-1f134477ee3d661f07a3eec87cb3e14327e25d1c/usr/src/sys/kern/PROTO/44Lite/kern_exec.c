/*-
 * Copyright (c) 1982, 1986, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)kern_exec.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/errno.h>
#include <sys/proc.h>

/*
 * exec system call
 */
struct execve_args {
	char	*fname;
	char	**argp;
	char	**envp;
};
/* ARGSUSED */
execve(a1, a2, a3)
	struct proc *a1;
	struct execve_args *a2;
	int *a3;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}
