/*-
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)sys_process.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/errno.h>

/*
 * Process debugging system call.
 */
struct ptrace_args {
	int	req;
	pid_t	pid;
	caddr_t	addr;
	int	data;
};
ptrace(a1, a2, a3)
	struct proc *a1;
	struct ptrace_args *a2;
	int *a3;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}

trace_req(a1)
	struct proc *a1;
{

	/*
	 * Body deleted.
	 */
	return (0);
}
