/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	8.3 (Berkeley) %G%
 */

#include <machine/exec.h>

/*
 * The following structure is found at the top of the user stack of each
 * user process. The ps program uses it to locate argv and environment
 * strings. Programs that wish ps to display other information may modify
 * it; normally ps_argvstr points to the text for argv[0], and ps_nargvstr
 * is the same as the program's argc. The fields ps_envstr and ps_nenvstr
 * are the equivalent for the environment.
 */
struct ps_strings {
	char	*ps_argvstr;	/* first of 0 or more argument strings */
	int	ps_nargvstr;	/* the number of argument strings */
	char	*ps_envstr;	/* first of 0 or more environment strings */
	int	ps_nenvstr;	/* the number of environment strings */
};

/*
 * Address of ps_strings structure (in user space).
 */
#define	PS_STRINGS \
	((struct ps_strings *)(USRSTACK - sizeof(struct ps_strings)))

/*
 * Arguments to the exec system call.
 */
struct execve_args {
	char	*fname;
	char	**argp;
	char	**envp;
};
