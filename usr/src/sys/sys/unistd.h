/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)unistd.h	5.5 (Berkeley) %G%
 */

/* compile-time symbolic constants */
#define	POSIX_JOB_CONTROL	/* implementation supports job control */
#define	POSIX_SAVED_IDS		/* saved set-user-ID and set-group-ID */
#define	POSIX_VERSION		198808L

/* execution-time symbolic constants */
#define	POSIX_CHOWN_RESTRICTED	/* chown requires appropriate privileges */
#define	POSIX_NO_TRUNC		/* too-long path components generate errors */
#define	POSIX_VDISABLE		/* may disable terminal special characters */

/* access function */
#define	F_OK		0	/* test for existence of file */
#define	X_OK		0x01	/* test for execute or search permission */
#define	W_OK		0x02	/* test for write permission */
#define	R_OK		0x04	/* test for read permission */

/* lseek function */
#define	SEEK_SET	0	/* set file offset to offset */
#define	SEEK_CUR	1	/* set file offset to current plus offset */
#define	SEEK_END	2	/* set file offset to EOF plus offset */

/* map a stream pointer to a file descriptor */
#define	STDIN_FILENO	0	/* standard input value, stdin */
#define	STDOUT_FILENO	1	/* standard output value, stdout */
#define	STDERR_FILENO	2	/* standard error value, stdout */

/* fnmatch function */
#define	FNM_PATHNAME	0x01	/* match pathnames, not filenames */
#ifndef _POSIX_SOURCE
#define	FNM_QUOTE	0x02	/* escape special chars with \ */
#endif

#ifndef NULL
#define	NULL		0	/* null pointer constant */
#endif

/* configurable pathname variables */
#define	_PC_LINK_MAX		1
#define	_PC_MAX_CANON		2
#define	_PC_MAX_INPUT		3
#define	_PC_NAME_MAX		4
#define	_PC_PATH_MAX		5
#define	_PC_PIPE_BUF		6
#define	_PC_CHOWN_RESTRICTED	7
#define	_PC_NO_TRUNC		8
#define	_PC_VDISABLE		9

/* configurable system variables */
#define	_SC_ARG_MAX		1
#define	_SC_CHILD_MAX		2
#define	_SC_CLK_TCK		3
#define	_SC_NGROUPS_MAX		4
#define	_SC_OPEN_MAX		5
#define	_SC_JOB_CONTROL		6
#define	_SC_SAVED_IDS		7
#define	_SC_VERSION		8
