/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)syslimits.h	7.3 (Berkeley) %G%
 */

#define	ARG_MAX		20480	/* max bytes for an exec function */
#define	CHILD_MAX	40	/* max simultaneous processes */
#define	LINK_MAX	32767	/* max file link count */
#define	MAX_CANON	255	/* max bytes in terminal canonical input line */
#define	MAX_INPUT	255	/* max bytes in terminal input */
#define	NAME_MAX	255	/* max number of bytes in a file name */
#define	NGROUPS_MAX	16	/* max number of supplemental group id's */
#define	OPEN_MAX	64	/* max open files per process */
#define	PATH_MAX	1024	/* max number of bytes in pathname */
#define	PIPE_BUF	512	/* max number of bytes for atomic pipe writes */
