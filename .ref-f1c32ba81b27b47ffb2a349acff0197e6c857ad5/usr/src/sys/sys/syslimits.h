/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)syslimits.h	7.4 (Berkeley) %G%
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

#define	BC_BASE_MAX	99	/* max ibase/obase values allowed by bc(1) */
#define	BC_DIM_MAX	2048	/* max array elements allowed by bc(1) */
#define	BC_SCALE_MAX	99	/* max scale value allowed by bc(1) */
#define	BC_STRING_MAX	1000	/* max const string length allowed by bc(1) */
#define	EQUIV_CLASS_MAX	2	/* max weights for order keyword; see locale */
#define	EXPR_NEST_MAX	32	/* max expressions nested in expr(1) */
#define	LINE_MAX	2048	/* max length in bytes of an input line */
#define	RE_DUP_MAX	255	/* max repeated RE's using interval notation */
