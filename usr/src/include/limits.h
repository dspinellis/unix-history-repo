/*
 * Copyright (c) 1988 The Regents of the University of California.
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
 *	@(#)limits.h	5.1 (Berkeley) %G%
 */

#include <machine/machlimits.h>

#define	ARG_MAX		4096	/* max bytes for an exec function */
#define	CHILD_MAX	6	/* max simultaneous processes */
#define	OPEN_MAX	16	/* max open files per process */
#define	LINK_MAX	8	/* max file link count */
#define	MAX_CANON	255	/* max bytes in terminal canonical input line */
#define	MAX_INPUT	255	/* max bytes in terminal input */
#define	NAME_MAX	14	/* max number of bytes in a file name */
#define	PATH_MAX	255	/* max number of bytes in pathname */
#define	PIPE_BUF	512	/* max number of bytes for atomic pipe writes */
#define	NGROUPS_MAX	0	/* max number of supplemental group id's */
