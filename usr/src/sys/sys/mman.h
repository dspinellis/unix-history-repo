/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mman.h	6.2 (Berkeley) %G%
 */

/* protections are chosen from these bits, or-ed together */
#define	PROT_READ	0x1		/* pages can be read */
#define	PROT_WRITE	0x2		/* pages can be written */
#define	PROT_EXEC	0x4		/* pages can be executed */

/* sharing types: choose either SHARED or PRIVATE */
#define	MAP_SHARED	1		/* share changes */
#define	MAP_PRIVATE	2		/* changes are private */

/* advice to madvise */
#define	MADV_NORMAL	0		/* no further special treatment */
#define	MADV_RANDOM	1		/* expect random page references */
#define	MADV_SEQUENTIAL	2		/* expect sequential page references */
#define	MADV_WILLNEED	3		/* will need these pages */
#define	MADV_DONTNEED	4		/* dont need these pages */
