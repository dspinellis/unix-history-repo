/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfsiom.h	8.1 (Berkeley) %G%
 */

#define MFS_MAPREG	(MAXPHYS/NBPG + 2) /* Kernel mapping pte's */
#define MFS_MAPSIZE	10		   /* Size of alloc map for pte's */
