/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mem.h	7.2 (Berkeley) %G%
 */

/*
 * Compute maximum possible number of memory controllers,
 * for sizing of the mcraddr array.
 */
#if VAX8200 || VAX780
#define	MAXNMCR		4
#else
#define	MAXNMCR		1
#endif

#if VAX780
/* controller types */
#define	M780C	1
#define	M780EL	2
#define	M780EU	3
#endif

#define	MEMINTVL	(60*10)		/* 10 minutes */

#ifdef	KERNEL
int	nmcr;
caddr_t	mcraddr[MAXNMCR];
#if VAX780
int	mcrtype[MAXNMCR];
#endif
#endif
