/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dkstat.h	8.1 (Berkeley) %G%
 */

#define	CP_USER		0
#define	CP_NICE		1
#define	CP_SYS		2
#define	CP_INTR		3
#define	CP_IDLE		4
#define	CPUSTATES	5

#define	DK_NDRIVE	8
#ifdef KERNEL
long cp_time[CPUSTATES];
long dk_seek[DK_NDRIVE];
long dk_time[DK_NDRIVE];
long dk_wds[DK_NDRIVE];
long dk_wpms[DK_NDRIVE];
long dk_xfer[DK_NDRIVE];

int dk_busy;
int dk_ndrive;

long tk_cancc;
long tk_nin;
long tk_nout;
long tk_rawcc;
#endif
