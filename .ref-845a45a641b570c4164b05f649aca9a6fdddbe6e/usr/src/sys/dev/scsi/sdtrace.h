/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sdtrace.h	5.3 (Berkeley) %G%
 *
 * from: $Header: sdtrace.h,v 1.6 92/12/02 03:53:47 torek Exp $ (LBL)
 */

/*
 * SCSI disk command tracing
 */

#if defined(SDTRACE) || !defined(KERNEL)
struct sdtrace {
	struct	timeval time;	/* timestamp */
	u_int	block;		/* disk block */
	u_int	bcount;		/* # bytes transferred */
	u_char	tcode;		/* trace code */
	u_char	target;		/* target number */
	u_char	unit;		/* unit number on target */
	u_char	read;		/* read operation */
};

#define	T_START		0x01
#define	T_MKCDB		0x02
#define	T_INTR		0x03
#endif

#ifdef SDTRACE
/* Allow kernel config to override number of entries */
#ifndef NSDOPBUF
#define	NSDOPBUF 1024
#endif

struct	sdtrace sdopbuf[NSDOPBUF];
struct	sdtrace *sdopptr = sdopbuf;
int	nsdopbuf = NSDOPBUF;	/* for sdtrace */
u_long	sdopcnt;

#define	SD_TRACE(code, sc, bp) { \
	if (++sdopptr >= &sdopbuf[NSDOPBUF]) \
		sdopptr = sdopbuf; \
	microtime(&sdopptr->time); \
	sdopptr->tcode = code; \
	sdopptr->read = bp->b_flags & B_READ; \
	sdopptr->block = bp->b_blkno; \
	sdopptr->bcount = bp->b_bcount; \
	sdopptr->target = sc->sc_unit.u_targ; \
	sdopptr->unit = sc->sc_unit.u_unit; \
	++sdopcnt; \
}
#else
#define	SD_TRACE(code, sc, bp) { }
#endif
