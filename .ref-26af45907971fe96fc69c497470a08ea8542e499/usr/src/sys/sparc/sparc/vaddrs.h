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
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vaddrs.h	7.3 (Berkeley) %G%
 *
 * from: $Header: vaddrs.h,v 1.3 92/11/26 03:05:11 torek Exp $
 */

/*
 * Special (fixed) virtual addresses on the SPARC.
 *
 * IO virtual space begins at 0xfe000000 (a segment boundary) and
 * continues up to the DMVA edge at 0xff000000.  (The upper all-1s
 * byte is special since some of the hardware supplies this to pad
 * a 24-bit address space out to 32 bits.  This is a legacy of the
 * IBM PC AT bus, actually, just so you know who to blame.)
 *
 * We reserve several pages at the base of our IO virtual space
 * for `oft-used' devices which must be present anyway in order to
 * configure.  In particular, we want the counter-timer register and
 * the Zilog ZSCC serial port chips to be mapped at fixed VAs to make
 * microtime() and the zs hardware interrupt handlers faster.
 *
 * Ideally, we should map the interrupt enable register here as well,
 * but that would require allocating pmegs in locore.s, so instead we
 * use one of the two `wasted' pages at KERNBASE+2*NBPG (see locore.s).
 */

#ifndef IODEV_0
#define	IODEV_0	0xfe000000	/* must match VM_MAX_KERNEL_ADDRESS */

#define	TIMERREG_VA	(IODEV_0 + 0*NBPG)
#define	ZS0_VA		(IODEV_0 + 1*NBPG)
#define	ZS1_VA		(IODEV_0 + 2*NBPG)
#define	AUXREG_VA	(IODEV_0 + 3*NBPG)
#define	IODEV_BASE	(IODEV_0 + 4*NBPG)
#define	IODEV_END	0xff000000		/* 16 MB of iospace */

#define	DVMA_BASE	0xfff00000
#define	DVMA_END	0xfffc0000

#endif /* IODEV_0 */
