/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: reg.h 1.1 90/07/09$
 *
 *	@(#)reg.h	8.2 (Berkeley) %G%
 */

/*
 * Location of the users' stored
 * registers relative to D0.
 * Usage is u.u_ar0[XX].
 */
#define	D0	(0)
#define	D1	(1)
#define	D2	(2)
#define	D3	(3)
#define	D4	(4)
#define	D5	(5)
#define	D6	(6)
#define	D7	(7)
#define	A0	(8)
#define	A1	(9)
#define	A2	(10)
#define	A3	(11)
#define	A4	(12)
#define	A5	(13)
#define	A6	(14)
#define	A7	(15)

#define	SP	A7
#define	PC	(17)
#define	PS	(16)

#ifdef IPCREG
#define	NIPCREG 16
int ipcreg[NIPCREG] =
	{D0,D1,D2,D3,D4,D5,D6,D7,A0,A1,A2,A3,A4,A5,A6,A7};
#endif

/*
 * Register set accessible via /proc/$pid/reg
 */
struct reg {
        int     r_regs[16];	/* numbered as above */
	int	r_pc;
	int	r_sr;
};


#ifdef KERNEL
/*
 * Due to a mental lapse somewhere down the line, wait returns its values
 * in strange registers.  Kludge it up here so we don't have to in the
 * machine-independent code.
 */
#define	R0	D1
#define	R1	A0
#endif
