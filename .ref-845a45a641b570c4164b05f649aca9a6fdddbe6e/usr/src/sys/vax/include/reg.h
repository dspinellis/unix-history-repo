/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)reg.h	7.3 (Berkeley) %G%
 */

/*
 * Location of the users' stored
 * registers relative to R0.
 * Usage is u.u_ar0[XX].
 */
#define	R0	(-18)
#define	R1	(-17)
#define	R2	(-16)
#define	R3	(-15)
#define	R4	(-14)
#define	R5	(-13)
#define	R6	(-12)
#define	R7	(-11)
#define	R8	(-10)
#define	R9	(-9)
#define	R10	(-8)
#define	R11	(-7)
#define	R12	(-21)
#define	R13	(-20)

#define AP	(-21)
#define	FP	(-20)
#define	SP	(-5)
#define	PS	(-1)
#define	PC	(-2)

#ifdef IPCREG
#define	NIPCREG 16
int ipcreg[NIPCREG] =
	{R0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,AP,FP,SP,PC};
#endif
