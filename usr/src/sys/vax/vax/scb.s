/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)scb.s	6.3 (Berkeley) %G%
 */

#include "uba.h"

/*
 * System control block
 */
	.set	INTSTK,1	# handle this interrupt on the interrupt stack
	.set	HALT,3		# halt if this interrupt occurs

_scb:	.globl	_scb

#define	STRAY	.long	_Xstray+INTSTK
#define	STRAY8	STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY
#define	STRAY15	STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY8
#define	KS(a)	.long	_X/**/a
#define	IS(a)	.long	_X/**/a+INTSTK
#define	STOP(a)	.long	_X/**/a+HALT

/* 000 */	STRAY;		IS(machcheck);	IS(kspnotval);	STOP(powfail);
/* 010 */	KS(privinflt);	KS(xfcflt);	KS(resopflt);	KS(resadflt);
/* 020 */	KS(protflt);	KS(transflt);	KS(tracep);	KS(bptflt);
/* 030 */	KS(compatflt);	KS(arithtrap);	STRAY;		STRAY;
/* 040 */	KS(syscall);	KS(chme);	KS(chms);	KS(chmu);
/* 050 */	STRAY;		IS(cmrd);	STRAY;		STRAY;
/* 060 */	IS(wtime);	STRAY;		STRAY;		STRAY;
/* 070 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 080 */	STRAY;		STRAY;		KS(astflt);	STRAY;
/* 090 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 0a0 */	IS(softclock);	STRAY;		STRAY;		STRAY;
/* 0b0 */	IS(netintr);	STRAY;		STRAY;		STRAY;
/* 0c0 */	IS(hardclock);	STRAY;		STRAY;		STRAY;
/* 0d0 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 0e0 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 0f0 */	IS(consdin);	IS(consdout);	IS(cnrint);	IS(cnxint);
/* 100 */	IS(nexzvec); STRAY15;		/* ipl 0x14, nexus 0-15 */
/* 140 */	IS(nexzvec); STRAY15;		/* ipl 0x15, nexus 0-15 */
/* 180 */	IS(nexzvec); STRAY15;		/* ipl 0x16, nexus 0-15 */
/* 1c0 */	IS(nexzvec); STRAY15;		/* ipl 0x17, nexus 0-15 */

	.globl	_UNIvec
_UNIvec:	.space	512		# 750 unibus intr vector
					# 1st UBA jump table on 780's
#if NUBA > 1
	.globl	_UNI1vec
_UNI1vec:	.space	512		# 750 second unibus intr vector
					# 2nd UBA jump table on 780's
#endif

/*
 * The following specify offsets into the intrcnt array for SCB interrupt
 * vectors.  Unibus devices will allocate slots after these.
 */
#define	I_CLOCK		(4 * 0)		/* interval timer */
#define	I_CNR		(4 * 1)		/* console receive */
#define	I_CNX		(4 * 2)		/* console transmit */
#define	I_TUR		(4 * 3)		/* console TU58 receive */
#define	I_TUX		(4 * 4)		/* console TU58 transmit */
#define	I_MBA0		(4 * 5)		/* massbus adaptor 0 */
#define	I_MBA1		(4 * 6)		/* massbus adaptor 1 */
#define	I_MBA2		(4 * 7)		/* massbus adaptor 2 */
#define	I_MBA3		(4 * 8)		/* massbus adaptor 3 */
#define	I_UBA		(4 * 9)		/* unibus adaptor base */
#define	I_UBA0		(4 * 9)		/* unibus adaptor 0 */
#define	I_UBA1		(4 * 10)	/* unibus adaptor 1 */
#define	I_UBA2		(4 * 11)	/* unibus adaptor 2 */
#define	I_UBA3		(4 * 12)	/* unibus adaptor 3 */
