/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)scb.s	7.4 (Berkeley) %G%
 */

#include "uba.h"

/*
 * System control block
 */
	.set	INTSTK,1	# handle this interrupt on the interrupt stack
	.set	HALT,3		# halt if this interrupt occurs

_scb:	.globl	_scb

#define	KS(a)	.long	_X/**/a
#define	IS(a)	.long	_X/**/a+INTSTK
#define	STOP(a)	.long	_X/**/a+HALT

#define	STRAY(x)	.long	_scbstray+2*(x)+INTSTK
#define	STRAY3(n)	STRAY(n);STRAY(n+4);STRAY(n+8)
#define	STRAY4(n)	STRAY3(n);STRAY(n+12)
#define	STRAY15(n)	STRAY4(n);STRAY4(n+16);STRAY4(n+32);STRAY3(n+48)
#define	STRAY16(n)	STRAY15(n);STRAY(n+60)
#define	NEX0(n)	IS(nex0zvec);STRAY15(n+4)
#define	NEX1(n)	IS(nex1zvec);STRAY15(n+4)

/* 000 */	IS(passiverel);	IS(machcheck);	IS(kspnotval);	STOP(powfail);
/* 010 */	KS(privinflt);	KS(xfcflt);	KS(resopflt);	KS(resadflt);
/* 020 */	KS(protflt);	KS(transflt);	KS(tracep);	KS(bptflt);
/* 030 */	KS(compatflt);	KS(arithtrap);	STRAY(0x38);	STRAY(0x3c);
/* 040 */	KS(syscall);	KS(chme);	KS(chms);	KS(chmu);
/* 050 */	IS(sbisilo);	IS(cmrd);	IS(sbi0alert);	IS(sbi0fault);
/* 060 */	IS(wtime);	IS(sbi0fail);	STRAY(0x68);	STRAY(0x6c);
/* 070 */	STRAY(0x70);	STRAY(0x74);	STRAY(0x78);	STRAY(0x7c);
/* 080 */	STRAY(0x80);	STRAY(0x84);	KS(astflt);	STRAY(0x8c);
/* 090 */	STRAY(0x90);	STRAY(0x94);	STRAY(0x98);	STRAY(0x9c);
/* 0a0 */	IS(softclock);	STRAY(0xa4);	STRAY(0xa8);	STRAY(0xac);
/* 0b0 */	IS(netintr);	STRAY(0xb4);	STRAY(0xb8);	IS(kdbintr);
/* 0c0 */	IS(hardclock);	STRAY(0xc4);	KS(emulate);	KS(emulateFPD);
/* 0d0 */	STRAY(0xd0);	STRAY(0xd4);	STRAY(0xd8);	STRAY(0xdc);
/* 0e0 */	STRAY(0xe0);	STRAY(0xe4);	STRAY(0xe8);	STRAY(0xec);
/* 0f0 */	IS(consdin);	IS(consdout);	IS(cnrint);	IS(cnxint);
/* 100 */	NEX0(0x100);		/* ipl 0x14, nexus 0-15 */
/* 140 */	NEX0(0x140);		/* ipl 0x15, nexus 0-15 */
/* 180 */	NEX0(0x180);		/* ipl 0x16, nexus 0-15 */
/* 1c0 */	NEX0(0x1c0);		/* ipl 0x17, nexus 0-15 */

/*
 * 750 hardware reads through UNIvec (scb + 512 bytes) to find Unibus
 * interrupt vectors.  780s use this space as a jump table (lookup
 * code in locore.s makes 780s work like 750s).  Additional pages
 * of interrupt vectors for additional UBAs follow immediately.
 *
 * 8600s may use the next page as a second SCB, for which purpose we init
 * it here.  Everything else will simply replace these with Unibus vectors.
 * An additional page is provided for UBA jump tables if the second
 * scb might be present.  Other CPUs with additional scbs should expand
 * this area as needed.
 */
	.globl	_UNIvec
	.globl	_eUNIvec
_UNIvec:
#if VAX8600
/* 200 */	STRAY16(0x200);		/* unused (?) */
/* 240 */	STRAY16(0x240);		/* sbi1fail etc. set at boot time */
/* 280 */	STRAY16(0x280);		/* unused (?) */
/* 2c0 */	STRAY16(0x2c0);		/* unused (?) */
/* 300 */	NEX1(0x300);		/* ipl 0x14, nexus 0-15, sbia 1 */
/* 340 */	NEX1(0x340);		/* ipl 0x15, nexus 0-15, sbia 1 */
/* 380 */	NEX1(0x380);		/* ipl 0x16, nexus 0-15, sbia 1 */
/* 3c0 */	NEX1(0x3c0);		/* ipl 0x17, nexus 0-15, sbia 1 */

#endif
#if NUBA > 0
		.space	512*NUBA	# 750 first/second unibus intr vector
					# UBA jump tables on 780's
#endif
_eUNIvec:
