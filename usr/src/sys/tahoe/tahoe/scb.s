/*	scb.s	1.1	86/01/05	*/
/*	scb.s	TAHOE 4.2 version 84/03/1	*/

/*
 * System control block
 */


#define	_Xnull	0
#define	STRAY	.long	_Xstray
#define	STRAY8	STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY
#define	STRAY15	STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY8
#define	KS(a)	.long	_X/**/a
#define	IS(a)	.long	_X/**/a

_scb:	.globl	_scb
/* 000 */	STRAY;		IS(powfail);	IS(doadump);	STRAY;
/* 004 */	STRAY;		STRAY;		STRAY;		IS(hardclock);
/* 008 */	STRAY;		STRAY;		IS(cnrint);	IS(cnxint);
/* 00c */	IS(rmtrint);	IS(rmtxint);	STRAY;		STRAY;
/* 010 */	IS(soft15);	IS(soft14);	IS(soft13);	IS(netintr);
/* 014 */	IS(soft11);	IS(soft10);	IS(soft9);	IS(softclock);
/* 018 */	IS(soft7); 	IS(soft6);	IS(soft5);	IS(soft4);
/* 01c */	IS(soft3);	IS(soft2);	IS(soft1);	STRAY;
/* 020 */	IS(buserr);	STRAY;		STRAY;		STRAY;
/* 024 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 028 */	STRAY;		STRAY;		STRAY;		KS(syscall);
/* 02c */	KS(privinflt);	KS(resopflt);	KS(resadflt);	KS(protflt);
/* 030 */	KS(transflt);	IS(kspnotval);	KS(tracep);	KS(bptflt);
/* 034 */	KS(arithtrap);	KS(alignflt);	KS(sfexcep);	KS(fpm);
/* 038 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 03c */	STRAY;		STRAY;		STRAY;		STRAY;
#include "vx.h"
#if NVX > 0
/* 040 */	IS(vackint0);	IS(vcmdrsp0);	IS(vunsol0);	STRAY;
#else
/* 040 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 1
/* 044 */	IS(vackint1);	IS(vcmdrsp1);	IS(vunsol1);	STRAY;
#else
/* 044 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 2
/* 048 */	IS(vackint2);	IS(vcmdrsp2);	IS(vunsol2);	STRAY;
#else
/* 048 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 3
/* 04c */	IS(vackint3);	IS(vcmdrsp3);	IS(vunsol3);	STRAY;
#else
/* 04c */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 4
/* 050 */	IS(vackint4);	IS(vcmdrsp4);	IS(vunsol4);	STRAY;
#else
/* 050 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 5
/* 054 */	IS(vackint5);	IS(vcmdrsp5);	IS(vunsol5);	STRAY;
#else
/* 054 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 6
/* 058 */	IS(vackint6);	IS(vcmdrsp6);	IS(vunsol6);	STRAY;
#else
/* 058 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 7
/* 05c */	IS(vackint7);	IS(vcmdrsp7);	IS(vunsol7);	STRAY;
#else
/* 05c */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 8
/* 060 */	IS(vackint8);	IS(vcmdrsp8);	IS(vunsol8);	STRAY;
#else
/* 060 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 9
/* 064 */	IS(vackint9);	IS(vcmdrsp9);	IS(vunsol9);	STRAY;
#else
/* 064 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 10
/* 068 */	IS(vackint10);	IS(vcmdrsp10);	IS(vunsol10);	STRAY;
#else
/* 068 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
#if NVX > 11
/* 06c */	IS(vackint11);	IS(vcmdrsp11);	IS(vunsol11);	STRAY;
#else
/* 06c */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
/* 070 */	STRAY;		STRAY15;
#include "cy.h"
#if NCY > 0
/* 080 */	IS(cyintr0);	STRAY15;
#else
/* 080 */	STRAY;		STRAY15;
#endif
#include "ace.h"
#if NACE > 0
/* 090 */	IS(acecint0);	IS(acerint0);	STRAY;		STRAY;
#else
/* 090 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
/* 094 */	STRAY;		STRAY;		STRAY;		STRAY;	
#if NACE > 1
/* 098 */	IS(acecint1);	IS(acerint1);	STRAY;		STRAY;
#else
/* 098 */	STRAY;		STRAY;		STRAY;		STRAY;
#endif
/* 09c */	STRAY;		STRAY;		STRAY;		STRAY;	
/* 0a0 */	STRAY;		STRAY15;
/* 0b0 */	STRAY;		STRAY15;
/* 0c0 */	STRAY;		STRAY15;
/* 0d0 */	STRAY;		STRAY15;
#include "fsd.h"
#if NVD > 0
/* 0e0 */	IS(vdintr0);
#else
/* 0e0 */	STRAY;
#endif
#if NVD > 1
/* 0e4 */	IS(vdintr1);
#else
/* 0e4 */	STRAY;
#endif
#if NVD > 2
/* 0e8 */	IS(vdintr2);
#else
/* 0e8 */	STRAY;
#endif
#if NVD > 3
/* 0ec */	IS(vdintr3);	
#else
/* 0ec */	STRAY;	
#endif

/* 0e4 */	STRAY;		STRAY;		STRAY;		STRAY;	
/* 0e8 */	STRAY8;
/* 0f0 */	STRAY;		STRAY;		STRAY;		STRAY;	
/* 0f4 */	STRAY;		STRAY;		STRAY;		STRAY;	
/* 0f8 */	STRAY;		STRAY;		STRAY;		STRAY;	
/* 0fC */	STRAY;		STRAY;		STRAY;		STRAY;
