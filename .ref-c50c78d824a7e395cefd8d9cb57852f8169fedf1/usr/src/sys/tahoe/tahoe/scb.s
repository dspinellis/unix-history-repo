/*
 *	@(#)scb.s	7.1 (Berkeley) %G%
 */

/*
 * System control block
 */
#define	STRAY	.long	_Xstray
#define	STRAY8	STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY;STRAY
#define	KS(a)	.long	_X/**/a
#define	IS(a)	.long	_X/**/a

_scb:	.globl	_scb
/* 000 */	STRAY;		IS(powfail);	IS(doadump);	STRAY;
/* 004 */	STRAY;		STRAY;		STRAY;		IS(hardclock);
/* 008 */	STRAY;		STRAY;		IS(cnrint);	IS(cnxint);
/* 00c */	IS(rmtrint);	IS(rmtxint);	STRAY;		STRAY;
/* 010 */	IS(kdbintr);	STRAY;		STRAY;		IS(netintr);
/* 014 */	STRAY;		STRAY;		STRAY;		IS(softclock);
/* 018 */	STRAY; 		STRAY;		STRAY;		STRAY;
/* 01c */	STRAY;		STRAY;		STRAY;		STRAY;
/* 020 */	IS(buserr);	STRAY;		STRAY;		STRAY;
/* 024 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 028 */	STRAY;		STRAY;		STRAY;		KS(syscall);
/* 02c */	KS(privinflt);	KS(resopflt);	KS(resadflt);	KS(protflt);
/* 030 */	KS(transflt);	IS(kspnotval);	KS(tracep);	KS(bptflt);
/* 034 */	KS(arithtrap);	KS(alignflt);	KS(sfexcep);	KS(fpm);
/* 038 */	STRAY;		STRAY;		STRAY;		STRAY;
/* 03c */	STRAY;		STRAY;		STRAY;		STRAY;
	/* device interrupt vectors */
/* 040 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
/* 060 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
/* 080 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
/* 0a0 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
/* 0c0 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
/* 0e0 */	STRAY8;		STRAY8;		STRAY8;		STRAY8;
