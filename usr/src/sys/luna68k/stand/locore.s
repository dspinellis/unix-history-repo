/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)locore.s	7.1 (Berkeley) %G%
 */

#define	T_BUSERR	0
#define	T_ADDRERR	1
#define	T_ILLINST	2
#define	T_ZERODIV	3
#define	T_CHKINST	4
#define	T_TRAPVINST	5
#define	T_PRIVINST	6
#define	T_MMUFLT	8
#define	T_FMTERR	10
#define	T_FPERR		11
#define	T_COPERR	12

#define	PSL_LOWIPL	8192
#define	PSL_HIGHIPL	9984

#define	SPL1		8448
#define	SPL2		8704
#define	SPL3		8960
#define	SPL4		9216
#define	SPL5		9472
#define	SPL6		9728

#define	CLOCK_REG	1660944384
#define	CLK_CLR		1

#define	ILLGINST	16
#define	NMIVEC		124
#define	EVTRAPF		188

	.text

	.globl	Reset
	.globl	_buserr,_addrerr
	.globl	_illinst,_zerodiv,_chkinst,_trapvinst,_privinst
	.globl	_lev6intr,_lev5intr,_lev3intr,_lev2intr,_badtrap

Reset:
	jmp start		/* 0: NOT USED (reset PC) */
	.word	0		/* 1: NOT USED (reset PC) */
	.long	_buserr		/* 2: bus error */
	.long	_addrerr	/* 3: address error */
	.long	_illinst	/* 4: illegal instruction */
	.long	_zerodiv	/* 5: zero divide */
	.long	_chkinst	/* 6: CHK instruction */
	.long	_trapvinst	/* 7: TRAPV instruction */
	.long	_privinst	/* 8: privilege violation */
	.long	_badtrap	/* 9: trace */
	.long	_illinst	/* 10: line 1010 emulator */
	.long	_illinst	/* 11: line 1111 emulator */
	.long	_badtrap	/* 12: unassigned, reserved */
	.long	_coperr		/* 13: coprocessor protocol violation */
	.long	_fmterr		/* 14: format error */
	.long	_badtrap	/* 15: uninitialized interrupt vector */
	.long	_badtrap	/* 16: unassigned, reserved */
	.long	_badtrap	/* 17: unassigned, reserved */
	.long	_badtrap	/* 18: unassigned, reserved */
	.long	_badtrap	/* 19: unassigned, reserved */
	.long	_badtrap	/* 20: unassigned, reserved */
	.long	_badtrap	/* 21: unassigned, reserved */
	.long	_badtrap	/* 22: unassigned, reserved */
	.long	_badtrap	/* 23: unassigned, reserved */
	.long	_badtrap	/* 24: spurious interrupt */
	.long	_badtrap	/* 25: level 1 interrupt autovector */
	.long	_lev2intr	/* 26: level 2 interrupt autovector */
	.long	_lev3intr	/* 27: level 3 interrupt autovector */
	.long	_badtrap	/* 28: level 4 interrupt autovector */
	.long	_lev5intr	/* 29: level 5 interrupt autovector */
	.long	_lev6intr	/* 30: level 6 interrupt autovector */
	.long	_badtrap	/* 31: level 7 interrupt autovector */
	.long	_illinst	/* 32: syscalls */
	.long	_illinst	/* 33: sigreturn syscall or breakpoint */
	.long	_illinst	/* 34: breakpoint or sigreturn syscall */
	.long	_illinst	/* 35: TRAP instruction vector */
	.long	_illinst	/* 36: TRAP instruction vector */
	.long	_illinst	/* 37: TRAP instruction vector */
	.long	_illinst	/* 38: TRAP instruction vector */
	.long	_illinst	/* 39: TRAP instruction vector */
	.long	_illinst	/* 40: TRAP instruction vector */
	.long	_illinst	/* 41: TRAP instruction vector */
	.long	_illinst	/* 42: TRAP instruction vector */
	.long	_illinst	/* 43: TRAP instruction vector */
	.long	_illinst	/* 44: TRAP instruction vector */
	.long	_illinst	/* 45: TRAP instruction vector */
	.long	_illinst	/* 46: TRAP instruction vector */
	.long	_illinst	/* 47: TRAP instruction vector */
 	.long	_fptrap		/* 48: FPCP branch/set on unordered cond */
 	.long	_fptrap		/* 49: FPCP inexact result */
 	.long	_fptrap		/* 50: FPCP divide by zero */
 	.long	_fptrap		/* 51: FPCP underflow */
 	.long	_fptrap		/* 52: FPCP operand error */
 	.long	_fptrap		/* 53: FPCP overflow */
 	.long	_fptrap		/* 54: FPCP signalling NAN */

	.long	_badtrap	/* 55: unassigned, reserved */
	.long	_badtrap	/* 56: unassigned, reserved */
	.long	_badtrap	/* 57: unassigned, reserved */
	.long	_badtrap	/* 58: unassigned, reserved */
	.long	_badtrap	/* 59: unassigned, reserved */
	.long	_badtrap	/* 60: unassigned, reserved */
	.long	_badtrap	/* 61: unassigned, reserved */
	.long	_badtrap	/* 62: unassigned, reserved */
	.long	_badtrap	/* 63: unassigned, reserved */
#define BADTRAP16	.long	_badtrap,_badtrap,_badtrap,_badtrap,\
				_badtrap,_badtrap,_badtrap,_badtrap,\
				_badtrap,_badtrap,_badtrap,_badtrap,\
				_badtrap,_badtrap,_badtrap,_badtrap
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */
	BADTRAP16		/* 64-255: user interrupt vectors */


	.globl  start
	.globl  _main
	.globl	_etext,_edata,_end

	START = 0x700000
	STACK = 0x800000
	DIPSW = 0x49000000

start:
        movw    #PSL_HIGHIPL,sr         | no interrupts
	movl	#STACK,sp		| set SP

	movl	#_prgcore, a2		| save program address
	movl	#Reset, a2@+		| save start of core
	movl	#_end,  a2@+		| save end of core
	movl	#STACK, a2@		| save initial stack addr

/* clear BSS area */
	movl	#_edata,a2		| start of BSS
	movl	#_end,a3		| end
Lbssclr:
	clrb	a2@+			| clear BSS
	cmpl	a2,a3			| done?
	bne	Lbssclr			| no, keep going

/* save address to goto ROM monitor */
	movec	vbr,a0			| ROM vbr to a0
	movl	a0@(NMIVEC),d0		| restore NMIVEC
	movl	#_gotoROM,a0		| save to _gotoROM
	movl	d0,a0@			|
	movl	#Reset,a0		| BP vbr to a0
	movl	#_exit,a0@(NMIVEC)	| save address
	

/* switch vector tabel */
	movec	vbr,a0
	movl	a0@(ILLGINST),sp@-	| save ILLINST vector for BrkPtr
	movl	a0@(EVTRAPF),sp@-

	movl	#Reset,a0
	movl	sp@+,a0@(EVTRAPF)
	movl	sp@+,a0@(ILLGINST)	| restore ILLINST vector
	movec	a0,vbr

	movl	#DIPSW,a0
	movw	a0@,d0
	lsrl	#8,d0
	andl	#0xFF,d0
	movl	d0,_dipsw1
	movw	a0@,d0
	andl	#0xFF,d0
	movl	d0,_dipsw2

/* final setup for C code */
        movw    #PSL_LOWIPL,sr		| no interrupts
	jsr     _main			| lets go
	jsr     start

/*
 * exit to ROM monitor
 */

	ROM_VBR = 0

	.globl	_exit

_exit:
        movw    #PSL_HIGHIPL,sr         | no interrupts
	movl	#ROM_VBR,a0
	movec	a0,vbr
	movl	#_gotoROM,a0
	movl	a0@,a1
	jmp	a1@

/*
 * Trap/interrupt vector routines
 */ 

	.globl	_trap,_nofault,_longjmp
_buserr:
	tstl	_nofault		| device probe?
	jeq	_addrerr		| no, handle as usual
	movl	_nofault,sp@-		| yes,
	jbsr	_longjmp		|  longjmp(nofault)
_addrerr:
	clrw	sp@-			| pad SR to longword
	moveml	#0xFFFF,sp@-		| save user registers
	movl	usp,a0			| save the user SP
	movl	a0,sp@(60)		|   in the savearea
	lea	sp@(64),a1		| grab base of HW berr frame
	movw	a1@(12),d0		| grab SSW for fault processing
	btst	#12,d0			| RB set?
	jeq	LbeX0			| no, test RC
	bset	#14,d0			| yes, must set FB
	movw	d0,a1@(12)		| for hardware too
LbeX0:
	btst	#13,d0			| RC set?
	jeq	LbeX1			| no, skip
	bset	#15,d0			| yes, must set FC
	movw	d0,a1@(12)		| for hardware too
LbeX1:
	btst	#8,d0			| data fault?
	jeq	Lbe0			| no, check for hard cases
	movl	a1@(18),d1		| fault address is as given in frame
	jra	Lbe10			| thats it
Lbe0:
	btst	#4,a1@(8)		| long (type B) stack frame?
	jne	Lbe4			| yes, go handle
	movl	a1@(4),d1		| no, can use save PC
	btst	#14,d0			| FB set?
	jeq	Lbe3			| no, try FC
	addql	#4,d1			| yes, adjust address
	jra	Lbe10			| done
Lbe3:
	btst	#15,d0			| FC set?
	jeq	Lbe10			| no, done
	addql	#2,d1			| yes, adjust address
	jra	Lbe10			| done
Lbe4:
	movl	a1@(38),d1		| long format, use stage B address
	btst	#15,d0			| FC set?
	jeq	Lbe10			| no, all done
	subql	#2,d1			| yes, adjust address
Lbe10:
	movl	d1,sp@-			| push fault VA
	movw	d0,sp@-			| and SSW
	clrw	sp@-			|   padded to longword
	movw	a1@(8),d0		| get frame format/vector offset
	andw	#0x0FFF,d0		| clear out frame format
	cmpw	#12,d0			| address error vector?
	jeq	Lisaerr			| yes, go to it
#if 0
	movl	d1,a0			| fault address
	.long	0xf0109e11		| ptestr #1,a0@,#7
	.long	0xf0176200		| pmove psr,sp@
	btst	#7,sp@			| bus error bit set?
	jeq	Lismerr			| no, must be MMU fault
	clrw	sp@			| yes, re-clear pad word
#endif
	jra	Lisberr			| and process as normal bus error
Lismerr:
	movl	#T_MMUFLT,sp@-		| show that we are an MMU fault
	jra	Lbexit			| and deal with it
Lisaerr:
	movl	#T_ADDRERR,sp@-		| mark address error
	jra	Lbexit			| and deal with it
Lisberr:
	movl	#T_BUSERR,sp@-		| mark bus error
Lbexit:
	jbsr	_trap			| handle the error
	lea	sp@(12),sp		| pop value args
	movl	sp@(60),a0		| restore user SP
	movl	a0,usp			|   from save area
	moveml	sp@+,#0x7FFF		| restore most user regs
	addql	#4,sp			| toss SSP
	tstw	sp@+			| do we need to clean up stack?
	jeq	rei			| no, just continue
	btst	#7,sp@(6)		| type 9/10/11 frame?
	jeq	rei			| no, nothing to do
	btst	#5,sp@(6)		| type 9?
	jne	Lbex1			| no, skip
	movw	sp@,sp@(12)		| yes, push down SR
	movl	sp@(2),sp@(14)		| and PC
	clrw	sp@(18)			| and mark as type 0 frame
	lea	sp@(12),sp		| clean the excess
	jra	rei			| all done
Lbex1:
	btst	#4,sp@(6)		| type 10?
	jne	Lbex2			| no, skip
	movw	sp@,sp@(24)		| yes, push down SR
	movl	sp@(2),sp@(26)		| and PC
	clrw	sp@(30)			| and mark as type 0 frame
	lea	sp@(24),sp		| clean the excess
	jra	rei			| all done
Lbex2:
	movw	sp@,sp@(84)		| type 11, push down SR
	movl	sp@(2),sp@(86)		| and PC
	clrw	sp@(90)			| and mark as type 0 frame
	lea	sp@(84),sp		| clean the excess
	jra	rei			| all done

_illinst:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_ILLINST,d0
	jra	_fault

_zerodiv:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_ZERODIV,d0
	jra	_fault

_chkinst:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_CHKINST,d0
	jra	_fault

_trapvinst:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_TRAPVINST,d0
	jra	_fault

_privinst:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_PRIVINST,d0
	jra	_fault

_coperr:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_COPERR,d0
	jra	_fault

_fmterr:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_FMTERR,d0
	jra	_fault

_fptrap:
#ifdef FPCOPROC
	clrw	sp@-		| pad SR to longword
	moveml	#0xFFFF,sp@-	| save user registers
	movl	usp,a0		| and save
	movl	a0,sp@(60)	|   the user stack pointer
	clrl	sp@-		| no VA arg
#if 0
	lea	_u+PCB_FPCTX,a0	| address of FP savearea
	.word	0xf310		| fsave a0@
	tstb	a0@		| null state frame?
	jeq	Lfptnull	| yes, safe
	clrw	d0		| no, need to tweak BIU
	movb	a0@(1),d0	| get frame size
	bset	#3,a0@(0,d0:w)	| set exc_pend bit of BIU
Lfptnull:
	.word	0xf227,0xa800	| fmovem fpsr,sp@- (code arg)
	.word	0xf350		| frestore a0@
#else
	clrl	sp@-		| push dummy FPSR
#endif
	movl	#T_FPERR,sp@-	| push type arg
	jbsr	_trap		| call trap
	lea	sp@(12),sp	| pop value args
	movl	sp@(60),a0	| restore
	movl	a0,usp		|   user SP
	moveml	sp@+,#0x7FFF	| and remaining user registers
	addql	#6,sp		| pop SSP and align word
	jra	rei		| all done
#else
	jra	_badtrap	| treat as an unexpected trap
#endif

	.globl	_fault
_fault:
	movl	usp,a0		| get and save
	movl	a0,sp@(60)	|   the user stack pointer
	clrl	sp@-		| no VA arg
	clrl	sp@-		| or code arg
	movl	d0,sp@-		| push trap type
	jbsr	_trap		| handle trap
	lea	sp@(12),sp	| pop value args
	movl	sp@(60),a0	| restore
	movl	a0,usp		|   user SP
	moveml	sp@+,#0x7FFF	| restore most user regs
	addql	#6,sp		| pop SP and pad word
	jra	rei		| all done

	.globl	_straytrap
_badtrap:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	movw	sp@(24),sp@-
	clrw	sp@-
	jbsr	_straytrap
	addql	#4,sp
	moveml	sp@+,#0x0303
	addql	#2,sp
	jra	rei

/*
 * Interrupt handlers.
 * All device interrupts are auto-vectored.  Most can be configured
 * to interrupt in the range IPL2 to IPL6.  Here are our assignments:
 *
 *	Level 0:	
 *	Level 1:	
 *	Level 2:	SCSI SPC
 *	Level 3:	
 *	Level 4:	
 *	Level 5:	System Clock
 *	Level 6:	Internal SIO used uPD7201A
 *	Level 7:	Non-maskable: Abort Key (Dispatched vector to ROM monitor)
 */
	.globl	_scintr, __siointr, _hardclock

_lev2intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	jbsr	_scintr
	moveml	sp@+,#0x0303
	addql	#2,sp
	jra	rei

_lev3intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	moveml	sp@+,#0x0303
	addql	#2,sp
	jra	rei

_lev5intr:
	clrw	sp@-			| push pad word
	moveml	#0xC0C0,sp@-		| save scratch regs
	movl	#CLOCK_REG,a0		| get clock CR addr
	movb	#CLK_CLR,a0@		| reset system clock
	lea	sp@(16),a1		| get pointer to PS
	movl	a1@,sp@-		| push padded PS
	movl	a1@(4),sp@-		| push PC
	jbsr	_hardclock		| call generic clock int routine
	addql	#8,sp			| pop params
	moveml	sp@+,#0x0303		| restore scratch regs
	addql	#2,sp			| pop pad word
	jra	rei			| all done

_lev6intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	jbsr	__siointr
	moveml	sp@+,#0x0303
	addql	#2,sp
	jra	rei


/*
 * Emulation of VAX REI instruction.
 *
 * This code deals with checking for and servicing ASTs
 * (profiling, scheduling) and software interrupts (network, softclock).
 * We check for ASTs first, just like the VAX.  To avoid excess overhead
 * the T_ASTFLT handling code will also check for software interrupts so we
 * do not have to do it here.
 *
 * This code is complicated by the fact that sendsig may have been called
 * necessitating a stack cleanup.  A cleanup should only be needed at this
 * point for coprocessor mid-instruction frames (type 9), but we also test
 * for bus error frames (type 10 and 11).
 */
#if 0
	.comm	_ssir,1
rei:
#ifdef DEBUG
	tstl	_panicstr		| have we paniced?
	jne	Ldorte			| yes, do not make matters worse
#endif
	btst	#PCB_ASTB,_u+PCB_FLAGS+1| AST pending?
	jeq	Lchksir			| no, go check for SIR
	btst	#5,sp@			| yes, are we returning to user mode?
	jne	Lchksir			| no, go check for SIR
	clrw	sp@-			| pad SR to longword
	moveml	#0xFFFF,sp@-		| save all registers
	movl	usp,a1			| including
	movl	a1,sp@(60)		|    the users SP
	clrl	sp@-			| VA == none
	clrl	sp@-			| code == none
	movl	#T_ASTFLT,sp@-		| type == async system trap
	jbsr	_trap			| go handle it
	lea	sp@(12),sp		| pop value args
	movl	sp@(60),a0		| restore
	movl	a0,usp			|   user SP
	moveml	sp@+,#0x7FFF		| and all remaining registers
	addql	#4,sp			| toss SSP
	tstw	sp@+			| do we need to clean up stack?
	jeq	Ldorte			| no, just continue
	btst	#7,sp@(6)		| type 9/10/11 frame?
	jeq	Ldorte			| no, nothing to do
	btst	#5,sp@(6)		| type 9?
	jne	Last1			| no, skip
	movw	sp@,sp@(12)		| yes, push down SR
	movl	sp@(2),sp@(14)		| and PC
	clrw	sp@(18)			| and mark as type 0 frame
	lea	sp@(12),sp		| clean the excess
	jra	Ldorte			| all done
Last1:
	btst	#4,sp@(6)		| type 10?
	jne	Last2			| no, skip
	movw	sp@,sp@(24)		| yes, push down SR
	movl	sp@(2),sp@(26)		| and PC
	clrw	sp@(30)			| and mark as type 0 frame
	lea	sp@(24),sp		| clean the excess
	jra	Ldorte			| all done
Last2:
	movw	sp@,sp@(84)		| type 11, push down SR
	movl	sp@(2),sp@(86)		| and PC
	clrw	sp@(90)			| and mark as type 0 frame
	lea	sp@(84),sp		| clean the excess
	jra	Ldorte			| all done
Lchksir:
	tstb	_ssir			| SIR pending?
	jeq	Ldorte			| no, all done
	movl	d0,sp@-			| need a scratch register
	movw	sp@(4),d0		| get SR
	andw	#PSL_IPL7,d0		| mask all but IPL
	jne	Lnosir			| came from interrupt, no can do
	movl	sp@+,d0			| restore scratch register
Lgotsir:
	movw	#SPL1,sr		| prevent others from servicing int
	tstb	_ssir			| too late?
	jeq	Ldorte			| yes, oh well...
	clrw	sp@-			| pad SR to longword
	moveml	#0xFFFF,sp@-		| save all registers
	movl	usp,a1			| including
	movl	a1,sp@(60)		|    the users SP
	clrl	sp@-			| VA == none
	clrl	sp@-			| code == none
	movl	#T_SSIR,sp@-		| type == software interrupt
	jbsr	_trap			| go handle it
	lea	sp@(12),sp		| pop value args
	movl	sp@(60),a0		| restore
	movl	a0,usp			|   user SP
	moveml	sp@+,#0x7FFF		| and all remaining registers
	addql	#6,sp			| pop SSP and align word
	rte
Lnosir:
	movl	sp@+,d0			| restore scratch register
Ldorte:
#else
rei:					| dummy Entry of rei
#endif
	rte				| real return


/*
 * Primitives
 */ 

#ifdef GPROF
#ifdef __GNUC__
#define	ENTRY(name) \
	.globl _/**/name; _/**/name: link a6,#0; jbsr mcount; unlk a6
#define ALTENTRY(name, rname) \
	ENTRY(name); jra rname+12
#else
#define	ENTRY(name) \
	.globl _/**/name; _/**/name: jbsr mcount
#define ALTENTRY(name, rname) \
	ENTRY(name); jra rname+6
#endif
#else
#define	ENTRY(name) \
	.globl _/**/name; _/**/name:
#define ALTENTRY(name, rname) \
	.globl _/**/name; _/**/name:
#endif

/*
 * non-local gotos
 */
ALTENTRY(savectx, _setjmp)
ENTRY(setjmp)
	movl	sp@(4),a0	| savearea pointer
	moveml	#0xFCFC,a0@	| save d2-d7/a2-a7
	movl	sp@,a0@(48)	| and return address
	moveq	#0,d0		| return 0
	rts

ENTRY(qsetjmp)
	movl	sp@(4),a0	| savearea pointer
	lea	a0@(40),a0	| skip regs we do not save
	movl	a6,a0@+		| save FP
	movl	sp,a0@+		| save SP
	movl	sp@,a0@		| and return address
	moveq	#0,d0		| return 0
	rts

ENTRY(longjmp)
	movl	sp@(4),a0
	moveml	a0@+,#0xFCFC
	movl	a0@,sp@
	moveq	#1,d0
	rts

	.globl	_getsfc, _getdfc
_getsfc:
	movc	sfc,d0
	rts
_getdfc:
	movc	dfc,d0
	rts

/*
 * Set processor priority level calls.  Most could (should) be replaced
 * by inline asm expansions.  However, SPL0 and SPLX require special
 * handling.  If we are returning to the base processor priority (SPL0)
 * we need to check for our emulated software interrupts.
 */

ENTRY(spl0)
	moveq	#0,d0
	movw	sr,d0			| get old SR for return
	movw	#PSL_LOWIPL,sr		| restore new SR
|	jra	Lsplsir
	rts

ENTRY(splx)
	moveq	#0,d0
	movw	sr,d0			| get current SR for return
	movw	sp@(6),d1		| get new value
	movw	d1,sr			| restore new SR
|	andw	#PSL_IPL7,d1		| mask all but PSL_IPL
|	jne	Lspldone		| non-zero, all done
|Lsplsir:
|	tstb	_ssir			| software interrupt pending?
|	jeq	Lspldone		| no, all done
|	subql	#4,sp			| make room for RTE frame
|	movl	sp@(4),sp@(2)		| position return address
|	clrw	sp@(6)			| set frame type 0
|	movw	#PSL_LOWIPL,sp@		| and new SR
|	jra	Lgotsir			| go handle it
|Lspldone:
	rts

ENTRY(spl1)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL1,sr
	rts

ALTENTRY(splscsi, _spl2)
ENTRY(spl2)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL2,sr
	rts

ENTRY(spl3)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL3,sr
	rts

ENTRY(spl4)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL4,sr
	rts

ENTRY(spl5)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL5,sr
	rts

ENTRY(spl6)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL6,sr
	rts

ALTENTRY(splhigh, _spl7)
ENTRY(spl7)
	moveq	#0,d0
	movw	sr,d0
	movw	#PSL_HIGHIPL,sr
	rts


ENTRY(_insque)
	movw	sr,d0
	movw	#PSL_HIGHIPL,sr		| atomic
	movl	sp@(8),a0		| where to insert (after)
	movl	sp@(4),a1		| element to insert (e)
	movl	a0@,a1@			| e->next = after->next
	movl	a0,a1@(4)		| e->prev = after
	movl	a1,a0@			| after->next = e
	movl	a1@,a0
	movl	a1,a0@(4)		| e->next->prev = e
	movw	d0,sr
	rts

ENTRY(_remque)
	movw	sr,d0
	movw	#PSL_HIGHIPL,sr		| atomic
	movl	sp@(4),a0		| element to remove (e)
	movl	a0@,a1
	movl	a0@(4),a0
	movl	a0,a1@(4)		| e->next->prev = e->prev
	movl	a1,a0@			| e->prev->next = e->next
	movw	d0,sr
	rts

ALTENTRY(blkclr, _bzero)
ENTRY(bzero)
	movl	sp@(4),a0
	movl	sp@(8),d0
	jeq	Lbzero1
	movl	a0,d1
	btst	#0,d1
	jeq	Lbzero2
	clrb	a0@+
	subql	#1,d0
	jeq	Lbzero1
Lbzero2:
	movl	d0,d1
	andl	#31,d0
	lsrl	#5,d1
	jeq	Lbzero3
Lbzero4:
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	subql	#1,d1
	jne	Lbzero4
	tstl	d0
	jeq	Lbzero1
Lbzero3:
	clrb	a0@+
	subql	#1,d0
	jne	Lbzero3
Lbzero1:
	rts

/*
 * strlen(str)
 */
ENTRY(strlen)
	moveq	#-1,d0
	movl	sp@(4),a0	| string
Lslloop:
	addql	#1,d0		| increment count
	tstb	a0@+		| null?
	jne	Lslloop		| no, keep going
	rts

/*
 * bcmp(s1, s2, len)
 *
 * WARNING!  This guy only works with counts up to 64K
 */
ENTRY(bcmp)
	movl	sp@(4),a0		| string 1
	movl	sp@(8),a1		| string 2
	moveq	#0,d0
	movw	sp@(14),d0		| length
	jeq	Lcmpdone		| if zero, nothing to do
	subqw	#1,d0			| set up for DBcc loop
Lcmploop:
	cmpmb	a0@+,a1@+		| equal?
	dbne	d0,Lcmploop		| yes, keep going
	addqw	#1,d0			| +1 gives zero on match
Lcmpdone:
	rts
	
/*
 * {ov}bcopy(from, to, len)
 *
 * Works for counts up to 128K.
 */
ALTENTRY(ovbcopy, _bcopy)
ENTRY(bcopy)
	movl	sp@(12),d0		| get count
	jeq	Lcpyexit		| if zero, return
	movl	sp@(4),a0		| src address
	movl	sp@(8),a1		| dest address
	cmpl	a1,a0			| src before dest?
	jlt	Lcpyback		| yes, copy backwards (avoids overlap)
	movl	a0,d1
	btst	#0,d1			| src address odd?
	jeq	Lcfeven			| no, go check dest
	movb	a0@+,a1@+		| yes, copy a byte
	subql	#1,d0			| update count
	jeq	Lcpyexit		| exit if done
Lcfeven:
	movl	a1,d1
	btst	#0,d1			| dest address odd?
	jne	Lcfbyte			| yes, must copy by bytes
	movl	d0,d1			| no, get count
	lsrl	#2,d1			| convert to longwords
	jeq	Lcfbyte			| no longwords, copy bytes
	subql	#1,d1			| set up for dbf
Lcflloop:
	movl	a0@+,a1@+		| copy longwords
	dbf	d1,Lcflloop		| til done
	andl	#3,d0			| get remaining count
	jeq	Lcpyexit		| done if none
Lcfbyte:
	subql	#1,d0			| set up for dbf
Lcfbloop:
	movb	a0@+,a1@+		| copy bytes
	dbf	d0,Lcfbloop		| til done
Lcpyexit:
	rts
Lcpyback:
	addl	d0,a0			| add count to src
	addl	d0,a1			| add count to dest
	movl	a0,d1
	btst	#0,d1			| src address odd?
	jeq	Lcbeven			| no, go check dest
	movb	a0@-,a1@-		| yes, copy a byte
	subql	#1,d0			| update count
	jeq	Lcpyexit		| exit if done
Lcbeven:
	movl	a1,d1
	btst	#0,d1			| dest address odd?
	jne	Lcbbyte			| yes, must copy by bytes
	movl	d0,d1			| no, get count
	lsrl	#2,d1			| convert to longwords
	jeq	Lcbbyte			| no longwords, copy bytes
	subql	#1,d1			| set up for dbf
Lcblloop:
	movl	a0@-,a1@-		| copy longwords
	dbf	d1,Lcblloop		| til done
	andl	#3,d0			| get remaining count
	jeq	Lcpyexit		| done if none
Lcbbyte:
	subql	#1,d0			| set up for dbf
Lcbbloop:
	movb	a0@-,a1@-		| copy bytes
	dbf	d0,Lcbbloop		| til done
	rts


	.data

/*
 * Memory Infomation Field for secondary booter memory allocator
 */
	.globl  _prgcore
	.globl	_dipsw1,_dipsw2

_prgcore:
	.long	0
	.long	0
	.long	0

_gotoROM:
	.long	0

_dipsw1:
	.long	0

_dipsw2:
	.long	0
