/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1980, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: locore.s 1.47 89/10/08$
 *
 *	@(#)locore.s	7.3 (Berkeley) 6/22/90
 */

	.text
/*
 * This is where we wind up if the kernel jumps to location 0.
 * (i.e. a bogus PC)  This is known to immediately follow the vector
 * table and is hence at 0x400 (see reset vector in vectors.s).
 */
	.globl	_panic
	pea	Ljmp0panic
	jbsr	_panic
	/* NOTREACHED */
Ljmp0panic:
	.asciz	"kernel jump to zero"
	.even

/*
 * Do a dump.
 * Called by auto-restart.
 */
	.globl	_dumpsys
	.globl	_doadump
_doadump:
	jbsr	_dumpsys
	jbsr	_doboot
	/*NOTREACHED*/

/*
 * Trap/interrupt vector routines
 */ 

	.globl	_trap, _nofault, _longjmp
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
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lbehpmmu		| yes, skip
	movl	d1,a0			| fault address
	.long	0xf0109e11		| ptestr #1,a0@,#7
	.long	0xf0176200		| pmove psr,sp@
	btst	#7,sp@			| bus error bit set?
	jeq	Lismerr			| no, must be MMU fault
	clrw	sp@			| yes, re-clear pad word
	jra	Lisberr			| and process as normal bus error
Lbehpmmu:
#endif
#if defined(HP320) || defined(HP350)
	lea	_IObase+MMUSTAT,a0	| no, get addr of MMU status
	movl	a0@,d0			| read status
	btst	#3,d0			| MMU fault?
	jeq	Lisberr			| no, just a non-MMU bus error so skip
	andl	#~MMU_FAULT,a0@		| yes, clear fault bits
	movw	d0,sp@			| pass MMU stat in upper half of code
#endif
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

	.globl	_syscall
_trap0:
	clrw	sp@-			| pad SR to longword
	moveml	#0xFFFF,sp@-		| save user registers
	movl	usp,a0			| save the user SP
	movl	a0,sp@(60)		|   in the savearea
	movl	d0,sp@-			| push syscall number
	jbsr	_syscall		| handle it
	addql	#4,sp			| pop syscall arg
	movl	sp@(60),a0		| grab and restore
	movl	a0,usp			|   user SP
	moveml	sp@+,#0x7FFF		| restore most registers
	addql	#6,sp			| pop SSP and align word
	jra	rei			| all done

/*
 * Routines for traps 1 and 2.  The meaning of the two traps depends
 * on whether we are an HPUX compatible process or a native 4.3 process.
 * Our native 4.3 implementation uses trap 1 as sigreturn() and trap 2
 * as a breakpoint trap.  HPUX uses trap 1 for a breakpoint, so we have
 * to make adjustments so that trap 2 is used for sigreturn.
 */
_trap1:
	btst	#PCB_TRCB,_u+PCB_FLAGS+1| being traced by an HPUX process?
	jeq	sigreturn		| no, trap1 is sigreturn
	jra	_trace			| yes, trap1 is breakpoint

_trap2:
	btst	#PCB_TRCB,_u+PCB_FLAGS+1| being traced by an HPUX process?
	jeq	_trace			| no, trap2 is breakpoint
	jra	sigreturn		| yes, trap2 is sigreturn

/*
 * Trap 15 is used for:
 *	- KGDB traps
 *	- trace traps for SUN binaries (not fully supported yet)
 * We just pass it on and let trap() sort it all out
 */
_trap15:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_TRAP15,d0
	jra	_fault

/*
 * Hit a breakpoint (trap 1 or 2) instruction.
 * Push the code and treat as a normal fault.
 */
_trace:
	clrw	sp@-
	moveml	#0xFFFF,sp@-
	moveq	#T_TRACE,d0
	jra	_fault

/*
 * The sigreturn() syscall comes here.  It requires special handling
 * because we must open a hole in the stack to fill in the (possibly much
 * larger) original stack frame.
 */
sigreturn:
	lea	sp@(-84),sp		| leave enough space for largest frame
	movl	sp@(84),sp@		| move up current 8 byte frame
	movl	sp@(88),sp@(4)
	movw	#0xFFFF,sp@-		| default: must clean stack
	moveml	#0xFFFF,sp@-		| save user registers
	movl	usp,a0			| save the user SP
	movl	a0,sp@(60)		|   in the savearea
	movl	#SYS_sigreturn,sp@-	| push syscall number
	jbsr	_syscall		| handle it
	addql	#4,sp			| pop syscall#
	movl	sp@(60),a0		| grab and restore
	movl	a0,usp			|   user SP
	lea	sp@(64),a1		| pointer to HW frame
	tstw	a1@+			| do we need to clean up stack?
	jeq	Lsigr1			| no, just continue
	movb	a1@(6),d0		| grab format byte
	lsrb	#4,d0			| get rid of excess
	cmpb	#10,d0			| type 10 frame?
	jne	Lsigr2			| no, continue
	movw	#32,d1			| yes, frame size is 32 bytes
	jra	Lsigrcp			| go to it
Lsigr2:
	cmpb	#9,d0			| type 9?
	jne	Lsigr3			| no, continue
	movw	#20,d1			| yes, frame size is 20 bytes
	jra	Lsigrcp			| go to it
Lsigr3:
	cmpb	#2,d0			| type 2?
	jne	Lsigr4			| no, continue
	movw	#12,d1			| yes, frame size is 12 bytes
	jra	Lsigrcp			| go to it
Lsigr4:
	movw	#8,d1			| must be type 0/1, size is 8 bytes
Lsigrcp:
	lea	a1@(92),a0		| destination
	addw	d1,a1			| source
	lsrw	#1,d1			| convert to word count
	subqw	#1,d1			| minus 1 for dbf
Lsigrlp:
	movw	a1@-,a0@-		| copy a word
	dbf	d1,Lsigrlp		| continue
	movl	a0,a1			| new HW frame base
Lsigr1:
	movl	a1,sp@(60)		| new SP value
	moveml	sp@+,#0x7FFF		| restore user registers
	movl	sp@,sp			| and our SP
	jra	rei			| all done

/*
 * Interrupt handlers.
 * All DIO device interrupts are auto-vectored.  Most can be configured
 * to interrupt in the range IPL3 to IPL5.  Here are our assignments:
 *
 *	Level 0:	Spurious: ignored.
 *	Level 1:	HIL
 *	Level 2:
 *	Level 3:	Internal HP-IB
 *	Level 4:	"Fast" HP-IBs, SCSI
 *	Level 5:	DMA, Ethernet, Built-in RS232
 *	Level 6:	Clock
 *	Level 7:	Non-maskable: parity errors, RESET key
 */
	.globl	_hilint, _intrhand, _hardclock, _nmihand

_spurintr:
	addql	#1,_intrcnt+0
	addql	#1,_cnt+V_INTR
	jra	rei

_lev1intr:
	addql	#1,_intrcnt+4
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	jbsr	_hilint
	moveml	sp@+,#0x0303
	addql	#2,sp
	addql	#1,_cnt+V_INTR
	jra	rei

/* check for DMA first to reduce overhead */
_lev5intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
	jbsr	_dmaintr
	tstl	d0
	jeq	Lnotdma
	addql	#1,_intrcnt+24
	moveml	sp@+,#0x0303
	addql	#2,sp
	addql	#1,_cnt+V_INTR
	jra	rei

_lev2intr:
_lev3intr:
_lev4intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
Lnotdma:
	lea	_intrcnt,a0
	movw	sp@(24),d0		| use vector offset
	andw	#0xfff,d0		|   sans frame type
	addql	#1,a0@(-0x60,d0:w)	|     to increment apropos counter
	movw	sr,sp@-			| push current SR value
	clrw	sp@-			|    padded to longword
	jbsr	_intrhand		| handle interrupt
	addql	#4,sp			| pop SR
	moveml	sp@+,#0x0303
	addql	#2,sp
	addql	#1,_cnt+V_INTR
	jra	rei

_lev6intr:
	clrw	sp@-
	moveml	#0xC0C0,sp@-
#ifdef DEBUG
	.globl	_panicstr, _regdump, _panic
	tstl	timebomb		| set to go off?
	jeq	Lnobomb			| no, skip it
	subql	#1,timebomb		| decrement
	jne	Lnobomb			| not ready to go off
	moveml	sp@+,#0x0303		| temporarily restore regs
	jra	Luseours		| go die
Lnobomb:
	cmpl	#_u+NBPG+NBPG,sp	| our we still in stack page?
	jcc	Lstackok		| yes, continue normally
	tstl	_panicstr		| have we paniced?
	jne	Lstackok		| yes, do not re-panic
	moveml	sp@+,#0x0303		| no, temporarily restore regs
	cmpl	#_u+NBPG+0x400,sp	| our we safely in redzone?
	jcc	Luseours		| yes, panic with this stack
	lea	tmpstk,sp		| no, switch to tmpstk
Luseours:
	moveml	#0xFFFF,sp@-		| push all registers
	movl	sp,a0			| remember this spot
	movl	#256,sp@-		| longword count
	movl	a0,sp@-			| and reg pointer
	jbsr	_regdump		| dump core
	addql	#8,sp			| pop params
	movl	#Lstkrip,sp@-		| push panic message
	jbsr	_panic			| ES and D
Lstkrip:
	.asciz	"k-stack overflow"
	.even
Lstackok:
#endif
	movb	_IObase+CLKSR,d0	| read clock status
#ifdef PROFTIMER
	.globl  _profon
	tstb	_profon			| profile clock on?
	jeq     Ltimer1			| no, then must be timer1 interrupt
	btst	#2,d0			| timer3 interrupt?
	jeq     Ltimer1			| no, must be timer1
	movb	_IObase+CLKMSB3,d1	| clear timer3 interrupt
	lea	sp@(16),a1		| get pointer to PS
#ifdef GPROF
	.globl	_profclock
	movl	d0,sp@-			| save status so jsr will not clobber
	movl	a1@,sp@-		| push padded PS
	movl	a1@(4),sp@-		| push PC
	jbsr	_profclock		| profclock(pc, ps)
	addql	#8,sp			| pop params
#else
	btst	#5,a1@(2)		| saved PS in user mode?
	jne	Lttimer1		| no, go check timer1
	tstl	_u+U_PROFSCALE		| process being profiled?
	jeq	Lttimer1		| no, go check timer1
	movl	d0,sp@-			| save status so jsr will not clobber
	movl	#1,sp@-
	movl	#_u+U_PROF,sp@-
	movl	a1@(4),sp@-
	jbsr    _addupc			| addupc(pc, &u.u_prof, 1)
	lea	sp@(12),sp		| pop params
#endif
	addql	#1,_intrcnt+32		| add another profile clock interrupt
	movl	sp@+,d0			| get saved clock status
Lttimer1:
	btst	#0,d0			| timer1 interrupt?
	jeq     Ltimend		        | no, check state of kernel profiling
Ltimer1:
#endif
	movb	_IObase+CLKMSB1,d1	| clear timer1 interrupt
	lea	sp@(16),a1		| get pointer to PS
	movl	a1@,sp@-		| push padded PS
	movl	a1@(4),sp@-		| push PC
	jbsr	_hardclock		| call generic clock int routine
	addql	#8,sp			| pop params
	addql	#1,_intrcnt+28		| add another system clock interrupt
#ifdef PROFTIMER
Ltimend:
#ifdef GPROF
	.globl	_profiling, _startprofclock
	tstl	_profiling		| kernel profiling desired?
	jne	Ltimdone		| no, all done
	bset	#7,_profon		| mark continuous timing
	jne	Ltimdone		| was already enabled, all done
	jbsr	_startprofclock		| else turn it on
Ltimdone:
#endif
#endif
	moveml	sp@+,#0x0303		| restore scratch regs
	addql	#2,sp			| pop pad word
	addql	#1,_cnt+V_INTR		| chalk up another interrupt
	jra	rei			| all done

_lev7intr:
#ifdef PROFTIMER
	addql	#1,_intrcnt+36
#else
	addql	#1,_intrcnt+32
#endif
	clrw	sp@-			| pad SR to longword
	moveml	#0xFFFF,sp@-		| save registers
	movl	usp,a0			| and save
	movl	a0,sp@(60)		|   the user stack pointer
	jbsr	_nmihand		| call handler
	movl	sp@(60),a0		| restore
	movl	a0,usp			|   user SP
	moveml	sp@+,#0x7FFF		| and remaining registers
	addql	#6,sp			| pop SSP and align word
	jra	rei			| all done

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
	rte				| real return

/*
 * System page table
 * Mbmap, Usrptmap, and Usriomap are enlarged by CLSIZE entries
 * as they are managed by resource maps starting with index 1 or CLSIZE.
 * Usrptmap is allocated last so that we can also use the pad space up
 * to eSysmap. (no point in wasting it!)
 */ 
#define	vaddr(x)	x-_Sysmap/4*NBPG
#define SYSMAP(mname,vname,size) \
	.globl	_/**/mname,_/**/vname; \
_/**/mname: \
	.space	size*4; \
	_/**/vname = vaddr(_/**/mname)
#define	ADDMAP(npte)	.space	npte*4

	.data
	SYSMAP(Sysmap	,Sysbase	,SYSPTSIZE	)
	SYSMAP(Forkmap	,forkutl	,UPAGES		)
	SYSMAP(Xswapmap	,xswaputl	,UPAGES		)
	SYSMAP(Xswap2map,xswap2utl	,UPAGES		)
	SYSMAP(Swapmap	,swaputl	,UPAGES		)
	SYSMAP(Pushmap	,pushutl	,UPAGES		)
	SYSMAP(Vfmap	,vfutl		,UPAGES		)
	SYSMAP(CMAP1	,CADDR1		,1		)
	SYSMAP(CMAP2	,CADDR2		,1		)
	SYSMAP(mmap	,vmmap		,1		)
	SYSMAP(msgbufmap,msgbuf		,MSGBUFPTECNT	)
	SYSMAP(Umap	,u		,UPAGES		)
	SYSMAP(Mbmap	,mbutl		,NMBCLUSTERS*MCLBYTES/NBPG+CLSIZE )
	/*
	 * This is the map used by the kernel memory allocator.
	 * It is expanded as necessary by the special features
	 * that use it.
	 *
	 * XXX: NEED way to compute kmem size from maxusers,
	 * device complement
	 */
	SYSMAP(kmempt	,kmembase	,NKMEMCLUSTERS*CLSIZE )
#ifdef	SYSVSHM
				ADDMAP(	SHMMAXPGS	)
#endif
#ifdef	GPROF
				ADDMAP(	768*1024/NBPG	)
#endif
	SYSMAP(ekmempt	,kmemlimit	,0		)
	SYSMAP(IOmap	,IObase		,IOMAPSIZE	)   /* map DIO space */
	SYSMAP(eIOmap	,IOlimit	,0		)
#if defined(HP360) || defined(HP370)
	SYSMAP(Grfmap	,grfregs	,1024		)   /* 340 @ SC132 */
#endif
	SYSMAP(Usriomap	,usrio		,USRIOSIZE+CLSIZE ) /* for PHYSIO */
	SYSMAP(Usrptmap	,usrpt		,USRPTSIZE+CLSIZE )
	. = . + NBPG - 1 & -NBPG	/* align to page boundry */
eSysmap:

/*
 * System segment table.  1 page is sufficient to map the entire
 * 4Gb address space. (funny how that works out...)
 */
	.globl	_Sysseg
_Sysseg:
	.space	NBPG
eSysseg:

	.globl	_Syssize, _Usrptsize
_Syssize	= eSysmap-_Sysmap/4
_Usrptsize	= eSysmap-_Usrptmap/4

/*
 * Initialization
 *
 * A5 contains physical load point from boot
 * VBR contains zero from ROM.  Exceptions will continue to vector
 * through ROM until MMU is turned on at which time they will vector
 * through our table (vectors.s).
 */
	.comm	_lowram,4

	.text
	.globl	_edata
	.globl	_etext,_end
	.globl	start
start:
	movw	#PSL_HIGHIPL,sr		| no interrupts
	lea	_lowram,a0
	addl	a5,a0
	movl	a5,a0@			| store start of physical memory
	movl	#CACHE_OFF,d0
	movc	d0,cacr			| clear and disable on-chip cache(s)

/* determine our CPU/MMU combo - check for all regardless of kernel config */
	movl	#0x200,d0		| data freeze bit
	movc	d0,cacr			|   only exists on 68030
	movc	cacr,d0			| read it back
	tstl	d0			| zero?
	jeq	Lis68020		| yes, we have 68020
	lea	_mmutype,a0		| no, we have 68030
	addl	a5,a0
	movl	#-1,a0@			| set to reflect 68030 PMMU
	lea	_machineid,a0
	addl	a5,a0
	movl	#0x80,IOBASE+MMUCMD	| set magic cookie
	movl	IOBASE+MMUCMD,d0	| read it back
	btst	#7,d0			| cookie still on?
	jeq	Lnot370			| no, 360 or 375
	movl	#0,IOBASE+MMUCMD	| clear magic cookie
	movl	IOBASE+MMUCMD,d0	| read it back
	btst	#7,d0			| still on?
	jeq	Lisa370			| no, must be a 370
	movl	#5,a0@			| yes, must be a 340
	jra	Lstart1
Lnot370:
	movl	#3,a0@			| type is at least a 360
	movl	#0,IOBASE+MMUCMD	| clear magic cookie2
	movl	IOBASE+MMUCMD,d0	| read it back
	btst	#16,d0			| still on?
	jeq	Lstart1			| no, must be a 360
	movl	#6,a0@			| yes, must be a 345/375
	jra	Lhaspac
Lisa370:
	movl	#4,a0@			| set to 370
Lhaspac:
	lea	_ectype,a0
	addl	a5,a0
	movl	#-1,a0@			| also has a physical address cache
	jra	Lstart1
Lis68020:
	movl	#1,IOBASE+MMUCMD	| a 68020, write HP MMU location
	movl	IOBASE+MMUCMD,d0	| read it back
	btst	#0,d0			| non-zero?
	jne	Lishpmmu		| yes, we have HP MMU
	lea	_mmutype,a0
	addl	a5,a0
	movl	#1,a0@			| no, we have PMMU
	lea	_machineid,a0
	addl	a5,a0
	movl	#1,a0@			| and 330 CPU
	jra	Lstart1
Lishpmmu:
	lea	_ectype,a0		| 320 or 350
	addl	a5,a0
	movl	#1,a0@			| both have a virtual address cache
	movl	#0x80,IOBASE+MMUCMD	| set magic cookie
	movl	IOBASE+MMUCMD,d0	| read it back
	btst	#7,d0			| cookie still on?
	jeq	Lstart1			| no, just a 320
	lea	_machineid,a0
	addl	a5,a0
	movl	#2,a0@			| yes, a 350

Lstart1:
	movl	#0,IOBASE+MMUCMD	| clear out MMU again
/* initialize source/destination control registers for movs */
	moveq	#FC_USERD,d0		| user space
	movc	d0,sfc			|   as source
	movc	d0,dfc			|   and destination of transfers
/* initialize proc. 0 (system) page table */
	movl	#_Sysmap,a0		| SYSPT map addr
	addl	a5,a0			| relocate
/* text pages are read-only */
	clrl	d0			| assume load at VA 0
	movl	a5,d1			| get load PA
	andl	#PG_FRAME,d1		| convert to a page frame
	orl	#PG_RO+PG_V,d1		| mark as valid and RO
	movl	#_etext,a1		| go til end of text
Lispt1:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	addl	#NBPG,d0		| and address counter
	cmpl	a1,d0			| done yet?
	jcs	Lispt1			| no, keep going
/* data and bss are read/write */
	andl	#PG_FRAME,d1		| mask out old prot bits
	orl	#PG_RW+PG_V,d1		| mark as valid and RW
	movl	#_end,a1		| go til end of data/bss
Lispt2:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	addl	#NBPG,d0		| and address counter
	cmpl	a1,d0			| done yet?
	jcs	Lispt2			| no, keep going
/* invalidate remainder of system page table */
	movl	#eSysmap,a1		| end of map
	addl	a5,a1			| relocate
Lispt3:
	movl	#PG_NV,a0@+		| invalidate PTE
	cmpl	a1,a0			| done yet?
	jcs	Lispt3			| no, keep going
/* go back and initialize IOmap */
	movl	#_IOmap,a0		| IO map addr
	addl	a5,a0			| relocate
	movl	#_eIOmap,a1		| end of map
	addl	a5,a1			| relocate
	movl	#IOBASE,d1		| physical IO base
	andl	#PG_FRAME,d1		| mask to frame number
	orl	#PG_RW+PG_CI+PG_V,d1	| mark valid, RW and CI
Lispt4:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	cmpl	a1,a0			| done yet?
	jcs	Lispt4			| no, keep going
/* initialize proc. 0 (system) segment table */
	movl	#_Sysseg,a0		| segment table
	addl	a5,a0			| relocate
	movl	#eSysmap-_Sysmap/NBPG*4,a1 | bytes of PTEs for Sysmap
	addl	a0,a1			| make an end count
	movl	#_Sysmap,d1		| system PT addr
	addl	a5,d1			| relocate
	andl	#SG_FRAME,d1		| mask to frame number
	orl	#SG_RW+SG_V,d1		| mark as RW and valid
Lispt5:
	movl	d1,a0@+			| load STE
	addl	#NBPG,d1		| increment page frame number
	cmpl	a1,a0			| done yet?
	jcs	Lispt5			| no, keep going
/* invalidate the unused part */
	movl	#eSysseg,a1		| end of segment table
	addl	a5,a1			| relocate
Lispt6:
	movl	#SG_NV,a0@+		| invalidate STE
	cmpl	a1,a0			| done yet?
	jcs	Lispt6			| no, keep going

/*
 * Setup page table for process 0.
 *
 * We set up page table access for the kernel via Usrptmap (usrpt)
 * and access to the u-area itself via Umap (u).  First page beyond
 * kernel BSS (d0) is used for proc0 page table.  Next UPAGES pages
 * following are for u-area.
 */
	addl	a5,d0			| relocate PT address
	movl	d0,d1
	andl	#PG_FRAME,d1		| mask to page frame number
	orl	#PG_RW+PG_V,d1		| RW and valid
	movl	#_Usrptmap,a1		| get PT map address
	addl	a5,a1			| relocate
	movl	d1,a1@			| validate PTE for proc0 PT
	movl	d0,a0			| base of proc0 PT
	addl	#NBPG,d0		| plus one page yields
	movl	d0,a2			| base of u-area
/* invalidate entire page table */
Liudot1:
	movl	#PG_NV,a0@+		| invalidate PTE
	cmpl	a2,a0			| done yet?
	jcs	Liudot1			| no, keep going
/* now go back and validate u-area PTEs */
	subl	#HIGHPAGES*4,a0		| base of PTEs for u-area (p_addr)
	movl	a0,a1
	addl	#UPAGES*4,a1		| end of PTEs for u-area
	movl	d0,d1			| get base of u-area
	andl	#PG_FRAME,d1		| mask to page frame number
	orl	#PG_RW+PG_V,d1		| add valid and writable
	movl	#_Umap,a3		| address of u
	addl	a5,a3			| relocate
Liudot2:
	movl	d1,a0@+			| validate p_addr PTE
	movl	d1,a3@+			| validate u PTE
	addl	#NBPG,d1		| to next page
	cmpl	a1,a0			| done yet?
	jcs	Liudot2			| no, keep going
/* clear process 0 u-area */
	addl	#NBPG*UPAGES,d0		| end of u-area
Lclru1:
	clrl	a2@+			| clear
	cmpl	d0,a2			| done yet?
	jcs	Lclru1			| no, keep going
	movl	a2,a4			| save addr of first avail page

/*
 * Prepare to enable MMU.
 * Since the kernel is not mapped logical == physical we must insure
 * that when the MMU is turned on, all prefetched addresses (including
 * the PC) are valid.  In order guarentee that, we map the last page of
 * memory logical == physical and load up that page with enough code to
 * defeat the prefetch, then we execute the jump back to here.
 *
 * Is this all really necessary, or am I paranoid??
 */
	movl	#_Sysseg,d1		| system segment table addr
	addl	a5,d1			| relocate
	lea	_mmutype,a0
	addl	a5,a0
	tstl	a0@			| HP MMU?
	jeq	Lhpmmu2			| yes, skip
	lea	_protorp,a0
	addl	a5,a0
	movl	#0x80000202,a0@		| nolimit + share global + 4 byte PTEs
	movl	d1,a0@(4)		| + segtable address
	.long	0xf0104800		| pmove a0@,srp
	movl	#0x80000002,a0@		| reinit upper half for CRP loads
	jra	Lstploaddone		| done
Lhpmmu2:
	moveq	#PGSHIFT,d2
	lsrl	d2,d1			| convert to page frame
	movl	d1,IOBASE+MMUSSTP	| load in sysseg table register
Lstploaddone:
	movl	#eSysseg-4,a1		| last entry in sysseg table
	addl	a5,a1			| relocate
	movl	d0,d1			| use page after proc0 u for tmp PT
	andl	#SG_FRAME,d1		| mask to page frame
	orl	#SG_RW+SG_V,d1		| mark valid and writable
	movl	d1,a1@			| load in segment table
	movl	d0,a1			| page table address
	addl	#NBPG-4,a1		| move to last entry
	movl	#MAXADDR,d1		| address of last page of memory
	movl	d1,a2
	andl	#PG_FRAME,d1		| mask to page frame
	orl	#PG_RW+PG_V,d1		| mark valid and writable
	movl	d1,a1@			| store PTE in table
	movl	#Lhighcode,a1		| addr of high code
	addl	a5,a1			| relocate
	movl	#Lehighcode,a3		| end addr
	addl	a5,a3			| relocate
Lcodecopy:
	movw	a1@+,a2@+		| copy a word
	cmpl	a3,a1			| done yet?
	jcs	Lcodecopy		| no, keep going
	jmp	MAXADDR			| go for it!

Lhighcode:
	lea	_mmutype,a0
	addl	a5,a0
	tstl	a0@				| HP MMU?
	jeq	Lhpmmu3				| yes, skip
	movl	#MMU_IEN+MMU_FPE,IOBASE+MMUCMD	| enable 68881 and i-cache
	movl	#0x82c0aa00,a2@			| value to load TC with
	.long	0xf0124000			| pmove a2@,tc
	jmp	Lenab1
Lhpmmu3:
	movl	#0,IOBASE+MMUCMD		| clear external cache
	movl	#MMU_ENAB,IOBASE+MMUCMD		| turn on MMU
	jmp	Lenab1				| jmp to mapped code
Lehighcode:

/*
 * Should be running mapped from this point on
 */
Lenab1:
/* while the ROM scratch page is mapped, check for internal HP-IB in SYSFLAG */
	btst	#5,0xfffffed2		| internal HP-IB?
	jeq	Linitmem		| yes, have HP-IB continue normally
	clrl	_internalhpib		| no, clear associated address
/* init mem sizes */
Linitmem:
	movl	#MAXADDR,d1		| last page
	moveq	#PGSHIFT,d2
	lsrl	d2,d1			| convert to page (click) number
	movl	d1,_maxmem		| save as maxmem
	movl	_lowram,d0		| lowram value from ROM via boot
	lsrl	d2,d0			| convert to page number
	subl	d0,d1			| compute amount of RAM present
	movl	d1,_freemem		| save as freemem
	movl	d1,_physmem		| and physmem
/* initialize (slightly) the pcb */
	movl	#_u,a1			| proc0 u-area
	movl	a1,sp
	addl	#UPAGES*NBPG-4,sp	| set kernel stack to end of u-area
	movl	#USRSTACK-4,a2
	movl	a2,usp			| init user SP
	movl	#_usrpt,a1@(PCB_P0BR)	| p0br: SVA of text/data user PT
	clrl	a1@(PCB_P0LR)		| p0lr: 0 (does not really exist)
	movl	#_usrpt+NBPG,d0		| addr of end of PT
	subl	#P1PAGES*4,d0		| backup size of P1 region
	movl	d0,a1@(PCB_P1BR)	| p1br: P1PAGES from end of PT
	movl	#P1PAGES-HIGHPAGES,a1@(PCB_P1LR)	| p1lr: vax style
	movl	#CLSIZE,a1@(PCB_SZPT)	| page table size
	clrw	a1@(PCB_FLAGS)		| clear flags
#ifdef FPCOPROC
	clrl	a1@(PCB_FPCTX)		| ensure null FP context
	movl	a1,sp@-
	jbsr	_m68881_restore		| restore it (does not kill a1)
	addql	#4,sp
#endif
	addl	#PCB_SIGC,a1		| address of proc0 sig trampoline code
	movl	#Lsigcode,a2		| address of sig trampoline proto
Lsigc:
	movw	a2@+,a1@+		| copy
	cmpl	#Lesigcode,a2		| done yet?
	jcs	Lsigc			| no, keep going
/* flush TLB and turn on caches */
	movl	#PG_NV,eSysseg-4	| invalidate last page
	jbsr	_TBIA			| invalidate TLB
	movl	#CACHE_ON,d0
	movc	d0,cacr			| clear cache(s)
	tstl	_ectype
	jeq	Lnocache0
	orl	#MMU_CEN,_IObase+MMUCMD	| turn on external cache
Lnocache0:
/* final setup for C code */
	movw	#PSL_LOWIPL,sr		| lower SPL
	movl	d7,_boothowto		| save reboot flags
	movl	d6,_bootdev		|   and boot device
	movl	a4,d1			| addr of first available RAM
	moveq	#PGSHIFT,d2
	lsrl	d2,d1			| convert to click
	movl	d1,sp@-			| param to main
	jbsr	_main			| main(firstaddr)
	addql	#4,sp
/* proc[1] == init now running here;
 * create a null exception frame and return to user mode in icode
 */
	clrw	sp@-			| vector offset/frame type
	clrl	sp@-			| return to icode location 0
	movw	#PSL_USER,sp@-		| in user mode
	rte

/*
 * Signal "trampoline" code (18 bytes).  Invoked from RTE setup by sendsig().
 * 
 * Stack looks like:
 *
 *	sp+0 ->	signal number
 *	sp+4	signal specific code
 *	sp+8	pointer to signal context frame (scp)
 *	sp+12	address of handler
 *	sp+16	saved hardware state
 *			.
 *			.
 *	scp+0->	beginning of signal context frame
 */
Lsigcode:
	movl	sp@(12),a0		| signal handler addr	(4 bytes)
	jsr	a0@			| call signal handler	(2 bytes)
	addql	#4,sp			| pop signo		(2 bytes)
	trap	#1			| special syscall entry	(2 bytes)
	movl	d0,sp@(4)		| save errno		(4 bytes)
	moveq	#1,d0			| syscall == exit	(2 bytes)
	trap	#0			| exit(errno)		(2 bytes)
Lesigcode:

/*
 * Icode is copied out to process 1 to exec init.
 * If the exec fails, process 1 exits.
 */
	argv1 = argv-_icode
	init1 = init-_icode
	.globl	_icode,_initflags,_szicode
_icode:
	movl	#argv1,sp@-
	movl	#init1,sp@-
	clrl	sp@-
	moveq	#SYS_execv,d0
	trap	#0
	moveq	#SYS_exit,d0
	trap	#0

init:
	.asciz	"/sbin/init"
	.even
_initflags:
	.long	0
argv:
	.long	init+6-_icode
	.long	_initflags-_icode
	.long	0
_szicode:
	.long	_szicode-_icode

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
 * update profiling information for the user
 * addupc(pc, &u.u_prof, ticks)
 */
ENTRY(addupc)
	movl	a2,sp@-			| scratch register
	movl	sp@(12),a2		| get &u.u_prof
	movl	sp@(8),d0		| get user pc
	subl	a2@(8),d0		| pc -= pr->pr_off
	jlt	Lauexit			| less than 0, skip it
	movl	a2@(12),d1		| get pr->pr_scale
	lsrl	#1,d0			| pc /= 2
	lsrl	#1,d1			| scale /= 2
	mulul	d1,d0			| pc /= scale
	moveq	#14,d1
	lsrl	d1,d0			| pc >>= 14
	bclr	#0,d0			| pc &= ~1
	cmpl	a2@(4),d0		| too big for buffer?
	jge	Lauexit			| yes, screw it
	addl	a2@,d0			| no, add base
	movl	d0,sp@-			| push address
	jbsr	_fusword		| grab old value
	movl	sp@+,a0			| grab address back
	cmpl	#-1,d0			| access ok
	jeq	Lauerror		| no, skip out
	addw	sp@(18),d0		| add tick to current value
	movl	d0,sp@-			| push value
	movl	a0,sp@-			| push address
	jbsr	_susword		| write back new value
	addql	#8,sp			| pop params
	tstl	d0			| fault?
	jeq	Lauexit			| no, all done
Lauerror:
	clrl	a2@(12)			| clear scale (turn off prof)
Lauexit:
	movl	sp@+,a2			| restore scratch reg
	rts

/*
 * copyinstr(fromaddr, toaddr, maxlength, &lencopied)
 *
 * Copy a null terminated string from the user address space into
 * the kernel address space.
 * NOTE: maxlength must be < 64K
 */
ENTRY(copyinstr)
	movl	sp@(4),a0		| a0 = fromaddr
	movl	sp@(8),a1		| a1 = toaddr
	moveq	#0,d0
	movw	sp@(14),d0		| d0 = maxlength
	jlt	Lcisflt1		| negative count, error
	jeq	Lcisdone		| zero count, all done
	movl	#Lcisflt1,_u+PCB_ONFAULT | set up to catch faults
	subql	#1,d0			| set up for dbeq
Lcisloop:
	movsb	a0@+,d1			| grab a byte
	movb	d1,a1@+			| copy it
	dbeq	d0,Lcisloop		| if !null and more, continue
	jne	Lcisflt2		| ran out of room, error
	moveq	#0,d0			| got a null, all done
Lcisdone:
	tstl	sp@(16)			| return length desired?
	jeq	Lcisret			| no, just return
	subl	sp@(4),a0		| determine how much was copied
	movl	sp@(16),a1		| return location
	movl	a0,a1@			| stash it
Lcisret:
	clrl	_u+PCB_ONFAULT		| clear fault addr
	rts
Lcisflt1:
	moveq	#EFAULT,d0		| copy fault
	jra	Lcisdone
Lcisflt2:
	moveq	#ENOENT,d0		| ran out of space
	jra	Lcisdone	

/*
 * copyoutstr(fromaddr, toaddr, maxlength, &lencopied)
 *
 * Copy a null terminated string from the kernel
 * address space to the user address space.
 * NOTE: maxlength must be < 64K
 */
ENTRY(copyoutstr)
	movl	sp@(4),a0		| a0 = fromaddr
	movl	sp@(8),a1		| a1 = toaddr
	moveq	#0,d0
	movw	sp@(14),d0		| d0 = maxlength
	jlt	Lcosflt1		| negative count, error
	jeq	Lcosdone		| zero count, all done
	movl	#Lcosflt1,_u+PCB_ONFAULT| set up to catch faults
	subql	#1,d0			| set up for dbeq
Lcosloop:
	movb	a0@+,d1			| grab a byte
	movsb	d1,a1@+			| copy it
	dbeq	d0,Lcosloop		| if !null and more, continue
	jne	Lcosflt2		| ran out of room, error
	moveq	#0,d0			| got a null, all done
Lcosdone:
	tstl	sp@(16)			| return length desired?
	jeq	Lcosret			| no, just return
	subl	sp@(4),a0		| determine how much was copied
	movl	sp@(16),a1		| return location
	movl	a0,a1@			| stash it
Lcosret:
	clrl	_u+PCB_ONFAULT		| clear fault addr
	rts
Lcosflt1:
	moveq	#EFAULT,d0		| copy fault
	jra	Lcosdone
Lcosflt2:
	moveq	#ENOENT,d0		| ran out of space
	jra	Lcosdone	

/*
 * copystr(fromaddr, toaddr, maxlength, &lencopied)
 *
 * Copy a null terminated string from one point to another in
 * the kernel address space.
 * NOTE: maxlength must be < 64K
 */
ENTRY(copystr)
	movl	sp@(4),a0		| a0 = fromaddr
	movl	sp@(8),a1		| a1 = toaddr
	moveq	#0,d0
	movw	sp@(14),d0		| d0 = maxlength
	jlt	Lcsflt1			| negative count, error
	jeq	Lcsdone			| zero count, all done
	movl	#Lcsflt1,_u+PCB_ONFAULT	| set up to catch faults
	subql	#1,d0			| set up for dbeq
Lcsloop:
	movb	a0@+,a1@+		| copy a byte
	dbeq	d0,Lcsloop		| if !null and more, continue
	jne	Lcsflt2			| ran out of room, error
	moveq	#0,d0			| got a null, all done
Lcsdone:
	tstl	sp@(16)			| return length desired?
	jeq	Lcsret			| no, just return
	subl	sp@(4),a0		| determine how much was copied
	movl	sp@(16),a1		| return location
	movl	a0,a1@			| stash it
Lcsret:
	clrl	_u+PCB_ONFAULT		| clear fault addr
	rts
Lcsflt1:
	moveq	#EFAULT,d0		| copy fault
	jra	Lcsdone
Lcsflt2:
	moveq	#ENOENT,d0		| ran out of space
	jra	Lcsdone	

/* 
 * Copyin(from, to, len)
 *
 * Copy specified amount of data from user space into the kernel.
 * NOTE: len must be < 64K
 */
ENTRY(copyin)
	movl	d2,sp@-			| scratch register
	movl	#Lciflt,_u+PCB_ONFAULT	| catch faults
	movl	sp@(16),d2		| check count
	jlt	Lciflt			| negative, error
	jeq	Lcidone			| zero, done
	movl	sp@(8),a0		| src address
	movl	sp@(12),a1		| dest address
	movl	a0,d0
	btst	#0,d0			| src address odd?
	jeq	Lcieven			| no, go check dest
	movsb	a0@+,d1			| yes, get a byte
	movb	d1,a1@+			| put a byte
	subql	#1,d2			| adjust count
	jeq	Lcidone			| exit if done
Lcieven:
	movl	a1,d0
	btst	#0,d0			| dest address odd?
	jne	Lcibyte			| yes, must copy by bytes
	movl	d2,d0			| no, get count
	lsrl	#2,d0			| convert to longwords
	jeq	Lcibyte			| no longwords, copy bytes
	subql	#1,d0			| set up for dbf
Lcilloop:
	movsl	a0@+,d1			| get a long
	movl	d1,a1@+			| put a long
	dbf	d0,Lcilloop		| til done
	andl	#3,d2			| what remains
	jeq	Lcidone			| all done
Lcibyte:
	subql	#1,d2			| set up for dbf
Lcibloop:
	movsb	a0@+,d1			| get a byte
	movb	d1,a1@+			| put a byte
	dbf	d2,Lcibloop		| til done
Lcidone:
	moveq	#0,d0			| success
Lciexit:
	clrl	_u+PCB_ONFAULT		| reset fault catcher
	movl	sp@+,d2			| restore scratch reg
	rts
Lciflt:
	moveq	#EFAULT,d0		| got a fault
	jra	Lciexit

/* 
 * Copyout(from, to, len)
 *
 * Copy specified amount of data from kernel to the user space
 * NOTE: len must be < 64K
 */
ENTRY(copyout)
	movl	d2,sp@-			| scratch register
	movl	#Lcoflt,_u+PCB_ONFAULT	| catch faults
	movl	sp@(16),d2		| check count
	jlt	Lcoflt			| negative, error
	jeq	Lcodone			| zero, done
	movl	sp@(8),a0		| src address
	movl	sp@(12),a1		| dest address
	movl	a0,d0
	btst	#0,d0			| src address odd?
	jeq	Lcoeven			| no, go check dest
	movb	a0@+,d1			| yes, get a byte
	movsb	d1,a1@+			| put a byte
	subql	#1,d2			| adjust count
	jeq	Lcodone			| exit if done
Lcoeven:
	movl	a1,d0
	btst	#0,d0			| dest address odd?
	jne	Lcobyte			| yes, must copy by bytes
	movl	d2,d0			| no, get count
	lsrl	#2,d0			| convert to longwords
	jeq	Lcobyte			| no longwords, copy bytes
	subql	#1,d0			| set up for dbf
Lcolloop:
	movl	a0@+,d1			| get a long
	movsl	d1,a1@+			| put a long
	dbf	d0,Lcolloop		| til done
	andl	#3,d2			| what remains
	jeq	Lcodone			| all done
Lcobyte:
	subql	#1,d2			| set up for dbf
Lcobloop:
	movb	a0@+,d1			| get a byte
	movsb	d1,a1@+			| put a byte
	dbf	d2,Lcobloop		| til done
Lcodone:
	moveq	#0,d0			| success
Lcoexit:
	clrl	_u+PCB_ONFAULT		| reset fault catcher
	movl	sp@+,d2			| restore scratch reg
	rts
Lcoflt:
	moveq	#EFAULT,d0		| got a fault
	jra	Lcoexit

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

/*
 * The following primitives manipulate the run queues.
 * _whichqs tells which of the 32 queues _qs
 * have processes in them.  Setrq puts processes into queues, Remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

	.globl	_whichqs,_qs,_cnt,_panic
	.comm	_noproc,4
	.comm	_runrun,4

/*
 * Setrq(p)
 *
 * Call should be made at spl6(), and p->p_stat should be SRUN
 */
ENTRY(setrq)
	movl	sp@(4),a0
	tstl	a0@(P_RLINK)
	jeq	Lset1
	movl	#Lset2,sp@-
	jbsr	_panic
Lset1:
	clrl	d0
	movb	a0@(P_PRI),d0
	lsrb	#2,d0
	movl	_whichqs,d1
	bset	d0,d1
	movl	d1,_whichqs
	lslb	#3,d0
	addl	#_qs,d0
	movl	d0,a0@(P_LINK)
	movl	d0,a1
	movl	a1@(P_RLINK),a0@(P_RLINK)
	movl	a0,a1@(P_RLINK)
	movl	a0@(P_RLINK),a1
	movl	a0,a1@(P_LINK)
	rts

Lset2:
	.asciz	"setrq"
	.even

/*
 * Remrq(p)
 *
 * Call should be made at spl6().
 */
ENTRY(remrq)
	movl	sp@(4),a0
	clrl	d0
	movb	a0@(P_PRI),d0
	lsrb	#2,d0
	movl	_whichqs,d1
	bclr	d0,d1
	jne	Lrem1
	movl	#Lrem3,sp@-
	jbsr	_panic
Lrem1:
	movl	d1,_whichqs
	movl	a0@(P_LINK),a1
	movl	a0@(P_RLINK),a1@(P_RLINK)
	movl	a0@(P_RLINK),a1
	movl	a0@(P_LINK),a1@(P_LINK)
	movl	#_qs,a1
	movl	d0,d1
	lslb	#3,d1
	addl	d1,a1
	cmpl	a1@(P_LINK),a1
	jeq	Lrem2
	movl	_whichqs,d1
	bset	d0,d1
	movl	d1,_whichqs
Lrem2:
	clrl	a0@(P_RLINK)
	rts

Lrem3:
	.asciz	"remrq"
Lsw0:
	.asciz	"swtch"
	.even

/*
 * When no processes are on the runq, Swtch branches to idle
 * to wait for something to come ready.
 */
	.globl	Idle
Idle:
idle:
	movw	#PSL_LOWIPL,sr
	tstl	_whichqs
	jne	Lsw1
	stop	#PSL_LOWIPL
	jra	idle

Lbadsw:
	movl	#Lsw0,sp@-
	jbsr	_panic
	/*NOTREACHED*/

/*
 * Swtch()
 */
ENTRY(swtch)
	movw	sr,_u+PCB_PS
	movl	#1,_noproc
	addql	#1,_cnt+V_SWTCH
Lsw1:
	clrl	d0
	movl	_whichqs,d1
Lswchk:
	btst	d0,d1
	jne	Lswfnd
	addqb	#1,d0
	cmpb	#32,d0
	jne	Lswchk
	jra	idle
Lswfnd:
	movw	#PSL_HIGHIPL,sr
	movl	_whichqs,d1
	bclr	d0,d1
	jeq	Lsw1
	movl	d1,_whichqs
	movl	d0,d1
	lslb	#3,d1
	addl	#_qs,d1
	movl	d1,a1
	cmpl	a1@(P_LINK),a1
	jeq	Lbadsw
	movl	a1@(P_LINK),a0
	movl	a0@(P_LINK),a1@(P_LINK)
	movl	a0@(P_LINK),a1
	movl	a0@(P_RLINK),a1@(P_RLINK)
	cmpl	a0@(P_LINK),d1
	jeq	Lsw2
	movl	_whichqs,d1
	bset	d0,d1
	movl	d1,_whichqs
Lsw2:
	clrl	_noproc
	clrl	_runrun
	tstl	a0@(P_WCHAN)
	jne	Lbadsw
	cmpb	#SRUN,a0@(P_STAT)
	jne	Lbadsw
	clrl	a0@(P_RLINK)
	movl	a0@(P_ADDR),d0
	jra	Lres0

/*
 * Resume(p_addr)
 *
 * NOTE: on the PMMU we attempt to avoid flushing the entire TAC.
 * The effort involved in selective flushing may not be worth it,
 * maybe we should just flush the whole thing?
 */
ENTRY(resume)
	movw	sr,_u+PCB_PS
	movl	sp@(4),d0
Lres0:
	lea	_u,a1			| &u
	movl	usp,a0			| grab USP
	movl	a0,a1@(PCB_USP)		| and save it
	moveml	#0xFCFC,a1@(PCB_REGS)	| save non-scratch registers
#ifdef FPCOPROC
	lea	a1@(PCB_FPCTX),a0	| pointer to FP save area
	.word	0xf310			| fsave a0@
	tstb	a0@			| null state frame?
	jeq	Lresnofpsave		| yes, all done
	.word	0xf228,0xf0ff,0x00d8	| fmovem fp0-fp7,a0@(216)
	.word	0xf228,0xbc00,0x0138	| fmovem fpcr/fpsr/fpiar,a0@(312)
Lresnofpsave:
#endif
#ifdef PROFTIMER
	movw	#SPL6,sr		| protect against clock interrupts
	bclr	#0,_profon		| clear user profiling bit, was set?
	jeq	Lskipoff		| no, clock off or doing kernel only
#ifdef GPROF
	tstb	_profon			| kernel profiling also enabled?
	jlt	Lskipoff		| yes, nothing more to do
#endif
	movb	#0,_IObase+CLKCR2	| no, just user, select CR3
	movb	#0,_IObase+CLKCR3	| and turn it off
Lskipoff:
#endif
	movl	_CMAP2,a1@(PCB_CMAP2)	| save temporary map PTE
	movw	#PSL_HIGHIPL,sr		| go crit while changing PTEs
	lea	tmpstk,sp		| now goto a tmp stack for NMI
	movl	d0,a0			| address of new context
	lea	_Umap,a1		| address of PTEs for u
	moveq	#UPAGES-1,d0		| sizeof u
Lres1:
	movl	a0@+,d1			| get PTE
	andl	#~PG_PROT,d1		| mask out old protection
	orl	#PG_RW+PG_V,d1		| ensure valid and writable
	movl	d1,a1@+			| load it up
	dbf	d0,Lres1		| til done
	lea	_u,a1			| reload &u
	movl	#CACHE_CLR,d0
	movc	d0,cacr			| invalidate cache(s)
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu4			| yes, skip
	jmi	Lnot68851a		| must flush all on 68030 MMU
#ifdef DEBUG
	tstl	fullflush		| 68851, conservative?
	jne	Lnot68851a		| yes, go flush all
#endif
	.long	0xf0003494		| no, pflushs #4,#4 (flush super side)
	jra	Lres2
Lnot68851a:
	.long	0xf0002400		| pflusha
Lres2:
	movl	a1@(PCB_USTP),d0	| get USTP
	moveq	#PGSHIFT,d1
	lsll	d1,d0			| convert to addr
	lea	_protorp,a0		| CRP prototype
	movl	d0,a0@(4)		| stash USTP
	.long	0xf0104C00		| pmove a0@,crp
	jra	Lcxswdone		| thats it
Lhpmmu4:	
#endif
#if defined(HP320) || defined(HP350)
	movl	_IObase+MMUTBINVAL,d1	| invalidate TLB
	tstl	_ectype			| got external VAC?
	jle	Lnocache1		| no, skip
	movl	#_IObase+MMUCMD,a0	| addr of MMU command register
	andl	#~MMU_CEN,a0@		| toggle cache enable
	orl	#MMU_CEN,a0@		| to clear data cache
Lnocache1:
	movl	a1@(PCB_USTP),_IObase+MMUUSTP	| context switch
#endif
Lcxswdone:
	movl	a1@(U_PROCP),a0		| u.u_procp
	bclr	#SPTECHGB-24,a0@(P_FLAG)| clear SPTECHG bit
#if defined(HP330)
	jeq	Lnot68851b		| if set need to flush user TLB
	tstl	_mmutype		| 68851 PMMU?
	jle	Lnot68851b		| no, skip
	.long	0xf0003490		| pflushs #0,#4
Lnot68851b:
#endif
	movl	a1@(PCB_CMAP2),_CMAP2	| reload tmp map
	moveml	a1@(PCB_REGS),#0xFCFC	| and registers
	movl	a1@(PCB_USP),a0
	movl	a0,usp			| and USP
#ifdef PROFTIMER
	tstl	a1@(U_PROFSCALE)	| process being profiled?
	jeq	Lskipon			| no, do nothing
	orb	#1,_profon		| turn on user profiling bit
#ifdef GPROF
	jlt	Lskipon			| already profiling kernel, all done
#endif
	lea	_IObase+CLKMSB3,a0	| address timer 3 counter
	movl	_profint,d1		| profiling interval
	subql	#1,d1			|   adjusted
	movepw	d1,a0@(0)		| set interval
	movb	#0,_IObase+CLKCR2	| select CR3
	movb	#64,_IObase+CLKCR3	| turn it on
Lskipon:
#endif
#ifdef FPCOPROC
	lea	a1@(PCB_FPCTX),a0	| pointer to FP save area
	tstb	a0@			| null state frame?
	jeq	Lresfprest		| yes, easy
	.word	0xf228,0x9c00,0x0138	| fmovem a0@(312),fpcr/fpsr/fpiar
	.word	0xf228,0xd0ff,0x00d8	| fmovem a0@(216),fp0-fp7
Lresfprest:
	.word	0xf350			| frestore a0@
#endif
	tstl	a1@(PCB_SSWAP)		| do an alternate return?
	jne	Lres3			| yes, go reload regs
	movw	a1@(PCB_PS),sr		| no, restore PS
	rts
Lres3:
	movl	a1@(PCB_SSWAP),a0	| addr of saved context
	clrl	a1@(PCB_SSWAP)		| clear flag
	moveml	a0@+,#0x7CFC		| restore registers
	movl	a0@+,a1			| and SP
	cmpl	sp,a1			| paranoia...
	jge	Lres4			| ...must be popping, yes?
	lea	tmpstk,sp		| no! set up a legit stack
	movl	#Lres5,sp@-		| push a panic message
	jbsr	_panic			| and panic
	/* NOTREACHED */
Lres4:
	movl	a1,sp			| restore SP
	movl	a0@,sp@			| and PC
	moveq	#1,d0			| arrange for non-zero return
	movw	#PSL_LOWIPL,sr		| lower SPL
	rts

Lres5:
	.asciz	"ldctx"
	.even

/*
 * {fu,su},{byte,sword,word}
 */
ALTENTRY(fuiword, _fuword)
ENTRY(fuword)
	movl	sp@(4),d0		| address to read
	btst	#0,d0			| is it odd?
	jne	Lfserr			| yes, a fault
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	d0,a0
	movsl	a0@,d0			| do read from user space
	jra	Lfsdone

ENTRY(fusword)
	movl	sp@(4),d0
	btst	#0,d0			| is address odd?
	jne	Lfserr			| yes, a fault
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	d0,a0			| address to read
	moveq	#0,d0
	movsw	a0@,d0			| do read from user space
	jra	Lfsdone

ALTENTRY(fuibyte, _fubyte)
ENTRY(fubyte)
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	sp@(4),a0		| address to read
	moveq	#0,d0
	movsb	a0@,d0			| do read from user space
	jra	Lfsdone

Lfserr:
	moveq	#-1,d0			| error indicator
Lfsdone:
	clrl	_u+PCB_ONFAULT		| clear fault address
	rts

ALTENTRY(suiword, _suword)
ENTRY(suword)
	movl	sp@(4),d0		| address to write
	btst	#0,d0			| is it odd?
	jne	Lfserr			| yes, a fault
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	d0,a0			| address to write
	movl	sp@(8),d0		| value to put there
	movsl	d0,a0@			| do write to user space
	moveq	#0,d0			| indicate no fault
	jra	Lfsdone

ENTRY(susword)
	movl	sp@(4),d0		| address to write
	btst	#0,d0			| is it odd?
	jne	Lfserr			| yes, a fault
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	d0,a0			| address to write
	movw	sp@(10),d0		| value to put there
	movsw	d0,a0@			| do write to user space
	moveq	#0,d0			| indicate no fault
	jra	Lfsdone

ALTENTRY(suibyte, _subyte)
ENTRY(subyte)
	movl	#Lfserr,_u+PCB_ONFAULT	| where to return to on a fault
	movl	sp@(4),a0		| address to write
	movb	sp@(11),d0		| value to put there
	movsb	d0,a0@			| do write to user space
	moveq	#0,d0			| indicate no fault
	jra	Lfsdone

/*
 * Copy 1 relocation unit (NBPG bytes)
 * from user virtual address to physical address
 */
ENTRY(copyseg)
	movl	sp@(8),d0			| destination page number
	moveq	#PGSHIFT,d1
	lsll	d1,d0				| convert to address
	orl	#PG_CI+PG_RW+PG_V,d0		| make sure valid and writable
	movl	d0,_CMAP2			| load in page table
	movl	#_CADDR2,sp@-			| destination kernel VA
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp
	movl	#_CADDR2,a1			| destination addr
	movl	sp@(4),a0			| source addr
	movl	#NBPG/4-1,d0			| count
	movl	#Lcpydone,_u+PCB_ONFAULT	| where to go on a fault
Lcpyloop:
	movsl	a0@+,d1				| read longword
	movl	d1,a1@+				| write longword
	dbf	d0,Lcpyloop			| continue until done
Lcpydone:
	clrl	_u+PCB_ONFAULT			| clear error catch
	rts

/*
 * zero out physical memory
 * specified in relocation units (NBPG bytes)
 */
ENTRY(clearseg)
	movl	sp@(4),d0			| destination page number
	moveq	#PGSHIFT,d1
	lsll	d1,d0				| convert to address
	orl	#PG_CI+PG_RW+PG_V,d0		| make sure valid and writable
	movl	d0,_CMAP1			| load in page map
	movl	#_CADDR1,sp@-			| destination kernel VA
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp
	movl	#_CADDR1,a1			| destination addr
	movl	#NBPG/4-1,d0			| count
/* simple clear loop is fastest on 68020 */
Lclrloop:
	clrl	a1@+				| clear a longword
	dbf	d0,Lclrloop			| continue til done
	rts

/*
 * Invalidate entire TLB.
 */
ENTRY(TBIA)
__TBIA:
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu6			| yes, skip
	.long	0xf0002400		| no, pflusha
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851a		| 68851 implies no d-cache
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
Lmc68851a:
#endif
	rts
Lhpmmu6:
#endif
#if defined(HP320) || defined(HP350)
	movl	_IObase+MMUTBINVAL,sp@-	| do not ask me, this
	addql	#4,sp			|   is how hpux does it
	jra	__DCIA			| XXX: invalidate entire cache
#endif
	rts

/*
 * Invalidate any TLB entry for given VA (TB Invalidate Single)
 */
ENTRY(TBIS)
#ifdef DEBUG
	tstl	fullflush		| being conservative?
	jne	__TBIA			| yes, flush entire TLB
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu5			| yes, skip
	movl	sp@(4),a0		| get addr to flush
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851b		| is 68851?
	.long	0xf0103810		| pflush #0,#0,a0@
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip data cache
	rts
Lmc68851b:
#endif
	.long	0xf0103c10		| pflushs #0,#0,a0@
	rts
Lhpmmu5:
#endif
#if defined(HP320) || defined(HP350)
	movl	sp@(4),d0		| VA to invalidate
	bclr	#0,d0			| ensure even
	movl	d0,a0
	movw	sr,d1			| go critical
	movw	#PSL_HIGHIPL,sr		|   while in purge space
	moveq	#FC_PURGE,d0		| change address space
	movc	d0,dfc			|   for destination
	moveq	#0,d0			| zero to invalidate?
	movsl	d0,a0@			| hit it
	moveq	#FC_USERD,d0		| back to old
	movc	d0,dfc			|   address space
	movw	d1,sr			| restore IPL
#endif
	rts

/*
 * Invalidate supervisor side of TLB
 */
ENTRY(TBIAS)
#ifdef DEBUG
	tstl	fullflush		| being conservative?
	jne	__TBIA			| yes, flush everything
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu7			| yes, skip
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851c		| 68851?
	.long	0xf0003094		| pflush #4,#4
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts
Lmc68851c:
#endif
	.long	0xf0003494		| pflushs #4,#4
	rts
Lhpmmu7:
#endif
#if defined(HP320) || defined(HP350)
	movl	#0x8000,d0		| more
	movl	d0,_IObase+MMUTBINVAL	|   HP magic
	jra	__DCIS			| XXX: invalidate entire sup. cache
#endif
	rts

/*
 * Invalidate user side of TLB
 */
ENTRY(TBIAU)
#ifdef DEBUG
	tstl	fullflush		| being conservative?
	jne	__TBIA			| yes, flush everything
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu8			| yes, skip
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851d		| 68851?
	.long	0xf0003090		| pflush #0,#4
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts
Lmc68851d:
#endif
	.long	0xf0003490		| pflushs #0,#4
	rts
Lhpmmu8:
#endif
#if defined(HP320) || defined(HP350)
	moveq	#0,d0			| more
	movl	d0,_IObase+MMUTBINVAL	|   HP magic
	jra	__DCIU			| XXX: invalidate entire user cache
#endif
	rts

/*
 * Invalidate instruction cache
 */
ENTRY(ICIA)
	movl	#IC_CLEAR,d0
	movc	d0,cacr			| invalidate i-cache
	rts

/*
 * Invalidate data cache.
 * HP external cache allows for invalidation of user/supervisor portions.
 */
ENTRY(DCIA)
__DCIA:
#if defined(HP360) || defined(HP370)
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
#endif
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache2		| no, all done
	movl	#_IObase+MMUCMD,a0	| MMU control reg
	andl	#~MMU_CEN,a0@		| disable cache
	orl	#MMU_CEN,a0@		| reenable cache
Lnocache2:
#endif
	rts

ENTRY(DCIS)
__DCIS:
#if defined(HP360) || defined(HP370)
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
#endif
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache3		| no, all done
	movl	_IObase+MMUSSTP,d0	| read the supervisor STP
	movl	d0,_IObase+MMUSSTP	| write it back
Lnocache3:
#endif
	rts

ENTRY(DCIU)
__DCIU:
#if defined(HP360) || defined(HP370)
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
#endif
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache4		| no, all done
	movl	_IObase+MMUUSTP,d0	| read the user STP
	movl	d0,_IObase+MMUUSTP	| write it back
Lnocache4:
#endif
	rts

#if defined(HP370)
ENTRY(PCIA)
	tstl	_ectype			| got external PAC?
	jge	Lnocache6		| no, all done
	movl	#_IObase+MMUCMD,a0	| MMU control reg
	andl	#~MMU_CEN,a0@		| disable cache
	orl	#MMU_CEN,a0@		| reenable cache
Lnocache6:
	rts
#endif

ENTRY(ecacheon)
	tstl	_ectype
	jeq	Lnocache7
	movl	#_IObase+MMUCMD,a0
	orl	#MMU_CEN,a0@
Lnocache7:
	rts

ENTRY(ecacheoff)
	tstl	_ectype
	jeq	Lnocache8
	movl	#_IObase+MMUCMD,a0
	andl	#~MMU_CEN,a0@
Lnocache8:
	rts

	.globl	_getsfc, _getdfc
_getsfc:
	movc	sfc,d0
	rts
_getdfc:
	movc	dfc,d0
	rts

/*
 * Load a new user segment table pointer.
 */
ENTRY(loadustp)
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu9			| yes, skip
	movl	sp@(4),d0		| new USTP
	moveq	#PGSHIFT,d1
	lsll	d1,d0			| convert to addr
	lea	_protorp,a0		| CRP prototype
	movl	d0,a0@(4)		| stash USTP
	.long	0xf0104C00		| pmove a0@,crp
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts				|   since pmove flushes TLB
Lhpmmu9:
#endif
#if defined(HP320) || defined(HP350)
	movl	sp@(4),_IObase+MMUUSTP	| load a new USTP
#endif
	rts

/*
 * Flush any hardware context associated with given USTP.
 * Only does something for HP330 where we must flush RPT
 * and ATC entries in PMMU.
 */
ENTRY(flushustp)
#if defined(HP330)
	tstl	_mmutype		| 68851 PMMU?
	jle	Lnot68851		| no, nothing to do
	movl	sp@(4),d0		| get USTP to flush
	moveq	#PGSHIFT,d1
	lsll	d1,d0			| convert to address
	movl	d0,_protorp+4		| stash USTP
	.long	0xf039a000,_protorp	| pflushr _protorp
Lnot68851:
#endif
	rts

ENTRY(ploadw)
#if defined(HP330) || defined(HP360) || defined(HP370)
	movl	sp@(4),a0		| address to load
	.long	0xf0102011		| pload #1,a0@
#endif
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
	jra	Lsplsir

ENTRY(splx)
	moveq	#0,d0
	movw	sr,d0			| get current SR for return
	movw	sp@(6),d1		| get new value
	movw	d1,sr			| restore new SR
	andw	#PSL_IPL7,d1		| mask all but PSL_IPL
	jne	Lspldone		| non-zero, all done
Lsplsir:
	tstb	_ssir			| software interrupt pending?
	jeq	Lspldone		| no, all done
	subql	#4,sp			| make room for RTE frame
	movl	sp@(4),sp@(2)		| position return address
	clrw	sp@(6)			| set frame type 0
	movw	#PSL_LOWIPL,sp@		| and new SR
	jra	Lgotsir			| go handle it
Lspldone:
	rts

ALTENTRY(splsoftclock, _spl1)
ALTENTRY(splnet, _spl1)
ENTRY(spl1)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL1,sr
	rts

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

ALTENTRY(splimp, _spl5)
ALTENTRY(splbio, _spl5)
ALTENTRY(spltty, _spl5)
ENTRY(spl5)
	moveq	#0,d0
	movw	sr,d0
	movw	#SPL5,sr
	rts

ALTENTRY(splclock, _spl6)
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

#ifdef GPROF
/*
 * Special versions of splhigh and splx called by mcount().
 * Note that __splx does not check for software interrupts.
 */
	.globl	__splhigh, __splx
__splhigh:
	moveq	#0,d0
	movw	sr,d0
	movw	#PSL_HIGHIPL,sr
	rts

__splx:
	moveq	#0,d0
	movw	sr,d0			| get current SR for return
	movw	sp@(6),d1		| get new value
	movw	d1,sr			| restore new SR
	rts
#endif

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
	jeq	1$
	movl	a0,d1
	btst	#0,d1
	jeq	2$
	clrb	a0@+
	subql	#1,d0
	jeq	1$
2$:
	movl	d0,d1
	andl	#31,d0
	lsrl	#5,d1
	jeq	3$
4$:
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	subql	#1,d1
	jne	4$
	tstl	d0
	jeq	1$
3$:
	clrb	a0@+
	subql	#1,d0
	jne	3$
1$:
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

/*
 * Emulate fancy VAX string operations:
 *	scanc(count, startc, table, mask)
 *	skpc(mask, count, startc)
 *	locc(mask, count, startc)
 */
ENTRY(scanc)
	movl	sp@(4),d0	| get length
	jeq	Lscdone		| nothing to do, return
	movl	sp@(8),a0	| start of scan
	movl	sp@(12),a1	| table to compare with
	movb	sp@(19),d1	| and mask to use
	movw	d2,sp@-		| need a scratch register
	clrw	d2		| clear it out
	subqw	#1,d0		| adjust for dbra
Lscloop:
	movb	a0@+,d2		| get character
	movb	a1@(0,d2:w),d2	| get table entry
	andb	d1,d2		| mask it
	dbne	d0,Lscloop	| keep going til no more or non-zero
	addqw	#1,d0		| overshot by one
	movw	sp@+,d2		| restore scratch
Lscdone:
	rts

ENTRY(skpc)
	movl	sp@(8),d0	| get length
	jeq	Lskdone		| nothing to do, return
	movb	sp@(7),d1	| mask to use
	movl	sp@(12),a0	| where to start
	subqw	#1,d0		| adjust for dbcc
Lskloop:
	cmpb	a0@+,d1		| compate with mask
	dbne	d0,Lskloop	| keep going til no more or zero
	addqw	#1,d0		| overshot by one
Lskdone:
	rts

ENTRY(locc)
	movl	sp@(8),d0	| get length
	jeq	Llcdone		| nothing to do, return
	movb	sp@(7),d1	| mask to use
	movl	sp@(12),a0	| where to start
	subqw	#1,d0		| adjust for dbcc
Llcloop:
	cmpb	a0@+,d1		| compate with mask
	dbeq	d0,Llcloop	| keep going til no more or non-zero
	addqw	#1,d0		| overshot by one
Llcdone:
	rts

/*
 * Emulate VAX FFS (find first set) instruction.
 */
ENTRY(ffs)
	moveq	#-1,d0
	movl	sp@(4),d1
	beq	Lffsdone
Lffsloop:
	addql	#1,d0
	btst	d0,d1
	beq	Lffsloop
Lffsdone:
	addql	#1,d0
	rts

#ifdef FPCOPROC
/*
 * Save and restore 68881 state.
 * Pretty awful looking since our assembler does not
 * recognize FP mnemonics.
 */
ENTRY(m68881_save)
	movl	sp@(4),a0		| save area pointer
	.word	0xf310			| fsave a0@
	tstb	a0@			| null state frame?
	jeq	Lm68881sdone		| yes, all done
	.word	0xf228,0xf0ff,0x00d8	| fmovem fp0-fp7,a0@(216)
	.word	0xf228,0xbc00,0x0138	| fmovem fpcr/fpsr/fpiar,a0@(312)
Lm68881sdone:
	rts

ENTRY(m68881_restore)
	movl	sp@(4),a0		| save area pointer
	tstb	a0@			| null state frame?
	jeq	Lm68881rdone		| yes, easy
	.word	0xf228,0x9c00,0x0138	| fmovem a0@(312),fpcr/fpsr/fpiar
	.word	0xf228,0xd0ff,0x00d8	| fmovem a0@(216),fp0-fp7
Lm68881rdone:
	.word	0xf350			| frestore a0@
	rts
#endif

/*
 * Handle the nitty-gritty of rebooting the machine.
 * Basically we just turn off the MMU and jump to the appropriate ROM routine.
 * Note that we must be running in an address range that is mapped one-to-one
 * logical to physical so that the PC is still valid immediately after the MMU
 * is turned off.
 */
	.globl	_doboot
_doboot:
	movl	#CACHE_OFF,d0
	movc	d0,cacr			| disable on-chip cache(s)
#if defined(HP320) || defined(HP350) || defined(HP370)
	tstl	_ectype
	jeq	Lnocache5
	andl	#~MMU_CEN,_IObase+MMUCMD| disable external cache
Lnocache5:
#endif
/* one-to-one map the last page of memory */
	movl	#MAXADDR,d0		| last page of RAM used to pass params
	orl	#PG_RW+PG_V,d0		| create a PTE
	movl	d0,_mmap		|   to access that page
	jbsr	_TBIA			| invalidate TLB
	movl	#MAXADDR,d0		| last page of RAM is also used
	orl	#SG_RW+SG_V,d0		|   as the page table for itself
	movl	d0,eSysseg-4		|   (ok since it needs only last word)
	movl	_vmmap+NBPG-4,d2	| save old contents
	movl	_mmap,_vmmap+NBPG-4	| store PTE in new page table
	jbsr	_TBIA			| invalidate again
	lea	MAXADDR,a0		| can now access last page
	movl	_boothowto,a0@+		| store howto
	movl	_bootdev,a0@+		| and devtype
	lea	Lbootcode,a1		| start of boot code
	lea	Lebootcode,a3		| end of boot code
Lbootcopy:
	movw	a1@+,a0@+		| copy a word
	cmpl	a3,a1			| done yet?
	jcs	Lbootcopy		| no, keep going
	jmp	MAXADDR+8		| jump to last page

Lbootcode:
	lea	MAXADDR+0x800,sp	| physical SP in case of NMI
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	LhpmmuB			| yes, skip
	movl	#0,a0@			| value for pmove to TC (turn off MMU)
	.long	0xf0104000		| pmove a0@,tc
	movl	d2,MAXADDR+NBPG-4	| restore old high page contents
	jmp	0x1A4			| goto REQ_REBOOT
LhpmmuB:
#endif
#if defined(HP320) || defined(HP350)
	movl	#0xFFFF0000,_IObase+MMUCMD	| totally disable MMU
	movl	d2,MAXADDR+NBPG-4	| restore old high page contents
	jmp	0x1A4			| goto REQ_REBOOT
#endif
Lebootcode:

	.data
	.space	NBPG
tmpstk:
	.globl	_machineid
_machineid:
	.long	0		| default to 320
	.globl	_mmutype,_protorp
_mmutype:
	.long	0		| default to HP MMU
_protorp:
	.long	0,0		| prototype root pointer
	.globl	_ectype
_ectype:
	.long	0		| external cache type, default to none
	.globl	_cold
_cold:
	.long	1		| cold start flag
#ifdef DEBUG
	.globl	fullflush
fullflush:
	.long	0
	.globl	timebomb
timebomb:
	.long	0
#endif
/* interrupt counters */
	.globl	_intrcnt,_eintrcnt,_intrnames,_eintrnames
_intrnames:
	.asciz	"spur"
	.asciz	"hil"
	.asciz	"lev2"
	.asciz	"lev3"
	.asciz	"lev4"
	.asciz	"lev5"
	.asciz	"dma"
	.asciz	"clock"
#ifdef PROFTIMER
	.asciz  "pclock"
#endif
	.asciz	"nmi"
_eintrnames:
	.even
_intrcnt:
#ifdef PROFTIMER
	.long	0,0,0,0,0,0,0,0,0,0
#else
	.long	0,0,0,0,0,0,0,0,0
#endif
_eintrcnt:
