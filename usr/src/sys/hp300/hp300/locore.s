/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1980, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: locore.s 1.2 90/07/14$
 *
 *	@(#)locore.s	7.5 (Berkeley) %G%
 */

#define MMUADDR(ar)	movl	_MMUbase,ar
#define CLKADDR(ar)	movl	_CLKbase,ar

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
	ptestr	#1,a0@,#7		| do a table search
	pmove	psr,sp@			| save result
	btst	#7,sp@			| bus error bit set?
	jeq	Lismerr			| no, must be MMU fault
	clrw	sp@			| yes, re-clear pad word
	jra	Lisberr			| and process as normal bus error
Lbehpmmu:
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	movl	a0@(MMUSTAT),d0		| read status
	btst	#3,d0			| MMU fault?
	jeq	Lisberr			| no, just a non-MMU bus error so skip
	andl	#~MMU_FAULT,a0@(MMUSTAT)| yes, clear fault bits
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
	fsave	a0@		| save state
	tstb	a0@		| null state frame?
	jeq	Lfptnull	| yes, safe
	clrw	d0		| no, need to tweak BIU
	movb	a0@(1),d0	| get frame size
	bset	#3,a0@(0,d0:w)	| set exc_pend bit of BIU
Lfptnull:
	fmovem	fpsr,sp@-	| push fpsr as code argument
	frestore a0@		| restore state
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
	cmpl	#_u+NBPG,sp		| our we still in stack pages?
	jcc	Lstackok		| yes, continue normally
	tstl	_panicstr		| have we paniced?
	jne	Lstackok		| yes, do not re-panic
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
	CLKADDR(a0)
	movb	a0@(CLKSR),d0		| read clock status
#ifdef PROFTIMER
	.globl  _profon
	tstb	_profon			| profile clock on?
	jeq     Ltimer1			| no, then must be timer1 interrupt
	btst	#2,d0			| timer3 interrupt?
	jeq     Ltimer1			| no, must be timer1
	movb	a0@(CLKMSB3),d1		| clear timer3 interrupt
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
	CLKADDR(a0)
Lttimer1:
	btst	#0,d0			| timer1 interrupt?
	jeq     Ltimend		        | no, check state of kernel profiling
Ltimer1:
#endif
	movb	a0@(CLKMSB1),d1		| clear timer1 interrupt
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
 * Kernel access to the current processes user struct is via a fixed
 * virtual address.  It is at the same address as in the users VA space.
 * Umap contains the KVA of the first of UPAGES PTEs mapping VA _u.
 */
	.data
	.set	_u,USRSTACK
_Umap:	.long	0
	.globl	_u, _Umap

#define	RELOC(var, ar)	\
	lea	var,ar;	\
	addl	a5,ar

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
	RELOC(tmpstk, a0)
	movl	a0,sp			| give ourselves a temporary stack
	RELOC(_lowram, a0)
	movl	a5,a0@			| store start of physical memory
	movl	#CACHE_OFF,d0
	movc	d0,cacr			| clear and disable on-chip cache(s)

/* determine our CPU/MMU combo - check for all regardless of kernel config */
	movl	#IOBASE+MMUBASE,a1
	movl	#0x200,d0		| data freeze bit
	movc	d0,cacr			|   only exists on 68030
	movc	cacr,d0			| read it back
	tstl	d0			| zero?
	jeq	Lis68020		| yes, we have 68020
	RELOC(_mmutype, a0)		| no, we have 68030
	movl	#-1,a0@			| set to reflect 68030 PMMU
	RELOC(_machineid, a0)
	movl	#0x80,a1@(MMUCMD)	| set magic cookie
	movl	a1@(MMUCMD),d0		| read it back
	btst	#7,d0			| cookie still on?
	jeq	Lnot370			| no, 360 or 375
	movl	#0,a1@(MMUCMD)		| clear magic cookie
	movl	a1@(MMUCMD),d0		| read it back
	btst	#7,d0			| still on?
	jeq	Lisa370			| no, must be a 370
	movl	#5,a0@			| yes, must be a 340
	jra	Lstart1
Lnot370:
	movl	#3,a0@			| type is at least a 360
	movl	#0,a1@(MMUCMD)		| clear magic cookie2
	movl	a1@(MMUCMD),d0		| read it back
	btst	#16,d0			| still on?
	jeq	Lstart1			| no, must be a 360
	movl	#6,a0@			| yes, must be a 345/375
	jra	Lhaspac
Lisa370:
	movl	#4,a0@			| set to 370
Lhaspac:
	RELOC(_ectype, a0)
	movl	#-1,a0@			| also has a physical address cache
	jra	Lstart1
Lis68020:
	movl	#1,a1@(MMUCMD)		| a 68020, write HP MMU location
	movl	a1@(MMUCMD),d0		| read it back
	btst	#0,d0			| non-zero?
	jne	Lishpmmu		| yes, we have HP MMU
	RELOC(_mmutype, a0)
	movl	#1,a0@			| no, we have PMMU
	RELOC(_machineid, a0)
	movl	#1,a0@			| and 330 CPU
	jra	Lstart1
Lishpmmu:
	RELOC(_ectype, a0)		| 320 or 350
	movl	#1,a0@			| both have a virtual address cache
	movl	#0x80,a1@(MMUCMD)	| set magic cookie
	movl	a1@(MMUCMD),d0		| read it back
	btst	#7,d0			| cookie still on?
	jeq	Lstart1			| no, just a 320
	RELOC(_machineid, a0)
	movl	#2,a0@			| yes, a 350

Lstart1:
	movl	#0,a1@(MMUCMD)		| clear out MMU again
/* initialize source/destination control registers for movs */
	moveq	#FC_USERD,d0		| user space
	movc	d0,sfc			|   as source
	movc	d0,dfc			|   and destination of transfers

/*
 * Allocate kernel segment/page table resources.
 *	a5 contains the PA of lowest RAM page
 *	a4 contains the PA of first available page at any time
 *	d5 contains the VA of first available page at any time
 *	   (since we assume a zero load point, it is also the size of
 *	   allocated space at any time)
 * We assume (i.e. do not check) that the initial page table size
 * (Sysptsize) is big enough to map everything we allocate here.
 *
 * We allocate the DIO map here since the 320/350 MMU registers are
 * mapped in this range and it would be nice to be able to access them
 * after the MMU is turned on.
 */
	.globl	_Sysseg, _Sysmap, _Sysptmap, _Sysptsize
	movl	#_end,d5		| end of static kernel text/data
	addl	#NBPG-1,d5
	andl	#PG_FRAME,d5		| round to a page
	movl	d5,a4
	addl	a5,a4
/* allocate kernel segment table */
	RELOC(_Sysseg, a0)
	movl	d5,a0@			| remember VA for pmap module
	movl	a4,sp@-			| remember PA for loading MMU
	addl	#NBPG,a4
	addl	#NBPG,d5
/* allocate initial page table pages (including IO map) */
	RELOC(_Sysptsize, a0)
	movl	a0@,d0			| initial system PT size (pages)
	addl	#(IOMAPSIZE+NPTEPG-1)/NPTEPG,d0	| add pages for IO map
	movl	#PGSHIFT,d1
	lsll	d1,d0			| convert to bytes
	movl	a4,sp@-			| remember PA for ST load
	addl	d0,a4
	addl	d0,d5
/* allocate kernel page table map */
	RELOC(_Sysptmap, a0)
	movl	d5,a0@			| remember VA for pmap module
	movl	a4,sp@-			| remember PA for PT map load
	addl	#NBPG,a4
	addl	#NBPG,d5
/* compute KVA of Sysptmap; mapped after page table pages */
	movl	d0,d2			| remember PT size (bytes)
	moveq	#SG_ISHIFT-PGSHIFT,d1
	lsll	d1,d0			| page table size serves as seg index
	RELOC(_Sysmap, a0)
	movl	d0,a0@			| remember VA for pmap module
/* initialize ST and PT map: PT pages + PT map */
	movl	sp@+,a1			| PT map PA
	movl	sp@+,d4			| start of PT pages
	movl	sp@+,a0			| ST phys addr
	lea	a0@(NBPG-4),a2		| (almost) end of ST
	movl	d4,d3
	orl	#SG_RW+SG_V,d4		| create proto STE for ST
	orl	#PG_RW+PG_CI+PG_V,d3	| create proto PTE for PT map
List1:
	movl	d4,a0@+
	movl	d3,a1@+
	addl	#NBPG,d4
	addl	#NBPG,d3
	cmpl	a4,d4			| sleezy, but works ok
	jcs	List1
/* initialize ST and PT map: invalidate up to last entry */
List2:
	movl	#SG_NV,a0@+
	movl	#PG_NV,a1@+
	cmpl	a2,a0
	jcs	List2
/*
 * Portions of the last segment of KVA space (0xFFF00000 - 0xFFFFFFFF)
 * are mapped for a couple of purposes. 0xFFF00000 for UPAGES is used
 * for mapping the current process u-area (u + kernel stack).  The
 * very last page (0xFFFFF000) is mapped to the last physical page of
 * RAM to give us a region in which PA == VA.  We use this page for
 * enabling/disabling mapping.
 */
	movl	a4,d1			| grab next available for PT page
	andl	#SG_FRAME,d1		| mask to frame number
	orl	#SG_RW+SG_V,d1		| RW and valid
	movl	d1,a0@+			| store in last ST entry
	movl	a0,a2			| remember addr for PT load
	andl	#PG_FRAME,d1
	orl	#PG_RW+PG_V,d1		| convert to PTE
	movl	d1,a1@+			| store in PT map
	movl	a4,a0			| physical beginning of PT page
	lea	a0@(NBPG-4),a1		| (almost) end of page
Lispt7:
	movl	#PG_NV,a0@+		| invalidate
	cmpl	a1,a0
	jcs	Lispt7
	movl	#MAXADDR,d1		| get last phys page addr
	andl	#PG_FRAME,d1
	orl	#PG_RW+PG_V,d1
	movl	d1,a0@+			| map to last virt page
	addl	#NBPG,a4
	addl	#NBPG,d5
/* record KVA at which to access current u-area PTEs */
	RELOC(_Sysmap, a0)
	movl	a0@,d0			| get system PT address
	addl	#NPTEPG*NBPG,d0		| end of system PT
	subl	#HIGHPAGES*4,d0		| back up to first PTE for u-area
	RELOC(_Umap, a0)
	movl	d0,a0@			| remember location
/* initialize page table pages */
	movl	a2,a0			| end of ST is start of PT
	addl	d2,a2			| add size to get end of PT
/* text pages are read-only */
	clrl	d0			| assume load at VA 0
	movl	a5,d1			| get load PA
	andl	#PG_FRAME,d1		| convert to a page frame
#ifdef KGDB
	orl	#PG_RW+PG_V,d1		| XXX: RW for now
#else
	orl	#PG_RO+PG_V,d1		| create proto PTE
#endif
	movl	#_etext,a1		| go til end of text
Lipt1:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	addl	#NBPG,d0		| and address counter
	cmpl	a1,d0			| done yet?
	jcs	Lipt1			| no, keep going
/* data, bss and dynamic tables are read/write */
	andl	#PG_FRAME,d1		| mask out old prot bits
	orl	#PG_RW+PG_V,d1		| mark as valid and RW
	movl	d5,a1			| go til end of data allocated so far
	addl	#(UPAGES+1)*NBPG,a1	| and proc0 PT/u-area (to be allocated)
Lipt2:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	addl	#NBPG,d0		| and address counter
	cmpl	a1,d0			| done yet?
	jcs	Lipt2			| no, keep going
/* invalidate remainder of kernel PT */
	movl	a2,a1			| end of PT
Lipt3:
	movl	#PG_NV,a0@+		| invalidate PTE
	cmpl	a1,a0			| done yet?
	jcs	Lipt3			| no, keep going
/* go back and validate IO PTEs at end of allocated PT space */
	movl	a2,a0			| end of allocated PT space
	subl	#IOMAPSIZE*4,a0		| back up IOMAPSIZE PTEs
	movl	#IOBASE,d1		| physical IO base
	orl	#PG_RW+PG_CI+PG_V,d1	| create proto PTE
Lipt4:
	movl	d1,a0@+			| load PTE
	addl	#NBPG,d1		| increment page frame number
	cmpl	a2,a0			| done yet?
	jcs	Lipt4			| no, keep going
/* record base KVA of IO space which is just before Sysmap */
	RELOC(_Sysmap, a0)
	movl	a0@,d0			| Sysmap VA
	subl	#IOMAPSIZE*NBPG,d0	| Back up size of IO space
	RELOC(_DIObase, a0)
	movl	d0,a0@
/* also record base of clock and MMU registers for fast access */
	addl	#CLKBASE,d0
	RELOC(_CLKbase, a0)
	movl	d0,a0@
	subl	#CLKBASE,d0
	addl	#MMUBASE,d0
	RELOC(_MMUbase, a0)
	movl	d0,a0@

/*
 * Setup page table for process 0.
 *
 * We set up page table access for the kernel via Usrptmap (usrpt)
 * and access to the u-area itself via Umap (u).  First available
 * page (VA: d5, PA: a4) is used for proc0 page table.  Next UPAGES
 * pages following are for u-area.
 */
	movl	a4,d0
	movl	d0,d1
	andl	#PG_FRAME,d1		| mask to page frame number
	orl	#PG_RW+PG_V,d1		| RW and valid
	movl	d1,d4			| remember for later Usrptmap load
	movl	d0,a0			| base of proc0 PT
	addl	#NBPG,d0		| plus one page yields base of u-area
	movl	d0,a2			|   and end of PT
	addl	#NBPG,d5		| keep VA in sync
/* invalidate entire page table */
Liudot1:
	movl	#PG_NV,a0@+		| invalidate PTE
	cmpl	a2,a0			| done yet?
	jcs	Liudot1			| no, keep going
/* now go back and validate u-area PTEs in PT and in Umap */
	lea	a0@(-HIGHPAGES*4),a0	| base of PTEs for u-area (p_addr)
	lea	a0@(UPAGES*4),a1	| end of PTEs for u-area
	lea	a4@(-HIGHPAGES*4),a3	| u-area PTE base in Umap PT
	movl	d0,d1			| get base of u-area
	andl	#PG_FRAME,d1		| mask to page frame number
	orl	#PG_RW+PG_V,d1		| add valid and writable
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
	movl	a2,a4			| save phys addr of first avail page
	RELOC(_proc0paddr, a0)
	movl	d5,a0@			| save KVA of proc0 u-area
	addl	#UPAGES*NBPG,d5		| and virtual addr as well

/*
 * Prepare to enable MMU.
 * Since the kernel is not mapped logical == physical we must insure
 * that when the MMU is turned on, all prefetched addresses (including
 * the PC) are valid.  In order guarentee that, we use the last physical
 * page (which is conveniently mapped == VA) and load it up with enough
 * code to defeat the prefetch, then we execute the jump back to here.
 *
 * Is this all really necessary, or am I paranoid??
 */
	RELOC(_Sysseg, a0)		| system segment table addr
	movl	a0@,a1			| read value (a KVA)
	addl	a5,a1			| convert to PA
	RELOC(_mmutype, a0)
	tstl	a0@			| HP MMU?
	jeq	Lhpmmu2			| yes, skip
	RELOC(_protorp, a0)
	movl	#0x80000202,a0@		| nolimit + share global + 4 byte PTEs
	movl	a1,a0@(4)		| + segtable address
	pmove	a0@,srp			| load the supervisor root pointer
	movl	#0x80000002,a0@		| reinit upper half for CRP loads
	jra	Lstploaddone		| done
Lhpmmu2:
	movl	a1,d1
	moveq	#PGSHIFT,d2
	lsrl	d2,d1			| convert to page frame
	movl	d1,IOBASE+MMUBASE+MMUSSTP | load in sysseg table register
Lstploaddone:
	lea	MAXADDR,a2		| PA of last RAM page
	RELOC(Lhighcode, a1)		| addr of high code
	RELOC(Lehighcode, a3)		| end addr
Lcodecopy:
	movw	a1@+,a2@+		| copy a word
	cmpl	a3,a1			| done yet?
	jcs	Lcodecopy		| no, keep going
	jmp	MAXADDR			| go for it!

Lhighcode:
	RELOC(_mmutype, a0)
	tstl	a0@			| HP MMU?
	jeq	Lhpmmu3			| yes, skip
	movl	#MMU_IEN+MMU_FPE,IOBASE+MMUBASE+MMUCMD | enable 68881 and i-cache
	movl	#0x82c0aa00,a2@			| value to load TC with
	pmove	a2@,tc				| load it
	jmp	Lenab1
Lhpmmu3:
	movl	#0,IOBASE+MMUBASE+MMUCMD	| clear external cache
	movl	#MMU_ENAB,IOBASE+MMUBASE+MMUCMD	| turn on MMU
	jmp	Lenab1				| jmp to mapped code
Lehighcode:

/*
 * Should be running mapped from this point on
 */
Lenab1:
/* check for internal HP-IB in SYSFLAG */
	btst	#5,0xfffffed2		| internal HP-IB?
	jeq	Linitmem		| yes, have HP-IB just continue
	clrl	_internalhpib		| no, clear flag
/* init mem sizes */
Linitmem:
	movl	#MAXADDR,d1		| last page
	moveq	#PGSHIFT,d2
	lsrl	d2,d1			| convert to page (click) number
	movl	d1,_maxmem		| save as maxmem
	movl	_lowram,d0		| lowram value from ROM via boot
	lsrl	d2,d0			| convert to page number
	subl	d0,d1			| compute amount of RAM present
	movl	d1,_physmem		| and physmem
/*
 * pmap_bootstrap is supposed to be called with mapping off early on
 * to set up the kernel VA space.  However, this only works easily if
 * you have a kernel PA == VA mapping.  Since we do not, we just set
 * up and enable mapping here and then call the bootstrap routine to
 * get the pmap module in sync with reality.
 */
	.globl	_avail_start
	lea	tmpstk,sp		| temporary stack
	movl	a5,sp@-			| phys load address (assumes VA 0)
	movl	a4,sp@-			| first available PA
	jbsr	_pmap_bootstrap		| sync up pmap module
	addql	#8,sp
|	movl	_avail_start,a4		| pmap_bootstrap may need RAM
/* initialize (slightly) the pcb */
	lea	_u,a1			| proc0 u-area
	lea	a1@(UPAGES*NBPG-4),sp	| set kernel stack to end of u-area
	movl	#USRSTACK-4,a2
	movl	a2,usp			| init user SP
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
	jbsr	_TBIA			| invalidate TLB
	movl	#CACHE_ON,d0
	movc	d0,cacr			| clear cache(s)
	tstl	_ectype
	jeq	Lnocache0
	MMUADDR(a0)
	orl	#MMU_CEN,a0@(MMUCMD)	| turn on external cache
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
	.globl	_icode,_initflags,_szicode
_icode:
	pea	pc@(argv-.-2)
	pea	pc@(init-.-2)
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
	moveq	#ENAMETOOLONG,d0	| ran out of space
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
	moveq	#ENAMETOOLONG,d0	| ran out of space
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
	moveq	#ENAMETOOLONG,d0	| ran out of space
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
 * Masterpaddr is the p->p_addr of the running process on the master
 * processor.  When a multiprocessor system, the slave processors will have
 * an array of slavepaddrs (on an HP it only exists for the benefit of adb).
 */
	.globl	_masterpaddr
	.data
_masterpaddr:
	.long	0
	.text

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
	movl	d0,_masterpaddr
	movl	a0@(P_MAP),a1		| map = p->p_map
	tstl	a1			| map == VM_MAP_NULL?
	jeq	Lswnochg		| yes, skip
	movl	a1@(PMAP),a1		| pmap = map->pmap
	tstl	a1			| pmap == PMAP_NULL?
	jeq	Lswnochg		| yes, skip
	tstl	a1@(PM_STCHG)		| pmap->st_changed?
	jeq	Lswnochg		| no, skip
	movl	d0,sp@-			| push pcb (at p_addr)
	pea	a1@			| push pmap
	jbsr	_pmap_activate		| pmap_activate(pmap, pcb)
	addql	#8,sp
	movl	_masterpaddr,d0		| restore p_addr for resume below
Lswnochg:
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
	fsave	a0@			| save FP state
	tstb	a0@			| null state frame?
	jeq	Lresnofpsave		| yes, all done
	fmovem	fp0-fp7,a0@(216)	| save FP general registers
	fmovem	fpcr/fpsr/fpi,a0@(312)	| save FP control registers
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
	CLKADDR(a0)
	movb	#0,a0@(CLKCR2)		| no, just user, select CR3
	movb	#0,a0@(CLKCR3)		| and turn it off
Lskipoff:
#endif
	movl	_CMAP2,a1@(PCB_CMAP2)	| save temporary map PTE
	movl	#PGSHIFT,d1
	lsrl	d1,d0			| convert p_addr to page number
	lsll	#2,d0			| and now to Systab offset
	addl	_Sysmap,d0		| add Systab base to get PTE addr
	movw	#PSL_HIGHIPL,sr		| go crit while changing PTEs
	lea	tmpstk,sp		| now goto a tmp stack for NMI
	movl	d0,a0			| address of new context
	movl	_Umap,a1		| address of PTEs for u
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
	tstl	fulltflush		| 68851, conservative?
	jne	Lnot68851a		| yes, go flush all
#endif
	pflushs	#4,#4			| flush only supervisor side
	jra	Lres2
Lnot68851a:
	pflusha				| flush entire TLB
Lres2:
	movl	a1@(PCB_USTP),d0	| get USTP
	moveq	#PGSHIFT,d1
	lsll	d1,d0			| convert to addr
	lea	_protorp,a0		| CRP prototype
	movl	d0,a0@(4)		| stash USTP
	pmove	a0@,crp			| load new user root pointer
	jra	Lcxswdone		| thats it
Lhpmmu4:	
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	movl	a0@(MMUTBINVAL),d1	| invalidate TLB
	tstl	_ectype			| got external VAC?
	jle	Lnocache1		| no, skip
	andl	#~MMU_CEN,a0@(MMUCMD)	| toggle cache enable
	orl	#MMU_CEN,a0@(MMUCMD)	| to clear data cache
Lnocache1:
	movl	a1@(PCB_USTP),a0@(MMUUSTP) | context switch
#endif
Lcxswdone:
	movl	a1@(U_PROCP),a0		| u.u_procp
	bclr	#SPTECHGB-24,a0@(P_FLAG)| clear SPTECHG bit
#if defined(HP330)
	jeq	Lnot68851b		| if set need to flush user TLB
	tstl	_mmutype		| 68851 PMMU?
	jle	Lnot68851b		| no, skip
	pflushs	#0,#4			| user PT changed, flush user TLB
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
	CLKADDR(a0)
	movl	_profint,d1		| profiling interval
	subql	#1,d1			|   adjusted
	movepw	d1,a0@(CLKMSB3)		| set interval
	movb	#0,a0@(CLKCR2)		| select CR3
	movb	#64,a0@(CLKCR3)		| turn it on
Lskipon:
#endif
#ifdef FPCOPROC
	lea	a1@(PCB_FPCTX),a0	| pointer to FP save area
	tstb	a0@			| null state frame?
	jeq	Lresfprest		| yes, easy
	fmovem	a0@(312),fpcr/fpsr/fpi	| restore FP control registers
	fmovem	a0@(216),fp0-fp7	| restore FP general registers
Lresfprest:
	frestore a0@			| restore state
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
	movl	_CMAP2,a0
	movl	_CADDR2,sp@-			| destination kernel VA
	movl	d0,a0@				| load in page table
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp
	movl	_CADDR2,a1			| destination addr
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
 * Copy 1 relocation unit (NBPG bytes)
 * from physical address to physical address
 */
ENTRY(physcopyseg)
	movl	sp@(4),d0			| source page number
	moveq	#PGSHIFT,d1
	lsll	d1,d0				| convert to address
	orl	#PG_CI+PG_RW+PG_V,d0		| make sure valid and writable
	movl	_CMAP1,a0
	movl	d0,a0@				| load in page table
	movl	_CADDR1,sp@-			| destination kernel VA
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp

	movl	sp@(8),d0			| destination page number
	moveq	#PGSHIFT,d1
	lsll	d1,d0				| convert to address
	orl	#PG_CI+PG_RW+PG_V,d0		| make sure valid and writable
	movl	_CMAP2,a0
	movl	d0,a0@				| load in page table
	movl	_CADDR2,sp@-			| destination kernel VA
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp

	movl	_CADDR1,a0			| source addr
	movl	_CADDR2,a1			| destination addr
	movl	#NBPG/4-1,d0			| count
Lpcpy:
	movl	a0@+,a1@+			| copy longword
	dbf	d0,Lpcpy			| continue until done
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
	movl	_CMAP1,a0
	movl	_CADDR1,sp@-			| destination kernel VA
	movl	d0,a0@				| load in page map
	jbsr	_TBIS				| invalidate any old mapping
	addql	#4,sp
	movl	_CADDR1,a1			| destination addr
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
	pflusha				| flush entire TLB
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
	MMUADDR(a0)
	movl	a0@(MMUTBINVAL),sp@-	| do not ask me, this
	addql	#4,sp			|   is how hpux does it
#ifdef DEBUG
	tstl	fullcflush
	jne	__DCIA			| XXX: invalidate entire cache
#endif
#endif
	rts

/*
 * Invalidate any TLB entry for given VA (TB Invalidate Single)
 */
ENTRY(TBIS)
#ifdef DEBUG
	tstl	fulltflush		| being conservative?
	jne	__TBIA			| yes, flush entire TLB
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu5			| yes, skip
	movl	sp@(4),a0		| get addr to flush
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851b		| is 68851?
	pflush	#0,#0,a0@		| flush address from both sides
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip data cache
	rts
Lmc68851b:
#endif
	pflushs	#0,#0,a0@		| flush address from both sides
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
	tstl	fulltflush		| being conservative?
	jne	__TBIA			| yes, flush everything
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu7			| yes, skip
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851c		| 68851?
	pflush #4,#4			| flush supervisor TLB entries
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts
Lmc68851c:
#endif
	pflushs #4,#4			| flush supervisor TLB entries
	rts
Lhpmmu7:
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	movl	#0x8000,d0		| more
	movl	d0,a0@(MMUTBINVAL)	|   HP magic
#ifdef DEBUG
	tstl	fullcflush
	jne	__DCIS			| XXX: invalidate entire sup. cache
#endif
#endif
	rts

/*
 * Invalidate user side of TLB
 */
ENTRY(TBIAU)
#ifdef DEBUG
	tstl	fulltflush		| being conservative?
	jne	__TBIA			| yes, flush everything
#endif
#if defined(HP330) || defined(HP360) || defined(HP370)
	tstl	_mmutype		| HP MMU?
	jeq	Lhpmmu8			| yes, skip
#if defined(HP360) || defined(HP370)
	jpl	Lmc68851d		| 68851?
	pflush	#0,#4			| flush user TLB entries
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts
Lmc68851d:
#endif
	pflushs	#0,#4			| flush user TLB entries
	rts
Lhpmmu8:
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	moveq	#0,d0			| more
	movl	d0,a0@(MMUTBINVAL)	|   HP magic
#ifdef DEBUG
	tstl	fullcflush
	jne	__DCIU			| XXX: invalidate entire user cache
#endif
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
 * NOTE: we do not flush 68030 on-chip cache as there are no aliasing
 * problems with DC_WA.  The only cases we have to worry about are context
 * switch and TLB changes, both of which are handled "in-line" in resume
 * and TBI*.
 */
ENTRY(DCIA)
__DCIA:
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache2		| no, all done
	MMUADDR(a0)
	andl	#~MMU_CEN,a0@(MMUCMD)	| disable cache in MMU control reg
	orl	#MMU_CEN,a0@(MMUCMD)	| reenable cache in MMU control reg
Lnocache2:
#endif
	rts

ENTRY(DCIS)
__DCIS:
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache3		| no, all done
	MMUADDR(a0)
	movl	a0@(MMUSSTP),d0		| read the supervisor STP
	movl	d0,a0@(MMUSSTP)		| write it back
Lnocache3:
#endif
	rts

ENTRY(DCIU)
__DCIU:
#if defined(HP320) || defined(HP350)
	tstl	_ectype			| got external VAC?
	jle	Lnocache4		| no, all done
	MMUADDR(a0)
	movl	a0@(MMUUSTP),d0		| read the user STP
	movl	d0,a0@(MMUUSTP)		| write it back
Lnocache4:
#endif
	rts

ENTRY(PCIA)
#if defined(HP360) || defined(HP370)
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	tstl	_ectype			| got external PAC?
	jge	Lnocache6		| no, all done
	MMUADDR(a0)
	andl	#~MMU_CEN,a0@(MMUCMD)	| disable cache in MMU control reg
	orl	#MMU_CEN,a0@(MMUCMD)	| reenable cache in MMU control reg
Lnocache6:
#endif
	rts

ENTRY(ecacheon)
	tstl	_ectype
	jeq	Lnocache7
	MMUADDR(a0)
	orl	#MMU_CEN,a0@(MMUCMD)
Lnocache7:
	rts

ENTRY(ecacheoff)
	tstl	_ectype
	jeq	Lnocache8
	MMUADDR(a0)
	andl	#~MMU_CEN,a0@(MMUCMD)
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
	pmove	a0@,crp			| load root pointer
	movl	#DC_CLEAR,d0
	movc	d0,cacr			| invalidate on-chip d-cache
	rts				|   since pmove flushes TLB
Lhpmmu9:
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	movl	sp@(4),a0@(MMUUSTP)	| load a new USTP
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
	pflushr	_protorp		| flush RPT/TLB entries
Lnot68851:
#endif
	rts

ENTRY(ploadw)
#if defined(HP330) || defined(HP360) || defined(HP370)
	movl	sp@(4),a0		| address to load
	ploadw	#1,a0@			| pre-load translation
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

/*
 * bzero(addr, count)
 */
ALTENTRY(blkclr, _bzero)
ENTRY(bzero)
	movl	sp@(4),a0	| address
	movl	sp@(8),d0	| count
	jeq	Lbzdone		| if zero, nothing to do
	movl	a0,d1
	btst	#0,d1		| address odd?
	jeq	Lbzeven		| no, can copy words
	clrb	a0@+		| yes, zero byte to get to even boundary
	subql	#1,d0		| decrement count
	jeq	Lbzdone		| none left, all done
Lbzeven:
	movl	d0,d1
	andl	#31,d0
	lsrl	#5,d1		| convert count to 8*longword count
	jeq	Lbzbyte		| no such blocks, zero byte at a time
Lbzloop:
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	clrl	a0@+; clrl	a0@+; clrl	a0@+; clrl	a0@+;
	subql	#1,d1		| one more block zeroed
	jne	Lbzloop		| more to go, do it
	tstl	d0		| partial block left?
	jeq	Lbzdone		| no, all done
Lbzbyte:
	clrb	a0@+
	subql	#1,d0		| one more byte cleared
	jne	Lbzbyte		| more to go, do it
Lbzdone:
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
	jeq	Lffsdone
Lffsloop:
	addql	#1,d0
	btst	d0,d1
	jeq	Lffsloop
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
	fsave	a0@			| save state
	tstb	a0@			| null state frame?
	jeq	Lm68881sdone		| yes, all done
	fmovem fp0-fp7,a0@(216)		| save FP general registers
	fmovem fpcr/fpsr/fpi,a0@(312)	| save FP control registers
Lm68881sdone:
	rts

ENTRY(m68881_restore)
	movl	sp@(4),a0		| save area pointer
	tstb	a0@			| null state frame?
	jeq	Lm68881rdone		| yes, easy
	fmovem	a0@(312),fpcr/fpsr/fpi	| restore FP control registers
	fmovem	a0@(216),fp0-fp7	| restore FP general registers
Lm68881rdone:
	frestore a0@			| restore state
	rts
#endif

/*
 * Handle the nitty-gritty of rebooting the machine.
 * Basically we just turn off the MMU and jump to the appropriate ROM routine.
 * Note that we must be running in an address range that is mapped one-to-one
 * logical to physical so that the PC is still valid immediately after the MMU
 * is turned off.  We have conveniently mapped the last page of physical
 * memory this way.
 */
	.globl	_doboot
_doboot:
	movl	#CACHE_OFF,d0
	movc	d0,cacr			| disable on-chip cache(s)
#if defined(HP320) || defined(HP350) || defined(HP370)
	tstl	_ectype
	jeq	Lnocache5
	MMUADDR(a0)
	andl	#~MMU_CEN,a0@(MMUCMD)	| disable external cache
Lnocache5:
#endif
	lea	MAXADDR,a0		| last page of physical memory
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
	pmove	a0@,tc			| disable MMU
	jmp	0x1A4			| goto REQ_REBOOT
LhpmmuB:
#endif
#if defined(HP320) || defined(HP350)
	MMUADDR(a0)
	movl	#0xFFFF0000,a0@(MMUCMD)	| totally disable MMU
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
	.globl	_internalhpib
_internalhpib:
	.long	1		| has internal HP-IB, default to yes
	.globl	_cold
_cold:
	.long	1		| cold start flag
	.globl	_DIObase, _CLKbase, _MMUbase, _proc0paddr
_proc0paddr:
	.long	0		| KVA of proc0 u-area
_DIObase:
	.long	0		| KVA of base of IO space
_CLKbase:
	.long	0		| KVA of base of clock registers
_MMUbase:
	.long	0		| KVA of base of HP MMU registers
#ifdef DEBUG
	.globl	fulltflush, fullcflush
fulltflush:
	.long	0
fullcflush:
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
