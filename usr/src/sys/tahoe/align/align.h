/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)align.h	7.1 (Berkeley) 12/6/90
 */

#ifndef	LOCORE
/*
 * Some special registers definitions.
 */

#ifndef	SLR
#define	SLR	1
#define	P0LR	3
#define	P1LR	5
#define	P2LR	7
#endif

#include	"defs.h"
/*
 * Definitions about the stack frame as seen by the routine
 * 'alignment' in the kernel. If you ever wondered what's the
 * meaning of 'machine dependent code', look here :-)
 * This structure is partly set up by locore.s, for 'alignment',
 * and partly by the allocation of local variables in 'alignment.c'
 * itself.
 *    All these things are passed between procedures on the
 * (current process' kernel) stack. The alternative (static
 * variables) is a little bit more elegant but it works fine
 * for one process only. Will not work for multiple processes
 * with alignment processing or for MP models of HW.
 *
 * WARNING : due to the intimate relationships involved, don't
 *	change the process_info structure unless you also
 *	change the alignment trap code in locore.s AND the
 *	allocation of local variables in 'alignment.c' !!
 */
typedef	struct {
	long		Saved_sp;		/* For exceptions */
	long		Saved_pc;
	long		Last_operand;		/* Last operand # processed */
	long		Opcode;			/* 'offending' opcode */
	struct	oprnd	Decoded[4];	
	long		REG0;
	long		REG1;
	long		REG2;
	long		REG3;
	long		REG4;
	long		REG5;
	long		REG6;
	long		REG7;
	long		REG8;
	long		REG9;
	long		REG10;
	long		REG11;
	long		REG12;
	long		return_pc;	/* Points into locore.s */
	long		mask_restored;
	long		REG13;		/* Original, from the process */
	long		Sp;		/* Alias R14 */
	long		ret_code;
	long		ret_addr;
	long		ret_exception;	/* To tell locore.s about problems */
	long		Ach;
	long		Acl;
	unsigned	unused:30;
	unsigned	pcb_acc_pnt:1;
	unsigned	acc_dbl:1;
	long		Pc;		/* Alias R15 */
	long		Psl;
} process_info;

#define	saved_pc	(infop->Saved_pc)
#define	saved_sp	(infop->Saved_sp)
#define last_operand	(infop->Last_operand)
#define	opCODE		(infop->Opcode)
#define	decoded		(infop->Decoded)
#define	r0		(infop->REG0)
#define	r1		(infop->REG1)
#define	r2		(infop->REG2)
#define	r3		(infop->REG3)
#define	r4		(infop->REG4)
#define	r5		(infop->REG5)
#define	r6		(infop->REG6)
#define	r7		(infop->REG7)
#define	r8		(infop->REG8)
#define	r9		(infop->REG9)
#define	r10		(infop->REG10)
#define	r11		(infop->REG11)
#define	r12		(infop->REG12)
#define	r13		(infop->REG13)
#define	fp		(infop->REG13)
#define	sp		(infop->Sp)
#define	acc_high	(infop->Ach)
#define	acc_low		(infop->Acl)
#define	pc		(infop->Pc)
#define	psl		(infop->Psl)

#define	PCOUNTER		15
#define	SPOINTER		14


/*
 * Setting new condition codes for the process.
#define Set_psl(z)	asm("	movl	z,r6"); \
			asm("	andl2	$15,r6"); \
			asm("	mnegl	$1,r7"); \
			asm("	xorl2	r6,r7"); \
			asm("	andl2	$15,r7"); \
			asm("	bicpsw	r7"); \
			asm("	bispsw	r6")
 */
#define Set_psl(z)	asm("	andl2	$15,z"); \
			asm("	mnegl	$1,r6"); \
			asm("	xorl2	z,r6"); \
			asm("	andl2	$15,r6"); \
			asm("	bicpsw	r6"); \
			asm("	bispsw	z")
#define	New_cc(x)	(x) &= PSL_ALLCC; psl = psl & ~PSL_ALLCC | (x)

#endif

/*
 * Definitions for ret_code. NOTE : DON"T USE 0 !! locore.s knows that
 *	0 means OK, no problems !
 */

#define	ILL_ADDRMOD	1
#define	ILL_ACCESS	2
#define	ILL_OPRND	3
#define	ARITHMETIC	4
#define	ALIGNMENT	5

/*
 * For use in u.u_eosys as a flag.
 */
#define	EMULATEALIGN	0x80
