/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)machdep.h	5.3 (Berkeley) 1/9/89
 */

#ifdef ADDR32
#define pushaddr(x)	push4((long)(x))
#define popaddr()	(char *)pop4()
#endif ADDR32
#ifdef ADDR16
#define pushaddr(x)	push2((short)(x))
#define popaddr()	(char *)pop2()
#endif ADDR16

#define popfile()	(FILE *)(popaddr())

#if defined(pdp11)
#define	popint	pop2
#define	pushint	push2
#else
#define popint	pop4
#define pushint	push4
#endif

/*
 * Machine specific macros for reading quantities from the
 * interpreter instruction stream. Operands in the instruction
 * stream are aligned to short, but not long boundries. Blockmarks
 * are always long aligned. Stack alignment indicates whether the
 * stack is short or long aligned. Stack alignment is assumed to
 * be no more than long aligned for ADDR32 machines, short aligned
 * for ADDR16 machines.
 */
#if defined(vax) || defined(mc68000) || defined(pdp11)
#define PCLONGVAL(target) target = *pc.lp++
#define GETLONGVAL(target, srcptr) target = *(long *)(srcptr)
#define STACKALIGN(target, value) target = ((value) + 1) &~ 1
#endif vax || mc68000 || pdp11

#ifdef tahoe
#define PCLONGVAL(target) target = *pc.sp++ << 16, target += *pc.usp++
#define GETLONGVAL(target, srcptr) \
	tsp = (short *)(srcptr), \
	target = *tsp++ << 16, target += *(unsigned short *)tsp
#define STACKALIGN(target, value) target = ((value) + 3) &~ 3
#endif tahoe

/*
 * The following macros implement all accesses to the interpreter stack.
 *
 * They used to be hard-coded assembler stuff massaged into the compiler
 * output by sed scripts, but things are cleaner now.
 *
 * The STACKSIZE is an arbitrary value.  I picked 100K since it was unlikely
 * that anybody's program would run out of stack.  Automatic allocation
 * would be nice, maybe procedure call should check for enough space + slop
 * and expand it if necessary.  Expanding the stack will require
 * pointer relocation if it moves, though.  Probably better would be a
 * command line option to set the stack size.
 */
#define	STACKSIZE	100000
#define	setup()		{ \
	extern char *malloc(); \
	stack.cp = STACKSIZE + malloc((unsigned)STACKSIZE); \
	}
#ifndef tahoe
#define	push2(x)	(*--stack.sp) = (x)
#else
#define	push2(x)	(*--stack.lp) = (x) << 16
#endif
#define push4(x)	(*--stack.lp)  = (x)
#define push8(x)	(*--stack.dbp) = (x)
#define pushsze8(x)	(*--stack.s8p) = (x)
#define pushsp(x)	(stack.cp -= (x))
#ifndef tahoe
#define pop2()		(*stack.sp++)
#else
#define pop2()		(*stack.lp++) >> 16
#endif
#define pop4()		(*stack.lp++)
#define pop8()		(*stack.dbp++)
#define popsze8()	(*stack.s8p++)
#define popsp(x)	(void)(stack.cp += (x))
#define	enableovrflo()	/*nop*/
#define	disableovrflo()	/*nop*/
