/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trap.h	7.3 (Berkeley) %G%
 *
 * from: $Header: trap.h,v 1.9 92/11/26 02:04:47 torek Exp $
 */

#ifndef	_MACHINE_TRAP_H
#define	_MACHINE_TRAP_H
/*
 *
 * The SPARC has a Trap Base Register (TBR) which holds the upper 20 bits
 * of the trap vector table.  The next eight bits are supplied by the
 * hardware when the trap occurs, and the bottom four bits are always
 * zero (so that we can shove up to 16 bytes of executable code---exactly
 * four instructions---into each trap vector).
 *
 * The hardware allocates half the trap vectors to hardware and half to
 * software.
 *
 * Traps have priorities assigned (lower number => higher priority).
 */

#if defined(KERNEL) && !defined(LOCORE)
struct trapvec {
	int	tv_instr[4];		/* the four instructions */
};
extern struct trapvec trapbase[256];	/* the 256 vectors */
#endif

/*	trap		vec	  (pri) description	*/
#define	T_RESET		0x00	/* (1) not actually vectored; jumps to 0 */
#define	T_TEXTFAULT	0x01	/* (2) address fault during instr fetch */
#define	T_ILLINST	0x02	/* (3) illegal instruction */
#define	T_PRIVINST	0x03	/* (4) privileged instruction */
#define	T_FPDISABLED	0x04	/* (5) fp instr while fp disabled */
#define	T_WINOF		0x05	/* (6) register window overflow */
#define	T_WINUF		0x06	/* (7) register window underflow */
#define	T_ALIGN		0x07	/* (8) address not properly aligned */
#define	T_FPE		0x08	/* (9) floating point exception */
#define	T_DATAFAULT	0x09	/* (10) address fault during data fetch */
#define	T_TAGOF		0x0a	/* (11) tag overflow */
/*			0x0b	   unused */
/*			0x0c	   unused */
/*			0x0d	   unused */
/*			0x0e	   unused */
/*			0x0f	   unused */
/*			0x10	   unused */
#define	T_L1INT		0x11	/* (27) level 1 interrupt */
#define	T_L2INT		0x12	/* (26) level 2 interrupt */
#define	T_L3INT		0x13	/* (25) level 3 interrupt */
#define	T_L4INT		0x14	/* (24) level 4 interrupt */
#define	T_L5INT		0x15	/* (23) level 5 interrupt */
#define	T_L6INT		0x16	/* (22) level 6 interrupt */
#define	T_L7INT		0x17	/* (21) level 7 interrupt */
#define	T_L8INT		0x18	/* (20) level 8 interrupt */
#define	T_L9INT		0x19	/* (19) level 9 interrupt */
#define	T_L10INT	0x1a	/* (18) level 10 interrupt */
#define	T_L11INT	0x1b	/* (17) level 11 interrupt */
#define	T_L12INT	0x1c	/* (16) level 12 interrupt */
#define	T_L13INT	0x1d	/* (15) level 13 interrupt */
#define	T_L14INT	0x1e	/* (14) level 14 interrupt */
#define	T_L15INT	0x1f	/* (13) level 15 interrupt */
/*			0x20	   unused */
/*	through		0x23	   unused */
#define	T_CPDISABLED	0x24	/* (5) coprocessor instr while disabled */
/*			0x25	   unused */
/*	through		0x27	   unused */
#define	T_CPEXCEPTION	0x28	/* (9) coprocessor exception */
/*			0x29	   unused */
/*	through		0x7f	   unused */

/* beginning of `user' vectors (from trap instructions) - all priority 12 */
#define	T_SUN_SYSCALL	0x80	/* system call */
#define	T_BREAKPOINT	0x81	/* breakpoint `instruction' */
#define	T_DIV0		0x82	/* division routine was handed 0 */
#define	T_FLUSHWIN	0x83	/* flush windows */
#define	T_CLEANWIN	0x84	/* provide clean windows */
#define	T_RANGECHECK	0x85	/* ? */
#define	T_FIXALIGN	0x86	/* fix up unaligned accesses */
#define	T_INTOF		0x87	/* integer overflow ? */
#define	T_KGDB_EXEC	0x88	/* for kernel gdb */
#define	T_BSD_SYSCALL	0x89	/* BSD system call */

/* 0x8a..0xff are currently unallocated */

#ifdef KERNEL			/* pseudo traps for locore.s */
#define	T_RWRET		-1	/* need first user window for trap return */
#define	T_AST		-2	/* no-op, just needed reschedule or profile */
#endif

/* flags to system call (flags in %g1 along with syscall number) */
#define	SYSCALL_G2RFLAG	0x400	/* on success, return to %g2 rather than npc */
#define	SYSCALL_G7RFLAG	0x800	/* use %g7 as above (deprecated) */

/*
 * `software trap' macros to keep people happy (sparc v8 manual says not
 * to set the upper bits).
 */
#define	ST_BREAKPOINT	(T_BREAKPOINT & 0x7f)
#define	ST_DIV0		(T_DIV0 & 0x7f)
#define	ST_FLUSHWIN	(T_FLUSHWIN & 0x7f)
#define	ST_SYSCALL	(T_BSD_SYSCALL & 0x7f)

#endif /* _MACHINE_TRAP_H_ */
