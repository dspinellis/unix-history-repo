/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)tss.h	5.2 (Berkeley) %G%
 */

/*
 * Intel 386 Context Data Type
 */


struct i386tss {
	long	tss_link;	/* actually 16 bits: top 16 bits must be zero */
	long	tss_esp0; 	/* kernel stack pointer priviledge level 0 */
#define	tss_ksp	tss_esp0
	long	tss_ss0;	/* actually 16 bits: top 16 bits must be zero */
	long	tss_esp1; 	/* kernel stack pointer priviledge level 1 */
	long	tss_ss1;	/* actually 16 bits: top 16 bits must be zero */
	long	tss_esp2; 	/* kernel stack pointer priviledge level 2 */
	long	tss_ss2;	/* actually 16 bits: top 16 bits must be zero */
	long	tss_cr3; 	/* page table directory physical address */
#define	tss_ptd	tss_cr3
	long	tss_eip; 	/* program counter */
#define	tss_pc	tss_eip
	long	tss_eflags; 	/* program status longword */
#define	tss_psl	tss_eflags
	long	tss_eax; 
	long	tss_ecx; 
	long	tss_edx; 
	long	tss_ebx; 
	long	tss_esp; 	/* user stack pointer */
#define	tss_usp	tss_esp
	long	tss_ebp; 	/* user frame pointer */
#define	tss_fp	tss_ebp
	long	tss_esi; 
	long	tss_edi; 
	long	tss_es;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_cs;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_ss;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_ds;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_fs;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_gs;		/* actually 16 bits: top 16 bits must be zero */
	long	tss_ldt;	/* actually 16 bits: top 16 bits must be zero */
	long	tss_ioopt;	/* options & io offset bitmap: currently zero */
				/* XXX unimplemented .. i/o permission bitmap */
};
