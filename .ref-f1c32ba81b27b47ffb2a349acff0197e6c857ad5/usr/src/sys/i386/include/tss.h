/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tss.h	5.4 (Berkeley) %G%
 */

/*
 * Intel 386 Context Data Type
 */

struct i386tss {
	int	tss_link;	/* actually 16 bits: top 16 bits must be zero */
	int	tss_esp0; 	/* kernel stack pointer priviledge level 0 */
#define	tss_ksp	tss_esp0
	int	tss_ss0;	/* actually 16 bits: top 16 bits must be zero */
	int	tss_esp1; 	/* kernel stack pointer priviledge level 1 */
	int	tss_ss1;	/* actually 16 bits: top 16 bits must be zero */
	int	tss_esp2; 	/* kernel stack pointer priviledge level 2 */
	int	tss_ss2;	/* actually 16 bits: top 16 bits must be zero */
	/* struct  ptd *tss_cr3; 	/* page table directory */
	int	tss_cr3; 	/* page table directory */
#define	tss_ptd	tss_cr3
	int	tss_eip; 	/* program counter */
#define	tss_pc	tss_eip
	int	tss_eflags; 	/* program status longword */
#define	tss_psl	tss_eflags
	int	tss_eax; 
	int	tss_ecx; 
	int	tss_edx; 
	int	tss_ebx; 
	int	tss_esp; 	/* user stack pointer */
#define	tss_usp	tss_esp
	int	tss_ebp; 	/* user frame pointer */
#define	tss_fp	tss_ebp
	int	tss_esi; 
	int	tss_edi; 
	int	tss_es;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_cs;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_ss;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_ds;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_fs;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_gs;		/* actually 16 bits: top 16 bits must be zero */
	int	tss_ldt;	/* actually 16 bits: top 16 bits must be zero */
	int	tss_ioopt;	/* options & io offset bitmap: currently zero */
				/* XXX unimplemented .. i/o permission bitmap */
};
