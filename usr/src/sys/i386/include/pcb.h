/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)pcb.h	5.3 (Berkeley) %G%
 */

/*
 * Intel 386 process control block
 */
#include "tss.h"
#include "../i386/npx.h"

struct pcb {
	struct	i386tss pcbtss;
#define	pcb_ksp	pcbtss.tss_esp0
#define	pcb_ptd	pcbtss.tss_cr3
#define	pcb_pc	pcbtss.tss_eip
#define	pcb_psl	pcbtss.tss_eflags
#define	pcb_usp	pcbtss.tss_esp
#define	pcb_fp	pcbtss.tss_ebp
/*
 * Software pcb (extension)
 */
	int	pcb_fpsav;
#define	FP_NEEDSAVE	0x1	/* need save on next context switch */
#define	FP_NEEDRESTORE	0x2	/* need restore on next DNA fault */
#define	FP_USESEMC	0x4	/* process uses EMC memory-mapped mode */
	struct	save87	pcb_savefpu;
	struct	emcsts	pcb_saveemc;
	struct	pte	*pcb_p0br;
	struct	pte	*pcb_p1br;
	int	pcb_p0lr;
	int	pcb_p1lr;
	int	pcb_szpt; 	/* number of pages of user page table */
	int	pcb_cmap2;
	int	*pcb_sswap;
	long	pcb_sigc[8];	/* sigcode actually 19 bytes */
	int	pcb_iml;	/* interrupt mask level */
};
