/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pcb.h	8.2 (Berkeley) %G%
 */

/*
 * Intel 386 process control block
 */
#include <machine/tss.h>
#include <machine/npx.h>

struct pcb {
	struct	i386tss pcb_tss;
#define	pcb_ksp	pcb_tss.tss_esp0
#define	pcb_ptd	pcb_tss.tss_cr3
#define	pcb_cr3	pcb_ptd
#define	pcb_pc	pcb_tss.tss_eip
#define	pcb_psl	pcb_tss.tss_eflags
#define	pcb_usp	pcb_tss.tss_esp
#define	pcb_fp	pcb_tss.tss_ebp
#ifdef	notyet
	u_char	pcb_iomap[NPORT/sizeof(u_char)]; /* i/o port bitmap */
#endif
	struct	save87	pcb_savefpu;	/* floating point state for 287/387 */
	struct	emcsts	pcb_saveemc;	/* Cyrix EMC state */
/*
 * Software pcb (extension)
 */
	int	pcb_flags;
#define	FP_WASUSED	0x01	/* floating point has been used in this proc */
#define	FP_NEEDSSAVE	0x02	/* needs save on next context switch */
#define	FP_NEEDSRESTORE	0x04	/* need restore on next DNA fault */
#define	FP_USESEMC	0x08	/* process uses EMC memory-mapped mode */
#define	FM_TRAP		0x10	/* process entered kernel on a trap frame */
	short	pcb_iml;	/* interrupt mask level */
	caddr_t	pcb_onfault;	/* copyin/out fault recovery */
	long	pcb_sigc[8];	/* XXX signal code trampoline */
	int	pcb_cmap2;	/* XXX temporary PTE - will prefault instead */
};

/*
 * The pcb is augmented with machine-dependent additional data for
 * core dumps. For the i386: ???
 */
struct md_coredump {
        int     pad;		/* XXX? -- cgd */
};

#ifdef KERNEL
struct pcb *curpcb;		/* our current running pcb */
#endif
