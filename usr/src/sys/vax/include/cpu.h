/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cpu.h	6.4 (Berkeley) %G%
 */

#ifndef LOCORE
/*
 * Cpu identification, from SID register.
 */
union cpusid {
	int	cpusid;
	struct cpuany {
		u_int	:24,
			cp_type:8;
	} cpuany;
	struct cpu8600 {
		u_int	cp_sno:12,		/* serial number */
			cp_plant:4,		/* plant number */
			cp_eco:8,		/* eco level */
			cp_type:8;		/* VAX_8600 */
	} cpu8600;
	struct cpu780 {
		u_int	cp_sno:12,		/* serial number */
			cp_plant:3,		/* plant number */
			cp_eco:9,		/* eco level */
			cp_type:8;		/* VAX_780 */
	} cpu780;
	struct cpu750 {
		u_int	cp_hrev:8,		/* hardware rev level */
			cp_urev:8,		/* ucode rev level */
			:8,
			cp_type:8;		/* VAX_750 */
	} cpu750;
	struct cpu730 {
		u_int	:8,			/* reserved */
			cp_urev:8,		/* ucode rev level */
			:8,			/* reserved */
			cp_type:8;		/* VAX_730 */
	} cpu730;
};
#endif
#define	VAX_780		1
#define	VAX_750		2
#define	VAX_730		3
#define VAX_8600	4

#define	VAX_MAX		4

#ifndef LOCORE
/*
 * Per-cpu information for system.
 */
struct	percpu {
	short	pc_cputype;		/* cpu type code */
	short	pc_nioa;		/* number of IO adaptors/SBI's */
	caddr_t	*pc_ioaaddr;		/* phys addresses of IO adaptors */
	int	pc_ioasize;		/* size of a IO adaptor */
	short	*pc_ioatype;		/* io adaptor types if no cfg reg */
};

struct persbi {
	short	psb_nnexus;		/* number of nexus slots */
	struct	nexus *psb_nexbase;	/* base of nexus space */
/* we should be able to have just one address for the unibus memories */
/* and calculate successive addresses by adding to the base, but the 750 */
/* doesn't obey the sensible rule: uba1 has a lower address than uba0! */
	caddr_t	*psb_umaddr;		/* unibus memory addresses */
	short	psb_nubabdp;		/* number of bdp's per uba */
	short	psb_haveubasr;		/* have uba status register */
/* the 750 has some slots which don't promise to tell you their types */
/* if this pointer is non-zero, then you get the type from this array */
/* rather than from the (much more sensible) low byte of the config register */
	short	*psb_nextype;		/* botch */
};

#ifdef KERNEL
int	cpu;
struct	percpu percpu[];
#if VAX730
struct persbi xxx730;
#endif
#if VAX750
struct persbi cmi750;
#endif
#if VAX780
struct persbi sbi780;
#endif
#if VAX8600
struct persbi sbi8600[];
#endif
#endif
#endif
