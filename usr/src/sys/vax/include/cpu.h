/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cpu.h	7.2 (Berkeley) %G%
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
 	struct cpu630 {
 		u_int	:24,			/* reserved */
 			cp_type:8;		/* VAX_630 */
 	} cpu630;
};
#endif
/*
 * Vax CPU types.
 * Similar types are grouped with their earliest example.
 */
#define	VAX_780		1
#define	VAX_750		2
#define	VAX_730		3
#define VAX_8600	4
#define	VAX_630		8

#define	VAX_MAX		8

/*
 * Main IO backplane types.
 * This gives us a handle on how to do autoconfiguration.
 */
#define	IO_SBI780	1
#define	IO_CMI750	2
#define	IO_XXX730	3
#define IO_ABUS		4
#define IO_QBUS		5

#ifndef LOCORE
/*
 * Per-cpu information for system.
 */
struct	percpu {
	short	pc_cputype;		/* cpu type code */
	short	pc_cpuspeed;		/* relative speed of cpu */
	short	pc_nioa;		/* number of IO adaptors/nexus blocks */
	struct	iobus *pc_io;		/* descriptions of IO adaptors */
};

/*
 * Generic description of an I/O "adaptor"
 * (any top-level I/O bus visible to software
 * and requiring autoconfiguration).
 * The remainder of the description
 * is pointed to by io_details.
 */
struct iobus {
	int	io_type;		/* io adaptor types */
	caddr_t	io_addr;		/* phys address of IO adaptor */
	int	io_size;		/* size of an IO space */
	caddr_t	io_details;		/* specific to adaptor types */
};

/*
 * Description of a main bus that maps "nexi", ala the 780 SBI.
 */
struct nexusconnect {
	short	psb_nnexus;		/* number of nexus slots */
	struct	nexus *psb_nexbase;	/* base of nexus space */
	short	psb_ubatype;		/* type of "unibus adaptor" */
	short	psb_nubabdp;		/* number of bdp's per uba */
	caddr_t	*psb_umaddr;		/* "unibus" memory addresses */
/* the 750 has some slots which don't promise to tell you their types */
/* if this pointer is non-zero, then you get the type from this array */
/* rather than from the (much more sensible) low byte of the config register */
	short	*psb_nextype;		/* botch */
};

/*
 * Description of a Q-bus configuration.
 */
struct qbus {
	int	qb_type;		/* type of "unibus adaptor" */
	int	qb_memsize;		/* size of (used) memory, pages */
	struct	pte *qb_map;		/* base of map registers */
	caddr_t	qb_maddr;		/* "unibus" memory address */
	caddr_t	qb_iopage;		/* "unibus" IO page address */
};

#ifdef KERNEL
int	cpu;
struct	percpu percpu[];
#endif
#endif
