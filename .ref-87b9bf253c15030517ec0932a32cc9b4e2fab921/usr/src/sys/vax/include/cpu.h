/*	cpu.h	4.7	82/05/26	*/

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
	/* need structure for 730 */
};
#endif
#define	VAX_780		1
#define	VAX_750		2
#define	VAX_730		3

#define	VAX_MAX		3

#ifndef LOCORE
/*
 * Per-cpu information for system.
 */
struct	percpu {
	short	pc_cputype;		/* cpu type code */
	short	pc_nnexus;		/* number of nexus slots */
	struct	nexus *pc_nexbase;	/* base of nexus space */
/* we should be able to have just one address for the unibus memories */
/* and calculate successive addresses by adding to the base, but the 750 */
/* doesn't obey the sensible rule: uba1 has a lower address than uba0! */
	caddr_t	*pc_umaddr;		/* unibus memory addresses */
	short	pc_nubabdp;		/* number of bdp's per uba */
	short	pc_haveubasr;		/* have uba status register */
/* the 750 has some slots which don't promise to tell you their types */
/* if this pointer is non-zero, then you get the type from this array */
/* rather than from the (much more sensible) low byte of the config register */
	short	*pc_nextype;		/* botch */
};

#ifdef KERNEL
int	cpu;
struct	percpu percpu[];
#endif
#endif
