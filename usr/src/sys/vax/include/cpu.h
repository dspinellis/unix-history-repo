/*	cpu.h	4.3	81/02/26	*/

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
};
#endif
#define	VAX_780		1
#define	VAX_750		2

#define	VAX_MAX		2

#ifndef LOCORE
/*
 * Per-cpu information for system.
 */
struct	percpu {
	int	(*pc_config)();
	short	pc_cputype;
};

#ifdef KERNEL
int	cpu;
#endif
#endif
