/*
 * Intel 386 process control block
 */
#include "tss.h"
#include "fpu.h"

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
	struct	save87	pcb_savefpu;
	struct	pte	*pcb_p0br;
	struct	pte	*pcb_p1br;
	int	pcb_p0lr;
	int	pcb_p1lr;
	int	pcb_szpt; 	/* number of pages of user page table */
	int	pcb_cmap2;
	int	*pcb_sswap;
	long	pcb_sigc[5];	/* sigcode actually 19 bytes */
};

