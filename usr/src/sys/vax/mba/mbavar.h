/*	mbavar.h	4.2	%G%	*/

#if VAX==780
/*
 * VAX Massbus adapter registers
 */

struct mba_regs
{
	int	mba_csr;
	int	mba_cr;
	int	mba_sr;
	int	mba_var;
	int	mba_bcr;
	int	mba_dr;
	int	mba_pad1[250];
	int	mba_erb[8][32];		/* external register base */
	struct	pte mba_map[256];
};

#define	MBAINIT		0x1
#define	MBAIE		0x4

#define	MBAEBITS	0xe0770

#define	PHYSMBA0	((struct mba_regs *)0x20010000)
#define	PHYSMBA1	((struct mba_regs *)0x20012000)

#define	mbadev(mba,unit)	((struct device *)mba->mba_erb[unit])

#ifdef KERNEL
struct mba_info
{
	struct	mba_regs *mi_loc;	/* virtual mba */
	struct	mba_regs *mi_phys;	/* physical mba */
	struct	pte *mi_map;		/* page table base for nexus */
};

#define	MBA0		((struct mba_regs *)0x80064000)
#define	MBA1		((struct mba_regs *)0x80066000)

int	mbanum[];
struct	mba_info mbainfo[];
int	mbaact;
#endif
#endif
