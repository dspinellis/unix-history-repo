/*	mbareg.h	4.3	%G%	*/

#if VAX==780
/*
 * VAX Massbus adapter registers
 */

struct mba_drv
{
	int	mbd_cs1;
	int	mbd_ds;
	int	mbd_er1;
	int	mbd_mr1;
	int	mbd_as;
	int	mbd_da;
#define	mbd_fc	mbd_da
	int	mbd_dt;
	int	mbd_la;
#define	mbd_ck	mbd_la
	int	mbd_sn;
	int	mbd_of;
#define	mbd_tc	mbd_of
	int	mbd_fill[22];
};
/*
 * Bits in mbd_dt.
 */
#define	MBDT_NSA	0x8000		/* not sector addressible */
#define	MBDT_TAP	0x4000		/* is a tape */
#define	MBDT_MOH	0x2000		/* moving head */
#define	MBDT_7CH	0x1000		/* 7 channel */
#define	MBDT_DRQ	0x800		/* drive request required */
#define	MBDT_SPR	0x400		/* slave present */

#define	MBDT_TYPE	0x1ff
#define	MBDT_MASK	(MBDT_NSA|MBDT_TAP|MBDT_TYPE)

#define	MBDT_RP04	020
#define	MBDT_RP05	021
#define	MBDT_RP06	022
#define	MBDT_RP07	042
#define	MBDT_RM03	024
#define	MBDT_RM05	027
#define	MBDT_RM80	026

#define	MBDT_TM03	050
#define	MBDT_TE16	051
#define	MBDT_TU45	052
#define	MBDT_TU77	054
#define	MBDT_TU78	0140

/*
 * Bits in mbd_ds.
 */
#define	MBD_DRY		0x80
#define	MBD_MOL		0x1000
#define	MBD_DPR		0x100
#define	MBD_ERR		0x4000

#define	MBD_WCOM	0x30
#define	MBD_RCOM	0x38
#define	MBD_GO		0x1

struct mba_regs
{
	int	mba_csr;
	int	mba_cr;
	int	mba_sr;
	int	mba_var;
	int	mba_bcr;
	int	mba_dr;
	int	mba_pad1[250];
	struct	mba_drv mba_drv[8];
	struct	pte mba_map[256];
	int	mba_pad2[256*5];
};

#define	MBAINIT		0x1
#define	MBAIE		0x4

#define	MBAEBITS	0xe0770

#define	PHYSMBA0	((struct mba_regs *)0x20010000)
#define	PHYSMBA1	((struct mba_regs *)0x20012000)

#define	mbadev(mba,unit)	((struct device *)&mba->mba_drv[unit])

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
