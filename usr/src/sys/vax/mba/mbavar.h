/*	mbavar.h	4.12	81/03/03	*/

/*
 * VAX Massbus adapter registers
 */

struct mba_regs
{
	int	mba_csr;		/* configuration register */
	int	mba_cr;			/* control register */
	int	mba_sr;			/* status register */
	int	mba_var;		/* virtual address register */
	int	mba_bcr;		/* byte count register */
	int	mba_dr;
	int	mba_pad1[250];
	struct mba_drv {		/* per drive registers */
		int	mbd_cs1;		/* control status */
		int	mbd_ds;			/* drive status */
		int	mbd_er1;		/* error register */
		int	mbd_mr1;		/* maintenance register */
		int	mbd_as;			/* attention status */
		int	mbd_da;			/* desired address (disks) */
#define	mbd_fc	mbd_da				/* frame count (tapes) */
		int	mbd_dt;			/* drive type */
		int	mbd_la;			/* look ahead (disks) */
#define	mbd_ck	mbd_la				/* ??? (tapes) */
		int	mbd_sn;			/* serial number */
		int	mbd_of;			/* ??? */
#define	mbd_tc	mbd_of				/* ??? */
		int	mbd_fill[22];
	} mba_drv[8];
	struct	pte mba_map[256];	/* io space virtual map */
	int	mba_pad2[256*5];	/* to size of a nexus */
};

/*
 * Bits in mba_cr
 */
#define	MBAINIT		0x1		/* init mba */
#define	MBAIE		0x4		/* enable mba interrupts */

/*
 * Bits in mba_sr
 */
#define	MBS_DTBUSY	0x80000000	/* data transfer busy */
#define	MBS_NRCONF	0x40000000	/* no response confirmation */
#define	MBS_CRD		0x20000000	/* corrected read data */
#define	MBS_CBHUNG	0x00800000	/* control bus hung */
#define	MBS_PGE		0x00080000	/* programming error */
#define	MBS_NED		0x00040000	/* non-existant drive */
#define	MBS_MCPE	0x00020000	/* massbus control parity error */
#define	MBS_ATTN	0x00010000	/* attention from massbus */
#define	MBS_SPE		0x00004000	/* silo parity error */
#define	MBS_DTCMP	0x00002000	/* data transfer completed */
#define	MBS_DTABT	0x00001000	/* data transfer aborted */
#define	MBS_DLT		0x00000800	/* data late */
#define	MBS_WCKUP	0x00000400	/* write check upper */
#define	MBS_WCKLWR	0x00000200	/* write check lower */
#define	MBS_MXF		0x00000100	/* miss transfer error */
#define	MBS_MBEXC	0x00000080	/* massbus exception */
#define	MBS_MDPE	0x00000040	/* massbus data parity error */
#define	MBS_MAPPE	0x00000020	/* page frame map parity error */
#define	MBS_INVMAP	0x00000010	/* invalid map */
#define	MBS_ERRCONF	0x00000008	/* error confirmation */
#define	MBS_RDS		0x00000004	/* read data substitute */
#define	MBS_ISTIMO	0x00000002	/* interface sequence timeout */
#define	MBS_RDTIMO	0x00000001	/* read data timeout */

#define MBASR_BITS \
"\20\40DTBUSY\37NRCONF\36CRD\30CBHUNG\24PGE\23NED\22MCPE\21ATTN\
\17SPE\16DTCMP\15DTABT\14DLT\13WCKUP\12WCKLWR\11MXF\10MBEXC\7MDPE\
\6MAPPE\5INVMAP\4ERRCONF\3RDS\2ISTIMO\1RDTIMO"

#define	MBASR_HARD	(MBS_PGE|MBS_ERRCONF|MBS_ISTIMO|MBS_RDTIMO)

#define MBAEBITS	(~(MBS_DTBUSY|MBS_CRD|MBS_ATTN|MBS_DTCMP))

extern	char	mbasr_bits[];

/*
 * Commands for mbd_cs1
 */
#define	MBD_WCOM	0x30
#define	MBD_RCOM	0x38
#define	MBD_GO		0x1

/*
 * Bits in mbd_ds.
 */
#define	MBD_DRY		0x80		/* drive ready */
#define	MBD_MOL		0x1000		/* medium on line */
#define	MBD_DPR		0x100		/* drive present */
#define	MBD_ERR		0x4000		/* error in drive */

/*
 * Bits in mbd_dt
 */
#define	MBDT_NSA	0x8000		/* not sector addressible */
#define	MBDT_TAP	0x4000		/* is a tape */
#define	MBDT_MOH	0x2000		/* moving head */
#define	MBDT_7CH	0x1000		/* 7 channel */
#define	MBDT_DRQ	0x800		/* drive request required */
#define	MBDT_SPR	0x400		/* slave present */

#define	MBDT_TYPE	0x1ff
#define	MBDT_MASK	(MBDT_NSA|MBDT_TAP|MBDT_TYPE)

/* type codes for disk drives */
#define	MBDT_RP04	020
#define	MBDT_RP05	021
#define	MBDT_RP06	022
#define	MBDT_RP07	042
#define	MBDT_RM03	024
#define	MBDT_RM05	027
#define	MBDT_RM80	026

/* type codes for tape drives */
#define	MBDT_TM03	050
#define	MBDT_TE16	051
#define	MBDT_TU45	052
#define	MBDT_TU77	054
#define	MBDT_TU78	0140		/* can't handle these (yet) */

/*
 * Each driver has an array of pointers to these structures, one for
 * each device it is willing to handle.  At bootstrap time, the
 * driver tables are filled in;
 */
struct	mba_info {
	struct	mba_driver *mi_driver;
	short	mi_name;	/* two character generic name */
	short	mi_unit;	/* unit number to the system */
	short	mi_mbanum;	/* the mba it is on */
	short	mi_drive;	/* controller on mba */
	short	mi_slave;	/* slave to controller (TM03/TU16) */
	short	mi_dk;		/* driver number for iostat */
	short	mi_alive;	/* device exists */
	short	mi_type;	/* driver specific unit type */
	struct	buf mi_tab;	/* head of queue for this device */
	struct	mba_info *mi_forw;
/* we could compute these every time, but hereby save time */
	struct	mba_regs *mi_mba;
	struct	mba_drv *mi_drv;
	struct	mba_hd *mi_hd;
};

/*
 * The initialization routine uses the information in the mbinit table
 * to initialize the drive type routines in the drivers and the
 * mbahd array summarizing which devices are hooked to which massbus slots.
 */
struct	mba_hd {
	short	mh_active;
	short	mh_ndrive;
	struct	mba_regs *mh_mba;	/* virt addr of mba */
	struct	mba_regs *mh_physmba;	/* phys addr of mba */
	struct	mba_info *mh_mbip[8];	/* what is attached */
	struct	mba_info *mh_actf;	/* head of queue to transfer */
	struct	mba_info *mh_actl;	/* tail of queue to transfer */
};
/*
 * Values for flags; normally MH_NOSEEK will be set when there is
 * only a single drive on an massbus.
 */
#define	MH_NOSEEK	1
#define	MH_NOSEARCH	2

/*
 * Each massbus driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 */
struct mba_driver {
	int	(*md_dkinit)();		/* setup dk info (mspw) */
	int	(*md_ustart)();		/* unit start routine */
	int	(*md_start)();		/* setup a data transfer */
	int	(*md_dtint)();		/* data transfer complete */
	int	(*md_ndint)();		/* non-data transfer interrupt */
	short	*md_type;		/* array of drive type codes */
	struct	mba_info **md_info;	/* backpointers to mbinit structs */
};

/*
 * Possible return values from unit start routines.
 */
#define	MBU_NEXT	0		/* skip to next operation */
#define	MBU_BUSY	1		/* dual port busy; wait for intr */
#define	MBU_STARTED	2		/* non-data transfer started */
#define	MBU_DODATA	3		/* data transfer ready; start mba */

/*
 * Possible return values from data transfer interrupt handling routines
 */
#define	MBD_DONE	0		/* data transfer complete */
#define	MBD_RETRY	1		/* error occurred, please retry */
#define	MBD_RESTARTED	2		/* driver restarted i/o itself */

/*
 * Possible return values from non-data-transfer interrupt handling routines
 */
#define	MBN_DONE	0		/* non-data transfer complete */
#define	MBN_RETRY	1		/* failed; retry the operation */

/*
 * Clear attention status for specified drive.
 */
#define	mbclrattn(mi)	((mi)->mi_mba->mba_drv[0].mbd_as = 1 << (mi)->mi_drive)

/*
 * Kernel definitions related to mba.
 */
#ifdef KERNEL
#if NMBA > 0
struct	mba_hd mba_hd[NMBA];
extern	Xmba0int(), Xmba1int(), Xmba2int(), Xmba3int();
extern	struct	mba_info mbinit[];	/* blanks for filling mba_info */
int	nummba;
#endif
#endif
