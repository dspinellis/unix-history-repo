/*	ubavar.h	4.11	81/02/23	*/

/*
 * UNIBUS adaptor
 */
#if VAX750
#define	UBA750	((struct uba_regs *)0xf30000)
#define	UMEM750	((u_short *)0xfc0000)
#endif

#if VAX780
/*
 * UBA registers
 */
struct uba_regs
{
	int	uba_cnfgr;		/* configuration register */
	int	uba_cr;			/* control register */
	int	uba_sr;			/* status register */
	int	uba_dcr;		/* diagnostic control register */
	int	uba_fmer;		/* failed map entry register */
	int	uba_fubar;		/* failed UNIBUS address register */
	int	pad1[2];
	int	uba_brsvr[4];
	int	uba_brrvr[4];		/* receive vector registers */
	int	uba_dpr[16];		/* buffered data path register */
	int	pad2[480];
	struct	pte uba_map[496];	/* unibus map register */
	int	pad3[16];		/* no maps for device address space */
};

/* UBA control register, UBACR */
#define	UBA_MRD16	0x40000000	/* map reg disable bit 4 */
#define	UBA_MRD8	0x20000000	/* map reg disable bit 3 */
#define	UBA_MRD4	0x10000000	/* map reg disable bit 2 */
#define	UBA_MRD2	0x08000000	/* map reg disable bit 1 */
#define	UBA_MRD1	0x04000000	/* map reg disable bit 0 */
#define	UBA_IFS		0x00000040	/* interrupt field switch */
#define	UBA_BRIE	0x00000020	/* BR interrupt enable */
#define	UBA_USEFIE	0x00000010	/* UNIBUS to SBI error field IE */
#define	UBA_SUEFIE	0x00000008	/* SBI to UNIBUS error field IE */
#define	UBA_CNFIE	0x00000004	/* configuration IE */
#define	UBA_UPF		0x00000002	/* UNIBUS power fail */
#define	UBA_ADINIT	0x00000001	/* adapter init */

/* UBA status register, UASR */
#define	UBA_BR7FULL	0x08000000	/* BR7 receive vector reg full */
#define	UBA_BR6FULL	0x04000000	/* BR6 receive vector reg full */
#define	UBA_BR5FULL	0x02000000	/* BR5 receive vector reg full */
#define	UBA_BR4FULL	0x01000000	/* BR4 receive vector reg full */
#define	UBA_RDTO	0x00000400	/* UNIBUS to SBI read data timeout */
#define	UBA_RDS		0x00000200	/* read data substitute */
#define	UBA_CRD		0x00000100	/* corrected read data */
#define	UBA_CXTER	0x00000080	/* command transmit error */
#define	UBA_CXTMO	0x00000040	/* command transmit timeout */
#define	UBA_DPPE	0x00000020	/* data path parity error */
#define	UBA_IVMR	0x00000010	/* invalid map register */
#define	UBA_MRPF	0x00000008	/* map register parity failure */
#define	UBA_LEB		0x00000004	/* lost error */
#define	UBA_UBSTO	0x00000002	/* UNIBUS select timeout */
#define	UBA_UBSSTO	0x00000001	/* UNIBUS slave sync timeout */

/* BR receive vector register, BRRVR */
#define	UBA_AIRI	0x80000000	/* adapter interrupt request */
#define	UBA_DIV		0x0000ffff	/* device interrupt vector field */
#endif
 
/* data path register, DPR */
#if VAX780
#define	UBA_BNE		0x80000000	/* buffer not empty - purge */
#define	UBA_BTE		0x40000000	/* buffer transfer error */
#define	UBA_DPF		0x20000000	/* DP function (RO) */
#define	UBA_BS		0x007f0000	/* buffer state field */
#define	UBA_BUBA	0x0000ffff	/* buffered UNIBUS address */
#endif
#if VAX750
#define	UBA_ERROR	0x80000000	/* error occurred */
#define	UBA_NXM		0x40000000	/* nxm from memory */
#define	UBA_UCE		0x20000000	/* uncorrectable error */
#define	UBA_PURGE	0x00000001	/* purge bdp */
#endif
 
/* map register, MR */
#define	UBA_MRV		0x80000000	/* map register valid */
#define	UBA_BO		0x02000000	/* byte offset bit */
#define	UBA_DPDB	0x01e00000	/* data path designator field */
#define	UBA_SBIPFN	0x000fffff	/* SBI page address field */

#define	UBA_DPSHIFT	21		/* shift to data path designator */

/*
 * Each UNIBUS mass storage controller has uba_minfo structure,
 * and a uba_dinfo structure (as below) for each attached drive.
 */
struct uba_minfo {
	struct	uba_driver *um_driver;
	short	um_ctlr;	/* controller index in driver */
	short	um_ubanum;	/* the uba it is on */
	short	um_alive;	/* controller exists */
	int	(**um_intr)();	/* interrupt handler(s) */
	caddr_t	um_addr;	/* address of device in i/o space */
	struct	uba_hd *um_hd;
	int	um_cmd;		/* communication to dgo() */
	int	um_ubinfo;	/* save unibus registers, etc */
	struct	buf um_tab;	/* queue for this controller */
};
/*
 * Each UNIBUS device has a uba_dinfo structure.
 * If a controller has many drives attached, then there will
 * be several uba_dinfo structures associated with a single uba_minfo
 * structure.
 */
struct uba_dinfo {
	struct	uba_driver *ui_driver;
	short	ui_unit;	/* unit number on the system */
	short	ui_ctlr;	/* mass ctlr number; -1 if none */
	short	ui_ubanum;	/* the uba it is on */
	short	ui_slave;	/* slave on controller */
	int	(**ui_intr)();	/* interrupt handler(s) */
	caddr_t	ui_addr;	/* address of device in i/o space */
	short	ui_dk;		/* if init 1 set to number for iostat */
	short	ui_flags;	/* param to device init. */
	short	ui_alive;	/* device exists */
	short	ui_type;	/* driver specific type information */
	caddr_t	ui_physaddr;	/* phys addr, for standalone (dump) code */
	struct	uba_dinfo *ui_forw;
/* if the device isn't also a controller, this is the controller it is on */
	struct	uba_minfo *ui_mi;
	struct	uba_hd *ui_hd;
};

#define	NUBA780	4
#define	NUBA750	1
#if VAX780
#define	MAXNUBA	4
#else
#define	MAXNUBA	1
#endif

/*
 * This structure exists per-uba.
 *
 * N.B.: THE SIZE AND SHAPE OF THIS STRUCTURE IS KNOWN IN uba.m.
 */
struct	uba_hd {
	int	uh_active;		/* bit per device transferring */
	struct	uba_regs *uh_uba;	/* virt addr of uba */
	struct	uba_regs *uh_physuba;	/* phys addr of uba */
	int	(**uh_vec)();		/* interrupt vector */
	struct	uba_dinfo *uh_actf;	/* head of queue to transfer */
	struct	uba_dinfo *uh_actl;	/* tail of queue to transfer */
	short	uh_mrwant;		/* someone is waiting for map reg */
	short	uh_bdpwant;		/* someone awaits bdp's */
	int	uh_bdpfree;		/* free bdp's */
	int	uh_hangcnt;		/* number of ticks hung */
	int	uh_zvcnt;		/* number of 0 vectors */
	short	uh_users;		/* transient bdp use count */
	short	uh_xclu;		/* an rk07 is using this uba! */
#define	UAMSIZ	50
	struct	map *uh_map;
} uba_hd[MAXNUBA];
/*
 * Each UNIBUS driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 */
struct uba_driver {
	int	(*ud_probe)();		/* see if a driver is really there */
	int	(*ud_slave)();		/* see if a slave is there */
	int	(*ud_attach)();		/* setup driver for a slave */
	int	(*ud_dgo)();		/* fill csr/ba to start transfer */
	u_short	*ud_addr;		/* device csr addresses */
	char	*ud_dname;		/* name of a device */
	struct	uba_dinfo **ud_dinfo;	/* backpointers to ubdinit structs */
	char	*ud_mname;		/* name of a controller */
	struct	uba_minfo **ud_minfo;	/* backpointers to ubminit structs */
	short	ud_xclu;		/* want exclusive use of bdp's */
};

/*
 * unibus maps
 */
#define	NBDP780	15
#define	NBDP750	3
#define	MAXNBDP	15

#define	NUBMREG	496

/*
 * flags to uba map/bdp allocation routines
 */
#define	UBA_NEEDBDP	1		/* transfer needs a bdp */
#define	UBA_CANTWAIT	2		/* don't block me */
#define	UBA_NEED16	3		/* need 16 bit addresses only */

/*
 * UNIBUS related kernel variables
 */
#ifdef KERNEL
extern	struct	uba_minfo ubminit[];
extern	struct	uba_dinfo ubdinit[];
int	numuba;
extern	struct pte UMEMmap[MAXNUBA][16];
extern	char umem[MAXNUBA][16*NBPG];
extern	int (*UNIvec[])();
#if VAX780
extern	Xua0int(), Xua1int(), Xua2int(), Xua3int();
#endif
#endif
