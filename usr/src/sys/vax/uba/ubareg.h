/*	ubareg.h	4.5	%G%	*/

/*
 * unibus adapter
 */

#if VAX780
/*
 * UBA registers
 */
struct uba_regs
{
	int	uba_cnfgr;
	int	uba_cr;
	int	uba_sr;
	int	uba_dcr;
	int	uba_fmer;
	int	uba_fubar;
	int	pad1[2];
	int	uba_brsvr[4];
	int	uba_brrvr[4];
	int	uba_dpr[16];
	int	pad2[480];
	struct	pte uba_map[496];
	int	pad3[16];
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
#define	UBA_ERROR	0x20000000
#define	UBA_NXM		0x40000000
#define	UBA_UCE		0x20000000
#define	UBA_PURGE	0x00000001
#endif
 
/* map register, MR */
#define	UBA_MRV		0x80000000	/* map register valid */
#define	UBA_BO		0x02000000	/* byte offset bit */
#define	UBA_DPDB	0x01e00000	/* data path designator field */
#define	UBA_SBIPFN	0x000fffff	/* SBI page address field */

#define	UBA_DPSHIFT	21		/* shift to data path designator */

/*
 * each UNIBUS mass storage controller has uba_minfo structure.
 */
struct	uba_minfo {
	short	um_num;		/* controller index in driver */
	short	um_ubanum;	/* the uba it is on */
	short	um_alive;	/* controller exists */
	caddr_t	um_addr;	/* address of device in i/o space */
	struct	buf um_tab;	/* queue for this controller */
	struct	uba_info *um_forw;
};
/*
 * each UNIBUS device has a uba_dinfo structure.
 * controllers which are not for mass storage often have ONLY
 * a uba_dinfo structure, and no uba_minfo structure.
 * if a controller has many drives attached, then there will
 * be several uba_dinfo structures pointing at the same uba_minfo
 * structure.
 */
struct	uba_dinfo {
	struct	uba_driver *ui_driver;
	short	ui_name;
	short	ui_unit;	/* unit number on the system */
	short	ui_ubanum;	/* the uba it is on */
	short	ui_slave;	/* slave on controller */
	int	(**ui_intr)();	/* interrupt handler(s) */
	caddr_t	ui_addr;	/* address of device in i/o space */
	short	ui_alive;	/* device exists */
	short	ui_type;	/* driver specific type information */
	short	ui_dk;		/* device number for iostat */
	caddr_t	ui_physaddr;	/* phys addr, for standalone (dump) code */
/* if the driver isn't also a controller, this is the controller it is on */
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
 * header per uba.		CAUTION: size & offsets known in uba.m
 */
struct	uba_hd {
	int	uh_active;		/* bit per device transferring */
	struct	uba_regs *uh_uba;	/* virt addr of uba */
	struct	uba_regs *uh_physuba;	/* phys addr of uba */
	int	(**uh_vec)();		/* interrupt vector */
	struct	uba_minfo *uh_actf;	/* head of queue to transfer */
	struct	uba_minfo *uh_actl;	/* tail of queue to transfer */
	short	uh_mrwant;		/* someone is waiting for map reg */
	short	uh_bdpwant;		/* someone awaits bdp's */
	int	uh_bdpfree;		/* free bdp's */
	int	uh_hangcnt;		/* number of ticks hung */
	int	uh_zvcnt;		/* number of 0 vectors */
#define	UAMSIZ 50
	struct	map uh_map[UAMSIZ];
} uba_hd[MAXNUBA];
#ifdef KERNEL
extern	struct	uba_minfo ubminit[];
extern	struct	uba_dinfo ubdinit[];
int	numuba;
#endif
/*
 * each unibus driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 */
struct uba_driver {
	int	(*ud_cntrlr)();		/* see if a driver is really there */
	int	(*ud_slave)();		/* see if a slave is there; init */
	int	(*ud_dgo)();		/* routine to stuff driver regs */
/* dgo is called back by the unibus (usu ubaalloc), when the bus is ready */
	short	ud_maxslave;		/* max number of slaves */
	short	ud_needexcl;		/* need exclusive use of uba (rk07) */
	u_short	*ud_addr;		/* device csr addresses */
	char	*ud_pname;
	struct	uba_dinfo **ud_info;	/* backpointers to ubinit structs */
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
