/*	ubareg.h	3.1	%H%	*/

/*
 * Unibus adapter
 */

#define	UBA0		0x80040000	/* sys virt i/o for UBA 0 */
#define	UBA0_DEV (UBA0+0x2000-0160000)	/* sys virt of device regs */

#define	UNIBASE 0760000	 		/* UNIBUS phys base of i/o reg's */

/* UBA Configuration Register, CNFGR */
#define	PARFLT		0x80000000	/* SBI Parity Fault */
#define	WSQFLT		0x40000000	/* SBI Write Sequence Fault */
#define	URDFLT		0x20000000	/* SBI Unexpected Read Fault */
#define	ISQFLT		0x10000000	/* SBI Interlock Sequence Fault */
#define	MXTFLT		0x8000000	/* SBI Multiple Transmitter Fault */
#define	XMTFLT		0x4000000	/* UBA is transmit faulter */
#define	ADPDN		0x800000	/* Adapter Power Down */
#define	ADPUP		0x400000	/* Adapter Power Up */
#define	UBINIT		0x40000		/* UNIBUS INIT is asserted */
#define	UBPDN		0x20000		/* UNIBUS Power Down */
#define	UBIC		0x10000		/* UNIBUS Initialization */
					/* UNIBUS Ready */
#define	UBACOD		0xff		/* UBA Code bits */
 
/* UBA Control Register, UBACR */
 
#define	MRD16		0x40000000	/* Map Reg Disable Bit 4 */
#define	MRD8		0x20000000	/* Map Reg Disable Bit 3 */
#define	MRD4		0x10000000	/* Map Reg Disable Bit 2 */
#define	MRD2		0x8000000	/* Map Reg Disable Bit 1 */
#define	MRD1		0x4000000	/* Map Reg Disable Bit 0 */
#define	IFS		0x40	  	/* Interrupt Field Switch */
#define	BRIE		0x20	  	/* BR Interrupt Enable */
#define	USEFIE		0x10	  	/* UNIBUS to SBI Error Field IE */
#define	SUEFIE		0x8	  	/* SBI to UNIBUS Error Field IE */
#define	CNFIE		0x4	  	/* Configuration IE */
#define	UPF		0x2	  	/* UNIBUS Power Fail */
#define	ADINIT		0x1	  	/* Adapter Init */
 
/* UBA Status Register, UASR */
#define	BR7FULL		0x8000000	/* BR7 Receive Vector Rg Full */
#define	BR6FULL		0x4000000	/* BR6 Receive Vector Reg Full */
#define	BR5FULL		0x2000000	/* BR5 Receive Vector Reg Full */
#define	BR4FULL		0x1000000	/* BR4 Receive Vector Reg Full */
#define	RDTO		0x400		/* UNIBUS to SBI Read Data Timeout */
#define	RDS		0x200		/* Read Data Substitute */
#define	CRD		0x100		/* Corrected Read Data */
#define	CXTER		0x80		/* Command Transmit Error */
#define	CXTMO		0x40		/* Command Transmit Timeout */
#define	DPPE		0x20		/* Data Path Parity Error */
#define	IVMR		0x10		/* Invalid Map Register */
#define	MRPF		0x8		/* Map Register Parity Failure */
#define	LEB		0x4		/* Lost Error */
#define	UBSTO		0x2		/* UNIBUS Select Timeout */
#define	UBSSTO		0x1		/* UNIBUS Slave Sync Timeout */
 
/* Failed Map Entry Register, FMER */
 
#define	FMRN		0x1ff		/* Failed Map Reg. No. Field */
 
/* Failed UNIBUS Address Register, FUBAR */
#define	FUA		0xffff		/* Failed UNIBUS Address Field */
 
/* BR Receive Vector register, BRRVR */
#define	AIRI		0x80000000	/* Adapter Interrupt Request */
#define	DIV		0xffff		/* Device Interrupt Vector Field */
 
/* Data Path Register, DPR */
#define	BNE		0x80000000	/* Buffer Not Empty - Purge */
#define	BTE		0x40000000	/* Buffer Transfer Error */
#define	DPF		0x20000000	/* DP Function (RO) */
#define	BS		0x7f0000	/* Buffer State Field */
#define	BUBA		0xffff		/* Buffered UNIBUS Address */
 
/* Map Register, MR */
#define	MRV		0x80000000	/* Map Register Valid */
#define	BO		0x2000000		/* Byte Offset Bit */
#define	DPDB		0x1e00000		/* Data Path Designator Field */
#define	SBIPFN		0xfffff			/* SBI Page Address Field */

/*
 * Unibus maps
 */
#ifdef	KERNEL
#define	UAMSIZ 50

struct	map ubamap[UAMSIZ];
char	bdpwant;		/* someone is waiting for buffered data path */ 
struct	map bdpmap[15];
char	umrwant;		/* ... for unibus map registers */
#endif

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
};
