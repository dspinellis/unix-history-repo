/*	ubareg.h	4.30	82/05/26	*/

/*
 * VAX UNIBUS adapter registers
 */
#ifndef LOCORE
/*
 * UBA hardware registers
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
#endif

#if VAX780
/* uba_cnfgr */
#define	UBACNFGR_UBINIT	0x00040000	/* unibus init asserted */
#define	UBACNFGR_UBPDN	0x00020000	/* unibus power down */
#define	UBACNFGR_UBIC	0x00010000	/* unibus init complete */

/* uba_cr */
#define	UBACR_MRD16	0x40000000	/* map reg disable bit 4 */
#define	UBACR_MRD8	0x20000000	/* map reg disable bit 3 */
#define	UBACR_MRD4	0x10000000	/* map reg disable bit 2 */
#define	UBACR_MRD2	0x08000000	/* map reg disable bit 1 */
#define	UBACR_MRD1	0x04000000	/* map reg disable bit 0 */
#define	UBACR_IFS	0x00000040	/* interrupt field switch */
#define	UBACR_BRIE	0x00000020	/* BR interrupt enable */
#define	UBACR_USEFIE	0x00000010	/* UNIBUS to SBI error field IE */
#define	UBACR_SUEFIE	0x00000008	/* SBI to UNIBUS error field IE */
#define	UBACR_CNFIE	0x00000004	/* configuration IE */
#define	UBACR_UPF	0x00000002	/* UNIBUS power fail */
#define	UBACR_ADINIT	0x00000001	/* adapter init */

/* uba_sr */
#define	UBASR_BR7FULL	0x08000000	/* BR7 receive vector reg full */
#define	UBASR_BR6FULL	0x04000000	/* BR6 receive vector reg full */
#define	UBASR_BR5FULL	0x02000000	/* BR5 receive vector reg full */
#define	UBASR_BR4FULL	0x01000000	/* BR4 receive vector reg full */
#define	UBASR_RDTO	0x00000400	/* UNIBUS to SBI read data timeout */
#define	UBASR_RDS	0x00000200	/* read data substitute */
#define	UBASR_CRD	0x00000100	/* corrected read data */
#define	UBASR_CXTER	0x00000080	/* command transmit error */
#define	UBASR_CXTMO	0x00000040	/* command transmit timeout */
#define	UBASR_DPPE	0x00000020	/* data path parity error */
#define	UBASR_IVMR	0x00000010	/* invalid map register */
#define	UBASR_MRPF	0x00000008	/* map register parity failure */
#define	UBASR_LEB	0x00000004	/* lost error */
#define	UBASR_UBSTO	0x00000002	/* UNIBUS select timeout */
#define	UBASR_UBSSYNTO	0x00000001	/* UNIBUS slave sync timeout */

#define	UBASR_BITS \
"\20\13RDTO\12RDS\11CRD\10CXTER\7CXTMO\6DPPE\5IVMR\4MRPF\3LEB\2UBSTO\1UBSSYNTO"

/* uba_brrvr[] */
#define	UBABRRVR_AIRI	0x80000000	/* adapter interrupt request */
#define	UBABRRVR_DIV	0x0000ffff	/* device interrupt vector field */
#endif VAX780
 
/* uba_dpr */
#if VAX780
#define	UBADPR_BNE	0x80000000	/* buffer not empty - purge */
#define	UBADPR_BTE	0x40000000	/* buffer transfer error */
#define	UBADPR_DPF	0x20000000	/* DP function (RO) */
#define	UBADPR_BS	0x007f0000	/* buffer state field */
#define	UBADPR_BUBA	0x0000ffff	/* buffered UNIBUS address */
#define	UBA_PURGE780(uba, bdp) \
    ((uba)->uba_dpr[bdp] |= UBADPR_BNE)
#endif VAX780
#if VAX750
#define	UBADPR_ERROR	0x80000000	/* error occurred */
#define	UBADPR_NXM	0x40000000	/* nxm from memory */
#define	UBADPR_UCE	0x20000000	/* uncorrectable error */
#define	UBADPR_PURGE	0x00000001	/* purge bdp */
#define	UBA_PURGE750(uba, bdp) \
    ((uba)->uba_dpr[bdp] |= (UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE))
#endif VAX750
 
/*
 * Macros for fast buffered data path purging in time-critical routines.
 *
 * Too bad C pre-processor doesn't have the power of LISP in macro
 * expansion...
 */
#if defined(VAX780) && defined(VAX750)
#define	UBAPURGE(uba, bdp) { \
	switch (cpu) { \
	case VAX_780: UBA_PURGE780((uba), (bdp)); break; \
	case VAX_750: UBA_PURGE750((uba), (bdp)); break; \
	} \
}
#endif
#if defined(VAX780) && !defined(VAX750)
#define	UBAPURGE(uba, bdp) { \
	if (cpu==VAX_780) { \
		UBA_PURGE780((uba), (bdp)); \
	} \
}
#endif
#if !defined(VAX780) && defined(VAX750)
#define	UBAPURGE(uba, bdp) { \
	if (cpu==VAX_750) { \
		UBA_PURGE750((uba), (bdp)); break; \
	} \
}
#endif
#if !defined(VAX780) && !defined(VAX750)
#define	IF_UBAPURGE(uba, bdp)
#endif

/* uba_mr[] */
#define	UBAMR_MRV	0x80000000	/* map register valid */
#define	UBAMR_BO	0x02000000	/* byte offset bit */
#define	UBAMR_DPDB	0x01e00000	/* data path designator field */
#define	UBAMR_SBIPFN	0x000fffff	/* SBI page address field */

#define	UBAMR_DPSHIFT	21		/* shift to data path designator */

/*
 * Number of UNIBUS map registers.  We can't use the last 8k of UNIBUS
 * address space for i/o transfers since it is used by the devices,
 * hence have slightly less than 256K of UNIBUS address space.
 */
#define	NUBMREG	496

/*
 * Number of unibus buffered data paths and possible uba's per cpu type.
 */
#define	NBDP780	15
#define	NBDP750	3
#define	NBDP730	0
#define	MAXNBDP	15

#define	NUBA780	4
#define	NUBA750	1
#define	NUBA730	1
#if VAX780
#define	MAXNUBA	4
#else
#define	MAXNUBA	1
#endif

/*
 * Symbolic addresses of UNIBUS memory for UBAs.
 */
#if VAX730
#define	UMEM730		((u_short *)(0xfc0000))
#endif
#if VAX750
#define	UMEM750(i)	((u_short *)(0xfc0000-(i)*0x40000))
#endif
#if VAX780
#define	UMEM780(i)	((u_short *)(0x20100000+(i)*0x40000))
#endif

/*
 * Macro to offset a UNIBUS device address, often expressed as
 * something like 0172520 by forcing it into the last 8K of UNIBUS space.
 */
#define	ubdevreg(addr)	(0760000|((addr)&017777))
