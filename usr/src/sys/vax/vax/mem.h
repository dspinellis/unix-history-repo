/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mem.h	7.1 (Berkeley) %G%
 */

/*
 * Memory controller registers
 *
 * The way in which the data is stored in these registers varies
 * per controller and cpu, so we define macros here to mask that.
 */
struct	mcr {
	int	mc_reg[6];
};

/*
 * Compute maximum possible number of memory controllers,
 * for sizing of the mcraddr array.
 */
#if VAX780
#define	MAXNMCR		4
#else
#define	MAXNMCR		1
#endif

/*
 * For each controller type we define 5 macros:
 *	M???_INH(mcr)		inhibits further crd interrupts from mcr
 *	M???_ENA(mcr)		enables another crd interrupt from mcr
 *	M???_ERR(mcr)		tells whether an error is waiting
 *	M???_SYN(mcr)		gives the syndrome bits of the error
 *	M???_ADDR(mcr)		gives the address of the error
 */

#if VAX8600
/*
 * 8600 register bit definitions
 */
#define	M8600_ICRD	0x400		/* inhibit crd interrupts */
#define M8600_TB_ERR	0xf00		/* translation buffer error mask */
/*
 * MDECC register
 */
#define	M8600_ADDR_PE	0x080000	/* address parity error */
#define M8600_DBL_ERR	0x100000	/* data double bit error */
#define	M8600_SNG_ERR	0x200000	/* data single bit error */
#define	M8600_BDT_ERR	0x400000	/* bad data error */

/*
 * ESPA register is used to address scratch pad registers in the Ebox.
 * To access a register in the scratch pad, write the ESPA with the address
 * and then read the ESPD register.  
 *
 * NOTE:  In assmebly code, the the mfpr instruction that reads the ESPD
 *	  register must immedately follow the mtpr instruction that setup
 *	  the ESPA register -- per the VENUS processor register spec.
 *
 * The scratchpad registers that are supplied for a single bit ECC 
 * error are:
 */
#define	SPAD_MSTAT1	0x25		/* scratch pad mstat1 register	*/
#define SPAD_MSTAT2	0x26		/* scratch pad mstat2 register	*/
#define SPAD_MDECC	0x27		/* scratch pad mdecc register	*/
#define SPAD_MEAR	0x2a		/* scratch pad mear register	*/

#define M8600_MEMERR(mdecc) ((mdecc) & 0x780000)
#define M8600_HRDERR(mdecc) ((mdecc) & 0x580000)
#define M8600_ENA (mtpr(MERG, (mfpr(MERG) & ~M8600_ICRD)))
#define M8600_INH (mtpr(EHSR, 0), mtpr(MERG, (mfpr(MERG) | M8600_ICRD)))
#define M8600_SYN(mdecc) (((mdecc) >> 9) & 0x3f)
#define M8600_ADDR(mear) ((mear) & 0x3ffffffc)
#define M8600_ARRAY(mear) (((mear) >> 22) & 0x0f)

#define M8600_MDECC_BITS "\20\27BAD_DT_ERR\26SNG_BIT_ERR\25DBL_BIT_ERR\
\24ADDR_PE"
#define M8600_MSTAT1_BITS "\20\30CPR_PE_A\27CPR_PE_B\26ABUS_DT_PE\
\25ABUS_CTL_MSK_PE\24ABUS_ADR_PE\23ABUS_C/A_CYCLE\22ABUS_ADP_1\21ABUS_ADP_0\
\20TB_MISS\17BLK_HIT\16C0_TAG_MISS\15CHE_MISS\14TB_VAL_ERR\13TB_PTE_B_PE\
\12TB_PTE_A_PE\11TB_TAG_PE\10WR_DT_PE_B3\7WR_DT_PE_B2\6WR_DT_PE_B1\
\5WR_DT_PE_B0\4CHE_RD_DT_PE\3CHE_SEL\2ANY_REFL\1CP_BW_CHE_DT_PE"
#define M8600_MSTAT2_BITS "\20\20CP_BYT_WR\17ABUS_BD_DT_CODE\10MULT_ERR\
\7CHE_TAG_PE\6CHE_TAG_W_PE\5CHE_WRTN_BIT\4NXM\3CP-IO_BUF_ERR\2MBOX_LOCK"
#endif VAX8600

#if VAX780
#define	M780_ICRD	0x40000000	/* inhibit crd interrupts, in [2] */
#define	M780_HIER	0x20000000	/* high error rate, in reg[2] */
#define	M780_ERLOG	0x10000000	/* error log request, in reg[2] */
/* on a 780, memory crd's occur only when bit 15 is set in the SBIER */
/* register; bit 14 there is an error bit which we also clear */
/* these bits are in the back of the ``red book'' (or in the VMS code) */

#define	M780C_INH(mcr)	\
	(((mcr)->mc_reg[2] = (M780_ICRD|M780_HIER|M780_ERLOG)), mtpr(SBIER, 0))
#define	M780C_ENA(mcr)	\
	(((mcr)->mc_reg[2] = (M780_HIER|M780_ERLOG)), mtpr(SBIER, 3<<14))
#define	M780C_ERR(mcr)	\
	((mcr)->mc_reg[2] & (M780_ERLOG))

#define	M780C_SYN(mcr)	((mcr)->mc_reg[2] & 0xff)
#define	M780C_ADDR(mcr)	(((mcr)->mc_reg[2] >> 8) & 0xfffff)

#define	M780EL_INH(mcr)	\
	(((mcr)->mc_reg[2] = (M780_ICRD|M780_HIER|M780_ERLOG)), mtpr(SBIER, 0))
#define	M780EL_ENA(mcr)	\
	(((mcr)->mc_reg[2] = (M780_HIER|M780_ERLOG)), mtpr(SBIER, 3<<14))
#define	M780EL_ERR(mcr)	\
	((mcr)->mc_reg[2] & (M780_ERLOG))

#define	M780EL_SYN(mcr)	((mcr)->mc_reg[2] & 0x7f)
#define	M780EL_ADDR(mcr)	(((mcr)->mc_reg[2] >> 11) & 0x1ffff)

#define	M780EU_INH(mcr)	\
	(((mcr)->mc_reg[3] = (M780_ICRD|M780_HIER|M780_ERLOG)), mtpr(SBIER, 0))
#define	M780EU_ENA(mcr)	\
	(((mcr)->mc_reg[3] = (M780_HIER|M780_ERLOG)), mtpr(SBIER, 3<<14))
#define	M780EU_ERR(mcr)	\
	((mcr)->mc_reg[3] & (M780_ERLOG))

#define	M780EU_SYN(mcr)	((mcr)->mc_reg[3] & 0x7f)
#define	M780EU_ADDR(mcr)	(((mcr)->mc_reg[3] >> 11) & 0x1ffff)
#endif

#if VAX750
#define	M750_ICRD	0x10000000	/* inhibit crd interrupts, in [1] */
#define	M750_UNCORR	0xc0000000	/* uncorrectable error, in [0] */
#define	M750_CORERR	0x20000000	/* correctable error, in [0] */

#define	M750_INH(mcr)	((mcr)->mc_reg[1] = 0)
#define	M750_ENA(mcr)	((mcr)->mc_reg[0] = (M750_UNCORR|M750_CORERR), \
			    (mcr)->mc_reg[1] = M750_ICRD)
#define	M750_ERR(mcr)	((mcr)->mc_reg[0] & (M750_UNCORR|M750_CORERR))

#define	M750_SYN(mcr)	((mcr)->mc_reg[0] & 0x7f)
#define	M750_ADDR(mcr)	(((mcr)->mc_reg[0] >> 9) & 0x7fff)
#endif

#if VAX730
#define	M730_UNCORR	0x80000000	/* rds, uncorrectable error, in [1] */
#define	M730_CRD	0x40000000	/* crd, in [1] */
#define	M730_FTBPE	0x20000000	/* force tbuf parity error, in [1] */
#define	M730_ENACRD	0x10000000	/* enable crd interrupt, in [1] */
#define	M730_MME	0x08000000	/* mem-man enable (ala ipr), in [1] */
#define	M730_DM		0x04000000	/* diagnostic mode, in [1] */
#define	M730_DISECC	0x02000000	/* disable ecc, in [1] */

#define	M730_INH(mcr)	((mcr)->mc_reg[1] = M730_MME)
#define	M730_ENA(mcr)	((mcr)->mc_reg[1] = (M730_MME|M730_ENACRD))
#define	M730_ERR(mcr)	((mcr)->mc_reg[1] & (M730_UNCORR|M730_CRD))
#define	M730_SYN(mcr)	((mcr)->mc_reg[0] & 0x7f)
#define	M730_ADDR(mcr)	(((mcr)->mc_reg[0] >> 8) & 0x7fff)
#endif

/* controller types */
#define	M780C	1
#define	M780EL	2
#define	M780EU	3
#define	M750	4
#define	M730	5

#define	MEMINTVL	(60*10)		/* 10 minutes */

#ifdef	KERNEL
int	nmcr;
struct	mcr *mcraddr[MAXNMCR];
int	mcrtype[MAXNMCR];
#endif
