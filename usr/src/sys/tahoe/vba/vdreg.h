/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)vdreg.h	7.1 (Berkeley) %G%
 */

/*
 * Versabus VDDC/SMDE disk controller definitions.
 */
#define	VDDC_SECSIZE	512	/* sector size for VDDC */
#define	VD_MAXSECSIZE	1024	/* max sector size for SMD/E */

/*
 * Controller communications block.
 */
struct vddevice {
	u_long	vdcdr;		/* controller device register */
	u_long	vdreset;	/* controller reset register */
	u_long	vdcsr;		/* control-status register */
	long	vdrstclr;	/* reset clear register */
	u_short	vdstatus[16];	/* per-drive status register */
	u_short	vdicf_status;	/* status change interupt control format */
	u_short	vdicf_done;	/* interrupt complete control format */
	u_short	vdicf_error;	/* interrupt error control format */
	u_short	vdicf_success;	/* interrupt success control format */
	u_short	vdtcf_mdcb;	/* mdcb transfer control format */
	u_short	vdtcf_dcb;	/* dcb transfer control format */
	u_short	vdtcf_trail;	/* trail transfer control format */
	u_short	vdtcf_data;	/* data transfer control format */
	u_long	vdccf;		/* controller configuration flags */
	u_long	vdsecsize;	/* sector size */
	u_short	vdfill0;
	u_char	vdcylskew;	/* cylinder to cylinder skew factor */
	u_char	vdtrackskew;	/* track to track skew factor */
	u_long	vdfill1;
	u_long	vddfr;		/* diagnostic flag register */
	u_long	vddda;		/* diagnostic dump address */
};

/* controller types */
#define	VDTYPE_VDDC	1	/* old vddc controller (smd only) */
#define	VDTYPE_SMDE	2	/* new smde controller (smd-e) */

/*
 * Controller status definitions.
 */
#define	CS_SCS	0xf		/* status change source (drive number) */
#define	CS_ELC	0x10		/* error on last command */
#define	CS_ICC	0x60		/* interupt cause code */
#define   ICC_NOI  0x00		/* no interupt */
#define   ICC_DUN  0x20		/* no interupt */
#define   ICC_ERR  0x40		/* no interupt */
#define   ICC_SUC  0x60		/* no interupt */
#define	CS_GO	0x80		/* go bit (controller busy) */
#define	CS_BE	0x100		/* buss error */
#define	CS_BOK	0x4000		/* board ok */
#define	CS_SFL	0x8000		/* system fail */
#define	CS_LEC	0xff000000	/* last error code */

/*
 * Drive status definitions.
 */
#define	STA_UR	0x1		/* unit ready */
#define	STA_OC	0x2		/* on cylinder */
#define	STA_SE	0x4		/* seek error */
#define	STA_DF	0x8		/* drive fault */
#define	STA_WP	0x10		/* write protected */
#define	STA_US	0x20		/* unit selected */

/*
 * Interupt Control Field definitions.
 */
#define	ICF_IPL	0x7		/* interupt priority level */
#define	ICF_IEN	0x8		/* interupt enable */
#define	ICF_IV	0xff00		/* interupt vector */

/*
 * Transfer Control Format definitions.
 */
#define	TCF_AM	0xff		/* Address Modifier */
#define	  AM_SNPDA   0x01	/* Standard Non-Privileged Data Access */
#define	  AM_SASA    0x81	/* Standard Ascending Sequential Access */
#define	  AM_ENPDA   0xf1	/* Extended Non-Privileged Data Access */
#define	  AM_EASA    0xe1	/* Extended Ascending Sequential Access */
#define	TCF_BTE	0x800		/* Block Transfer Enable */

/*
 * Controller Configuration Flags.
 */
#define	CCF_STS	0x1		/* sectors per track selectable */
#define	CCF_EAV	0x2		/* enable auto vector */
#define	CCF_ERR	0x4		/* enable reset register */
#define CCF_DER 0x8		/* disable error recovery */
#define	CCF_XMD	0x60		/* xmd transfer mode (bus size) */
#define	  XMD_8BIT  0x20	/*   do only 8 bit transfers */
#define	  XMD_16BIT 0x40	/*   do only 16 bit transfers */
#define	  XMD_32BIT 0x60	/*   do only 32 bit transfers */
#define	CCF_BSZ	0x300		/* burst size */
#define	  BSZ_16WRD 0x000	/*   16 word transfer burst */
#define	  BSZ_12WRD 0x100	/*   12 word transfer burst */
#define	  BSZ_8WRD  0x200	/*   8 word transfer burst */
#define	  BSZ_4WRD  0x300	/*   4 word transfer burst */
#define CCF_SEN	0x400		/* cylinder/track skew enable (for format) */
#define	CCF_ENP	0x1000		/* enable parity */
#define	CCF_EPE	0x2000		/* enable parity errors */
#define	CCF_EDE	0x10000		/* error detection enable */
#define	CCF_ECE	0x20000		/* error correction enable */

/*
 * Diagnostic register definitions.
 */
#define	DIA_DC	0x7f		/* dump count mask */
#define	DIA_DWR	0x80		/* dump write/read flag */
#define	DIA_ARE	0x100		/* auto rebuild enable */
#define	DIA_CEN	0x200		/* call enable flag */
#define	DIA_KEY	0xAA550000	/* reset enable key */

/*
 * Hardware interface flags, in dcb.devselect and d_devflags
 */
#define VD_ESDI	0x10		/* drive is on ESDI interface */
#define	d_devflags	d_drivedata[0]		/* in disk label */

/*
 * Error recovery flags.
 */
#define	VDRF_RTZ	0x0001	/* return to zero */
#define	VDRF_OCF	0x0002	/* on cylinder false */
#define	VDRF_OSP	0x0004	/* offset plus */
#define	VDRF_OSM	0x0008	/* offset minus */
#define	VDRF_DSE	0x0080	/* data strobe early */
#define	VDRF_DSL	0x0100	/* data strobe late */

#define	VDRF_NONE	0
#define	VDRF_NORMAL	(VDRF_RTZ|VDRF_OCF|VDRF_OSP|VDRF_OSM|VDRF_DSE|VDRF_DSE)

/*
 * Perform a reset on the controller.
 */
#define	VDRESET(a,t) { \
	if ((t) == VDTYPE_SMDE) { \
		((struct vddevice *)(a))->vddfr = DIA_KEY|DIA_CEN; \
		((struct vddevice *)(a))->vdcdr = (u_long)0xffffffff; \
		DELAY(5000000); \
	} else { \
		((struct vddevice *)(a))->vdreset = 0; \
		DELAY(1500000); \
	} \
}

/*
 * Abort a controller operation.
 */
#define	VDABORT(a,t) { \
	if ((t) == VDTYPE_VDDC) { \
		movow((a), (VDOP_ABORT&0xffff0000)>>16) ; \
		movow((int)(a)+2, VDOP_ABORT&0xffff); \
	} else \
		((struct vddevice *)(a))->vdcdr = (u_long)VDOP_ABORT; \
	DELAY(1000000); \
}

/*
 * Start a command.
 */
#define VDGO(a,mdcb,t) {\
	if ((t) == VDTYPE_VDDC) { \
		movow((a), ((int)(mdcb)&0xffff0000)>>16) ; \
		movow((int)((a))+2, (int)(mdcb)&0xffff); \
	} else \
		((struct vddevice *)(a))->vdcdr = (mdcb); \
}

/*
 * MDCB layout.
 */
struct mdcb {
	struct	dcb *mdcb_head;		/* first dcb in list */
	struct	dcb *mdcb_busy;		/* dcb being processed */
	struct	dcb *mdcb_intr;		/* dcb causing interrupt */
	long	mdcb_status;		/* status of dcb in mdcb_busy */
};

/*
 * DCB definitions.
 */

/*
 * A disk address.
 */
typedef struct {
	u_char	track;			/* all 8 bits */
	u_char	sector;			/* all 8  bits */
	u_short	cylinder;		/* low order 12 bits */
} dskadr;

/*
 * DCB trailer formats.
 */
/* read/write trailer */
struct trrw {
	u_long	memadr;		/* memory address */
	u_long	wcount;		/* 16 bit word count */
	dskadr	disk;		/* disk address */
};

/* scatter/gather trailer */
#define	VDMAXPAGES	(MAXPHYS / NBPG)
struct trsg {
	struct	trrw start_addr;
	struct addr_chain {
		u_long	nxt_addr;
		u_long	nxt_len;
	} addr_chain[VDMAXPAGES + 1];
};

/* seek trailer format */
struct trseek {
	dskadr	skaddr;
};

/* format trailer */
struct trfmt {
	char	*addr;		/* data buffer to be filled on sector*/
	long	nsectors;	/* # of sectors to be formatted */
	dskadr	disk;		/* disk physical address info */
	dskadr  hdr;		/* header address info */
};

/* reset/configure trailer */
struct treset {
	long	ncyl;		/* # cylinders */
	long	nsurfaces;	/* # surfaces */
	long	nsectors;	/* # sectors */
	long	slip_sec;	/* # of slip sectors */
	long	recovery;	/* recovery flags */
};

/* ident trailer */
struct trid {
	long	name;
	long	id;
	long	date;
};

/*
 * DCB layout.
 */
struct dcb {
	struct	dcb *nxtdcb;	/* next dcb */
	short	intflg;		/* interrupt settings and flags */
	short	opcode;		/* DCB command code etc... */
	long	operrsta;	/* error & status info */
	short	fill;		/* not used */
	char	devselect;	/* drive selection */
	char	trailcnt;	/* trailer Word Count */
	long	err_memadr;	/* error memory address */
	char	err_code;	/* error codes for SMD/E */
	char	fill2;		/* not used */
	short	err_wcount;	/* error word count */
	char	err_trk;	/* error track/sector */
	char	err_sec;	/* error track/sector */
	short	err_cyl;	/* error cylinder adr */
	union {
		struct	trid idtrail;	/* ident command trailer */
		struct	trseek sktrail;	/* seek command trailer */
		struct	trsg sgtrail;	/* scatter/gather trailer */
		struct	trrw rwtrail;	/* read/write trailer */
		struct	trfmt fmtrail;	/* format trailer */
		struct	treset rstrail;	/* reset/configure trailer */
	} trail;
};

/*
 * smaller DCB with seek trailer only (no scatter-gather).
 */
struct skdcb {
	struct	dcb *nxtdcb;	/* next dcb */
	short	intflg;		/* interrupt settings and flags */
	short	opcode;		/* DCB command code etc... */
	long	operrsta;	/* error & status info */
	short	fill;		/* not used */
	char	devselect;	/* drive selection */
	char	trailcnt;	/* trailer Word Count */
	long	err_memadr;	/* error memory address */
	char	err_code;	/* error codes for SMD/E */
	char	fill2;		/* not used */
	short	err_wcount;	/* error word count */
	char	err_trk;	/* error track/sector */
	char	err_sec;	/* error track/sector */
	short	err_cyl;	/* error cylinder adr */
	union {
		struct	trseek sktrail;	/* seek command trailer */
	} trail;
};

/*
 * DCB command codes.
 */
#define	VDOP_RD		0x80		/* read data */
#define	VDOP_FTR	0xc0		/* full track read */
#define	VDOP_RAS	0x90		/* read and scatter */
#define	VDOP_RDRAW	0x600		/* read unformatted disk sector */
#define	VDOP_CMP	0xa0		/* compare */
#define	VDOP_FTC	0xe0		/* full track compare */
#define	VDOP_RHDE	0x180		/* read header, data & ecc */
#define	VDOP_WD		0x00		/* write data */
#define	VDOP_FTW	0x40		/* full track write */
#define	VDOP_WTC	0x20		/* write then compare */
#define	VDOP_FTWTC	0x60		/* full track write then compare */
#define	VDOP_GAW	0x10		/* gather and write */
#define	VDOP_WDE	0x100		/* write data & ecc */
#define	VDOP_FSECT	0x900		/* format sector */
#define	VDOP_GWC	0x30		/* gather write & compare */
#define	VDOP_START	0x800		/* start drives */
#define	VDOP_RELEASE	0xa00		/* stop drives */
#define	VDOP_SEEK	0xb00		/* seek */
#define	VDOP_INIT	0xc00		/* initialize controller */
#define	VDOP_DIAG	0xd00		/* diagnose (self-test) controller */
#define	VDOP_CONFIG	0xe00		/* reset & configure drive */
#define	VDOP_STATUS	0xf00		/* get drive status */
#define	VDOP_IDENT	0x700		/* identify controller */

#define	VDOP_ABORT	0x80000000	/* abort current command */

/*
 * DCB status definitions.
 */
#define	DCBS_HCRC	0x00000001	/* header crc error */
#define	DCBS_HCE	0x00000002	/* header compare error */
#define	DCBS_WPT	0x00000004	/* drive write protected */
#define	DCBS_CHE	0x00000008	/* controller hardware error */
#define	DCBS_SKI	0x00000010	/* seek incomplete */
#define	DCBS_UDE	0x00000020	/* uncorrectable data error */
#define	DCBS_OCYL	0x00000040	/* off cylinder */
#define	DCBS_NRDY	0x00000080	/* drive not ready */
#define	DCBS_ATA	0x00000100	/* alternate track accessed */
#define	DCBS_SKS	0x00000200	/* seek started */
#define	DCBS_IVA	0x00000400	/* invalid disk address error */
#define	DCBS_NEM	0x00000800	/* non-existant memory error */
#define	DCBS_DPE	0x00001000	/* memory data parity error */
#define	DCBS_DCE	0x00002000	/* data compare error */
#define	DCBS_DDI	0x00004000	/* ddi ready */
#define	DCBS_OAB	0x00008000	/* operation aborted */
#define	DCBS_DSE	0x00010000	/* data strobe early */
#define	DCBS_DSL	0x00020000	/* data strobe late */
#define	DCBS_TOP	0x00040000	/* track offset plus */
#define	DCBS_TOM	0x00080000	/* track offset minus */
#define	DCBS_CCD	0x00100000	/* controller corrected data */
#define	DCBS_HARD	0x00200000	/* hard error */
#define	DCBS_SOFT	0x00400000	/* soft error (retry succesful) */
#define	DCBS_ERR	0x00800000	/* composite error */
#define DCBS_IVC	0x01000000	/* invalid command error */
/* bits 24-27 unused */
#define	DCBS_BSY	0x10000000	/* controller busy */
#define	DCBS_ICC	0x60000000	/* interrupt cause code */
#define	DCBS_INT	0x80000000	/* interrupt generated for this dcb */

#define	VDERRBITS	"\20\1HCRC\2HCE\3WPT\4CHE\5DSKI\6UDE\7OCYL\10NRDY\
\11ATA\12SKS\13IVA\14NEM\15DPE\16DCE\17DDI\20OAB\21DSE\22DSL\23TOP\24TOM\
\25CCD\26HARD\27SOFT\30ERR\31IVC\35ABORTED\36FAIL\37COMPLETE\40STARTED"

/* drive related errors */
#define	VDERR_DRIVE	(DCBS_SKI|DCBS_OCYL|DCBS_NRDY|DCBS_IVA)
/* controller related errors */
#define	VDERR_CTLR	(DCBS_CHE|DCBS_OAB|DCBS_IVC|DCBS_NEM)
/* potentially recoverable errors */
#define	VDERR_RETRY \
    (VDERR_DRIVE|VDERR_CTLR|DCBS_DCE|DCBS_DPE|DCBS_HCRC|DCBS_HCE)
/* uncorrected data errors */
#define	VDERR_HARD	(VDERR_RETRY|DCBS_WPT|DCBS_UDE)

/*
 * DCB status codes.
 */
#define	DCBS_ABORT	0x10000000	/* dcb aborted */
#define	DCBS_FAIL	0x20000000	/* dcb unsuccesfully completed */
#define	DCBS_DONE	0x40000000	/* dcb complete */
#define	DCBS_START	0x80000000	/* dcb started */

/*
 * DCB interrupt control.
 */
#define	DCBINT_NONE	0x0		/* don't interrupt */
#define	DCBINT_ERR	0x2		/* interrupt on error */
#define	DCBINT_SUC	0x1		/* interrupt on success */
#define	DCBINT_DONE	(DCBINT_ERR|DCBINT_SUC)
#define	DCBINT_PBA	0x4		/* proceed before acknowledge */

/*
 * Sector formats.
 */
typedef union {
	struct {
		dskadr	hdr_addr;
		short	smd_crc;
	} smd;
	struct {
		dskadr	physical;
		dskadr	logical;
		long	smd_e_crc;
	} smd_e;
} fmt_hdr;

/* Sector Header bit assignments */
#define	VDMF	0x8000		/* Manufacturer Fault 1=good sector */
#define	VDUF	0x4000		/* User Fault 1=good sector */
#define	VDALT	0x2000		/* Alternate Sector 1=alternate */
#define	VDWPT	0x1000		/* Write Protect 1=Read Only Sector */

/* input register assignments for DIOCWFORMAT ioctl */
#define	dk_op		df_reg[0]	/* opcode */
#define	dk_althdr	df_reg[1]	/* alt. sect. header, in an int! */
#define	dk_fmtflags	df_reg[2]	/* header format flags */

/* output register assignments for DIOCWFORMAT ioctl */
#define	dk_operrsta	df_reg[0]	/* dcb operrsta */
#define	dk_ecode	df_reg[1]	/* smd-e err_code */
