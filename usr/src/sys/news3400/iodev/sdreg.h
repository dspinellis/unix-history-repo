/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: sdreg.h,v 4.300 91/06/09 06:38:28 root Rel41 $ SONY
 *
 *	@(#)sdreg.h	7.2 (Berkeley) %G%
 */

/*
 * Copyright (c) 1989- by SONY Corporation.
 */
/*
 *	sdreg.h
 */

#ifndef __SDREG__
#define __SDREG__ 1

#include <machine/fix_machine_type.h>

#define	MAXSLAVE	8
#define	PNUM		8
#define	RSEN_CNT	32

/******************************************************/
/*
 *	software control structure / controller
 */
struct sdc_softc {
/*00*/	int	sdc_wticks;
/*04*/	int	sdc_state;
#define SDCS_NORMAL	0x0
#define SDCS_RSENSE	0x1
#define SDCS_ECC	0x2
#define SDCS_RASREAD	0x4
#define SDCS_RASBLK	0x8
#define SDCS_RASWRITE	0x10
#define SDCS_RETRY	0x20
#define SDCS_PREVRMB	0x40
#define SDCS_LOSTDATA	0x1000
#define SDCS_ECC_HOLD	0x100000
#define SDCS_ECCOFF	0x200000
#define SDCS_REZERO	0x400000
#define	SDCS_OPEN_WAIT	0x20000000
#define SDCS_SCUNLOCK	0x40000000
#define SDCS_IOCTL	0x80000000
/*08*/	int	sdc_nhrderr;
/*0c*/	int	sdc_retrycnt;
/*10*/	int	sdc_softas;
/*14*/	int	sdc_timeo;
/*18*/	int	sdc_countcc;
/*1c*/	int	sdc_firmware;
#define	SDCFW_DEFMODE	0x1
#define	SDCFW_CACHE	0x2
#define	SDCFW_AWRE	0x4
#define	SDCFW_ARRE	0x8
#define	SDCFW_OLDWREN3	0x10
#define	SDCFW_WANTED	0x20
#define	SDCFW_BUSY	0x40
#define	SDCFW_RMB	0x80
#define	SDCFW_CDBUGS	0x100
#define	SDCFW_CONTR	0x200
#define	SDCFW_DEVMASK	0xf0000000
#define	SDCFW_HD	0x10000000
#define	SDCFW_MO	0x20000000
#define	SDCFW_CD	0x40000000
/*20*/
};

/*
 *	software control structure / drive
 */
struct sdd_softc {
/*00*/	int	sdd_sectsize;
/*04*/	int	sdd_nsect;
/*08*/	int	sdd_badsect;
/*0c*/	int	sdd_flags;
#define	SDDF_DSKCHGD	0x0
#define	SDDF_SAMEDSK	0x1
#define	SDDF_WPROTECT	0x2
#define	SDDF_XUSE	0x4
#define	SDDF_NONFMT	0x8
#define	SDDF_FMTDONE	0x10
#define	SDDF_REQ_EJECT	0x20
#define	SDDF_ERASEOFF	0x40
#define SDDF_VBADSECT	0x80
#define SDDF_SKIPCHECK	0x100
#define	SDDF_SYNCTR	0x200
#define	SDDF_INHRMV	0x400
#define	SDDF_GETTMP	0x800
#define	SDDF_AUDIO_START	0x80000000
/*10*/	int	sdd_start;		/* for removable disk */
/*14*/	int	sdd_stoptime;		/* for removable disk */
	daddr_t	sdd_lastblk;
};

struct sdst {
/*00*/	short	nsect;			/* # sectors / track */
/*02*/	short	ntrak;			/* # tracks / cylinder */
/*04*/	short	nspc;			/* # sectors / cylinder */
/*06*/	short	ncyl;			/* # cylinders */
/*08*/	short	rps;			/* # revolutions / second */
/*0a*/	short	xxxx;			/* reserved */
/*0c*/	struct	size *sizes;		/* partition table */
/*10*/
};

struct size {
/*00*/	daddr_t	sd_nblocks;
/*04*/	daddr_t	sd_blkoff;
/*08*/
};

/*
 * Device configuration table.
 */
struct sddevinfo {
/*00*/	int		id_len;		/* identify length */
/*04*/	char		*id_name;	/* vendor & product ID */
/*08*/	char		*revs;		/* revision */
/*0c*/	int		capacity;	/* drive capacity */
/*10*/	char		*call_name;	/* device nickname */
/*14*/	int		type;		/* device type code */
#define		UNKNOWN_DISK	0
/**/
#define		WREN3_101	110
#define		WREN3_182	111
#define		WREN3_101_HH	112
#define		WREN4_286	120
#define		WREN5_170_HH	130
#define		WREN6_415_HH	141
#define		ST1480N		171
/**/
#define		DK515C_78	210
#define		DK516C_16	211
#define		DK312C_25	220
#define		DK312C_20	221
/**/
#define		SRD_2040Z	310
#define		SMO_S501	330	/* format mode 2 */
#define		SMO_S501_ISO	331	/* format mode 3 (spare band 1024) */
#define		SMO_S501_ISO2	332	/* format mode 3 (spare band 2048) */
/**/
#define		XT_8760S	810
/**/
/*18*/	int		firm_flags;	/* firmware spec */
# define	FIRM_CACHE_ON	0x01
# define	FIRM_CACHE_OFF	0x02
# define	FIRM_SYNCTR	0x04
# define	FIRM_AWRE	0x08
# define	FIRM_ARRE	0x10
/*1c*/	int		tr_period;	/* synchronous transfer period */
/*20*/	int		tr_offset;	/* synchronous transfer offset */
/*24*/	struct sc_ureq	**setup_cmds;	/* device setup commands */
/*28*/	struct sdst	*sdstp;		/* device specifications */
#define calc_disk_sizes	NULL
/*2c*/	char		*ERP_page;	/* normal Error Recovery Parameters */
/*30*/	char		*max_ERP_page;	/* max Error Recovery Parameters */
/*34*/	char		**other_pages;	/* Other Parameters */
/*38*/	int		fmt_opts;	/* Format options */
# define	FMT_DLFMT	0x07	/* Defect list format */
# define	FMT_DLF_BLK	0x00		/* block format */
# define	FMT_DLF_IDX	0x04		/* bytes from index format */
# define	FMT_DLF_PHYS	0x05		/* physical sector format */
# define	FMT_DCRT	0x20	/* Disable Certification */
/*3c*/
};

#ifdef news1800
# define	SYNC_TR_OFS	4
#else
# define	SYNC_TR_OFS	0
#endif

/*********************************************************/

/*
 *	MODE SELECT/SENSE bit assign
 */
#define	SDM_PG_NULL	0x00	/* product unique not used */
#define	SDM_PG_ERR	0x01	/* error recovery parameters */
#define	SDM_PG_CNCT	0x02	/* disconnect/reconnect control parameters */
#define	SDM_PG_FMT1	0x03	/* direct access device format parameters */
#define	SDM_PG_GEOM	0x04	/* rigid disk drive geometry parameters */

#define	SDM_PG_CACHE1	0x08	/* cache control parameters */

#define	SDM_PG_CACHE2	0x38	/* cache control parameters */

#define	SDM_PG_FMT2	0x20	/* format parameters */

#define	SDM_PG_MODIFY	0x20	/* modification parameters */

#define	SDM_PAGE	0x00	/* page cache mode */
#define	SDM_RING	0x10	/* ring cache mode */
#define	SDM_RCD		0x01	/* Read Cache Disable */

#define	SDM_STOD	0x08	/* SCSI Time Out Delay*/
#define	SDM_ESDT	0x04	/* enable SCSI data timeout */

#define	SDM_PF		0x10	/* page format */

#define	SDM_PC_CUR	0x00	/* page control field (current value) */
#define	SDM_PC_CHG	0x40	/* page control field (changeable value) */
#define	SDM_PC_DEF	0x80	/* page control field (default value) */
#define	SDM_PC_SAVE	0xc0	/* page control field (saved value) */

#define	SDM_PCODE_ER	SDM_PG_ERR
#define	SDM_PCODE_CON	SDM_PG_CNCT
#define	SDM_PCODE_FMT	SDM_PG_FMT2
#define	SDM_PCODE_ALL	0x3f

#define	SDM_DCR		0x01	/* Disable Correction */
#define	SDM_DTE		0x02	/* Disable Transfer on Error */
#define	SDM_PER		0x04	/* Post Error */
#define	SDM_EEC		0x08	/* Enable Early Correction */
#define	SDM_RC		0x10	/* Read Continuous */
#define	SDM_TB		0x20	/* Transfer Block */
#define	SDM_ARRE	0x40	/* automatic read reallocate enable */
#define	SDM_AWRE	0x80	/* automatic write reallocate enable */

/*
 *	READ DEFECT LIST bit assign
 */
#define	SDDL_PLIST	0x10	/* primary defect list */
#define	SDDL_GLIST	0x08	/* grown defect list  */
#define	SDDL_PHYSFMT	0x05	/* physical sector format */

/*
 *	FORMAT UNIT bit assign
 */
#define	SDF_FMTDAT	0x10	/* format data */
#define	SDF_CMPLST	0x08	/* complete list */

#define	SDF_BLKFMT	0x10	/* block format */
#define	SDF_PHYSFMT	0x15	/* physical sector format */
#define	SDF_MKCDA	0x02	/* make controll data area */
#define	SDF_MKPLST	0x01	/* make P list */

/*
 *	PREVENT/ALLOW MEDIUM REMOVAL bit assign
 */
#define	SDRMV_PREV	0x01	/* prevent medium removal */
#define	SDRMV_ALLOW	0x00	/* allow medium removal */

/*
 *	START/STOP UNIT bit assign
 */
#define	SDSS_STOP	0x00	/* stop unit */
#define	SDSS_START	0x01	/* start unit */
#define	SDSS_EJECT	0x02	/* eject */

/*********************************************************/
/*
 *	IOCTL DEFINES
 */
#define	SDIOC_PRVRMV	_IO('D', 0)		/* prevent medium removal */
#define	SDIOC_ALWRMV	_IO('D', 1)		/* allow medium removal */
#define	SDIOC_FORMAT	_IOWR('D', 2, struct fmt_data)	/* format unit */
struct fmt_data {
/*00*/	int	noglist;			/* ignore G-list flag */
/*04*/	u_char	*dlh;				/* defect list header */
/*08*/
};

#define	SDIOC_GSTOPT	_IOR('D', 3, int)		/* get stop time */
#define	SDIOC_SSTOPT	_IOW('D', 4, int)		/* set stop time */

#define	SDIOC_SEJECT	_IO('D', 5)		/* set medium eject request */

#define	SDIOC_SXUSE	_IO('D', 6)		/* set exclusive use */
#define	SDIOC_RXUSE	_IO('D', 7)		/* reset exclusive use */
#define	SDIOC_ERSOFF	_IO('D', 8)		/* no erase before write */
#define	SDIOC_ERSON	_IO('D', 9)		/* erase before write */

#define	SDIOC_INQUIRY	_IOR('D', 10, struct sc_inq)
#define	SDIOC_RCAP	_IOR('D', 11, int)

#define	SDIOC_ECODE	_IOW('D', 14, int)	/* extend ecode on/off */

#define	SDIOC_GFLAGS	_IOR('D', 15, int)	/* get ii->ii_flags */
#define	SDIOC_SFLAGS	_IOW('D', 16, int)	/* set ii->ii_flags */

/*
 * ii->ii_flags bits assign
 */
#define SD_F_ENW	0x01			/* enable write open */
#define SD_F_ECODE	0x02			/* enable extend code */
#define	SD_F_WRTVRFY	0x04			/* execute write & verify */
#define	SD_F_IGNLABEL	0x08			/* ignore disklabel/diskinfo */
#define	SD_F_EOLBA	0x10			/* error if outside LBA */

#define	SDIOC_RASBLK	_IOW('D', 17, int)	/* reassign block */
#define	SDIOC_GNICKNAME	_IOW('D', 18, char *)	/* get device nickname */
#define	SDIOC_GTYPINDEX	_IOR('D', 19, int)	/* get ii->ii_type */

#define	SDIOC_SSYNCPARAM	_IOW('D', 20, struct sync_param)
						/* set sync tr param */
#define	SDIOC_GSYNCPARAM	_IOR('D', 21, struct sync_param)
						/* get sync tr param */
struct sync_param {
	int	tr_period;
	int	tr_offset;
};

#define	SDIOC_FORMAT2	_IOWR('D', 22, struct sc_ureq)	/* format unit */

#endif /* !__SDREG__ */
