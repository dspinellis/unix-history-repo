/*-
 * Copyright (c) 1982, 1988, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)saio.h	7.19 (Berkeley) %G%
 */

#include <sys/time.h>

#include <ufs/ffs/fs.h>
#include <ufs/ufs/dinode.h>

#include <stand/saioctl.h>
#include <stand/saerrno.h>

#define	UNIX	"/vmunix"

#ifndef NULL
#define	NULL	0
#endif

/* I/O block */
struct iob {
	int	i_flgs;		/* flags (see F_*) */
	int	i_adapt;	/* adapter or bus */
	int	i_ctlr;		/* controller */
	int	i_unit;		/* pseudo device unit */
	int	i_part;		/* disk partition */
	daddr_t	i_boff;		/* block offset on device */
	daddr_t	i_cyloff;	/* cylinder offset on device */
	off_t	i_offset;	/* seek offset in file */
	dev_t	i_dev;		/* associated device */
	daddr_t	i_bn;		/* 1st block # of next read */
	char	*i_ma;		/* memory address of I/O buffer */
	int	i_cc;		/* character count of transfer */
	int	i_error;	/* error # return */
	int	i_errcnt;	/* error count for driver retries */
	int	i_errblk;	/* block # in error for error reporting */
	char	i_buf[MAXBSIZE];/* I/O buffer */
	struct	dinode i_ino;	/* dinode, if file */
	union {			/* file system super block info */
		struct fs ui_fs;
		char dummy[SBSIZE];
	} i_un;
#define	i_fs	i_un.ui_fs
#define	i_bus	i_adapt
};

#define	SOPEN_MAX	4
extern struct iob iob[SOPEN_MAX];

/* Codes for sector header word 1 */
#define	HDR1_FMT22	0x1000	/* standard 16 bit format */
#define	HDR1_OKSCT	0xc000	/* sector ok */
#define	HDR1_SSF	0x2000	/* skip sector flag */

/* I/O flag values */
#define	F_READ		0x0001	/* file opened for reading */
#define	F_WRITE		0x0002	/* file opened for writing */
#define	F_ALLOC		0x0004	/* buffer allocated */
#define	F_FILE		0x0008	/* file instead of device */
#define	F_NBSF		0x0010	/* no bad sector forwarding */
#define	F_ECCLM		0x0020	/* limit # of bits in ecc correction */
#define	F_SSI		0x0040	/* set skip sector inhibit */
#define	F_SEVRE		0x0080	/* Severe burnin (no retries, no ECC) */

/* I/O types */
#define	F_RDDATA	0x0100	/* read data */
#define	F_WRDATA	0x0200	/* write data */
#define	F_HDR		0x0400	/* include header on next i/o */
#define	F_CHECK		0x0800	/* perform check of data read/write */
#define	F_HCHECK	0x1000	/* perform check of header and data */

#define	F_TYPEMASK	0xff00	/* I/O type mask */

/* Lseek values */
#define	L_SET		0	/* absolute offset */

/* Device switch */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy)();
	int	(*dv_open)();
	int	(*dv_close)();
	int	(*dv_ioctl)();
};

extern struct devsw devsw[];	/* device array */
extern int ndevs;		/* number of elements in devsw[] */

#ifdef COMPAT_42
/*
 * Old drive description table.
 * still used by some drivers for now.
 */
struct st {
	short	nsect;		/* # sectors/track */
	short	ntrak;		/* # tracks/surfaces/heads */
	short	nspc;		/* # sectors/cylinder */
	short	ncyl;		/* # cylinders */
	short	*off;		/* partition offset table (cylinders) */
};
#endif
