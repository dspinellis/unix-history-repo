/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)saio.h	7.1 (Berkeley) %G%
 */

/*
 * Header file for standalone package
 */

/*
 * Io block: includes an
 * inode, cells for the use of seek, etc,
 * and a buffer.
 */
struct	iob {
	int	i_flgs;		/* see F_ below */
	struct	inode i_ino;	/* inode, if file */
	int	i_unit;		/* pseudo device unit */
	daddr_t	i_boff;		/* block offset on device */
	off_t	i_offset;	/* seek offset in file */
	daddr_t	i_bn;		/* 1st block # of next read */
	char	*i_ma;		/* memory address of i/o buffer */
	int	i_cc;		/* character count of transfer */
	int	i_error;	/* error # return */
	int	i_errcnt;	/* error count for driver retries */
	int	i_errblk;	/* block # in error for error reporting */
	struct	fs i_fs;	/* file system super block info */
	char	i_buf[MAXBSIZE];/* i/o buffer */
};

/*
 * Macros to handle conversion from unit and partition to dev_t's.
 */
#define minor_partition(s)	(s & 7)
#define minor_unit(s)		((s>>3) & 31)
#define make_minor(u,p)		((u<<3) | p)

#define NULL 0

#define F_READ		0x1	/* file opened for reading */
#define F_WRITE		0x2	/* file opened for writing */
#define F_ALLOC		0x4	/* buffer allocated */
#define F_FILE		0x8	/* file instead of device */
#define F_NBSF		0x10	/* no bad sector forwarding */
#define F_ECCLM		0x20	/* limit # of bits in ecc correction */
#define F_SSI		0x40	/* set skip sector inhibit */
#define F_SEVRE		0x80	/* Severe burnin (no retries, no ECC) */
/* io types */
#define	F_RDDATA	0x0100	/* read data */
#define	F_WRDATA	0x0200	/* write data */
#define F_HDR		0x0400	/* include header on next i/o */
#define F_CHECK		0x0800	/* perform check of data read/write */
#define F_HCHECK	0x1000	/* perform check of header and data */

#define	F_TYPEMASK	0xff00

/*
 * Device switch.
 */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy)();
	int	(*dv_open)();
	int	(*dv_close)();
	int	(*dv_ioctl)();
};

struct devsw devsw[];

/*
 * Drive description table.
 * Returned from SAIODEVDATA call.
 */
struct st {
	short	nsect;		/* # sectors/track */
	short	ntrak;		/* # tracks/surfaces/heads */
	short	nspc;		/* # sectors/cylinder */
	short	ncyl;		/* # cylinders */
	short	*off;		/* partition offset table (cylinders) */
};

/*
 * Request codes. Must be the same a F_XXX above
 */
#define	READ	1
#define	WRITE	2

#define	NFILES	2
struct	iob iob[NFILES];

extern	int errno;	/* just like unix */

/* error codes */
#define	EBADF	1	/* bad file descriptor */
#define	EOFFSET	2	/* relative seek not supported */
#define	EDEV	3	/* improper device specification on open */
#define	ENXIO	4	/* unknown device specified */
#define	EUNIT	5	/* improper unit specification */
#define	ESRCH	6	/* directory search for file failed */
#define	EIO	7	/* generic error */
#define	ECMD	10	/* undefined driver command */
#define	EBSE	11	/* bad sector error */
#define	EWCK	12	/* write check error */
#define	EECC	13	/* uncorrectable ecc error */
#define	EHER	14	/* hard error */

/* ioctl's -- for disks just now */
#define	SAIOHDR		(('d'<<8)|1)	/* next i/o includes header */
#define	SAIOCHECK	(('d'<<8)|2)	/* next i/o checks data */
#define	SAIOHCHECK	(('d'<<8)|3)	/* next i/o checks header & data */
#define	SAIONOBAD	(('d'<<8)|4)	/* inhibit bad sector forwarding */
#define	SAIODOBAD	(('d'<<8)|5)	/* enable bad sector forwarding */
#define	SAIOECCLIM	(('d'<<8)|6)	/* limit ecc correction to 5 bits */
#define	SAIOECCUNL	(('d'<<8)|7)	/* use standard ecc procedures */
#define	SAIODEVDATA	(('d'<<8)|8)	/* get device data */
#define	SAIOSSI		(('d'<<8)|9)	/* set skip sector inhibit */
#define	SAIONOSSI	(('d'<<8)|10)	/* inhibit skip sector handling */
#define	SAIOSSDEV	(('d'<<8)|11)	/* is device skip sector type? */
#define	SAIODEBUG	(('d'<<8)|12)	/* enable/disable debugging */
#define	SAIOSEVRE	(('d'<<8)|13)	/* severe burnin, no ECC, no retries */
#define	SAIONSEVRE	(('d'<<8)|14)	/* clear severe burnin */
