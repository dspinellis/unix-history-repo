/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sdvar.h	7.1 (Berkeley) %G%
 */

struct sdinfo {
	struct	disklabel si_label;	/* label */
	int	si_bopen;		/* mask of open block devs */
	int	si_copen;		/* mask of open char devs */
	int	si_open;		/* composite mask of open devs */
};

struct	sd_softc {
	struct	hp_device *sc_hd;
	struct	devqueue sc_dq;
	int	sc_format_pid;	/* process using "format" mode */
	short	sc_flags;
	short	sc_type;	/* drive type */
	short	sc_punit;	/* physical unit (scsi lun) */
	u_short	sc_bshift;	/* convert device blocks to DEV_BSIZE blks */
	u_int	sc_blks;	/* number of blocks on device */
	int	sc_blksize;	/* device block size in bytes */
	u_int	sc_wpms;	/* average xfer rate in 16 bit wds/sec. */
	struct	sdinfo sc_info;	/* disklabel and related info */
};

/* sc_flags values */
#define	SDF_ALIVE	0x01
#define SDF_OPENING	0x02
#define SDF_CLOSING	0x04
#define SDF_WANTED	0x08
#define SDF_WLABEL	0x10
#define SDF_RMEDIA	0x20
#define SDF_ERROR	0x40

struct sdstats {
	long	sdresets;
	long	sdtransfers;
	long	sdpartials;
};

#define	sdunit(x)	(minor(x) >> 3)
#define sdpart(x)	(minor(x) & 0x7)
#define	sdpunit(x)	((x) & 7)
#define sdlabdev(d)	(dev_t)(((int)(d)&~7)|2)	/* sd?c */

#define	b_cylin		b_resid

#define	SDRETRY		2
