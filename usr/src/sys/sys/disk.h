/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)disk.h	8.1 (Berkeley) %G%
 *
 * from: $Header: disk.h,v 1.5 92/11/19 04:33:03 torek Exp $ (LBL)
 */

/*
 * Disk device structures.
 *
 * Note that this is only a preliminary outline.  The final disk structures
 * may be somewhat different.
 */
struct buf;

struct dkdevice {
	struct	device dk_dev;		/* base device */
	struct	dkdevice *dk_next;	/* list of disks; not yet used */
	int	dk_bps;			/* xfer rate: bytes per second */
	int	dk_bopenmask;		/* block devices open */
	int	dk_copenmask;		/* character devices open */
	int	dk_openmask;		/* composite (bopen|copen) */
	int	dk_state;		/* label state   ### */
	int	dk_blkshift;		/* shift to convert DEV_BSIZE to blks */
	int	dk_byteshift;		/* shift to convert bytes to blks */
	struct	dkdriver *dk_driver;	/* pointer to driver */
	daddr_t	dk_labelsector;		/* sector containing label */
	struct	disklabel dk_label;	/* label */
};

struct dkdriver {
	void	(*d_strategy) __P((struct buf *));
#ifdef notyet
	int	(*d_open) __P((dev_t dev, int ifmt, int, struct proc *));
	int	(*d_close) __P((dev_t dev, int, int ifmt, struct proc *));
	int	(*d_ioctl) __P((dev_t dev, int cmd, caddr_t data, int fflag,
				struct proc *));
	int	(*d_dump) __P((dev_t));
	void	(*d_start) __P((struct buf *, daddr_t));
	int	(*d_mklabel) __P((struct dkdevice *));
#endif
};

/* states */
#define	DK_CLOSED	0		/* drive is closed */
#define	DK_WANTOPEN	1		/* drive being opened */
#define	DK_WANTOPENRAW	2		/* drive being opened */
#define	DK_RDLABEL	3		/* label being read */
#define	DK_OPEN		4		/* label read, drive open */
#define	DK_OPENRAW	5		/* open without label */

#ifdef DISKSORT_STATS
/*
 * Stats from disksort().
 */
struct disksort_stats {
	long	ds_newhead;		/* # new queue heads created */
	long	ds_newtail;		/* # new queue tails created */
	long	ds_midfirst;		/* # insertions into sort list */
	long	ds_endfirst;		/* # insertions at end of sort list */
	long	ds_newsecond;		/* # inversions (2nd lists) created */
	long	ds_midsecond;		/* # insertions into 2nd list */
	long	ds_endsecond;		/* # insertions at end of 2nd list */
};
#endif

#ifdef KERNEL
void	disksort __P((struct buf *, struct buf *));
char	*readdisklabel __P((struct dkdevice *, int));
int	setdisklabel __P((struct dkdevice *, struct disklabel *));
int	writedisklabel __P((struct dkdevice *, int));
int	diskerr __P((struct dkdevice *, struct buf *, char *, int, int));
#endif
