/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vd.c	7.12 (Berkeley) 6/28/90
 */

#include "dk.h"
#if NVD > 0
/*
 * Versabus VDDC/SMDE driver.
 */
#include "param.h"
#include "buf.h"
#include "cmap.h"
#include "conf.h"
#include "dkstat.h"
#include "disklabel.h"
#include "map.h"
#include "file.h"
#include "systm.h"
#include "user.h"
#include "vmmac.h"
#include "proc.h"
#include "syslog.h"
#include "kernel.h"
#include "ioctl.h"
#include "stat.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/vdreg.h"

#ifndef	COMPAT_42
#define	COMPAT_42
#endif
#define	B_FORMAT	B_XXX		/* XXX */

#define vdunit(dev)	(minor(dev) >> 3)
#define vdpart(dev)	(minor(dev) & 0x07)
#define	vdminor(unit,part)	(((unit) << 3) | (part))

struct	vba_ctlr *vdminfo[NVD];
struct  vba_device *vddinfo[NDK];
int	vdprobe(), vdslave(), vdattach(), vddgo(), vdstrategy();
long	vdstd[] = { 0xffff2000, 0xffff2100, 0xffff2200, 0xffff2300, 0 };
struct	vba_driver vddriver =
  { vdprobe, vdslave, vdattach, vddgo, vdstd, "dk", vddinfo, "vd", vdminfo };

/*
 * Per-controller state.
 */
struct vdsoftc {
	u_short	vd_flags;
#define	VD_PRINT	0x1	/* controller info printed */
#define	VD_STARTED	0x2	/* start command issued */
#define	VD_DOSEEKS	0x4	/* should overlap seeks */
#define	VD_SCATGATH	0x8	/* can do scatter-gather commands (correctly) */
#define	VD_LOCKED	0x10	/* locked for direct controller access */
#define	VD_WAIT		0x20	/* someone needs direct controller access */
	u_short	vd_type;	/* controller type */
	u_short	vd_wticks;	/* timeout */
	u_short	vd_secsize;	/* sector size for controller */
	struct	mdcb vd_mdcb;	/* master command block */
	u_long	vd_mdcbphys;	/* physical address of vd_mdcb */
	struct	dcb vd_dcb;	/* i/o command block */
	u_long	vd_dcbphys;	/* physical address of vd_dcb */
	struct	vb_buf vd_rbuf;	/* vba resources */
} vdsoftc[NVD];

#define	VDMAXTIME	20	/* max time for operation, sec. */

/*
 * Per-drive state.
 */
struct	dksoftc {
	int	dk_state;	/* open fsm */
#ifndef SECSIZE
	u_short	dk_bshift;	/* shift for * (DEV_BSIZE / sectorsize) XXX */
#endif SECSIZE
	int	dk_wlabel;	/* label sector is currently writable */
	u_long	dk_copenpart;	/* character units open on this drive */
	u_long	dk_bopenpart;	/* block units open on this drive */
	u_long	dk_openpart;	/* all units open on this drive */
	u_int	dk_curcyl;	/* last selected cylinder */
	struct	skdcb dk_dcb;	/* seek command block */
	u_long	dk_dcbphys;	/* physical address of dk_dcb */
	int	df_reg[3];	/* for formatting, in-out parameters */
} dksoftc[NDK];

/*
 * Drive states.  Used during steps of open/initialization.
 * States < OPEN (> 0) are transient, during an open operation.
 * OPENRAW is used for unlabeled disks, to allow format operations.
 */
#define	CLOSED		0		/* disk is closed */
#define	WANTOPEN	1		/* open requested, not started */
#define	WANTOPENRAW	2		/* open requested, no label */
#define	RDLABEL		3		/* reading pack label */
#define	OPEN		4		/* intialized and ready */
#define	OPENRAW		5		/* open, no label */

struct	buf dkutab[NDK];	/* i/o queue headers */
struct	disklabel dklabel[NDK];	/* pack labels */

#define b_cylin	b_resid
#define	b_track	b_error		/* used for seek commands */
#define	b_seekf	b_forw		/* second queue on um_tab */
#define	b_seekl	b_back		/* second queue on um_tab */

int	vdwstart, vdwatch();

/*
 * See if the controller is really there; if so, initialize it.
 */
vdprobe(reg, vm)
	caddr_t reg;
	struct vba_ctlr *vm;
{
	register br, cvec;		/* must be r12, r11 */
	register struct vddevice *vdaddr = (struct vddevice *)reg;
	struct vdsoftc *vd;
	int s;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	vdintr(0);
#endif
	if (badaddr((caddr_t)reg, 2))
		return (0);
	vd = &vdsoftc[vm->um_ctlr];
	vdaddr->vdreset = 0xffffffff;
	DELAY(1000000);
	if (vdaddr->vdreset != (unsigned)0xffffffff) {
		vd->vd_type = VDTYPE_VDDC;
		vd->vd_flags &= ~VD_DOSEEKS;
		DELAY(1000000);
	} else {
		vd->vd_type = VDTYPE_SMDE;
		vd->vd_flags |= VD_DOSEEKS;
		vdaddr->vdrstclr = 0;
		DELAY(3000000);
	}
	vd->vd_mdcbphys = vtoph((struct proc *)0, (unsigned)&vd->vd_mdcb);
	vd->vd_dcbphys = vtoph((struct proc *)0, (unsigned)&vd->vd_dcb);
	vm->um_addr = reg;		/* XXX */
	s = spl7();
	if (vdinit_ctlr(vm, vd) == 0) {
		splx(s);
		return (0);
	}
	if (vd->vd_type == VDTYPE_SMDE) {
#ifdef notdef
		/*
		 * Attempt PROBE to get all drive status;
		 * we take advantage of this in vdreset_drive
		 * to try to avoid guessing games.
		 */
		(void) vdcmd(vm, VDOP_PROBE, 5, 0);
#endif
		/*
		 * Check for scatter-gather by checking firmware date
		 * with IDENT command.  The date is printed when
		 * vdslave is first called, thus this must be
		 * the last controller operation in vdprobe.
		 */
		vd->vd_dcb.trail.idtrail.date = 0;
		if (vdcmd(vm, VDOP_IDENT, 10, 0)) {
			uncache(&vd->vd_dcb.trail.idtrail.date);
			if (vd->vd_dcb.trail.idtrail.date != 0)
				vd->vd_flags |= VD_SCATGATH;
		}
	}
	splx(s);
	/*
	 * Allocate page tables and i/o buffer.
	 */
	if (vbainit(&vd->vd_rbuf, MAXPHYS,
	    vd->vd_type == VDTYPE_VDDC ? VB_24BIT : VB_32BIT) == 0) {
		printf("vd%d: vbainit failed\n", vm->um_ctlr);
		return (0);
	}
	br = 0x17, cvec = 0xe0 + vm->um_ctlr;	/* XXX */
	return (sizeof (struct vddevice));
}

/*
 * See if a drive is really there.
 *
 * Can't read pack label here as various data structures
 * aren't setup for doing a read in a straightforward
 * manner.  Instead just probe for the drive and leave
 * the pack label stuff to the attach routine.
 */
/* ARGSUSED */
vdslave(vi, vdaddr)
	register struct vba_device *vi;
	struct vddevice *vdaddr;
{
	register struct disklabel *lp = &dklabel[vi->ui_unit];
	register struct dksoftc *dk = &dksoftc[vi->ui_unit];
	struct vdsoftc *vd = &vdsoftc[vi->ui_ctlr];
	int bcd();

	if ((vd->vd_flags&VD_PRINT) == 0) {
		printf("vd%d: %s controller", vi->ui_ctlr,
		    vd->vd_type == VDTYPE_VDDC ? "VDDC" : "SMDE");
		if (vd->vd_flags & VD_SCATGATH) {
			char rev[5];

			bcopy((caddr_t)&vd->vd_dcb.trail.idtrail.rev, rev,
			    sizeof(vd->vd_dcb.trail.idtrail.rev));
			printf(" firmware rev %s (%d-%d-%d)", rev,
			    bcd((vd->vd_dcb.trail.idtrail.date >> 8) & 0xff),
			    bcd(vd->vd_dcb.trail.idtrail.date & 0xff),
			    bcd((vd->vd_dcb.trail.idtrail.date >> 16)&0xffff));
		}
		printf("\n");
		vd->vd_flags |= VD_PRINT;
	}

	/*
	 * Initialize label enough to do a reset on
	 * the drive.  The remainder of the default
	 * label values will be filled in in vdinit
	 * at attach time.
	 */
	if (vd->vd_type == VDTYPE_SMDE)
		lp->d_secsize = VD_MAXSECSIZE;
	else
		lp->d_secsize = VDDC_SECSIZE;
	lp->d_nsectors = 66;		/* only used on smd-e */
	lp->d_ntracks = 23;
	lp->d_ncylinders = 850;
	lp->d_secpercyl = 66*23;
	lp->d_rpm = 3600;
	lp->d_npartitions = 1;
	lp->d_partitions[0].p_offset = 0;
	lp->d_partitions[0].p_size = LABELSECTOR + 1;

	/*
	 * Initialize invariant portion of
	 * dcb used for overlapped seeks.
	 */
	dk->dk_dcb.opcode = VDOP_SEEK;
	dk->dk_dcb.intflg = DCBINT_NONE | DCBINT_PBA;
	dk->dk_dcb.devselect = vi->ui_slave;
	dk->dk_dcb.trailcnt = sizeof (struct trseek) / sizeof (long);
	dk->dk_dcb.trail.sktrail.skaddr.sector = 0;
	dk->dk_dcbphys = vtoph((struct proc *)0, (unsigned)&dk->dk_dcb);
#ifndef SECSIZE
	vd_setsecsize(dk, lp);
#endif
	return (vdreset_drive(vi));
}

static int
bcd(n)
	register u_int n;
{
	register int bin = 0;
	register int mul = 1;

	while (n) {
		bin += (n & 0xf) * mul;
		n >>= 4;
		mul *= 10;
	}
	return (bin);
}

vdattach(vi)
	register struct vba_device *vi;
{
	register int unit = vi->ui_unit;
	register struct disklabel *lp = &dklabel[unit];

	/*
	 * Try to initialize device and read pack label.
	 */
	if (vdinit(vdminor(unit, 0), 0) != 0) {
		printf(": unknown drive type");
		return;
	}
	if (dksoftc[unit].dk_state == OPEN)
		printf(": %s <secsize %d, ntrak %d, ncyl %d, nsec %d>",
		    lp->d_typename, lp->d_secsize,
		    lp->d_ntracks, lp->d_ncylinders, lp->d_nsectors);
	/*
	 * (60 / rpm) / (sectors per track * (bytes per sector / 2))
	 */
	if (vi->ui_dk >= 0)
		dk_wpms[vi->ui_dk] =
		    (lp->d_rpm * lp->d_nsectors * lp->d_secsize) / 120;
#ifdef notyet
	addswap(makedev(VDMAJOR, vdminor(unit, 0)), lp);
#endif
}

vdopen(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register unit = vdunit(dev);
	register struct disklabel *lp;
	register struct dksoftc *dk;
	register struct partition *pp;
	struct vba_device *vi;
	int s, error = 0, part = vdpart(dev), mask = 1 << part;
	daddr_t start, end;

	if (unit >= NDK || (vi = vddinfo[unit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	lp = &dklabel[unit];
	dk = &dksoftc[unit];

	s = spl7();
	while (dk->dk_state != OPEN && dk->dk_state != OPENRAW &&
	    dk->dk_state != CLOSED)
		if (error = tsleep((caddr_t)dk, (PZERO+1) | PCATCH, devopn, 0))
			break;
	splx(s);
	if (error)
		return (error);
	if (dk->dk_state != OPEN && dk->dk_state != OPENRAW)
		if (error = vdinit(dev, flags))
			return (error);

	if (vdwstart == 0) {
		timeout(vdwatch, (caddr_t)0, hz);
		vdwstart++;
	}
	/*
	 * Warn if a partion is opened
	 * that overlaps another partition which is open
	 * unless one is the "raw" partition (whole disk).
	 */
#define	RAWPART		8		/* 'x' partition */	/* XXX */
	if ((dk->dk_openpart & mask) == 0 && part != RAWPART) {
		pp = &lp->d_partitions[part];
		start = pp->p_offset;
		end = pp->p_offset + pp->p_size;
		for (pp = lp->d_partitions;
		     pp < &lp->d_partitions[lp->d_npartitions]; pp++) {
			if (pp->p_offset + pp->p_size <= start ||
			    pp->p_offset >= end)
				continue;
			if (pp - lp->d_partitions == RAWPART)
				continue;
			if (dk->dk_openpart & (1 << (pp - lp->d_partitions)))
				log(LOG_WARNING,
				    "dk%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a',
				    pp - lp->d_partitions + 'a');
		}
	}
	if (part >= lp->d_npartitions)
		return (ENXIO);
	dk->dk_openpart |= mask;
	switch (fmt) {
	case S_IFCHR:
		dk->dk_copenpart |= mask;
		break;
	case S_IFBLK:
		dk->dk_bopenpart |= mask;
		break;
	}
	return (0);
}

/* ARGSUSED */
vdclose(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register int unit = vdunit(dev);
	register struct dksoftc *dk = &dksoftc[unit];
	int part = vdpart(dev), mask = 1 << part;

	switch (fmt) {
	case S_IFCHR:
		dk->dk_copenpart &= ~mask;
		break;
	case S_IFBLK:
		dk->dk_bopenpart &= ~mask;
		break;
	}
	if (((dk->dk_copenpart | dk->dk_bopenpart) & mask) == 0)
		dk->dk_openpart &= ~mask;
	/*
	 * Should wait for i/o to complete on this partition
	 * even if others are open, but wait for work on blkflush().
	 */
	if (dk->dk_openpart == 0) {
		int s = spl7();
		while (dkutab[unit].b_actf)
			sleep((caddr_t)dk, PZERO-1);
		splx(s);
		dk->dk_state = CLOSED;
		dk->dk_wlabel = 0;
	}
	return (0);
}

vdinit(dev, flags)
	dev_t dev;
	int flags;
{
	register struct disklabel *lp;
	register struct dksoftc *dk;
	struct vba_device *vi;
	int unit = vdunit(dev), error = 0;
	char *msg, *readdisklabel();
	extern int cold;

	dk = &dksoftc[unit];
	if (flags & O_NDELAY) {
		dk->dk_state = OPENRAW;
		return (0);
	}
	dk->dk_state = RDLABEL;
	lp = &dklabel[unit];
	vi = vddinfo[unit];
	if (msg = readdisklabel(dev, vdstrategy, lp)) {
		if (cold) {
			printf(": %s", msg);
			dk->dk_state = CLOSED;
		} else {
			log(LOG_ERR, "dk%d: %s\n", unit, msg);
			dk->dk_state = OPENRAW;
		}
#ifdef COMPAT_42
		vdlock(vi->ui_ctlr);
		if (vdmaptype(vi, lp))
			dk->dk_state = OPEN;
		vdunlock(vi->ui_ctlr);
#endif
	} else {
		/*
		 * Now that we have the label, configure
		 * the correct drive parameters.
		 */
		vdlock(vi->ui_ctlr);
		if (vdreset_drive(vi))
			dk->dk_state = OPEN;
		else {
			dk->dk_state = CLOSED;
			error = ENXIO;
		}
		vdunlock(vi->ui_ctlr);
	}
#ifndef SECSIZE
	vd_setsecsize(dk, lp);
#endif
	wakeup((caddr_t)dk);
	return (error);
}

#ifndef SECSIZE
vd_setsecsize(dk, lp)
	register struct dksoftc *dk;
	register struct disklabel *lp;
{
	int mul;

	/*
	 * Calculate scaling shift for mapping
	 * DEV_BSIZE blocks to drive sectors.
	 */
	mul = DEV_BSIZE / lp->d_secsize;
	dk->dk_bshift = 0;
	while ((mul >>= 1) > 0)
		dk->dk_bshift++;
}
#endif SECSIZE

/*ARGSUSED*/
vddgo(vm)
	struct vba_device *vm;
{

}

vdstrategy(bp)
	register struct buf *bp;
{
	register struct vba_device *vi;
	register struct disklabel *lp;
	register struct dksoftc *dk;
	register int unit;
	register daddr_t sn;
	struct buf *dp;
	daddr_t sz, maxsz;
	int part, s;

	unit = vdunit(bp->b_dev);
	if (unit >= NDK) {
		bp->b_error = ENXIO;
		goto bad;
	}
	vi = vddinfo[unit];
	lp = &dklabel[unit];
	if (vi == 0 || vi->ui_alive == 0) {
		bp->b_error = ENXIO;
		goto bad;
	}
	dk = &dksoftc[unit];
	if (dk->dk_state < OPEN) {
		if (dk->dk_state == CLOSED) {
			bp->b_error = EIO;
			goto bad;
		}
		goto q;
	}
	if (dk->dk_state != OPEN && (bp->b_flags & B_READ) == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	part = vdpart(bp->b_dev);
	if ((dk->dk_openpart & (1 << part)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}
	sz = (bp->b_bcount + lp->d_secsize - 1) / lp->d_secsize;
	maxsz = lp->d_partitions[part].p_size;
#ifndef SECSIZE
	sn = bp->b_blkno << dk->dk_bshift;
#else SECSIZE
	sn = bp->b_blkno;
#endif SECSIZE
	if (sn + lp->d_partitions[part].p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
	    sn + lp->d_partitions[part].p_offset + sz > LABELSECTOR &&
#endif
	    (bp->b_flags & B_READ) == 0 && dk->dk_wlabel == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	if (sn < 0 || sn + sz > maxsz) {
		if (sn == maxsz) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		sz = maxsz - sn;
		if (sz <= 0) {
			bp->b_error = EINVAL;
			goto bad;
		}
		bp->b_bcount = sz * lp->d_secsize;
	}
	bp->b_cylin = (sn + lp->d_partitions[part].p_offset) / lp->d_secpercyl;
#ifdef SECSIZE
if (bp->b_blksize != lp->d_secsize && (bp->b_flags & B_PGIN) == 0)
panic("vdstrat blksize");
#endif SECSIZE
q:
	s = spl7();
	dp = &dkutab[vi->ui_unit];
	disksort(dp, bp);
	if (!dp->b_active) {
		(void) vdustart(vi);
		if (!vi->ui_mi->um_tab.b_active)
			vdstart(vi->ui_mi);
	}
	splx(s);
	return;
bad:	
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
	return;
}

vdustart(vi)
	register struct vba_device *vi;
{
	register struct buf *bp, *dp;
	register struct vba_ctlr *vm;
	register int unit = vi->ui_unit;
	register struct dksoftc *dk;
	register struct vdsoftc *vd;
	struct disklabel *lp;

	dp = &dkutab[unit];
	/*
	 * If queue empty, nothing to do.
	 */
	if ((bp = dp->b_actf) == NULL)
		return;
	/*
	 * If drive is off-cylinder and controller supports seeks,
	 * place drive on seek queue for controller.
	 * Otherwise, place on transfer queue.
	 */
	vd = &vdsoftc[vi->ui_ctlr];
	dk = &dksoftc[unit];
	vm = vi->ui_mi;
	if (bp->b_cylin != dk->dk_curcyl && vd->vd_flags&VD_DOSEEKS) {
		lp = &dklabel[unit];
		bp->b_track = (bp->b_blkno % lp->d_secpercyl) / lp->d_nsectors;
		if (vm->um_tab.b_seekf == NULL)
			vm->um_tab.b_seekf = dp;
		else
			vm->um_tab.b_seekl->b_forw = dp;
		vm->um_tab.b_seekl = dp;
	} else {
		if (vm->um_tab.b_actf == NULL)
			vm->um_tab.b_actf = dp;
		else
			vm->um_tab.b_actl->b_forw = dp;
		vm->um_tab.b_actl = dp;
	}
	dp->b_forw = NULL;
	dp->b_active++;
}

/*
 * Start next transfer on a controller.
 * There are two queues of drives, the first on-cylinder
 * and the second off-cylinder from their next transfers.
 * Perform the first transfer for the first drive on the on-cylinder
 * queue, if any, otherwise the first transfer for the first drive
 * on the second queue.  Initiate seeks on remaining drives on the
 * off-cylinder queue, then move them all to the on-cylinder queue.
 */
vdstart(vm)
	register struct vba_ctlr *vm;
{
	register struct buf *bp;
	register struct vba_device *vi;
	register struct vdsoftc *vd;
	register struct dksoftc *dk;
	register struct disklabel *lp;
	register struct dcb **dcbp;
	struct buf *dp;
	int sn, tn;

loop:
	/*
	 * Pull a request off the controller queue.
	 */
	if ((dp = vm->um_tab.b_actf) == NULL &&
	    (dp = vm->um_tab.b_seekf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		if (dp == vm->um_tab.b_actf)
			vm->um_tab.b_actf = dp->b_forw;
		else
			vm->um_tab.b_seekf = dp->b_forw;
		goto loop;
	}

	/*
	 * Mark controller busy, and determine
	 * destination of this request.
	 */
	vm->um_tab.b_active++;
	vi = vddinfo[vdunit(bp->b_dev)];
	dk = &dksoftc[vi->ui_unit];
#ifndef SECSIZE
	sn = bp->b_blkno << dk->dk_bshift;
#else SECSIZE
	sn = bp->b_blkno;
#endif SECSIZE
	lp = &dklabel[vi->ui_unit];
	sn %= lp->d_secpercyl;
	tn = sn / lp->d_nsectors;
	sn %= lp->d_nsectors;

	/*
	 * Construct dcb for read/write command.
	 */
	vd = &vdsoftc[vm->um_ctlr];
	vd->vd_dcb.intflg = DCBINT_DONE;
	vd->vd_dcb.devselect = dk->dk_dcb.devselect;
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.trail.rwtrail.disk.cylinder = bp->b_cylin;
	vd->vd_dcb.trail.rwtrail.disk.track = tn;
	vd->vd_dcb.trail.rwtrail.disk.sector = sn;
	dk->dk_curcyl = bp->b_cylin;
	bp->b_track = 0;		/* init overloaded field */
	vd->vd_dcb.trailcnt = sizeof (struct trrw) / sizeof (long);
	if (bp->b_flags & B_FORMAT)
		vd->vd_dcb.opcode = dk->dk_op;
	else if (vd->vd_flags & VD_SCATGATH &&
	    ((int)bp->b_un.b_addr & (sizeof(long) - 1)) == 0)
		vd->vd_dcb.opcode = (bp->b_flags & B_READ)? VDOP_RAS : VDOP_GAW;
	else
		vd->vd_dcb.opcode = (bp->b_flags & B_READ)? VDOP_RD : VDOP_WD;

	switch (vd->vd_dcb.opcode) {
	case VDOP_FSECT:
		vd->vd_dcb.trailcnt = sizeof (struct trfmt) / sizeof (long);
		vd->vd_dcb.trail.fmtrail.nsectors = bp->b_bcount /
		    lp->d_secsize;
		vd->vd_dcb.trail.fmtrail.hdr = *(dskadr *)&dk->dk_althdr;
		vd->vd_dcb.trail.rwtrail.disk.cylinder |= dk->dk_fmtflags;
		goto setupaddr;

	case VDOP_RDRAW:
	case VDOP_RD:
	case VDOP_RHDE:
	case VDOP_WD:
		vd->vd_dcb.trail.rwtrail.wcount = (bp->b_bcount+1) >> 1;
setupaddr:
		vd->vd_dcb.trail.rwtrail.memadr =
			vbasetup(bp, &vd->vd_rbuf, (int)lp->d_secsize);
		break;

	case VDOP_RAS:
	case VDOP_GAW:
		vd->vd_dcb.trailcnt += vd_sgsetup(bp, &vd->vd_rbuf,
		    &vd->vd_dcb.trail.sgtrail);
		break;
	}
	if (vi->ui_dk >= 0) {
		dk_busy |= 1<<vi->ui_dk;
		dk_xfer[vi->ui_dk]++;
		dk_wds[vi->ui_dk] += bp->b_bcount>>6;
	}

	/*
	 * Look for any seeks to be performed on other drives on this
	 * controller.  If overlapped seeks exist, insert seek commands
	 * on the controller's command queue before the transfer.
	 */
	dcbp = &vd->vd_mdcb.mdcb_head;

	if (dp == vm->um_tab.b_seekf)
		dp = dp->b_forw;
	else
		dp = vm->um_tab.b_seekf;
	for (; dp != NULL; dp = dp->b_forw) {
		if ((bp = dp->b_actf) == NULL)
			continue;
		vi = vddinfo[vdunit(bp->b_dev)];
		dk = &dksoftc[vi->ui_unit];
		dk->dk_curcyl = bp->b_cylin;
		if (vi->ui_dk >= 0)
			dk_seek[vi->ui_dk]++;
		dk->dk_dcb.operrsta = 0;
		dk->dk_dcb.trail.sktrail.skaddr.cylinder = bp->b_cylin;
		dk->dk_dcb.trail.sktrail.skaddr.track = bp->b_track;
		*dcbp = (struct dcb *)dk->dk_dcbphys;
		dcbp = &dk->dk_dcb.nxtdcb;
	}
	*dcbp = (struct dcb *)vd->vd_dcbphys;
	if (vm->um_tab.b_actf)
		vm->um_tab.b_actl->b_forw = vm->um_tab.b_seekf;
	else
		vm->um_tab.b_actf = vm->um_tab.b_seekf;
	if (vm->um_tab.b_seekf)
		vm->um_tab.b_actl = vm->um_tab.b_seekl;
	vm->um_tab.b_seekf = 0;

	/*
	 * Initiate operation.
	 */
	vd->vd_mdcb.mdcb_status = 0;
	VDGO(vm->um_addr, vd->vd_mdcbphys, vd->vd_type);
}

/*
 * Wait for controller to finish current operation
 * so that direct controller accesses can be done.
 */
vdlock(ctlr)
{
	register struct vba_ctlr *vm = vdminfo[ctlr];
	register struct vdsoftc *vd = &vdsoftc[ctlr];
	int s;

	s = spl7();
	while (vm->um_tab.b_active || vd->vd_flags & VD_LOCKED) {
		vd->vd_flags |= VD_WAIT;
		sleep((caddr_t)vd, PRIBIO);
	}
	vd->vd_flags |= VD_LOCKED;
	splx(s);
}

/*
 * Continue normal operations after pausing for 
 * munging the controller directly.
 */
vdunlock(ctlr)
{
	register struct vba_ctlr *vm = vdminfo[ctlr];
	register struct vdsoftc *vd = &vdsoftc[ctlr];

	vd->vd_flags &= ~VD_LOCKED;
	if (vd->vd_flags & VD_WAIT) {
		vd->vd_flags &= ~VD_WAIT;
		wakeup((caddr_t)vd);
	} else if (vm->um_tab.b_actf || vm->um_tab.b_seekf)
		vdstart(vm);
}

#define	DONTCARE (DCBS_DSE|DCBS_DSL|DCBS_TOP|DCBS_TOM|DCBS_FAIL|DCBS_DONE)
/*
 * Handle a disk interrupt.
 */
vdintr(ctlr)
	register ctlr;
{
	register struct buf *bp, *dp;
	register struct vba_ctlr *vm = vdminfo[ctlr];
	register struct vba_device *vi;
	register struct vdsoftc *vd = &vdsoftc[ctlr];
	register status;
	int timedout;
	struct dksoftc *dk;

	if (!vm->um_tab.b_active) {
		printf("vd%d: stray interrupt\n", ctlr);
		return;
	}
	/*
	 * Get device and block structures, and a pointer
	 * to the vba_device for the drive.
	 */
	dp = vm->um_tab.b_actf;
	bp = dp->b_actf;
	vi = vddinfo[vdunit(bp->b_dev)];
	dk = &dksoftc[vi->ui_unit];
	if (vi->ui_dk >= 0)
		dk_busy &= ~(1<<vi->ui_dk);
	timedout = (vd->vd_wticks >= VDMAXTIME);
	/*
	 * Check for and process errors on
	 * either the drive or the controller.
	 */
	uncache(&vd->vd_dcb.operrsta);
	status = vd->vd_dcb.operrsta;
	if (bp->b_flags & B_FORMAT) {
		dk->dk_operrsta = status;
		uncache(&vd->vd_dcb.err_code);
		/* ecodecnt gets err_code + err_wcnt from the same longword */
		dk->dk_ecodecnt = *(long *)&vd->vd_dcb.err_code;
		uncache(&vd->vd_dcb.err_trk);
		/* erraddr gets error trk/sec/cyl from the same longword */
		dk->dk_erraddr = *(long *)&vd->vd_dcb.err_trk;
	} else if (status & VDERR_HARD || timedout) {
		if (vd->vd_type == VDTYPE_SMDE)
			uncache(&vd->vd_dcb.err_code);
		if (status & DCBS_WPT) {
			/*
			 * Give up on write locked devices immediately.
			 */
			printf("dk%d: write locked\n", vi->ui_unit);
			bp->b_flags |= B_ERROR;
		} else if (status & VDERR_RETRY || timedout) {
			if (status & VDERR_CTLR || timedout) {
				vdharderr(timedout ?
				    "controller timeout" : "controller err",
				    vd, bp, &vd->vd_dcb);
				printf("; resetting controller...");
				vdreset_ctlr(vm);
			} else if (status & VDERR_DRIVE) {
				vdharderr("drive err", vd, bp, &vd->vd_dcb);
				printf("; resetting drive...");
				if (!vdreset_drive(vi))
					dk->dk_state = CLOSED;
			} else
				vdharderr("data err", vd, bp, &vd->vd_dcb);
			/*
			 * Retry transfer once, unless reset failed.
			 */
			if (!vi->ui_alive || dp->b_errcnt++ >= 1) {
				printf("\n");
				goto hard;
			}

			printf(" retrying\n");
			vm->um_tab.b_active = 0;	/* force retry */
		} else  {
			vdharderr("hard error", vd, bp, &vd->vd_dcb);
			printf("\n");
	hard:
			bp->b_flags |= B_ERROR;
		}
	} else if (status & DCBS_SOFT)
		vdsofterr(bp, &vd->vd_dcb);
if (vd->vd_wticks > 3) {
vd->vd_dcb.err_code = vd->vd_wticks;
vdharderr("slow transfer (ecode is sec.)", vd, bp, &vd->vd_dcb);
printf("\n");
}
	vd->vd_wticks = 0;
	if (vm->um_tab.b_active) {
		vm->um_tab.b_active = 0;
		vm->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_resid = 0;
		vbadone(bp, &vd->vd_rbuf);
		biodone(bp);
		/*
		 * If this unit has more work to do,
		 * then start it up right away.
		 */
		if (dp->b_actf)
			vdustart(vi);
		else if (dk->dk_openpart == 0)
			wakeup((caddr_t)dk);
	}
	/*
	 * If there are devices ready to
	 * transfer, start the controller.
	 */
	if (vd->vd_flags & VD_WAIT) {
		vd->vd_flags &= ~VD_WAIT;
		wakeup((caddr_t)vd);
	} else if (vm->um_tab.b_actf || vm->um_tab.b_seekf)
		vdstart(vm);
}

vdharderr(what, vd, bp, dcb)
	char *what;
	struct vdsoftc *vd;
	register struct buf *bp;
	register struct dcb *dcb;
{
	int unit = vdunit(bp->b_dev), status = dcb->operrsta;
	register struct disklabel *lp = &dklabel[unit];
	int blkdone;

	if (vd->vd_wticks < VDMAXTIME)
		status &= ~DONTCARE;
	blkdone = ((((dcb->err_cyl & 0xfff) * lp->d_ntracks + dcb->err_trk) *
	    lp->d_nsectors + dcb->err_sec -
	    lp->d_partitions[vdpart(bp->b_dev)].p_offset) >>
	    dksoftc[unit].dk_bshift) - bp->b_blkno;
	diskerr(bp, "dk", what, LOG_PRINTF, blkdone, lp);
	printf(", status %b", status, VDERRBITS);
	if (vd->vd_type == VDTYPE_SMDE)
		printf(" ecode %x", dcb->err_code);
}

vdsofterr(bp, dcb)
	register struct buf *bp;
	register struct dcb *dcb;
{
	int unit = vdunit(bp->b_dev);
	struct disklabel *lp = &dklabel[unit];
	int status = dcb->operrsta;
	int blkdone;

	blkdone = ((((dcb->err_cyl & 0xfff) * lp->d_ntracks + dcb->err_trk) *
	    lp->d_nsectors + dcb->err_sec -
	    lp->d_partitions[vdpart(bp->b_dev)].p_offset) >>
	    dksoftc[unit].dk_bshift) - bp->b_blkno;

	if (status != (DCBS_CCD|DCBS_SOFT|DCBS_ERR|DCBS_DONE)) {
		diskerr(bp, "dk", "soft error", LOG_WARNING, blkdone, lp);
		addlog(", status %b ecode %x\n", status, VDERRBITS,
		    dcb->err_code);
	} else {
		diskerr(bp, "dk", "soft ecc", LOG_WARNING, blkdone, lp);
		addlog("\n");
	}
}

vdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register int unit = vdunit(dev);
	register struct disklabel *lp = &dklabel[unit];
	register struct dksoftc *dk = &dksoftc[unit];
	int error = 0, vdformat();

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[vdpart(dev)];
		break;

	case DIOCSDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			error = setdisklabel(lp, (struct disklabel *)data,
			    (dk->dk_state == OPENRAW) ? 0 : dk->dk_openpart);
		if (error == 0 && dk->dk_state == OPENRAW &&
		    vdreset_drive(vddinfo[unit]))
			dk->dk_state = OPEN;
		break;

	case DIOCWLABEL:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			dk->dk_wlabel = *(int *)data;
		break;

	case DIOCWDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else if ((error = setdisklabel(lp, (struct disklabel *)data,
		    (dk->dk_state == OPENRAW) ? 0 : dk->dk_openpart)) == 0) {
			int wlab;

			if (error == 0 && dk->dk_state == OPENRAW &&
			    vdreset_drive(vddinfo[unit]))
				dk->dk_state = OPEN;
			/* simulate opening partition 0 so write succeeds */
			dk->dk_openpart |= (1 << 0);		/* XXX */
			wlab = dk->dk_wlabel;
			dk->dk_wlabel = 1;
			error = writedisklabel(dev, vdstrategy, lp);
			dk->dk_openpart = dk->dk_copenpart | dk->dk_bopenpart;
			dk->dk_wlabel = wlab;
		}
		break;

	case DIOCWFORMAT:
	    {
		register struct format_op *fop;
		struct uio auio;
		struct iovec aiov;

		if ((flag & FWRITE) == 0) {
			error = EBADF;
			break;
		}
		fop = (struct format_op *)data;
		aiov.iov_base = fop->df_buf;
		aiov.iov_len = fop->df_count;
		auio.uio_iov = &aiov;
		auio.uio_iovcnt = 1;
		auio.uio_resid = fop->df_count;
		auio.uio_segflg = UIO_USERSPACE;
		auio.uio_offset = fop->df_startblk * lp->d_secsize;
		/* This assumes one active format operation per disk... */
		dk->dk_op = fop->dk_op;
		dk->dk_althdr = fop->dk_althdr;
		dk->dk_fmtflags = fop->dk_fmtflags;
		/*
		 * Don't return errors, as the format op won't get copied
		 * out if we return nonzero.  Callers must check the returned
		 * registers and count.
		 */
		error = physio(vdformat, (struct buf *)NULL, dev,
		     B_WRITE, minphys, &auio);
		if (error == EIO)
			error = 0;
		fop->df_count -= auio.uio_resid;
		/* This assumes one active format operation per disk... */
		fop->dk_operrsta = dk->dk_operrsta;
		fop->dk_ecodecnt = dk->dk_ecodecnt;
		fop->dk_erraddr = dk->dk_erraddr;
		break;
	    }

	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

vdformat(bp)
	struct buf *bp;
{
	bp->b_flags |= B_FORMAT;
	vdstrategy(bp);
}

/*
 * Watch for lost interrupts.
 */
vdwatch()
{
	register struct vdsoftc *vd;
	register struct vba_ctlr *vm;
	register int ctlr;
	int s;

	timeout(vdwatch, (caddr_t)0, hz);
	for (ctlr = 0; ctlr < NVD; ctlr++) {
		vm = vdminfo[ctlr];
		if (vm == 0 || vm->um_alive == 0)
			continue;
		vd = &vdsoftc[ctlr];
		s = spl7();
		if (vm->um_tab.b_active && vd->vd_wticks++ >= VDMAXTIME) {
			printf("vd%d: lost interrupt\n", ctlr);
#ifdef maybe
			VDABORT((struct vddevice *)vm->um_addr, vd->vd_type);
#endif
			vdintr(ctlr);
		}
		splx(s);
	}
}

#define	DBSIZE	64	/* controller limit with 1K sectors */
/*
 * Crash dump.
 */
vddump(dev)
	dev_t dev;
{
	register struct vba_device *vi;
	register struct vba_ctlr *vm;
	register struct disklabel *lp;
	register struct vdsoftc *vd;
	struct dksoftc *dk;
	int part, unit, num;
	u_long start;

	start = 0;
	unit = vdunit(dev);
	if (unit > NDK || (vi = vddinfo[unit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	dk = &dksoftc[unit];
	if (dk->dk_state != OPEN && dk->dk_state != OPENRAW &&
	    vdinit(vdminor(unit, 0), 0) != 0)
		return (ENXIO);
	lp = &dklabel[unit];
	part = vdpart(dev);
	if (part >= lp->d_npartitions)
		return (ENXIO);
	vm = vi->ui_mi;
	vdreset_ctlr(vm);
	if (dumplo < 0)
		return (EINVAL);
	/*
	 * Maxfree is in pages, dumplo is in DEV_BSIZE units.
	 */
	num = maxfree * (NBPG / lp->d_secsize);
	dumplo *= DEV_BSIZE / lp->d_secsize;
	if (dumplo + num >= lp->d_partitions[vdpart(dev)].p_size)
		num = lp->d_partitions[vdpart(dev)].p_size - dumplo;
	vd = &vdsoftc[vm->um_ctlr];
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.opcode = VDOP_WD;
	vd->vd_dcb.devselect = dk->dk_dcb.devselect;
	vd->vd_dcb.trailcnt = sizeof (struct trrw) / sizeof (long);
	while (num > 0) {
		int nsec, cn, sn, tn;

		nsec = MIN(num, DBSIZE);
		sn = dumplo + start / lp->d_secsize;
		cn = (sn + lp->d_partitions[vdpart(dev)].p_offset) /
		    lp->d_secpercyl;
		sn %= lp->d_secpercyl;
		tn = sn / lp->d_nsectors;
		sn %= lp->d_nsectors;
		vd->vd_mdcb.mdcb_head = (struct dcb *)vd->vd_dcbphys;
		vd->vd_dcb.trail.rwtrail.memadr = start;
		vd->vd_dcb.trail.rwtrail.wcount = (nsec * lp->d_secsize) >> 1;
		vd->vd_dcb.trail.rwtrail.disk.cylinder = cn;
		vd->vd_dcb.trail.rwtrail.disk.track = tn;
		vd->vd_dcb.trail.rwtrail.disk.sector = sn;
		vd->vd_dcb.operrsta = 0;
		VDGO(vm->um_addr, vd->vd_mdcbphys, vd->vd_type);
		if (!vdpoll(vm, 5)) {
			printf(" during dump\n");
			return (EIO);
		}
		if (vd->vd_dcb.operrsta & VDERR_HARD) {
			printf("dk%d: hard error, status=%b\n", unit,
			    vd->vd_dcb.operrsta, VDERRBITS);
			return (EIO);
		}
		start += nsec * lp->d_secsize;
		num -= nsec;
	}
	return (0);
}

vdsize(dev)
	dev_t dev;
{
	register int unit = vdunit(dev);
	register struct dksoftc *dk;
	struct vba_device *vi;
	struct disklabel *lp;

	if (unit >= NDK || (vi = vddinfo[unit]) == 0 || vi->ui_alive == 0 ||
	    (dk = &dksoftc[unit])->dk_state != OPEN)
		return (-1);
	lp = &dklabel[unit];
#ifdef SECSIZE
	return ((int)lp->d_partitions[vdpart(dev)].p_size);
#else SECSIZE
	return ((int)lp->d_partitions[vdpart(dev)].p_size >> dk->dk_bshift);
#endif SECSIZE
}

/*
 * Initialize controller.
 */
vdinit_ctlr(vm, vd)
	struct vba_ctlr *vm;
	struct vdsoftc *vd;
{
	register struct vddevice *vdaddr = (struct vddevice *)vm->um_addr;

	if (vd->vd_type == VDTYPE_SMDE) {
		vdaddr->vdcsr = 0;
		vdaddr->vdtcf_mdcb = AM_ENPDA;
		vdaddr->vdtcf_dcb = AM_ENPDA;
		vdaddr->vdtcf_trail = AM_ENPDA;
		vdaddr->vdtcf_data = AM_ENPDA;
		vdaddr->vdccf = CCF_SEN | CCF_DIU | CCF_STS | CCF_RFE |
		    XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	if (!vdcmd(vm, VDOP_INIT, 10, 0) || !vdcmd(vm, VDOP_DIAG, 10, 0)) {
		printf("vd%d: %s cmd failed\n", vm->um_ctlr,
		    vd->vd_dcb.opcode == VDOP_INIT ? "init" : "diag");
		return (0);
	}
	vd->vd_secsize = vdaddr->vdsecsize << 1;
	return (1);
}

/*
 * Perform a controller reset.
 */
vdreset_ctlr(vm)
	register struct vba_ctlr *vm;
{
	register struct vddevice *vdaddr = (struct vddevice *)vm->um_addr;
	register struct vdsoftc *vd = &vdsoftc[vm->um_ctlr];
	register int unit;
	struct vba_device *vi;
	
	VDRESET(vdaddr, vd->vd_type);
	if (vdinit_ctlr(vm, vd) == 0)
		return;
	for (unit = 0; unit < NDK; unit++)
		if ((vi = vddinfo[unit])->ui_mi == vm && vi->ui_alive)
			(void) vdreset_drive(vi);
}

vdreset_drive(vi)
	register struct vba_device *vi;
{
	register struct disklabel *lp = &dklabel[vi->ui_unit];
	struct vba_ctlr *vm = vdminfo[vi->ui_ctlr];
	struct vddevice *vdaddr = (struct vddevice *)vm->um_addr;
	register struct vdsoftc *vd = &vdsoftc[vi->ui_ctlr];
	register struct dksoftc *dk = &dksoftc[vi->ui_unit];
	int config_status, config_ecode, saw_drive = 0;

#ifdef notdef
	/*
	 * check for ESDI distribution panel already configured,
	 * e.g. on boot drive, or if PROBE on controller actually
	 * worked.  Status will be zero if drive hasn't
	 * been probed yet.
	 */
#if STA_ESDI != 0
	if ((vdaddr->vdstatus[vi->ui_slave] & STA_TYPE) == STA_ESDI)
		lp->d_devflags |= VD_ESDI;
#endif
#endif
top:
	vd->vd_dcb.opcode = VDOP_CONFIG;		/* command */
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.devselect = vi->ui_slave | lp->d_devflags;
	vd->vd_dcb.trail.rstrail.ncyl = lp->d_ncylinders;
	vd->vd_dcb.trail.rstrail.nsurfaces = lp->d_ntracks;
	if (vd->vd_type == VDTYPE_SMDE) {
		vd->vd_dcb.trailcnt = sizeof (struct treset) / sizeof (long);
		vd->vd_dcb.trail.rstrail.nsectors = lp->d_nsectors;
		vd->vd_dcb.trail.rstrail.slip_sec = lp->d_sparespertrack;
		vd->vd_dcb.trail.rstrail.recovery =
		    (lp->d_flags & D_REMOVABLE) ? VDRF_NORMAL :
		    (VDRF_NORMAL &~ (VDRF_OSP|VDRF_OSM));
	} else
		vd->vd_dcb.trailcnt = 2;		/* XXX */
	vd->vd_mdcb.mdcb_head = (struct dcb *)vd->vd_dcbphys;
	vd->vd_mdcb.mdcb_status = 0;
	VDGO(vdaddr, vd->vd_mdcbphys, vd->vd_type);
	if (!vdpoll(vm, 5)) {
		printf(" during config\n");
		return (0);
	}
	config_status = vd->vd_dcb.operrsta;
	config_ecode = (u_char)vd->vd_dcb.err_code;
	if (config_status & VDERR_HARD) {
		if (vd->vd_type == VDTYPE_SMDE) {
			/*
			 * If drive status was updated successfully,
			 * STA_US (unit selected) should be set
			 * if the drive is attached and powered up.
			 * (But only if we've guessed right on SMD
			 * vs. ESDI; if that flag is wrong, we won't
			 * see the drive.)  If we don't see STA_US
			 * with either SMD or ESDI set for the unit,
			 * we assume that the drive doesn't exist,
			 * and don't wait for it to spin up.
			 */
			(void) vdcmd(vm, VDOP_STATUS, 5, vi->ui_slave);
			uncache(&vdaddr->vdstatus[vi->ui_slave]);
			if (vdaddr->vdstatus[vi->ui_slave] & STA_US)
				saw_drive = 1;
			else if (lp->d_devflags == 0) {
				lp->d_devflags = VD_ESDI;
				goto top;
			}
		} else
			saw_drive = 1;
		if ((config_status & (DCBS_OCYL|DCBS_NRDY)) == 0)
			printf("dk%d: config error %b ecode %x\n", vi->ui_unit,
			   config_status, VDERRBITS, config_ecode);
		else if ((vd->vd_flags & VD_STARTED) == 0 && saw_drive) {
			int started;

			printf(" starting drives, wait ... ");
			vd->vd_flags |= VD_STARTED;
			started = (vdcmd(vm, VDOP_START, 10) == 1);
			DELAY(62000000);
			printf("done\n");
			lp->d_devflags = 0;
			if (started)
				goto top;
		}
		return (0);
	}
	dk->dk_dcb.devselect |= lp->d_devflags;
	return (1);
}

/*
 * Perform a command w/o trailer.
 */
vdcmd(vm, cmd, t, slave)
	register struct vba_ctlr *vm;
{
	register struct vdsoftc *vd = &vdsoftc[vm->um_ctlr];

	vd->vd_dcb.opcode = cmd;		/* command */
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.devselect = slave;
	vd->vd_dcb.trailcnt = 0;
	vd->vd_mdcb.mdcb_head = (struct dcb *)vd->vd_dcbphys;
	vd->vd_mdcb.mdcb_status = 0;
	VDGO(vm->um_addr, vd->vd_mdcbphys, vd->vd_type);
	if (!vdpoll(vm, t)) {
		printf(" during init\n");
		return (0);
	}
	return ((vd->vd_dcb.operrsta&VDERR_HARD) == 0);
}

/*
 * Poll controller until operation
 * completes or timeout expires.
 */
vdpoll(vm, t)
	register struct vba_ctlr *vm;
	register int t;
{
	register struct vdsoftc *vd = &vdsoftc[vm->um_ctlr];
	register struct vddevice *vdaddr = (struct vddevice *)vm->um_addr;

	t *= 1000;
	for (;;) {
		uncache(&vd->vd_dcb.operrsta);
		if (vd->vd_dcb.operrsta & (DCBS_DONE|DCBS_ABORT))
			break;
		if (--t <= 0) {
			printf("vd%d: controller timeout", vm->um_ctlr);
			VDABORT(vdaddr, vd->vd_type);
			return (0);
		}
		DELAY(1000);
	}
	if (vd->vd_type == VDTYPE_SMDE) {
		do {
			DELAY(50);
			uncache(&vdaddr->vdcsr);
		} while (vdaddr->vdcsr & CS_GO);
	 	DELAY(300);
		uncache(&vd->vd_dcb.err_code);
	}
	DELAY(200);
	uncache(&vd->vd_dcb.operrsta);
	return (1);
}

#ifdef COMPAT_42
struct	vdst {
	int	nsec;		/* sectors/track */
	int	ntrack;		/* tracks/cylinder */
	int	ncyl;		/* cylinders */
	int	secsize;	/* sector size */
	char	*name;		/* type name */
	struct {
		int	off;	/* partition offset in sectors */
		int	size;	/* partition size in sectors */
	} parts[8];
} vdst[] = {
	{ 66, 23, 850, 512, "NEC 800",
		{0,	 1290300},	/* a cyl   0 - 849 */
	},
	{ 64, 20, 842, 512, "2361a",
		{0,	 61440},	/* a cyl   0 - 47 */
		{61440,	 67840},	/* b cyl  48 - 100 */
		{129280, 942080}, 	/* c cyl 101 - 836 */
		{0,      1071360}, 	/* d cyl   0 - 836 */
		{449280, 311040},	/* e cyl 351 - 593 */
		{760320, 311040}, 	/* f cyl 594 - 836 */
		{449280, 622080},	/* g cyl 351 - 836 */
		{129280, 320000}	/* h cyl 101 - 350 */
	},
	{ 48, 24, 711, 512, "xsd",
		{0,	 61056},	/* a cyl   0 - 52 */
		{61056,	 61056},	/* b cyl  53 - 105 */
		{122112, 691200}, 	/* c cyl 106 - 705 */
		{237312, 576000}, 	/* d cyl 206 - 705 */
		{352512, 460800},	/* e cyl 306 - 705 */
		{467712, 345600}, 	/* f cyl 406 - 705 */
		{582912, 230400},	/* g cyl 506 - 705 */
		{698112, 115200}	/* h cyl 606 - 705 */
	},
	{ 44, 20, 842, 512, "eagle",
		{0,	 52800},	/* egl0a cyl   0 - 59 */
		{52800,	 66000},	/* egl0b cyl  60 - 134 */
		{118800, 617760}, 	/* egl0c cyl 135 - 836 */
		{736560, 4400}, 	/* egl0d cyl 837 - 841 */
		{0, 	 736560},	/* egl0e cyl   0 - 836 */
		{0, 	 740960}, 	/* egl0f cyl   0 - 841 */
		{118800, 310640},	/* egl0g cyl 135 - 487 */
		{429440, 307120}	/* egl0h cyl 488 - 836 */
	},
	{ 64, 10, 823, 512, "fuj",
		{0,	 38400},	/* fuj0a cyl   0 - 59 */
		{38400,	 48000},	/* fuj0b cyl  60 - 134 */
		{86400,	 437120}, 	/* fuj0c cyl 135 - 817 */
		{159360, 364160}, 	/* fuj0d cyl 249 - 817 */
		{232320, 291200},	/* fuj0e cyl 363 - 817 */
		{305280, 218240}, 	/* fuj0f cyl 477 - 817 */
		{378240, 145280},	/* fuj0g cyl 591 - 817 */
		{451200, 72320}		/* fug0h cyl 705 - 817 */
	},
	{ 32, 24, 711, 512, "xfd",
		{ 0,	 40704 },	/* a cyl   0 - 52 */
		{ 40704, 40704 },	/* b cyl  53 - 105 */
		{ 81408, 460800 },	/* c cyl 106 - 705 */
		{ 0,	 81408 },	/* d cyl 709 - 710 (a & b) */
		{ 0,	 542208 },	/* e cyl   0 - 705 */
		{ 40704, 501504 },	/* f cyl  53 - 705 (b & c) */
		{ 81408, 230400 },	/* g cyl 106 - 405 (1/2 of c) */
		{ 311808,230400 }	/* h cyl 406 - 705 (1/2 of c) */
	},
	{ 32, 19, 823, 512, "smd",
		{0,	 40128},	/* a cyl   0-65 */
		{40128,  27360},	/* b cyl  66-110 */
		{67488,  429856},	/* c cyl 111-817 */
		{139232, 358112},	/* d cyl 229 - 817 */
		{210976, 286368},	/* e cyl 347 - 817 */
		{282720, 214624},	/* f cyl 465 - 817 */
		{354464, 142880},	/* g cyl 583 - 817 */
		{426208, 71136}		/* h cyl 701 - 817 */
	},
	{ 18, 15, 1224, 1024, "mxd",
		{0,	 21600},	/* a cyl   0-79 */
		{21600,  22410},	/* b cyl  80-162 */
		{44010,  285120},	/* c cyl 163-1217 */
#ifdef notyet
		{x, 237600},	/* d cyl y - 1217 */
		{x, 190080},	/* e cyl y - 1217 */
		{x, 142560},	/* f cyl y - 1217 */
		{x, 95040},	/* g cyl y - 1217 */
		{x, 47520}		/* h cyl 701 - 817 */
#endif
	},
	{ 32, 10, 823, 512, "fsd",
		{0,	 19200},	/* a cyl   0 -  59 */
		{19200,	 24000},	/* b cyl  60 - 134 */
		{43200,	 218560},	/* c cyl 135 - 817 */
	}
};
#define	NVDST	(sizeof (vdst) / sizeof (vdst[0]))

/*
 * Construct a label for an unlabeled pack.  We
 * deduce the drive type by reading from the last
 * track on successively smaller drives until we
 * don't get an error.
 */
vdmaptype(vi, lp)
	register struct vba_device *vi;
	register struct disklabel *lp;
{
	register struct vdsoftc *vd;
	register struct vdst *p;
	struct vba_ctlr *vm = vi->ui_mi;
	int i;

	vd = &vdsoftc[vi->ui_ctlr];
	for (p = vdst; p < &vdst[NVDST]; p++) {
		if (vd->vd_type == VDTYPE_VDDC && p->nsec != 32)
			continue;
		lp->d_nsectors = p->nsec;
		lp->d_ntracks = p->ntrack;
		lp->d_ncylinders = p->ncyl;
		lp->d_secsize = p->secsize;
		DELAY(100000);
		if (!vdreset_drive(vi))
			return (0);
		DELAY(100000);
		vd->vd_dcb.opcode = VDOP_RD;
		vd->vd_dcb.intflg = DCBINT_NONE;
		vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
		vd->vd_dcb.devselect = dksoftc[vi->ui_unit].dk_dcb.devselect;
		vd->vd_dcb.trailcnt = sizeof (struct trrw) / sizeof (long);
		vd->vd_dcb.trail.rwtrail.memadr =
		    vtoph((struct proc *)0, (unsigned)vd->vd_rbuf.vb_rawbuf);
		vd->vd_dcb.trail.rwtrail.wcount = lp->d_secsize / sizeof(short);
		vd->vd_dcb.operrsta = 0;
		vd->vd_dcb.trail.rwtrail.disk.cylinder = p->ncyl - 2;
		vd->vd_dcb.trail.rwtrail.disk.track = p->ntrack - 1;
		vd->vd_dcb.trail.rwtrail.disk.sector = p->nsec - 1;
		vd->vd_mdcb.mdcb_head = (struct dcb *)vd->vd_dcbphys;
		vd->vd_mdcb.mdcb_status = 0;
		VDGO(vm->um_addr, vd->vd_mdcbphys, vd->vd_type);
		if (!vdpoll(vm, 60))
			printf(" during probe\n");
		if ((vd->vd_dcb.operrsta & VDERR_HARD) == 0)
			break;
	}
	if (p >= &vdst[NVDST])
		return (0);

	for (i = 0; i < 8; i++) {
		lp->d_partitions[i].p_offset = p->parts[i].off;
		lp->d_partitions[i].p_size = p->parts[i].size;
	}
	lp->d_npartitions = 8;
	lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
	bcopy(p->name, lp->d_typename, 4);
	return (1);
}
#endif COMPAT_42
#endif
