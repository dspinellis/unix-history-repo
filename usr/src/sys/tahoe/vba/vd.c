/*	vd.c	1.15	87/02/28	*/

#include "dk.h"
#if NVD > 0
/*
 * Versabus VDDC/SMDE driver.
 */
#include "param.h"
#include "buf.h"
#include "cmap.h"
#include "conf.h"
#include "dir.h"
#include "dkstat.h"
#include "disklabel.h"
#include "map.h"
#include "file.h"
#include "systm.h"
#include "user.h"
#include "vmmac.h"
#include "proc.h"
#include "uio.h"
#include "syslog.h"
#include "kernel.h"
#include "ioctl.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/vdreg.h"

#define	COMPAT_42

#define	VDMAXIO		(MAXBPTE*NBPG)

#define vdunit(dev)	(minor(dev) >> 3)
#define vdpart(dev)	(minor(dev) & 0x07)
#define	vdminor(unit,part)	(((unit) << 3) | (part))

struct	vba_ctlr *vdminfo[NVD];
struct  vba_device *vddinfo[NDK];
int	vdprobe(), vdslave(), vdattach(), vddgo();
long	vdaddr[] = { 0xffff2000, 0xffff2100, 0xffff2200, 0xffff2300, 0 };
struct	vba_driver vddriver =
  { vdprobe, vdslave, vdattach, vddgo, vdaddr, "dk", vddinfo, "vd", vdminfo };

/*
 * Per-controller state.
 */
struct vdsoftc {
	u_short	vd_flags;
#define	VD_INIT		0x1	/* controller initialized */
#define	VD_STARTED	0x2	/* start command issued */
#define	VD_DOSEEKS	0x4	/* should overlap seeks */
	u_short	vd_type;	/* controller type */
	u_short	vd_wticks;	/* timeout */
	u_short	vd_offcyl;	/* off cylinder bitmask */
	struct	mdcb vd_mdcb;	/* master command block */
	u_long	vd_mdcbphys;	/* physical address of vd_mdcb */
	struct	dcb vd_dcb;	/* i/o command block */
	u_long	vd_dcbphys;	/* physical address of vd_dcb */
	struct	pte *vd_map;	/* i/o page map */
	caddr_t	vd_utl;		/* mapped i/o space */
	caddr_t	vd_rawbuf;	/* buffer for raw+swap i/o */
} vdsoftc[NVD];

/*
 * Per-drive state.
 */
struct	dksoftc {
	u_short	dk_state;	/* open fsm */
	u_short	dk_openpart;	/* units open on this drive */
	u_short	dk_curdaddr;	/* last selected track & sector */
	u_int	dk_curcyl;	/* last selected cylinder */
	struct	dcb dk_dcb;	/* seek command block */
	u_long	dk_dcbphys;	/* physical address of dk_dcb */
} dksoftc[NDK];

/*
 * Drive states.  Used during steps of open/initialization.
 * States < OPEN (> 0) are transient, during an open operation.
 * OPENRAW is used for unabeled disks, to allow format operations.
 */
#define	CLOSED		0		/* disk is closed */
#define	WANTOPEN	1		/* open requested, not started */
#define	WANTOPENRAW	2		/* open requested, no label */
#define	RDLABEL		3		/* reading pack label */
#define	OPEN		4		/* intialized and ready */
#define	OPENRAW		5		/* open, no label */

struct	buf rdkbuf[NDK];	/* raw i/o buffer headers */
struct	buf dkutab[NDK];	/* i/o queue headers */
struct	disklabel dklabel[NDK];	/* pack labels */

#define b_cylin	b_resid
#define	b_daddr	b_error

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
		vdaddr->vdcsr = 0;
		vdaddr->vdtcf_mdcb = AM_ENPDA;
		vdaddr->vdtcf_dcb = AM_ENPDA;
		vdaddr->vdtcf_trail = AM_ENPDA;
		vdaddr->vdtcf_data = AM_ENPDA;
		vdaddr->vdccf = CCF_SEN | CCF_DER | CCF_STS |
		    XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	vd->vd_mdcbphys = vtoph((struct proc *)0, (unsigned)&vd->vd_mdcb);
	vd->vd_dcbphys = vtoph((struct proc *)0, (unsigned)&vd->vd_dcb);
	vm->um_addr = reg;		/* XXX */
	s = spl7();
	if (!vdcmd(vm, VDOP_INIT, 10) || !vdcmd(vm, VDOP_DIAG, 10)) {
		printf("vd%d: %s cmd failed\n", vm->um_ctlr,
		    vd->vd_dcb.opcode == VDOP_INIT ? "init" : "diag");
		splx(s);
		return (0);
	}
	splx(s);
	/*
	 * Allocate page tables and i/o buffer.
	 */
	vbmapalloc(btoc(VDMAXIO)+1, &vd->vd_map, &vd->vd_utl);
	vd->vd_rawbuf = calloc(VDMAXIO);
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
vdslave(vi, addr)
	register struct vba_device *vi;
	struct vddevice *vdaddr;
{
	register struct disklabel *lp = &dklabel[vi->ui_unit];
	struct vdsoftc *vd = &vdsoftc[vi->ui_ctlr];

	if ((vd->vd_flags&VD_INIT) == 0) {
		printf("vd%d: %s controller\n", vi->ui_ctlr,
		    vd->vd_type == VDTYPE_VDDC ? "VDDC" : "SMDE");
		vd->vd_flags |= VD_INIT;
	}

	/*
	 * Initialize label enough to do a reset on
	 * the drive.  The remainder of the default
	 * label values will be filled in in vdinit
	 * at attach time.
	 */
	lp->d_secsize = DEV_BSIZE / 2;		/* XXX */
	lp->d_nsectors = 32;
	lp->d_ntracks = 24;
	lp->d_ncylinders = 711;
	lp->d_secpercyl = 32*24;
	return (vdreset_drive(vi));
}

/*
 * Read pack label.
 */
vdattach(vi)
	register struct vba_device *vi;
{
	register int unit = vi->ui_unit;
	register struct dksoftc *dk = &dksoftc[unit];
	register struct disklabel *lp;

	/*
	 * Try to initialize device and read pack label.
	 */
	if (vdinit(vdminor(unit, 0), 0) != 0) {
		printf(": unknown drive type");
		return;
	}
	/*
	 * Initialize invariant portion of
	 * dcb used for overlapped seeks.
	 */
	dk->dk_dcb.opcode = VDOP_SEEK;
	dk->dk_dcb.intflg = DCBINT_NONE | DCBINT_PBA;
	dk->dk_dcb.devselect = vi->ui_slave;
	dk->dk_dcb.trailcnt = sizeof (trseek) / sizeof (long);
	dk->dk_dcb.trail.sktrail.skaddr.sector = 0;
	dk->dk_dcbphys = vtoph((struct proc *)0, (unsigned)&dk->dk_dcb);
	lp = &dklabel[unit];
	printf(": %s <ntrak %d, ncyl %d, nsec %d>",
	    lp->d_typename, lp->d_ntracks, lp->d_ncylinders, lp->d_nsectors);
	/*
	 * (60 / rpm) / (sectors per track * (bytes per sector / 2))
	 */
	if (vi->ui_dk >= 0)
		dk_mspw[vi->ui_dk] = 120.0 /
		    (lp->d_rpm * lp->d_nsectors * lp->d_secsize);
#ifdef notyet
	addswap(makedev(VDMAJOR, vdminor(unit, 0)), lp);
#endif
}

/*ARGSUSED*/
vdopen(dev, flags)
	dev_t dev;
	int flags;
{
	register unit = vdunit(dev);
	register struct disklabel *lp;
	register struct dksoftc *dk;
	register struct partition *pp;
	struct vba_device *vi;
	int s, error, part = vdpart(dev);
	daddr_t start, end;

	if (unit >= NDK || (vi = vddinfo[unit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	lp = &dklabel[unit];
	dk = &dksoftc[unit];

	s = spl7();
	while (dk->dk_state != OPEN && dk->dk_state != OPENRAW &&
	    dk->dk_state != CLOSED)
		sleep((caddr_t)dk, PZERO+1);
	splx(s);
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
#define	RAWPART		2		/* 'c' partition */	/* XXX */
	if ((dk->dk_openpart & (1 << part)) == 0 &&
	    part != RAWPART) {
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
	dk->dk_openpart |= 1 << part;
	return (0);
}

vdclose(dev, flags)
	dev_t dev;
	int flags;
{
	register int unit = vdunit(dev);
	register struct dksoftc *dk = &dksoftc[unit];

	dk->dk_openpart &= ~(1 << vdpart(dev));
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
	}
}

vdinit(dev, flags)
	dev_t dev;
	int flags;
{
	register struct buf *bp = NULL;
	register struct disklabel *lp;
	register struct dksoftc *dk;
	struct vba_device *vi;
	struct disklabel *dlp;
	int unit = vdunit(dev), error = 0;
	extern int cold;

	dk = &dksoftc[unit];
	if (flags & O_NDELAY) {
		dk->dk_state = OPENRAW;
		goto done;
	}

	/*
	 * Initialize portion of the label
	 * not set up in the slave routine.
	 */
	dk->dk_state = RDLABEL;
	lp = &dklabel[unit];
	lp->d_secperunit = 0x1fffffff;
	lp->d_npartitions = 1;
	lp->d_partitions[0].p_size = 0x1fffffff;
	lp->d_partitions[0].p_offset = 0;

	bp = geteblk(DEV_BSIZE);		/* max sector size */
	bp->b_dev = dev;
	bp->b_blkno = LABELSECTOR;
	bp->b_bcount = DEV_BSIZE;
	bp->b_flags = B_BUSY | B_READ;
	bp->b_cylin = LABELSECTOR / lp->d_secpercyl;
	vdstrategy(bp);
	biowait(bp);
	if (bp->b_flags & B_ERROR) {
		error = u.u_error;		/* XXX */
		u.u_error = 0;
		dk->dk_state = CLOSED;
		goto done;
	}
	vi = vddinfo[unit];
	dlp = (struct disklabel *)(bp->b_un.b_addr + LABELOFFSET);
	if (dlp->d_magic == DISKMAGIC && dlp->d_magic2 == DISKMAGIC &&
	    dkcksum(dlp) == 0) {
		*lp = *dlp;
		/*
		 * Now that we have the label, configure
		 * the correct drive parameters.
		 */
		if (!vdreset_drive(vi))
			dk->dk_state = CLOSED;
		else
			dk->dk_state = OPEN;
	} else {
		if (cold)
			printf(": no disk label");
		else
			log(LOG_ERR, "dk%d: no disk label\n", vi->ui_unit);
#ifdef COMPAT_42
		if (!vdmaptype(vi, lp)) {
			error = ENXIO;
			dk->dk_state = CLOSED;
		} else
			dk->dk_state = OPEN;
#else
		dk->dk_state = OPENRAW;
#endif
	}
done:
	if (bp) {
		bp->b_flags = B_INVAL | B_AGE;
		brelse(bp);
	}
	wakeup((caddr_t)dk);
	return (error);
}

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

	sz = bp->b_bcount;
	sz = (sz + DEV_BSIZE - 1) >> DEV_BSHIFT;
	unit = vdunit(bp->b_dev);
	if (unit > NDK) {
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
	if (dk->dk_state < OPEN)
		goto q;
	part = vdpart(bp->b_dev);
	if ((dk->dk_openpart & (1 << part)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}
	maxsz = lp->d_partitions[part].p_size;
	sn = bp->b_blkno;
	if (sn < 0 || sn + sz > maxsz) {
		if (sn == maxsz) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		sz = maxsz - bp->b_blkno;
		if (sz <= 0) {
			bp->b_error = EINVAL;
			goto bad;
		}
		bp->b_bcount = sz * lp->d_secsize;
	}
	bp->b_cylin = (sn + lp->d_partitions[part].p_offset) / lp->d_secpercyl;
q:
	vbasetup(bp, lp->d_secsize);
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
	 * If drive is off-cylinder, mark unit to force
	 * overlap seek with next transfer on this controller.
	 */
	vd = &vdsoftc[vi->ui_ctlr];
	dk = &dksoftc[unit];
	if (bp->b_cylin != dk->dk_curcyl && vd->vd_flags&VD_DOSEEKS) {
		lp = &dklabel[unit];
		bp->b_daddr = (bp->b_blkno % lp->d_secpercyl) / lp->d_nsectors;
		if (bp->b_daddr != dk->dk_curdaddr)
			vd->vd_offcyl |= 1 << vi->ui_slave;
	}
	/*
	 * If controller is not busy, place request on the
	 * controller's ready queue).
	 */
	dp->b_forw = NULL;
	vm = vi->ui_mi;
	if (vm->um_tab.b_actf == NULL)
		vm->um_tab.b_actf = dp;
	else
		vm->um_tab.b_actl->b_forw = dp;
	vm->um_tab.b_actl = dp;
	dp->b_active++;
}

/*
 * Start next transfer on a controller.
 */
vdstart(vm)
	register struct vba_ctlr *vm;
{
	register struct buf *bp;
	register struct vba_device *vi;
	register struct vdsoftc *vd;
	register struct dksoftc *dk;
	register struct disklabel *lp;
	register int slave;
	register struct dcb **dcbp;
	struct mdcb *mdcb;
	struct buf *dp;
	int sn, tn;

loop:
	/*
	 * Pull a request off the controller queue.
	 */
	if ((dp = vm->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		vm->um_tab.b_actf = dp->b_forw;
		goto loop;
	}

	/*
	 * Mark controller busy, and determine
	 * destination of this request.
	 */
	vm->um_tab.b_active++;
	vi = vddinfo[vdunit(bp->b_dev)];
	dk = &dksoftc[vi->ui_unit];
	sn = bp->b_blkno;
	lp = &dklabel[vi->ui_unit];
	sn %= lp->d_secpercyl;
	tn = sn / lp->d_nsectors;
	sn %= lp->d_nsectors;

	/*
	 * Construct dcb for read/write command.
	 */
	vd = &vdsoftc[vm->um_ctlr];
	slave = vi->ui_slave;
	vd->vd_dcb.opcode = (bp->b_flags & B_READ) ? VDOP_RD : VDOP_WD;
	vd->vd_dcb.intflg = DCBINT_DONE;
	vd->vd_dcb.devselect = slave;
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.trailcnt = sizeof (trrw) / sizeof (long);
	vd->vd_dcb.trail.rwtrail.memadr = (char *)
	    vbastart(bp, vd->vd_rawbuf, (long *)vd->vd_map, vd->vd_utl);
	vd->vd_dcb.trail.rwtrail.wcount = (bp->b_bcount+1) >> 1;
	vd->vd_dcb.trail.rwtrail.disk.cylinder = bp->b_cylin;
	vd->vd_dcb.trail.rwtrail.disk.track = tn;
	vd->vd_dcb.trail.rwtrail.disk.sector = sn;

	/*
	 * Look for any seeks to be performed on other drives on this
	 * controller.  If overlapped seeks exist, insert seek commands
	 * on the controller's command queue before the transfer.
	 */
	dcbp = &vd->vd_mdcb.mdcb_head;
	if (vd->vd_offcyl &~ (1<<slave)) {
		register struct dksoftc *tdk;
		register struct buf *tp;

		for (dp = dp->b_forw; dp != NULL; dp = dp->b_forw) {
			if ((tp = dp->b_actf) == NULL)
				continue;
			slave = (vi = vddinfo[vdunit(tp->b_dev)])->ui_slave;
			if ((vd->vd_offcyl & (1<<slave)) == 0)
				continue;
			vd->vd_offcyl &= ~(1 << slave);
			tdk = &dksoftc[vi->ui_unit];
			if (tdk->dk_curcyl != tp->b_cylin) {
				tdk->dk_curcyl = tp->b_cylin;
				dk_seek[vi->ui_dk]++;
			}
			tdk->dk_curdaddr = tp->b_daddr;
			tdk->dk_dcb.operrsta = 0;
			tdk->dk_dcb.trail.sktrail.skaddr.cylinder = tp->b_cylin;
			tdk->dk_dcb.trail.sktrail.skaddr.track = tp->b_daddr>>8;
			tdk->dk_dcb.trail.sktrail.skaddr.sector =
			    tp->b_daddr & 0xff;
			*dcbp = (struct dcb *)tdk->dk_dcbphys;
			dcbp = &tdk->dk_dcb.nxtdcb;
		}
	} else {
		dk->dk_curcyl = bp->b_cylin;
		dk->dk_curdaddr = (tn << 8) | sn;
		vd->vd_offcyl = 0;
	}
	*dcbp = (struct dcb *)vd->vd_dcbphys;

	/*
	 * Initiate operation.
	 */
	bp->b_daddr = 0;		/* init overloaded field */
	if (vi->ui_dk >= 0) {
		dk_busy |= 1<<vi->ui_dk;
		dk_xfer[vi->ui_dk]++;
		dk_wds[vi->ui_dk] += bp->b_bcount>>6;
	}
	vd->vd_mdcb.mdcb_status = 0;
	VDGO(vm->um_addr, vd->vd_mdcbphys, vd->vd_type);
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
	struct dksoftc *dk;

	vd->vd_wticks = 0;
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
	dk_busy &= ~(1<<vi->ui_dk);
	/*
	 * Check for and process errors on
	 * either the drive or the controller.
	 */
	uncache(&vd->vd_dcb.operrsta);
	status = vd->vd_dcb.operrsta;
	if (status & VDERR_HARD) {
		if (status & DCBS_WPT) {
			/*
			 * Give up on write locked devices immediately.
			 */
			printf("dk%d: write locked\n", vi->ui_unit);
			bp->b_flags |= B_ERROR;
		} else if (status & VDERR_RETRY) {
			if (status & VDERR_DRIVE) {
				if (!vdreset_drive(vi))
					vi->ui_alive = 0;
			} else if (status & VDERR_CTLR)
				vdreset_ctlr(vm);
			/*
			 * Retry transfer once, unless reset failed.
			 */
			if (!vi->ui_alive || bp->b_errcnt++ >= 2)
				goto hard;
			vm->um_tab.b_active = 0;	/* force retry */
		} else  {
	hard:
			bp->b_flags |= B_ERROR;
			/* NEED TO ADJUST b_blkno to failed sector */
			harderr(bp, "dk");
			printf("status %x (%b)", status,
			   status &~ DONTCARE, VDERRBITS);
			if (vd->vd_type == VDTYPE_SMDE) {
				uncache(&vd->vd_dcb.err_code);
				printf(" ecode %x", vd->vd_dcb.err_code);
			}
			printf("\n");
		}
	} else if (status & DCBS_SOFT)
		vdsofterr(vd, bp, &vd->vd_dcb);
	if (vm->um_tab.b_active) {
		vm->um_tab.b_active = 0;
		vm->um_tab.b_errcnt = 0;
		vm->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_resid = 0;
		vbadone(bp, vd->vd_rawbuf, (long *)vd->vd_map, vd->vd_utl);
		biodone(bp);
		/*
		 * If this unit has more work to do,
		 * then start it up right away.
		 */
		if (dp->b_actf)
			vdustart(vi);
		else if ((dk = &dksoftc[vi->ui_unit])->dk_openpart == 0)
			wakeup((caddr_t)dk);
	}
	/*
	 * If there are devices ready to
	 * transfer, start the controller.
	 */
	if (vm->um_tab.b_actf)
		vdstart(vm);
}

vdsofterr(vd, bp, dcb)
	struct vdsoftc *vd;
	register struct buf *bp;
	register struct dcb *dcb;
{
	int unit = vdunit(bp->b_dev), status = dcb->operrsta;
	char part = 'a' + vdpart(bp->b_dev);

	if (status != (DCBS_DCE|DCBS_CCD|DCBS_SOFT|DCBS_ERR)) {
		if (vd->vd_type == VDTYPE_SMDE)
			uncache(&dcb->err_code);
		log(LOG_WARNING, "dk%d%c: soft error sn%d status %b ecode %x\n",
		    unit, part, bp->b_blkno, status, VDERRBITS, dcb->err_code);
	} else
		log(LOG_WARNING, "dk%d%c: soft ecc sn%d\n",
		    unit, part, bp->b_blkno);
}

vdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = vdunit(dev);

	if (unit >= NDK)
		return (ENXIO);
	return (physio(vdstrategy, &rdkbuf[unit], dev, B_READ, minphys, uio));
}

vdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = vdunit(dev);

	if (unit >= NDK)
		return (ENXIO);
	return (physio(vdstrategy, &rdkbuf[unit], dev, B_WRITE, minphys, uio));
}

vdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	int unit = vdunit(dev);
	register struct disklabel *lp = &dklabel[unit];
	int error = 0;

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
			*lp = *(struct disklabel *)data;
		break;

	case DIOCWDINFO: {
		struct buf *bp;
		struct disklabel *dlp;

		if ((flag & FWRITE) == 0) {
			error = EBADF;
			break;
		}
		*lp = *(struct disklabel *)data;
		bp = geteblk(lp->d_secsize);
		bp->b_dev = dev;
		bp->b_blkno = LABELSECTOR;
		bp->b_bcount = lp->d_secsize;
		bp->b_flags = B_READ;
		dlp = (struct disklabel *)(bp->b_un.b_addr + LABELOFFSET);
		vdstrategy(bp);
		biowait(bp);
		if (bp->b_flags & B_ERROR) {
			error = u.u_error;		/* XXX */
			u.u_error = 0;
			goto bad;
		}
		*dlp = *lp;
		bp->b_flags = B_WRITE;
		vdstrategy(bp);
		biowait(bp);
		if (bp->b_flags & B_ERROR) {
			error = u.u_error;		/* XXX */
			u.u_error = 0;
		}
bad:
		brelse(bp);
		break;
	}

	default:
		error = ENOTTY;
		break;
	}
	return (0);
}

/*
 * Watch for lost interrupts.
 */
vdwatch()
{
	register struct vdsoftc *vd;
	register struct vba_ctlr *vm;
	register int ctlr, unit;

	timeout(vdwatch, (caddr_t)0, hz);
	for (ctlr = 0; ctlr < NVD; ctlr++) {
		vm = vdminfo[ctlr];
		if (vm == 0 || vm->um_alive == 0)
			continue;
		vd = &vdsoftc[ctlr];
		if (!vm->um_tab.b_active) {
			for (unit = 0; unit < NDK; unit++)
				if (dkutab[unit].b_active &&
				    vddinfo[unit]->ui_mi == vm)
					goto active;
			vd->vd_wticks = 0;
			continue;
		}
active:
		vd->vd_wticks++;
		if (vd->vd_wticks >= 20) {
			vd->vd_wticks = 0;
			printf("vd%d: lost interrupt\n", ctlr);
			/* abort pending dcb's and restart controller */
		}
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
	caddr_t start;

	start = 0;
	unit = vdunit(dev);
	if (unit > NDK || (vi = vddinfo[unit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	dk = &dksoftc[unit];
	if (dk->dk_state != OPEN && dk->dk_state != OPENRAW)
		return (ENXIO);
	lp = &dklabel[unit];
	part = vdpart(dev);
	if (part >= lp->d_npartitions)
		return (ENXIO);
	vm = vdminfo[vi->ui_ctlr];
	vdreset_ctlr(vm);
	if (dumplo < 0)
		return (EINVAL);
	/*
	 * Dumplo and maxfree are in pages.
	 */
	num = maxfree * (NBPG / lp->d_secsize);
	dumplo *= NBPG / lp->d_secsize;
	if (dumplo + num >= lp->d_partitions[vdpart(dev)].p_size)
		num = lp->d_partitions[vdpart(dev)].p_size - dumplo;
	vd = &vdsoftc[vm->um_ctlr];
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.opcode = VDOP_WD;
	vd->vd_dcb.devselect = vi->ui_slave;
	vd->vd_dcb.trailcnt = sizeof (trrw) / sizeof (long);
	while (num > 0) {
		int nsec, cn, sn, tn;

		nsec = MIN(num, DBSIZE);
		sn = dumplo + (unsigned)start / lp->d_secsize;
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
	return ((int)lp->d_partitions[vdpart(dev)].p_size);
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
	if (vd->vd_type == VDTYPE_SMDE) {
		vdaddr->vdcsr = 0;
		vdaddr->vdtcf_mdcb = AM_ENPDA;
		vdaddr->vdtcf_dcb = AM_ENPDA;
		vdaddr->vdtcf_trail = AM_ENPDA;
		vdaddr->vdtcf_data = AM_ENPDA;
		vdaddr->vdccf = CCF_STS | XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	if (!vdcmd(vm, VDOP_INIT, 10) || !vdcmd(vm, VDOP_DIAG, 10)) {
		printf("%s cmd failed\n",
		    vd->vd_dcb.opcode == VDOP_INIT ? "init" : "diag");
		return;
	}
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
	struct vdsoftc *vd = &vdsoftc[vi->ui_ctlr];

top:
	vd->vd_dcb.opcode = VDOP_CONFIG;		/* command */
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.devselect = vi->ui_slave;
	vd->vd_dcb.trail.rstrail.ncyl = lp->d_ncylinders;
	vd->vd_dcb.trail.rstrail.nsurfaces = lp->d_ntracks;
	if (vd->vd_type == VDTYPE_SMDE) {
		vd->vd_dcb.trailcnt = sizeof (treset) / sizeof (long);
		vd->vd_dcb.trail.rstrail.nsectors = lp->d_nsectors;
		vd->vd_dcb.trail.rstrail.slip_sec = lp->d_trackskew;
		vd->vd_dcb.trail.rstrail.recovery = 0x18f;
	} else
		vd->vd_dcb.trailcnt = 2;		/* XXX */
	vd->vd_mdcb.mdcb_head = (struct dcb *)vd->vd_dcbphys;
	vd->vd_mdcb.mdcb_status = 0;
	VDGO(vdaddr, vd->vd_mdcbphys, vd->vd_type);
	if (!vdpoll(vm, 5)) {
		printf(" during config\n");
		return (0);
	}
	if (vd->vd_dcb.operrsta & VDERR_HARD) {
		if (vd->vd_type == VDTYPE_SMDE &&
		    (vdaddr->vdstatus[vi->ui_slave]&STA_US) == 0)
			return (0);
		if ((vd->vd_dcb.operrsta & (DCBS_OCYL|DCBS_NRDY)) == 0)
			printf("dk%d: config error\n", vi->ui_unit);
		else if ((vd->vd_flags&VD_STARTED) == 0) {
			int started;

			printf("vd%d: starting drives, wait ... ", vm->um_ctlr);
			vd->vd_flags |= VD_STARTED;
			started = (vdcmd(vm, VDOP_START, 10) == 1);
			DELAY(62000000);
			printf("\n");
			if (started)
				goto top;
		}
		return (0);
	}
	return (1);
}

/*
 * Perform a command w/o trailer.
 */
vdcmd(vm, cmd, t)
	register struct vba_ctlr *vm;
{
	register struct vdsoftc *vd = &vdsoftc[vm->um_ctlr];

	vd->vd_dcb.opcode = cmd;		/* command */
	vd->vd_dcb.intflg = DCBINT_NONE;
	vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	vd->vd_dcb.operrsta = 0;
	vd->vd_dcb.devselect = 0;
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
			DELAY(30000);
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
	char	*name;		/* type name */
	struct {
		int	off;	/* partition offset in sectors */
		int	size;	/* partition size in sectors */
	} parts[8];
} vdst[] = {
	{ 48, 24, 711, "xsd",
		{0,	 30528},	/* a cyl   0 - 52 */
		{30528,	 30528},	/* b cyl  53 - 105 */
		{61056,	 345600}, 	/* c cyl 106 - 705 */
		{118656, 288000}, 	/* d cyl 206 - 705 */
		{176256, 230400},	/* e cyl 306 - 705 */
		{233856, 172800}, 	/* f cyl 406 - 705 */
		{291456, 115200},	/* g cyl 506 - 705 */
		{349056, 57600}		/* h cyl 606 - 705 */
	},
	{ 44, 20, 842, "egl",
		{0,	 26400},	/* egl0a cyl   0 - 59 */
		{26400,	 33000},	/* egl0b cyl  60 - 134 */
		{59400,	 308880}, 	/* egl0c cyl 135 - 836 */
		{368280, 2640}, 	/* egl0d cyl 837 - 842 */
		{0, 368280},		/* egl0e cyl 0 - 836 */
		{0, 370920}, 		/* egl0f cyl 0 - 842 */
		{59400, 155320},	/* egl0g cyl 135 - 487 */
		{214720, 153560}	/* egl0h cyl 488 - 836 */
	},
	{ 64, 10, 823, "fuj",
		{0,	 19200},	/* fuj0a cyl   0 - 59 */
		{19200,	 24000},	/* fuj0b cyl  60 - 134 */
		{43200,	 218560}, 	/* fuj0c cyl 135 - 817 */
		{79680,	 182080}, 	/* fuj0d cyl 249 - 817 */
		{116160, 145600},	/* fuj0e cyl 363 - 817 */
		{152640, 109120}, 	/* fuj0f cyl 477 - 817 */
		{189120, 72640},	/* fuj0g cyl 591 - 817 */
		{225600, 36160}		/* fug0h cyl 705 - 817 */
	},
	{ 32, 24, 711, "xfd",
		{ 0,	 20352 },	/* a cyl   0 - 52 */
		{ 20352, 20352 },	/* b cyl  53 - 105 */
		{ 40704, 230400 },	/* c cyl 106 - 705 */
		{ 0,	 40704 },	/* d cyl 709 - 710 (a & b) */
		{ 0,	 271104 },	/* e cyl   0 - 705 */
		{ 20352, 250752 },	/* f cyl  53 - 705 (b & c) */
		{ 40704, 115200 },	/* g cyl 106 - 405 (1/2 of c) */
		{ 155904,115200 }	/* h cyl 406 - 705 (1/2 of c) */
	},
	{ 32, 19, 823, "smd",
		{0,	 20064},	/* a cyl   0-65 */
		{20064, 13680},		/* b cyl  66-110 */
		{33744, 214928},	/* c cyl 111-817 */
		{69616,	 179056},	/* d cyl 229 - 817 */
		{105488, 143184},	/* e cyl 347 - 817 */
		{141360, 107312},	/* f cyl 465 - 817 */
		{177232, 71440},	/* g cyl 583 - 817 */
		{213104, 35568}		/* h cyl 701 - 817 */
	},
	{ 32, 10, 823, "fsd",
		{0,	 9600},		/* a cyl   0 -  59 */
		{9600,	 12000},	/* b cyl  60 - 134 */
		{21600,	 109280},	/* c cyl 135 - 817 */
		{39840,	 91040},	/* d cyl 249 - 817 */
		{58080,	 72800},	/* e cyl 363 - 817 */
		{76320,	 54560},	/* f cyl 477 - 817 */
		{94560,  36320},	/* g cyl 591 - 817 */
		{112800, 18080}		/* h cyl 705 - 817 */
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
	struct vba_ctlr *vm = vdminfo[vi->ui_ctlr];
	int i;

	vd = &vdsoftc[vi->ui_ctlr];
	for (p = vdst; p < &vdst[NVDST]; p++) {
		if (vd->vd_type == VDTYPE_VDDC && p->nsec != 32)
			continue;
		lp->d_nsectors = p->nsec;
		lp->d_ntracks = p->ntrack;
		lp->d_ncylinders = p->ncyl;
		if (!vdreset_drive(vi))
			return (0);
		vd->vd_dcb.opcode = VDOP_RD;
		vd->vd_dcb.intflg = DCBINT_NONE;
		vd->vd_dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
		vd->vd_dcb.devselect = vi->ui_slave;
		vd->vd_dcb.trailcnt = sizeof (trrw) / sizeof (long);
		vd->vd_dcb.trail.rwtrail.memadr = (char *)
		    vtoph((struct proc *)0, (unsigned)vd->vd_rawbuf);
		vd->vd_dcb.trail.rwtrail.wcount = 512 / sizeof(short);
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
	if (p >= &vdst[NVDST]) {
		printf("dk%d: unknown drive type\n", vi->ui_unit);
		return (0);
	}
	for (i = 0; i < 8; i++) {
		lp->d_partitions[i].p_offset = p->parts[i].off;
		lp->d_partitions[i].p_size = p->parts[i].size;
	}
	lp->d_npartitions = 8;
	lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
	lp->d_rpm = 3600;
	lp->d_secsize = 512;
	bcopy(p->name, lp->d_typename, 4);
	return (1);
}
#endif COMPAT_42
#endif
