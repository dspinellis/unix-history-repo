/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Harris Corp.
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
 *	@(#)hd.c	7.11 (Berkeley) 6/28/90
 */

#include "hd.h"

#if NHD > 0
#include "param.h"
#include "buf.h"
#include "conf.h"
#include "dkstat.h"
#include "disklabel.h"
#include "file.h"
#include "systm.h"
#include "vmmac.h"
#include "time.h"
#include "proc.h"
#include "uio.h"
#include "syslog.h"
#include "kernel.h"
#include "ioctl.h"
#include "stat.h"
#include "errno.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/hdreg.h"

#define	b_cylin	b_resid

#define	hdunit(dev)		(minor(dev)>>3)
#define	hdpart(dev)		(minor(dev)&0x07)
#define	hdminor(unit, part)	(((unit)<<3)|(part))

struct vba_ctlr *hdcminfo[NHDC];
struct vba_device *hddinfo[NHD];
int hdcprobe(), hdslave(), hdattach(), hddgo(), hdstrategy();
long hdstd[] = { 0 };
struct vba_driver hdcdriver =
    { hdcprobe, hdslave, hdattach, hddgo, hdstd, "hd", hddinfo, "hdc", hdcminfo };

/*
 * Per-controller state.
 */
struct hdcsoftc {
	u_short	hdc_flags;
#define	HDC_INIT	0x01	/* controller initialized */
#define	HDC_STARTED	0x02	/* start command issued */
#define	HDC_LOCKED	0x04	/* locked for direct controller access */
#define	HDC_WAIT	0x08	/* someone needs direct controller access */
	u_short	hdc_wticks;		/* timeout */
	struct master_mcb *hdc_mcbp;	/* address of controller mcb */
	struct registers *hdc_reg;	/* base address of i/o regs */
	struct vb_buf hdc_rbuf;		/* vba resources */
	struct master_mcb hdc_mcb;	/* controller mcb */
} hdcsoftc[NHDC];

#define	HDCMAXTIME	20		/* max time for operation, sec. */
#define	HDCINTERRUPT	0xf0		/* interrupt vector */

/*
 * Per-drive state; probably everything should be "hd_", not "dk_",
 * but it's not worth it, and dk is a better mnemonic for disk anyway.
 */
struct dksoftc {
#ifdef COMPAT_42
	u_short	dk_def_cyl;	/* definition track cylinder address */
#endif
	int	dk_state;	/* open fsm */
	u_short	dk_bshift;	/* shift for * (DEV_BSIZE / sectorsize) XXX */
	int	dk_wlabel;	/* if label sector is writeable */
	u_long	dk_copenpart;	/* character units open on this drive */
	u_long	dk_bopenpart;	/* block units open on this drive */
	u_long	dk_openpart;	/* all units open on this drive */
	int	dk_unit;	/* unit# */
	int	dk_ctlr;	/* controller# */
	int	dk_format;	/* if format program is using disk */
	struct buf dk_utab;		/* i/o queue header */
	struct disklabel dk_label;	/* disklabel for this disk */
	struct mcb dk_mcb;		/* disk mcb */
} dksoftc[NHD];

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

int hdcwstart, hdcwatch();

/* see if the controller is really there, if so, init it. */
/* ARGSUSED */
hdcprobe(reg, vm)
	caddr_t reg;
	/* register */ struct vba_ctlr *vm;
{
	register int br, cvec;		/* must be r12, r11 */
	register struct hdcsoftc *hdc;
	static struct module_id id;
	struct pte *dummypte;
	caddr_t putl;

	/* initialize the hdc controller structure. */
	hdc = &hdcsoftc[vm->um_ctlr];
	if (!vbmemalloc(1, reg, &dummypte, &putl)) {
		printf("hdc%d: vbmemalloc failed.\n", vm->um_ctlr);
		return(0);
	}
	hdc->hdc_reg = (struct registers *)putl;

	/*
	 * try and ping the MID register; side effect of wbadaddr is to read
	 * the module id; the controller is bad if it's not an hdc, the hdc's
	 * writeable control store is not loaded, or the hdc failed the
	 * functional integrity test;
	 */
	if (wbadaddr(&hdc->hdc_reg->module_id, 4,
	    vtoph((struct process *)NULL, &id)))
		return(0);
	DELAY(10000);
	mtpr(PADC, 0);
	if (id.module_id != (u_char)HDC_MID) {
		printf("hdc%d: bad module id; id = %x.\n",
		    vm->um_ctlr, id.module_id);
		return(0);
	}
	if (id.code_rev == (u_char)0xff) {
		printf("hdc%d: micro-code not loaded.\n", vm->um_ctlr);
		return(0);
	}
	if (id.fit != (u_char)0xff) {
		printf("hdc%d: FIT test failed.\n", vm->um_ctlr);
		return(0);
	}

	/* reset that pup; flag as inited */
	hdc->hdc_reg->soft_reset = 0;
	DELAY(1000000);
	hdc->hdc_flags |= HDC_INIT;

	/* allocate page tables and i/o buffer. */
	if (!vbainit(&hdc->hdc_rbuf, MAXPHYS, VB_32BIT|VB_SCATTER)) {
		printf("hdc%d: vbainit failed\n", vm->um_ctlr);
		return (0);
	}

	/* set pointer to master control block */
	hdc->hdc_mcbp =
	    (struct master_mcb *)vtoph((struct proc *)NULL, &hdc->hdc_mcb);

	br = 0x17, cvec = HDCINTERRUPT + vm->um_ctlr;		/* XXX */
	return(sizeof(struct registers));
}

/* ARGSUSED */
hdslave(vi, vdaddr)
	struct vba_device *vi;
	struct vddevice *vdaddr;
{
	register struct mcb *mcb;
	register struct disklabel *lp;
	register struct dksoftc *dk;
	static struct status status;

	dk = &dksoftc[vi->ui_unit];
	dk->dk_unit = vi->ui_unit;
	dk->dk_ctlr = vi->ui_ctlr;

	mcb = &dk->dk_mcb;
	mcb->command = HCMD_STATUS;
	mcb->chain[0].wcount = sizeof(struct status) / sizeof(long);
	mcb->chain[0].memadr  = (u_long)vtoph((struct process *)0, &status);
	if (hdimcb(dk)) {
		printf(" (no status)\n");
		return(0);
	}

	/*
	 * Report the drive down if anything in the drive status looks bad.
	 * If the drive is offline and it is not on cylinder, then the drive
	 * is not there.  If there is a fault condition, the hdc will try to
	 * clear it when we read the disklabel information.
	 */
	if (!(status.drs&DRS_ONLINE)) {
		if (status.drs&DRS_ON_CYLINDER)
			printf(" (not online)\n");
		return(0);
	}
	if (status.drs&DRS_FAULT)
		printf(" (clearing fault)");

	lp = &dk->dk_label;
#ifdef RAW_SIZE
	lp->d_secsize = status.bytes_per_sec;
#else
	lp->d_secsize = 512;
#endif
	lp->d_nsectors = status.max_sector + 1;
	lp->d_ntracks = status.max_head + 1;
	lp->d_ncylinders = status.max_cyl + 1;
	lp->d_secpercyl = lp->d_ntracks * lp->d_nsectors;
	lp->d_npartitions = 1;
	lp->d_partitions[0].p_offset = 0;
	lp->d_partitions[0].p_size = LABELSECTOR + 1;
	lp->d_rpm = status.rpm;
	lp->d_typename[0] = 'h';
	lp->d_typename[1] = 'd';
	lp->d_typename[2] = '\0';
#ifdef COMPAT_42
	dk->dk_def_cyl = status.def_cyl;
#endif
	return(1);
}

hdattach(vi)
	register struct vba_device *vi;
{
	register struct dksoftc *dk;
	register struct disklabel *lp;
	register int unit;

	unit = vi->ui_unit;
	if (hdinit(hdminor(unit, 0), 0)) {
		printf(": unknown drive type");
		return;
	}
	dk = &dksoftc[unit];
	lp = &dk->dk_label;
	hd_setsecsize(dk, lp);
	if (dk->dk_state == OPEN)
		printf(": %s <secsize %d, ntrak %d, ncyl %d, nsec %d>",
		    lp->d_typename, lp->d_secsize, lp->d_ntracks,
		    lp->d_ncylinders, lp->d_nsectors);

	/*
	 * (60 / rpm) / (sectors per track * (bytes per sector / 2))
	 */
	if (vi->ui_dk >= 0)
		dk_wpms[vi->ui_dk] =
		    (lp->d_rpm * lp->d_nsectors * lp->d_secsize) / 120;
#ifdef notyet
	addswap(makedev(HDMAJOR, hdminor(unit, 0)), lp);
#endif
}

hdopen(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register struct disklabel *lp;
	register struct dksoftc *dk;
	register struct partition *pp;
	register int unit;
	struct vba_device *vi;
	int s, error, part = hdpart(dev), mask = 1 << part;
	daddr_t start, end;

	unit = hdunit(dev);
	if (unit >= NHD || (vi = hddinfo[unit]) == 0 || vi->ui_alive == 0)
		return(ENXIO);
	dk = &dksoftc[unit];
	lp = &dk->dk_label;
	s = spl7();
	while (dk->dk_state != OPEN && dk->dk_state != OPENRAW &&
	    dk->dk_state != CLOSED)
		if (error = tsleep((caddr_t)dk, (PZERO+1) | PCATCH,
		    devopn, 0)) {
			splx(s);
			return (error);
		}
	splx(s);
	if (dk->dk_state != OPEN && dk->dk_state != OPENRAW)
		if (error = hdinit(dev, flags))
			return(error);

	if (hdcwstart == 0) {
		timeout(hdcwatch, (caddr_t)0, hz);
		hdcwstart++;
	}
	/*
	 * Warn if a partion is opened that overlaps another partition
	 * which is open unless one is the "raw" partition (whole disk).
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
				    "hd%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a',
				    pp - lp->d_partitions + 'a');
		}
	}
	if (part >= lp->d_npartitions)
		return(ENXIO);
	dk->dk_openpart |= mask;
	switch (fmt) {
	case S_IFCHR:
		dk->dk_copenpart |= mask;
		break;
	case S_IFBLK:
		dk->dk_bopenpart |= mask;
		break;
	}
	return(0);
}

/* ARGSUSED */
hdclose(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register struct dksoftc *dk;
	int mask;

	dk = &dksoftc[hdunit(dev)];
	mask = 1 << hdpart(dev);
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
		while (dk->dk_utab.b_actf)
			sleep((caddr_t)dk, PZERO-1);
		splx(s);
		dk->dk_state = CLOSED;
		dk->dk_wlabel = 0;
	}
	return(0);
}

hdinit(dev, flags)
	dev_t dev;
	int flags;
{
	register struct dksoftc *dk;
	register struct disklabel *lp;
	struct vba_device *vi;
	int error, unit;
	char *msg, *readdisklabel();
	extern int cold;

	vi = hddinfo[unit = hdunit(dev)];
	dk = &dksoftc[unit];
	dk->dk_unit = vi->ui_slave;
	dk->dk_ctlr = vi->ui_ctlr;

	if (flags & O_NDELAY) {
		dk->dk_state = OPENRAW;
		return(0);
	}

	error = 0;
	lp = &dk->dk_label;
	dk->dk_state = RDLABEL;
	if (msg = readdisklabel(dev, hdstrategy, lp)) {
		if (cold) {
			printf(": %s\n", msg);
			dk->dk_state = CLOSED;
		} else {
			log(LOG_ERR, "hd%d: %s\n", unit, msg);
			dk->dk_state = OPENRAW;
		}
#ifdef COMPAT_42
		hdclock(vi->ui_ctlr);
		if (!(error = hdreadgeometry(dk)))
			dk->dk_state = OPEN;
		hdcunlock(vi->ui_ctlr);
#endif
	} else
		dk->dk_state = OPEN;
	wakeup((caddr_t)dk);
	return(error);
}

hd_setsecsize(dk, lp)
	register struct dksoftc *dk;
	struct disklabel *lp;
{
	register int mul;

	/*
	 * Calculate scaling shift for mapping
	 * DEV_BSIZE blocks to drive sectors.
	 */
	mul = DEV_BSIZE / lp->d_secsize;
	dk->dk_bshift = 0;
	while ((mul >>= 1) > 0)
		dk->dk_bshift++;
}

/* ARGSUSED */
hddgo(vm)
	struct vba_device *vm;
{}

extern int name_ext;
hdstrategy(bp)
	register struct buf *bp;
{
	register struct vba_device *vi;
	register struct disklabel *lp;
	register struct dksoftc *dk;
	struct buf *dp;
	register int unit;
	daddr_t sn, sz, maxsz;
	int part, s;

	vi = hddinfo[unit = hdunit(bp->b_dev)];
	if (unit >= NHD || vi == 0 || vi->ui_alive == 0) {
		bp->b_error = ENXIO;
		goto bad;
	}
	dk = &dksoftc[unit];
	if (dk->dk_state < OPEN)
		goto q;
	if (dk->dk_state != OPEN && (bp->b_flags & B_READ) == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	part = hdpart(bp->b_dev);
	if ((dk->dk_openpart & (1 << part)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}
	lp = &dk->dk_label;
	sz = (bp->b_bcount + lp->d_secsize - 1) / lp->d_secsize;
	maxsz = lp->d_partitions[part].p_size;
	sn = bp->b_blkno << dk->dk_bshift;
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

q:	s = spl7();
	dp = &dk->dk_utab;
	disksort(dp, bp);
	if (!dp->b_active) {
		(void)hdustart(vi);
		if (!vi->ui_mi->um_tab.b_active)
			hdcstart(vi->ui_mi);
	}
	splx(s);
	return;
bad:
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
}

hdustart(vi)
	register struct vba_device *vi;
{
	register struct buf *bp, *dp;
	register struct vba_ctlr *vm;
	register struct dksoftc *dk;

	dk = &dksoftc[vi->ui_unit];
	dp = &dk->dk_utab;

	/* if queue empty, nothing to do.  impossible? */
	if (dp->b_actf == NULL)
		return;

	/* place on controller transfer queue */
	vm = vi->ui_mi;
	if (vm->um_tab.b_actf == NULL)
		vm->um_tab.b_actf = dp;
	else
		vm->um_tab.b_actl->b_forw = dp;
	vm->um_tab.b_actl = dp;
	dp->b_forw = NULL;
	dp->b_active++;
}

hdcstart(vm)
	register struct vba_ctlr *vm;
{
	register struct buf *bp;
	register struct dksoftc *dk;
	register struct disklabel *lp;
	register struct master_mcb *master;
	register struct mcb *mcb;
	struct vba_device *vi;
	struct hdcsoftc *hdc;
	struct buf *dp;
	int sn;

	/* pull a request off the controller queue */
	for (;;) {
		if ((dp = vm->um_tab.b_actf) == NULL)
			return;
		if (bp = dp->b_actf)
			break;
		vm->um_tab.b_actf = dp->b_forw;
	}

	/* mark controller active */
	vm->um_tab.b_active++;

	vi = hddinfo[hdunit(bp->b_dev)];
	dk = &dksoftc[vi->ui_unit];
	lp = &dk->dk_label;
	sn = bp->b_blkno << dk->dk_bshift;

	/* fill in mcb */
	mcb = &dk->dk_mcb;
	mcb->forw_phaddr = 0;
	/* mcb->priority = 0; */
	mcb->interrupt = 1;
	mcb->command = (bp->b_flags & B_READ) ? HCMD_READ:HCMD_WRITE;
	mcb->cyl = bp->b_cylin;
/* assumes partition starts on cylinder boundary */
	mcb->head = (sn / lp->d_nsectors) % lp->d_ntracks;
	mcb->sector = sn % lp->d_nsectors;
	mcb->drive = vi->ui_slave;
	/* mcb->context = 0;		/* what do we want on interrupt? */

	hdc = &hdcsoftc[vm->um_ctlr];
	if (!hd_sgsetup(bp, &hdc->hdc_rbuf, mcb->chain)) {
		mcb->chain[0].wcount = (bp->b_bcount+3) >> 2;
		mcb->chain[0].memadr =
		    vbasetup(bp, &hdc->hdc_rbuf, (int)lp->d_secsize);
	}

	if (vi->ui_dk >= 0) {
		dk_busy |= 1<<vi->ui_dk;
		dk_xfer[vi->ui_dk]++;
		dk_wds[vi->ui_dk] += bp->b_bcount>>6;
	}

	master = &hdc->hdc_mcb;
	master->mcw = MCL_QUEUED;
	master->interrupt = HDCINTERRUPT + vm->um_ctlr;
	master->forw_phaddr = (u_long)vtoph((struct proc *)NULL, mcb);
	hdc->hdc_reg->master_mcb = (u_long)hdc->hdc_mcbp;
}

/*
 * Wait for controller to finish current operation
 * so that direct controller accesses can be done.
 */
hdclock(ctlr)
	int ctlr;
{
	register struct vba_ctlr *vm = hdcminfo[ctlr];
	register struct hdcsoftc *hdc;
	int s;

	hdc = &hdcsoftc[ctlr];
	s = spl7();
	while (vm->um_tab.b_active || hdc->hdc_flags & HDC_LOCKED) {
		hdc->hdc_flags |= HDC_WAIT;
		sleep((caddr_t)hdc, PRIBIO);
	}
	hdc->hdc_flags |= HDC_LOCKED;
	splx(s);
}

/*
 * Continue normal operations after pausing for 
 * munging the controller directly.
 */
hdcunlock(ctlr)
	int ctlr;
{
	register struct vba_ctlr *vm;
	register struct hdcsoftc *hdc = &hdcsoftc[ctlr];

	hdc->hdc_flags &= ~HDC_LOCKED;
	if (hdc->hdc_flags & HDC_WAIT) {
		hdc->hdc_flags &= ~HDC_WAIT;
		wakeup((caddr_t)hdc);
	} else {
		vm = hdcminfo[ctlr];
		if (vm->um_tab.b_actf)
			hdcstart(vm);
	}
}

hdintr(ctlr)
	int ctlr;
{
	register struct buf *bp, *dp;
	register struct vba_ctlr *vm;
	register struct vba_device *vi;
	register struct hdcsoftc *hdc;
	register struct mcb *mcb;
	struct master_mcb *master;
	register int status;
	int timedout;
	struct dksoftc *dk;

	hdc = &hdcsoftc[ctlr];
	master = &hdc->hdc_mcb;
	uncache(&master->mcs);
	uncache(&master->context);

	vm = hdcminfo[ctlr];
	if (!vm->um_tab.b_active || !(master->mcs&MCS_DONE)) {
		printf("hd%d: stray interrupt\n", ctlr);
		return;
	}

	dp = vm->um_tab.b_actf;
	bp = dp->b_actf;
	vi = hddinfo[hdunit(bp->b_dev)];
	dk = &dksoftc[vi->ui_unit];
	if (vi->ui_dk >= 0)
		dk_busy &= ~(1<<vi->ui_dk);
	timedout = (hdc->hdc_wticks >= HDCMAXTIME);

	mcb = &dk->dk_mcb;

	if (master->mcs & (MCS_SOFTERROR | MCS_FATALERROR) || timedout)
		hdcerror(ctlr, *(u_long *)master->xstatus);
	else 
		hdc->hdc_wticks = 0;
	if (vm->um_tab.b_active) {
		vm->um_tab.b_active = 0;
		vm->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_resid = 0;
		vbadone(bp, &hdc->hdc_rbuf);
		biodone(bp);
		/* start up now, if more work to do */
		if (dp->b_actf)
			hdustart(vi);
		else if (dk->dk_openpart == 0)
			wakeup((caddr_t)dk);
	}
	/* if there are devices ready to transfer, start the controller. */
	if (hdc->hdc_flags & HDC_WAIT) {
		hdc->hdc_flags &= ~HDC_WAIT;
		wakeup((caddr_t)hdc);
	} else if (vm->um_tab.b_actf)
		hdcstart(vm);
}

hdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd, flag;
	caddr_t data;
{
	register int unit;
	register struct dksoftc *dk;
	register struct disklabel *lp;
	int error;

	unit = hdunit(dev);
	dk = &dksoftc[unit];
	lp = &dk->dk_label;
	error = 0;
	switch (cmd) {
	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;
	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[hdpart(dev)];
		break;
	case DIOCSDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			error = setdisklabel(lp, (struct disklabel *)data,
			    (dk->dk_state == OPENRAW) ? 0 : dk->dk_openpart);
		if (error == 0 && dk->dk_state == OPENRAW)
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

			if (error == 0 && dk->dk_state == OPENRAW)
				dk->dk_state = OPEN;
			/* simulate opening partition 0 so write succeeds */
			dk->dk_openpart |= (1 << 0);		/* XXX */
			wlab = dk->dk_wlabel;
			dk->dk_wlabel = 1;
			error = writedisklabel(dev, hdstrategy, lp);
			dk->dk_openpart = dk->dk_copenpart | dk->dk_bopenpart;
			dk->dk_wlabel = wlab;
		}
		break;
	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

/*
 * Watch for lost interrupts.
 */
hdcwatch()
{
	register struct hdcsoftc *hdc;
	register struct vba_ctlr **vmp;
	register int ctlr;
	int s;

	timeout(hdcwatch, (caddr_t)0, hz);
	for (vmp = hdcminfo, hdc = hdcsoftc, ctlr = 0; ctlr < NHDC;
	    ++ctlr, ++vmp, ++hdc) {
		if (*vmp == 0 || (*vmp)->um_alive == 0)
			continue;
		s = spl7();
		if ((*vmp)->um_tab.b_active &&
		    hdc->hdc_wticks++ >= HDCMAXTIME) {
			printf("hd%d: lost interrupt\n", ctlr);
			hdintr(ctlr);
		}
		splx(s);
	}
}

hddump(dev)
	dev_t dev;
{
	return(ENXIO);
}

hdsize(dev)
	dev_t dev;
{
	register int unit = hdunit(dev);
	register struct dksoftc *dk;
	struct vba_device *vi;
	struct disklabel *lp;

	if (unit >= NHD || (vi = hddinfo[unit]) == 0 || vi->ui_alive == 0 ||
	    (dk = &dksoftc[unit])->dk_state != OPEN)
		return (-1);
	lp = &dk->dk_label;
	return ((int)lp->d_partitions[hdpart(dev)].p_size >> dk->dk_bshift);
}

hdimcb(dk)
	register struct dksoftc *dk;
{
	register struct master_mcb *master;
	register struct mcb *mcb;
	register struct hdcsoftc *hdc;
	int timeout;

	/* fill in mcb */
	mcb = &dk->dk_mcb;
	mcb->interrupt = 0;
	mcb->forw_phaddr = 0;
	mcb->drive = dk->dk_unit;

	hdc = &hdcsoftc[dk->dk_ctlr];
	master = &hdc->hdc_mcb;

	/* fill in master mcb */
	master->mcw = MCL_IMMEDIATE;
	master->forw_phaddr = (u_long)vtoph((struct proc *)NULL, mcb);
	master->mcs = 0;

	/* kick controller and wait */
	hdc->hdc_reg->master_mcb = (u_long)hdc->hdc_mcbp;
	for (timeout = 15000; timeout; --timeout) {
		DELAY(1000);
		mtpr(PADC, 0);
		if (master->mcs&MCS_FATALERROR) {
			printf("hdc%d: fatal error\n", dk->dk_ctlr);
			hdcerror(dk->dk_ctlr, *(u_long *)master->xstatus);
			return(1);
		}
		if (master->mcs&MCS_DONE)
			return(0);
	}
	printf("hdc%d: timed out\n", dk->dk_ctlr);
	return(1);
}

hdcerror(ctlr, code)
	int ctlr;
	u_long code;
{
	printf("hd%d: error %lx\n", ctlr, code);
}

#ifdef COMPAT_42
hdreadgeometry(dk)
	struct dksoftc *dk;
{
	static geometry_sector geometry;
	register struct mcb *mcb;
	register struct disklabel *lp;
	geometry_block *geo;
	int cnt;

	/*
	 * Read the geometry block (at head = 0 sector = 0 of the drive
	 * definition cylinder), validate it (must have the correct version
	 * number, header, and checksum).
	 */
	mcb = &dk->dk_mcb;
	mcb->command = HCMD_READ;
	mcb->cyl = dk->dk_def_cyl;
	mcb->head = 0;
	mcb->sector = 0;
	mcb->chain[0].wcount = sizeof(geometry_sector) / sizeof(long);
	mcb->chain[0].memadr  = (u_long)vtoph((struct process *)0, &geometry);
	/* mcb->chain[0].memadr = (long)&geometry; */
	if (hdimcb(dk)) {
 		printf("hd%d: can't read default geometry.\n", dk->dk_unit);
		return(1);
	}
	geo = &geometry.geometry_block;
 	if (geo->version > 64000  ||  geo->version < 0) {
 		printf("hd%d: bad default geometry version#.\n", dk->dk_unit);
		return(1);
	}
 	if (bcmp(&geo->id[0], GB_ID, GB_ID_LEN)) {
 		printf("hd%d: bad default geometry header.\n", dk->dk_unit);
		return(1);
	}
	GB_CHECKSUM(geo, cnt);
	if (geometry.checksum != cnt) {
		printf("hd%d: bad default geometry checksum.\n", dk->dk_unit);
		return(1);
	}
	lp = &dk->dk_label;

	/* 1K block in Harris geometry; convert to sectors for disklabels */
	for (cnt = 0; cnt < GB_MAXPART; cnt++) {
		lp->d_partitions[cnt].p_offset =
		    geo->partition[cnt].start * (1024 / lp->d_secsize);
		lp->d_partitions[cnt].p_size =
		    geo->partition[cnt].length * (1024 / lp->d_secsize);
	}
	lp->d_npartitions = GB_MAXPART;
	return(0);
}
#endif /* COMPAT_42 */
#endif /* NHD */
