/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rl.c	7.7 (Berkeley) %G%
 */

#include "rl.h"
#if NRL > 0
/*
 * UNIBUS RL02 disk driver
 */
#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "dkstat.h"
#include "dkbad.h"
#include "ioctl.h"
#include "disklabel.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "map.h"
#include "vm.h"
#include "cmap.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "ubavar.h"
#include "ubareg.h"
#include "rlreg.h"

/* Pending Controller items and statistics */
struct	rl_softc {
	int	rl_softas;	/* Attention sumary, (seeks pending) */
	int	rl_ndrive;	/* Number of drives on controller */
	int	rl_wticks;	/* Monitor time for function */
} rl_softc[NHL];

/* 
 * State of controller from last transfer.
 * Since only one transfer can be done at a time per
 * controller, only allocate one for each controller.
 */
struct	rl_stat {
	short	rl_cyl[4];	/* Current cylinder for each drive */
	short	rl_dn;		/* drive number currently transferring */
	short	rl_cylnhd;	/* current cylinder and head of transfer */
	u_short	rl_bleft;	/* bytes left to transfer */
	u_short	rl_bpart;	/* bytes transferred */
} rl_stat[NHL];

#define rlunit(dev)	(minor(dev) >> 3)

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
/* Last cylinder not used. Saved for Bad Sector File */
struct	size {
	daddr_t	nblocks;
	int	cyloff;
} rl02_sizes[8] = {
	15884,		0,		/* A=cyl   0 thru 397 */
	 4520,		398,		/* B=cyl 398 thru 510 */
	   -1,		0,		/* C=cyl   0 thru 511 */
	 4520,		398,		/* D=cyl 398 thru 510 */
	    0,          0,		/* E= Not Defined     */
	    0,          0,		/* F= Not Defined     */
	20440,	        0,		/* G=cyl   0 thru 510 */
	    0,          0,		/* H= Not Defined     */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

int	rlprobe(), rlslave(), rlattach(), rldgo(), rlintr();
struct	uba_ctlr	*rlminfo[NHL];
struct	uba_device	*rldinfo[NRL];
struct	uba_device	*rlip[NHL][4];

/* RL02 driver structure */
u_short	rlstd[] = { 0174400, 0 };
struct	uba_driver hldriver =
    { rlprobe, rlslave, rlattach, rldgo, rlstd, "rl", rldinfo, "hl", rlminfo };

/* User table per controller */
struct	buf	rlutab[NRL];

/* RL02 drive structure */
struct	RL02 {
	short	nbpt;		/* Number of 512 byte blocks/track */
	short	ntrak;
	short	nbpc;		/* Number of 512 byte blocks/cylinder */
	short	ncyl;
	short	btrak;		/* Number of bytes/track */
	struct	size *sizes;
} rl02 = {
	20,	2,	40,	512,	20*512,	rl02_sizes /* rl02/DEC*/
};

#define	b_cylin b_resid		/* Last seek as CYL<<1 | HD */

int	rlwstart, rlwatch();		/* Have started guardian */

/* Check that controller exists */
/*ARGSUSED*/
rlprobe(reg)
	caddr_t reg;
{
	register int br, cvec;

#ifdef lint	
	br = 0; cvec = br; br = cvec;
	rlintr(0);
#endif
	((struct rldevice *)reg)->rlcs = RL_IE | RL_NOOP;
	DELAY(10);
	((struct rldevice *)reg)->rlcs &= ~RL_IE;
	return (sizeof (struct rldevice));
}

rlslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	register struct rldevice *rladdr = (struct rldevice *)reg;
	short ctr = 0;

	/*
	 * DEC reports that:
	 * For some unknown reason the RL02 (seems to be only drive 1)
	 * does not return a valid drive status the first time that a
	 * GET STATUS request is issued for the drive, in fact it can
	 * take up to three or more GET STATUS requests to obtain the
	 * correct status.
	 * In order to overcome this, the driver has been modified to
	 * issue a GET STATUS request and validate the drive status
	 * returned.  If a valid status is not returned after eight
	 * attempts, then an error message is printed.
	 */
	do {
		rladdr->rlda.getstat = RL_RESET;
		rladdr->rlcs = (ui->ui_slave <<8) | RL_GETSTAT; /* Get status*/
		rlwait(rladdr);
	} while ((rladdr->rlcs & (RL_CRDY|RL_ERR)) != RL_CRDY && ++ctr < 8);

	if ((rladdr->rlcs & RL_DE) || (ctr >= 8))
		return (0); 
	if ((rladdr->rlmp.getstat & RLMP_DT) == 0 ) {
		printf("rl%d: rl01's not supported\n", ui->ui_slave);
		return(0);
	}
	return (1);
}

rlattach(ui)
	register struct uba_device *ui;
{
	register struct rldevice *rladdr;

	if (rlwstart == 0) {
		timeout(rlwatch, (caddr_t)0, hz);
		rlwstart++;
	}
	/* Initialize iostat values */
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = .000003906;   /* 16bit transfer time? */
	rlip[ui->ui_ctlr][ui->ui_slave] = ui;
	rl_softc[ui->ui_ctlr].rl_ndrive++;
	rladdr = (struct rldevice *)ui->ui_addr;
	/* reset controller */
	rladdr->rlda.getstat = RL_RESET;	/* SHOULD BE REPEATED? */
	rladdr->rlcs = (ui->ui_slave << 8) | RL_GETSTAT; /* Reset DE bit */
	rlwait(rladdr);
	/* determine disk posistion */
	rladdr->rlcs = (ui->ui_slave << 8) | RL_RHDR;
	rlwait(rladdr);
	/* save disk drive posistion */
	rl_stat[ui->ui_ctlr].rl_cyl[ui->ui_slave] =
	     (rladdr->rlmp.readhdr & 0177700) >> 6;
	rl_stat[ui->ui_ctlr].rl_dn = -1;
}
 
rlopen(dev)
	dev_t dev;
{
	register int unit = rlunit(dev);
	register struct uba_device *ui;

	if (unit >= NRL || (ui = rldinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	return (0);
}

rlstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register int drive;
	register struct buf *dp;
	int partition = minor(bp->b_dev) & 07, s;
	long bn, sz;

	sz = (bp->b_bcount+511) >> 9;
	drive = rlunit(bp->b_dev);
	if (drive >= NRL) {
		bp->b_error = ENXIO;
		goto bad;
	}
	ui = rldinfo[drive];
	if (ui == 0 || ui->ui_alive == 0) {
		bp->b_error = ENXIO;
		goto bad;
	}
	if (bp->b_blkno < 0 ||
	    (bn = bp->b_blkno)+sz > rl02.sizes[partition].nblocks) {
		if (bp->b_blkno == rl02.sizes[partition].nblocks) {
		    bp->b_resid = bp->b_bcount;
		    goto done;
		}
		bp->b_error = EINVAL;
		goto bad;
	}
	/* bn is in 512 byte block size */
	bp->b_cylin = bn/rl02.nbpc + rl02.sizes[partition].cyloff;
	s = spl5();
	dp = &rlutab[ui->ui_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		rlustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			rlstart(ui->ui_mi);
	}
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
done:
	iodone(bp);
	return;
}

/*
 * Unit start routine.
 * Seek the drive to be where the data is
 * and then generate another interrupt
 * to actually start the transfer.
 */
rlustart(ui)
	register struct uba_device *ui;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um;
	register struct rldevice *rladdr;
	daddr_t bn;
	short hd, diff;

	if (ui == 0)
		return;
	um = ui->ui_mi;
	dk_busy &= ~(1 << ui->ui_dk);
	dp = &rlutab[ui->ui_unit];
	if ((bp = dp->b_actf) == NULL)
		return;
	/*
	 * If the controller is active, just remember
	 * that this device has to be positioned...
	 */
	if (um->um_tab.b_active) {
		rl_softc[um->um_ctlr].rl_softas |=  1<<ui->ui_slave;
		return;
	}
	/*
	 * If we have already positioned this drive,
	 * then just put it on the ready queue.
	 */
	if (dp->b_active)
		goto done;
	dp->b_active = 1;	/* positioning drive */
	rladdr = (struct rldevice *)um->um_addr;

	/*
	 * Figure out where this transfer is going to
	 * and see if we are seeked correctly.
	 */
	bn = bp->b_blkno;		/* Block # desired */
	/*
	 * Map 512 byte logical disk blocks
	 * to 256 byte sectors (rl02's are stupid).
	 */
	hd = (bn / rl02.nbpt) & 1;	/* Get head required */
	diff = (rl_stat[um->um_ctlr].rl_cyl[ui->ui_slave] >> 1) - bp->b_cylin;
	if ( diff == 0 && (rl_stat[um->um_ctlr].rl_cyl[ui->ui_slave] & 1) == hd)
		goto done;		/* on cylinder and head */
	/*
	 * Not at correct position.
	 */
	rl_stat[um->um_ctlr].rl_cyl[ui->ui_slave] = (bp->b_cylin << 1) | hd;
	if (diff < 0)
		rladdr->rlda.seek = -diff << 7 | RLDA_HGH | hd << 4;
	else
		rladdr->rlda.seek = diff << 7 | RLDA_LOW | hd << 4;
	rladdr->rlcs = (ui->ui_slave << 8) | RL_SEEK;

	/*
	 * Mark unit busy for iostat.
	 */
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dk_seek[ui->ui_dk]++;
	}
	rlwait(rladdr);
done:
	/*
	 * Device is ready to go.
	 * Put it on the ready queue for the controller
	 * (unless its already there.)
	 */
	if (dp->b_active != 2) {
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
		dp->b_active = 2;	/* Request on ready queue */
	}
}

/*
 * Start up a transfer on a drive.
 */
rlstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct uba_device *ui;
	register struct rldevice *rladdr;
	register struct rl_stat *st = &rl_stat[um->um_ctlr];
	daddr_t bn;
	short sn, cyl, cmd;

loop:
	if ((dp = um->um_tab.b_actf) == NULL) {
		st->rl_dn = -1;
		st->rl_cylnhd = 0;
		st->rl_bleft = 0;
		st->rl_bpart = 0;
		return;
	}
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	/*
	 * Mark controller busy, and
	 * determine destination.
	 */
	um->um_tab.b_active++;
	ui = rldinfo[rlunit(bp->b_dev)];	/* Controller */
	bn = bp->b_blkno;			/* 512 byte Block number */
	cyl = bp->b_cylin << 1;			/* Cylinder */
	cyl |= (bn / rl02.nbpt) & 1;		/* Get head required */
	sn = (bn % rl02.nbpt) << 1;		/* Sector number */
	rladdr = (struct rldevice *)ui->ui_addr;
	rlwait(rladdr);
	rladdr->rlda.rw = cyl<<6 | sn;
	/* save away current transfers drive status */
	st->rl_dn = ui->ui_slave;
	st->rl_cylnhd = cyl;
	st->rl_bleft = bp->b_bcount;
	st->rl_bpart = rl02.btrak - (sn * NRLBPSC);
	/*
	 * RL02 must seek between cylinders and between tracks,
	 * determine maximum data transfer at this time.
	 */
	if (st->rl_bleft < st->rl_bpart)
		st->rl_bpart = st->rl_bleft;
	rladdr->rlmp.rw = -(st->rl_bpart >> 1);
	if (bp->b_flags & B_READ)
		cmd = RL_IE | RL_READ | (ui->ui_slave << 8);
	else
		cmd = RL_IE | RL_WRITE | (ui->ui_slave << 8);
	um->um_cmd = cmd;
	(void) ubago(ui);
}

rldgo(um)
	register struct uba_ctlr *um;
{
	register struct rldevice *rladdr = (struct rldevice *)um->um_addr;

	rladdr->rlba = um->um_ubinfo;
	rladdr->rlcs = um->um_cmd|((um->um_ubinfo>>12)&RL_BAE);
}

/*
 * Handle a disk interrupt.
 */
rlintr(rl21)
	register rl21;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um = rlminfo[rl21];
	register struct uba_device *ui;
	register struct rldevice *rladdr = (struct rldevice *)um->um_addr;
	register unit;
	struct rl_softc *rl = &rl_softc[um->um_ctlr];
	struct rl_stat *st = &rl_stat[um->um_ctlr];
	int as = rl->rl_softas, status;

	rl->rl_wticks = 0;
	rl->rl_softas = 0;
	dp = um->um_tab.b_actf;
	bp = dp->b_actf;
	ui = rldinfo[rlunit(bp->b_dev)];
	dk_busy &= ~(1 << ui->ui_dk);

	/*
	 * Check for and process errors on
	 * either the drive or the controller.
	 */
	if (rladdr->rlcs & RL_ERR) {
		u_short err;
		rlwait(rladdr);
		err = rladdr->rlcs;
		/* get staus and reset controller */
		rladdr->rlda.getstat = RL_GSTAT;
		rladdr->rlcs = (ui->ui_slave << 8) | RL_GETSTAT;
		rlwait(rladdr);
		status = rladdr->rlmp.getstat;
		/* reset drive */
		rladdr->rlda.getstat = RL_RESET;
		rladdr->rlcs = (ui->ui_slave <<8) | RL_GETSTAT; /* Get status*/
		rlwait(rladdr);
		if ((status & RLMP_WL) == RLMP_WL) {
			/*
			 * Give up on write protected devices
			 * immediately.
			 */
			printf("rl%d: write protected\n", rlunit(bp->b_dev));
			bp->b_flags |= B_ERROR;
		} else if (++um->um_tab.b_errcnt > 10) {
			/*
			 * After 10 retries give up.
			 */
			diskerr(bp, "rl", "hard error", LOG_PRINTF, -1,
			    (struct disklabel *)0);
			printf(" cs=%b mp=%b\n", err, RLCS_BITS,
			    status, RLER_BITS);
			bp->b_flags |= B_ERROR;
		} else
			um->um_tab.b_active = 0;	 /* force retry */
		/* determine disk position */
		rladdr->rlcs = (ui->ui_slave << 8) | RL_RHDR;
		rlwait(rladdr);
		/* save disk drive position */
		st->rl_cyl[ui->ui_slave] =
		    (rladdr->rlmp.readhdr & 0177700) >> 6;
	}
	/*
	 * If still ``active'', then don't need any more retries.
	 */
	if (um->um_tab.b_active) {
		/* RL02 check if more data from previous request */
		if ((bp->b_flags & B_ERROR) == 0 &&
		     (int)(st->rl_bleft -= st->rl_bpart) > 0) {
			/*
			 * The following code was modeled from the rk07
			 * driver when an ECC error occured.  It has to
			 * fix the bits then restart the transfer which is
			 * what we have to do (restart transfer).
			 */
			int reg, npf, o, cmd, ubaddr, diff, head;

			/* seek to next head/track */
			/* increment head and/or cylinder */
			st->rl_cylnhd++;
			diff = (st->rl_cyl[ui->ui_slave] >> 1) -
				(st->rl_cylnhd >> 1);
			st->rl_cyl[ui->ui_slave] = st->rl_cylnhd;
			head = st->rl_cylnhd & 1;
			rlwait(rladdr);
			if (diff < 0)
				rladdr->rlda.seek =
				    -diff << 7 | RLDA_HGH | head << 4;
			else
				rladdr->rlda.seek =
				    diff << 7 | RLDA_LOW | head << 4;
			rladdr->rlcs = (ui->ui_slave << 8) | RL_SEEK;
			npf = btop( bp->b_bcount - st->rl_bleft );
			reg = btop(UBAI_ADDR(um->um_ubinfo)) + npf;
			o = (int)bp->b_un.b_addr & PGOFSET;
			ubapurge(um);
			um->um_tab.b_active++;
			rlwait(rladdr);
			rladdr->rlda.rw = st->rl_cylnhd << 6;
			if (st->rl_bleft < (st->rl_bpart = rl02.btrak))
				st->rl_bpart = st->rl_bleft;
			rladdr->rlmp.rw = -(st->rl_bpart >> 1);
			cmd = (bp->b_flags&B_READ ? RL_READ : RL_WRITE) |
			    RL_IE | (ui->ui_slave << 8);
			ubaddr = (int)ptob(reg) + o;
			cmd |= ((ubaddr >> 12) & RL_BAE);
			rladdr->rlba = ubaddr;
			rladdr->rlcs = cmd;
			return;
		}
		um->um_tab.b_active = 0;
		um->um_tab.b_errcnt = 0;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		/* "b_resid" words remaining after error */
		bp->b_resid = st->rl_bleft;
		um->um_tab.b_actf = dp->b_forw;
		dp->b_actf = bp->av_forw;
		st->rl_dn = -1;
		st->rl_bpart = st->rl_bleft = 0;
		iodone(bp);
		/*
		 * If this unit has more work to do,
		 * then start it up right away.
		 */
		if (dp->b_actf)
			rlustart(ui);
		as &= ~(1<<ui->ui_slave);
	} else
		as |= (1<<ui->ui_slave);
	ubadone(um);
	/* reset state info */
	st->rl_dn = -1;
	st->rl_cylnhd = st->rl_bpart = st->rl_bleft = 0;
	/*
	 * Process other units which need attention.
	 * For each unit which needs attention, call
	 * the unit start routine to place the slave
	 * on the controller device queue.
	 */
	while (unit = ffs((long)as)) {
		unit--;		/* was 1 origin */
		as &= ~(1<<unit);
		rlustart(rlip[rl21][unit]);
	}
	/*
	 * If the controller is not transferring, but
	 * there are devices ready to transfer, start
	 * the controller.
	 */
	if (um->um_tab.b_actf && um->um_tab.b_active == 0)
		rlstart(um);
}

rlwait(rladdr)
	register struct rldevice *rladdr;
{

	while ((rladdr->rlcs & RL_CRDY) == 0)
		;
}

/*
 * Reset driver after UBA init.
 * Cancel software state of all pending transfers
 * and restart all units and the controller.
 */
rlreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register struct rldevice *rladdr;
	register struct rl_stat *st;
	register int rl21, unit;

	for (rl21 = 0; rl21 < NHL; rl21++) {
		if ((um = rlminfo[rl21]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" hl%d", rl21);
		rladdr = (struct rldevice *)um->um_addr;
		st = &rl_stat[rl21];
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			um->um_ubinfo = 0;
		}
		/* reset controller */
		st->rl_dn = -1;
		st->rl_cylnhd = 0;
		st->rl_bleft = 0;
		st->rl_bpart = 0;
		rlwait(rladdr);
		for (unit = 0; unit < NRL; unit++) {
			rladdr->rlcs = (unit << 8) | RL_GETSTAT;
			rlwait(rladdr);
			/* Determine disk posistion */
			rladdr->rlcs = (unit << 8) | RL_RHDR;
			rlwait(rladdr);
			/* save disk drive posistion */
			st->rl_cyl[unit] =
				(rladdr->rlmp.readhdr & 0177700) >> 6;
			if ((ui = rldinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0 || ui->ui_mi != um)
				continue;
			rlutab[unit].b_active = 0;
			rlustart(ui);
		}
		rlstart(um);
	}
}

/*
 * Wake up every second and if an interrupt is pending
 * but nothing has happened increment a counter.
 * If nothing happens for 20 seconds, reset the UNIBUS
 * and begin anew.
 */
rlwatch()
{
	register struct uba_ctlr *um;
	register rl21, unit;
	register struct rl_softc *rl;

	timeout(rlwatch, (caddr_t)0, hz);
	for (rl21 = 0; rl21 < NHL; rl21++) {
		um = rlminfo[rl21];
		if (um == 0 || um->um_alive == 0)
			continue;
		rl = &rl_softc[rl21];
		if (um->um_tab.b_active == 0) {
			for (unit = 0; unit < NRL; unit++)
				if (rlutab[unit].b_active &&
				    rldinfo[unit]->ui_mi == um)
					goto active;
			rl->rl_wticks = 0;
			continue;
		}
active:
		rl->rl_wticks++;
		if (rl->rl_wticks >= 20) {
			rl->rl_wticks = 0;
			printf("hl%d: lost interrupt\n", rl21);
			ubareset(um->um_ubanum);
		}
	}
}

/*ARGSUSED*/
rldump(dev)
	dev_t dev;
{

	/* don't think there is room on swap for it anyway. */
}

rlsize(dev)
	dev_t dev;
{
	register int unit = rlunit(dev);
	register struct uba_device *ui;

	if (unit >= NRL || (ui = rldinfo[unit]) == 0 || ui->ui_alive == 0)
		return (-1);
	return (rl02.sizes[minor(dev) & 07].nblocks);
}
#endif
