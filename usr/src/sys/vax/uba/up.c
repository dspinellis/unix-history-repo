/*	up.c	4.30	81/03/07	*/

#include "up.h"
#if NSC > 0
/*
 * UNIBUS disk driver with overlapped seeks and ECC recovery.
 *
 * TODO:
 *	Add reading of bad sector information and disk layout from sector 1
 *	Add bad sector forwarding code
 *	Check multiple drive handling
 *	Check unibus reset code
 *	Check that offset recovery code, etc works
 */
#define	DELAY(N)		{ register int d; d = N; while (--d > 0); }

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/cpu.h"
#include "../h/nexus.h"
#include "../h/dk.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "../h/ubavar.h"
#include "../h/ubareg.h"
#include "../h/cmap.h"

#include "../h/upreg.h"

struct	up_softc {
	int	sc_softas;
	int	sc_ndrive;
	int	sc_wticks;
	int	sc_recal;
} up_softc[NSC];

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct	size
{
	daddr_t	nblocks;
	int	cyloff;
} up_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	495520,	0,		/* C=cyl 0 thru 814 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
	81472,	681,		/* F=cyl 681 thru 814 */
	153824,	562,		/* G=cyl 562 thru 814 */
	291346,	82,		/* H=cyl 82 thru 561 */
}, fj_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 49 */
	33440,	50,		/* B=cyl 50 thru 154 */
	263360,	0,		/* C=cyl 0 thru 822 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	213760,	155,		/* H=cyl 155 thru 822 */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

#define	_upSDIST	2		/* 1.0 msec */
#define	_upRDIST	4		/* 2.0 msec */

int	upSDIST = _upSDIST;
int	upRDIST = _upRDIST;

int	upprobe(), upslave(), upattach(), updgo(), upintr();
struct	uba_ctlr *upminfo[NSC];
struct	uba_device *updinfo[NUP];
struct	uba_device *upip[NSC][4];

u_short	upstd[] = { 0776700, 0774400, 0776300, 0 };
struct	uba_driver scdriver =
    { upprobe, upslave, upattach, updgo, upstd, "up", updinfo, "sc", upminfo };
struct	buf	uputab[NUP];

struct	upst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} upst[] = {
	32,	19,	32*19,	823,	up_sizes,	/* 9300/cdc */
/* 9300 actually has 815 cylinders... */
	32,	10,	32*10,	823,	fj_sizes,	/* fujitsu 160m */
};

u_char	up_offset[16] = {
    UP_P400, UP_M400, UP_P400, UP_M400, UP_P800, UP_M800, UP_P800, UP_M800, 
    UP_P1200, UP_M1200, UP_P1200, UP_M1200, 0, 0, 0, 0
};

struct	buf	rupbuf[NUP];

#define	b_cylin b_resid

#ifdef INTRLVE
daddr_t dkblock();
#endif

int	upwstart, upwatch();		/* Have started guardian */
int	upseek;
int	upwaitdry;

/*ARGSUSED*/
upprobe(reg)
	caddr_t reg;
{
	register int br, cvec;

#ifdef lint	
	br = 0; cvec = br; br = cvec;
#endif
	((struct updevice *)reg)->upcs1 = UP_IE|UP_RDY;
	DELAY(10);
	((struct updevice *)reg)->upcs1 = 0;
	return (1);
}

upslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	register struct updevice *upaddr = (struct updevice *)reg;

	upaddr->upcs1 = 0;		/* conservative */
	upaddr->upcs2 = ui->ui_slave;
	if (upaddr->upcs2&UP_NED) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		return (0);
	}
	return (1);
}

upattach(ui)
	register struct uba_device *ui;
{
#ifdef notdef
	register struct updevice *upaddr;
#endif

	if (upwstart == 0) {
		timeout(upwatch, (caddr_t)0, hz);
		upwstart++;
	}
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = .0000020345;
	upip[ui->ui_ctlr][ui->ui_slave] = ui;
	up_softc[ui->ui_ctlr].sc_ndrive++;
#ifdef notdef
	upaddr = (struct updevice *)ui->ui_addr;
	upaddr->upcs1 = 0;
	upaddr->upcs2 = ui->ui_slave;
	upaddr->uphr = -1;
	/* ... */
	if (upaddr-> ... == 10)
		ui->ui_type = 1;
#endif
}
 
upstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct upst *st;
	register int unit;
	register struct buf *dp;
	int xunit = minor(bp->b_dev) & 07;
	long bn, sz;

	sz = (bp->b_bcount+511) >> 9;
	unit = dkunit(bp);
	if (unit >= NUP)
		goto bad;
	ui = updinfo[unit];
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	st = &upst[ui->ui_type];
	if (bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > st->sizes[xunit].nblocks)
		goto bad;
	bp->b_cylin = bn/st->nspc + st->sizes[xunit].cyloff;
	(void) spl5();
	dp = &uputab[ui->ui_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		(void) upustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			(void) upstart(ui->ui_mi);
	}
	(void) spl0();
	return;

bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

/*
 * Unit start routine.
 * Seek the drive to be where the data is
 * and then generate another interrupt
 * to actually start the transfer.
 * If there is only one drive on the controller,
 * or we are very close to the data, don't
 * bother with the search.  If called after
 * searching once, don't bother to look where
 * we are, just queue for transfer (to avoid
 * positioning forever without transferrring.)
 */
upustart(ui)
	register struct uba_device *ui;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um;
	register struct updevice *upaddr;
	register struct upst *st;
	daddr_t bn;
	int sn, csn;
	/*
	 * The SC21 cancels commands if you just say
	 *	cs1 = UP_IE
	 * so we are cautious about handling of cs1.
	 * Also don't bother to clear as bits other than in upintr().
	 */
	int didie = 0;

	if (ui == 0)
		return (0);
	um = ui->ui_mi;
	dk_busy &= ~(1<<ui->ui_dk);
	dp = &uputab[ui->ui_unit];
	if ((bp = dp->b_actf) == NULL)
		goto out;
	/*
	 * If the controller is active, just remember
	 * that this device would like to be positioned...
	 * if we tried to position now we would confuse the SC21.
	 */
	if (um->um_tab.b_active) {
		up_softc[um->um_ctlr].sc_softas |= 1<<ui->ui_slave;
		return (0);
	}
	/*
	 * If we have already positioned this drive,
	 * then just put it on the ready queue.
	 */
	if (dp->b_active)
		goto done;
	dp->b_active = 1;
	upaddr = (struct updevice *)um->um_addr;
	upaddr->upcs2 = ui->ui_slave;
	/*
	 * If drive has just come up,
	 * setup the pack.
	 */
	if ((upaddr->upds & UP_VV) == 0) {
		/* SHOULD WARN SYSTEM THAT THIS HAPPENED */
		upaddr->upcs1 = UP_IE|UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_IE|UP_PRESET|UP_GO;
		upaddr->upof = UP_FMT22;
		didie = 1;
	}
	/*
	 * If drive is offline, forget about positioning.
	 */
	if ((upaddr->upds & (UP_DPR|UP_MOL)) != (UP_DPR|UP_MOL))
		goto done;
	/*
	 * If there is only one drive,
	 * dont bother searching.
	 */
	if (up_softc[um->um_ctlr].sc_ndrive == 1)
		goto done;
	/*
	 * Figure out where this transfer is going to
	 * and see if we are close enough to justify not searching.
	 */
	st = &upst[ui->ui_type];
	bn = dkblock(bp);
	sn = bn%st->nspc;
	sn = (sn + st->nsect - upSDIST) % st->nsect;
	if (bp->b_cylin - upaddr->updc)
		goto search;		/* Not on-cylinder */
	else if (upseek)
		goto done;		/* Ok just to be on-cylinder */
	csn = (upaddr->upla>>6) - sn - 1;
	if (csn < 0)
		csn += st->nsect;
	if (csn > st->nsect - upRDIST)
		goto done;
search:
	upaddr->updc = bp->b_cylin;
	/*
	 * Not on cylinder at correct position,
	 * seek/search.
	 */
	if (upseek)
		upaddr->upcs1 = UP_IE|UP_SEEK|UP_GO;
	else {
		upaddr->upda = sn;
		upaddr->upcs1 = UP_IE|UP_SEARCH|UP_GO;
	}
	didie = 1;
	/*
	 * Mark unit busy for iostat.
	 */
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dk_seek[ui->ui_dk]++;
	}
	goto out;
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
		dp->b_active = 2;
	}
out:
	return (didie);
}

/*
 * Start up a transfer on a drive.
 */
upstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct uba_device *ui;
	register struct updevice *upaddr;
	struct upst *st;
	daddr_t bn;
	int dn, sn, tn, cmd, waitdry;

loop:
	/*
	 * Pull a request off the controller queue
	 */
	if ((dp = um->um_tab.b_actf) == NULL)
		return (0);
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	/*
	 * Mark controller busy, and
	 * determine destination of this request.
	 */
	um->um_tab.b_active++;
	ui = updinfo[dkunit(bp)];
	bn = dkblock(bp);
	dn = ui->ui_slave;
	st = &upst[ui->ui_type];
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	upaddr = (struct updevice *)ui->ui_addr;
	/*
	 * Select drive if not selected already.
	 */
	if ((upaddr->upcs2&07) != dn)
		upaddr->upcs2 = dn;
	/*
	 * Check that it is ready and online
	 */
	waitdry = 0;
	while ((upaddr->upds&UP_DRY) == 0) {
		if (++waitdry > 512)
			break;
		upwaitdry++;
	}
	if ((upaddr->upds & UP_DREADY) != UP_DREADY) {
		printf("up%d: not ready", dkunit(bp));
		if ((upaddr->upds & UP_DREADY) != UP_DREADY) {
			printf("\n");
			um->um_tab.b_active = 0;
			um->um_tab.b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			dp->b_active = 0;
			bp->b_flags |= B_ERROR;
			iodone(bp);
			goto loop;
		}
		/*
		 * Oh, well, sometimes this
		 * happens, for reasons unknown.
		 */
		printf(" (flakey)\n");
	}
	/*
	 * After 16th retry, do offset positioning
	 */
	if (um->um_tab.b_errcnt >= 16 && (bp->b_flags&B_READ) != 0) {
		upaddr->upof = up_offset[um->um_tab.b_errcnt & 017] | UP_FMT22;
		upaddr->upcs1 = UP_IE|UP_OFFSET|UP_GO;
		while (upaddr->upds & UP_PIP)
			DELAY(25);
	}
	/*
	 * Setup for the transfer, and get in the
	 * UNIBUS adaptor queue.
	 */
	upaddr->updc = bp->b_cylin;
	upaddr->upda = (tn << 8) + sn;
	upaddr->upwc = -bp->b_bcount / sizeof (short);
	if (bp->b_flags & B_READ)
		cmd = UP_IE|UP_RCOM|UP_GO;
	else
		cmd = UP_IE|UP_WCOM|UP_GO;
	um->um_cmd = cmd;
	ubago(ui);
	return (1);
}

/*
 * Now all ready to go, stuff the registers.
 */
updgo(um)
	struct uba_ctlr *um;
{
	register struct updevice *upaddr = (struct updevice *)um->um_addr;

	upaddr->upba = um->um_ubinfo;
	upaddr->upcs1 = um->um_cmd|((um->um_ubinfo>>8)&0x300);
}

/*
 * Handle a disk interrupt.
 */
upintr(sc21)
	register sc21;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um = upminfo[sc21];
	register struct uba_device *ui;
	register struct updevice *upaddr = (struct updevice *)um->um_addr;
	register unit;
	struct up_softc *sc = &up_softc[um->um_ctlr];
	int as = (upaddr->upas & 0377) | sc->sc_softas;
	int needie = 1, waitdry;

	sc->sc_wticks = 0;
	sc->sc_softas = 0;
	/*
	 * If controller wasn't transferring, then this is an
	 * interrupt for attention status on seeking drives.
	 * Just service them.
	 */
	if (um->um_tab.b_active == 0) {
		if (upaddr->upcs1 & UP_TRE)
			upaddr->upcs1 = UP_TRE;
		goto doattn;
	}
	/*
	 * Get device and block structures, and a pointer
	 * to the uba_device for the drive.  Select the drive.
	 */
	dp = um->um_tab.b_actf;
	bp = dp->b_actf;
	ui = updinfo[dkunit(bp)];
	dk_busy &= ~(1 << ui->ui_dk);
	if ((upaddr->upcs2&07) != ui->ui_slave)
		upaddr->upcs2 = ui->ui_slave;
	/*
	 * Check for and process errors on
	 * either the drive or the controller.
	 */
	if ((upaddr->upds&UP_ERR) || (upaddr->upcs1&UP_TRE)) {
		waitdry = 0;
		while ((upaddr->upds & UP_DRY) == 0) {
			if (++waitdry > 512)
				break;
			upwaitdry++;
		}
		if (upaddr->uper1&UP_WLE) {
			/*
			 * Give up on write locked devices
			 * immediately.
			 */
			printf("up%d: write locked\n", dkunit(bp));
			bp->b_flags |= B_ERROR;
		} else if (++um->um_tab.b_errcnt > 27) {
			/*
			 * After 28 retries (16 without offset, and
			 * 12 with offset positioning) give up.
			 */
			harderr(bp, "up");
			printf("cs2=%b er1=%b er2=%b\n",
			    upaddr->upcs2, UPCS2_BITS,
			    upaddr->uper1, UPER1_BITS,
			    upaddr->uper2, UPER2_BITS);
			bp->b_flags |= B_ERROR;
		} else {
			/*
			 * Retriable error.
			 * If a soft ecc, correct it (continuing
			 * by returning if necessary.
			 * Otherwise fall through and retry the transfer
			 */
			um->um_tab.b_active = 0;	 /* force retry */
			if ((upaddr->uper1&(UP_DCK|UP_ECH))==UP_DCK)
				if (upecc(ui))
					return;
		}
		/*
		 * Clear drive error and, every eight attempts,
		 * (starting with the fourth)
		 * recalibrate to clear the slate.
		 */
		upaddr->upcs1 = UP_TRE|UP_IE|UP_DCLR|UP_GO;
		needie = 0;
		if ((um->um_tab.b_errcnt&07) == 4) {
			upaddr->upcs1 = UP_RECAL|UP_IE|UP_GO;
			um->um_tab.b_active = 1;
			sc->sc_recal = 1;
			return;
		}
	}
	/*
	 * Done retrying transfer... release
	 * resources... if we were recalibrating,
	 * then retry the transfer.
	 * Mathematical note: 28%8 != 4.
	 */
	ubadone(um);
	if (sc->sc_recal) {
		sc->sc_recal = 0;
		um->um_tab.b_active = 0;	/* force retry */
	}
	/*
	 * If still ``active'', then don't need any more retries.
	 */
	if (um->um_tab.b_active) {
		/*
		 * If we were offset positioning,
		 * return to centerline.
		 */
		if (um->um_tab.b_errcnt >= 16) {
			upaddr->upof = UP_FMT22;
			upaddr->upcs1 = UP_RTC|UP_GO|UP_IE;
			while (upaddr->upds & UP_PIP)
				DELAY(25);
			needie = 0;
		}
		um->um_tab.b_active = 0;
		um->um_tab.b_errcnt = 0;
		um->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_resid = (-upaddr->upwc * sizeof(short));
		iodone(bp);
		/*
		 * If this unit has more work to do,
		 * then start it up right away.
		 */
		if (dp->b_actf)
			if (upustart(ui))
				needie = 0;
	}
	as &= ~(1<<ui->ui_slave);
doattn:
	/*
	 * Process other units which need attention.
	 * For each unit which needs attention, call
	 * the unit start routine to place the slave
	 * on the controller device queue.
	 */
	for (unit = 0; as; as >>= 1, unit++)
		if (as & 1) {
			upaddr->upas = 1<<unit;
			if (upustart(upip[sc21][unit]))
				needie = 0;
		}
	/*
	 * If the controller is not transferring, but
	 * there are devices ready to transfer, start
	 * the controller.
	 */
	if (um->um_tab.b_actf && um->um_tab.b_active == 0)
		if (upstart(um))
			needie = 0;
	if (needie)
		upaddr->upcs1 = UP_IE;
}

upread(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NUP)
		u.u_error = ENXIO;
	else
		physio(upstrategy, &rupbuf[unit], dev, B_READ, minphys);
}

upwrite(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NUP)
		u.u_error = ENXIO;
	else
		physio(upstrategy, &rupbuf[unit], dev, B_WRITE, minphys);
}

/*
 * Correct an ECC error, and restart the i/o to complete
 * the transfer if necessary.  This is quite complicated because
 * the transfer may be going to an odd memory address base and/or
 * across a page boundary.
 */
upecc(ui)
	register struct uba_device *ui;
{
	register struct updevice *up = (struct updevice *)ui->ui_addr;
	register struct buf *bp = uputab[ui->ui_unit].b_actf;
	register struct uba_ctlr *um = ui->ui_mi;
	register struct upst *st;
	struct uba_regs *ubp = ui->ui_hd->uh_uba;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o, cmd, ubaddr;
	int bn, cn, tn, sn;

	/*
	 * Npf is the number of sectors transferred before the sector
	 * containing the ECC error, and reg is the UBA register
	 * mapping (the first part of) the transfer.
	 * O is offset within a memory page of the first byte transferred.
	 */
	npf = btop((up->upwc * sizeof(short)) + bp->b_bcount) - 1;
	reg = btop(um->um_ubinfo&0x3ffff) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("up%d%c: soft ecc sn%d\n", dkunit(bp),
	    'a'+(minor(bp->b_dev)&07), bp->b_blkno + npf);
	mask = up->upec2;
	/*
	 * Flush the buffered data path, and compute the
	 * byte and bit position of the error.  The variable i
	 * is the byte offset in the transfer, the variable byte
	 * is the offset from a page boundary in main memory.
	 */
	ubapurge(um);
	i = up->upec1 - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	/*
	 * Correct while possible bits remain of mask.  Since mask
	 * contains 11 bits, we continue while the bit offset is > -11.
	 * Also watch out for end of this block and the end of the whole
	 * transfer.
	 */
	while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
		addr = ptob(ubp->uba_map[reg+btop(byte)].pg_pfnum)+
		    (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	um->um_tab.b_active++;	/* Either complete or continuing... */
	if (up->upwc == 0)
		return (0);
	/*
	 * Have to continue the transfer... clear the drive,
	 * and compute the position where the transfer is to continue.
	 * We have completed npf+1 sectors of the transfer already;
	 * restart at offset o of next sector (i.e. in UBA register reg+1).
	 */
#ifdef notdef
	up->uper1 = 0;
	up->upcs1 |= UP_GO;
#else
	up->upcs1 = UP_TRE|UP_IE|UP_DCLR|UP_GO;
	bn = dkblock(bp);
	st = &upst[ui->ui_type];
	cn = bp->b_cylin;
	sn = bn%st->nspc + npf + 1;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
	up->updc = cn;
	up->upda = (tn << 8) | sn;
	ubaddr = (int)ptob(reg+1) + o;
	up->upba = ubaddr;
	cmd = (ubaddr >> 8) & 0x300;
	cmd |= UP_IE|UP_GO|UP_RCOM;
	up->upcs1 = cmd;
#endif
	return (1);
}

/*
 * Reset driver after UBA init.
 * Cancel software state of all pending transfers
 * and restart all units and the controller.
 */
upreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register sc21, unit;

	for (sc21 = 0; sc21 < NSC; sc21++) {
		if ((um = upminfo[sc21]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" sc%d", sc21);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		up_softc[sc21].sc_recal = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone(um);
		}
		((struct updevice *)(um->um_addr))->upcs2 = UP_CLR;
		for (unit = 0; unit < NUP; unit++) {
			if ((ui = updinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0 || ui->ui_mi != um)
				continue;
			uputab[unit].b_active = 0;
			(void) upustart(ui);
		}
		(void) upstart(um);
	}
}

/*
 * Wake up every second and if an interrupt is pending
 * but nothing has happened increment a counter.
 * If nothing happens for 20 seconds, reset the UNIBUS
 * and begin anew.
 */
upwatch()
{
	register struct uba_ctlr *um;
	register sc21, unit;
	register struct up_softc *sc;

	timeout(upwatch, (caddr_t)0, hz);
	for (sc21 = 0; sc21 < NSC; sc21++) {
		um = upminfo[sc21];
		if (um == 0 || um->um_alive == 0)
			continue;
		sc = &up_softc[sc21];
		if (um->um_tab.b_active == 0) {
			for (unit = 0; unit < NUP; unit++)
				if (uputab[unit].b_active &&
				    updinfo[unit]->ui_mi == um)
					goto active;
			sc->sc_wticks = 0;
			continue;
		}
active:
		sc->sc_wticks++;
		if (sc->sc_wticks >= 20) {
			sc->sc_wticks = 0;
			printf("sc%d: lost interrupt\n", sc21);
			ubareset(um->um_ubanum);
		}
	}
}

#define	DBSIZE	20

updump(dev)
	dev_t dev;
{
	struct updevice *upaddr;
	char *start;
	int num, blk, unit, i;
	struct size *sizes;
	register struct uba_regs *uba;
	register struct uba_device *ui;
	register short *rp;
	struct upst *st;

	unit = minor(dev) >> 3;
	if (unit >= NUP)
		return (ENXIO);
#define	phys(cast, addr) ((cast)((int)addr & 0x7fffffff))
	ui = phys(struct uba_device *, updinfo[unit]);
	if (ui->ui_alive == 0)
		return (ENXIO);
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
	ubainit(uba);
	upaddr = (struct updevice *)ui->ui_physaddr;
	DELAY(2000000);
	num = maxfree;
	start = 0;
	upaddr->upcs2 = unit;
	DELAY(100);
	if ((upaddr->upcs1&UP_DVA) == 0)
		return (EFAULT);
	if ((upaddr->upds & UP_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UP_FMT22;
	}
	if ((upaddr->upds & UP_DREADY) != UP_DREADY)
		return (EFAULT);
	st = &upst[ui->ui_type];
	sizes = phys(struct size *, st->sizes);
	if (dumplo < 0 || dumplo + num >= sizes[minor(dev)&07].nblocks)
		return (EINVAL);
	while (num > 0) {
		register struct pte *io;
		register int i;
		int cn, sn, tn;
		daddr_t bn;

		blk = num > DBSIZE ? DBSIZE : num;
		io = uba->uba_map;
		for (i = 0; i < blk; i++)
			*(int *)io++ = (btop(start)+i) | (1<<21) | UBAMR_MRV;
		*(int *)io = 0;
		bn = dumplo + btop(start);
		cn = bn/st->nspc + sizes[minor(dev)&07].cyloff;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		upaddr->updc = cn;
		rp = (short *) &upaddr->upda;
		*rp = (tn << 8) + sn;
		*--rp = 0;
		*--rp = -blk*NBPG / sizeof (short);
		*--rp = UP_GO|UP_WCOM;
		do {
			DELAY(25);
		} while ((upaddr->upcs1 & UP_RDY) == 0);
		if (upaddr->upcs1&UP_ERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
