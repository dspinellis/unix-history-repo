/*	up.c	4.17	81/02/19	*/

#include "up.h"
#if NSC21 > 0
/*
 * UNIBUS disk driver with overlapped seeks and ECC recovery.
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
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "../h/uba.h"
#include "../h/cmap.h"

#include "../h/upreg.h"

struct	up_softc {
	int	sc_softas;
	int	sc_seek;
	int	sc_wticks;
	/* struct uba_minfo sc_minfo; */
} up_softc[NSC21];

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

int	upcntrlr(), upslave(), updgo(), upintr();
struct	uba_minfo *upminfo[NSC21];
struct	uba_dinfo *updinfo[NUP];

u_short	upstd[] = { 0776700, 0774400, 0776300 };
struct	uba_driver updriver =
	{ upcntrlr, upslave, updgo, 0, upstd, "up", updinfo, "sc", upminfo };
struct	buf	uputab[NUP];

struct	upst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} upst[] = {
	32,	19,	32*19,	815,	up_sizes,	/* 9300 */
	32,	19,	32*19,	823,	up_sizes,	/* so cdc will boot */
	32,	10,	32*10,	823,	fj_sizes,	/* fujitsu 160m */
};

int	up_offset[16] =
{
	P400, M400, P400, M400,
	P800, M800, P800, M800,
	P1200, M1200, P1200, M1200,
	0, 0, 0, 0,
};

struct	buf	rupbuf;			/* GROT */

#define	b_cylin b_resid

#ifdef INTRLVE
daddr_t dkblock();
#endif

int	upwstart, upwatch();		/* Have started guardian */
int	upseek;

/*ARGSUSED*/
upcntrlr(um, reg)
	struct uba_minfo *um;
	caddr_t reg;
{
	register int br, cvec;

	((struct device *)reg)->upcs1 |= (IE|RDY);
	return (1);
}

upslave(ui, reg, slaveno)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	register struct device *upaddr = (struct device *)reg;

	upaddr->upcs1 = 0;		/* conservative */
	upaddr->upcs2 = slaveno;
	if (upaddr->upcs2&NED) {
		upaddr->upcs1 = DCLR|GO;
		return (0);
	}
	if (upwstart == 0) {
		timeout(upwatch, (caddr_t)0, HZ);
		upwstart++;
	}
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = .0000020345;
	return (1);
}
 
upstrategy(bp)
	register struct buf *bp;
{
	register struct uba_dinfo *ui;
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

upustart(ui)
	register struct uba_dinfo *ui;
{
	register struct buf *bp, *dp;
	register struct uba_minfo *um;
	register struct device *upaddr;
	register struct upst *st;
	daddr_t bn;
	int sn, cn, csn;
	int didie = 0;

	/* SC21 cancels commands if you say cs1 = IE, so dont */
	/* being ultra-cautious, we clear as bits only in upintr() */
	dk_busy &= ~(1<<ui->ui_dk);
	dp = &uputab[ui->ui_unit];
	if ((bp = dp->b_actf) == NULL)
		goto out;
	/* dont confuse controller by giving SEARCH while dt in progress */
	um = ui->ui_mi;
	if (um->um_tab.b_active) {
		up_softc[um->um_ctlr].sc_softas |= 1<<ui->ui_slave;
		return (0);
	}
	if (dp->b_active)
		goto done;
	dp->b_active = 1;
	upaddr = (struct device *)um->um_addr;
	upaddr->upcs2 = ui->ui_slave;
	if ((upaddr->upds & VV) == 0) {
		upaddr->upcs1 = IE|DCLR|GO;
		upaddr->upcs1 = IE|PRESET|GO;
		upaddr->upof = FMT22;
		didie = 1;
	}
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL))
		goto done;
	st = &upst[ui->ui_type];
	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%st->nspc;
	sn = (sn + st->nsect - upSDIST) % st->nsect;
	if (cn - upaddr->updc)
		goto search;		/* Not on-cylinder */
	else if (upseek)
		goto done;		/* Ok just to be on-cylinder */
	csn = (upaddr->upla>>6) - sn - 1;
	if (csn < 0)
		csn += st->nsect;
	if (csn > st->nsect - upRDIST)
		goto done;
search:
	upaddr->updc = cn;
	if (upseek)
		upaddr->upcs1 = IE|SEEK|GO;
	else {
		upaddr->upda = sn;
		upaddr->upcs1 = IE|SEARCH|GO;
	}
	didie = 1;
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dk_seek[ui->ui_dk]++;
	}
	goto out;
done:
	dp->b_forw = NULL;
	if (um->um_tab.b_actf == NULL)
		um->um_tab.b_actf = dp;
	else
		um->um_tab.b_actl->b_forw = dp;
	um->um_tab.b_actl = dp;
out:
	return (didie);
}

upstart(um)
	register struct uba_minfo *um;
{
	register struct buf *bp, *dp;
	register struct uba_dinfo *ui;
	register unit;
	register struct device *upaddr;
	struct upst *st;
	struct up_softc *sc = &up_softc[um->um_ctlr];
	daddr_t bn;
	int dn, sn, tn, cmd;

loop:
	if ((dp = um->um_tab.b_actf) == NULL)
		return (0);
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	/*
	 * Mark the controller busy, and multi-part disk address.
	 * Select the unit on which the i/o is to take place.
	 */
	um->um_tab.b_active++;
	ui = updinfo[dkunit(bp)];
	bn = dkblock(bp);
	dn = ui->ui_slave;
	st = &upst[ui->ui_type];
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	upaddr = (struct device *)ui->ui_addr;
	if ((upaddr->upcs2 & 07) != dn)
		upaddr->upcs2 = dn;
	/*
	 * If drive is not present and on-line, then
	 * get rid of this with an error and loop to get
	 * rid of the rest of its queued requests.
	 * (Then on to any other ready drives.)
	 */
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL)) {
		printf("!DPR || !MOL, unit %d, ds %o", dn, upaddr->upds);
		if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL)) {
			printf("-- hard\n");
			um->um_tab.b_active = 0;
			um->um_tab.b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			dp->b_active = 0;
			bp->b_flags |= B_ERROR;
			iodone(bp);
			goto loop;
		}
		printf("-- came back\n");
	}
	/*
	 * If this is a retry, then with the 16'th retry we
	 * begin to try offsetting the heads to recover the data.
	 */
	if (um->um_tab.b_errcnt >= 16 && (bp->b_flags&B_READ) != 0) {
		upaddr->upof = up_offset[um->um_tab.b_errcnt & 017] | FMT22;
		upaddr->upcs1 = IE|OFFSET|GO;
		while (upaddr->upds & PIP)
			DELAY(25);
	}
	/*
	 * Now set up the transfer, retrieving the high
	 * 2 bits of the UNIBUS address from the information
	 * returned by ubasetup() for the cs1 register bits 8 and 9.
	 */
	upaddr->updc = bp->b_cylin;
	upaddr->upda = (tn << 8) + sn;
	upaddr->upwc = -bp->b_bcount / sizeof (short);
	if (bp->b_flags & B_READ)
		cmd = IE|RCOM|GO;
	else
		cmd = IE|WCOM|GO;
	um->um_cmd = cmd;
	ubago(ui);
	return (1);
}

updgo(um)
	struct uba_minfo *um;
{
	register struct device *upaddr = (struct device *)um->um_addr;

	upaddr->upba = um->um_ubinfo;
	upaddr->upcs1 = um->um_cmd|((um->um_ubinfo>>8)&0x300);
}

/*
 * Handle a device interrupt.
 *
 * If the transferring drive needs attention, service it
 * retrying on error or beginning next transfer.
 * Service all other ready drives, calling ustart to transfer
 * their blocks to the ready queue in um->um_tab, and then restart
 * the controller if there is anything to do.
 */
upintr(sc21)
	register sc21;
{
	register struct buf *bp, *dp;
	register struct uba_minfo *um = upminfo[sc21];
	register struct uba_dinfo *ui;
	register struct device *upaddr = (struct device *)um->um_addr;
	register unit;
	struct up_softc *sc = &up_softc[um->um_ctlr];
	int as = upaddr->upas & 0377;
	int needie = 1;

	sc->sc_wticks = 0;
	if (um->um_tab.b_active) {
		if ((upaddr->upcs1 & RDY) == 0)
			printf("upintr !RDY\n");
		dp = um->um_tab.b_actf;
		bp = dp->b_actf;
		ui = updinfo[dkunit(bp)];
		dk_busy &= ~(1 << ui->ui_dk);
		upaddr->upcs2 = ui->ui_slave;
		if ((upaddr->upds&ERR) || (upaddr->upcs1&TRE)) {
			int cs2;
			while ((upaddr->upds & DRY) == 0)
				DELAY(25);
			if (++um->um_tab.b_errcnt > 28 || upaddr->uper1&WLE)
				bp->b_flags |= B_ERROR;
			else
				um->um_tab.b_active = 0; /* force retry */
			if (um->um_tab.b_errcnt > 27) {
				cs2 = (int)upaddr->upcs2;
				deverror(bp, cs2, (int)upaddr->uper1);
			}
			if ((upaddr->uper1&(DCK|ECH))==DCK && upecc(ui))
				return;
			upaddr->upcs1 = TRE|IE|DCLR|GO;
			needie = 0;
			if ((um->um_tab.b_errcnt&07) == 4) {
				upaddr->upcs1 = RECAL|GO|IE;
				while(upaddr->upds & PIP)
					DELAY(25);
			}
			if (um->um_tab.b_errcnt == 28 && cs2&(NEM|MXF)) {
				printf("FLAKEY UP ");
				ubareset(um->um_ubanum);
				return;
			}
		}
		ubarelse(ui->ui_ubanum, &um->um_ubinfo);
		if (um->um_tab.b_active) {
			if (um->um_tab.b_errcnt >= 16) {
				upaddr->upcs1 = RTC|GO|IE;
				while (upaddr->upds & PIP)
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
			/* CHECK FOR WRITE LOCK HERE... */
			if (bp->b_resid)
				printf("resid %d ds %o er? %o %o %o\n",
				    bp->b_resid, upaddr->upds,
				    upaddr->uper1, upaddr->uper2, upaddr->uper3);
			iodone(bp);
			if (dp->b_actf)
				if (upustart(ui))
					needie = 0;
		}
		sc->sc_softas &= ~(1<<ui->ui_slave);
	} else {
		if (upaddr->upcs1 & TRE)
			upaddr->upcs1 = TRE;
	}
	as |= sc->sc_softas;
	sc->sc_softas = 0;
	for (unit = 0; unit < NUP; unit++) {
		if ((ui = updinfo[unit]) == 0 || ui->ui_mi != um)
			continue;
		if (as & (1<<unit)) {
			upaddr->upas = 1<<unit;
			if (upustart(ui))
				needie = 0;
		}
	}
	if (um->um_tab.b_actf && um->um_tab.b_active == 0)
		if (upstart(um))
			needie = 0;
	if (needie)
		upaddr->upcs1 = IE;
}

upread(dev)
{

	physio(upstrategy, &rupbuf, dev, B_READ, minphys);
}

upwrite(dev)
{

	physio(upstrategy, &rupbuf, dev, B_WRITE, minphys);
}

/*
 * Correct an ECC error, and restart the i/o to complete
 * the transfer if necessary.  This is quite complicated because
 * the transfer may be going to an odd memory address base and/or
 * across a page boundary.
 */
upecc(ui)
	register struct uba_dinfo *ui;
{
	register struct device *up = (struct device *)ui->ui_addr;
	register struct buf *bp = uputab[ui->ui_unit].b_actf;
	register struct uba_minfo *um = ui->ui_mi;
	register struct upst *st;
	struct up_softc *sc = &up_softc[um->um_ctlr];
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
	printf("%D ", bp->b_blkno+npf);
	prdev("ECC", bp->b_dev);
	mask = up->upec2;
	if (mask == 0) {
		up->upof = FMT22;		/* == RTC ???? */
		return (0);
	}
	/*
	 * Flush the buffered data path, and compute the
	 * byte and bit position of the error.  The variable i
	 * is the byte offset in the transfer, the variable byte
	 * is the offset from a page boundary in main memory.
	 */
	ubp->uba_dpr[(um->um_ubinfo>>28)&0x0f] |= UBA_BNE;
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
	up->upcs1 = TRE|IE|DCLR|GO;
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
	cmd |= IE|GO|RCOM;
	up->upcs1 = cmd;
	return (1);
}

/*
 * Reset driver after UBA init.
 * Cancel software state of all pending transfers
 * and restart all units and the controller.
 */
upreset(uban)
{
	register struct uba_minfo *um;
	register struct uba_dinfo *ui;
	register sc21, unit;
	struct up_softc *sc;
	int any = 0;

	for (sc21 = 0; sc21 < NSC21; sc21++) {
		if ((um = upminfo[sc21]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		sc = &up_softc[um->um_ctlr];
		if (any == 0) {
			printf(" up");
			DELAY(10000000);	/* give it time to self-test */
			any++;
		}
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubarelse(um->um_ubanum, &um->um_ubinfo);
		}
		((struct device *)(um->um_addr))->upcs2 = CLR;
		for (unit = 0; unit < NUP; unit++) {
			if ((ui = updinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0)
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
 * If nothing happens for 20 seconds, reset the controller
 * and begin anew.
 */
upwatch()
{
	register struct uba_minfo *um;
	register sc21, unit;
	register struct up_softc *sc;

	timeout(upwatch, (caddr_t)0, HZ);
	for (sc21 = 0; sc21 < NSC21; sc21++) {
		um = upminfo[sc21];
		if (um == 0 || um->um_alive == 0)
			continue;
		sc = &up_softc[sc21];
		if (um->um_tab.b_active == 0) {
			for (unit = 0; unit < NUP; unit++)
				if (updinfo[unit]->ui_mi == um &&
				    uputab[unit].b_active)
					goto active;
			sc->sc_wticks = 0;
			continue;
		}
    active:
		sc->sc_wticks++;
		if (sc->sc_wticks >= 20) {
			sc->sc_wticks = 0;
			printf("LOST INTERRUPT RESET");
			/* SHOULD JUST RESET ONE CTLR, NOT ALL ON UBA */
			upreset(um->um_ubanum);
			printf("\n");
		}
	}
}

#define	DBSIZE	20

updump(dev)
	dev_t dev;
{
	struct device *upaddr;
	char *start;
	int num, blk, unit, nsect, ntrak, nspc;
	struct size *sizes;
	register struct uba_regs *uba;
	register struct uba_dinfo *ui;
	register short *rp;
	struct upst *st;

	unit = minor(dev) >> 3;
	if (unit >= NUP) {
		printf("bad unit\n");
		return (-1);
	}
#define	phys(cast, addr) ((cast)((int)addr & 0x7fffffff))
	ui = phys(struct uba_dinfo *, updinfo[unit]);
	if (ui->ui_alive == 0) {
		printf("dna\n");
		return(-1);
	}
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
#if VAX780
	if (cpu == VAX_780)
		ubainit(uba);
#endif
	DELAY(1000000);
	upaddr = (struct device *)ui->ui_physaddr;
	while ((upaddr->upcs1&DVA) == 0)
		;
	num = maxfree;
	start = 0;
	upaddr->upcs2 = unit;
	if ((upaddr->upds & VV) == 0) {
		upaddr->upcs1 = DCLR|GO;
		upaddr->upcs1 = PRESET|GO;
		upaddr->upof = FMT22;
	}
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL)) {
		printf("up !DPR || !MOL\n");
		return (-1);
	}
	st = &upst[ui->ui_type];
	nsect = st->nsect;
	ntrak = st->ntrak;
	sizes = phys(struct size *, st->sizes);
	if (dumplo < 0 || dumplo + num >= sizes[minor(dev)&07].nblocks) {
		printf("oor\n");
		return (-1);
	}
	nspc = st->nspc;
	while (num > 0) {
		register struct pte *io;
		register int i;
		int cn, sn, tn;
		daddr_t bn;

		blk = num > DBSIZE ? DBSIZE : num;
		io = uba->uba_map;
		for (i = 0; i < blk; i++)
			*(int *)io++ = (btop(start)+i) | (1<<21) | UBA_MRV;
		*(int *)io = 0;
		bn = dumplo + btop(start);
		cn = bn/nspc + sizes[minor(dev)&07].cyloff;
		sn = bn%nspc;
		tn = sn/nsect;
		sn = sn%nsect;
		upaddr->updc = cn;
		rp = (short *) &upaddr->upda;
		*rp = (tn << 8) + sn;
		*--rp = 0;
		*--rp = -blk*NBPG / sizeof (short);
		*--rp = GO|WCOM;
		do {
			DELAY(25);
		} while ((upaddr->upcs1 & RDY) == 0);
		if (upaddr->upcs1&ERR) {
			printf("up dump dsk err: (%d,%d,%d) cs1=%x, er1=%x\n",
			    cn, tn, sn, upaddr->upcs1, upaddr->uper1);
			return (-1);
		}
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
