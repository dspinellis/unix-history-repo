/*	rk.c	4.29	81/03/11	*/

#include "rk.h"
#if NHK > 0
int	rkpip;		/* DEBUG */
int	rknosval;	/* DEBUG */
/*
 * RK11/RK07 disk driver
 *
 * This driver mimics up.c; see it for an explanation of common code.
 *
 * TODO:
 *	Learn why we lose an interrupt sometime when spinning drives down
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/dk.h"
#include "../h/cpu.h"
#include "../h/cmap.h"

#include "../h/rkreg.h"

struct	rk_softc {
	int	sc_softas;
	int	sc_ndrive;
	int	sc_wticks;
	int	sc_recal;
} rk_softc[NHK];

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct size {
	daddr_t	nblocks;
	int	cyloff;
} rk7_sizes[] ={
	15884,	0,		/* A=cyl 0 thru 240 */
	10032,	241,		/* B=cyl 241 thru 392 */
	53790,	0,		/* C=cyl 0 thru 814 */
	0,	0,
	0,	0,
	0,	0,
	27786,	393,		/* G=cyl 393 thru 813 */
	0,	0,
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

int	rkprobe(), rkslave(), rkattach(), rkdgo(), rkintr();
struct	uba_ctlr *rkminfo[NHK];
struct	uba_device *rkdinfo[NRK];
struct	uba_device *rkip[NHK][4];

u_short	rkstd[] = { 0777440, 0 };
struct	uba_driver hkdriver =
 { rkprobe, rkslave, rkattach, rkdgo, rkstd, "rk", rkdinfo, "hk", rkminfo, 1 };
struct	buf rkutab[NRK];
short	rkcyl[NRK];

struct	rkst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} rkst[] = {
	NRKSECT, NRKTRK, NRKSECT*NRKTRK,	NRK7CYL,	rk7_sizes,
};

u_char 	rk_offset[16] =
  { P400,M400,P400,M400,P800,M800,P800,M800,P1200,M1200,P1200,M1200,0,0,0,0 };

struct	buf rrkbuf[NRK];

#define	b_cylin	b_resid

#ifdef INTRLVE
daddr_t	dkblock();
#endif

int	rkwstart, rkwatch();

rkprobe(reg)
	caddr_t reg;
{
	register int br, cvec;

#ifdef lint	
	br = 0; cvec = br; br = cvec;
#endif
	((struct rkdevice *)reg)->rkcs1 = RK_CDT|RK_IE|RK_CRDY;
	DELAY(10);
	((struct rkdevice *)reg)->rkcs1 = RK_CDT;
	return (1);
}

rkslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	register struct rkdevice *rkaddr = (struct rkdevice *)reg;

	rkaddr->rkcs1 = RK_CDT|RK_CCLR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	DELAY(50);
	if (rkaddr->rkcs2&RK_NED || (rkaddr->rkds&RK_SVAL) == 0) {
		rkaddr->rkcs1 = RK_CDT|RK_CCLR;
		return (0);
	}
	return (1);
}

rkattach(ui)
	register struct uba_device *ui;
{

	if (rkwstart == 0) {
		timeout(rkwatch, (caddr_t)0, hz);
		rkwstart++;
	}
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = 1.0 / (60 * NRKSECT * 256);
	rkip[ui->ui_ctlr][ui->ui_slave] = ui;
	rk_softc[ui->ui_ctlr].sc_ndrive++;
	rkcyl[ui->ui_unit] = -1;
}
 
rkstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct rkst *st;
	register int unit;
	register struct buf *dp;
	int xunit = minor(bp->b_dev) & 07;
	long bn, sz;

	sz = (bp->b_bcount+511) >> 9;
	unit = dkunit(bp);
	if (unit >= NRK)
		goto bad;
	ui = rkdinfo[unit];
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	st = &rkst[ui->ui_type];
	if (bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > st->sizes[xunit].nblocks)
		goto bad;
	bp->b_cylin = bn/st->nspc + st->sizes[xunit].cyloff;
	(void) spl5();
	dp = &rkutab[ui->ui_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		(void) rkustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			(void) rkstart(ui->ui_mi);
	}
	(void) spl0();
	return;

bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

rkustart(ui)
	register struct uba_device *ui;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um;
	register struct rkdevice *rkaddr;
	int didie = 0;

	if (ui == 0)
		return (0);
	dk_busy &= ~(1<<ui->ui_dk);
	dp = &rkutab[ui->ui_unit];
	um = ui->ui_mi;
	rkaddr = (struct rkdevice *)um->um_addr;
	if (um->um_tab.b_active) {
		rk_softc[um->um_ctlr].sc_softas |= 1<<ui->ui_slave;
		return (0);
	}
	rkaddr->rkcs1 = RK_CDT|RK_CERR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((bp = dp->b_actf) == NULL) {
		rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
		rkwait(rkaddr);
		return (0);
	}
	if ((rkaddr->rkds & RK_VV) == 0) {
		/* SHOULD WARN SYSTEM THAT THIS HAPPENED */
		rkaddr->rkcs1 = RK_CDT|RK_PACK|RK_GO;
		rkwait(rkaddr);
	}
	if (dp->b_active)
		goto done;
	dp->b_active = 1;
	if ((rkaddr->rkds & RK_DREADY) != RK_DREADY)
		goto done;
	if (rk_softc[um->um_ctlr].sc_ndrive == 1)
		goto done;
	if (bp->b_cylin == rkcyl[ui->ui_unit])
		goto done;
	rkaddr->rkcyl = bp->b_cylin;
	rkcyl[ui->ui_unit] = bp->b_cylin;
	rkaddr->rkcs1 = RK_CDT|RK_IE|RK_SEEK|RK_GO;
	didie = 1;
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dk_seek[ui->ui_dk]++;
	}
	goto out;
done:
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

rkstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct uba_device *ui;
	register struct rkdevice *rkaddr;
	struct rkst *st;
	daddr_t bn;
	int sn, tn, cmd;

loop:
	if ((dp = um->um_tab.b_actf) == NULL)
		return (0);
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	um->um_tab.b_active++;
	ui = rkdinfo[dkunit(bp)];
	bn = dkblock(bp);
	st = &rkst[ui->ui_type];
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	rkaddr = (struct rkdevice *)ui->ui_addr;
retry:
	rkaddr->rkcs1 = RK_CDT|RK_CERR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((rkaddr->rkds&RK_SVAL) == 0) {
		rknosval++;
		goto nosval;
	}
	if (rkaddr->rkds&RK_PIP) {
		rkpip++;
		goto retry;
	}
	if ((rkaddr->rkds&RK_DREADY) != RK_DREADY) {
		printf("rk%d: not ready", dkunit(bp));
		if ((rkaddr->rkds&RK_DREADY) != RK_DREADY) {
			printf("\n");
			rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
			rkwait(rkaddr);
			rkaddr->rkcs1 = RK_CDT|RK_CERR;
			rkwait(rkaddr);
			um->um_tab.b_active = 0;
			um->um_tab.b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			dp->b_active = 0;
			bp->b_flags |= B_ERROR;
			iodone(bp);
			goto loop;
		}
		printf(" (came back!)\n");
	}
nosval:
	rkaddr->rkcyl = bp->b_cylin;
	rkcyl[ui->ui_unit] = bp->b_cylin;
	rkaddr->rkda = (tn << 8) + sn;
	rkaddr->rkwc = -bp->b_bcount / sizeof (short);
	if (bp->b_flags & B_READ)
		cmd = RK_CDT|RK_IE|RK_READ|RK_GO;
	else
		cmd = RK_CDT|RK_IE|RK_WRITE|RK_GO;
	um->um_cmd = cmd;
	(void) ubago(ui);
	return (1);
}

rkdgo(um)
	register struct uba_ctlr *um;
{
	register struct rkdevice *rkaddr = (struct rkdevice *)um->um_addr;

	rkaddr->rkba = um->um_ubinfo;
	rkaddr->rkcs1 = um->um_cmd|((um->um_ubinfo>>8)&0x300);
}

rkintr(rk11)
	int rk11;
{
	register struct uba_ctlr *um = rkminfo[rk11];
	register struct uba_device *ui;
	register struct rkdevice *rkaddr = (struct rkdevice *)um->um_addr;
	register struct buf *bp, *dp;
	int unit;
	struct rk_softc *sc = &rk_softc[um->um_ctlr];
	int as = (rkaddr->rkatt >> 8) | sc->sc_softas;
	int needie = 1;

	sc->sc_wticks = 0;
	sc->sc_softas = 0;
	if (um->um_tab.b_active) {
		ubadone(um);
		dp = um->um_tab.b_actf;
		bp = dp->b_actf;
		ui = rkdinfo[dkunit(bp)];
		dk_busy &= ~(1 << ui->ui_dk);
		if (rkaddr->rkcs1 & RK_CERR) {
			int recal;
			u_short ds = rkaddr->rkds;
			u_short cs2 = rkaddr->rkcs2;
			u_short er = rkaddr->rker;
			if (ds & RK_WLE) {
				printf("rk%d: write locked\n", dkunit(bp));
				bp->b_flags |= B_ERROR;
			} else if (++um->um_tab.b_errcnt > 28 ||
			    ds&RKDS_HARD || er&RKER_HARD || cs2&RKCS2_HARD) {
				harderr(bp, "rk");
				printf("cs2=%b ds=%b er=%b\n",
				    cs2, RKCS2_BITS, ds, 
				    RKDS_BITS, er, RKER_BITS);
				bp->b_flags |= B_ERROR;
				sc->sc_recal = 0;
			} else
				um->um_tab.b_active = 0;
			if (cs2&RK_MDS) {
				rkaddr->rkcs2 = RK_SCLR;
				goto retry;
			}
			recal = 0;
			if (ds&RK_DROT || er&(RK_OPI|RK_SKI|RK_UNS) ||
			    (um->um_tab.b_errcnt&07) == 4)
				recal = 1;
			if ((er & (RK_DCK|RK_ECH)) == RK_DCK)
				if (rkecc(ui))
					return;
			rkaddr->rkcs1 = RK_CDT|RK_CCLR;
			rkaddr->rkcs2 = ui->ui_slave;
			rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
			rkwait(rkaddr);
			if (recal && um->um_tab.b_active == 0) {
				rkaddr->rkcs1 = RK_CDT|RK_IE|RK_RECAL|RK_GO;
				rkcyl[ui->ui_unit] = -1;
				sc->sc_recal = 0;
				goto nextrecal;
			}
		}
retry:
		switch (sc->sc_recal) {

		case 1:
			rkaddr->rkcyl = bp->b_cylin;
			rkcyl[ui->ui_unit] = bp->b_cylin;
			rkaddr->rkcs1 = RK_CDT|RK_IE|RK_SEEK|RK_GO;
			goto nextrecal;
		case 2:
			if (um->um_tab.b_errcnt < 16 ||
			    (bp->b_flags&B_READ) != 0)
				goto donerecal;
			rkaddr->rkatt = rk_offset[um->um_tab.b_errcnt & 017];
			rkaddr->rkcs1 = RK_CDT|RK_IE|RK_OFFSET|RK_GO;
			/* fall into ... */
		nextrecal:
			sc->sc_recal++;
			rkwait(rkaddr);
			um->um_tab.b_active = 1;
			return;
		donerecal:
		case 3:
			sc->sc_recal = 0;
			um->um_tab.b_active = 0;
			break;
		}
		if (um->um_tab.b_active) {
			um->um_tab.b_active = 0;
			um->um_tab.b_errcnt = 0;
			um->um_tab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = -rkaddr->rkwc * sizeof(short);
			iodone(bp);
			if (dp->b_actf)
				if (rkustart(ui))
					needie = 0;
		}
		as &= ~(1<<ui->ui_slave);
	}
	for (unit = 0; as; as >>= 1, unit++)
		if (as & 1) {
			ui = rkip[rk11][unit];
			if (ui) {
				if (rkustart(rkip[rk11][unit]))
					needie = 0;
			} else {
				rkaddr->rkcs1 = RK_CERR|RK_CDT;
				rkaddr->rkcs2 = unit;
				rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
				rkwait(rkaddr);
			}
		}
	if (um->um_tab.b_actf && um->um_tab.b_active == 0)
		if (rkstart(um))
			needie = 0;
	if (needie)
		rkaddr->rkcs1 = RK_CDT|RK_IE;
}

rkwait(addr)
	register struct rkdevice *addr;
{

	while ((addr->rkcs1 & RK_CRDY) == 0)
		;
}

rkread(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRK)
		u.u_error = ENXIO;
	else
		physio(rkstrategy, &rrkbuf[unit], dev, B_READ, minphys);
}

rkwrite(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRK)
		u.u_error = ENXIO;
	else
		physio(rkstrategy, &rrkbuf[unit], dev, B_WRITE, minphys);
}

rkecc(ui)
	register struct uba_device *ui;
{
	register struct rkdevice *rk = (struct rkdevice *)ui->ui_addr;
	register struct buf *bp = rkutab[ui->ui_unit].b_actf;
	register struct uba_ctlr *um = ui->ui_mi;
	register struct rkst *st;
	struct uba_regs *ubp = ui->ui_hd->uh_uba;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o, cmd, ubaddr;
	int bn, cn, tn, sn;

	npf = btop((rk->rkwc * sizeof(short)) + bp->b_bcount) - 1;
	reg = btop(um->um_ubinfo&0x3ffff) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("rk%d%c: soft ecc sn%d\n", dkunit(bp),
	    'a'+(minor(bp->b_dev)&07), bp->b_blkno + npf);
	mask = rk->rkec2;
	ubapurge(um);
	i = rk->rkec1 - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
		addr = ptob(ubp->uba_map[reg+btop(byte)].pg_pfnum)+
		    (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	um->um_tab.b_active++;	/* Either complete or continuing... */
	if (rk->rkwc == 0)
		return (0);
#ifdef notdef
	rk->rkcs1 |= RK_GO;
#else
	rk->rkcs1 = RK_CDT|RK_CCLR;
	rk->rkcs2 = ui->ui_slave;
	rk->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rk);
	bn = dkblock(bp);
	st = &rkst[ui->ui_type];
	cn = bp->b_cylin;
	sn = bn%st->nspc + npf + 1;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
	rk->rkcyl = cn;
	rk->rkda = (tn << 8) | sn;
	ubaddr = (int)ptob(reg+1) + o;
	rk->rkba = ubaddr;
	cmd = (ubaddr >> 8) & 0x300;
	cmd |= RK_CDT|RK_IE|RK_GO|RK_READ;
	rk->rkcs1 = cmd;
#endif
	return (1);
}

rkreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register rk11, unit;

	for (rk11 = 0; rk11 < NHK; rk11++) {
		if ((um = rkminfo[rk11]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" hk%d", rk11);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		rk_softc[um->um_ctlr].sc_recal = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone(um);
		}
		for (unit = 0; unit < NHK; unit++) {
			if ((ui = rkdinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0)
				continue;
			rkutab[unit].b_active = 0;
			(void) rkustart(ui);
		}
		(void) rkstart(um);
	}
}

rkwatch()
{
	register struct uba_ctlr *um;
	register rk11, unit;
	register struct rk_softc *sc;

	timeout(rkwatch, (caddr_t)0, hz);
	for (rk11 = 0; rk11 < NHK; rk11++) {
		um = rkminfo[rk11];
		if (um == 0 || um->um_alive == 0)
			continue;
		sc = &rk_softc[rk11];
		if (um->um_tab.b_active == 0) {
			for (unit = 0; unit < NRK; unit++)
				if (rkutab[unit].b_active &&
				    rkdinfo[unit]->ui_mi == um)
					goto active;
			sc->sc_wticks = 0;
			continue;
		}
active:
		sc->sc_wticks++;
		if (sc->sc_wticks >= 20) {
			sc->sc_wticks = 0;
			printf("hk%d: lost interrupt\n", rk11);
			ubareset(um->um_ubanum);
		}
	}
}

#define	DBSIZE	20

rkdump(dev)
	dev_t dev;
{
	struct rkdevice *rkaddr;
	char *start;
	int num, blk, unit;
	struct size *sizes;
	register struct uba_regs *uba;
	register struct uba_device *ui;
	register short *rp;
	struct rkst *st;

	unit = minor(dev) >> 3;
	if (unit >= NRK)
		return (ENXIO);
#define	phys(cast, addr) ((cast)((int)addr & 0x7fffffff))
	ui = phys(struct uba_device *, rkdinfo[unit]);
	if (ui->ui_alive == 0)
		return (ENXIO);
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
#if VAX780
	if (cpu == VAX_780)
		ubainit(uba);
#endif
	rkaddr = (struct rkdevice *)ui->ui_physaddr;
	num = maxfree;
	start = 0;
	rkaddr->rkcs1 = RK_CDT|RK_CERR;
	rkaddr->rkcs2 = unit;
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((rkaddr->rkds & RK_VV) == 0) {
		rkaddr->rkcs1 = RK_CDT|RK_IE|RK_PACK|RK_GO;
		rkwait(rkaddr);
	}
	st = &rkst[ui->ui_type];
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
		rkaddr->rkcyl = cn;
		rp = (short *) &rkaddr->rkda;
		*rp = (tn << 8) + sn;
		*--rp = 0;
		*--rp = -blk*NBPG / sizeof (short);
		*--rp = RK_CDT|RK_GO|RK_WRITE;
		rkwait(rkaddr);
		if (rkaddr->rkcs1 & RK_CERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
