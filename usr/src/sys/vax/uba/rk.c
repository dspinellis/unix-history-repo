/*	rk.c	4.41	82/05/19	*/

#include "rk.h"
#if NHK > 0
int	rkpip;		/* DEBUG */
int	rknosval;	/* DEBUG */
#ifdef RKDEBUG
int	rkdebug;
#endif
#ifdef RKBDEBUG
int	rkbdebug;
#endif
/*
 * RK611/RK0[67] disk driver
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
#include "../h/dkbad.h"

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
} rk7_sizes[8] ={
	15884,	0,		/* A=cyl 0 thru 240 */
	10032,	241,		/* B=cyl 241 thru 392 */
	53790,	0,		/* C=cyl 0 thru 814 */
	0,	0,
	0,	0,
	0,	0,
	27786,	393,		/* G=cyl 393 thru 813 */
	0,	0,
}, rk6_sizes[8] ={
	15884,	0,		/* A=cyl 0 thru 240 */
	11154,	241,		/* B=cyl 241 thru 409 */
	27126,	0,		/* C=cyl 0 thru 410 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0,
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

short	rktypes[] = { RK_CDT, 0 };

int	rkprobe(), rkslave(), rkattach(), rkdgo(), rkintr();
struct	uba_ctlr *rkminfo[NHK];
struct	uba_device *rkdinfo[NRK];
struct	uba_device *rkip[NHK][4];

u_short	rkstd[] = { 0777440, 0 };
struct	uba_driver hkdriver =
 { rkprobe, rkslave, rkattach, rkdgo, rkstd, "rk", rkdinfo, "hk", rkminfo, 1 };
struct	buf rkutab[NRK];
short	rkcyl[NRK];
#ifndef NOBADSECT
struct	dkbad rkbad[NRK];
struct	buf brkbuf[NRK];
#endif

struct	rkst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} rkst[] = {
	NRKSECT, NRKTRK, NRKSECT*NRKTRK,	NRK7CYL,	rk7_sizes,
	NRKSECT, NRKTRK, NRKSECT*NRKTRK,	NRK6CYL,	rk6_sizes,
};

u_char 	rk_offset[16] =
  { RKAS_P400,RKAS_M400,RKAS_P400,RKAS_M400,RKAS_P800,RKAS_M800,RKAS_P800,
    RKAS_M800,RKAS_P1200,RKAS_M1200,RKAS_P1200,RKAS_M1200,0,0,0,0
  };

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
	rkintr(0);
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

	ui->ui_type = 0;
	rkaddr->rkcs1 = RK_CCLR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	DELAY(50);
	if (rkaddr->rkcs2&RKCS2_NED || (rkaddr->rkds&RKDS_SVAL) == 0) {
		rkaddr->rkcs1 = RK_CCLR;
		return (0);
	}
	if (rkaddr->rkcs1&RK_CERR && rkaddr->rker&RKER_DTYE) {
		ui->ui_type = 1;
		rkaddr->rkcs1 = RK_CCLR;
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
	ui->ui_flags = 0;
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
	int s;

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
	s = spl5();
	dp = &rkutab[ui->ui_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		(void) rkustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			(void) rkstart(ui->ui_mi);
	}
	splx(s);
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

	if (ui == 0)
		return;
	dk_busy &= ~(1<<ui->ui_dk);
	dp = &rkutab[ui->ui_unit];
	um = ui->ui_mi;
	rkaddr = (struct rkdevice *)um->um_addr;
	if (um->um_tab.b_active) {
		rk_softc[um->um_ctlr].sc_softas |= 1<<ui->ui_slave;
		return;
	}
	if ((bp = dp->b_actf) == NULL)
		return;
	rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_CERR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((rkaddr->rkds & RKDS_VV) == 0 || ui->ui_flags == 0) {
		/* SHOULD WARN SYSTEM THAT THIS HAPPENED */
#ifndef NOBADSECT
		struct rkst *st = &rkst[ui->ui_type];
		struct buf *bbp = &brkbuf[ui->ui_unit];
#endif

		rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_PACK|RK_GO;
		ui->ui_flags = 1;
#ifndef NOBADSECT
		bbp->b_flags = B_READ|B_BUSY;
		bbp->b_dev = bp->b_dev;
		bbp->b_bcount = 512;
		bbp->b_un.b_addr = (caddr_t)&rkbad[ui->ui_unit];
		bbp->b_blkno = st->ncyl*st->nspc - st->nsect;
		bbp->b_cylin = st->ncyl - 1;
		dp->b_actf = bbp;
		bbp->av_forw = bp;
		bp = bbp;
#endif
		rkwait(rkaddr);
	}
	if (dp->b_active)
		goto done;
	dp->b_active = 1;
	if ((rkaddr->rkds & RKDS_DREADY) != RKDS_DREADY)
		goto done;
	if (rk_softc[um->um_ctlr].sc_ndrive == 1)
		goto done;
	if (bp->b_cylin == rkcyl[ui->ui_unit])
		goto done;
	rkaddr->rkcyl = bp->b_cylin;
	rkcyl[ui->ui_unit] = bp->b_cylin;
	rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_IE|RK_SEEK|RK_GO;
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
	return;
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
		return;
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
	rkaddr->rkcs1 = RK_CCLR;
	rkaddr->rkcs2 = ui->ui_slave;
	rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((rkaddr->rkds&RKDS_SVAL) == 0) {
		rknosval++;
		goto nosval;
	}
	if (rkaddr->rkds&RKDS_PIP) {
		rkpip++;
		goto retry;
	}
	if ((rkaddr->rkds&RKDS_DREADY) != RKDS_DREADY) {
		printf("rk%d: not ready", dkunit(bp));
		if ((rkaddr->rkds&RKDS_DREADY) != RKDS_DREADY) {
			printf("\n");
			rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
			rkwait(rkaddr);
			rkaddr->rkcs1 = RK_CCLR;
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
		cmd = rktypes[ui->ui_type]|RK_IE|RK_READ|RK_GO;
	else
		cmd = rktypes[ui->ui_type]|RK_IE|RK_WRITE|RK_GO;
	um->um_cmd = cmd;
	(void) ubago(ui);
}

rkdgo(um)
	register struct uba_ctlr *um;
{
	register struct rkdevice *rkaddr = (struct rkdevice *)um->um_addr;

	um->um_tab.b_active++;	/* should now be 2 */
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
	if (um->um_tab.b_active == 2 || sc->sc_recal) {
		dp = um->um_tab.b_actf;
		bp = dp->b_actf;
		ui = rkdinfo[dkunit(bp)];
		dk_busy &= ~(1 << ui->ui_dk);
#ifndef NOBADSECT
		if (bp->b_flags&B_BAD)
			if (rkecc(ui, CONT))
				return;
#endif
		if (rkaddr->rkcs1 & RK_CERR) {
			int recal;
			u_short ds = rkaddr->rkds;
			u_short cs2 = rkaddr->rkcs2;
			u_short er = rkaddr->rker;
#ifdef RKDEBUG
			if (rkdebug) {
				printf("cs2=%b ds=%b er=%b\n",
				    cs2, RKCS2_BITS, ds, 
				    RKDS_BITS, er, RKER_BITS);
			}
#endif
			if (er & RKER_WLE) {
				printf("rk%d: write locked\n", dkunit(bp));
				bp->b_flags |= B_ERROR;
			} else if (++um->um_tab.b_errcnt > 28 ||
			    ds&RKDS_HARD || er&RKER_HARD || cs2&RKCS2_HARD) {
hard:
				harderr(bp, "rk");
				printf("cs2=%b ds=%b er=%b\n",
				    cs2, RKCS2_BITS, ds, 
				    RKDS_BITS, er, RKER_BITS);
				bp->b_flags |= B_ERROR;
				sc->sc_recal = 0;
			} else if (er & RKER_BSE) {
#ifndef NOBADSECT
				if (rkecc(ui, BSE))
					return;
				else
#endif
					goto hard;
			} else
				um->um_tab.b_active = 0;
			if (cs2&RKCS2_MDS) {
				rkaddr->rkcs2 = RKCS2_SCLR;
				goto retry;
			}
			recal = 0;
			if (ds&RKDS_DROT || er&(RKER_OPI|RKER_SKI|RKER_UNS) ||
			    (um->um_tab.b_errcnt&07) == 4)
				recal = 1;
			if ((er & (RKER_DCK|RKER_ECH)) == RKER_DCK)
				if (rkecc(ui, ECC))
					return;
			rkaddr->rkcs1 = RK_CCLR;
			rkaddr->rkcs2 = ui->ui_slave;
			rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
			rkwait(rkaddr);
			if (recal && um->um_tab.b_active == 0) {
				rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_IE|RK_RECAL|RK_GO;
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
			rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_IE|RK_SEEK|RK_GO;
			goto nextrecal;
		case 2:
			if (um->um_tab.b_errcnt < 16 ||
			    (bp->b_flags&B_READ) == 0)
				goto donerecal;
			rkaddr->rkatt = rk_offset[um->um_tab.b_errcnt & 017];
			rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_IE|RK_OFFSET|RK_GO;
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
		ubadone(um);
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
				rkustart(ui);
		}
		as &= ~(1<<ui->ui_slave);
	}
	for (unit = 0; as; as >>= 1, unit++)
		if (as & 1) {
			ui = rkip[rk11][unit];
			if (ui) {
				rkustart(rkip[rk11][unit]);
			} else {
				rkaddr->rkcs1 = RK_CCLR;
				rkaddr->rkcs2 = unit;
				rkaddr->rkcs1 = RK_DCLR|RK_GO;
				rkwait(rkaddr);
				rkaddr->rkcs1 = RK_CCLR;
			}
		}
	if (um->um_tab.b_actf && um->um_tab.b_active == 0)
		rkstart(um);
	if (((needie = rkaddr->rkcs1) & RK_IE) == 0)
		rkaddr->rkcs1 = RK_IE;
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

rkecc(ui, flag)
	register struct uba_device *ui;
{
	register struct rkdevice *rk = (struct rkdevice *)ui->ui_addr;
	register struct buf *bp = rkutab[ui->ui_unit].b_actf;
	register struct uba_ctlr *um = ui->ui_mi;
	register struct rkst *st;
	struct uba_regs *ubp = ui->ui_hd->uh_uba;
	caddr_t addr;
	int reg, npf, o, cmd, ubaddr;
	int bn, cn, tn, sn;

#ifndef NOBADSECT
	if (flag == CONT)
		npf = bp->b_error;
	else
#endif
		npf = btop((rk->rkwc * sizeof(short)) + bp->b_bcount);
	reg = btop(um->um_ubinfo&0x3ffff) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	bn = dkblock(bp);
	st = &rkst[ui->ui_type];
	cn = bp->b_cylin;
	sn = bn%st->nspc + npf;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
	ubapurge(um);
	um->um_tab.b_active = 2;	/* Either complete or continuing... */
	switch (flag) {
	case ECC:
		{
		register int i;
		int bit, byte, mask;

		npf--;
		reg--;
		printf("rk%d%c: soft ecc sn%d\n", dkunit(bp),
		    'a'+(minor(bp->b_dev)&07), bp->b_blkno + npf);
		mask = rk->rkec2;
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
		if (rk->rkwc == 0)
			return (0);
		npf++;
		reg++;
		break;
		}

#ifndef NOBADSECT
	case BSE:
#ifdef RKBDEBUG
		if (rkbdebug)
	printf("rkecc, BSE: bn %d cn %d tn %d sn %d\n", bn, cn, tn, sn);
#endif
		if ((bn = isbad(&rkbad[ui->ui_unit], cn, tn, sn)) < 0)
			return(0);
		bp->b_flags |= B_BAD;
		bp->b_error = npf + 1;
		bn = st->ncyl*st->nspc - st->nsect - 1 - bn;
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn %= st->nsect;
#ifdef RKBDEBUG
		if (rkbdebug)
	printf("revector to cn %d tn %d sn %d\n", cn, tn, sn);
#endif
		rk->rkwc = -(512 / sizeof (short));
		break;

	case CONT:
#ifdef RKBDEBUG
		if (rkbdebug)
	printf("rkecc, CONT: bn %d cn %d tn %d sn %d\n", bn,cn,tn,sn);
#endif
		bp->b_flags &= ~B_BAD;
		rk->rkwc = -((bp->b_bcount - (int)ptob(npf)) / sizeof (short));
		if (rk->rkwc == 0)
			return(0);
		break;
#endif
	}
	rk->rkcs1 = RK_CCLR;
	rk->rkcs2 = ui->ui_slave;
	rk->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
	rkwait(rk);
	rk->rkcyl = cn;
	rk->rkda = (tn << 8) | sn;
	ubaddr = (int)ptob(reg) + o;
	rk->rkba = ubaddr;
	cmd = (bp->b_flags&B_READ ? RK_READ : RK_WRITE)|RK_IE|RK_GO;
	cmd |= (ubaddr >> 8) & 0x300;
	cmd |= rktypes[ui->ui_type];
	rk->rkcs1 = cmd;
	um->um_tab.b_errcnt = 0;	/* error has been corrected */
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
		rk_softc[um->um_ctlr].sc_wticks = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone(um);
		}
		for (unit = 0; unit < NRK; unit++) {
			if ((ui = rkdinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0 || ui->ui_mi != um)
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
	ubainit(uba);
	rkaddr = (struct rkdevice *)ui->ui_physaddr;
	num = maxfree;
	start = 0;
	rkaddr->rkcs1 = RK_CCLR;
	rkaddr->rkcs2 = unit;
	rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	if ((rkaddr->rkds & RKDS_VV) == 0) {
		rkaddr->rkcs1 = rktypes[ui->ui_type]|RK_IE|RK_PACK|RK_GO;
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
		*--rp = rktypes[ui->ui_type]|RK_GO|RK_WRITE;
		rkwait(rkaddr);
		if (rkaddr->rkcs1 & RK_CERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
