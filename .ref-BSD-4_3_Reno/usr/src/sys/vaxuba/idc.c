/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)idc.c	7.9 (Berkeley) 2/17/90
 */

#include "rb.h"
#if NIDC > 0
int	idcdebug = 0;
#define	printd if(idcdebug)printf
int	idctrb[1000];
int	*trp = idctrb;
#define	trace(a,b) {*trp++ = *(int*)a; *trp++ = (int)b; if(trp>&idctrb[998])trp=idctrb;}
/*
 * IDC (RB730) disk driver
 *
 * There can only ever be one IDC on a machine,
 * and only on a VAX-11/730.  We take advantage
 * of that to simplify the driver.
 *
 * TODO:
 *	ecc
 */
#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "user.h"
#include "map.h"
#include "vm.h"
#include "ioctl.h"
#include "disklabel.h"
#include "dkstat.h"
#include "cmap.h"
#include "dkbad.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "machine/pte.h"
#include "../vax/cpu.h"
#include "ubareg.h"
#include "ubavar.h"
#include "idcreg.h"

struct idc_softc {
	int	sc_bcnt;	/* number of bytes to transfer */
	int	sc_resid;	/* total number of bytes to transfer */
	int	sc_ubaddr;	/* Unibus address of data */
	short	sc_unit;	/* unit doing transfer */
	short	sc_softas;	/* software attention summary bits */
	union idc_dar {
		long	dar_l;
		u_short	dar_w[2];
		u_char	dar_b[4];
	} sc_un;		/* prototype disk address register */
} idc_softc;

#define	dar_dar		dar_l		/* the whole disk address */
#define	dar_cyl		dar_w[1]	/* cylinder address */
#define	dar_trk		dar_b[1]	/* track */
#define	dar_sect	dar_b[0]	/* sector */
#define	sc_dar		sc_un.dar_dar
#define	sc_cyl		sc_un.dar_cyl
#define	sc_trk		sc_un.dar_trk
#define	sc_sect		sc_un.dar_sect

#define idcunit(dev)	(minor(dev) >> 3)

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct size {
	daddr_t	nblocks;
	int	cyloff;
} rb02_sizes[8] ={
	15884,	0,		/* A=cyl 0 thru 399 */
	4480,	400,		/* B=cyl 400 thru 510 */
	20480,	0,		/* C=cyl 0 thru 511 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0,
}, rb80_sizes[8] ={
	15884,	0,		/* A=cyl 0 thru 36 */
	33440,	37,		/* B=cyl 37 thru 114 */
	242606,	0,		/* C=cyl 0 thru 558 */
	0,	0,
	0,	0,
	0,	0,
	82080,	115,		/* G=cyl 115 thru 304 */
	110143,	305,		/* H=cyl 305 thru 558 */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

int	idcprobe(), idcslave(), idcattach(), idcdgo(), idcintr();
struct	uba_ctlr *idcminfo[NIDC];
struct	uba_device *idcdinfo[NRB];

u_short	idcstd[] = { 0174400, 0};
struct	uba_driver idcdriver =
 { idcprobe, idcslave, idcattach, idcdgo, idcstd, "rb", idcdinfo, "idc", idcminfo, 0 };
struct	buf idcutab[NRB];
union	idc_dar idccyl[NRB];

struct	idcst {
	short	nbps;
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} idcst[] = {
	256, NRB02SECT, NRB02TRK, NRB02SECT*NRB02TRK, NRB02CYL,	rb02_sizes,
	512, NRB80SECT, NRB80TRK, NRB80SECT*NRB80TRK, NRB80CYL,	rb80_sizes,
};

#define	b_cylin	b_resid

int	idcwstart, idcwticks, idcwatch();

/*ARGSUSED*/
idcprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct idcdevice *idcaddr;

#ifdef lint	
	br = 0; cvec = br; br = cvec;
#endif
	idcaddr = (struct idcdevice *)((caddr_t)uba_hd[0].uh_uba + 0x200);
	idcaddr->idccsr = IDC_ATTN|IDC_IE;
	while ((idcaddr->idccsr & IDC_CRDY) == 0)
		;
	idcaddr->idccsr = IDC_ATTN|IDC_CRDY;
	return (sizeof (struct idcdevice));
}

/*ARGSUSED*/
idcslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	register struct idcdevice *idcaddr;
	register int i;

	idcaddr = (struct idcdevice *)((caddr_t)uba_hd[0].uh_uba + 0x200);
	ui->ui_type = 0;
	idcaddr->idcmpr = IDCGS_GETSTAT;
	idcaddr->idccsr = IDC_GETSTAT|(ui->ui_slave<<8);
	(void) idcwait(idcaddr, 0);
	i = idcaddr->idcmpr;
	idcaddr->idccsr = IDC_CRDY|(1<<(ui->ui_slave+16));
	(void) idcwait(idcaddr, 0);
	/* read header to synchronize microcode */
	idcaddr->idccsr = (ui->ui_slave<<8)|IDC_RHDR;
	(void) idcwait(idcaddr, 0);
	i = idcaddr->idcmpr;		/* read header word 1 */
	i = idcaddr->idcmpr;		/* read header word 2 */
#ifdef lint
	i = i;
#endif
	if ((idcaddr->idccsr & (IDC_ERR|IDC_R80)) == IDC_R80)
		ui->ui_type = 1;
	else if ((idcaddr->idccsr & (IDC_DE|IDC_R80)) == 0)
		/*
		 * RB02 may not have pack spun up, just look for drive error.
		 */
		ui->ui_type = 0;
	else
		return (0);
	return (1);
}

idcattach(ui)
	register struct uba_device *ui;
{

	/*
	 * Fix all addresses to correspond
	 * to the "real" IDC address.
	 */
	ui->ui_mi->um_addr = ui->ui_addr = (caddr_t)uba_hd[0].uh_uba + 0x200;
	ui->ui_physaddr = (caddr_t)uba_hd[0].uh_physuba + 0x200;
	if (idcwstart == 0) {
		timeout(idcwatch, (caddr_t)0, hz);
		idcwstart++;
	}
	if (ui->ui_dk >= 0)
		if (ui->ui_type)
			dk_wpms[ui->ui_dk] = (60 * NRB80SECT * 256);
		else
			dk_wpms[ui->ui_dk] = (60 * NRB02SECT * 128);
	idccyl[ui->ui_unit].dar_dar = -1;
	ui->ui_flags = 0;
}

idcopen(dev)
	dev_t dev;
{
	register int unit = idcunit(dev);
	register struct uba_device *ui;

	if (unit >= NRB || (ui = idcdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	return (0);
}
 
idcstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct idcst *st;
	register int unit;
	register struct buf *dp;
	int xunit = minor(bp->b_dev) & 07;
	long bn, sz;

	sz = (bp->b_bcount+511) >> 9;
	unit = idcunit(bp->b_dev);
	if (unit >= NRB) {
		bp->b_error = ENXIO;
		goto bad;
	}
	ui = idcdinfo[unit];
	if (ui == 0 || ui->ui_alive == 0) {
		bp->b_error = ENXIO;
		goto bad;
	}
	st = &idcst[ui->ui_type];
	if (bp->b_blkno < 0 ||
	    (bn = bp->b_blkno)+sz > st->sizes[xunit].nblocks) {
		if (bp->b_blkno == st->sizes[xunit].nblocks) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		bp->b_error = EINVAL;
		goto bad;
	}
	if (ui->ui_type == 0)
		bn *= 2;
	bp->b_cylin = bn/st->nspc + st->sizes[xunit].cyloff;
	(void) spl5();
	trace("strt",bp);
	dp = &idcutab[ui->ui_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		trace("!act",dp);
		(void) idcustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			(void) idcstart(ui->ui_mi);
	}
	(void) spl0();
	return;

bad:
	bp->b_flags |= B_ERROR;
done:
	iodone(bp);
	return;
}

idcustart(ui)
	register struct uba_device *ui;
{
	register struct buf *bp, *dp;
	register struct uba_ctlr *um;
	register struct idcdevice *idcaddr;
	register struct idcst *st;
	union idc_dar cyltrk;
	daddr_t bn;
	int unit;

	if (ui == 0)
		return (0);
	dk_busy &= ~(1<<ui->ui_dk);
	dp = &idcutab[ui->ui_unit];
	um = ui->ui_mi;
	unit = ui->ui_slave;
	trace("ust", dp);
	idcaddr = (struct idcdevice *)um->um_addr;
	if (um->um_tab.b_active) {
		idc_softc.sc_softas |= 1<<unit;
		trace("umac",idc_softc.sc_softas);
		return (0);
	}
	if ((bp = dp->b_actf) == NULL) {
		trace("!bp",0);
		return (0);
	}
	if (dp->b_active) {
		trace("dpac",dp->b_active);
		goto done;
	}
	dp->b_active = 1;
	/* CHECK DRIVE READY? */
	bn = bp->b_blkno;
	trace("seek", bn);
	if (ui->ui_type == 0)
		bn *= 2;
	st = &idcst[ui->ui_type];
	cyltrk.dar_cyl = bp->b_cylin;
	cyltrk.dar_trk = (bn / st->nsect) % st->ntrak;
	cyltrk.dar_sect = 0;
	printd("idcustart, unit %d, cyltrk 0x%x\n", unit, cyltrk.dar_dar);
	/*
	 * If on cylinder, no need to seek.
	 */
	if (cyltrk.dar_dar == idccyl[ui->ui_unit].dar_dar)
		goto done;
	/*
	 * RB80 can change heads (tracks) just by loading
	 * the disk address register, perform optimization
	 * here instead of doing a full seek.
	 */
	if (ui->ui_type && cyltrk.dar_cyl == idccyl[ui->ui_unit].dar_cyl) {
		idcaddr->idccsr = IDC_CRDY|IDC_IE|IDC_SEEK|(unit<<8);
		idcaddr->idcdar = cyltrk.dar_dar;
		idccyl[ui->ui_unit].dar_dar = cyltrk.dar_dar;
		goto done;
	}
	/*
	 * Need to do a full seek.  Select the unit, clear
	 * its attention bit, set the command, load the
	 * disk address register, and then go.
	 */
	idcaddr->idccsr =
	    IDC_CRDY|IDC_IE|IDC_SEEK|(unit<<8)|(1<<(unit+16));
	idcaddr->idcdar = cyltrk.dar_dar;
	idccyl[ui->ui_unit].dar_dar = cyltrk.dar_dar;
	printd("  seek");
	idcaddr->idccsr = IDC_IE|IDC_SEEK|(unit<<8);
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dk_seek[ui->ui_dk]++;
	}
	/*
	 * RB80's initiate seeks very quickly.  Wait for it
	 * to come ready rather than taking the interrupt.
	 */
	if (ui->ui_type) {
		if (idcwait(idcaddr, 10) == 0)
			return (1);
		idcaddr->idccsr &= ~IDC_ATTN;
		/* has the seek completed? */
		if (idcaddr->idccsr & IDC_DRDY) {
			printd(", drdy");
			idcaddr->idccsr =
			    IDC_CRDY|IDC_IE|IDC_SEEK|(unit<<8)|(1<<(unit+16));
			goto done;
		}
	}
	printd(", idccsr = 0x%x\n", idcaddr->idccsr);
	return (1);
done:
	if (dp->b_active != 2) {
		trace("!=2",dp->b_active);
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else {
			trace("!NUL",um->um_tab.b_actl);
			um->um_tab.b_actl->b_forw = dp;
		}
		um->um_tab.b_actl = dp;
		dp->b_active = 2;
	}
	return (0);
}

idcstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct uba_device *ui;
	register struct idcdevice *idcaddr;
	register struct idc_softc *sc;
	struct idcst *st;
	daddr_t bn;
	int sn, tn, cmd;

loop:
	if ((dp = um->um_tab.b_actf) == NULL) {
		trace("nodp",um);
		return (0);
	}
	if ((bp = dp->b_actf) == NULL) {
		trace("nobp", dp);
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	um->um_tab.b_active = 1;
	ui = idcdinfo[idcunit(bp->b_dev)];
	bn = bp->b_blkno;
	trace("star",bp);
	if (ui->ui_type == 0)
		bn *= 2;
	sc = &idc_softc;
	st = &idcst[ui->ui_type];
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	sc->sc_sect = sn;
	sc->sc_trk = tn;
	sc->sc_cyl = bp->b_cylin;
	idcaddr = (struct idcdevice *)ui->ui_addr;
	printd("idcstart, unit %d, dar 0x%x", ui->ui_slave, sc->sc_dar);
	if (bp->b_flags & B_READ)
		cmd = IDC_IE|IDC_READ|(ui->ui_slave<<8);
	else
		cmd = IDC_IE|IDC_WRITE|(ui->ui_slave<<8);
	idcaddr->idccsr = IDC_CRDY|cmd;
	if ((idcaddr->idccsr&IDC_DRDY) == 0) {
		printf("rb%d: not ready\n", idcunit(bp->b_dev));
		um->um_tab.b_active = 0;
		um->um_tab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		dp->b_active = 0;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	idccyl[ui->ui_unit].dar_dar = sc->sc_dar;
	idccyl[ui->ui_unit].dar_sect = 0;
	sn = (st->nsect - sn) * st->nbps;
	if (sn > bp->b_bcount)
		sn = bp->b_bcount;
	sc->sc_bcnt = sn;
	sc->sc_resid = bp->b_bcount;
	sc->sc_unit = ui->ui_slave;
	printd(", bcr 0x%x, cmd 0x%x\n", sn, cmd);
	um->um_cmd = cmd;
	(void) ubago(ui);
	return (1);
}

idcdgo(um)
	register struct uba_ctlr *um;
{
	register struct idcdevice *idcaddr = (struct idcdevice *)um->um_addr;
	register struct idc_softc *sc = &idc_softc;

	/*
	 * VERY IMPORTANT: must load registers in this order.
	 */
	idcaddr->idcbar = sc->sc_ubaddr = UBAI_ADDR(um->um_ubinfo);
	idcaddr->idcbcr = -sc->sc_bcnt;
	idcaddr->idcdar = sc->sc_dar;
	printd("idcdgo, ubinfo 0x%x, cmd 0x%x\n", um->um_ubinfo, um->um_cmd);
	idcaddr->idccsr = um->um_cmd;
	trace("go", um);
	um->um_tab.b_active = 2;
	/*** CLEAR SPURIOUS ATTN ON R80? ***/
}

idcintr(idc)
	int idc;
{
	register struct uba_ctlr *um = idcminfo[idc];
	register struct uba_device *ui;
	register struct idcdevice *idcaddr = (struct idcdevice *)um->um_addr;
	register struct idc_softc *sc = &idc_softc;
	register struct buf *bp, *dp;
	struct idcst *st;
	int unit, as, er, cmd, ds = 0;

	printd("idcintr, idccsr 0x%x", idcaddr->idccsr);
top:
	idcwticks = 0;
	trace("intr", um->um_tab.b_active);
	if (um->um_tab.b_active == 2) {
		/*
		 * Process a data transfer complete interrupt.
		 */
		um->um_tab.b_active = 1;
		dp = um->um_tab.b_actf;
		bp = dp->b_actf;
		ui = idcdinfo[idcunit(bp->b_dev)];
		unit = ui->ui_slave;
		st = &idcst[ui->ui_type];
		idcaddr->idccsr = IDC_IE|IDC_CRDY|(unit<<8);
		if ((er = idcaddr->idccsr) & IDC_ERR) {
			if (er & IDC_DE) {
				idcaddr->idcmpr = IDCGS_GETSTAT;
				idcaddr->idccsr = IDC_GETSTAT|(unit<<8);
				(void) idcwait(idcaddr, 0);
				ds = idcaddr->idcmpr;
				idcaddr->idccsr =
				    IDC_IE|IDC_CRDY|(1<<(unit+16));
			}
			printd(", er 0x%x, ds 0x%x", er, ds);
			if (ds & IDCDS_WL) {
				printf("rb%d: write locked\n",
					idcunit(bp->b_dev));
				bp->b_flags |= B_ERROR;
			} else if (++um->um_tab.b_errcnt > 28 || er&IDC_HARD) {
hard:
				diskerr(bp, "rb", "hard error", LOG_PRINTF, -1,
				    (struct disklabel *)0);
				printf(" csr=%b ds=%b\n", er, IDCCSR_BITS, ds, 
				    ui->ui_type?IDCRB80DS_BITS:IDCRB02DS_BITS);
				bp->b_flags |= B_ERROR;
			} else if (er & IDC_DCK) {
				switch ((int)(er & IDC_ECS)) {
				case IDC_ECS_NONE:
					break;
				case IDC_ECS_SOFT:
					idcecc(ui);
					break;
				case IDC_ECS_HARD:
				default:
					goto hard;
				}
			} else
				/* recoverable error, set up for retry */
				goto seek;
		}
		if ((sc->sc_resid -= sc->sc_bcnt) != 0) {
			sc->sc_ubaddr += sc->sc_bcnt;
			/*
			 * Current transfer is complete, have
			 * we overflowed to the next track?
			 */
			if ((sc->sc_sect += sc->sc_bcnt/st->nbps) == st->nsect) {
				sc->sc_sect = 0;
				if (++sc->sc_trk == st->ntrak) {
					sc->sc_trk = 0;
					sc->sc_cyl++;
				} else if (ui->ui_type) {
					/*
					 * RB80 can change heads just by
					 * loading the disk address register.
					 */
					idcaddr->idccsr = IDC_SEEK|IDC_CRDY|
					    IDC_IE|(unit<<8);
					printd(", change to track 0x%x", sc->sc_dar);
					idcaddr->idcdar = sc->sc_dar;
					idccyl[ui->ui_unit].dar_dar = sc->sc_dar;
					idccyl[ui->ui_unit].dar_sect = 0;
					goto cont;
				}
				/*
				 * Changing tracks on RB02 or cylinders
				 * on RB80, start a seek.
				 */
seek:
				cmd = IDC_IE|IDC_SEEK|(unit<<8);
				idcaddr->idccsr = cmd|IDC_CRDY;
				idcaddr->idcdar = sc->sc_dar;
				printd(", seek to 0x%x\n", sc->sc_dar);
				idccyl[ui->ui_unit].dar_dar = sc->sc_dar;
				idccyl[ui->ui_unit].dar_sect = 0;
				sc->sc_bcnt = 0;
				idcaddr->idccsr = cmd;
				if (ui->ui_type) {
					if (idcwait(idcaddr, 10) == 0)
						return;
					idcaddr->idccsr &= ~IDC_ATTN;
					if (idcaddr->idccsr & IDC_DRDY)
						goto top;
				}
			} else {
				/*
				 * Continue transfer on current track.
				 */
cont:
				sc->sc_bcnt = (st->nsect-sc->sc_sect)*st->nbps;
				if (sc->sc_bcnt > sc->sc_resid)
					sc->sc_bcnt = sc->sc_resid;
				if (bp->b_flags & B_READ)
					cmd = IDC_IE|IDC_READ|(unit<<8);
				else
					cmd = IDC_IE|IDC_WRITE|(unit<<8);
				idcaddr->idccsr = cmd|IDC_CRDY;
				idcaddr->idcbar = sc->sc_ubaddr;
				idcaddr->idcbcr = -sc->sc_bcnt;
				idcaddr->idcdar = sc->sc_dar;
				printd(", continue I/O 0x%x, 0x%x\n", sc->sc_dar, sc->sc_bcnt);
				idcaddr->idccsr = cmd;
				um->um_tab.b_active = 2;
			}
			return;
		}
		/*
		 * Entire transfer is done, clean up.
		 */
		ubadone(um);
		dk_busy &= ~(1 << ui->ui_dk);
		um->um_tab.b_active = 0;
		um->um_tab.b_errcnt = 0;
		um->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		trace("done", dp); trace(&um->um_tab.b_actf, dp->b_actf);
		bp->b_resid = sc->sc_resid;
		printd(", iodone, resid 0x%x\n", bp->b_resid);
		iodone(bp);
		if (dp->b_actf)
			if (idcustart(ui))
				return;
	} else if (um->um_tab.b_active == 1) {
		/*
		 * Got an interrupt while setting up for a command
		 * or doing a mid-transfer seek.  Save any attentions
		 * for later and process a mid-transfer seek complete.
		 */
		as = idcaddr->idccsr;
		idcaddr->idccsr = IDC_IE|IDC_CRDY|(as&IDC_ATTN);
		as = (as >> 16) & 0xf;
		unit = sc->sc_unit;
		sc->sc_softas |= as & ~(1<<unit);
		if (as & (1<<unit)) {
			printd(", seek1 complete");
			um->um_tab.b_active = 2;
			goto top;
		}
		printd(", as1 %o\n", as);
		return;
	}
	/*
	 * Process any seek initiated or complete interrupts.
	 */
	as = idcaddr->idccsr;
	idcaddr->idccsr = IDC_IE|IDC_CRDY|(as&IDC_ATTN);
	as = ((as >> 16) & 0xf) | sc->sc_softas;
	sc->sc_softas = 0;
	trace("as", as);
	printd(", as %o", as);
	for (unit = 0; unit < NRB; unit++)
		if (as & (1<<unit)) {
			as &= ~(1<<unit);
			idcaddr->idccsr = IDC_IE|IDC_CRDY|(unit<<8);
			ui = idcdinfo[unit];
			if (ui) {
				printd(", attn unit %d", unit);
				if (idcaddr->idccsr & IDC_DRDY)
					if (idcustart(ui)) {
						sc->sc_softas = as;
						return;
					}
			} else {
				printd(", unsol. intr. unit %d", unit);
			}
		}
	printd("\n");
	if (um->um_tab.b_actf && um->um_tab.b_active == 0) {
		trace("stum",um->um_tab.b_actf);
		(void) idcstart(um);
	}
}

idcwait(addr, n)
	register struct idcdevice *addr;
	register int n;
{
	register int i;

	while (--n && (addr->idccsr & IDC_CRDY) == 0)
		for (i = 10; i; i--)
			;
	return (n);
}

idcecc(ui)
	register struct uba_device *ui;
{
	register struct idcdevice *idc = (struct idcdevice *)ui->ui_addr;
	register struct buf *bp = idcutab[ui->ui_unit].b_actf;
	register struct uba_ctlr *um = ui->ui_mi;
	register int i;
	struct uba_regs *ubp = ui->ui_hd->uh_uba;
	int bit, byte, mask;
	caddr_t addr;
	int reg, npf, o;

	npf = btop(idc->idcbcr + idc_softc.sc_bcnt) - 1;;
	reg = btop(idc_softc.sc_ubaddr) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	um->um_tab.b_active = 1;	/* Either complete or continuing... */
	diskerr(bp, "rb", "soft ecc", LOG_WARNING, npf, (struct disklabel *)0);
	addlog("\n");
	mask = idc->idceccpat;
	i = idc->idceccpos - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	while (i < 512 && (int)ptob(npf)+i < idc_softc.sc_bcnt && bit > -11) {
		/*
		 * should be:
		 *	addr = ptob(ubp->uba_map[reg+btop(byte)].pg_pfnum)+
		 *		(byte & PGOFSET);
		 * but this generates an extzv which hangs the UNIBUS.
		 */
		addr = ptob(*(int *)&ubp->uba_map[reg+btop(byte)]&0x1fffff)+
		    (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	idc_softc.sc_bcnt += idc->idcbcr;
	um->um_tab.b_errcnt = 0;	/* error has been corrected */
	return;
}

idcreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register unit;

	if ((um = idcminfo[0]) == 0 || um->um_ubanum != uban ||
	    um->um_alive == 0)
		return;
	printf(" idc0");
	um->um_tab.b_active = 0;
	um->um_tab.b_actf = um->um_tab.b_actl = 0;
	if (um->um_ubinfo) {
		printf("<%d>", (um->um_ubinfo>>28)&0xf);
		um->um_ubinfo = 0;
	}
	for (unit = 0; unit < NRB; unit++) {
		if ((ui = idcdinfo[unit]) == 0 || ui->ui_alive == 0)
			continue;
		idcutab[unit].b_active = 0;
		(void) idcustart(ui);
	}
	(void) idcstart(um);
}

idcwatch()
{
	register struct uba_ctlr *um;
	register unit;

	timeout(idcwatch, (caddr_t)0, hz);
	um = idcminfo[0];
	if (um == 0 || um->um_alive == 0)
		return;
	if (um->um_tab.b_active == 0) {
		for (unit = 0; unit < NRB; unit++)
			if (idcutab[unit].b_active)
				goto active;
		idcwticks = 0;
		return;
	}
active:
	idcwticks++;
	if (idcwticks >= 20) {
		idcwticks = 0;
		printf("idc0: lost interrupt\n");
		idcintr(0);
	}
}

/*ARGSUSED*/
idcdump(dev)
	dev_t dev;
{
	struct idcdevice *idcaddr;
	char *start;
	int num, blk, unit;
	struct size *sizes;
	register struct uba_regs *uba;
	register struct uba_device *ui;
	struct idcst *st;
	union idc_dar dar;
	int nspg;

	unit = idcunit(dev);
	if (unit >= NRB)
		return (ENXIO);
#define	phys(cast, addr) ((cast)((int)addr & 0x7fffffff))
	ui = phys(struct uba_device *, idcdinfo[unit]);
	if (ui->ui_alive == 0)
		return (ENXIO);
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
	ubainit(uba);
	idcaddr = (struct idcdevice *)ui->ui_physaddr;
	if (idcwait(idcaddr, 100) == 0)
		return (EFAULT);
	/*
	 * Since we can only transfer one track at a time, and
	 * the rl02 has 256 byte sectors, all the calculations
	 * are done in terms of physical sectors (i.e. num and blk
	 * are in sectors not NBPG blocks.
	 */
	st = phys(struct idcst *, &idcst[ui->ui_type]);
	sizes = phys(struct size *, st->sizes);
	if (dumplo < 0)
		return (EINVAL);
	if (dumplo + maxfree >= sizes[minor(dev)&07].nblocks)
		num = sizes[minor(dev)&07].nblocks - dumplo;
	nspg = NBPG / st->nbps;
	num = num * nspg;
	start = 0;

	while (num > 0) {
		register struct pte *io;
		register int i;
		daddr_t bn;

		bn = (dumplo + btop(start)) * nspg;
		dar.dar_cyl = bn / st->nspc + sizes[minor(dev)&07].cyloff;
		bn %= st->nspc;
		dar.dar_trk = bn / st->nsect;
		dar.dar_sect = bn % st->nsect;
		blk = st->nsect - dar.dar_sect;
		if (num < blk)
			blk = num;

		io = uba->uba_map;
		for (i = 0; i < (blk + nspg - 1) / nspg; i++)
			*(int *)io++ = (btop(start)+i) | (1<<21) | UBAMR_MRV;
		*(int *)io = 0;

		idcaddr->idccsr = IDC_CRDY | IDC_SEEK | unit<<8;
		if ((idcaddr->idccsr&IDC_DRDY) == 0)
			return (EFAULT);
		idcaddr->idcdar = dar.dar_dar;
		idcaddr->idccsr = IDC_SEEK | unit << 8;
		while ((idcaddr->idccsr & (IDC_CRDY|IDC_DRDY))
			!= (IDC_CRDY|IDC_DRDY))
			;
		if (idcaddr->idccsr & IDC_ERR) {
			printf("rb%d: seek, csr=%b\n",
				unit, idcaddr->idccsr, IDCCSR_BITS);
			return (EIO);
		}

		idcaddr->idccsr = IDC_CRDY | IDC_WRITE | unit<<8;
		if ((idcaddr->idccsr&IDC_DRDY) == 0)
			return (EFAULT);
		idcaddr->idcbar = 0;			/* start addr 0 */
		idcaddr->idcbcr = - (blk * st->nbps);
		idcaddr->idcdar = dar.dar_dar;
		idcaddr->idccsr = IDC_WRITE | unit << 8;
		while ((idcaddr->idccsr & (IDC_CRDY|IDC_DRDY))
			!= (IDC_CRDY|IDC_DRDY))
			;
		if (idcaddr->idccsr & IDC_ERR) {
			printf("rb%d: write, csr=%b\n",
				unit, idcaddr->idccsr, IDCCSR_BITS);
			return (EIO);
		}

		start += blk * st->nbps;
		num -= blk;
	}
	return (0);
}
 
idcsize(dev)
	dev_t dev;
{
	int unit = idcunit(dev);
	struct uba_device *ui;
	struct idcst *st;

	if (unit >= NRB || (ui = idcdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (-1);
	st = &idcst[ui->ui_type];
	return (st->sizes[minor(dev) & 07].nblocks);
}
#endif
