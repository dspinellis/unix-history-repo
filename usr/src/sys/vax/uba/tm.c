/*	tm.c	4.12	%G%	*/

#include "tm.h"
#if NTM03 > 0
/*
 * TM tape driver
 *
 * THIS HANDLES ONLY ONE DRIVE ON ONE CONTROLER, AS WE HAVE NO
 * WAY TO TEST MULTIPLE TRANSPORTS.
 */
#define	DELAY(N)		{ register int d = N; while (--d > 0); }
#include "../h/param.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/conf.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/cpu.h"

#include "../h/tmreg.h"

struct	buf	ctmbuf;
struct	buf	rtmbuf;

int	tmcntrlr(), tmslave(), tmdgo(), tmintr();
struct	uba_minfo *tmminfo[NTM03];
struct	uba_dinfo *tmdinfo[NTM11];
u_short	tmstd[] = { 0772520, 0 };
struct	uba_driver tmdriver =
	{ tmcntrlr, tmslave, tmdgo, 0, tmstd, "tm", tmdinfo, tmminfo };

/* bits in minor device */
#define	T_NOREWIND	04
#define	T_1600BPI	08

#define	INF	(daddr_t)1000000L

struct	tm_softc {
	char	sc_openf;
	char	sc_flags;
	daddr_t	sc_blkno;
	daddr_t	sc_nxrec;
	u_short	sc_erreg;
	u_short	sc_dsreg;
	short	sc_resid;
	int	sc_ubinfo;
} tm_softc[NTM03];

#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */

#define	LASTIOW 1		/* last op was a write */
#define	WAITREW	2		/* someone is waiting for a rewind */

/*
 * Determine if there is a controller for
 * a tm at address reg.  Our goal is to make the
 * device interrupt.
 */
tmcntrlr(um, reg)
	struct uba_minfo *um;
	caddr_t reg;
{
	register int br, cvec;

	((struct device *)reg)->tmcs = IENABLE;
	/*
	 * If this is a tm03/tc11, it ought to have interrupted
	 * by now, if it isn't (ie: it is a ts04) then we just
	 * hope that it didn't interrupt, so autoconf will ignore it.
	 * Just in case, we will reference one
	 * of the more distant registers, and hope for a machine
	 * check, or similar disaster if this is a ts.
	 *
	 * Note: on an 11/780, badaddr will just generate
	 * a uba error for a ts; but our caller will notice that
	 * so we won't check for it.
	 */
	if (badaddr(&((struct device *)reg)->tmrd, 2))
		return (0);
	return (1);
}

tmslave(ui, reg, slaveno)
	struct uba_dinfo *ui;
	caddr_t reg;
{

	/*
	 * Due to a design flaw, we cannot ascertain if the tape
	 * exists or not unless it is on line - ie: unless a tape is
	 * mounted. This is too servere a restriction to bear.
	 * As we can only handle one tape, we might just as well insist
	 * that it be slave #0, and just assume that it exists.
	 * Something better will have to be done if you have two
	 * tapes on one controller, or two controllers
	 */
	if (slaveno != 0 || tmdinfo[0])
		return(0);
	return (1);
}

tmopen(dev, flag)
	dev_t dev;
	int flag;
{
	register ds, unit;
	register struct uba_dinfo *ui;
	register struct tm_softc *sc = &tm_softc[0];

	tmminfo[0]->um_tab.b_flags |= B_TAPE;
	unit = minor(dev)&03;
	if (unit>=NTM11 || sc->sc_openf || (ui = tmdinfo[0]) == 0 || ui->ui_alive==0) {
		u.u_error = ENXIO;		/* out of range or open */
		return;
	}
	tcommand(dev, NOP, 1);
	if ((sc->sc_erreg&SELR) == 0) {
		u.u_error = EIO;
		goto eio;
	}
	sc->sc_openf = 1;
	if (sc->sc_erreg&RWS)
		tmwaitrws(dev);			/* wait for rewind complete */
	while (sc->sc_erreg&SDWN)
		tcommand(dev, NOP, 1);		/* await settle down */
	if ((sc->sc_erreg&TUR)==0 ||
	    ((flag&(FREAD|FWRITE)) == FWRITE && (sc->sc_erreg&WRL))) {
		((struct device *)ui->ui_addr)->tmcs = DCLR|GO;
		u.u_error = EIO;		/* offline or write protect */
	}
	if (u.u_error != 0) {
		sc->sc_openf = 0;
		if (u.u_error == EIO)
eio:
			uprintf("tape offline or protected\n");
		return;
	}
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_flags = 0;
	sc->sc_openf = 1;
}

tmwaitrws(dev)
	register dev;
{
	register struct device *addr =
	    (struct device *)tmdinfo[0]->ui_addr;
	register struct tm_softc *sc = &tm_softc[0];

	spl5();
	for (;;) {
		if ((addr->tmer&RWS) == 0) {
			spl0();		/* rewind complete */
			return;
		}
		sc->sc_flags |= WAITREW;
		sleep((caddr_t)&sc->sc_flags, PRIBIO);
	}
}

tmclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct tm_softc *sc = &tm_softc[0];

	if (flag == FWRITE || ((flag&FWRITE) && (sc->sc_flags&LASTIOW))) {
		tcommand(dev, WEOF, 1);
		tcommand(dev, WEOF, 1);
		tcommand(dev, SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		tcommand(dev, REW, 1);
	sc->sc_openf = 0;
}

tcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;

	bp = &ctmbuf;
	(void) spl5();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	(void) spl0();
	bp->b_dev = dev;
	bp->b_repcnt = -count;
	bp->b_command = com;
	bp->b_blkno = 0;
	tmstrategy(bp);
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

tmstrategy(bp)
	register struct buf *bp;
{
	register daddr_t *p;
	register struct buf *tmi;

	tmwaitrws(bp->b_dev);
	if (bp != &ctmbuf) {
		p = &tm_softc[0].sc_nxrec;
		if (dbtofsb(bp->b_blkno) > *p) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;		/* past EOF */
			iodone(bp);
			return;
		} else if (dbtofsb(bp->b_blkno) == *p && bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);			/* at EOF */
			iodone(bp);
			return;
		} else if ((bp->b_flags&B_READ) == 0)
			*p = dbtofsb(bp->b_blkno) + 1;	/* write sets EOF */
	}
	bp->av_forw = NULL;
	(void) spl5();
	tmi = &tmminfo[0]->um_tab;
	if (tmi->b_actf == NULL)
		tmi->b_actf = bp;
	else
		tmi->b_actl->av_forw = bp;
	tmi->b_actl = bp;
	if (tmi->b_active == 0)
		tmstart();
	(void) spl0();
}

tmstart()
{
	register struct buf *bp;
	register struct uba_minfo *um = tmminfo[0];
	register struct uba_dinfo *ui;
	register struct device *addr;
	register struct tm_softc *sc = &tm_softc[0];
	int cmd, s;
	daddr_t blkno;

loop:
	if ((bp = um->um_tab.b_actf) == 0)
		return;
	ui = tmdinfo[0];
	addr = (struct device *)ui->ui_addr;
	sc->sc_dsreg = addr->tmcs;
	sc->sc_erreg = addr->tmer;
	sc->sc_resid = addr->tmbc;
	sc->sc_flags &= ~LASTIOW;
	if (sc->sc_openf < 0 || (addr->tmcs&CUR) == 0) {
		/* sc->sc_openf = -1; ??? */
		bp->b_flags |= B_ERROR;		/* hard error'ed or !SELR */
		goto next;
	}
	cmd = IENABLE | GO;
	if ((minor(bp->b_dev) & T_1600BPI) == 0)
		cmd |= D800;
	if (bp == &ctmbuf) {
		if (bp->b_command == NOP)
			goto next;		/* just get status */
		else {
			cmd |= bp->b_command;
			um->um_tab.b_active = SCOM;
			if (bp->b_command == SFORW || bp->b_command == SREV)
				addr->tmbc = bp->b_repcnt;
			addr->tmcs = cmd;
			return;
		}
	}
	if ((blkno = sc->sc_blkno) == dbtofsb(bp->b_blkno)) {
		addr->tmbc = -bp->b_bcount;
		s = spl6();
		if (sc->sc_ubinfo == 0)
			sc->sc_ubinfo = ubasetup(ui->ui_ubanum, bp, 1);
		splx(s);
		if ((bp->b_flags&B_READ) == 0) {
			if (um->um_tab.b_errcnt)
				cmd |= WIRG;
			else
				cmd |= WCOM;
		} else
			cmd |= RCOM;
		cmd |= (sc->sc_ubinfo >> 12) & 0x30;
		um->um_tab.b_active = SIO;
		addr->tmba = sc->sc_ubinfo;
		addr->tmcs = cmd; 
		return;
	}
	um->um_tab.b_active = SSEEK;
	if (blkno < dbtofsb(bp->b_blkno)) {
		cmd |= SFORW;
		addr->tmbc = blkno - dbtofsb(bp->b_blkno);
	} else {
		cmd |= SREV;
		addr->tmbc = dbtofsb(bp->b_blkno) - blkno;
	}
	addr->tmcs = cmd;
	return;

next:
	ubarelse(ui->ui_ubanum, &sc->sc_ubinfo);
	um->um_tab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

tmdgo()
{

}

/*ARGSUSED*/
tmintr(d)
	int d;
{
	register struct buf *bp;
	register struct uba_minfo *um = tmminfo[0];
	register struct device *addr = (struct device *)tmdinfo[0]->ui_addr;
	register struct tm_softc *sc = &tm_softc[0];
	register state;

	if (sc->sc_flags&WAITREW && (addr->tmer&RWS) == 0) {
		sc->sc_flags &= ~WAITREW;
		wakeup((caddr_t)&sc->sc_flags);
	}
	if ((bp = um->um_tab.b_actf) == NULL)
		return;
	sc->sc_dsreg = addr->tmcs;
	sc->sc_erreg = addr->tmer;
	sc->sc_resid = addr->tmbc;
	if ((bp->b_flags & B_READ) == 0)
		sc->sc_flags |= LASTIOW;
	state = um->um_tab.b_active;
	um->um_tab.b_active = 0;
	if (addr->tmcs&ERROR) {
		while(addr->tmer & SDWN)
			;			/* await settle down */
		if (addr->tmer&EOF) {
			tmseteof(bp);	/* set blkno and nxrec */
			state = SCOM;
			addr->tmbc = -bp->b_bcount;
			goto errout;
		}
		if ((bp->b_flags&B_READ) && (addr->tmer&(HARD|SOFT)) == RLE)
			goto out;
		if ((addr->tmer&HARD)==0 && state==SIO) {
			if (++um->um_tab.b_errcnt < 7) {
				if((addr->tmer&SOFT) == NXM)
					printf("TM UBA late error\n");
				sc->sc_blkno++;
				ubarelse(um->um_ubanum, &sc->sc_ubinfo);
				tmstart();
				return;
			}
		} else if (sc->sc_openf>0 && bp != &rtmbuf)
			sc->sc_openf = -1;
		deverror(bp, sc->sc_erreg, sc->sc_dsreg);
		bp->b_flags |= B_ERROR;
		state = SIO;
	}
out:
	switch (state) {

	case SIO:
		sc->sc_blkno++;
		/* fall into ... */

	case SCOM:
		if (bp == &ctmbuf) {
			switch (bp->b_command) {
			case SFORW:
				sc->sc_blkno -= bp->b_repcnt;
				break;

			case SREV:
				sc->sc_blkno += bp->b_repcnt;
				break;

			default:
				if (++bp->b_repcnt < 0) {
					tmstart();	/* continue */
					return;
				}
			}
		}
errout:
		um->um_tab.b_errcnt = 0;
		um->um_tab.b_actf = bp->av_forw;
		bp->b_resid = -addr->tmbc;
		ubarelse(um->um_ubanum, &sc->sc_ubinfo);
		iodone(bp);
		break;

	case SSEEK:
		sc->sc_blkno = dbtofsb(bp->b_blkno);
		break;

	default:
		return;
	}
	tmstart();
}

tmseteof(bp)
	register struct buf *bp;
{
	register struct device *addr = 
	    (struct device *)tmdinfo[0]->ui_addr;
	register struct tm_softc *sc = &tm_softc[0];

	if (bp == &ctmbuf) {
		if (sc->sc_blkno > dbtofsb(bp->b_blkno)) {
			/* reversing */
			sc->sc_nxrec = dbtofsb(bp->b_blkno) - addr->tmbc;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			/* spacing forward */
			sc->sc_blkno = dbtofsb(bp->b_blkno) + addr->tmbc;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
		return;
	} 
	/* eof on read */
	sc->sc_nxrec = dbtofsb(bp->b_blkno);
}

tmread(dev)
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_READ, minphys);
}

tmwrite(dev)
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_WRITE, minphys);
}

tmphys(dev)
{
	register daddr_t a;
	register struct tm_softc *sc = &tm_softc[0];

	a = dbtofsb(u.u_offset >> 9);
	sc->sc_blkno = a;
	sc->sc_nxrec = a + 1;
}

/*ARGSUSED*/
tmioctl(dev, cmd, addr, flag)
	caddr_t addr;
	dev_t dev;
{
	register callcount;
	register struct tm_softc *sc = &tm_softc[0];
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static tmops[] = {WEOF, SFORW, SREV, SFORW, SREV, REW, OFFL, NOP};

	switch(cmd) {
		case MTIOCTOP:	/* tape operation */
		if (copyin((caddr_t)addr, (caddr_t)&mtop, sizeof(mtop))) {
			u.u_error = EFAULT;
			return;
		}
		switch(mtop.mt_op) {
		case MTWEOF: case MTFSF: case MTBSF:
			callcount = mtop.mt_count;
			fcount = INF;
			break;
		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop.mt_count;
			break;
		case MTREW: case MTOFFL: case MTNOP:
			callcount = 1;
			fcount = 1;
			break;
		default:
			u.u_error = ENXIO;
			return;
		}
		if (callcount <= 0 || fcount <= 0)
			u.u_error = ENXIO;
		else while (--callcount >= 0) {
			tcommand(dev, tmops[mtop.mt_op], fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    ctmbuf.b_resid) {
				u.u_error = EIO;
				break;
			}
			if ((ctmbuf.b_flags&B_ERROR) ||
			    sc->sc_erreg&BOT)
				break;
		}
		geterror(&ctmbuf);
		return;
	case MTIOCGET:
		mtget.mt_dsreg = sc->sc_dsreg;
		mtget.mt_erreg = sc->sc_erreg;
		mtget.mt_resid = sc->sc_resid;
		if (copyout((caddr_t)&mtget, addr, sizeof(mtget)))
			u.u_error = EFAULT;
		return;
	default:
		u.u_error = ENXIO;
	}
}

#define	DBSIZE	20

tmdump()
{
	register struct uba_dinfo *ui;
	register struct uba_regs *up;
	register struct device *addr;
	int blk, num;
	int start;

	start = 0;
	num = maxfree;
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	if (tmdinfo[0] == 0) {
		printf("dna\n");
		return (-1);
	}
	ui = phys(tmdinfo[0], struct uba_dinfo *);
	up = phys(ui->ui_hd, struct uba_hd *)->uh_physuba;
#if VAX780
	if (cpu == VAX_780)
		ubainit(up);
#endif
	DELAY(1000000);
	addr = (struct device *)ui->ui_physaddr;
	tmwait(addr);
	addr->tmcs = DCLR | GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tmdwrite(start, blk, addr, up);
		start += blk;
		num -= blk;
	}
	tmeof(addr);
	tmeof(addr);
	tmwait(addr);
	addr->tmcs = REW | GO;
	tmwait(addr);
	return (0);
}

tmdwrite(buf, num, addr, up)
	register buf, num;
	register struct device *addr;
	struct uba_regs *up;
{
	register struct pte *io;
	register int npf;

	tmwait(addr);
	io = up->uba_map;
	npf = num+1;
	while (--npf != 0)
		 *(int *)io++ = (buf++ | (1<<UBA_DPSHIFT) | UBA_MRV);
	*(int *)io = 0;
	addr->tmbc = -(num*NBPG);
	addr->tmba = 0;
	addr->tmcs = WCOM | GO;
}

tmwait(addr)
	register struct device *addr;
{
	register s;

	do
		s = addr->tmcs;
	while ((s & CUR) == 0);
}

tmeof(addr)
	struct device *addr;
{

	tmwait(addr);
	addr->tmcs = WEOF | GO;
}
#endif
