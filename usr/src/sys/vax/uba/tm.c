/*	tm.c	4.15	%G%	*/

#include "tm.h"
#if NTM03 > 0
/*
 * TM tape driver
 *
 * THIS DRIVER HAS NOT BEEN TESTED WITH MORE THAN ONE TRANSPORT.
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
#include "../h/vm.h"
#include "../h/uba.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/cmap.h"
#include "../h/cpu.h"

#include "../h/tmreg.h"

struct	buf	ctmbuf[NTM11];
struct	buf	rtmbuf[NTM11];

int	tmprobe(), tmslave(), tmattach(), tmdgo(), tmintr();
struct	uba_minfo *tmminfo[NTM03];
struct	uba_dinfo *tmdinfo[NTM11];
struct	buf tmutab[NTM11];
#ifdef notyet
struct	uba_dinfo *tmip[NTM03][4];
#endif
u_short	tmstd[] = { 0772520, 0 };
struct	uba_driver tmdriver =
  { tmprobe, tmslave, tmattach, tmdgo, tmstd, "mtm", tmdinfo, "tm", tmminfo };

/* bits in minor device */
#define	TMUNIT(dev)	(minor(dev)&03)
#define	T_NOREWIND	04
#define	T_1600BPI	08

#define	INF	(daddr_t)1000000L

/*
 * Software state per tape transport.
 */
struct	tm_softc {
	char	sc_openf;	/* lock against multiple opens */
	char	sc_lastiow;	/* last op was a write */
	daddr_t	sc_blkno;	/* block number, for block device tape */
	daddr_t	sc_nxrec;	/* desired block position */
	u_short	sc_erreg;	/* copy of last erreg */
	u_short	sc_dsreg;	/* copy of last dsreg */
	short	sc_resid;	/* copy of last bc */
} tm_softc[NTM03];

/*
 * States for um->um_tab.b_active, the
 * per controller state flag.
 */
#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */
#define	SREW	4		/* sending a drive rewind */

/* WE CURRENTLY HANDLE REWINDS PRIMITIVELY, BUSYING OUT THE CONTROLLER */
/* DURING THE REWIND... IF WE EVER GET TWO TRANSPORTS, WE CAN DEBUG MORE */
/* SOPHISTICATED LOGIC... THIS SIMPLE CODE AT LEAST MAY WORK. */

/*
 * Determine if there is a controller for
 * a tm at address reg.  Our goal is to make the
 * device interrupt.
 */
tmprobe(reg)
	caddr_t reg;
{
	register int br, cvec;

#ifdef lint
	br = 0; br = cvec; cvec = br;
#endif
	((struct device *)reg)->tmcs = TM_IE;
	/*
	 * If this is a tm03, it ought to have interrupted
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

/*
 * Due to a design flaw, we cannot ascertain if the tape
 * exists or not unless it is on line - ie: unless a tape is
 * mounted. This is too servere a restriction to bear,
 * so all units are assumed to exist.
 */
/*ARGSUSED*/
tmslave(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{

	return (1);
}

/*
 * Record attachment of the unit to the controller port.
 */
/*ARGSUSED*/
tmattach(ui)
	struct uba_dinfo *ui;
{

#ifdef notyet
	tmip[ui->ui_ctlr][ui->ui_slave] = ui;
#endif
}

/*
 * Open the device.  Tapes are unique open
 * devices, so we refuse if it is already open.
 * We also check that a tape is available, and
 * don't block waiting here.
 */
tmopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct uba_dinfo *ui;
	register struct tm_softc *sc;

	unit = TMUNIT(dev);
	if (unit>=NTM11 || (sc = &tm_softc[unit])->sc_openf ||
	    (ui = tmdinfo[unit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	tmcommand(dev, TM_SENSE, 1);
	if ((sc->sc_erreg&(TM_SELR|TM_TUR)) != (TM_SELR|TM_TUR)) {
		uprintf("tape not online\n");
		u.u_error = EIO;
		return;
	}
	if ((flag&(FREAD|FWRITE)) == FWRITE && sc->sc_erreg&TM_WRL) {
		uprintf("tape write protected\n");
		u.u_error = EIO;
		return;
	}
	sc->sc_openf = 1;
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_lastiow = 0;
	sc->sc_openf = 1;
	return;
}

/*
 * Close tape device.
 *
 * If tape was open for writing or last operation was
 * a write, then write two EOF's and backspace over the last one.
 * Unless this is a non-rewinding special file, rewind the tape.
 * Make the tape available to others.
 */
tmclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct tm_softc *sc = &tm_softc[TMUNIT(dev)];

	if (flag == FWRITE || (flag&FWRITE) && sc->sc_lastiow) {
		tmcommand(dev, TM_WEOF, 1);
		tmcommand(dev, TM_WEOF, 1);
		tmcommand(dev, TM_SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		tmcommand(dev, TM_REW, 1);
	sc->sc_openf = 0;
}

/*
 * Execute a command on the tape drive
 * a specified number of times.
 */
tmcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;

	bp = &ctmbuf[TMUNIT(dev)];
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

/*
 * Decipher a tape operation and do what is needed
 * to see that it happens.
 */
tmstrategy(bp)
	register struct buf *bp;
{
	int unit = TMUNIT(bp->b_dev);
	register struct uba_minfo *um;
	register struct buf *dp;
	register struct tm_softc *sc = &tm_softc[unit];

	/*
	 * Put transfer at end of unit queue
	 */
	dp = &tmutab[unit];
	bp->av_forw = NULL;
	(void) spl5();
	if (dp->b_actf == NULL) {
		dp->b_actf = bp;
		/*
		 * Transport not already active...
		 * put at end of controller queue.
		 */
		dp->b_forw = NULL;
		um = tmdinfo[unit]->ui_mi;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
	} else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	/*
	 * If the controller is not busy, get
	 * it going.
	 */
	if (um->um_tab.b_active == 0)
		tmstart(um);
	(void) spl0();
}

/*
 * Start activity on a tm controller.
 */
tmstart(um)
	register struct uba_minfo *um;
{
	register struct buf *bp, *dp;
	register struct device *addr = (struct device *)um->um_addr;
	register struct tm_softc *sc;
	register struct uba_dinfo *ui;
	int unit, cmd;
	daddr_t blkno;

	/*
	 * Look for an idle transport on the controller.
	 */
loop:
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	unit = TMUNIT(bp->b_dev);
	ui = tmdinfo[unit];
	/*
	 * Record pre-transfer status (e.g. for TM_SENSE)
	 */
	sc = &tm_softc[unit];
	addr = (struct device *)um->um_addr;
	addr->tmcs = (ui->ui_slave << 8);
	sc->sc_dsreg = addr->tmcs;
	sc->sc_erreg = addr->tmer;
	sc->sc_resid = addr->tmbc;
	/*
	 * Default is that last command was NOT a write command;
	 * if we do a write command we will notice this in tmintr().
	 */
	sc->sc_lastiow = 1;
	if (sc->sc_openf < 0 || (addr->tmcs&TM_CUR) == 0) {
		/*
		 * Have had a hard error on this (non-raw) tape,
		 * or the tape unit is now unavailable (e.g. taken off
		 * line).
		 */
		bp->b_flags |= B_ERROR;
		goto next;
	}
	/*
	 * If operation is not a control operation,
	 * check for boundary conditions.
	 */
	if (bp != &ctmbuf[unit]) {
		if (dbtofsb(bp->b_blkno) > sc->sc_nxrec) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;		/* past EOF */
			goto next;
		}
		if (dbtofsb(bp->b_blkno) == sc->sc_nxrec &&
		    bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);			/* at EOF */
			goto next;
		}
		if ((bp->b_flags&B_READ) == 0)
			/* write sets EOF */
			sc->sc_nxrec = dbtofsb(bp->b_blkno) + 1;
	}
	/*
	 * Set up the command, and then if this is a mt ioctl,
	 * do the operation using, for TM_SFORW and TM_SREV, the specified
	 * operation count.
	 */
	cmd = TM_IE | TM_GO | (ui->ui_slave << 8);
	if ((minor(bp->b_dev) & T_1600BPI) == 0)
		cmd |= TM_D800;
	if (bp == &ctmbuf[unit]) {
		if (bp->b_command == TM_SENSE)
			goto next;
		cmd |= bp->b_command;
		um->um_tab.b_active =
		    bp->b_command == TM_REW ? SREW : SCOM;
		if (bp->b_command == TM_SFORW || bp->b_command == TM_SREV)
			addr->tmbc = bp->b_repcnt;
		addr->tmcs = cmd;
		return;
	}
	/*
	 * If the data transfer command is in the correct place,
	 * set up all the registers except the csr, and give
	 * control over to the UNIBUS adapter routines, to
	 * wait for resources to start the i/o.
	 */
	if ((blkno = sc->sc_blkno) == dbtofsb(bp->b_blkno)) {
		addr->tmbc = -bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0) {
			if (um->um_tab.b_errcnt)
				cmd |= TM_WIRG;
			else
				cmd |= TM_WCOM;
		} else
			cmd |= TM_RCOM;
		um->um_tab.b_active = SIO;
		um->um_cmd = cmd;
		ubago(ui);
		return;
	}
	/*
	 * Block tape positioned incorrectly;
	 * seek forwards or backwards to the correct spot.
	 */
	um->um_tab.b_active = SSEEK;
	if (blkno < dbtofsb(bp->b_blkno)) {
		cmd |= TM_SFORW;
		addr->tmbc = blkno - dbtofsb(bp->b_blkno);
	} else {
		cmd |= TM_SREV;
		addr->tmbc = dbtofsb(bp->b_blkno) - blkno;
	}
	addr->tmcs = cmd;
	return;

next:
	/*
	 * Done with this operation due to error or
	 * the fact that it doesn't do anything.
	 * Release UBA resources (if any), dequeue
	 * the transfer and continue processing this slave.
	 */
	if (um->um_ubinfo)
		ubadone(um);
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

/*
 * The UNIBUS resources we needed have been
 * allocated to us; start the device.
 */
tmdgo(um)
	register struct uba_minfo *um;
{
	register struct device *addr = (struct device *)um->um_addr;

	addr->tmba = um->um_ubinfo;
	addr->tmcs = um->um_cmd | ((um->um_ubinfo >> 12) & 0x30);
}

/*
 * Tm interrupt routine.
 */
/*ARGSUSED*/
tmintr(tm03)
	int tm03;
{
	struct buf *dp;
	register struct buf *bp;
	register struct uba_minfo *um = tmminfo[tm03];
	register struct device *addr = (struct device *)tmdinfo[tm03]->ui_addr;
	register struct tm_softc *sc;
	int unit;
	register state;

	/*
	 * If last command was a rewind, and tape is still
	 * rewinding, wait for the rewind complete interrupt.
	 */
	if (um->um_tab.b_active == SREW) {
		um->um_tab.b_active = SCOM;
		if (addr->tmer&TM_RWS)
			return;
	}
	/*
	 * An operation completed... record status
	 */
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	bp = dp->b_actf;
	unit = TMUNIT(bp->b_dev);
	sc = &tm_softc[unit];
	sc->sc_dsreg = addr->tmcs;
	sc->sc_erreg = addr->tmer;
	sc->sc_resid = addr->tmbc;
	if ((bp->b_flags & B_READ) == 0)
		sc->sc_lastiow = 1;
	state = um->um_tab.b_active;
	um->um_tab.b_active = 0;
	/*
	 * Check for errors.
	 */
	if (addr->tmcs&TM_ERR) {
		while (addr->tmer & TM_SDWN)
			;			/* await settle down */
		/*
		 * If we hit the end of the tape update our position.
		 */
		if (addr->tmer&TM_EOF) {
			tmseteof(bp);		/* set blkno and nxrec */
			state = SCOM;		/* force completion */
			/*
			 * Stuff bc so it will be unstuffed correctly
			 * later to get resid.
			 */
			addr->tmbc = -bp->b_bcount;
			goto opdone;
		}
		/*
		 * If we were reading and the only error was that the
		 * record was to long, then we don't consider this an error.
		 */
		if ((bp->b_flags&B_READ) &&
		    (addr->tmer&(TM_HARD|TM_SOFT)) == TM_RLE)
			goto ignoreerr;
		/*
		 * If error is not hard, and this was an i/o operation
		 * retry up to 8 times.
		 */
		if ((addr->tmer&TM_HARD)==0 && state==SIO) {
			if (++um->um_tab.b_errcnt < 7) {
/* SHOULD CHECK THAT RECOVERY WORKS IN THIS CASE */
/* AND THEN ONLY PRINT IF errcnt==7 */
				if((addr->tmer&TM_SOFT) == TM_NXM)
					printf("TM UBA late error\n");
				sc->sc_blkno++;
				ubadone(um);
				goto opcont;
			}
		} else
			/*
			 * Hard or non-i/o errors on non-raw tape
			 * cause it to close.
			 */
			if (sc->sc_openf>0 && bp != &rtmbuf[unit])
				sc->sc_openf = -1;
		/*
		 * Couldn't recover error
		 */
		deverror(bp, sc->sc_erreg, sc->sc_dsreg);
		bp->b_flags |= B_ERROR;
		goto opdone;
	}
	/*
	 * Advance tape control FSM.
	 */
ignoreerr:
	switch (state) {

	case SIO:
		/*
		 * Read/write increments tape block number
		 */
		sc->sc_blkno++;
		goto opdone;

	case SCOM:
		/*
		 * Unless special operation, op completed.
		 */
		if (bp != &ctmbuf[unit])
			goto opdone;
		/*
		 * Operation on block device...
		 * iterate operations which don't repeat
		 * for themselves in the hardware; for forward/
		 * backward space record update the current position.
		 */
		switch (bp->b_command) {

		case TM_SFORW:
			sc->sc_blkno -= bp->b_repcnt;
			goto opdone;

		case TM_SREV:
			sc->sc_blkno += bp->b_repcnt;
			goto opdone;

		default:
			if (++bp->b_repcnt < 0)
				goto opcont;
			goto opdone;
		}

	case SSEEK:
		sc->sc_blkno = dbtofsb(bp->b_blkno);
		goto opcont;

	default:
		panic("tmintr");
	}
opdone:
	/*
	 * Reset error count and remove
	 * from device queue.
	 */
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	bp->b_resid = -addr->tmbc;
	ubadone(um);
	iodone(bp);
	/*
	 * Circulate slave to end of controller
	 * queue to give other slaves a chance.
	 */
	um->um_tab.b_actf = dp->b_forw;
	if (dp->b_actf) {
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
	}
	if (um->um_tab.b_actf == 0)
		return;
opcont:
	tmstart(um);
}

tmseteof(bp)
	register struct buf *bp;
{
	register int unit = TMUNIT(bp->b_dev);
	register struct device *addr = 
	    (struct device *)tmdinfo[unit]->ui_addr;
	register struct tm_softc *sc = &tm_softc[unit];

	if (bp == &ctmbuf[unit]) {
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
	dev_t dev;
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf[TMUNIT(dev)], dev, B_READ, minphys);
}

tmwrite(dev)
	dev_t dev;
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf[TMUNIT(dev)], dev, B_WRITE, minphys);
}

tmphys(dev)
	dev_t dev;
{
	register daddr_t a;
	register struct tm_softc *sc = &tm_softc[TMUNIT(dev)];

	a = dbtofsb(u.u_offset >> 9);
	sc->sc_blkno = a;
	sc->sc_nxrec = a + 1;
}

tmreset(uban)
	int uban;
{
	int printed = 0;
	register struct uba_minfo *um;
	register tm03, unit;
	register struct uba_dinfo *ui;
	register struct buf *dp;

	for (tm03 = 0; tm03 < NTM03; tm03++) {
		if ((um = tmminfo[tm03]) == 0 || um->um_alive == 0 ||
		   um->um_ubanum != uban)
			continue;
		if (printed == 0) {
			printf(" tm");
			DELAY(2000000);		/* time to self test */
			printed = 1;
		}
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone(um);
		}
		((struct device *)(um->um_addr))->tmcs = TM_DCLR;
		for (unit = 0; unit < NTM11; unit++) {
			if ((ui = tmdinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0)
				continue;
			dp = &tmutab[unit];
			dp->b_active = 0;
			dp->b_forw = 0;
			if (um->um_tab.b_actf == NULL)
				um->um_tab.b_actf = dp;
			else
				um->um_tab.b_actl->b_forw = dp;
			um->um_tab.b_actl = dp;
			tm_softc[unit].sc_openf = -1;
		}
		tmstart(um);
	}
}

/*ARGSUSED*/
tmioctl(dev, cmd, addr, flag)
	caddr_t addr;
	dev_t dev;
{
	int unit = TMUNIT(dev);
	register struct tm_softc *sc = &tm_softc[unit];
	register struct buf *bp = &ctmbuf[unit];
	register callcount;
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static tmops[] =
	   {TM_WEOF,TM_SFORW,TM_SREV,TM_SFORW,TM_SREV,TM_REW,TM_OFFL,TM_SENSE};

	switch (cmd) {
		case MTIOCTOP:	/* tape operation */
		if (copyin((caddr_t)addr, (caddr_t)&mtop, sizeof(mtop))) {
			u.u_error = EFAULT;
			return;
		}
		switch(mtop.mt_op) {
		case MTWEOF:
			callcount = mtop.mt_count;
			fcount = 1;
			break;
		case MTFSF: case MTBSF:
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
		if (callcount <= 0 || fcount <= 0) {
			u.u_error = ENXIO;
			return;
		}
		while (--callcount >= 0) {
			tmcommand(dev, tmops[mtop.mt_op], fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    bp->b_resid) {
				u.u_error = EIO;
				break;
			}
			if ((bp->b_flags&B_ERROR) || sc->sc_erreg&TM_BOT)
				break;
		}
		geterror(bp);
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
	addr->tmcs = TM_DCLR | TM_GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tmdwrite(start, blk, addr, up);
		start += blk;
		num -= blk;
	}
	tmeof(addr);
	tmeof(addr);
	tmwait(addr);
	addr->tmcs = TM_REW | TM_GO;
	tmwait(addr);
	return (0);
}

tmdwrite(dbuf, num, addr, up)
	register dbuf, num;
	register struct device *addr;
	struct uba_regs *up;
{
	register struct pte *io;
	register int npf;

	tmwait(addr);
	io = up->uba_map;
	npf = num+1;
	while (--npf != 0)
		 *(int *)io++ = (dbuf++ | (1<<UBA_DPSHIFT) | UBA_MRV);
	*(int *)io = 0;
	addr->tmbc = -(num*NBPG);
	addr->tmba = 0;
	addr->tmcs = TM_WCOM | TM_GO;
}

tmwait(addr)
	register struct device *addr;
{
	register s;

	do
		s = addr->tmcs;
	while ((s & TM_CUR) == 0);
}

tmeof(addr)
	struct device *addr;
{

	tmwait(addr);
	addr->tmcs = TM_WEOF | TM_GO;
}
#endif
