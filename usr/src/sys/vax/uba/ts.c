/*	ts.c	4.11	81/04/15	*/

#include "ts.h"
#include "te.h"
#if NTS > 0
#define printd if(tsdebug)printf
int tsdebug;
/*
 * TS11 tape driver
 *
 * TODO:
 *	test driver with more than one controller
 *	test reset code
 *	test dump code
 *	test rewinds without hanging in driver
 *	what happens if you offline tape during rewind?
 *	test using file system on tape
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/conf.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/cmap.h"
#include "../h/cpu.h"

#include "../h/tsreg.h"

/*
 * There is a ctsbuf per tape controller.
 * It is used as the token to pass to the internal routines
 * to execute tape ioctls.
 * In particular, when the tape is rewinding on close we release
 * the user process but any further attempts to use the tape drive
 * before the rewind completes will hang waiting for ctsbuf.
 */
struct	buf	ctsbuf[NTS];

/*
 * Raw tape operations use rtsbuf.  The driver
 * notices when rtsbuf is being used and allows the user
 * program to continue after errors and read records
 * not of the standard length (BSIZE).
 */
struct	buf	rtsbuf[NTS];

/*
 * Driver unibus interface routines and variables.
 */
int	tsprobe(), tsslave(), tsattach(), tsdgo(), tsintr();
struct	uba_ctlr *tsminfo[NTS];
struct	uba_device *tsdinfo[NTS];
struct buf	tsbuf[NTS];
u_short	tsstd[] = { 0772520, 0 };
/*** PROBABLY DON'T NEED ALL THESE SINCE CONTROLLER == DRIVE ***/
struct	uba_driver zsdriver =
 { tsprobe, tsslave, tsattach, tsdgo, tsstd, "ts", tsdinfo, "zs", tsminfo, 0 };

/* bits in minor device */
#define	TSUNIT(dev)	(minor(dev)&03)
#define	T_NOREWIND	04

#define	INF	(daddr_t)1000000L

/*
 * Software state per tape transport.
 * Also contains hardware state in message packets.
 *
 * 1. A tape drive is a unique-open device; we refuse opens when it is already.
 * 2. We keep track of the current position on a block tape and seek
 *    before operations by forward/back spacing if necessary.
 * 3. We remember if the last operation was a write on a tape, so if a tape
 *    is open read write and the last thing done is a write we can
 *    write a standard end of tape mark (two eofs).
 * 4. We remember the status registers after the last command, using
 *    then internally and returning them to the SENSE ioctl.
 */
struct	ts_softc {
	char	sc_openf;	/* lock against multiple opens */
	char	sc_lastiow;	/* last op was a write */
	short	sc_resid;	/* copy of last bc */
	daddr_t	sc_blkno;	/* block number, for block device tape */
	daddr_t	sc_nxrec;	/* position of end of tape, if known */
	struct ts_cmd sc_cmd;	/* the command packet - ADDR MUST BE 0 MOD 4 */
	struct ts_sts sc_sts;	/* status packet, for returned status */
	struct ts_char sc_char;	/* characteristics packet */
	u_short	sc_uba;		/* Unibus addr of cmd pkt for tsdb */
} ts_softc[NTS];

struct ts_softc *ts_ubaddr;	/* Unibus address of ts_softc */

/*
 * States for um->um_tab.b_active, the per controller state flag.
 * This is used to sequence control in the driver.
 */
#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */
#define	SREW	4		/* sending a drive rewind */

#if NTM > 0
/* kludge... see tm.c */
extern	havetm;
#endif
/*
 * Determine if there is a controller for
 * a ts at address reg.  Our goal is to make the
 * device interrupt.
 */
tsprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* must be r11,r10; value-result */

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	/****************/
	/*		*/
	/*  K L U D G E */
	/*		*/
	/****************/

#if NTM > 0
	if (havetm)
		return (0);
#endif
	/* IT'S TOO HARD TO MAKE THIS THING INTERRUPT
	   JUST TO FIND ITS VECTOR */
	cvec = 0224;
	br = 0x15;
}

/*
 * TS11 only supports one drive per controller;
 * check for ui_slave == 0.
 *
 * DO WE REALLY NEED THIS ROUTINE???
 */
/*ARGSUSED*/
tsslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{

	if (ui->ui_slave)	/* non-zero slave not allowed */
		return(0);
	return (1);
}

/*
 * Record attachment of the unit to the controller.
 *
 * SHOULD THIS ROUTINE DO ANYTHING???
 */
/*ARGSUSED*/
tsattach(ui)
	struct uba_device *ui;
{

}

/*
 * Open the device.  Tapes are unique open
 * devices, so we refuse if it is already open.
 * We also check that a tape is available, and
 * don't block waiting here; if you want to wait
 * for a tape you should timeout in user code.
 */
tsopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int tsunit;
	register struct uba_device *ui;
	register struct ts_softc *sc;

	tsunit = TSUNIT(dev);
	if (tsunit>=NTS || (sc = &ts_softc[tsunit])->sc_openf ||
	    (ui = tsdinfo[tsunit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	if (tsinit(tsunit)) {
		u.u_error = ENXIO;
		printd("init failed\n");
		return;
	}
	printd("init ok\n");
	tscommand(dev, TS_SENSE, 1);
	printd("sense xs0 %o\n", sc->sc_sts.s_xs0);
	if ((sc->sc_sts.s_xs0&TS_ONL) == 0 || ((flag&(FREAD|FWRITE)) ==
	    FWRITE && (sc->sc_sts.s_xs0&TS_WLK))) {
		/*
		 * Not online or write locked.
		 */
		u.u_error = EIO;
		return;
	}
	sc->sc_openf = 1;
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_lastiow = 0;
}

/*
 * Close tape device.
 *
 * If tape was open for writing or last operation was
 * a write, then write two EOF's and backspace over the last one.
 * Unless this is a non-rewinding special file, rewind the tape.
 * Make the tape available to others.
 */
tsclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct ts_softc *sc = &ts_softc[TSUNIT(dev)];

	if (flag == FWRITE || (flag&FWRITE) && sc->sc_lastiow) {
		tscommand(dev, TS_WEOF, 1);
		tscommand(dev, TS_WEOF, 1);
		tscommand(dev, TS_SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		/*
		 * 0 count means don't hang waiting for rewind complete
		 * rather ctsbuf stays busy until the operation completes
		 * preventing further opens from completing by
		 * preventing a TS_SENSE from completing.
		 */
		tscommand(dev, TS_REW, 0);
	sc->sc_openf = 0;
}

/*
 * Initialize the TS11.  Set up Unibus mapping for command
 * packets and set device characteristics.
 */
tsinit(unit)
	register int unit;
{
	register struct ts_softc *sc = &ts_softc[unit];
	register struct uba_ctlr *um = tsminfo[unit];
	register struct device *addr = (struct device *)um->um_addr;
	register int i;

	/*
	 * Map the command and message packets into Unibus
	 * address space.  We do all the command and message
	 * packets at once to minimize the amount of Unibus
	 * mapping necessary.
	 */
	if (ts_ubaddr == 0) {
		ctsbuf[unit].b_un.b_addr = (caddr_t)ts_softc;
		ctsbuf[unit].b_bcount = sizeof(ts_softc);
		i = ubasetup(um->um_ubanum, &ctsbuf[unit], 0);
		i &= 0777777;
		ts_ubaddr = (struct ts_softc *)i;
		/* MAKE SURE WE DON'T GET UNIBUS ADDRESS ZERO */
		if (ts_ubaddr == 0)
			printf("ts%d: zero ubaddr\n", unit);
	}
	/*
	 * Now initialize the TS11 controller.
	 * Set the characteristics.
	 */
	if (addr->tssr & TS_NBA) {
		addr->tssr = 0;		/* subsystem initialize */
		tswait(addr);
		i = (int)&ts_ubaddr[unit].sc_cmd;	/* Unibus addr of cmd */
		if (i&3) {
			printf("addr mod 4 != 0\n");
			return(1);
		}
		sc->sc_uba = (u_short)(i + ((i>>16)&3));
		sc->sc_char.char_addr = (int)&ts_ubaddr[unit].sc_sts;
		sc->sc_char.char_size = sizeof(struct ts_sts);
		sc->sc_char.char_mode = TS_ESS;
		sc->sc_cmd.c_cmd = TS_ACK | TS_SETCHR;
		i = (int)&ts_ubaddr[unit].sc_char;
		sc->sc_cmd.c_loba = i;
		sc->sc_cmd.c_hiba = (i>>16)&3;
		sc->sc_cmd.c_size = sizeof(struct ts_char);
		addr->tsdb = sc->sc_uba;
		tswait(addr);
/*
		printd("%o %o %o %o %o %o %o %o\n", addr->tssr, sc->sc_sts.s_sts, sc->sc_sts.s_len, sc->sc_sts.s_rbpcr, sc->sc_sts.s_xs0, sc->sc_sts.s_xs1,sc->sc_sts.s_xs1,sc->sc_sts.s_xs2,sc->sc_sts.s_xs3);
*/
		if (addr->tssr & TS_NBA)
			return(1);
	}
	return(0);
}

/*
 * Execute a command on the tape drive
 * a specified number of times.
 */
tscommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;

	bp = &ctsbuf[TSUNIT(dev)];
	(void) spl5();
	while (bp->b_flags&B_BUSY) {
		/*
		 * This special check is because B_BUSY never
		 * gets cleared in the non-waiting rewind case.
		 */
		if (bp->b_repcnt == 0 && (bp->b_flags&B_DONE))
			break;
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	(void) spl0();
	printd("command %o dev %x count %d\n", com, dev, count);
	bp->b_dev = dev;
	bp->b_repcnt = count;
	bp->b_command = com;
	bp->b_blkno = 0;
	tsstrategy(bp);
	/*
	 * In case of rewind from close, don't wait.
	 * This is the only case where count can be 0.
	 */
	if (count == 0)
		return;
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

/*
 * Queue a tape operation.
 */
tsstrategy(bp)
	register struct buf *bp;
{
	int tsunit = TSUNIT(bp->b_dev);
	register struct uba_ctlr *um;
	register struct buf *dp;

	/*
	 * Put transfer at end of controller queue
	 */
	bp->av_forw = NULL;
	um = tsdinfo[tsunit]->ui_mi;
	dp = &tsbuf[tsunit];
	(void) spl5();
	if (dp->b_actf == NULL)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	um->um_tab.b_actf = um->um_tab.b_actl = dp;
	/*
	 * If the controller is not busy, get
	 * it going.
	 */
	if (um->um_tab.b_active == 0)
		tsstart(um);
	(void) spl0();
}

/*
 * Start activity on a ts controller.
 */
tsstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp;
	register struct device *addr = (struct device *)um->um_addr;
	register struct ts_softc *sc;
	register struct ts_cmd *tc;
	register struct uba_device *ui;
	int tsunit, cmd;
	daddr_t blkno;

	/*
	 * Start the controller if there is something for it to do.
	 */
loop:
	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	tsunit = TSUNIT(bp->b_dev);
	ui = tsdinfo[tsunit];
	sc = &ts_softc[tsunit];
	tc = &sc->sc_cmd;
	/*
	 * Default is that last command was NOT a write command;
	 * if we do a write command we will notice this in tsintr().
	 */
	sc->sc_lastiow = 1;
	if (sc->sc_openf < 0 || (addr->tssr&TS_OFL)) {
		/*
		 * Have had a hard error on a non-raw tape
		 * or the tape unit is now unavailable
		 * (e.g. taken off line).
		 */
		bp->b_flags |= B_ERROR;
		goto next;
	}
	if (bp == &ctsbuf[TSUNIT(bp->b_dev)]) {
		/*
		 * Execute control operation with the specified count.
		 */
		um->um_tab.b_active =
		    bp->b_command == TS_REW ? SREW : SCOM;
		tc->c_repcnt = bp->b_repcnt;
		printd("strat: do cmd\n");
		goto dobpcmd;
	}
	/*
	 * The following checks handle boundary cases for operation
	 * on non-raw tapes.  On raw tapes the initialization of
	 * sc->sc_nxrec by tsphys causes them to be skipped normally
	 * (except in the case of retries).
	 */
	if (dbtofsb(bp->b_blkno) > sc->sc_nxrec) {
		/*
		 * Can't read past known end-of-file.
		 */
		bp->b_flags |= B_ERROR;
		bp->b_error = ENXIO;
		goto next;
	}
	if (dbtofsb(bp->b_blkno) == sc->sc_nxrec &&
	    bp->b_flags&B_READ) {
		/*
		 * Reading at end of file returns 0 bytes.
		 */
		bp->b_resid = bp->b_bcount;
		clrbuf(bp);
		goto next;
	}
	if ((bp->b_flags&B_READ) == 0)
		/*
		 * Writing sets EOF
		 */
		sc->sc_nxrec = dbtofsb(bp->b_blkno) + 1;
	/*
	 * If the data transfer command is in the correct place,
	 * set up all the registers except the csr, and give
	 * control over to the UNIBUS adapter routines, to
	 * wait for resources to start the i/o.
	 */
	if ((blkno = sc->sc_blkno) == dbtofsb(bp->b_blkno)) {
		tc->c_size = bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0)
			cmd = TS_WCOM;
		else
			cmd = TS_RCOM;
		if (um->um_tab.b_errcnt)
			cmd |= TS_RETRY;
		um->um_tab.b_active = SIO;
		tc->c_cmd = TS_ACK | TS_CVC | TS_IE | cmd;
		printd("r/w %o size %d\n", tc->c_cmd, tc->c_size);
		(void) ubago(ui);
		return;
	}
	/*
	 * Tape positioned incorrectly;
	 * set to seek forwards or backwards to the correct spot.
	 * This happens for raw tapes only on error retries.
	 */
	um->um_tab.b_active = SSEEK;
	printd("seek blkno %d b_blkno %d\n", blkno, bp->b_blkno);
	if (blkno < dbtofsb(bp->b_blkno)) {
		bp->b_command = TS_SFORW;
		tc->c_repcnt = dbtofsb(bp->b_blkno) - blkno;
	} else {
		bp->b_command = TS_SREV;
		tc->c_repcnt = blkno - dbtofsb(bp->b_blkno);
	}
dobpcmd:
	/*
	 * Do the command in bp.
	 */
	tc->c_cmd = TS_ACK | TS_CVC | TS_IE | bp->b_command;
	addr->tsdb = sc->sc_uba;
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
	um->um_tab.b_actf->b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

/*
 * The UNIBUS resources we needed have been
 * allocated to us; start the device.
 */
tsdgo(um)
	register struct uba_ctlr *um;
{
	register struct device *addr = (struct device *)um->um_addr;
	register struct ts_softc *sc = &ts_softc[um->um_ctlr];
	register int i;

	i = um->um_ubinfo & 0777777;
	printd("dgo addr %o\n", i);
	sc->sc_cmd.c_loba = i;
	sc->sc_cmd.c_hiba = (i>>16)&3;
	addr->tsdb = sc->sc_uba;
}

/*
 * Ts interrupt routine.
 */
/*ARGSUSED*/
tsintr(ts11)
	int ts11;
{
	register struct buf *bp;
	register struct uba_ctlr *um = tsminfo[ts11];
	register struct device *addr;
	register struct ts_softc *sc;
	int tsunit;
	register state;

	printd("intr\n");
	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	tsunit = TSUNIT(bp->b_dev);
	addr = (struct device *)tsdinfo[tsunit]->ui_addr;
	/*
	 * If last command was a rewind, and tape is still
	 * rewinding, wait for the rewind complete interrupt.
	 *
	 * SHOULD NEVER GET AN INTERRUPT IN THIS STATE.
	 */
	if (um->um_tab.b_active == SREW) {
		um->um_tab.b_active = SCOM;
		if ((addr->tssr&TS_SSR) == 0)
			return;
	}
	/*
	 * An operation completed... record status
	 */
	printd("  ok1\n");
	sc = &ts_softc[tsunit];
	if ((bp->b_flags & B_READ) == 0)
		sc->sc_lastiow = 1;
	state = um->um_tab.b_active;
	um->um_tab.b_active = 0;
	/*
	 * Check for errors.
	 */
	if (addr->tssr&TS_SC) {
		switch (addr->tssr & TS_TC) {
		case TS_UNREC:		/* unrecoverable */
		case TS_FATAL:		/* fatal error */
		case TS_ATTN:		/* attention (shouldn't happen) */
		case TS_RECNM:		/* recoverable, no motion */
			break;

		case TS_SUCC:		/* success termination */
			printf("ts%d: success\n", TSUNIT(minor(bp->b_dev)));
			goto ignoreerr;

		case TS_ALERT:		/* tape status alert */
			/*
			 * If we hit the end of the tape file,
			 * update our position.
			 */
			if (sc->sc_sts.s_xs0 & (TS_TMK|TS_EOT)) {
				tsseteof(bp);		/* set blkno and nxrec */
				state = SCOM;		/* force completion */
				/*
				 * Stuff bc so it will be unstuffed correctly
				 * later to get resid.
				 */
				sc->sc_sts.s_rbpcr = bp->b_bcount;
				goto opdone;
			}
			/*
			 * If we were reading raw tape and the record was too long
			 * or too short, then we don't consider this an error.
			 */
			if (bp == &rtsbuf[TSUNIT(bp->b_dev)] && (bp->b_flags&B_READ) &&
			    sc->sc_sts.s_xs0&(TS_RLS|TS_RLL))
				goto ignoreerr;
		case TS_RECOV:		/* recoverable, tape moved */
			/*
			 * If this was an i/o operation retry up to 8 times.
			 */
			if (state==SIO) {
				if (++um->um_tab.b_errcnt < 7) {
					ubadone(um);
					goto opcont;
				} else
					sc->sc_blkno++;
			} else {
				/*
				 * Non-i/o errors on non-raw tape
				 * cause it to close.
				 */
				if (sc->sc_openf>0 && bp != &rtsbuf[TSUNIT(bp->b_dev)])
					sc->sc_openf = -1;
			}
			break;

		case TS_REJECT:		/* function reject */
			if (state == SIO && sc->sc_sts.s_xs0 & TS_WLE)
				printf("ts%d: write locked\n", TSUNIT(bp->b_dev));
			if ((sc->sc_sts.s_xs0 & TS_ONL) == 0)
				printf("ts%d: offline\n", TSUNIT(bp->b_dev));
			break;
		}
		/*
		 * Couldn't recover error
		 */
		printf("ts%d: hard error bn%d xs0=%b\n", TSUNIT(bp->b_dev),
		    bp->b_blkno, sc->sc_sts.s_xs0, TSXS0_BITS);
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
		 * For forward/backward space record update current position.
		 */
		if (bp == &ctsbuf[TSUNIT(bp->b_dev)])
		switch (bp->b_command) {

		case TS_SFORW:
			sc->sc_blkno += bp->b_repcnt;
			break;

		case TS_SREV:
			sc->sc_blkno -= bp->b_repcnt;
			break;
		}
		goto opdone;

	case SSEEK:
		sc->sc_blkno = dbtofsb(bp->b_blkno);
		goto opcont;

	default:
		panic("tsintr");
	}
opdone:
	/*
	 * Reset error count and remove
	 * from device queue.
	 */
	um->um_tab.b_errcnt = 0;
	um->um_tab.b_actf->b_actf = bp->av_forw;
	bp->b_resid = sc->sc_sts.s_rbpcr;
	ubadone(um);
	printd("  iodone\n");
	iodone(bp);
	if (um->um_tab.b_actf->b_actf == 0)
		return;
opcont:
	tsstart(um);
}

tsseteof(bp)
	register struct buf *bp;
{
	register int tsunit = TSUNIT(bp->b_dev);
	register struct ts_softc *sc = &ts_softc[tsunit];

	if (bp == &ctsbuf[TSUNIT(bp->b_dev)]) {
		if (sc->sc_blkno > dbtofsb(bp->b_blkno)) {
			/* reversing */
			sc->sc_nxrec = dbtofsb(bp->b_blkno) - sc->sc_sts.s_rbpcr;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			/* spacing forward */
			sc->sc_blkno = dbtofsb(bp->b_blkno) + sc->sc_sts.s_rbpcr;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
		return;
	} 
	/* eof on read */
	sc->sc_nxrec = dbtofsb(bp->b_blkno);
}

tsread(dev)
	dev_t dev;
{

	tsphys(dev);
	if (u.u_error)
		return;
	physio(tsstrategy, &rtsbuf[TSUNIT(dev)], dev, B_READ, minphys);
}

tswrite(dev)
	dev_t dev;
{

	tsphys(dev);
	if (u.u_error)
		return;
	physio(tsstrategy, &rtsbuf[TSUNIT(dev)], dev, B_WRITE, minphys);
}

/*
 * Check that a raw device exists.
 * If it does, set up sc_blkno and sc_nxrec
 * so that the tape will appear positioned correctly.
 */
tsphys(dev)
	dev_t dev;
{
	register int tsunit = TSUNIT(dev);
	register daddr_t a;
	register struct ts_softc *sc;
	register struct uba_device *ui;

	if (tsunit >= NTS || (ui=tsdinfo[tsunit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	sc = &ts_softc[tsunit];
	a = dbtofsb(u.u_offset >> 9);
	sc->sc_blkno = a;
	sc->sc_nxrec = a + 1;
}

tsreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register ts11;
	register struct buf *dp;

	for (ts11 = 0; ts11 < NTS; ts11++) {
		if ((um = tsminfo[ts11]) == 0 || um->um_alive == 0 ||
		   um->um_ubanum != uban)
			continue;
		printf(" ts%d", ts11);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		ts_softc[ts11].sc_openf = -1;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone(um);
		}
		tsinit(ts11);
		tsstart(um);
	}
}

/*ARGSUSED*/
tsioctl(dev, cmd, addr, flag)
	caddr_t addr;
	dev_t dev;
{
	int tsunit = TSUNIT(dev);
	register struct ts_softc *sc = &ts_softc[tsunit];
	register struct buf *bp = &ctsbuf[TSUNIT(dev)];
	register callcount;
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static tsops[] =
	 {TS_WEOF,TS_SFORW,TS_SREV,TS_SFORWF,TS_SREVF,TS_REW,TS_OFFL,TS_SENSE};

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
			tscommand(dev, tsops[mtop.mt_op], fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    bp->b_resid) {
				u.u_error = EIO;
				break;
			}
			if ((bp->b_flags&B_ERROR) || sc->sc_sts.s_xs0&TS_BOT)
				break;
		}
		geterror(bp);
		return;
	case MTIOCGET:
		mtget.mt_dsreg = 0;
		mtget.mt_erreg = sc->sc_sts.s_xs0;
		mtget.mt_resid = sc->sc_resid;
		mtget.mt_type = MT_ISTS;
		if (copyout((caddr_t)&mtget, addr, sizeof(mtget)))
			u.u_error = EFAULT;
		return;
	default:
		u.u_error = ENXIO;
	}
}

#define	DBSIZE	20

tsdump()
{
	register struct uba_device *ui;
	register struct uba_regs *up;
	register struct device *addr;
	int blk, num;
	int start;

	start = 0;
	num = maxfree;
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	if (tsdinfo[0] == 0)
		return (ENXIO);
	ui = phys(tsdinfo[0], struct uba_device *);
	up = phys(ui->ui_hd, struct uba_hd *)->uh_physuba;
	ubainit(up);
	DELAY(1000000);
	addr = (struct device *)ui->ui_physaddr;
	addr->tssr = 0;
	tswait(addr);
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tsdwrite(start, blk, addr, up);
		start += blk;
		num -= blk;
	}
	tseof(addr);
	tseof(addr);
	tswait(addr);
	if (addr->tssr&TS_SC)
		return (EIO);
	addr->tssr = 0;
	tswait(addr);
	return (0);
}

tsdwrite(dbuf, num, addr, up)
	register dbuf, num;
	register struct device *addr;
	struct uba_regs *up;
{
	register struct pte *io;
	register int npf;

	tswait(addr);
	io = up->uba_map;
	npf = num+1;
	while (--npf != 0)
		 *(int *)io++ = (dbuf++ | (1<<UBAMR_DPSHIFT) | UBAMR_MRV);
	*(int *)io = 0;
#ifdef notyet
	addr->tsbc = -(num*NBPG);
	addr->tsba = 0;
	addr->tscs = TS_WCOM | TM_GO;
#endif
}

tswait(addr)
	register struct device *addr;
{
	register s;

	do
		s = addr->tssr;
	while ((s & TS_SSR) == 0);
}

tseof(addr)
	struct device *addr;
{

	tswait(addr);
#ifdef notyet
	addr->tscs = TS_WEOF | TM_GO;
#endif
}
#endif
