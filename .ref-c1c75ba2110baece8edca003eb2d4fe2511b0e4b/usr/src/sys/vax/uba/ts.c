/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ts.c	7.6 (Berkeley) %G%
 */

#include "ts.h"
#if NTS > 0
/*
 * TS11 tape driver
 *
 * NB: This driver takes advantage of the fact that there is only one
 *	drive per controller.
 *
 * TODO:
 *	test dump code
 */
#include "param.h"
#include "systm.h"
#include "buf.h"
#include "dir.h"
#include "conf.h"
#include "user.h"
#include "file.h"
#include "map.h"
#include "vm.h"
#include "ioctl.h"
#include "mtio.h"
#include "cmap.h"
#include "uio.h"
#include "tty.h"
#include "syslog.h"

#include "../machine/pte.h"
#include "../vax/cpu.h"
#include "ubareg.h"
#include "ubavar.h"
#include "tsreg.h"

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
 * Driver unibus interface routines and variables.
 */
int	tsprobe(), tsslave(), tsattach(), tsdgo(), tsintr();
struct	uba_ctlr *tsminfo[NTS];
struct	uba_device *tsdinfo[NTS];
struct buf	tsutab[NTS];
u_short	tsstd[] = { 0772520, 0 };
/* need all these even though controller == drive */
struct	uba_driver zsdriver =
 { tsprobe, tsslave, tsattach, tsdgo, tsstd, "ts", tsdinfo, "zs", tsminfo };

/* bits in minor device */
#define	TSUNIT(dev)	(minor(dev)&03)
#define	T_NOREWIND	04
#define	T_1600BPI	010

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
struct	ts_tsdata {		/* data shared with ts11 controller */
	struct	ts_cmd t_cmd;	/* the command packet (must be first) */
	struct	ts_sts t_sts;	/* status packet, for returned status */
	struct	ts_char t_char;	/* characteristics packet */
};
struct	ts_softc {
	char	sc_openf;	/* lock against multiple opens */
	char	sc_lastiow;	/* last op was a write */
	short	sc_resid;	/* copy of last bc */
	daddr_t	sc_blkno;	/* block number, for block device tape */
	daddr_t	sc_nxrec;	/* position of end of tape, if known */
	struct	ts_tsdata sc_ts;/* command and status packets */
	struct	ts_tsdata *sc_ubaddr; /* Unibus address of tsdata structure */
	u_short	sc_uba;		/* Unibus addr of cmd pkt for tsdb */
	short	sc_density;	/* value |'ed into char_mode for TC13 density */
	struct	tty *sc_ttyp;	/* record user's tty for errors */
	int	sc_blks;	/* number of I/O operations since open */
	int	sc_softerrs;	/* number of soft I/O errors since open */
} ts_softc[NTS];

/*
 * States for um->um_tab.b_active, the per controller state flag.
 * This is used to sequence control in the driver.
 */
#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */
#define	SREW	4		/* sending a drive rewind */

/*
 * Determine if there is a controller for
 * a ts at address reg.  Our goal is to make the
 * device interrupt.
 */
/*ARGSUSED*/
tsprobe(reg, ctlr, um)
	caddr_t reg;
	int ctlr;
	struct uba_ctlr *um;
{
	register int br, cvec;		/* must be r11,r10; value-result */
	register struct tsdevice *addr = (struct tsdevice *)reg;
	register struct ts_softc *sc;
	register int i;
	int a;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	tsintr(0);
#endif
	addr->tssr = 0;		/* initialize subsystem */
	DELAY(100);
	if ((addr->tssr & TS_NBA) == 0)
		return (0);

	/*
	 * Make it interrupt.
	 * TS_SETCHR|TS_IE alone refuses to interrupt for me.
	 */
	sc = &ts_softc[ctlr];
	tsmap(sc, numuba, &a);
	i = (int)&sc->sc_ubaddr->t_char;
	sc->sc_ts.t_cmd.c_loba = i;
	sc->sc_ts.t_cmd.c_hiba = (i >> 16) & 3;
	sc->sc_ts.t_cmd.c_size = sizeof(struct ts_char);
	sc->sc_ts.t_cmd.c_cmd = TS_ACK | TS_SETCHR;
	sc->sc_ts.t_char.char_addr = (int)&sc->sc_ubaddr->t_sts;
	sc->sc_ts.t_char.char_size = sizeof(struct ts_sts);
	sc->sc_ts.t_char.char_mode = 0;	/* mode is unimportant */
	addr->tsdb = sc->sc_uba;
	DELAY(20000);
	sc->sc_ts.t_cmd.c_cmd = TS_ACK | TS_CVC | TS_IE | TS_SENSE;
	sc->sc_ts.t_cmd.c_repcnt = 1;
	addr->tsdb = sc->sc_uba;
	DELAY(20000);
	/* should have interrupted by now */

	if (cvec == 0 || cvec == 0x200)	/* no interrupt */
		ubarelse(numuba, &a);

	return (sizeof (struct tsdevice));
}

/*
 * Map in the command, status, and characteristics packet.  We
 * make them contiguous to keep overhead down.  This also sets
 * sc_uba (which then never changes).
 */
tsmap(sc, uban, a)
	register struct ts_softc *sc;
	int uban, *a;
{
	register int i;

	i = uballoc(uban, (caddr_t)&sc->sc_ts, sizeof(sc->sc_ts), 0);
	if (a != NULL)
		*a = i;
	i = UBAI_ADDR(i);
	sc->sc_ubaddr = (struct ts_tsdata *)i;
	/*
	 * Note that i == the Unibus address of the command packet,
	 * and that it is a multiple of 4 (guaranteed by the compiler).
	 */
	sc->sc_uba = i + ((i >> 16) & 3);
}

/*
 * TS11 only supports one drive per controller;
 * check for ui_slave == 0.
 */
/*ARGSUSED*/
tsslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{

	return (ui->ui_slave == 0);	/* non-zero slave not allowed */
}

/*
 * Record attachment of the unit to the controller.
 */
/*ARGSUSED*/
tsattach(ui)
	struct uba_device *ui;
{

	/* void */
}

/*
 * Open the device.  Tapes are unique open
 * devices, so we refuse if it is already open.
 */
tsopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int tsunit = TSUNIT(dev);
	register struct uba_device *ui;
	register struct ts_softc *sc;

	if (tsunit >= NTS || (ui = tsdinfo[tsunit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	if ((sc = &ts_softc[ui->ui_ctlr])->sc_openf)
		return (EBUSY);
	sc->sc_openf = 1;
	sc->sc_density = (minor(dev) & T_1600BPI) ? TS_NRZI : 0;
	tscommand(dev, TS_SENSE, 1);
	if (ctsbuf[tsunit].b_flags & B_ERROR)
		/*
		 * Try it again in case it went off-line
		 */
		tscommand(dev, TS_SENSE, 1);
	if (tsinit(ui->ui_ctlr)) {
		sc->sc_openf = 0;
		return (ENXIO);
	}
	if ((sc->sc_ts.t_sts.s_xs0&TS_ONL) == 0) {
		sc->sc_openf = 0;
		uprintf("ts%d: not online\n", tsunit);
		return (EIO);
	}
	if ((flag&FWRITE) && (sc->sc_ts.t_sts.s_xs0&TS_WLK)) {
		sc->sc_openf = 0;
		uprintf("ts%d: no write ring\n", tsunit);
		return (EIO);
	}
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_lastiow = 0;
	sc->sc_blks = 0;
	sc->sc_softerrs = 0;
	sc->sc_ttyp = u.u_ttyp;
	return (0);
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
	register int flag;
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
	if (sc->sc_blks > 100 && sc->sc_softerrs > sc->sc_blks / 100)
		log(LOG_INFO, "ts%d: %d soft errors in %d blocks\n",
		    TSUNIT(dev), sc->sc_softerrs, sc->sc_blks);
	sc->sc_openf = 0;
}

/*
 * Initialize a TS11.  Set device characteristics.
 */
tsinit(ctlr)
	register int ctlr;
{
	register struct ts_softc *sc = &ts_softc[ctlr];
	register struct uba_ctlr *um = tsminfo[ctlr];
	register struct tsdevice *addr = (struct tsdevice *)um->um_addr;
	register int i;

	if (addr->tssr & (TS_NBA|TS_OFL) || sc->sc_ts.t_sts.s_xs0 & TS_BOT) {
		addr->tssr = 0;		/* subsystem initialize */
		tswait(addr);
		i = (int)&sc->sc_ubaddr->t_char;
		sc->sc_ts.t_cmd.c_loba = i;
		sc->sc_ts.t_cmd.c_hiba = (i >> 16) & 3;
		sc->sc_ts.t_cmd.c_size = sizeof(struct ts_char);
		sc->sc_ts.t_cmd.c_cmd = TS_ACK | TS_CVC | TS_SETCHR;
		sc->sc_ts.t_char.char_addr = (int)&sc->sc_ubaddr->t_sts;
		sc->sc_ts.t_char.char_size = sizeof(struct ts_sts);
		sc->sc_ts.t_char.char_mode = TS_ESS | TS_EAI | TS_ERI |
			/* TS_ENB | */ sc->sc_density;
		addr->tsdb = sc->sc_uba;
		tswait(addr);
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
	register int s;
	int didsleep = 0;

	bp = &ctsbuf[TSUNIT(dev)];
	s = spl5();
	while (bp->b_flags&B_BUSY) {
		/*
		 * This special check is because B_BUSY never
		 * gets cleared in the non-waiting rewind case.
		 */
		if (bp->b_repcnt == 0 && (bp->b_flags&B_DONE))
			break;
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
		didsleep = 1;
	}
	bp->b_flags = B_BUSY|B_READ;
	splx(s);
	if (didsleep)
		(void) tsinit(tsdinfo[TSUNIT(dev)]->ui_ctlr);
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
	biowait(bp);
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
	register int tsunit = TSUNIT(bp->b_dev);
	register struct uba_ctlr *um;
	register struct buf *dp;
	int s;

	/*
	 * Put transfer at end of controller queue
	 */
	bp->av_forw = NULL;
	um = tsdinfo[tsunit]->ui_mi;
	dp = &tsutab[tsunit];
	s = spl5();
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
	splx(s);
}

/*
 * Start activity on a ts controller.
 */
tsstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp;
	register struct tsdevice *addr = (struct tsdevice *)um->um_addr;
	register struct ts_softc *sc;
	register struct uba_device *ui;
	register int tsunit;
	int cmd;
	daddr_t blkno;

	/*
	 * Start the controller if there is something for it to do.
	 */
loop:
	if ((bp = um->um_tab.b_actf->b_actf) == NULL) {
		um->um_tab.b_active = 0;
		return;
	}
	tsunit = TSUNIT(bp->b_dev);
	ui = tsdinfo[tsunit];
	sc = &ts_softc[tsunit];
	/*
	 * Default is that last command was NOT a write command;
	 * if we finish a write command we will notice this in tsintr().
	 */
	sc->sc_lastiow = 0;
	if (sc->sc_openf < 0 || (addr->tssr&TS_OFL)) {
		/*
		 * Have had a hard error on a non-raw tape
		 * or the tape unit is now unavailable
		 * (e.g. taken off line).
		 */
		bp->b_flags |= B_ERROR;
		goto next;
	}
	if (bp == &ctsbuf[tsunit]) {
		/*
		 * Execute control operation with the specified count.
		 */
		um->um_tab.b_active =
		    bp->b_command == TS_REW ? SREW : SCOM;
		sc->sc_ts.t_cmd.c_repcnt = bp->b_repcnt;
		goto dobpcmd;
	}
	/*
	 * For raw I/O, save the current block
	 * number in case we have to retry.
	 */
	if (bp->b_flags & B_RAW) {
		if (um->um_tab.b_errcnt == 0)
			sc->sc_blkno = bdbtofsb(bp->b_blkno);
	} else {
		/*
		 * Handle boundary cases for operation
		 * on non-raw tapes.
		 */
		if (bdbtofsb(bp->b_blkno) > sc->sc_nxrec) {
			/*
			 * Can't read past known end-of-file.
			 */
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			goto next;
		}
		if (bdbtofsb(bp->b_blkno) == sc->sc_nxrec &&
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
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) + 1;
	}

	/*
	 * If the data transfer command is in the correct place,
	 * set up all the registers except the csr, and give
	 * control over to the UNIBUS adapter routines, to
	 * wait for resources to start the i/o.
	 */
	if ((blkno = sc->sc_blkno) == bdbtofsb(bp->b_blkno)) {
		sc->sc_ts.t_cmd.c_size = bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0)
			cmd = TS_WCOM;
		else
			cmd = TS_RCOM;
		if (um->um_tab.b_errcnt)
			cmd |= TS_RETRY;
		um->um_tab.b_active = SIO;
		sc->sc_ts.t_cmd.c_cmd = TS_ACK | TS_CVC | TS_IE | cmd;
		(void) ubago(ui);
		return;
	}
	/*
	 * Tape positioned incorrectly;
	 * set to seek forwards or backwards to the correct spot.
	 * This happens for raw tapes only on error retries.
	 */
	um->um_tab.b_active = SSEEK;
	if (blkno < bdbtofsb(bp->b_blkno)) {
		bp->b_command = TS_SFORW;
		sc->sc_ts.t_cmd.c_repcnt = bdbtofsb(bp->b_blkno) - blkno;
	} else {
		bp->b_command = TS_SREV;
		sc->sc_ts.t_cmd.c_repcnt = blkno - bdbtofsb(bp->b_blkno);
	}
dobpcmd:
	/*
	 * Do the command in bp.
	 */
	sc->sc_ts.t_cmd.c_cmd = TS_ACK | TS_CVC | TS_IE | bp->b_command;
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
	biodone(bp);
	goto loop;
}

/*
 * The UNIBUS resources we needed have been
 * allocated to us; start the device.
 */
tsdgo(um)
	register struct uba_ctlr *um;
{
	register struct ts_softc *sc = &ts_softc[um->um_ctlr];
	register int i;

	/*
	 * The uba code uses byte-offset mode if using bdp;
	 * mask off the low bit here.
	 */
	i = UBAI_ADDR(um->um_ubinfo);
	if (UBAI_BDP(um->um_ubinfo))
		i &= ~1;
	sc->sc_ts.t_cmd.c_loba = i;
	sc->sc_ts.t_cmd.c_hiba = (i >> 16) & 3;
	((struct tsdevice *)um->um_addr)->tsdb = sc->sc_uba;
}

/*
 * Ts interrupt routine.
 */
/*ARGSUSED*/
tsintr(tsunit)
	register int tsunit;
{
	register struct buf *bp;
	register struct uba_ctlr *um;
	register struct tsdevice *addr;
	register struct ts_softc *sc;
	register int state;

#ifdef QBA
	(void) spl5();
#endif
	um = tsdinfo[tsunit]->ui_mi;
	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	addr = (struct tsdevice *)um->um_addr;
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
	sc = &ts_softc[um->um_ctlr];
	if ((bp->b_flags & B_READ) == 0)
		sc->sc_lastiow = 1;
	state = um->um_tab.b_active;
	/*
	 * Check for errors.
	 */
	if (addr->tssr&TS_SC) {
		switch (addr->tssr & TS_TC) {
		case TS_UNREC:		/* unrecoverable */
		case TS_FATAL:		/* fatal error */
		case TS_RECNM:		/* recoverable, no motion */
			break;
		case TS_ATTN:		/* attention */
			if (sc->sc_ts.t_sts.s_xs0 & TS_VCK) {
				/* volume check - may have changed tapes */
				bp->b_flags |= B_ERROR;
				goto ignoreerr;
			}
			break;

		case TS_SUCC:		/* success termination */
			printf("ts%d: success\n", tsunit);
			goto ignoreerr;

		case TS_ALERT:		/* tape status alert */
			/*
			 * If we hit the end of the tape file,
			 * update our position.
			 */
			if (sc->sc_ts.t_sts.s_xs0 & (TS_TMK|TS_EOT)) {
				tsseteof(bp);	/* set blkno and nxrec */
				state = SCOM;	/* force completion */
				/*
				 * Stuff bc so it will be unstuffed correctly
				 * later to get resid.
				 */
				sc->sc_ts.t_sts.s_rbpcr = bp->b_bcount;
				goto opdone;
			}
			/*
			 * If we were reading raw tape and the record was too
			 * long or too short, we don't consider this an error.
			 */
			if ((bp->b_flags & (B_READ|B_RAW)) == (B_READ|B_RAW) &&
			    sc->sc_ts.t_sts.s_xs0&(TS_RLS|TS_RLL))
				goto ignoreerr;
			/* FALLTHROUGH */

		case TS_RECOV:		/* recoverable, tape moved */
			/*
			 * If this was an i/o operation retry up to 8 times.
			 */
			if (state == SIO) {
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
				if ((bp->b_flags&B_RAW) == 0 &&
				    sc->sc_openf > 0)
					sc->sc_openf = -1;
			}
			break;

		case TS_REJECT:		/* function reject */
			if (state == SIO && sc->sc_ts.t_sts.s_xs0 & TS_WLE)
				tprintf(sc->sc_ttyp, "ts%d: write locked\n",
				    tsunit);
			if ((sc->sc_ts.t_sts.s_xs0 & TS_ONL) == 0)
				tprintf(sc->sc_ttyp, "ts%d: offline\n",
				    tsunit);
			break;
		}
		/*
		 * Couldn't recover error
		 */
		tprintf(sc->sc_ttyp, "ts%d: hard error bn%d tssr=%b xs0=%b",
		    tsunit, bp->b_blkno, addr->tssr, TSSR_BITS,
		    sc->sc_ts.t_sts.s_xs0, TSXS0_BITS);
		if (sc->sc_ts.t_sts.s_xs1)
			tprintf(sc->sc_ttyp, " xs1=%b", sc->sc_ts.t_sts.s_xs1,
			    TSXS1_BITS);
		if (sc->sc_ts.t_sts.s_xs2)
			tprintf(sc->sc_ttyp, " xs2=%b", sc->sc_ts.t_sts.s_xs2,
			    TSXS2_BITS);
		if (sc->sc_ts.t_sts.s_xs3)
			tprintf(sc->sc_ttyp, " xs3=%b", sc->sc_ts.t_sts.s_xs3,
			    TSXS3_BITS);
		tprintf(sc->sc_ttyp, "\n");
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
		sc->sc_blks++;
		if (um->um_tab.b_errcnt)
			sc->sc_softerrs++;
		goto opdone;

	case SCOM:
		/*
		 * For forward/backward space record update current position.
		 */
		if (bp == &ctsbuf[tsunit])
			switch ((int)bp->b_command) {

			case TS_SFORW:
				sc->sc_blkno += bp->b_repcnt;
				break;

			case TS_SREV:
				sc->sc_blkno -= bp->b_repcnt;
				break;
			}
		goto opdone;

	case SSEEK:
		sc->sc_blkno = bdbtofsb(bp->b_blkno);
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
	bp->b_resid = sc->sc_ts.t_sts.s_rbpcr;
	ubadone(um);
	biodone(bp);
	if (um->um_tab.b_actf->b_actf == 0) {
		um->um_tab.b_active = 0;
		return;
	}
opcont:
	tsstart(um);
}

tsseteof(bp)
	register struct buf *bp;
{
	register int tsunit = TSUNIT(bp->b_dev);
	register struct ts_softc *sc = &ts_softc[tsdinfo[tsunit]->ui_ctlr];

	if (bp == &ctsbuf[tsunit]) {
		if (sc->sc_blkno > bdbtofsb(bp->b_blkno)) {
			/* reversing */
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) -
				sc->sc_ts.t_sts.s_rbpcr;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			/* spacing forward */
			sc->sc_blkno = bdbtofsb(bp->b_blkno) +
				sc->sc_ts.t_sts.s_rbpcr;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
		return;
	} 
	/* eof on read */
	sc->sc_nxrec = bdbtofsb(bp->b_blkno);
}

tsreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register int ts11, i;

	for (ts11 = 0; ts11 < NTS; ts11++) {
		if ((um = tsminfo[ts11]) == 0 || um->um_alive == 0 ||
		    um->um_ubanum != uban)
			continue;
		printf(" ts%d", ts11);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		if (ts_softc[ts11].sc_openf > 0)
			ts_softc[ts11].sc_openf = -1;
		if (um->um_ubinfo) {
			printf("<%d>", UBAI_BDP(um->um_ubinfo));
			um->um_ubinfo = 0;
		}
		/*
		 * tsdinfo should be 1-to-1 with tsminfo, but someone
		 * might have screwed up the config file:
		 */
		for (i = 0; i < NTS; i++) {
			if ((ui = tsdinfo[i]) != NULL &&
			    ui->ui_alive && ui->ui_mi == um) {
				um->um_tab.b_actf = um->um_tab.b_actl =
				    &tsutab[i];
				break;
			}
		}
		tsmap(&ts_softc[ts11], uban, (int *)NULL);
		(void) tsinit(ts11);
		tsstart(um);
	}
}

/*ARGSUSED*/
tsioctl(dev, cmd, data, flag)
	caddr_t data;
	dev_t dev;
{
	int tsunit = TSUNIT(dev);
	register struct ts_softc *sc = &ts_softc[tsdinfo[tsunit]->ui_ctlr];
	register struct buf *bp = &ctsbuf[TSUNIT(dev)];
	register int callcount;
	int fcount;
	struct mtop *mtop;
	struct mtget *mtget;
	/* we depend of the values and order of the MT codes here */
	static int tsops[] =
	 {TS_WEOF,TS_SFORWF,TS_SREVF,TS_SFORW,TS_SREV,TS_REW,TS_OFFL,TS_SENSE};

	switch (cmd) {

	case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		switch (mtop->mt_op) {

		case MTWEOF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTFSF: case MTBSF:
		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop->mt_count;
			break;

		case MTREW: case MTOFFL: case MTNOP:
			callcount = 1;
			fcount = 1;
			break;

		default:
			return (ENXIO);
		}
		if (callcount <= 0 || fcount <= 0)
			return (EINVAL);
		while (--callcount >= 0) {
			tscommand(dev, tsops[mtop->mt_op], fcount);
			if ((mtop->mt_op == MTFSR || mtop->mt_op == MTBSR) &&
			    bp->b_resid)
				return (EIO);
			if ((bp->b_flags&B_ERROR) ||
			    sc->sc_ts.t_sts.s_xs0&TS_BOT)
				break;
		}
		return (geterror(bp));

	case MTIOCGET:
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = 0;
		mtget->mt_erreg = sc->sc_ts.t_sts.s_xs0;
		mtget->mt_resid = sc->sc_resid;
		mtget->mt_type = MT_ISTS;
		break;

	default:
		return (ENXIO);
	}
	return (0);
}

#define	DBSIZE	20

tsdump(dev)
	dev_t dev;
{
	register struct uba_device *ui;
	register struct uba_regs *uba;
	register struct tsdevice *addr;
	register int i;
	register struct pte *io;
	int blk, num, unit, reg, start;
	u_short db;
	struct ts_tsdata *tc, *tc_ubaddr;

	unit = TSUNIT(dev);
	if (unit >= NTS)
		return (ENXIO);
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	ui = phys(tsdinfo[unit], struct uba_device *);
	if (ui->ui_alive == 0)
		return (ENXIO);
	uba = phys(ui->ui_hd, struct uba_hd *)->uh_physuba;
	ubainit(uba);
	addr = (struct tsdevice *)ui->ui_physaddr;

	/* map a ts_tsdata structure */
	tc = phys(&ts_softc[0].sc_ts, struct ts_tsdata *);
	num = btoc(sizeof(struct ts_tsdata)) + 1;
	io = &uba->uba_map[reg = NUBMREG - num];
	for (i = 0; i < num; i++)
		*(int *)io++ = UBAMR_MRV | (btop(tc) + i);
	i = (((int)tc & PGOFSET) | (reg << 9));
	tc_ubaddr = (struct ts_tsdata *)i;
	db = i + ((i >> 16) & 3);

	/* init the drive */
	addr->tssr = 0;
	tswait(addr);
	if ((addr->tssr & (TS_NBA|TS_OFL)) != TS_NBA)
		return (EFAULT);

	/* set characteristics */
	i = (int)&tc_ubaddr->t_char;
	tc->t_cmd.c_loba = i;
	tc->t_cmd.c_hiba = (i >> 16) & 3;
	tc->t_cmd.c_size = sizeof(struct ts_char);
	tc->t_cmd.c_cmd = TS_ACK | TS_CVC | TS_SETCHR;
	tc->t_char.char_addr = (int)&tc_ubaddr->t_sts;
	tc->t_char.char_size = sizeof(struct ts_sts);
	tc->t_char.char_mode = TS_ESS;
	addr->tsdb = db;
	tswait(addr);
	if (addr->tssr & TS_NBA)
		return (ENXIO);

	/* dump */
	tc->t_cmd.c_cmd = TS_ACK | TS_WCOM;
	tc->t_cmd.c_repcnt = 1;
	num = maxfree;
	for (start = 0, num = maxfree; num > 0; start += blk, num -= blk) {
		blk = num > DBSIZE ? DBSIZE : num;
		io = uba->uba_map;
		for (i = 0; i < blk; i++)
			*(int *)io++ = UBAMR_MRV | (1 << UBAMR_DPSHIFT) |
				(start + i);
		*(int *)io = 0;
		addr->tsdb = db;
		tswait(addr);
	}

	/* eof */
	tc->t_cmd.c_cmd = TS_WEOF;
	addr->tsdb = db;
	tswait(addr);
	addr->tsdb = db;
	tswait(addr);

	if (addr->tssr&TS_SC)
		return (EIO);
	addr->tssr = 0;
	tswait(addr);
	return (0);
}

tswait(addr)
	register struct tsdevice *addr;
{

	while ((addr->tssr & TS_SSR) == 0)
		/* void */;
}

#endif /* NTS > 0 */
