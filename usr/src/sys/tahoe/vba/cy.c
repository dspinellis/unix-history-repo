/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)cy.c	7.1 (Berkeley) %G%
 */

#include "yc.h"
#if NCY > 0
/*
 * Cipher Tapemaster driver.
 */
#define CYDEBUG
#ifdef	CYDEBUG
int	cydebug = 0;
#define	dlog(params)	if (cydebug) log params
#else
#define dlog(params)	/* */
#endif

#include "param.h"
#include "systm.h"
#include "vm.h"
#include "buf.h"
#include "file.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "signal.h"
#include "uio.h"
#include "ioctl.h"
#include "mtio.h"
#include "errno.h"
#include "cmap.h"
#include "kernel.h"
#include "syslog.h"
#include "tty.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#define	CYERROR
#include "../tahoevba/cyreg.h"

/*
 * There is a ccybuf per tape controller.
 * It is used as the token to pass to the internal routines
 * to execute tape ioctls, and also acts as a lock on the slaves
 * on the controller, since there is only one per controller.
 * In particular, when the tape is rewinding on close we release
 * the user process but any further attempts to use the tape drive
 * before the rewind completes will hang waiting for ccybuf.
 */
struct	buf ccybuf[NCY];

int	cyprobe(), cyslave(), cyattach();
struct	buf ycutab[NYC];
short	yctocy[NYC];
struct	vba_ctlr *cyminfo[NCY];
struct	vba_device *ycdinfo[NYC];
long	cystd[] = { 0 };
struct	vba_driver cydriver =
   { cyprobe, cyslave, cyattach, 0, cystd, "yc", ycdinfo, "cy", cyminfo };

/* bits in minor device */
#define	YCUNIT(dev)	(minor(dev)&03)
#define	CYUNIT(dev)	(yctocy[YCUNIT(dev)])
#define	T_NOREWIND	0x04
#define	T_1600BPI	0x00		/* pseudo */
#define	T_3200BPI	0x08		/* unused */

#define	INF	1000000L		/* close to infinity */

/*
 * Software state and shared command areas per controller.
 *
 * The i/o intermediate buffer must be allocated in startup()
 * so its address will fit in 20-bits (YECH!!!!!!!!!!!!!!).
 */
struct cy_softc {
	int	cy_bs;		/* controller's buffer size */
	struct	cyscp *cy_scp;	/* system configuration block address */
	struct	cyccb cy_ccb;	/* channel control block */
	struct	cyscb cy_scb;	/* system configuration block */
	struct	cytpb cy_tpb;	/* tape parameter block */
	struct	cytpb cy_nop;	/* nop parameter block for cyintr */
	struct	vb_buf cy_rbuf;	/* vba resources */
} cy_softc[NCY];

/*
 * Software state per tape transport.
 */
struct	yc_softc {
	char	yc_openf;	/* lock against multiple opens */
	char	yc_lastiow;	/* last operation was a write */
	short	yc_tact;	/* timeout is active */
	long	yc_timo;	/* time until timeout expires */
	u_short	yc_control;	/* copy of last tpcb.tpcontrol */
	u_short	yc_status;	/* copy of last tpcb.tpstatus */
	u_short	yc_resid;	/* copy of last bc */
	u_short	yc_dens;	/* prototype control word with density info */
	struct	tty *yc_ttyp;	/* user's tty for errors */
	daddr_t	yc_blkno;	/* block number, for block device tape */
	daddr_t	yc_nxrec;	/* position of end of tape, if known */
	int	yc_blksize;	/* current tape blocksize estimate */
	int	yc_blks;	/* number of I/O operations since open */
	int	yc_softerrs;	/* number of soft I/O errors since open */
} yc_softc[NYC];

/*
 * States for vm->um_tab.b_active, the per controller state flag.
 * This is used to sequence control in the driver.
 */
#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */
#define	SREW	4		/* sending a rewind */
#define	SERASE	5		/* erase inter-record gap */
#define	SERASED	6		/* erased inter-record gap */

/* there's no way to figure these out dynamically? -- yech */
struct	cyscp *cyscp[] =
    { (struct cyscp *)0xc0000c06, (struct cyscp *)0xc0000c16 };
#define	NCYSCP	(sizeof (cyscp) / sizeof (cyscp[0]))

cyprobe(reg, vm)
	caddr_t reg;
	struct vba_ctlr *vm;
{
	register br, cvec;			/* must be r12, r11 */
	register struct cy_softc *cy;
	int ctlr = vm->um_ctlr;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	cyintr(0);
#endif
	if (badcyaddr(reg+1))
		return (0);
	if (ctlr > NCYSCP || cyscp[ctlr] == 0)		/* XXX */
		return (0);
	cy = &cy_softc[ctlr];
	cy->cy_scp = cyscp[ctlr];			/* XXX */
	/*
	 * Tapemaster controller must have interrupt handler
	 * disable interrupt, so we'll just kludge things
	 * (stupid multibus non-vectored interrupt crud).
	 */
	if (cyinit(ctlr, reg)) {
		uncache(&cy->cy_tpb.tpcount);
		cy->cy_bs = htoms(cy->cy_tpb.tpcount);
		/*
		 * Setup nop parameter block for clearing interrupts.
		 */
		cy->cy_nop.tpcmd = CY_NOP;
		cy->cy_nop.tpcontrol = 0;
		/*
		 * Allocate page tables.
		 */
		if (cybuf == 0) {
			printf("no cy buffer!!!\n");
			return (0);
		}
		cy->cy_rbuf.vb_rawbuf = cybuf + ctlr * CYMAXIO;
		if (vbainit(&cy->cy_rbuf, CYMAXIO, VB_20BIT) == 0) {
			printf("cy%d: vbainit failed\n", ctlr);
			return (0);
		}

		br = 0x13, cvec = 0x80;			/* XXX */
		return (sizeof (struct cyccb));
	} else
		return (0);
}

/*
 * Check to see if a drive is attached to a controller.
 * Since we can only tell that a drive is there if a tape is loaded and
 * the drive is placed online, we always indicate the slave is present.
 */
cyslave(vi, addr)
	struct vba_device *vi;
	caddr_t addr;
{

#ifdef lint
	vi = vi; addr = addr;
#endif
	return (1);
}

cyattach(vi)
	struct vba_device *vi;
{
	register struct cy_softc *cy;
	int ctlr = vi->ui_mi->um_ctlr;

	yctocy[vi->ui_unit] = ctlr;
	cy = &cy_softc[ctlr];
	if (vi->ui_slave == 0 && cy->cy_bs)
		printf("; %dkb buffer", cy->cy_bs/1024);
}

/*
 * Initialize the controller after a controller reset or
 * during autoconfigure.  All of the system control blocks
 * are initialized and the controller is asked to configure
 * itself for later use.
 */
cyinit(ctlr, addr)
	int ctlr;
	register caddr_t addr;
{
	register struct cy_softc *cy = &cy_softc[ctlr];
	register int *pte;

	/*
	 * Initialize the system configuration pointer.
	 */
	/* make kernel writable */
	pte = (int *)&Sysmap[btop((int)cy->cy_scp &~ KERNBASE)]; 
	*pte &= ~PG_PROT; *pte |= PG_KW;
	mtpr(TBIS, cy->cy_scp);
	/* load the correct values in the scp */
	cy->cy_scp->csp_buswidth = CSP_16BITS;
	cyldmba(cy->cy_scp->csp_scb, (caddr_t)&cy->cy_scb);
	/* put it back to read-only */
	*pte &= ~PG_PROT; *pte |= PG_KR;
	mtpr(TBIS, cy->cy_scp);

	/*
	 * Init system configuration block.
	 */
	cy->cy_scb.csb_fixed = CSB_FIXED;
	/* set pointer to the channel control block */
	cyldmba(cy->cy_scb.csb_ccb, (caddr_t)&cy->cy_ccb);

	/*
	 * Initialize the chanel control block.
	 */
	cy->cy_ccb.cbcw = CBCW_CLRINT;
	cy->cy_ccb.cbgate = GATE_OPEN;
	/* set pointer to the tape parameter block */
	cyldmba(cy->cy_ccb.cbtpb, (caddr_t)&cy->cy_tpb);

	/*
	 * Issue a nop cmd and get the internal buffer size for buffered i/o.
	 */
	cy->cy_tpb.tpcmd = CY_NOP;
	cy->cy_tpb.tpcontrol = CYCW_16BITS;
	cy->cy_ccb.cbgate = GATE_CLOSED;	
	CY_GO(addr);
	if (cywait(&cy->cy_ccb) || (cy->cy_tpb.tpstatus&CYS_ERR)) {
		uncache(&cy->cy_tpb.tpstatus);
		printf("cy%d: timeout or err during init, status=%b\n", ctlr,
		    cy->cy_tpb.tpstatus, CYS_BITS);
		return (0);
	}
	cy->cy_tpb.tpcmd = CY_CONFIG;
	cy->cy_tpb.tpcontrol = CYCW_16BITS;
	cy->cy_ccb.cbgate = GATE_CLOSED;	
	CY_GO(addr);
	if (cywait(&cy->cy_ccb) || (cy->cy_tpb.tpstatus&CYS_ERR)) {
		uncache(&cy->cy_tpb.tpstatus);
		printf("cy%d: configuration failure, status=%b\n", ctlr,
		    cy->cy_tpb.tpstatus, CYS_BITS);
		return (0);
	}
	return (1);
}

int	cytimer();
/*
 * Open the device.  Tapes are unique open
 * devices, so we refuse if it is already open.
 * We also check that a tape is available, and
 * don't block waiting here; if you want to wait
 * for a tape you should timeout in user code.
 */
cyopen(dev, flag)
	dev_t dev;
	register int flag;
{
	register int ycunit;
	register struct vba_device *vi;
	register struct yc_softc *yc;

	ycunit = YCUNIT(dev);
	if (ycunit >= NYC || (vi = ycdinfo[ycunit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	if ((yc = &yc_softc[ycunit])->yc_openf)
		return (EBUSY);
	yc->yc_openf = 1;
#define	PACKUNIT(vi) \
    (((vi->ui_slave&1)<<11)|((vi->ui_slave&2)<<9)|((vi->ui_slave&4)>>2))
	/* no way to select density */
	yc->yc_dens = PACKUNIT(vi)|CYCW_IE|CYCW_16BITS;
	if (yc->yc_tact == 0) {
		yc->yc_timo = INF;
		yc->yc_tact = 1;
		timeout(cytimer, (caddr_t)dev, 5*hz);
	}
	cycommand(dev, CY_SENSE, 1);
	if ((yc->yc_status&CYS_OL) == 0) {	/* not on-line */
		uprintf("cy%d: not online\n", ycunit);
		yc->yc_openf = 0;
		return (EIO);
	}
	if ((flag&FWRITE) && (yc->yc_status&CYS_WP)) {
		uprintf("cy%d: no write ring\n", ycunit);
		yc->yc_openf = 0;
		return (EIO);
	}
	yc->yc_blkno = (daddr_t)0;
	yc->yc_nxrec = INF;
	yc->yc_lastiow = 0;
	yc->yc_blksize = CYMAXIO;		/* guess > 0 */
	yc->yc_blks = 0;
	yc->yc_softerrs = 0;
	yc->yc_ttyp = u.u_ttyp;
	return (0);
}

/*
 * Close tape device.
 *
 * If tape was open for writing or last operation was a write,
 * then write two EOF's and backspace over the last one.
 * Unless this is a non-rewinding special file, rewind the tape.
 * Make the tape available to others.
 */
cyclose(dev, flag)
	dev_t dev;
	int flag;
{
	struct yc_softc *yc = &yc_softc[YCUNIT(dev)];

	if (flag == FWRITE || (flag&FWRITE) && yc->yc_lastiow) {
		cycommand(dev, CY_WEOF, 1);	/* can't use count with WEOF */
		cycommand(dev, CY_WEOF, 1);
		cycommand(dev, CY_SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		/*
		 * 0 count means don't hang waiting for rewind complete
		 * rather ccybuf stays busy until the operation completes
		 * preventing further opens from completing by preventing
		 * a CY_SENSE from completing.
		 */
		cycommand(dev, CY_REW, 0);
	if (yc->yc_blks > 10 && yc->yc_softerrs > yc->yc_blks / 10)
		log(LOG_INFO, "yc%d: %d soft errors in %d blocks\n",
		    YCUNIT(dev), yc->yc_softerrs, yc->yc_blks);
	dlog((LOG_INFO, "%d soft errors in %d blocks\n",
	    yc->yc_softerrs, yc->yc_blks));
	yc->yc_openf = 0;
	return (0);
}

/*
 * Execute a command on the tape drive a specified number of times.
 */
cycommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	int s;
	
	bp = &ccybuf[CYUNIT(dev)];
	s = spl3();
	dlog((LOG_INFO, "cycommand(%o, %x, %d), b_flags %x\n",
	    dev, com, count, bp->b_flags));
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
	splx(s);
	bp->b_dev = dev;
	bp->b_repcnt = count;
	bp->b_command = com;
	bp->b_blkno = 0;
	cystrategy(bp);
	/*
	 * In case of rewind from close; don't wait.
	 * This is the only case where count can be 0.
	 */
	if (count == 0)
		return;
	biowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

cystrategy(bp)
	register struct buf *bp;
{
	int ycunit = YCUNIT(bp->b_dev);
	register struct vba_ctlr *vm;
	register struct buf *dp;
	int s;

	/*
	 * Put transfer at end of unit queue.
	 */
	dlog((LOG_INFO, "cystrategy(%o, %x)\n", bp->b_dev, bp->b_command));
	dp = &ycutab[ycunit];
	bp->av_forw = NULL;
	vm = ycdinfo[ycunit]->ui_mi;
	/* BEGIN GROT */
	if (bp->b_flags & B_RAW) {
		if (bp->b_bcount >= CYMAXIO) {
			uprintf("cy%d: i/o size too large\n", vm->um_ctlr);
			bp->b_error = EINVAL;
			bp->b_resid = bp->b_bcount;
			bp->b_flags |= B_ERROR;
			biodone(bp);
			return;
		}
	}
	/* END GROT */
	s = spl3();
	if (dp->b_actf == NULL) {
		dp->b_actf = bp;
		/*
		 * Transport not already active...
		 * put at end of controller queue.
		 */
		 dp->b_forw = NULL;
		 if (vm->um_tab.b_actf == NULL)
			vm->um_tab.b_actf = dp;
		else
			vm->um_tab.b_actl->b_forw = dp;
	} else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	/*
	 * If the controller is not busy, get it going.
	 */
	if (vm->um_tab.b_active == 0)
		cystart(vm);
	splx(s);
}

/*
 * Start activity on a cy controller.
 */
cystart(vm)
	register struct vba_ctlr *vm;
{
	register struct buf *bp, *dp;
	register struct yc_softc *yc;
	register struct cy_softc *cy;
	int ycunit;
	daddr_t blkno;

	dlog((LOG_INFO, "cystart()\n"));
	/*
	 * Look for an idle transport on the controller.
	 */
loop:
	if ((dp = vm->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		vm->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	ycunit = YCUNIT(bp->b_dev);
	yc = &yc_softc[ycunit];
	cy = &cy_softc[CYUNIT(bp->b_dev)];
	/*
	 * Default is that last command was NOT a write command;
	 * if we do a write command we will notice this in cyintr().
	 */
	yc->yc_lastiow = 0;
	if (yc->yc_openf < 0 ||
	    (bp->b_command != CY_SENSE && (cy->cy_tpb.tpstatus&CYS_OL) == 0)) {
		/*
		 * Have had a hard error on a non-raw tape
		 * or the tape unit is now unavailable (e.g.
		 * taken off line).
		 */
		dlog((LOG_INFO, "openf %d command %x status %b\n",
		   yc->yc_openf, bp->b_command, cy->cy_tpb.tpstatus, CYS_BITS));
		bp->b_flags |= B_ERROR;
		goto next;
	}
	if (bp == &ccybuf[CYUNIT(bp->b_dev)]) {
		/*
		 * Execute control operation with the specified count.
		 *
		 * Set next state; give 5 minutes to complete
		 * rewind or file mark search, or 10 seconds per
		 * iteration (minimum 60 seconds and max 5 minutes)
		 * to complete other ops.
		 */
		if (bp->b_command == CY_REW) {
			vm->um_tab.b_active = SREW;
			yc->yc_timo = 5*60;
		} else if (bp->b_command == CY_FSF ||
		    bp->b_command == CY_BSF) {
			vm->um_tab.b_active = SCOM;
			yc->yc_timo = 5*60;
		} else {
			vm->um_tab.b_active = SCOM;
			yc->yc_timo = imin(imax(10*(int)bp->b_repcnt,60),5*60);
		}
		cy->cy_tpb.tprec = htoms(bp->b_repcnt);
		dlog((LOG_INFO, "bpcmd "));
		goto dobpcmd;
	}
	/*
	 * For raw I/O, save the current block
	 * number in case we have to retry.
	 */
	if (bp->b_flags & B_RAW) {
		if (vm->um_tab.b_errcnt == 0) {
			yc->yc_blkno = bp->b_blkno;
			yc->yc_nxrec = yc->yc_blkno + 1;
		}
	} else {
		/*
		 * Handle boundary cases for operation
		 * on non-raw tapes.
		 */
		if (bp->b_blkno > yc->yc_nxrec) {
			/*
			 * Can't read past known end-of-file.
			 */
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			goto next;
		}
		if (bp->b_blkno == yc->yc_nxrec && bp->b_flags&B_READ) {
			/*
			 * Reading at end of file returns 0 bytes.
			 */
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);
			goto next;
		}
		if ((bp->b_flags&B_READ) == 0)
			/*
			 * Writing sets EOF.
			 */
			yc->yc_nxrec = bp->b_blkno + 1;
	}
	if ((blkno = yc->yc_blkno) == bp->b_blkno) {
		caddr_t addr;
		int cmd;

		/*
		 * Choose the appropriate i/o command based on the
		 * transfer size, the estimated block size,
		 * and the controller's internal buffer size.
		 * If the request length is longer than the tape
		 * block length, a buffered read will fail,
		 * thus, we request at most the size that we expect.
		 * We then check for larger records when the read completes.
		 * If we're retrying a read on a raw device because
		 * the original try was a buffer request which failed
		 * due to a record length error, then we force the use
		 * of the raw controller read (YECH!!!!).
		 */
		if (bp->b_flags&B_READ) {
			if (yc->yc_blksize <= cy->cy_bs &&
			    vm->um_tab.b_errcnt == 0)
				cmd = CY_BRCOM;
			else
				cmd = CY_RCOM;
		} else {
			/*
			 * On write error retries erase the
			 * inter-record gap before rewriting.
			 */
			if (vm->um_tab.b_errcnt &&
			    vm->um_tab.b_active != SERASED) {
				vm->um_tab.b_active = SERASE;
				bp->b_command = CY_ERASE;
				yc->yc_timo = 60;
				goto dobpcmd;
			}
			cmd = (bp->b_bcount > cy->cy_bs) ? CY_WCOM : CY_BWCOM;
		}
		vm->um_tab.b_active = SIO;
		addr = (caddr_t)vbasetup(bp, &cy->cy_rbuf, 1);
		cy->cy_tpb.tpcmd = cmd;
		cy->cy_tpb.tpcontrol = yc->yc_dens;
		if (cmd == CY_RCOM || cmd == CY_WCOM)
			cy->cy_tpb.tpcontrol |= CYCW_LOCK;
		cy->cy_tpb.tpstatus = 0;
		cy->cy_tpb.tpcount = 0;
		cyldmba(cy->cy_tpb.tpdata, (caddr_t)addr);
		cy->cy_tpb.tprec = 0;
		if (cmd == CY_BRCOM)
			cy->cy_tpb.tpsize = htoms(imin(yc->yc_blksize,
			    (int)bp->b_bcount));
		else
			cy->cy_tpb.tpsize = htoms(bp->b_bcount);
		cyldmba(cy->cy_tpb.tplink, (caddr_t)0);
		do
			uncache(&cy->cy_ccb.cbgate);
		while (cy->cy_ccb.cbgate == GATE_CLOSED);
		cyldmba(cy->cy_ccb.cbtpb, (caddr_t)&cy->cy_tpb);
		cy->cy_ccb.cbcw = CBCW_IE;
		cy->cy_ccb.cbgate = GATE_CLOSED;
		dlog((LOG_INFO, "CY_GO(%x) cmd %x control %x size %d\n",
		    vm->um_addr, cy->cy_tpb.tpcmd, cy->cy_tpb.tpcontrol,
		    htoms(cy->cy_tpb.tpsize)));
		CY_GO(vm->um_addr);
		return;
	}
	/*
	 * Tape positioned incorrectly; set to seek forwards
	 * or backwards to the correct spot.  This happens 
	 * for raw tapes only on error retries.
	 */
	vm->um_tab.b_active = SSEEK;
	if (blkno < bp->b_blkno) {
		bp->b_command = CY_SFORW;
		cy->cy_tpb.tprec = htoms(bp->b_blkno - blkno);
	} else {
		bp->b_command = CY_SREV;
		cy->cy_tpb.tprec = htoms(blkno - bp->b_blkno);
	}
	yc->yc_timo = imin(imax((int)(10 * htoms(cy->cy_tpb.tprec)), 60), 5*60);
dobpcmd:
	/*
	 * Do the command in bp.  Reverse direction commands
	 * are indicated by having CYCW_REV or'd into their
	 * value.  For these we must set the appropriate bit
	 * in the control field.
	 */
	if (bp->b_command&CYCW_REV) {
		cy->cy_tpb.tpcmd = bp->b_command &~ CYCW_REV;
		cy->cy_tpb.tpcontrol = yc->yc_dens | CYCW_REV;
dlog((LOG_INFO, "cmd %x control %x\n", cy->cy_tpb.tpcmd, cy->cy_tpb.tpcontrol));
	} else {
		cy->cy_tpb.tpcmd = bp->b_command;
		cy->cy_tpb.tpcontrol = yc->yc_dens;
dlog((LOG_INFO, "cmd %x control %x\n", cy->cy_tpb.tpcmd, cy->cy_tpb.tpcontrol));
	}
	cy->cy_tpb.tpstatus = 0;
	cy->cy_tpb.tpcount = 0;
	cyldmba(cy->cy_tpb.tplink, (caddr_t)0);
	do
		uncache(&cy->cy_ccb.cbgate);
	while (cy->cy_ccb.cbgate == GATE_CLOSED);
	cyldmba(cy->cy_ccb.cbtpb, (caddr_t)&cy->cy_tpb);
	cy->cy_ccb.cbcw = CBCW_IE;
	cy->cy_ccb.cbgate = GATE_CLOSED;
	dlog((LOG_INFO, "CY_GO(%x) cmd %x control %x rec %d\n",
	    vm->um_addr, cy->cy_tpb.tpcmd, cy->cy_tpb.tpcontrol,
	    htoms(cy->cy_tpb.tprec)));
	CY_GO(vm->um_addr);
	return;
next:
	/*
	 * Done with this operation due to error or the
	 * fact that it doesn't do anything.
	 * Dequeue the transfer and continue
	 * processing this slave.
	 */
	vm->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	biodone(bp);
	goto loop;
}

/*
 * Cy interrupt routine.
 */
cyintr(cyunit)
	int cyunit;
{
	struct buf *dp;
	register struct buf *bp;
	register struct vba_ctlr *vm = cyminfo[cyunit];
	register struct cy_softc *cy;
	register struct yc_softc *yc;
	int err;
	register state;

	dlog((LOG_INFO, "cyintr(%d)\n", cyunit));
	/*
	 * First, turn off the interrupt from the controller
	 * (device uses Multibus non-vectored interrupts...yech).
	 */
	cy = &cy_softc[vm->um_ctlr];
	cy->cy_ccb.cbcw = CBCW_CLRINT;
	cyldmba(cy->cy_ccb.cbtpb, (caddr_t)&cy->cy_nop);
	cy->cy_ccb.cbgate = GATE_CLOSED;
	CY_GO(vm->um_addr);
	if ((dp = vm->um_tab.b_actf) == NULL) {
		dlog((LOG_ERR, "cy%d: stray interrupt", vm->um_ctlr));
		return;
	}
	bp = dp->b_actf;
	cy = &cy_softc[cyunit];
	cyuncachetpb(cy);
	yc = &yc_softc[YCUNIT(bp->b_dev)];
	/*
	 * If last command was a rewind and tape is
	 * still moving, wait for the operation to complete.
	 */
	if (vm->um_tab.b_active == SREW) {
		vm->um_tab.b_active = SCOM;
		if ((cy->cy_tpb.tpstatus&CYS_RDY) == 0) {
			yc->yc_timo = 5*60;	/* 5 minutes */
			return;
		}
	}
	/*
	 * An operation completed...record status.
	 */
	yc->yc_timo = INF;
	yc->yc_control = cy->cy_tpb.tpcontrol;
	yc->yc_status = cy->cy_tpb.tpstatus;
	yc->yc_resid = bp->b_bcount - htoms(cy->cy_tpb.tpcount);
	dlog((LOG_INFO, "cmd %x control %b status %b resid %d\n",
	    cy->cy_tpb.tpcmd, yc->yc_control, CYCW_BITS,
	    yc->yc_status, CYS_BITS, yc->yc_resid));
	if ((bp->b_flags&B_READ) == 0)
		yc->yc_lastiow = 1;
	state = vm->um_tab.b_active;
	vm->um_tab.b_active = 0;
	/*
	 * Check for errors.
	 */
	if (cy->cy_tpb.tpstatus&CYS_ERR) {
		err = cy->cy_tpb.tpstatus&CYS_ERR;
		dlog((LOG_INFO, "error %d\n", err));
		/*
		 * If we hit the end of tape file, update our position.
		 */
		if (err == CYER_FM) {
			yc->yc_status |= CYS_FM;
			state = SCOM;		/* force completion */
			cyseteof(bp);		/* set blkno and nxrec */
			goto opdone;
		}
		/*
		 * Fix up errors which occur due to backspacing over
		 * the beginning of the tape.
		 */
		if (err == CYER_BOT && cy->cy_tpb.tpcontrol&CYCW_REV) {
			yc->yc_status |= CYS_BOT;
			goto ignoreerr;
		}
		/*
		 * If we were reading raw tape and the only error was that the
		 * record was too long, then we don't consider this an error.
		 */
		if ((bp->b_flags & (B_READ|B_RAW)) == (B_READ|B_RAW) &&
		    err == CYER_STROBE) {
			/*
			 * Retry reads with the command changed to
			 * a raw read if necessary.  Setting b_errcnt
			 * here causes cystart (above) to force a CY_RCOM.
			 */
			if (cy->cy_tpb.tpcmd == CY_BRCOM &&
			    vm->um_tab.b_errcnt++ == 0) {
				yc->yc_blkno++;
				goto opcont;
			} else
				goto ignoreerr;
		}
		/*
		 * If error is not hard, and this was an i/o operation
		 * retry up to 8 times.
		 */
		if (state == SIO && (CYMASK(err) &
		    ((bp->b_flags&B_READ) ? CYER_RSOFT : CYER_WSOFT))) {
			if (++vm->um_tab.b_errcnt < 7) {
				yc->yc_blkno++;
				goto opcont;
			}
		} else
			/*
			 * Hard or non-i/o errors on non-raw tape
			 * cause it to close.
			 */
			if ((bp->b_flags&B_RAW) == 0 &&
			    yc->yc_openf > 0)
				yc->yc_openf = -1;
		/*
		 * Couldn't recover from error.
		 */
		tprintf(yc->yc_ttyp,
		    "yc%d: hard error bn%d status=%b, %s\n", YCUNIT(bp->b_dev),
		    bp->b_blkno, yc->yc_status, CYS_BITS,
		    (err < NCYERROR) ? cyerror[err] : "");
		bp->b_flags |= B_ERROR;
		goto opdone;
	} else if (cy->cy_tpb.tpcmd == CY_BRCOM) {
		int reclen = htoms(cy->cy_tpb.tprec);

		/*
		 * If we did a buffered read, check whether the read
		 * was long enough.  If we asked the controller for less
		 * than the user asked for because the previous record
		 * was shorter, update our notion of record size
		 * and retry.  If the record is longer than the buffer,
		 * bump the errcnt so the retry will use direct read.
		 */
		if (reclen > yc->yc_blksize && bp->b_bcount > yc->yc_blksize) {
			yc->yc_blksize = reclen;
			if (reclen > cy->cy_bs)
				vm->um_tab.b_errcnt++;
			yc->yc_blkno++;
			goto opcont;
		}
	}
	/*
	 * Advance tape control FSM.
	 */
ignoreerr:
	/*
	 * If we hit a tape mark update our position.
	 */
	if (yc->yc_status&CYS_FM && bp->b_flags&B_READ) {
		cyseteof(bp);
		goto opdone;
	}
	switch (state) {

	case SIO:
		/*
		 * Read/write increments tape block number.
		 */
		yc->yc_blkno++;
		yc->yc_blks++;
		if (vm->um_tab.b_errcnt || yc->yc_status & CYS_CR)
			yc->yc_softerrs++;
		yc->yc_blksize = htoms(cy->cy_tpb.tpcount);
		dlog((LOG_ERR, "blocksize %d", yc->yc_blksize));
		goto opdone;

	case SCOM:
		/*
		 * For forward/backward space record update current position.
		 */
		if (bp == &ccybuf[CYUNIT(bp->b_dev)])
			switch ((int)bp->b_command) {

			case CY_SFORW:
				yc->yc_blkno -= bp->b_repcnt;
				break;

			case CY_SREV:
				yc->yc_blkno += bp->b_repcnt;
				break;
			}
		goto opdone;
	
	case SSEEK:
		yc->yc_blkno = bp->b_blkno;
		goto opcont;

	case SERASE:
		/*
		 * Completed erase of the inter-record gap due to a
		 * write error; now retry the write operation.
		 */
		vm->um_tab.b_active = SERASED;
		goto opcont;
	}

opdone:
	/*
	 * Reset error count and remove from device queue.
	 */
	vm->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	/*
	 * Save resid and release resources.
	 */
	bp->b_resid = bp->b_bcount - htoms(cy->cy_tpb.tpcount);
	if (bp != &ccybuf[cyunit])
		vbadone(bp, &cy->cy_rbuf);
	biodone(bp);
	/*
	 * Circulate slave to end of controller
	 * queue to give other slaves a chance.
	 */
	vm->um_tab.b_actf = dp->b_forw;
	if (dp->b_actf) {
		dp->b_forw = NULL;
		if (vm->um_tab.b_actf == NULL)
			vm->um_tab.b_actf = dp;
		else
			vm->um_tab.b_actl->b_forw = dp;
	}
	if (vm->um_tab.b_actf == 0)
		return;
opcont:
	cystart(vm);
}

cytimer(dev)
	int dev;
{
	register struct yc_softc *yc = &yc_softc[YCUNIT(dev)];
	int s;

	if (yc->yc_openf == 0 && yc->yc_timo == INF) {
		yc->yc_tact = 0;
		return;
	}
	if (yc->yc_timo != INF && (yc->yc_timo -= 5) < 0) {
		printf("yc%d: lost interrupt\n", YCUNIT(dev));
		yc->yc_timo = INF;
		s = spl3();
		cyintr(CYUNIT(dev));
		splx(s);
	}
	timeout(cytimer, (caddr_t)dev, 5*hz);
}

cyseteof(bp)
	register struct buf *bp;
{
	register int cyunit = CYUNIT(bp->b_dev);
	register struct cy_softc *cy = &cy_softc[cyunit];
	register struct yc_softc *yc = &yc_softc[YCUNIT(bp->b_dev)];

	if (bp == &ccybuf[cyunit]) {
		if (yc->yc_blkno > bp->b_blkno) {
			/* reversing */
			yc->yc_nxrec = bp->b_blkno - htoms(cy->cy_tpb.tpcount);
			yc->yc_blkno = yc->yc_nxrec;
		} else {
			yc->yc_blkno = bp->b_blkno + htoms(cy->cy_tpb.tpcount);
			yc->yc_nxrec = yc->yc_blkno - 1;
		}
		return;
	}
	/* eof on read */
	yc->yc_nxrec = bp->b_blkno;
}

/*ARGSUSED*/
cyioctl(dev, cmd, data, flag)
	caddr_t data;
	dev_t dev;
{
	int ycunit = YCUNIT(dev);
	register struct yc_softc *yc = &yc_softc[ycunit];
	register struct buf *bp = &ccybuf[CYUNIT(dev)];
	register callcount;
	int fcount, op;
	struct mtop *mtop;
	struct mtget *mtget;
	/* we depend of the values and order of the MT codes here */
	static cyops[] =
	{CY_WEOF,CY_FSF,CY_BSF,CY_SFORW,CY_SREV,CY_REW,CY_OFFL,CY_SENSE};

	switch (cmd) {

	case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		switch (op = mtop->mt_op) {

		case MTWEOF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop->mt_count;
			break;

		case MTFSF: case MTBSF:
			callcount = mtop->mt_count;
			fcount = 1;
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
#ifdef notdef
			/*
			 * Gagh, this controller is the pits...
			 */
			if (op == MTFSF || op == MTBSF) {
				do
					cycommand(dev, cyops[op], 1);
				while ((bp->b_flags&B_ERROR) == 0 &&
				 (yc->yc_status&(CYS_EOT|CYS_BOT|CYS_FM)) == 0);
			} else
#endif
				cycommand(dev, cyops[op], fcount);
			dlog((LOG_INFO,
			    "cyioctl: status %x, b_flags %x, resid %d\n",
			    yc->yc_status, bp->b_flags, bp->b_resid));
			if ((bp->b_flags&B_ERROR) ||
			    (yc->yc_status&(CYS_BOT|CYS_EOT)))
				break;
		}
		bp->b_resid = callcount + 1;
		return (geterror(bp));

	case MTIOCGET:
		cycommand(dev, CY_SENSE, 1);
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = yc->yc_status;
		mtget->mt_erreg = yc->yc_control;
		mtget->mt_resid = yc->yc_resid;
		mtget->mt_type = MT_ISCY;
		break;

	default:
		return (ENXIO);
	}
	return (0);
}

/*
 * Poll until the controller is ready.
 */
cywait(cp)
	register struct cyccb *cp;
{
	register int i = 5000;

	uncache(&cp->cbgate);
	while (i-- > 0 && cp->cbgate == GATE_CLOSED) {
		DELAY(1000);
		uncache(&cp->cbgate);
	}
	return (i <= 0);
}

/*
 * Load a 20 bit pointer into a Tapemaster pointer.
 */
cyldmba(reg, value)
	register u_char *reg;
	caddr_t value;
{
	register int v = (int)value;

	*reg++ = v;
	*reg++ = v >> 8;
	*reg++ = 0;
	*reg = (v&0xf0000) >> 12;
}

/*
 * Unconditionally reset all controllers to their initial state.
 */
cyreset(vba)
	int vba;
{
	register caddr_t addr;
	register int ctlr;

	for (ctlr = 0; ctlr < NCY; ctlr++)
		if (cyminfo[ctlr] && cyminfo[ctlr]->um_vbanum == vba) {
			addr = cyminfo[ctlr]->um_addr;
			CY_RESET(addr);
			if (!cyinit(ctlr, addr)) {
				printf("cy%d: reset failed\n", ctlr);
				cyminfo[ctlr] = NULL;
			}
		}
}

cyuncachetpb(cy)
	struct cy_softc *cy;
{
	register long *lp = (long *)&cy->cy_tpb;
	register int i;

	for (i = 0; i < howmany(sizeof (struct cytpb), sizeof (long)); i++)
		uncache(lp++);
}

/*
 * Dump routine.
 */
#define	DUMPREC	(32*1024)
cydump(dev)
	dev_t dev;
{
	register struct cy_softc *cy;
	register int bs, num, start;
	register caddr_t addr;
	int unit = CYUNIT(dev), error;

	if (unit >= NCY || cyminfo[unit] == 0 ||
	    (cy = &cy_softc[unit])->cy_bs == 0 || YCUNIT(dev) >= NYC)
		return (ENXIO);
	if (cywait(&cy->cy_ccb))
		return (EFAULT);
#define	phys(a)	((caddr_t)((int)(a)&~0xc0000000))
	addr = phys(cyminfo[unit]->um_addr);
	num = maxfree, start = NBPG*2;
	while (num > 0) {
		bs = num > btoc(DUMPREC) ? btoc(DUMPREC) : num;
		error = cydwrite(cy, start, bs, addr);
		if (error)
			return (error);
		start += bs, num -= bs;
	}
	cyweof(cy, addr);
	cyweof(cy, addr);
	uncache(&cy->cy_tpb);
	if (cy->cy_tpb.tpstatus&CYS_ERR)
		return (EIO);
	cyrewind(cy, addr);
	return (0);
}

cydwrite(cy, pf, npf, addr)
	register struct cy_softc *cy;
	int pf, npf;
	caddr_t addr;
{

	cy->cy_tpb.tpcmd = CY_WCOM;
	cy->cy_tpb.tpcontrol = CYCW_LOCK|CYCW_25IPS|CYCW_16BITS;
	cy->cy_tpb.tpstatus = 0;
	cy->cy_tpb.tpsize = htoms(npf*NBPG);
	cyldmba(cy->cy_tpb.tplink, (caddr_t)0);
	cyldmba(cy->cy_tpb.tpdata, (caddr_t)(pf*NBPG));
	cyldmba(cy->cy_ccb.cbtpb, (caddr_t)&cy->cy_tpb);
	cy->cy_ccb.cbgate = GATE_CLOSED;
	CY_GO(addr);
	if (cywait(&cy->cy_ccb))
		return (EFAULT);
	uncache(&cy->cy_tpb);
	if (cy->cy_tpb.tpstatus&CYS_ERR)
		return (EIO);
	return (0);
}

cyweof(cy, addr)
	register struct cy_softc *cy;
	caddr_t addr;
{

	cy->cy_tpb.tpcmd = CY_WEOF;
	cy->cy_tpb.tpcount = htoms(1);
	cy->cy_ccb.cbgate = GATE_CLOSED;	
	CY_GO(addr);
	(void) cywait(&cy->cy_ccb);
}

cyrewind(cy, addr)
	register struct cy_softc *cy;
	caddr_t addr;
{

	cy->cy_tpb.tpcmd = CY_REW;	
	cy->cy_tpb.tpcount = htoms(1);
	cy->cy_ccb.cbgate = GATE_CLOSED;	
	CY_GO(addr);
	(void) cywait(&cy->cy_ccb);
}
#endif
