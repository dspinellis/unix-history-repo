/*	cy.c	1.1	85/07/21	*/
/*	cy.c	Tahoe version 	Mar 1983.	*/

#include "cy.h"
#if NCY > 0 /* number of CYPHER tapes in system */
/*
 * Cypher tape driver
 *
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/conf.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../machine/pte.h"
#include "../vba/vbavar.h"
#include "../h/mtio.h"
#include "../machine/mtpr.h"
#include "../h/ioctl.h"
#include "../h/cmap.h"
#include "../h/uio.h"

#include "../vba/cyvar.h"

#define NTM 1		/* number of TAPEMASTER controllers */

/*
 * There is a ccybuf per tape controller.
 * It is used as the token to pass to the control routines
 * and also acts as a lock on the slaves on the
 * controller, since there is only one per controller.
 * In particular, when the tape is rewinding on close we release
 * the user process but any further attempts to use the tape drive
 * before the rewind completes will hang waiting for ccybuf.
 */
struct	buf	ccybuf[NTM];

/*
 * Raw tape operations use rcybuf.  The driver
 * notices when rcybuf is being used and allows the user
 * program to continue after errors and read records
 * not of the standard length (BSIZE).
 */
struct	buf	rcybuf[NTM];
long	cybufused = 0;

/*
 * Driver interface routines and variables.
 */
int	cyprobe(), cyslave(), cyattach(), cydgo(), cyintr();
int	cywait(), cyrewind();
unsigned	tminphys();
struct	vba_ctlr *cyminfo[NTM];
struct	vba_device *cydinfo[NCY];
struct	buf cyutab[NCY];
short	cytotm[NCY];
extern char	cyutl[];
long	cystd[] = { 0x400000, 0 };
struct	vba_driver cydriver =
 { cyprobe, cyslave, cyattach, cydgo, cystd, "yc", cydinfo, "cy",
	cyminfo, 0 };

/* bits in minor device */
#define	CYUNIT(dev)	(minor(dev)&07)		/* tape unit number */
#define	TMUNIT(dev)	(cytotm[CYUNIT(dev)])	/* tape controller number */
#define	T_NOREWIND	0x08			/* no rewind bit */
#define	T_100IPS	0x10			/* high speed flag */

int	pflag;			/* probe flag, set every interrupt by cyintr */

#define	INF	(daddr_t)1000000L
extern int hz;

struct scp	/* SYSTEM CONFIGUREATION POINTER */
{
  char sysbus ;	/* width of system buss 0=8;1=16 */
  char nu1 ;
  char pt_scb[4] ;	/* pointer to ->SYSTEM CONFIGUREATION BLOCK */
};

/* absolute address - jumpered on the controller */
#define	SCP	((struct scp *)0xc0000c06)

struct Scb	/* SYSTEM CONFIGUREATION BLOCK */
{
  char sysblk[1] ;	/* 0x03 fixed value code */
  char nu2[1] ;
  char pt_ccb[4] ;	/* pointer to ->CHANNEL CONTROL BLOCK */
}Scb;

struct ccb	/* CHANNEL CONTROL BLOCK */
{
  char ccw[1] ;		/* 0x11 normal; 0x09 clear non_vect interrupt */
  char gate[1] ;	/* This is "the" GATE */
  char pt_tpb[4] ;	/* pointer to ->TAPE OPERATION BLOCK or MOVE BLOCK */
}ccb;

struct tpb	/* TAPE OPERATIONS PARAMETER BLOCK */
{
  long cmd ;		/* COMMAND (input) */
  char control[2] ;	/* CONTROL (input) */
  short count ;	/* RETURN COUNT (output) */
  short size ;	/* BUFFER SIZE (input/output) */
  short rec_over ;	/* RECORDS/OVERRUN (input/output) */
  char pt_data[4] ;	/* pointer to ->SOURCE/DEST (input) */
  char status[2] ;	/* STATUS (output) */
  char pt_link[4] ;	/* pointer to ->INTERRUPT/PARAMETER BLOCK (input) */
} tpb[NTM];

struct tpb cycool	/* tape parameter block to clear interrupts */
= {
	0L,		/* command */
	0, 0,		/* control */
	0,		/* count */
	0,		/* size */
	0,		/* rec_over */
	0, 0, 0, 0,	/* pt_data */
	0, 0,		/* status */
	0, 0, 0, 0		/* pt_link */
} ;	
/*
 * Software state per tape transport.
 *
 * 1. A tape drive is a unique-open device; we refuse opens when it is already.
 * 2. We keep track of the current position on a block tape and seek
 *    before operations by forward/back spacing if necessary.
 * 3. We remember if the last operation was a write on a tape, so if a tape
 *    is open read write and the last thing done is a write we can
 *    write a standard end of tape mark (two eofs).
 */
struct	cy_softc {
	char	cy_openf;	/* lock against multiple opens */
	char	cy_lastiow;	/* last op was a write */
	daddr_t	cy_blkno;	/* block number, for block device tape */
	daddr_t	cy_nxrec;	/* position of end of tape, if known */
	daddr_t	cy_timo;	/* time until timeout expires */
	short	cy_tact;	/* timeout is active */
	short	cy_count;	/* return count of last operation */
	char	cy_status[2];	/* return status of last operation */
} cy_softc[NTM];

/* 
 * I/O buffer for raw devices.
 */
char cybuf[TBUFSIZ*NBPG]; 		/* 10k buffer */

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
 * a cypher at address ctlr_vaddr.  
 * Reset the controller.
 * Our goal is to make the device interrupt.
 */
cyprobe(ctlr_vaddr)
	caddr_t ctlr_vaddr;
{
	int *ip;

	pflag = 0;			/* clear interrupt flag */
	if (badcyaddr(ctlr_vaddr + 1))	/* check for versabuss timeout  */
		return (0);
	/*
	 * Initialize the system configuration pointer
	 */
	ip = (int *)vtopte(0, btop(SCP)); *ip &= ~PG_PROT; *ip |= PG_KW;
	mtpr(SCP, TBIS);
	SCP->sysbus = 1;			/* system width = 16 bits. */
	/* initialize the pointer to the system configuration block */
	set_pointer((int)&Scb.sysblk[0], (char *)SCP->pt_scb);
	/*
	 * Initialize the system configuration block.
	 */
	Scb.sysblk[0] = 0x3;		/* fixed value */
	/* initialize the pointer to the channel control block */
	set_pointer((int)&ccb.ccw[0], (char *)Scb.pt_ccb);
	/*
	 * Initialize the channel control block.
	 */
	ccb.ccw[0] = 0x11;		/* normal interrupts */
	/* initialize the pointer to the tape parameter block */
	set_pointer((int)&tpb[0], (char *)ccb.pt_tpb);
	/*
	 * set the command to be CONFIGURE.
	 */
	tpb[0].cmd = CONFIG;
	tpb[0].control[0] = CW_I;	/* interrupt on completion */
	tpb[0].control[1] = CW_16bits;
	ccb.gate[0] = GATE_CLOSED;	
	*ip &= ~PG_PROT; *ip |= PG_KR;
	mtpr(SCP, TBIS);
	TM_ATTENTION(ctlr_vaddr, 0xff);	/* execute! */
	if (cywait()) return(0);
	else return(1);
}

/*
 * Due to a design flaw, we cannot ascertain if the tape
 * exists or not unless it is on line - ie: unless a tape is
 * mounted. This is too severe a restriction to bear,
 * so all units are assumed to exist.
 */
/*ARGSUSED*/
cyslave(ui, ctlr_vaddr)
	struct vba_device *ui;
	caddr_t ctlr_vaddr;
{

	return (1);
}

/*
 * Record attachment of the unit to the controller.
 */
/*ARGSUSED*/
cyattach(ui)
	struct vba_device *ui;
{

	/*
	 * Cytotm is used in TMUNIT to index the ccybuf and rcybuf
	 * arrays given a cy unit number.
	 */
	cytotm[ui->ui_unit] = ui->ui_mi->um_ctlr;
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
	int flag;
{
	register int cyunit, s;
	register struct vba_device *ui;
	register struct cy_softc *cy;

	cyunit = CYUNIT(dev);
	if (cyunit>=NCY || (cy = &cy_softc[cyunit])->cy_openf ||
	    (ui = cydinfo[cyunit]) == 0 || ui->ui_alive == 0)
		return ENXIO;
	cycommand(dev, (int)DRIVE_S, 1);	/* drive status */
	uncache(&tpb[cyunit].status[0]);
	if ((tpb[cyunit].status[0]&(CS_DR|CS_OL)) != (CS_DR|CS_OL)) {
		uprintf("cy%d: not online\n", cyunit);
		return EIO;
	}
	if ((flag&FWRITE) && (tpb[cyunit].status[0]&CS_P)) {
		uprintf("cy%d: no write ring\n", cyunit);
		return EIO;
	}
	cy->cy_openf = 1;
	cy->cy_blkno = (daddr_t)0;
	cy->cy_nxrec = INF;
	cy->cy_lastiow = 0;
	s = spl8();
	if (cy->cy_tact == 0) {
		cy->cy_timo = INF;
		cy->cy_tact = 1;
		timeout(cytimer, (caddr_t)dev, 5*hz);
	}
	splx(s);
	return 0;
}

/*
 * Close tape device.
 *
 * If tape was open for writing or last operation was
 * a write, then write two EOF's and backspace over the last one.
 * Unless this is a non-rewinding special file, rewind the tape.
 * Make the tape available to others.
 */
cyclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct cy_softc *cy = &cy_softc[CYUNIT(dev)];

	if (flag == FWRITE || (flag&FWRITE) && cy->cy_lastiow) {
		cycommand(dev, (int)WRIT_FM, 1);	/* write file mark */
		cycommand(dev, (int)WRIT_FM, 1);
		cycommand(dev, (int)SP_BACK, 1);	/* space back */
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		/*
		 * 0 count means don't hang waiting for rewind complete
		 * rather ccybuf stays busy until the operation completes
		 * preventing further opens from completing by
		 * preventing a SENSE operation from completing.
		 */
		cycommand(dev, (int)REWD_TA, 0);
	cy->cy_openf = 0;
}

int commflag;	/* signal cystrategy that it is called from cycommand */

/*
 * Execute a command on the tape drive
 * a specified number of times.
 */
cycommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	int s;

	bp = &ccybuf[TMUNIT(dev)];
	s = spl8();
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
	commflag = 1;
	cystrategy(bp);
	commflag = 0;
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
cystrategy(bp)
	register struct buf *bp;
{
	int cyunit = CYUNIT(bp->b_dev);
	int s;
	register struct vba_ctlr *um;
	register struct buf *dp;

	/*
	 * Put transfer at end of unit queue
	 */
	dp = &cyutab[cyunit];
	bp->av_forw = NULL;
	s = spl8();
/*
 * Next piece of logic takes care of unusual cases when more than
 * a full block is required. 
 * The driver reads the tape to a temporary buffer and
 * then moves the amount needed back to the process.
 * In this case, the flag NOT1K is set.
 */

	if (commflag == 0)
		buf_setup(bp, 1);
	um = cydinfo[cyunit]->ui_mi;
	if (dp->b_actf == NULL) {
		dp->b_actf = bp;
		/*
		 * Transport not already active...
		 * put at end of controller queue.
		 */
		dp->b_forw = NULL;
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
		cystart(um);
	splx(s);
}

/*
 * Start activity on a cypher controller.
 */
cystart(um)
	register struct vba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct tpb *tp; 
	register struct cy_softc *cy;
	register int phadr;
	int cyunit, timer;
	daddr_t blkno;
	caddr_t	ctlr_vaddr;
	ctlr_vaddr = um->um_addr;
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
	cyunit = CYUNIT(bp->b_dev);
	cy = &cy_softc[cyunit];
	tp = &tpb[cyunit];
	/*
	 * Default is that last command was NOT a write command;
	 * if we do a write command we will notice this in cyintr().
	 */
	cy->cy_lastiow = 0;
	uncache(&tp->status[0]);
	uncache(&tp->count);
	cy->cy_count = TM_SHORT(tp->count);
	cy->cy_status[0] = tp->status[0];
	cy->cy_status[1] = tp->status[1];
	if (cy->cy_openf < 0 || 
		(bp->b_command != DRIVE_S) && 
		((tp->status[0]&CS_OL) != CS_OL)) {
		/*
		 * Have had a hard error on a non-raw tape
		 * or the tape unit is now unavailable
		 * (e.g. taken off line).
		 */
		bp->b_flags |= B_ERROR;
		goto next;
	}
	if (bp == &ccybuf[TMUNIT(bp->b_dev)]) {
		/*
		 * Execute control operation with the specified count.
		 * Set next state; give 5 minutes to complete
		 * rewind, or 10 seconds per iteration (minimum 60
		 * seconds and max 5 minutes) to complete other ops.
		 */
		if (bp->b_command == REWD_TA) {
			um->um_tab.b_active = SREW;
			cy->cy_timo = 5 * 60;
		} else {
			um->um_tab.b_active = SCOM;
			cy->cy_timo = imin(imax(10*(int)bp->b_repcnt, 60), 5*60);
		}
		/*
		 * Prepare parameter block for controller
		 */
		tp->cmd = bp->b_command;
		tp->control[0] = (CW_I | (cyunit<<CW_TSs));
		if (minor(bp->b_dev)&T_100IPS)
			tp->control[1] = (CW_100ips | CW_16bits);
		else	tp->control[1] = (CW_25ips | CW_16bits);
		if (bp->b_command == SP_BACK) {
			tp->control[1] |= CW_R;
			tp->cmd = SPACE;
			tp->rec_over = TM_SHORT((short)bp->b_repcnt);
		}
		if (bp->b_command == SP_FORW) 
			tp->rec_over = TM_SHORT((short)bp->b_repcnt);
		if (bp->b_command == SRFM_BK) {
			tp->control[1] |= CW_R;
			tp->cmd = SERH_FM;
			tp->rec_over = TM_SHORT((short)bp->b_repcnt);
		}
		if (bp->b_command == SRFM_FD) 
			tp->rec_over = TM_SHORT((short)bp->b_repcnt);
		tp->status[0] = tp->status[1] = 0;
		tp->count = 0;
		set_pointer((int)&tpb[cyunit], (char *)ccb.pt_tpb);
		goto dobpcmd;
	}
	/*
	 * The following checks handle boundary cases for operation
	 * on non-raw tapes.  On raw tapes the initialization of
	 * cy->cy_nxrec by cyphys causes them to be skipped normally
	 */
	if (bdbtofsb(bp->b_blkno) > cy->cy_nxrec) {
		/*
		 * Can't read past known end-of-file.
		 */
		bp->b_flags |= B_ERROR;
		bp->b_error = ENXIO;
		goto next;
	}
	if (bdbtofsb(bp->b_blkno) == cy->cy_nxrec &&
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
		cy->cy_nxrec = bdbtofsb(bp->b_blkno) + 1;
	/*
	 * If the data transfer command is in the correct place,
	 * set up the tape parameter block, and start the i/o.
	 */
	if ((blkno = cy->cy_blkno) == bdbtofsb(bp->b_blkno)) {
		um->um_tab.b_active = SIO;
		cy->cy_timo = 60;	/* premature, but should serve */

		phadr = get_ioadr(bp, cybuf, CYmap, cyutl);

		if ( (bp->b_flags & B_READ) == 0) 
			tp->cmd = WRIT_BU;
		else tp->cmd = READ_BU;
		tp->control[0] = (CW_I | (cyunit<<CW_TSs));
		if (minor(bp->b_dev)&T_100IPS)
			tp->control[1] = (CW_100ips | CW_16bits);
		else	tp->control[1] = (CW_25ips | CW_16bits);
		tp->status[0] = tp->status[1] = 0;
		tp->count = 0;
		tp->size = TM_SHORT(bp->b_bcount);
		set_pointer(phadr, (char *)tp->pt_data);
		set_pointer((int)&tpb[cyunit], (char *)ccb.pt_tpb);
		goto dobpcmd;
	}
	/*
	 * Tape positioned incorrectly;
	 * set to seek forwards or backwards to the correct spot.
	 */
	um->um_tab.b_active = SSEEK;
	tp->cmd = SPACE;
	tp->control[0] = (CW_I | (cyunit<<CW_TSs));
	if (minor(bp->b_dev)&T_100IPS)
		tp->control[1] = (CW_100ips | CW_16bits);
	else	tp->control[1] = (CW_25ips | CW_16bits);
	tp->status[0] = tp->status[1] = 0;
	set_pointer((int)&tpb[cyunit], (char *)ccb.pt_tpb);
	if (blkno < bdbtofsb(bp->b_blkno)) 
		tp->rec_over = TM_SHORT((short)(blkno - bdbtofsb(bp->b_blkno)));
	else {
		tp->rec_over = TM_SHORT((short)(bdbtofsb(bp->b_blkno) - blkno));
		tp->control[1] |= CW_R;
	}
	cy->cy_timo = imin(imax(10 * (int)TM_SHORT(tp->rec_over), 60), 5 * 60);
dobpcmd:
	/*
	 * Do the command in bp.
	 */
	timer = 8000;			/* software tolerance for gate open */
	uncache(&ccb.gate[0]);
	while (ccb.gate[0] != GATE_OPEN) {
		if (--timer == 0) {
			ccb.ccw[0] = 0x9;	/* forget it...... */
			TM_RESET(ctlr_vaddr, 0xff);
			bp->b_flags |= B_ERROR;
			goto next;
		}
		uncache(&ccb.gate[0]);
	}
	ccb.ccw[0] = 0x11;		/* normal mode */
	ccb.gate[0] = GATE_CLOSED;	
	TM_ATTENTION(ctlr_vaddr, 0xff);		/* execute! */
	return;

next:
	/*
	 * Done with this operation due to error or
	 * the fact that it doesn't do anything.
	 * dequeue the transfer and continue processing this slave.
	 */
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

/*
 * Kept for historical reasons. Probably not neccessary. 
 */
cydgo(um)
	struct vba_ctlr *um;
{
}

/*
 * Cy interrupt routine.
 */
/*ARGSUSED*/
cyintr(ctlr)
	int ctlr;
{
	struct buf *dp;
	register struct buf *bp;
	register struct tpb *tp;
	register struct vba_ctlr *um = cyminfo[ctlr];
	register struct cy_softc *cy;
	caddr_t ctlr_vaddr;
	int cyunit;
	register state;

	/*
	 * First we clear the interrupt and close the gate.
	 */
	ctlr_vaddr = um->um_addr;
	ccb.ccw[0] = 0x9;	/* clear the interrupt */
	ccb.gate[0] = GATE_CLOSED;
	set_pointer((int)&cycool, (char *)ccb.pt_tpb);
	cycool.cmd = NO_OP;	/* no operation */
	cycool.control[0] = 0;	/* No INTERRUPTS */
	cycool.control[1] = 0;
	TM_ATTENTION(ctlr_vaddr, 0xff);	/* cool it ! */
	cywait();
	/*
	 * Now we can start handling the interrupt.
	 */
	pflag = 1;		/* set for the probe routine */
	if (intenable == 0) return;	/* ignore all interrupts */
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	bp = dp->b_actf;
	cyunit = CYUNIT(bp->b_dev);
	tp = &tpb[cyunit];
	cy = &cy_softc[cyunit];
	/*
	 * If last command was a rewind, and tape is still
	 * rewinding, wait for the rewind complete interrupt.
	 */
	if (um->um_tab.b_active == SREW) {
		um->um_tab.b_active = SCOM;
		/* uncache(&tp->status[1]); */
		/* if (tp->status[1]&CS_CC != CS_CC) { */ /* not completed */
			/* cy->cy_timo = 5*60; */	 /* 5 minutes */
			/* return; */
		/* } */
	}
	/*
	 * An operation completed... update status
	 */
	cy->cy_timo = INF;
	uncache(&tp->count);
	uncache(&tp->status[0]);
	cy->cy_count = TM_SHORT(tp->count);
	cy->cy_status[0] = tp->status[0];
	cy->cy_status[1] = tp->status[1];
	if ((bp->b_flags & B_READ) == 0)
		cy->cy_lastiow = 1;
	state = um->um_tab.b_active;
	um->um_tab.b_active = 0;
	/*
	 * Check for errors.
	 */
	if (tp->status[1] & CS_ERm) {
		/*
		 * If we hit the end of the tape file, update our position.
		 */
		if (tp->status[0] & CS_FM) 
		{
			cyseteof(bp);		/* set blkno and nxrec */
			state = SCOM;
			goto opdone;
		}
		/* If reading raw device and block was too short ignore the
		 * error and let the user program decide what to do.
		 */
		if ((tp->status[0] & ER_TOF) && /* (bp->b_flags & B_PHYS) && */
			(bp->b_flags & B_READ)) goto cont;
		cy->cy_openf = -1;		/* cause to close */
		printf("cy%d: hard error bn %d er=%x\n", cyunit,
		    bp->b_blkno, tp->status[1]&CS_ERm);
		bp->b_flags |= B_ERROR;
		goto opdone;
	}
	/*
	 * If we were reading block tape and the record
	 * was too long, we consider this an error.
	 */
cont:
	uncache(&tp->count);
	uncache(&tp->cmd);
	if (bp != &rcybuf[TMUNIT(bp->b_dev)] && (tp->cmd == READ_BU) &&
	    bp->b_bcount < TM_SHORT(tp->count)) {
		cy->cy_openf = -1;		/* cause to close */
		printf("cy%d: error - tape block too long \n", cyunit);
		bp->b_flags |= B_ERROR;
		goto opdone;
	}
	/*
	 * No errors.
	 * Advance tape control FSM.
	 */
	switch (state) {

	case SIO:
		/*
		 * Read/write increments tape block number
		 */
		cy->cy_blkno++;
		end_transfer(bp, cybuf, CYmap, cyutl);
		goto opdone;

	case SCOM:
		/*
		 * For forward/backward space record update current position.
		 */
		if (bp == &ccybuf[TMUNIT(bp->b_dev)])
		switch (bp->b_command) {

		case SP_FORW:
			cy->cy_blkno += bp->b_repcnt;
			break;

		case SP_BACK:
			cy->cy_blkno -= bp->b_repcnt;
			break;
		}
		goto opdone;

	case SSEEK:
		cy->cy_blkno = bdbtofsb(bp->b_blkno);
		goto opcont;

	default:
		panic("cyintr");
	}
opdone:
	/*
	 * Reset error count and remove
	 * from device queue.
	 */
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	uncache(&tp->count);
	bp->b_resid = bp->b_bcount - TM_SHORT(tp->count);
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
	cystart(um);
}

cytimer(dev)
	int dev;
{
	register struct cy_softc *cy = &cy_softc[CYUNIT(dev)];
	int	s;

	if (cy->cy_timo != INF && (cy->cy_timo -= 5) < 0) {
		printf("cy%d: lost interrupt\n", CYUNIT(dev));
		cy->cy_timo = INF;
		s = spl8();
		cyintr(TMUNIT(dev));
		splx(s);
		return;
	}
	if (cy->cy_timo != INF ) timeout(cytimer, (caddr_t)dev, 5*hz);
}

cyseteof(bp)
	register struct buf *bp;
{
	register int cyunit = CYUNIT(bp->b_dev);
	register struct cy_softc *cy = &cy_softc[cyunit];
	register struct tpb *tp;

	tp = &tpb[cyunit];
	uncache(&tp->rec_over);
	if (bp == &ccybuf[TMUNIT(bp->b_dev)]) {
		if (cy->cy_blkno > bdbtofsb(bp->b_blkno)) {
			/* reversing */
			cy->cy_nxrec = bdbtofsb(bp->b_blkno) - (int)TM_SHORT(tp->rec_over);
			cy->cy_blkno = cy->cy_nxrec;
		} else {
			/* spacing forward */
			cy->cy_blkno = bdbtofsb(bp->b_blkno) + (int)TM_SHORT(tp->rec_over);
			cy->cy_nxrec = cy->cy_blkno - 1;
		}
		return;
	} 
	/* eof on read */
	cy->cy_nxrec = bdbtofsb(bp->b_blkno);
}

cyread(dev, uio)
dev_t dev;
struct uio *uio;
{
	register error;

	error = cyphys(dev, uio);
	if (error)
		return error;
	while (cybufused) sleep (&cybufused, PRIBIO+1);
	cybufused = 1;
	error = physio(cystrategy, &rcybuf[TMUNIT(dev)], dev, B_READ, tminphys, uio);
	cybufused = 0;
	wakeup (&cybufused);
	return error;
}

cywrite(dev, uio)
dev_t dev;
struct uio *uio;
{
	register error;

	error = cyphys(dev, uio);
	if (error)
		return error;
	while (cybufused) sleep (&cybufused, PRIBIO+1);
	cybufused = 1;
	error = physio(cystrategy, &rcybuf[TMUNIT(dev)], dev, B_WRITE, tminphys, uio);
	cybufused = 0;
	wakeup (&cybufused);
	return error;
}


cyreset(uban)
	int uban;
{
	register struct vba_ctlr *um;
	register cy0f, cyunit;
	register struct vba_device *ui;
	register struct buf *dp;

	for (cy0f = 0; cy0f < NTM; cy0f++) {
		if ((um = cyminfo[cy0f]) == 0 || um->um_alive == 0 ||
		   um->um_vbanum != uban)
			continue;
		printf(" cy%d", cy0f);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		for (cyunit = 0; cyunit < NCY; cyunit++) {
			if ((ui = cydinfo[cyunit]) == 0 || ui->ui_mi != um ||
			    ui->ui_alive == 0)
				continue;
			dp = &cyutab[cyunit];
			dp->b_active = 0;
			dp->b_forw = 0;
			dp->b_command = DRIVE_R;
			if (um->um_tab.b_actf == NULL)
				um->um_tab.b_actf = dp;
			else
				um->um_tab.b_actl->b_forw = dp;
			um->um_tab.b_actl = dp;
			if (cy_softc[cyunit].cy_openf > 0)
				cy_softc[cyunit].cy_openf = -1;
		}
		cystart(um);
	}
}


cyioctl(dev, cmd, data, flag)
	caddr_t data;
	dev_t dev;
{
	int cyunit = CYUNIT(dev);
	register struct cy_softc *cy = &cy_softc[cyunit];
	register struct buf *bp = &ccybuf[TMUNIT(dev)];
	register callcount;
	int fcount;
	struct mtop *mtop;
	struct mtget *mtget;
	/* we depend of the values and order of the MT codes here */
	static cyops[] =
	   {WRIT_FM, SRFM_FD, SRFM_BK, SP_FORW, SP_BACK, REWD_TA, OFF_UNL, NO_OP};

	switch (cmd) {
		case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		switch(mtop->mt_op) {
		case MTWEOF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;
		case MTFSF: case MTBSF:
			callcount = mtop->mt_count;
			fcount = INF;
			break;
		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop->mt_count;
			break;
		case MTREW: case MTOFFL: case MTNOP:
			callcount = 1;
			fcount = 1;
			break;
		default:
			return ENXIO;
		}
		if (callcount <= 0 || fcount <= 0)
			return EINVAL;
		while (--callcount >= 0) {
			cycommand(dev, cyops[mtop->mt_op], fcount);
			if ((bp->b_flags&B_ERROR) || cy->cy_status[1]&CS_ERm)
				break;
		}
		return geterror(bp);
	case MTIOCGET:
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = cy->cy_status[0];
		mtget->mt_erreg = cy->cy_status[1];
		mtget->mt_resid = cy->cy_count;
		mtget->mt_type = MT_ISCY;
		break;
	default:
		return ENXIO;
	}
	return 0;
}



/*
 * Check that a raw device exists.
 * If it does, set up cy_blkno and cy_nxrec
 * so that the tape will appear positioned correctly.
 */
cyphys(dev, uio)
dev_t dev;
struct uio *uio;
{
	register int cyunit = CYUNIT(dev);
	register daddr_t a;
	register struct cy_softc *cy;
	register struct vba_device *ui;

	if (cyunit >= NCY || (ui=cydinfo[cyunit]) == 0 || ui->ui_alive == 0)
		return ENXIO;
	cy = &cy_softc[cyunit];
	a = bdbtofsb(uio->uio_offset >> PGSHIFT);
	cy->cy_blkno = a;
	cy->cy_nxrec = a + 1;
	return 0;
}

/*
 *  Set a TAPEMASTER pointer (first parameter), into the
 *  4 bytes array pointed by the second parameter.
 */
set_pointer(pointer, dest)
int pointer;
char * dest;
{
	*dest++ = pointer & 0xff;		/* low byte - offset */
	*dest++ = (pointer >> 8) & 0xff;	/* high byte - offset */
	*dest++ = 0; 
	*dest   = (pointer & 0xf0000) >> 12;	/* base */
}

cydump(dev)
dev_t	dev;
{
	register struct vba_device *ui;
	register struct tpb *tp;
	int cyunit = CYUNIT(dev);
	int blk, num;
	int start;

	start = 0x800;
	num = maxfree;
	tp = &tpb[cyunit];
	if (cyunit >= NCY || (ui=cydinfo[cyunit]) == 0 || ui->ui_alive == 0) 
		return(ENXIO);
	if (cywait) return(EFAULT);
	while (num > 0) {
		blk = num > TBUFSIZ ? TBUFSIZ : num;
		bcopy(start*NBPG, cybuf, blk*NBPG);
		tp->cmd = WRIT_BU;	
		tp->control[0] = cyunit<<CW_TSs;
		tp->control[1] = (CW_100ips | CW_16bits);
		tp->status[0] = tp->status[1] = 0;
		tp->size = TM_SHORT(blk*NBPG);
		set_pointer((int)cybuf, (char *)tp->pt_data);
		set_pointer((int)&tpb[cyunit], (char *)ccb.pt_tpb);
		ccb.gate[0] = GATE_CLOSED;	
		TM_ATTENTION(cyaddr, 0xff);		/* execute! */
		start += blk;
		num -= blk;
		if (cywait) return(EFAULT);
		uncache(&tp->status[1]);
		if (tp->status[1]&CS_ERm)		/* error */
			return (EIO);
	}
	cyeof(tp, cyunit);
	if (cywait) return(EFAULT);
	cyeof(tp, cyunit);
	if (cywait) return(EFAULT);
	uncache(&tp->status[1]);
	if (tp->status[1]&CS_ERm)		/* error */
		return (EIO);
	cyrewind(tp, cyunit);
	return (0);
}

cywait()
{
	register cnt;

	cnt = 5000;		/* 5 seconds timeout */
	do {
		--cnt;
		DELAY(1000);
		uncache(&ccb.gate[0]);
	}
	while (cnt>0 && ccb.gate[0] == GATE_CLOSED);
	if (cnt == 0) return(1);	/* timeout */
	else return(0);
}

cyeof(tp, unit)
	register struct tpb *tp;
	int unit;
{
	tp->cmd = WRIT_FM;	
	tp->control[0] = unit<<CW_TSs;
	tp->control[1] = (CW_100ips | CW_16bits);
	tp->status[0] = tp->status[1] = 0;
	tp->rec_over = TM_SHORT(1);
	set_pointer((int)&tpb[unit], (char *)ccb.pt_tpb);
	ccb.gate[0] = GATE_CLOSED;	
	TM_ATTENTION(cyaddr, 0xff);		/* execute! */
}


cyrewind(tp, unit)
	register struct tpb *tp;
	int unit;
{
	tp->cmd = REWD_TA;	
	tp->control[0] = unit<<CW_TSs;
	tp->control[1] = (CW_100ips | CW_16bits);
	tp->status[0] = tp->status[1] = 0;
	set_pointer((int)&tpb[unit], (char *)ccb.pt_tpb);
	ccb.gate[0] = GATE_CLOSED;	
	TM_ATTENTION(cyaddr, 0xff);		/* execute! */
}

unsigned
tminphys(bp)
register struct buf *bp;
{

	if (bp->b_bcount > sizeof cybuf)
		bp->b_bcount = sizeof cybuf;
}
#endif
