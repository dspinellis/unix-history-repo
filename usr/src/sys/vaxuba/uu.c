/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uu.c	7.1 (Berkeley) 6/5/86
 */

#include "uu.h"
#if NUU > 0
/*
 * TU58 DECtape II/DL11 device driver
 *
 * The TU58 is treated as a block device (only).  Error detection and
 * recovery is not very extensive, but sufficient to handle the most
 * common errors. It is assumed that the TU58 will follow the RSP 
 * protocol exactly, very few protocol errors are checked for.  
 *
 * To reduce interrupt latency, `options UUDMA' should be specified 
 * in the config file to make sure the `pseudo-DMA' code in locore.s
 * will be compiled into the system. Otherwise overrun errors will 
 * occur frequently (these errors are not reported).
 *
 * TODO:
 *
 * - Add ioctl code to wind/rewind cassette
 *
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "time.h"
#include "kernel.h"
#include "errno.h"
#include "file.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "../vax/rsp.h"

#include "ubavar.h"
#include "ubareg.h"
#include "uureg.h"

#define	NTUBLK	512		/* number of blocks on a TU58 cassette */
#define	WRV     01              /* bit in minor dev => write w. read verify */
#define	NDPC	02		/* drives per controller */
#define	NUX	NDPC * NUU	/* number of drives */
#define	NUUQ	02		/* # of block which can be queued up */
#define	UMASK	01		/* unit number mask */
#define UUIPL	0x14		/* ipl level to use */

struct packet uucmd[NUU];	/* a command sent to the TU58 */
struct packet uudata[NUU];	/* a command or data returned from TU58 */
struct buf uitab[NUU];		/* buffer queue headers */

/*
 * Driver soft carrier structure
 */
struct uu_softc {
	u_char	*tu_rbptr;	/* pointer to buffer for read */
	int	tu_rcnt;	/* how much to read */
	u_char	*tu_wbptr;	/* pointer to buffer for write */
	int	tu_wcnt;	/* how much to write */
	int	tu_state;	/* current state of tansfer operation */
	int	tu_flag;	/* read in progress flag */
	char	*tu_addr;	/* real buffer data address */
	int	tu_count;	/* real requested count */
	int	tu_serrs;	/* count of soft errors */
	int	tu_cerrs;	/* count of checksum errors */
	int	tu_herrs;	/* count of hard errors */
	char    tu_dopen[2];	/* drive is open */
} uu_softc[NUU];

#if defined(VAX750) || defined(VAX730)
extern char *tustates[];
#else
char *tustates[TUS_NSTATES] = {
	"INIT1", "INIT2", "IDLE", "SENDH", "SENDD", "SENDC", "SENDR",
	"SENDW", "GETH", "GETD", "GETC", "GET", "WAIT", "RCVERR", "CHKERR"
};
#endif

#define	UNIT(dev)	(minor(dev)>>1)

u_char	uunull[2] = { 0, 0 };		/* nulls to send for initialization */
u_char	uuinit[2] = { TUF_INITF, TUF_INITF };	/* inits to send */

struct	uba_device	*uudinfo[NUU];

int uuprobe(), uuattach(), uurintr(), uuxintr(), uuwatch();
u_short uustd[] = { 0176500, 0 };
struct uba_driver uudriver =
    { uuprobe, 0, uuattach, 0, uustd, "uu", uudinfo };

int	uuwstart;
int	uuwake();
static char uu_pcnt[NUX];		/* pee/vee counters, one per drive */

/*ARGSUSED*/
uuprobe(reg)
	caddr_t reg;
{
	register int br, cvec;			/* value result */
	struct uudevice *uuaddr = (struct uudevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	uurintr(0); uuxintr(0);
#endif
	uuaddr->tcs = UUCS_INTR;
	DELAY(1000);
	uuaddr->tcs = 0;
	cvec -= 4;		/* since we are using the xmitter intrpt */
	return(sizeof (*uuaddr));
}

uuattach(ui)
	register struct uba_device *ui;
{
}

/*ARGSUSED1*/
uuopen(dev, flag)
	dev_t dev;
	int flag;	
{
	register struct uba_device *ui;
	register struct uu_softc *uuc;
	register struct uudevice *uuaddr;
	int ctlr, unit = UNIT(dev), s;

	ctlr = unit / NDPC;
	if (unit >= NUX || (ui = uudinfo[ctlr]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	uuc = &uu_softc[ctlr];
	if (uuc->tu_dopen[unit&UMASK])
		return (EBUSY);
	if (uuwstart++ == 0)
		timeout(uuwatch, (caddr_t)0, hz);

	uuc->tu_dopen[unit&UMASK]++;
	uuaddr = (struct uudevice *)ui->ui_addr;
	s = splx(UUIPL);
	/*
	 * If the other device on this controller
	 * is already active, no need to initialize
	 */
	if (uuc->tu_dopen[0] && uuc->tu_dopen[1])
		goto ok;

	/*
	 * If the unit already initialized,
	 * just enable interrupts and return.
	 */
	if (uuc->tu_state == TUS_IDLE) {
		uuaddr->rcs = UUCS_INTR;
		goto ok;
	}

	/* 
	 * Must initialize, reset the cassette
	 * and wait for things to settle down.
	 */
	uureset(ctlr);
	sleep((caddr_t)uuc, PZERO+1);
	uitab[ctlr].b_active = NULL;
	if (uuc->tu_state != TUS_IDLE) {
		uuc->tu_state = TUS_INIT1;
		uuc->tu_dopen[unit&UMASK] = 0;
		uuc->tu_rcnt = uuc->tu_wcnt = 0;
		uuaddr->rcs = 0;
		uuaddr->tcs = 0;
		splx(s);
		return (EIO);
	}
ok:
	splx(s);
	return (0);
}

/*
 * Wait for all outstanding IO on this drive
 * complete, before closing. If both drives on
 * this controller are idle, mark the controller
 * `inactive'.
 */

uuclose(dev, flag)
	dev_t dev;
	int flag;
{
	int s, unit = UNIT(dev);
	register struct uu_softc *uuc = &uu_softc[unit/NDPC];
	struct buf *bp, *last = NULL;
	struct uudevice *uuaddr = (struct uudevice *)uudinfo[unit/NDPC]->ui_addr;

	s = splx(UUIPL);
	while (uu_pcnt[unit])
		sleep(&uu_pcnt[unit], PRIBIO);
	/*
	 * No more writes are pending, scan the 
	 * buffer queue for oustanding reads from
	 * this unit.
	 */
	for (bp = uitab[unit/NDPC].b_actf; bp; bp = bp->b_actf) {
		if (bp->b_dev == dev)
			last = bp;
	}
	if (last) {
		last->b_flags |= B_CALL;
		last->b_iodone = uuwake;
		sleep((caddr_t)last, PRIBIO);
	}
	uuc->tu_dopen[unit&UMASK] = 0;
	if (!uuc->tu_dopen[0] && !uuc->tu_dopen[1]) {
		uuc->tu_flag = 0;
		uuaddr->rcs = 0;
	}
	splx(s);
}

uuwake(bp)
	struct buf *bp;
{
	wakeup(bp);
}

uureset(ctlr)
	int ctlr;
{
	register struct uu_softc *uuc = &uu_softc[ctlr];
	register struct packet *cmd = &uucmd[ctlr];
	struct uba_device *ui = uudinfo[ctlr];
	register struct uudevice *uuaddr = (struct uudevice *)ui->ui_addr;

	uitab[ctlr].b_active++;
	uuc->tu_state = TUS_INIT1;
	uuc->tu_wbptr = uunull;
	uuc->tu_wcnt = sizeof (uunull);
	uuc->tu_rcnt = 0;
	cmd->pk_flag = TUF_CMD;
	cmd->pk_mcount = sizeof (*cmd) - 4;
	cmd->pk_mod = 0;
	cmd->pk_seq = 0;
	cmd->pk_sw = 0;
	uuaddr->rcs = 0;
	uuaddr->tcs = UUCS_INTR | UUCS_BREAK;
	uuxintr(ctlr);				/* start output */
}

/*
 * Strategy routine for block I/O
 */
uustrategy(bp)
	register struct buf *bp;
{
	register struct buf *uutab;
	struct uba_device *ui;
	int s, unit = UNIT(bp->b_dev);

	if ((unit > NUX) || (bp->b_blkno >= NTUBLK))
		goto bad;
	ui = uudinfo[unit/NDPC];
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	uutab = &uitab[unit/NDPC];	/* one request queue per controller */
	s = splx(UUIPL);
	if ((bp->b_flags&B_READ) == 0)
		tu_pee(&uu_pcnt[unit]);
	bp->b_actf = NULL;
	if (uutab->b_actf == NULL)
		uutab->b_actf = bp;
	else
		uutab->b_actl->b_actf = bp;
	uutab->b_actl = bp;
	if (uutab->b_active == 0)
		uustart(ui);
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
	bp->b_error = ENXIO;
	iodone(bp);
	return;
}

/*
 * Start the transfer
 */
uustart(ui)
	register struct uba_device *ui;
{
	register struct buf *bp;
	register struct uu_softc *uuc;
	struct packet *cmd;
	int ctlr = ui->ui_unit, s;

	if ((bp = uitab[ctlr].b_actf) == NULL)
		return;
	s = splx(UUIPL);
	uuc = &uu_softc[ctlr];
	if (uuc->tu_state != TUS_IDLE) {
		uureset(ctlr);
		splx(s);
		return;
	}
	cmd = &uucmd[ctlr];
	uitab[ctlr].b_active++;
	uitab[ctlr].b_errcnt = 0;
	uuc->tu_addr = bp->b_un.b_addr;
	uuc->tu_count = cmd->pk_count = bp->b_bcount;
	cmd->pk_block = bp->b_blkno;
	if (bp->b_flags&B_READ) {
		cmd->pk_op = TUOP_READ;
		cmd->pk_mod = 0;
		uuc->tu_state = TUS_SENDR;
	} else {
		cmd->pk_op = TUOP_WRITE;
		cmd->pk_mod = minor(bp->b_dev)&WRV ? TUMD_WRV : 0;
		uuc->tu_state = TUS_SENDW;
	}
	cmd->pk_unit = UNIT(bp->b_dev)&UMASK;
	cmd->pk_sw = 0;
	cmd->pk_chksum =
	    tuchk(*((short *)cmd), (u_short *)&cmd->pk_op, (int)cmd->pk_mcount);
	uuc->tu_wbptr = (u_char *)cmd;
	uuc->tu_wcnt = sizeof (*cmd);
	uuxintr(ctlr);
	splx(s);
}

/*
 * TU58 receiver interrupt, handles whatever condition the
 * pseudo DMA routine in locore is unable to handle, 
 * or, if UUDMA is undefined, handle all receiver interrupt
 * processing.
 */
uurintr(ctlr)
	int ctlr;
{
	struct uba_device *ui = uudinfo[ctlr];
	register struct uu_softc *uuc = &uu_softc[ctlr];
	register struct uudevice *uuaddr = (struct uudevice *)ui->ui_addr;
	register struct buf *uutab = &uitab[ctlr];
	struct packet *data, *cmd;
	struct buf *bp;
	int c, unit;

	c = uuaddr->rdb;
	data = &uudata[ctlr];
	cmd = &uucmd[ctlr];
#if !defined(UUDMA)
	if (c & UURDB_ERROR)
		uuc->tu_state = TUS_RCVERR;
	else {
		if (uuc->tu_rcnt) {
			*uuc->tu_rbptr++ = c;
			if (--uuc->tu_rcnt)
				return;
		}
	}
#endif

	/*
	 * Switch on the tu_state of the transfer.
	 */
	switch(uuc->tu_state) {

	/*
	 * A data error occured in uudma
	 * (either overrun or break)
	 */
	case TUS_RCVERR:
		if ((c & UURDB_ORUN) == 0)
			printf("uu%d: break received, transfer restarted\n",
			    data->pk_unit);
#ifdef UUDEBUG
		else
			printf("uu%d: data overrun, recovered\n", 
			    data->pk_unit);
#endif
		uuc->tu_serrs++;
		uu_restart(ctlr, ui);	
		break;

	/*
	 * If we get an unexpected "continue",
	 * start all over again...
	 */
	case TUS_INIT2:
		uuc->tu_state = c == TUF_CONT ? TUS_IDLE : TUS_INIT1;
		uuc->tu_flag = 0;
		wakeup((caddr_t)uuc);
		uustart(ui);
		break;

	/*
	 * Only transition from this state
	 * is on a "continue", so if we don't
	 * get it, reset the world.
	 */
	case TUS_WAIT:			/* waiting for continue */
		switch(c) {
		case TUF_CONT:  /* got the expected continue */
			uuc->tu_flag = 0;
			data->pk_flag = TUF_DATA;
			data->pk_mcount = MIN(128, uuc->tu_count);
			data->pk_chksum =
			    tuchk(*((short *)data), (caddr_t)uuc->tu_addr,
				(int)data->pk_mcount);
			uuc->tu_state = TUS_SENDH;
			uuc->tu_wbptr = (u_char *)data;
			uuc->tu_wcnt = 2;
			uuxintr(ctlr);
			break;

		case TUF_CMD:   /* sending us an END packet...error */
			uuc->tu_state = TUS_GET;
			uuc->tu_rbptr = (u_char *)data;
			uuc->tu_rcnt = sizeof (*data) - 1;
			uuc->tu_flag = 1;
			uuaddr->tcs = 0;
			*uuc->tu_rbptr++ = c & UUDB_DMASK;
			break;

		case TUF_INITF:
			uureset(ctlr);
			break;

		default:        /* something random...bad news */
			uuc->tu_state = TUS_INIT1;
			break;
		}
		break;

	case TUS_SENDW:
		if (c != TUF_CONT && c != TUF_INITF)
			goto bad;
		uu_restart(ctlr, ui);
		break;

	/*
	 * Got header, now get data; amount to
	 * fetch is included in packet.
	 * (data packets are handled entirely
	 * in uudma)
	 */
	case TUS_GETH:
#ifndef UUDMA
		if (data->pk_flag == TUF_DATA)
			uuc->tu_rbptr = (u_char *)uuc->tu_addr;
#endif
		uuc->tu_rcnt = data->pk_mcount;
		uuc->tu_state = TUS_GETD;
		break;

	/*
	 * Got the data, now fetch the checksum.
	 */
	case TUS_GETD:
		uuc->tu_rbptr = (u_char *)&data->pk_chksum;
		uuc->tu_rcnt = sizeof (data->pk_chksum);
		uuc->tu_state = TUS_GETC;
		break;

	case TUS_GETC:
		/* got entire packet */
		if (data->pk_chksum !=
		    tuchk(*((short *)data), (u_short *)
		     (data->pk_flag == TUF_DATA ?
		     (u_short *) uuc->tu_addr : (u_short *)&data->pk_op),
		     (int)data->pk_mcount))
	case TUS_CHKERR:
			uuc->tu_cerrs++;
	case TUS_GET:
		if (data->pk_flag == TUF_DATA) {
			/* data packet, advance to next */
			uuc->tu_addr += data->pk_mcount;
			uuc->tu_count -= data->pk_mcount;
			uuc->tu_state = TUS_GETH;
			uuc->tu_rbptr = (u_char *)data;	/* next packet */
			uuc->tu_rcnt = 2;
		} else if (data->pk_flag==TUF_CMD && data->pk_op==TUOP_END) {
			/* end packet, idle and reenable transmitter */
			uuc->tu_state = TUS_IDLE;
			uuc->tu_flag = 0;
			uuaddr->tcs = UUCS_INTR;
			if ((bp = uutab->b_actf) == NULL) {
				printf("uu%d: no bp, active %d\n", 
				    data->pk_unit, uitab[ctlr].b_active);
				uustart(ui);
				return;
			}
			unit = UNIT(bp->b_dev);
			if (data->pk_mod > 1) {        /* hard error */
				printf("uu%d: hard error bn%d,", unit, 
				    bp->b_blkno);
				printf(" pk_mod 0%o\n", data->pk_mod&0xff);
				bp->b_flags |= B_ERROR;
				uuc->tu_herrs++;
			} else if (data->pk_mod)	/* soft error */
				uuc->tu_serrs++;
			uutab->b_active = NULL;
			uutab->b_actf = bp->b_actf;
			bp->b_resid = uuc->tu_count;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&uu_pcnt[unit]);
			iodone(bp);
			uustart(ui);
		} else {
			/*
			 * Neither data nor end: data was lost
			 * somehow, flush and restart the transfer.
			 */
			uuaddr->rcs = 0;
			uu_restart(ctlr, ui);
			uuc->tu_serrs++;
		}
		break;

	case TUS_IDLE:
	case TUS_INIT1:
		break;

	default:
bad:
		if (c == TUF_INITF) {
			printf("uu%d protocol error, state=", data->pk_unit);
			printstate(uuc->tu_state);
			printf(", op=%x, cnt=%d, block=%d\n",
			    cmd->pk_op, cmd->pk_count, cmd->pk_block);
			uutab->b_active = NULL;
			if (bp = uutab->b_actf) {
				bp->b_flags |= B_ERROR;
				uutab->b_actf = bp->b_actf;
				if ((bp->b_flags&B_READ) == 0)
					tu_vee(&uu_pcnt[unit]);
				iodone(bp);
			}
			uuc->tu_state = TUS_INIT1;
		} else {
			printf("uu%d receive state error, state=", 
				data->pk_unit);
			printstate(uuc->tu_state);
			printf(", byte=%x\n", c & 0xff);
#ifdef notdef
			uuc->tu_state = TUS_INIT1;
#endif
			wakeup((caddr_t)uuc);
		}
	}
}


/*
 * TU58 transmitter interrupt
 */
uuxintr(ctlr)
	int ctlr;
{
	register struct uu_softc *uuc = &uu_softc[ctlr];
	register struct uudevice *uuaddr;
	register struct packet *data;
	struct uba_device *ui = uudinfo[ctlr];
	int c;

	data = &uudata[ctlr];
	uuaddr = (struct uudevice *) ui->ui_addr;
top:
	if (uuc->tu_wcnt > 0) {
		/* still stuff to send, send one byte */
		while ((uuaddr->tcs & UUCS_READY) == 0)
			;
		uuaddr->tdb = *uuc->tu_wbptr++;
		uuc->tu_wcnt--;
		return;
	}

	/*
	 * Last message byte was sent out.
	 * Switch on tu_state of transfer.
	 */
	switch(uuc->tu_state) {

	/*
	 * Two nulls have been sent, remove break, and send inits
	 */
	case TUS_INIT1:	
		uuc->tu_flag = 0;
		uuaddr->tcs = UUCS_INTR;
		uuc->tu_state = TUS_INIT2;
		uuc->tu_wbptr = uuinit;
		uuc->tu_wcnt = sizeof (uuinit);
		goto top;

	/*
	 * Inits have been sent, wait for a continue msg.
	 */
	case TUS_INIT2:	
		c = uuaddr->rdb;	/* prevent overrun error */
		uuaddr->rcs = UUCS_INTR;
		uuc->tu_flag = 1;
		break;

	/*
	 * Read cmd packet sent, get ready for data
	 */
	case TUS_SENDR:
		uuc->tu_state = TUS_GETH;
		uuc->tu_rbptr = (u_char *)data;
		uuc->tu_rcnt = 2;
		uuc->tu_flag = 1;
		uuaddr->tcs = 0;
		uuaddr->rcs = UUCS_INTR;
		break;

	/*
	 * Write cmd packet sent, wait for continue
	 */
	case TUS_SENDW:	
		uuc->tu_state = TUS_WAIT;
		uuc->tu_flag = 1;
		if ((uuaddr->rcs&UUCS_INTR) == 0) {
			printf("NO IE\n");
			uuaddr->rcs = UUCS_INTR;
		}
		break;

	/*
	 * Header sent, send data.
	 */
	case TUS_SENDH:
		uuc->tu_state = TUS_SENDD;
		uuc->tu_wbptr = (u_char *)uuc->tu_addr;
		uuc->tu_wcnt = data->pk_mcount;
		goto top;

	/*
	 * Data sent, follow with checksum.
	 */
	case TUS_SENDD:	
		uuc->tu_state = TUS_SENDC;
		uuc->tu_wbptr = (u_char *)&data->pk_chksum;
		uuc->tu_wcnt = 2;
		goto top;

	/* 
	 * Checksum sent, wait for continue.
	 */
	case TUS_SENDC:
		/*
		 * Update buffer address and count.
		 */
		uuc->tu_addr += data->pk_mcount;
		uuc->tu_count -= data->pk_mcount;
		if (uuc->tu_count > 0) {
			uuc->tu_state = TUS_WAIT;
			uuc->tu_flag = 1;
			break;
		}

		/*
		 * End of transmission, get ready for end packet.
		 */
		uuc->tu_state = TUS_GET;
		uuc->tu_rbptr = (u_char *)data;
		uuc->tu_rcnt = sizeof (*data);
		uuc->tu_flag = 1;
		uuaddr->tcs = 0;
		break;

	/*
	 * Random interrupt
	 */
	case TUS_IDLE:		/* stray interrupt? */

	default:
		break;
	}
}

uuwatch()
{
	register struct uu_softc *uuc;
	register struct uudevice *uuaddr;
	struct uba_device *ui;
	struct buf *bp, *uutab;
	int s, ctlr, active = 0;

	for (ctlr=0; ctlr<NUU; ctlr++) {
		int i;

		uuc = &uu_softc[ctlr];

		if (uuc->tu_dopen[0] || uuc->tu_dopen[1])
			active++;
		if (uuc->tu_flag == 0)
			/*
			 * If no read is in progress
			 * just skip
			 */
			continue;

		ui = uudinfo[ctlr];
		uuaddr = (struct uudevice *)ui->ui_addr;
		uutab = &uitab[ctlr];
		if (uuc->tu_flag++ < 40)
			continue;
		printf("uu%d: read stalled\n", uudata[ctlr].pk_unit);
#ifdef UUDEBUG
		printf("%X %X %X %X %X %X %X\n", uuc->tu_rbptr, uuc->tu_rcnt,
		       uuc->tu_wbptr, uuc->tu_wcnt, uuc->tu_state, uuc->tu_addr,
		       uuc->tu_count);
#endif
		s = splx(UUIPL);
		uuc->tu_flag = 0;
		i = uuaddr->rdb;		/* dummy */
		uuaddr->rcs = UUCS_INTR;	/* in case we were flushing */
		uuaddr->tcs = UUCS_INTR;
		uuc->tu_state = TUS_IDLE;
		if (!uutab->b_active) {
			wakeup((caddr_t)uuc);
			goto retry;
		}
		if (++uutab->b_errcnt <= 1) {
			uustart(ui);
			goto retry;
		}
		if (bp = uutab->b_actf) {
			bp->b_flags |= B_ERROR;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&uu_pcnt[UNIT(bp->b_dev)]);
			iodone(bp);
		}
retry:
		(void) splx(s);
	}
	if (active)
		timeout(uuwatch, (caddr_t)0, hz);
	else
		uuwstart = 0;
	return;
}

#if !defined(VAX750) && !defined(VAX730)
/*
 * Compute checksum TU58 fashion
 */
#ifdef lint
tuchk(word, cp, n)
	register word;
	register unsigned short *cp;
	int n;
{
	register int c = n >> 1;
	register long temp;

	do {
		temp = *cp++;	/* temp, only because vax cc won't *r++ */
		word += temp;
	} while (--c > 0);
	if (n & 1)
		word += *(unsigned char *)cp;
	while (word & 0xffff0000)
		word = (word & 0xffff) + ((word >> 16) & 0xffff);
	return (word);
}
#else
tuchk(word0, wp, n)
	register int word0;			/* r11 */
	register char *wp;			/* r10 */
	register int n;				/* r9 */
{
	asm("loop:");
	asm("	addw2	(r10)+,r11");		/* add a word to sum */
	asm("	adwc	$0,r11");		/* add in carry, end-around */
	asm("	acbl	$2,$-2,r9,loop");	/* done yet? */
	asm("	blbc	r9,ok");		/* odd byte count? */
	asm("	movzbw	(r10),r10");		/* yes, get last byte */
	asm("	addw2	r10,r11");		/* add it in */
	asm("	adwc	$0,r11");		/* and the carry */
	asm("ok:");
	asm("	movl	r11,r0");		/* return sum */
}
#endif

/*
 * Make sure this incredibly slow device
 * doesn't eat up all the buffers in the
 * system by putting the requesting process
 * (remember: this device is 'single-user')
 * to sleep if the write-behind queue grows
 * larger than NUUQ.
 */
tu_pee(cp)
	char *cp;
{
	register int s;

	s = splx(UUIPL);
	if (++(*cp) > NUUQ) 
		sleep(cp, PRIBIO);
	splx(s);
}

tu_vee(cp)
	char *cp;
{
	register int s;

	s = splx(UUIPL);
	if (--(*cp) <= NUUQ) 
		wakeup(cp);
	splx(s);
}
#endif

uuioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	/*
	 * add code to wind/rewind cassette here
	 */
	return (ENXIO);
}

uu_restart(ctlr, ui)
	int ctlr;
	struct uba_device *ui;
{
	uureset(ctlr);
	timeout(uustart, (caddr_t)ui, hz * 3);
}

#endif
