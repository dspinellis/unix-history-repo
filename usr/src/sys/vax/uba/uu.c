/*	uu.c	4.5	83/06/08	*/

#include "uu.h"
#if NUU > 0
/*
 * TU58 DECtape II/DL11 device driver
 *
 * The TU58 * is treated as a block device (only).  Error detection and
 * recovery is almost non-existant.  It is assumed that the
 * TU58 will follow the RSP protocol exactly, very few protocol
 * errors are checked for.  
 */

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/errno.h"
#include "../h/uio.h"
#include "../h/file.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "../vax/rsp.h"

#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/uureg.h"

#define	NTUBLK	512		/* number of blocks on a TU58 cassette */
#define	WRV     01              /* bit in minor dev => write w. read verify */
#define	NDPC	02		/* drives per controller */
#define	NUX	NDPC * NUU	/* number of drives */
#define	NTUQ	02		/* # of block which can be queued up */
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
};

struct uu_softc uu_softc[NUU];

#if defined(VAX750) || defined(VAX730)
extern char *tustates[];
#else
char *tustates[TUS_NSTATES] = {
	"INIT1", "INIT2", "IDLE", "SENDH", "SENDD", "SENDC", "SENDR",
	"SENDW", "GETH", "GETD", "GETC", "GET", "WAIT"
};
#endif

#define	UNIT(dev)	(minor(dev)>>1)

u_char	uunull[2] = { 0, 0 };	/* nulls to send for initialization */
u_char	uuinit[2] = { TUF_INITF, TUF_INITF };	/* inits to send */

struct	uba_device	*uudinfo[NUU];

int uuprobe(), uuattach(), uurintr(), uuxintr(), uuwatch();
u_short uustd[] = { 0176500 };
struct uba_driver uudriver =
    { uuprobe, 0, uuattach, 0, uustd, "uu", uudinfo };

int	uuwstart;
static char pcnt[NUX];			/* pee/vee counters, one per drive */

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
	register struct tu *uuc;
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

uuclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tu *uuc;
	int unit = UNIT(dev);
	int ctlr = unit/NDPC;

	uuc = &uu_softc[ctlr];
	if (uuc->tu_serrs + uuc->tu_cerrs + uuc->tu_herrs != 0) {
		/*
		 * A tu58 is like nothing ever seen before;
		 * I guess this is appropriate then...
		 */
		uprintf(
		   "uu%d: %d soft errors, %d checksum errors, %d hard errors\n",
		    unit, uuc->tu_serrs, uuc->tu_cerrs, uuc->tu_herrs);
		    uuc->tu_serrs = uuc->tu_cerrs = uuc->tu_herrs = 0;
	}
	uuc->tu_dopen[unit&UMASK] = 0;
}

uureset(ctlr)
	int ctlr;
{
	register struct tu *uuc = &uu_softc[ctlr];
	register struct packet *cmd = &uucmd[ctlr];
	struct uba_device *ui = uudinfo[ctlr];
	register struct uudevice *uuaddr = (struct uudevice *)ui->ui_addr;

	printf ("uureset\n");
	uitab[ctlr].b_active++;
	uuc->tu_state = TUS_INIT1;
	uuc->tu_wbptr = uunull;
	uuc->tu_wcnt = sizeof (uunull);
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
	int s, unit = UNIT(minor(bp->b_dev));

	if (unit > NUX)
		goto bad;
	if (bp->b_blkno >= NTUBLK)
		goto bad;
	ui = uudinfo[unit/NDPC];
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	uutab = &uitab[unit/NDPC];	/* one request queue per controller */
	if ((bp->b_flags&B_READ) == 0)
		tu_pee(&pcnt[unit]);
	s = splx(UUIPL);
	bp->av_forw = NULL;
	if (uutab->b_actf == NULL)
		uutab->b_actf = bp;
	else
		uutab->b_actl->av_forw = bp;
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
	register struct tu *uuc;
	struct packet *cmd;
	int ctlr = ui->ui_unit;

	if ((bp = uitab[ctlr].b_actf) == NULL)
		return;
	uuc = &uu_softc[ctlr];
	cmd = &uucmd[ctlr];
	if (uuc->tu_state != TUS_IDLE) {
		uureset(ctlr);
		return;
	}
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
	cmd->pk_unit = UNIT(minor(bp->b_dev));
	cmd->pk_sw = 0;
	cmd->pk_chksum =
	    tuchk(*((short *)cmd), (u_short *)&cmd->pk_op, (int)cmd->pk_mcount);
	uuc->tu_wbptr = (u_char *)cmd;
	uuc->tu_wcnt = sizeof (*cmd);
	uuxintr(ctlr);
}

/*
 * TU58 receiver interrupt
 */
uurintr(ctlr)
	int ctlr;
{
	struct uba_device *ui = uudinfo[ctlr];
	register struct tu *uuc = &uu_softc[ctlr];
	register struct uudevice *uuaddr = (struct uudevice *)ui->ui_addr;
	register struct buf *uutab = &uitab[ctlr];
	struct packet *data, *cmd;
	struct buf *bp;
	int c, unit;

	c = uuaddr->rdb;
	data = &uudata[ctlr];
	if (c & UURDB_ERROR) {
		if (c & UURDB_ORUN) 
			printf("uu(%d): data overrun, bytes left: %d",
			  ui->ui_unit, 
			  uuc->tu_count + uuc->tu_rcnt - data->pk_mcount);
		else
			printf("uu(%d): break received", ui->ui_unit);
		printf(", device reset, state="); 
		printstate(uuc->tu_state);
		uureset(ctlr);
		printf("\n");
		timeout(uustart, (caddr_t)ui, hz/2);	/* start uustart when */
							/* reset is done */
		return;
	}
top:
	c &= UUDB_DMASK;
	if (uuc->tu_rcnt) {		/* still waiting for data? */
		*uuc->tu_rbptr++ = c;	/* yup, put it there */
		if (--uuc->tu_rcnt)	/* decrement count, any left? */
			return;		/* get some more */
	}
	cmd = &uucmd[ctlr];

	/*
	 * We got all the data we were expecting for now,
	 * switch on the tu_state of the transfer.
	 */
	switch(uuc->tu_state) {

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
			uuc->tu_rcnt = sizeof (*data);
			uuc->tu_flag = 1;
			uuaddr->tcs = 0;
			goto top;

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
		uureset(ctlr);
		break;

	/*
	 * Got header, now get data; amount to
	 * fetch is included in packet.
	 */
	case TUS_GETH:
		if (data->pk_flag == TUF_DATA) {
			uu_getblk((u_char *)uuc->tu_addr, data, uuaddr);
			goto getc;
		} 
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

	case TUS_GET:
	case TUS_GETC:
getc:
		/* got entire packet */
#ifdef notdef
		if (data->pk_chksum !=
		    tuchk(*((short *)data), (u_short *)
		     (data->pk_flag == TUF_DATA ? uuc->tu_addr : &data->pk_op),
		     (int)data->pk_mcount))
			uuc->tu_cerrs++;
#endif
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
				printf("uu(%d): no bp, active %d\n", 
					ui->ui_unit, uitab[ctlr].b_active);
				uustart(ui);
				return;
			}
			unit = UNIT(minor(bp->b_dev));
			if (data->pk_mod > 1) {        /* hard error */
				bp->b_flags |= B_ERROR;
				uuc->tu_herrs++;
				harderr(bp, "uu");
				printf(" pk_mod %o\n", data->pk_mod&0xff);
			} else if (data->pk_mod != 0)	/* soft error */
				uuc->tu_serrs++;
			uutab->b_active = NULL;
			uutab->b_actf = bp->av_forw;
			bp->b_resid = uuc->tu_count;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&pcnt[unit]);
			iodone(bp);
			printf(".");
			uustart(ui);
		} else {
			printf("neither data nor end: %o %o\n",
			    data->pk_flag&0xff, data->pk_op&0xff);
			uuaddr->rcs = 0;		/* flush the rest */
			uuc->tu_state = TUS_INIT1;
		}
		break;

	case TUS_IDLE:
	case TUS_INIT1:
		break;

	default:
bad:
		if (c == TUF_INITF) {
			printf("uu%d protocol error, state=", unit);
			printstate(uuc->tu_state);
			printf(", op=%x, cnt=%d, block=%d\n",
			    cmd->pk_op, cmd->pk_count, cmd->pk_block);
			uutab->b_active = NULL;
			if (bp = uutab->b_actf) {
				bp->b_flags |= B_ERROR;
				uutab->b_actf = bp->av_forw;
				if ((bp->b_flags&B_READ) == 0)
					tu_vee(&pcnt[unit]);
				iodone(bp);
			}
			uuc->tu_state = TUS_INIT1;
		} else {
			printf("uu%d receive state error, state=", 
				unit);
			printstate(uuc->tu_state);
			printf(", byte=%x\n", c);
#ifdef notdef
			uuc->tu_state = TUS_INIT1;
#endif
			wakeup((caddr_t)uuc);
		}
	}
}

/*
 * Simulate DMA input from the TU58,
 * usually 128 bytes plus the 2 byte checksum
 * will be read, leaving the CPU `dead' for 
 * approximately 0.135 seconds @ 9600 baud
 */
uu_getblk(buffer, data, uuaddr)
	register u_char *buffer;
	register struct uudevice *uuaddr;
	struct packet *data;
{
	int s;
	int count = (unsigned) data->pk_mcount + 2;

	s = spl5();		/* make sure we don't get interrupted by */
				/* disk i/o */
	uuaddr->rcs = 0;	/* disable interrupts temporarily */
	while (count--) {
		while ((uuaddr->rcs & UUCS_READY) == 0)
			;	
		*buffer++ = uuaddr->rdb & UUDB_DMASK;
		if (count == 2)
			buffer = (u_char *)&data->pk_chksum;
	}
	uuaddr->rcs = UUCS_INTR;
	(void) splx(s);
}

/*
 * TU58 transmitter interrupt
 */
uuxintr(ctlr)
	int ctlr;
{
	register struct tu *uuc = &uu_softc[ctlr];
	register struct uudevice *uuaddr;
	register struct packet *data;
	struct uba_device *ui = uudinfo[ctlr];
	int c;

	data = &uudata[ctlr];
	uuaddr = (struct uudevice *) ui->ui_addr;
top:
	if (uuc->tu_wcnt) {
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

	case TUS_IDLE:		/* stray interrupt? */
		break;

	/*
	 * Read cmd packet sent, get ready for data
	 */
	case TUS_SENDR:
		uuc->tu_state = TUS_GETH;
		uuc->tu_rbptr = (u_char *)data;
		uuc->tu_rcnt = 2;
		uuc->tu_flag = 1;
		uuaddr->tcs = 0;	/* disable transmitter interrupts */
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
		uuc->tu_wcnt = sizeof (data->pk_chksum);
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
		if (uuc->tu_count) {
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
		uuaddr->tcs = 0;		/* disable transm. interrupts */
		break;

	/*
	 * Random interrupt
	 */
	default:
		break;
	}
}

uuwatch()
{
	register struct tu *uuc;
	register struct uudevice *uuaddr;
	struct uba_device *ui;
	struct buf *bp, *uutab;
	int s, ctlr, active = 0;

	for (ctlr=0; ctlr<NUU; ctlr++) {
		int i;

		uuc = &uu_softc[ctlr];
		ui = uudinfo[ctlr];
		uuaddr = (struct uudevice *)ui->ui_addr;
		uutab = &uitab[ctlr];
		if ((uuc->tu_dopen[0] == 0) && (uuc->tu_dopen[1] == 0) && 
		    (uutab->b_active == 0)) {
			uuc->tu_flag = 0;
			uuaddr->rcs = 0;
			continue;
		}
		active++;
		if (uuc->tu_flag)
			uuc->tu_flag++;
		if (uuc->tu_flag <= 40)
			continue;
		printf("uu(%d): read stalled\n", ctlr);
		printf("%X %X %X %X %X %X %X\n", uuc->tu_rbptr, uuc->tu_rcnt,
		       uuc->tu_wbptr, uuc->tu_wcnt, uuc->tu_state, uuc->tu_addr, uuc->tu_count);
		uuc->tu_flag = 0;
		s = splx(UUIPL);
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
				tu_vee(&pcnt[UNIT(minor(bp->b_dev))]);
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

tu_pee(cp)
char *cp;
{
	register int s;

	s = splx(UUIPL);
	if (++(*cp) > NTUQ) {
		sleep(cp, PRIBIO);
	}
	splx(s);
}

tu_vee(cp)
char *cp;
{
	register int s;

	s = splx(UUIPL);
	if (--(*cp) <= NTUQ) {
		wakeup(cp);
	}
	splx(s);
}
#endif

uuioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	/*
	 * to be added later
	 */
	return (ENXIO);
}

#endif

