/*	uu.c	4.2	83/04/10	*/

#include "uu->h"
#if NDL > 0
/*
 * TU58 DECtape II/DL11 device driver
 *
 * The TU58 * is treated as a block device (only).  Error detection and
 * recovery is almost non-existant.  It is assumed that the
 * TU58 will follow the RSP protocol exactly, very few protocol
 * errors are checked for.  
 */

/* 
 * TODO:
 * -	Split the uu structure into a per controller 
 * 	part and a per drive part.
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/kernel.h"
#include "../h/errno.h"
#include "../h/uio.h"
#include "../h/user.h"
#include "../h/file.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/uureg.h"

#define	printd	if(uudebug) printf
#ifdef	printd
int	uudebug;	/* printd */
#endif	printd

#if	!defined(MRSP) || lint
#define	MRSP	(cpu != VAX_750)
#endif
#define	NTUBLK	512		/* number of blocks on a TU58 cassette */
#define	WRV     02              /* bit in minor dev => write w. read verify */
#define	NDPC	02		/* # drives per controller */

/*
 * Structure of a command packet
 */
struct packet {
	u_char	pk_flag;	/* indicates packet type (cmd, data, etc.) */
	u_char	pk_mcount;	/* length of packet (bytes) */
	u_char	pk_op;		/* operation to perform (read, write, etc.) */
	u_char	pk_mod;		/* modifier for op or returned status */
	u_char	pk_unit;	/* unit number */
	u_char	pk_sw;		/* switches */
	u_short	pk_seq;		/* sequence number, always zero */
	u_short	pk_count;	/* requested byte count for read or write */
	u_short	pk_block;	/* block number for read, write, or seek */
	u_short	pk_chksum;	/* checksum, by words with end around carry */
};

struct packet uucmd[NDL];	/* a command sent to the TU58 */
struct packet uudata[NDL];	/* a command or data returned from TU58 */

/*
 * per controller state information
 */
struct uu_ctlr {
	u_char	*uu_rbptr;	/* pointer to buffer for read */
	int	uu_rcnt;	/* how much to read */
	u_char	*uu_wbptr;	/* pointer to buffer for write */
	int	uu_wcnt;	/* how much to write */
	int	uu_state;	/* current uu_state of tansfer operation */
	int	uu_flag;	/* read in progress flag */
	char	*uu_addr;	/* real buffer data address */
	int	uu_count;	/* real requested count */
	int	uu_serrs;	/* count of soft errors */
	int	uu_cerrs;	/* count of checksum errors */
	int	uu_herrs;	/* count of hard errors */
	char    uu_dopen[NDPC];	/* drive is open */
} uu_ctlr[NDL];

/*
 * controller states
 */
#define	UUS_INIT1	0	/* sending nulls */
#define	UUS_INIT2	1	/* sending inits */
#define	UUS_IDLE	2	/* initialized, no transfer in progress */
#define	UUS_SENDH	3	/* sending header */
#define	UUS_SENDD	4	/* sending data */
#define	UUS_SENDC	5	/* sending checksum */
#define	UUS_SENDR	6	/* sending read command packet */
#define	UUS_SENDW	7	/* sending write command packet */
#define	UUS_GETH	8	/* reading header */
#define	UUS_GETD	9	/* reading data */
#define	UUS_GETC	10	/* reading checksum */
#define	UUS_GET		11	/* reading an entire packet */
#define	UUS_WAIT	12	/* waiting for continue */

#define	UUS_NSTATES	13
char *uustates[UUS_NSTATES] = {
	"INIT1", "INIT2", "IDLE", "SENDH", "SENDD", "SENDC", "SENDR",
	"SENDW", "GETH", "GETD", "GETC", "GET", "WAIT"
};
#define	printstate(state) \
	if ((state) < UUS_NSTATES) \
		printf("%s", uustates[(state)]); \
	else \
		printf("%d", (state));

/*
 * Packet Flags
 */
#define	TUF_DATA	1		/* data packet */
#define	TUF_CMD		2		/* command packet */
#define	TUF_INITF	4		/* initialize */
#define	TUF_CONT	020		/* continue */
#define	TUF_XOFF	023		/* flow control */

/*
 * Op Codes
 */
#define	TUOP_NOOP	0		/* no operation */
#define	TUOP_INIT	1		/* initialize */
#define	TUOP_READ	2		/* read block */
#define	TUOP_WRITE	3		/* write block */
#define	TUOP_SEEK	5		/* seek to block */
#define	TUOP_DIAGNOSE	7		/* run micro-diagnostics */
#define	TUOP_END	0100		/* end packet */

/*
 * Mod Flags
 */
#define TUMD_WRV        1               /* write with read verify */

/*
 * Switches
 */
#define	TUSW_MRSP	010		/* use Modified RSP */

u_char	uunull[2] = { 0, 0 };	/* nulls to send for initialization */
u_char	uuinit[2] = { TUF_INITF, TUF_INITF };	/* inits to send */

struct	uba_device	*uudinfo[NUU];
struct	uba_ctlr	*uuminfo[NDL];

int uuprobe(), uuslave(), uuattach(), uurintr(), uuxintr(), uuwatch();
u_short uustd[] = { 0176500, 0 };
struct uba_driver dldriver =
    { uuprobe, uuslave, uuattach, uudgo, uustd, "uu", uudinfo, "dl", uuminfo };

int	uuwstart;

/*ARGSUSED*/
uuprobe(reg)
	caddr_t reg;
{
	register int br, cvec;			/* value result */
	struct uudevice *uuaddr = (struct uudevice *)reg;
	int i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	uurintr(0); uuxintr(0);
#endif
	uuaddr->rcs = UUCS_INTR;
	uuaddr->tdb = TUF_INITF;
	DELAY(10000);
	i = uuaddr->rdb;
	uuaddr->rcs = 0;
	return(sizeof (*uuaddr));
}

uuslave(ui, reg);
	struct uba_device *ui;
	caddr_t reg;
{
	return (ui->ui_slave == 0 || ui->ui_slave == 1);
}

/*ARGSUSED*/
uuattach(ui)
	struct uba_device *ui;
{

}

/*ARGSUSED1*/
uuopen(dev, flag)
	dev_t dev;
	int flag;	
{
	register struct uba_device *ui;
	register struct uu_ctlr *uuc;
	register struct uudevice *uuaddr;
	register struct uba_ctlr *um;
	int ctlr, unit = minor(dev), s;

	if (unit >= NUU || (ui = uudinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	um = ui->ui_mi;
	ctlr = um->um_ctlr;
	uuc = &uu_ctlr[ctlr];
	if (uuc->uu_dopen[unit%NDPC])
		return (EBUSY);
	if (uuwstart++ == 0)
		timeout(uuwatch, (caddr_t)0, hz);

	uuc->uu_dopen[unit%NDPC]++;
	uuaddr = (struct uudevice *)ui->ui_mi->um_addr;
	s = spl5();
	/*
	 * If the unit already initialized,
	 * just enable interrupts and return.
	 */
	if (uu->uu_state == TUS_IDLE) {
		uuaddr->rcs = UUCS_INTR;
		splx(s);
		return (0);
	}

	/* 
	 * Must initialize, reset the cassette
	 * and wait for things to settle down.
	 */
	uureset(ctlr);
	sleep((caddr_t)uuc, PZERO+1);
	um->um_tab.b_active = NULL;
	if (uu->uu_state != TUS_IDLE) {
		uu->uu_state = TUS_INIT1;
		uu->uu_dopen[unit%NDPC] = 0;
		uu->uu_rcnt = uu->uu_wcnt = 0;
		uuaddr->rcs = 0;
		uuaddr->tcs = 0;
		splx(s);
		return (ENXIO);
	}
	splx(s);
	return (0);
}

uuclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct uba_ctlr *um = uudinfo[minor(dev)]->ui_mi;
	register struct uudevice *uuaddr;
	register struct uu_ctlr *uuc;

	if (um->um_tab.b_active == 0) {
		uuaddr = (struct uudevice *)um->um_addr;
		uuaddr->rcs = 0;
		uuwstart--;
	}
	uuc = &uu_ctlr[um->um_ctlr];
	if (uuc->uu_serrs + uuc->uu_cerrs + uuc->uu_herrs != 0) {
		/*
		 * A tu58 is like nothing ever seen before;
		 * I guess this is appropriate then...
		 */
		uprintf(
		   "uu%d: %d soft errors, %d chksum errors, %d hard errors\n",
		    minor(dev), uuc->uu_serrs, uuc->uu_cerrs, uuc->uu_herrs);
		    uuc->uu_serrs = uuc->uu_cerrs = uuc->uu_herrs = 0;
	}
	uuc->uu_dopen[minor(dev)%NDPC] = 0;
}

uureset(ctlr)
	int ctlr;
{
	register struct uu_ctlr *uuc = uu_ctlr[ctlr];
	register struct packet *cmd = uucmd[ctlr];
	register struct uudevice *uuaddr;
	struct uba_ctlr *um = uuminfo[ctlr];

	um->um_tab.b_active++;
	uuc->uu_state = TUS_INIT1;
	uuc->uu_wbptr = uunull;
	uuc->uu_wcnt = sizeof (uunull);
	cmd->pk_flag = TUF_CMD;
	cmd->pk_mcount = sizeof (uucmd) - 4;
	cmd->pk_mod = 0;
	cmd->pk_seq = 0;
	cmd->pk_sw = MRSP ? TUSW_MRSP : 0;
	uuaddr = (struct uudevice *)um->um_addr;
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
	register struct buf *dp;
	struct uba_device *ui;
	struct uu_ctlr *uuc;
	int s, unit = minor(bp->b_dev);

	if (unit > NUU)
		goto bad;
	if (bp->b_blkno >= NTUBLK)
		goto bad;
	ui = uudinfo[unit];
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	uuc = &uu_ctlr[ui->ui_mi->um_ctlr];
	s = spl5();
	bp->b_cylin = bp->b_blkno;
	dp = &uuutab[unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		uuustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			uustart(ui->ui_mi);
	}
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

/*
 * Unit start routine.
 * Put this unit on the ready queue for the controller
 */
uuustart(ui)
	register struct uba_device *ui;
{
	struct buf *dp = &uuutab[ui->ui_unit];
	struct uba_ctlr *um = ui->ui_mi;
	
	dp->b_forw = NULL;
	if (um->um_tab.b_actf == NULL)
		um->um_tab.b_actf = dp;
	else
		um->um_tab.b_actl->b_forw = dp;
	um->um_tab.b_actl = dp;
	dp->b_active++;
}
/*
 * Start the transfer
 */
uustart(um)
	register struct uba_ctlr *um;
{
	register struct uudevice *uuaddr;
	register struct buf *bp;
	register struct uu_ctlr *uuc;
	struct buf *dp;
	struct packet *cmd;
	int unit;

loop:
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	unit = minor(bp->b_dev);
	uuc = &uu_ctlr[um->um_ctlr];
	cmd = &uucmd[um->um_ctlr];
	if (uuc->uu_state != TUS_IDLE) {
		uureset(um->um_ctlr);
		return;
	}
	um->um_tab.b_active++;
	uuaddr = (struct uudevice *)um->um_addr;
	cmd->pk_op = bp->b_flags&B_READ ? TUOP_READ : TUOP_WRITE;
	cmd->pk_mod = ((bp->b_flags&B_READ) == 0 && (minor(bp->b_dev)&WRV)) ?
	    TUMD_WRV : 0;
	cmd->pk_unit = (minor(bp->b_dev)&DNUM);
	cmd->pk_sw = MRSP ? TUSW_MRSP : 0;
	cmd->pk_count = uu->uu_count = bp->b_bcount;
	cmd->pk_block = bp->b_blkno;
	cmd->pk_chksum =
	    uuchk(*((short *)&cmd), (u_short *)&cmd.pk_op,
		(int)cmd.pk_mcount);
	uuc->uu_state = bp->b_flags&B_READ ? TUS_SENDR : TUS_SENDW;
	uuc->uu_addr = bp->b_un.b_addr;
	uuc->uu_count = bp->b_bcount;
	uuc->uu_wbptr = (u_char *)&uucmd;
	uuc->uu_wcnt = sizeof (uucmd);
	uuxintr(um->um_ctlr);
}

/*
 * TU58 receiver interrupt
 */
uurintr()
{
	register struct buf *bp;
	register int c;

	c = mfpr(CSRD)&0xff;		/* get the char, clear the interrupt */
	if (MRSP) {
		while ((mfpr(CSTS)&READY) == 0)
			;
		mtpr(CSTD, TUF_CONT);	/* ACK */
	}
	if (uu->uu_rcnt) {		/* still waiting for data? */
		*uu->uu_rbptr++ = c;	/* yup, put it there */
		if (--uu->uu_rcnt)	/* decrement count, any left? */
			return;		/* get some more */
	}

	/*
	 * We got all the data we were expecting for now,
	 * switch on the uu_state of the transfer.
	 */
	switch(uu->uu_state) {

	/*
	 * If we get an unexpected "continue",
	 * start all over again...
	 */
	case TUS_INIT2:
		uu->uu_state = c == TUF_CONT ? TUS_IDLE : TUS_INIT1;
		uu->uu_flag = 0;
		wakeup((caddr_t)&uu);
		uustart();
		break;

	/*
	 * Only transition from this state
	 * is on a "continue", so if we don't
	 * get it, reset the world.
	 */
	case TUS_WAIT:			/* waiting for continue */
		if (c != TUF_CONT) {
			uu->uu_state = TUS_INIT1;
			break;
		}
		uu->uu_flag = 0;
		uudata.pk_flag = TUF_DATA;
		uudata.pk_mcount = MIN(128, uu->uu_count);
		uudata.pk_chksum =
		    uuchk(*((short *)&uudata), (u_short *)uu->uu_addr,
			(int)uudata.pk_mcount);
		uu->uu_state = TUS_SENDH;
		uu->uu_wbptr = (u_char *)&uudata;
		uu->uu_wcnt = 2;
		uuxintr();
		break;

	case TUS_SENDW:
		if (c != TUF_CONT)
			goto bad;
		uureset();
		break;

	/*
	 * Got header, now get data; amount to
	 * fetch is included in packet.
	 */
	case TUS_GETH:
		if (uudata.pk_flag == TUF_DATA)
			uu->uu_rbptr = (u_char *)uu->uu_addr;
		uu->uu_rcnt = uudata.pk_mcount;
		uu->uu_state = TUS_GETD;
		break;

	/*
	 * Got the data, now fetch the checksum.
	 */
	case TUS_GETD:
		uu->uu_rbptr = (u_char *)&uudata.pk_chksum;
		uu->uu_rcnt = sizeof (uudata.pk_chksum);
		uu->uu_state = TUS_GETC;
		break;

	case TUS_GET:
	case TUS_GETC:
		/* got entire packet */
#ifdef notdef
		if (uudata.pk_chksum !=
		    uuchk(*((short *)&uudata), (u_short *)
		     (uudata.pk_flag == TUF_DATA ? uu->uu_addr : &uudata.pk_op),
		     (int)uudata.pk_mcount))
			uu->uu_cerrs++;
#endif
		if (uudata.pk_flag == TUF_DATA) {
			/* data packet, advance to next */
			uu->uu_addr += uudata.pk_mcount;
			uu->uu_count -= uudata.pk_mcount;
			uu->uu_state = TUS_GETH;
			uu->uu_rbptr = (u_char *)&uudata; /* next packet */
			uu->uu_rcnt = 2;
		} else if (uudata.pk_flag==TUF_CMD && uudata.pk_op==TUOP_END) {
			/* end packet, idle and reenable transmitter */
			uu->uu_state = TUS_IDLE;
			uu->uu_flag = 0;
			mtpr(CSTS, IE);
			printd("ON ");
			if ((bp = uutab.b_actf) == NULL) {
				printf("uu: no bp, active %d\n",uutab.b_active);
				uustart();
				return;
			}
			if (uudata.pk_mod > 1) {        /* hard error */
				bp->b_flags |= B_ERROR;
				uu->uu_herrs++;
				harderr(bp, "uu");
				printf("  pk_mod %o\n", uudata.pk_mod&0377);
			} else if (uudata.pk_mod != 0)	/* soft error */
				uu->uu_serrs++;
			uutab.b_active = NULL;
			uutab.b_actf = bp->av_forw;
			bp->b_resid = uu->uu_count;
			if ((bp->b_flags&B_READ) == 0)
				uu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
			iodone(bp);
			uustart();
		} else {
			printf("neither data nor end: %o %o\n",
			    uudata.pk_flag&0xff, uudata.pk_op&0xff);
			mtpr(CSRS, 0);		/* flush the rest */
			uu->uu_state = TUS_INIT1;
		}
		break;

	case TUS_IDLE:
	case TUS_INIT1:
		break;

	default:
bad:
		if (c == TUF_INITF) {
			printf("uu protocol error, state=");
			printstate(uu->uu_state);
			printf(", op=%x, cnt=%d, block=%d\n",
			    uucmd.pk_op, uucmd.pk_count, uucmd.pk_block);
			uutab.b_active = NULL;
			if (bp = uutab.b_actf) {
				bp->b_flags |= B_ERROR;
				uutab.b_actf = bp->av_forw;
				if ((bp->b_flags&B_READ) == 0)
					uu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
				iodone(bp);
			}
			uu->uu_state = TUS_INIT1;
		} else {
			printf("uu receive state error, state=");
			printf(", byte=%x\n", c);
#ifdef notdef
			uu->uu_state = TUS_INIT1; */
#endif
			wakeup((caddr_t)&uu);
		}
	}
}

/*
 * TU58 transmitter interrupt
 */
uuxintr()
{

top:
	if (uu->uu_wcnt) {
		/* still stuff to send, send one byte */
		while ((mfpr(CSTS) & READY) == 0)
			;
		mtpr(CSTD, *uu->uu_wbptr++);
		uu->uu_wcnt--;
		return;
	}

	/*
	 * Last message byte was sent out.
	 * Switch on uu_state of transfer.
	 */
	if (uudebug) {
		printf("uuxintr: state=");
		printstate(uu->uu_state);
	}
	switch(uu->uu_state) {

	/*
	 * Two nulls have been sent, remove break, and send inits
	 */
	case TUS_INIT1:	
		mtpr(CSTS, IE);
		printd("ON2 ");
		uu->uu_state = TUS_INIT2;
		uu->uu_wbptr = uuinit;
		uu->uu_wcnt = sizeof (uuinit);
		goto top;

	/*
	 * Inits have been sent, wait for a continue msg.
	 */
	case TUS_INIT2:	
		(void) mfpr(CSRD);
		mtpr(CSRS, IE);
		uu->uu_flag = 1;
		break;

	case TUS_IDLE:		/* stray interrupt? */
		break;

	/*
	 * Read cmd packet sent, get ready for data
	 */
	case TUS_SENDR:
		uu->uu_state = TUS_GETH;
		uu->uu_rbptr = (u_char *)&uudata;
		uu->uu_rcnt = 2;
		uu->uu_flag = 1;
		mtpr(CSTS, 0);	/* disable transmitter interrupts */
		printd("OFF ");
		break;

	/*
	 * Write cmd packet sent, wait for continue
	 */
	case TUS_SENDW:	
		uu->uu_state = TUS_WAIT;
		uu->uu_flag = 1;
		if ((mfpr(CSRS)&IE) == 0) {
			printf("NO IE\n");
			mtpr(CSRS, IE);
		}
		break;

	/*
	 * Header sent, send data.
	 */
	case TUS_SENDH:
		uu->uu_state = TUS_SENDD;
		uu->uu_wbptr = (u_char *)uu->uu_addr;
		uu->uu_wcnt = uudata.pk_mcount;
		goto top;

	/*
	 * Data sent, follow with checksum.
	 */
	case TUS_SENDD:	
		uu->uu_state = TUS_SENDC;
		uu->uu_wbptr = (u_char *)&uudata.pk_chksum;
		uu->uu_wcnt = sizeof uudata.pk_chksum;
		goto top;

	/* 
	 * Checksum sent, wait for continue.
	 */
	case TUS_SENDC:
		/*
		 * Updata buffer address and count.
		 */
		uu->uu_addr += uudata.pk_mcount;
		uu->uu_count -= uudata.pk_mcount;
		if (uu->uu_count) {
			uu->uu_state = TUS_WAIT;
			uu->uu_flag = 1;
			break;
		}

		/*
		 * End of transmission, get ready for end packet.
		 */
		uu->uu_state = TUS_GET;
		uu->uu_rbptr = (u_char *)&uudata;
		uu->uu_rcnt = sizeof (uudata);
		uu->uu_flag = 1;
		mtpr(CSTS, 0);
		printd("OFF2 ");
		break;

	/*
	 * Random interrupt, probably from MRSP ACK
	 */
	default:
		break;
	}
	if (uudebug) {
		printd("  new uu_state=");
		printstate(uu->uu_state);
	}
}

/*
 * Compute checksum TU58 fashion
 */
#ifdef lint
uuchk(word, cp, n)
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
uuchk(word0, wp, n)
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

uuwatch()
{
	register int s;
	register struct buf *bp;

	if (uutimer == 0) {
		uu->uu_flag = 0;
		return;
	}
	if (uu->uu_flag)
		uu->uu_flag++;
	if (uu->uu_flag <= 40) {
		timeout(uuwatch, (caddr_t)0, hz);
		return;
	}
	printf("uu: read stalled\n");
	printf("%X %X %X %X %X %X %X %X\n", uu->uu_rbptr, uu->uu_rcnt,
		uu->uu_wbptr, uu->uu_wcnt, uu->uu_state, uu->uu_flag,
		uu->uu_addr, uu->uu_count);
	uu->uu_flag = 0;
	s = splx(TUIPL);
	(void) mfpr(CSRD);
	mtpr(CSRS, IE);		/* in case we were flushing */
	mtpr(CSTS, IE);
	uu->uu_state = TUS_IDLE;
	if (!uutab.b_active) {
		wakeup((caddr_t)&uu);
		goto retry;
	}
	if (++uutab.b_errcnt <= 1) {
		uustart();
		goto retry;
	}
	if (bp = uutab.b_actf) {
		bp->b_flags |= B_ERROR;
		if ((bp->b_flags&B_READ) == 0)
			uu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
		iodone(bp);
	}
retry:
	splx(s);
	timeout(uuwatch, (caddr_t)0, hz);
}
#endif
