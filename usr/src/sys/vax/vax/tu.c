/*	tu.c	4.6	82/05/27	*/

#if defined(VAX750) || defined(VAX730)
/*
 * TU58 DECtape II device driver
 *
 * This driver controls the console TU58s on a VAX-11/750 or VAX-11/730.
 * It could be easily modified for a Unibus TU58.  The TU58
 * is treated as a block device (only).  Error detection and
 * recovery is almost non-existant.  It is assumed that the
 * TU58 will follow the RSP protocol exactly, very few protocol
 * errors are checked for.  It is assumed that the 750 uses standard
 * RSP while the 730 uses Modified RSP (MRSP).  At the time when 750's
 * are converted to MRSP (by replacing EPROMS in the TU58), the tests
 * based on MRSP can be removed.
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mtpr.h"
#include "../h/cpu.h"

#define	printd	if(tudebug) printf
#ifdef	printd
int	tudebug;	/* printd */
#endif	printd

#define	NTU	((cpu == VAX_750) ? 1 : 2)
#define DNUM    01      /* mask for drive number (should match NTU) */
#define	MRSP	(cpu != VAX_750)
#define	NTUBLK	512		/* number of blocks on a TU58 cassette */
#define WRV     02              /* bit in minor dev => write w. read verify */
#define NTUQ    2               /* # of blocks which can be queued up */
#define	TUIPL	((cpu == VAX_750) ? 0x17 : 0x14)

/*
 * Device register bits
 */
#define	READY	0200		/* transmitter ready */
#define	DONE	0200		/* receiver done */
#define	IE	0100		/* interrupt enable */
#define	BREAK	1		/* send break */

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

struct packet tucmd;		/* a command sent to the TU58 */
struct packet tudata;		/* a command or data returned from TU58 */

/*
 * State information
 */
struct tu {
	u_char	*rbptr;		/* pointer to buffer for read */
	int	rcnt;		/* how much to read */
	u_char	*wbptr;		/* pointer to buffer for write */
	int	wcnt;		/* how much to write */
	int	state;		/* current state of tansfer operation */
	int	flag;		/* read in progress flag */
	char	*addr;		/* real buffer data address */
	int	count;		/* real requested count */
	int	serrs;		/* count of soft errors */
	int	cerrs;		/* count of checksum errors */
	int	herrs;		/* count of hard errors */
	char    dopen[2];       /* drive is open */
} tu;

/*
 * States
 */
#define	INIT1	0		/* sending nulls */
#define	INIT2	1		/* sending inits */
#define	IDLE	2		/* initialized, no transfer in progress */
#define	SENDH	3		/* sending header */
#define	SENDD	4		/* sending data */
#define	SENDC	5		/* sending checksum */
#define	SENDR	6		/* sending read command packet */
#define	SENDW	7		/* sending write command packet */
#define	GETH	8		/* reading header */
#define	GETD	9		/* reading data */
#define	GETC	10		/* reading checksum */
#define	GET	11		/* reading an entire packet */
#define	WAIT	12		/* waiting for continue */

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
#define	TUOP_INIT	1		/* initialize */
#define	TUOP_READ	2		/* read block */
#define	TUOP_WRITE	3		/* write block */
#define	TUOP_SEEK	5		/* seek to block */
#define TUOP_DIAGNOSE	7		/* run micro-diagnostics */
#define	TUOP_END	0100		/* end packet */

/*
 * Mod Flags
 */
#define TUMD_WRV        1               /* write with read verify */

/*
 * Switches
 */
#define	TUSW_MRSP	010		/* use Modified RSP */

u_char	tunull[2] = { 0, 0 };	/* nulls to send for initialization */
u_char	tuinit[2] = { TUF_INITF, TUF_INITF };	/* inits to send */
static char pcnt[2];            /* pee/vee counters */
int	tutimer = 0;
struct buf tutab;		/* I/O queue header */

/*
 * Open the TU58
 */
/*ARGSUSED*/
tuopen(dev, flag)
{
	extern int tuwatch();
	register s;

#ifdef lint
	turintr(); tuwintr();
#endif
	if ((minor(dev)&DNUM) >= NTU || tu.dopen[minor(dev)&DNUM]) {
		u.u_error = ENXIO;
		return;
	}
	if (tutimer == 0) {
		tutimer++;
		timeout(tuwatch, (caddr_t)0, hz);
	}
	tu.dopen[minor(dev)&DNUM]++;
	s = splx(TUIPL);
	if (tu.state != IDLE) {
		tureset();
		sleep((caddr_t)&tu, PZERO+1);
		tutab.b_active = NULL;
		if (tu.state != IDLE) {		/* couldn't initialize */
			u.u_error = ENXIO;
			tu.state = INIT1;
			tu.dopen[minor(dev)&DNUM] = 0;
			tu.rcnt = tu.wcnt = 0;
			mtpr(CSTS, 0);
			mtpr(CSRS, 0);
		}
	} else
		mtpr(CSRS, IE);
	splx(s);
}

/*
 * Close the TU58
 */
tuclose(dev)
{

	if (tutab.b_active == 0) {
		mtpr(CSRS, 0);
		tutimer = 0;
	}
	if (tu.serrs + tu.cerrs + tu.herrs != 0) {	/* any errors ? */
		uprintf(
		   "tu%d: %d soft errors, %d chksum errors, %d hard errors\n",
			minor(dev), tu.serrs, tu.cerrs, tu.herrs);
		tu.serrs = tu.cerrs = tu.herrs = 0;
	}
	tu.dopen[minor(dev)&DNUM] = 0;
}

/*
 * Reset the TU58
 */
tureset()
{

	tu.state = INIT1;
	tu.wbptr = tunull;
	tu.wcnt = sizeof tunull;
	tucmd.pk_flag = TUF_CMD;
	tucmd.pk_mcount = sizeof tucmd - 4;
	tucmd.pk_mod = 0;
	tucmd.pk_seq = 0;
	tucmd.pk_sw = MRSP ? TUSW_MRSP : 0;
	tutab.b_active++;
	mtpr(CSRS, 0);
	mtpr(CSTS, IE|BREAK);
	tuxintr();		/* start output */
	return;
}

/*
 * Strategy routine for block I/O
 */
tustrategy(bp)
	register struct buf *bp;
{
	register int s;

	if (bp->b_blkno >= NTUBLK) {	/* block number out of range? */
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	if ((bp->b_flags&B_READ) == 0)
		tu_pee(&pcnt[minor(bp->b_dev)&DNUM]);
	bp->av_forw = NULL;
	s = splx(TUIPL);
	if (tutab.b_actf == NULL)
		tutab.b_actf = bp;
	else
		tutab.b_actl->av_forw = bp;
	tutab.b_actl = bp;
	if (tutab.b_active == NULL)
		tustart();
	splx(s);
}

/*
 * Start the transfer
 */
tustart()
{
	register struct buf *bp;

	if ((bp = tutab.b_actf) == NULL)
		return;
	if (tu.state != IDLE) {
		tureset();
		return;
	}
	tutab.b_active++;
	tutab.b_errcnt = 0;
	tucmd.pk_op = bp->b_flags&B_READ ? TUOP_READ : TUOP_WRITE;
	tucmd.pk_mod = ((bp->b_flags&B_READ) == 0 && (minor(bp->b_dev)&WRV)) ?
	    TUMD_WRV : 0;
	tucmd.pk_unit = (minor(bp->b_dev)&DNUM);
	tucmd.pk_sw = MRSP ? TUSW_MRSP : 0;
	tucmd.pk_count = tu.count = bp->b_bcount;
	tucmd.pk_block = bp->b_blkno;
	tucmd.pk_chksum =
	    tuchk(*((short *)&tucmd), (caddr_t)&tucmd.pk_op,
		(int)tucmd.pk_mcount);
	tu.state = bp->b_flags&B_READ ? SENDR : SENDW;
	tu.addr = bp->b_un.b_addr;
	tu.count = bp->b_bcount;
	tu.wbptr = (u_char *)&tucmd;
	tu.wcnt = sizeof tucmd;
	tuxintr();
}

/*
 * TU58 receiver interrupt
 */
turintr()
{
	register struct buf *bp;
	register int c;

	c = mfpr(CSRD)&0xff;		/* get the char, clear the interrupt */
	if (MRSP) {
		while ((mfpr(CSTS)&READY) == 0)
			;
		mtpr(CSTD, TUF_CONT);	/* ACK */
	}
	if (tu.rcnt) {			/* still waiting for data? */
		*tu.rbptr++ = c;	/* yup, put it there */
		if (--tu.rcnt)		/* decrement count, any left? */
			return;		/* get some more */
	}

	/*
	 * We got all the data we were expecting for now,
	 * switch on the state of the transfer.
	 */
	switch(tu.state) {

	case INIT2:
		if (c == TUF_CONT)	/* did we get the expected continue? */
			tu.state = IDLE;
		else
			tu.state = INIT1;	/* bad news... */
		tu.flag = 0;
		wakeup((caddr_t)&tu);
		tustart();
		break;

	case WAIT:			/* waiting for continue */
		if (c != TUF_CONT) {
			tu.state = INIT1;	/* bad news... */
			break;
		}
		tu.flag = 0;
		tudata.pk_flag = TUF_DATA;
		tudata.pk_mcount = MIN(128, tu.count);
		tudata.pk_chksum =
		    tuchk(*((short *)&tudata), (caddr_t)tu.addr,
			(int)tudata.pk_mcount);
		tu.state = SENDH;
		tu.wbptr = (u_char *)&tudata;
		tu.wcnt = 2;
		tuxintr();
		break;

	case SENDW:
		if (c == TUF_CONT) {
			tureset();
			break;
		} else
			goto bad;

	case GETH:		/* got header, get data */
		if (tudata.pk_flag == TUF_DATA)		/* data message? */
			tu.rbptr = (u_char *)tu.addr;	/* yes put in buffer */
		tu.rcnt = tudata.pk_mcount;		/* amount to get */
		tu.state = GETD;
		break;

	case GETD:		/* got data, get checksum */
		tu.rbptr = (u_char *)&tudata.pk_chksum;
		tu.rcnt = sizeof tudata.pk_chksum;
		tu.state = GETC;
		break;

	case GET:
	case GETC:		/* got entire packet */
#ifdef notdef
		if (tudata.pk_chksum !=
		    tuchk(*((short *)&tudata),
		     tudata.pk_flag == TUF_DATA ? tu.addr : &tudata.pk_op,
		     (int)tudata.pk_mcount))
			tu.cerrs++;
#endif
		if (tudata.pk_flag == TUF_DATA) {
			/* data packet, advance to next */
			tu.addr += tudata.pk_mcount;
			tu.count -= tudata.pk_mcount;
			tu.state = GETH;
			tu.rbptr = (u_char *)&tudata;	/* next packet */
			tu.rcnt = 2;
		} else if (tudata.pk_flag==TUF_CMD && tudata.pk_op==TUOP_END) {
			/* end packet, idle and reenable transmitter */
			tu.state = IDLE;
			tu.flag = 0;
			mtpr(CSTS, IE);
			printd("ON ");
			if ((bp = tutab.b_actf) == NULL) {
				printf("tu: no bp!\n");
				printf("active %d\n", tutab.b_active);
				tustart();
				return;
			}
			if (tudata.pk_mod > 1) {        /* hard error */
				bp->b_flags |= B_ERROR;
				tu.herrs++;
				harderr(bp, "tu");
				printf("  pk_mod %o\n", tudata.pk_mod&0377);
			} else if (tudata.pk_mod > 0)	/* soft error */
				tu.serrs++;
			tutab.b_active = NULL;
			tutab.b_actf = bp->av_forw;
			bp->b_resid = tu.count;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
			iodone(bp);
			tustart();
		} else {
			printf("neither data nor end: %o %o\n",
			    tudata.pk_flag&0xff, tudata.pk_op&0xff);
			mtpr(CSRS, 0);		/* flush the rest */
			tu.state = INIT1;
		}
		break;

	case IDLE:
	case INIT1:
		break;

	bad:
	default:
		if (c == TUF_INITF) {
			printf("TU protocol error, state %d\n", tu.state);
			printf("%o %d %d\n",
			    tucmd.pk_op, tucmd.pk_count, tucmd.pk_block);
			tutab.b_active = NULL;
			if (bp = tutab.b_actf) {
				bp->b_flags |= B_ERROR;
				tutab.b_actf = bp->av_forw;
				if ((bp->b_flags&B_READ) == 0)
					tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
				iodone(bp);
			}
			tu.state = INIT1;
		} else {
			printf("TU receive state error %d %o\n", tu.state, c);
		/*	tu.state = INIT1; */
			wakeup((caddr_t)&tu);
		}
	}
}

/*
 * TU58 transmitter interrupt
 */
tuxintr()
{

top:
	if (tu.wcnt) {
		/* still stuff to send, send one byte */
		while ((mfpr(CSTS) & READY) == 0)
			;
		mtpr(CSTD, *tu.wbptr++);
		tu.wcnt--;
		return;
	}

	/*
	 * Last message byte was sent out.
	 * Switch on state of transfer.
	 */
	printd("tuxintr: state %d\n", tu.state);
	switch(tu.state) {

	case INIT1:		/* two nulls sent, remove break, send inits */
		mtpr(CSTS, IE);
		printd("ON2 ");
		tu.state = INIT2;
		tu.wbptr = tuinit;
		tu.wcnt = sizeof tuinit;
		goto top;

	case INIT2:		/* inits sent, wait for continue */
		(void) mfpr(CSRD);
		mtpr(CSRS, IE);
		tu.flag = 1;
		break;

	case IDLE:		/* stray interrupt? */
		break;

	case SENDR:		/* read cmd packet sent, get ready for data */
		tu.state = GETH;
		tu.rbptr = (u_char *)&tudata;
		tu.rcnt = 2;
		tu.flag = 1;
		mtpr(CSTS, 0);	/* disable transmitter interrupts */
		printd("OFF ");
		break;

	case SENDW:		/* write cmd packet sent, wait for continue */
		tu.state = WAIT;
		tu.flag = 1;
		if ((mfpr(CSRS)&IE) == 0) {
			printf("NO IE\n");
			mtpr(CSRS, IE);
		}
		break;

	case SENDH:		/* header sent, send data */
		tu.state = SENDD;
		tu.wbptr = (u_char *)tu.addr;
		tu.wcnt = tudata.pk_mcount;
		goto top;

	case SENDD:		/* data sent, send checksum */
		tu.state = SENDC;
		tu.wbptr = (u_char *)&tudata.pk_chksum;
		tu.wcnt = sizeof tudata.pk_chksum;
		goto top;

	case SENDC:		/* checksum sent, wait for continue */
		tu.addr += tudata.pk_mcount;	/* update buffer address */
		tu.count -= tudata.pk_mcount;	/* and count */
		if (tu.count == 0) {		/* all done? */
			tu.state = GET;		/* set up to get end packet */
			tu.rbptr = (u_char *)&tudata;
			tu.rcnt = sizeof tudata;
			tu.flag = 1;
			mtpr(CSTS, 0);
			printd("OFF2 ");
		} else {
			tu.state = WAIT;	/* wait for continue */
			tu.flag = 1;
		}
		break;

	default:	/* random interrupt, probably from MRSP ACK */
		break;
	}
	printd("  new state %d\n", tu.state);
}

/*
 * Compute checksum TU58 fashion
 */
#ifdef notdef
tuchk(word, cp, n)
	register word;
	register unsigned short *cp;
{
	register c = n >> 1;
	register long temp;

	do {
		temp = *cp++;	/* temp, only because vax cc won't *r++ */
		word += temp;
	} while (--c > 0);
	if (n & 1)
		word += *(unsigned char *)cp;
	while (word & 0xFFFF0000)
		word = (word & 0xFFFF) + ((word >> 16) & 0xFFFF);
	return (word);
}
#else
tuchk(word0, wp, n)
register int word0;	/* r11 */
register char *wp;	/* r10 */
register int n;		/* r9 */
{
	asm("loop:");
	asm("	addw2	(r10)+,r11");	/* add a word to sum */
	asm("	adwc	$0,r11");	/* add in carry, end-around */
	asm("	acbl	$2,$-2,r9,loop");	/* done yet? */
	asm("	blbc	r9,ok");	/* odd byte count? */
	asm("	movzbw	(r10),r10");	/* yes, get last byte */
	asm("	addw2	r10,r11");	/* add it in */
	asm("	adwc	$0,r11");	/* and the carry */
	asm("ok:");
	asm("	movl	r11,r0");	/* return sum */
}
#endif

tuwatch()
{
	register int s;
	register struct buf *bp;

	if (tutimer == 0) {
		tu.flag = 0;
		return;
	}
	if (tu.flag)
		tu.flag++;
	if (tu.flag > 40) {
		printf("tu: read stalled\n");
		printf("%X %X %X %X %X %X %X %X\n", tu.rbptr, tu.rcnt,
		tu.wbptr, tu.wcnt, tu.state, tu.flag, tu.addr, tu.count);
		tu.flag = 0;
		s = splx(TUIPL);
		(void) mfpr(CSRD);
		mtpr(CSRS, IE);		/* in case we were flushing */
		mtpr(CSTS, IE);
		tu.state = IDLE;
		if (tutab.b_active) {
			if (++tutab.b_errcnt > 1) {
				if (bp = tutab.b_actf) {
					bp->b_flags |= B_ERROR;
					if ((bp->b_flags&B_READ) == 0)
						tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
					iodone(bp);
				}
			} else
				tustart();
		} else
			wakeup((caddr_t)&tu);
		splx(s);
	}
	timeout(tuwatch, (caddr_t)0, hz);
}

tu_pee(cp)
char *cp;
{
	register int s;

	s = splx(TUIPL);
	if (++(*cp) > NTUQ) {
		sleep(cp, PRIBIO);
	}
	splx(s);
}

tu_vee(cp)
char *cp;
{
	register int s;

	s = splx(TUIPL);
	if (--(*cp) <= NTUQ) {
		wakeup(cp);
	}
	splx(s);
}
#endif
