/*	tu.c	4.13	83/02/10	*/

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
#include "../h/kernel.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"

#define	printd	if(tudebug) printf
#ifdef	printd
int	tudebug;	/* printd */
#endif	printd

#define	NTU	((cpu == VAX_750) ? 1 : 2)
#define DNUM    01      /* mask for drive number (should match NTU) */
#if !defined(MRSP) || lint
#define	MRSP	(cpu != VAX_750)
#endif
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
	u_char	*tu_rbptr;	/* pointer to buffer for read */
	int	tu_rcnt;	/* how much to read */
	u_char	*tu_wbptr;	/* pointer to buffer for write */
	int	tu_wcnt;	/* how much to write */
	int	tu_state;	/* current tu_state of tansfer operation */
	int	tu_flag;	/* read in progress flag */
	char	*tu_addr;	/* real buffer data address */
	int	tu_count;	/* real requested count */
	int	tu_serrs;	/* count of soft errors */
	int	tu_cerrs;	/* count of checksum errors */
	int	tu_herrs;	/* count of hard errors */
	char    tu_dopen[2];	/* drive is open */
} tu;

/*
 * States
 */
#define	TUS_INIT1	0	/* sending nulls */
#define	TUS_INIT2	1	/* sending inits */
#define	TUS_IDLE	2	/* initialized, no transfer in progress */
#define	TUS_SENDH	3	/* sending header */
#define	TUS_SENDD	4	/* sending data */
#define	TUS_SENDC	5	/* sending checksum */
#define	TUS_SENDR	6	/* sending read command packet */
#define	TUS_SENDW	7	/* sending write command packet */
#define	TUS_GETH	8	/* reading header */
#define	TUS_GETD	9	/* reading data */
#define	TUS_GETC	10	/* reading checksum */
#define	TUS_GET		11	/* reading an entire packet */
#define	TUS_WAIT	12	/* waiting for continue */

#define	TUS_NSTATES	13
char *tustates[TUS_NSTATES] = {
	"INIT1", "INIT2", "IDLE", "SENDH", "SENDD", "SENDC", "SENDR",
	"SENDW", "GETH", "GETD", "GETC", "GET", "WAIT"
};
#define	printstate(state) \
	if ((state) < TUS_NSTATES) \
		printf("%s", tustates[(state)]); \
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
	if ((minor(dev)&DNUM) >= NTU || tu.tu_dopen[minor(dev)&DNUM])
		return (ENXIO);
	if (tutimer == 0) {
		tutimer++;
		timeout(tuwatch, (caddr_t)0, hz);
	}
	tu.tu_dopen[minor(dev)&DNUM]++;
	s = splx(TUIPL);
	/*
	 * If the cassette's already initialized,
	 * just enable interrupts and return.
	 */
	if (tu.tu_state == TUS_IDLE) {
		mtpr(CSRS, IE);
		splx(s);
		return (0);
	}

	/* 
	 * Must initialize, reset the cassette
	 * and wait for things to settle down.
	 */
	tureset();
	sleep((caddr_t)&tu, PZERO+1);
	tutab.b_active = NULL;
	if (tu.tu_state != TUS_IDLE) {
		u.u_error = ENXIO;
		tu.tu_state = TUS_INIT1;
		tu.tu_dopen[minor(dev)&DNUM] = 0;
		tu.tu_rcnt = tu.tu_wcnt = 0;
		mtpr(CSTS, 0);
		mtpr(CSRS, 0);
	}
	splx(s);
	return (0);
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
	if (tu.tu_serrs + tu.tu_cerrs + tu.tu_herrs != 0) {
		/*
		 * A tu58 is like nothing ever seen before;
		 * I guess this is appropriate then...
		 */
		uprintf(
		   "tu%d: %d soft errors, %d chksum errors, %d hard errors\n",
			minor(dev), tu.tu_serrs, tu.tu_cerrs, tu.tu_herrs);
		tu.tu_serrs = tu.tu_cerrs = tu.tu_herrs = 0;
	}
	tu.tu_dopen[minor(dev)&DNUM] = 0;
}

/*
 * Reset the TU58
 */
tureset()
{

	tu.tu_state = TUS_INIT1;
	tu.tu_wbptr = tunull;
	tu.tu_wcnt = sizeof (tunull);
	tucmd.pk_flag = TUF_CMD;
	tucmd.pk_mcount = sizeof (tucmd) - 4;
	tucmd.pk_mod = 0;
	tucmd.pk_seq = 0;
	tucmd.pk_sw = MRSP ? TUSW_MRSP : 0;
	tutab.b_active++;
	mtpr(CSRS, 0);
	mtpr(CSTS, IE|BREAK);
	tuxintr();		/* start output */
}

/*
 * Strategy routine for block I/O
 */
tustrategy(bp)
	register struct buf *bp;
{
	register int s;

	if (bp->b_blkno >= NTUBLK) {
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
	if (tu.tu_state != TUS_IDLE) {
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
	tucmd.pk_count = tu.tu_count = bp->b_bcount;
	tucmd.pk_block = bp->b_blkno;
	tucmd.pk_chksum =
	    tuchk(*((short *)&tucmd), (u_short *)&tucmd.pk_op,
		(int)tucmd.pk_mcount);
	tu.tu_state = bp->b_flags&B_READ ? TUS_SENDR : TUS_SENDW;
	tu.tu_addr = bp->b_un.b_addr;
	tu.tu_count = bp->b_bcount;
	tu.tu_wbptr = (u_char *)&tucmd;
	tu.tu_wcnt = sizeof (tucmd);
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
	if (tu.tu_rcnt) {		/* still waiting for data? */
		*tu.tu_rbptr++ = c;	/* yup, put it there */
		if (--tu.tu_rcnt)	/* decrement count, any left? */
			return;		/* get some more */
	}

	/*
	 * We got all the data we were expecting for now,
	 * switch on the tu_state of the transfer.
	 */
	switch(tu.tu_state) {

	/*
	 * If we get an unexpected "continue",
	 * start all over again...
	 */
	case TUS_INIT2:
		tu.tu_state = c == TUF_CONT ? TUS_IDLE : TUS_INIT1;
		tu.tu_flag = 0;
		wakeup((caddr_t)&tu);
		tustart();
		break;

	/*
	 * Only transition from this state
	 * is on a "continue", so if we don't
	 * get it, reset the world.
	 */
	case TUS_WAIT:			/* waiting for continue */
		if (c != TUF_CONT) {
			tu.tu_state = TUS_INIT1;
			break;
		}
		tu.tu_flag = 0;
		tudata.pk_flag = TUF_DATA;
		tudata.pk_mcount = MIN(128, tu.tu_count);
		tudata.pk_chksum =
		    tuchk(*((short *)&tudata), (u_short *)tu.tu_addr,
			(int)tudata.pk_mcount);
		tu.tu_state = TUS_SENDH;
		tu.tu_wbptr = (u_char *)&tudata;
		tu.tu_wcnt = 2;
		tuxintr();
		break;

	case TUS_SENDW:
		if (c != TUF_CONT)
			goto bad;
		tureset();
		break;

	/*
	 * Got header, now get data; amount to
	 * fetch is include in packet.
	 */
	case TUS_GETH:
		if (tudata.pk_flag == TUF_DATA)
			tu.tu_rbptr = (u_char *)tu.tu_addr;
		tu.tu_rcnt = tudata.pk_mcount;
		tu.tu_state = TUS_GETD;
		break;

	/*
	 * Got the data, now fetch the checksum.
	 */
	case TUS_GETD:
		tu.tu_rbptr = (u_char *)&tudata.pk_chksum;
		tu.tu_rcnt = sizeof (tudata.pk_chksum);
		tu.tu_state = TUS_GETC;
		break;

	case TUS_GET:
	case TUS_GETC:
		/* got entire packet */
#ifdef notdef
		if (tudata.pk_chksum !=
		    tuchk(*((short *)&tudata), (u_short *)
		     (tudata.pk_flag == TUF_DATA ? tu.tu_addr : &tudata.pk_op),
		     (int)tudata.pk_mcount))
			tu.tu_cerrs++;
#endif
		if (tudata.pk_flag == TUF_DATA) {
			/* data packet, advance to next */
			tu.tu_addr += tudata.pk_mcount;
			tu.tu_count -= tudata.pk_mcount;
			tu.tu_state = TUS_GETH;
			tu.tu_rbptr = (u_char *)&tudata; /* next packet */
			tu.tu_rcnt = 2;
		} else if (tudata.pk_flag==TUF_CMD && tudata.pk_op==TUOP_END) {
			/* end packet, idle and reenable transmitter */
			tu.tu_state = TUS_IDLE;
			tu.tu_flag = 0;
			mtpr(CSTS, IE);
			printd("ON ");
			if ((bp = tutab.b_actf) == NULL) {
				printf("tu: no bp, active %d\n",tutab.b_active);
				tustart();
				return;
			}
			if (tudata.pk_mod > 1) {        /* hard error */
				bp->b_flags |= B_ERROR;
				tu.tu_herrs++;
				harderr(bp, "tu");
				printf("  pk_mod %o\n", tudata.pk_mod&0377);
			} else if (tudata.pk_mod != 0)	/* soft error */
				tu.tu_serrs++;
			tutab.b_active = NULL;
			tutab.b_actf = bp->av_forw;
			bp->b_resid = tu.tu_count;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
			iodone(bp);
			tustart();
		} else {
			printf("neither data nor end: %o %o\n",
			    tudata.pk_flag&0xff, tudata.pk_op&0xff);
			mtpr(CSRS, 0);		/* flush the rest */
			tu.tu_state = TUS_INIT1;
		}
		break;

	case TUS_IDLE:
	case TUS_INIT1:
		break;

	default:
bad:
		if (c == TUF_INITF) {
			printf("tu protocol error, state=");
			printstate(tu.tu_state);
			printf(", op=%x, cnt=%d, block=%d\n",
			    tucmd.pk_op, tucmd.pk_count, tucmd.pk_block);
			tutab.b_active = NULL;
			if (bp = tutab.b_actf) {
				bp->b_flags |= B_ERROR;
				tutab.b_actf = bp->av_forw;
				if ((bp->b_flags&B_READ) == 0)
					tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
				iodone(bp);
			}
			tu.tu_state = TUS_INIT1;
		} else {
			printf("tu receive state error, state=");
			printf(", byte=%x\n", c);
#ifdef notdef
			tu.tu_state = TUS_INIT1; */
#endif
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
	if (tu.tu_wcnt) {
		/* still stuff to send, send one byte */
		while ((mfpr(CSTS) & READY) == 0)
			;
		mtpr(CSTD, *tu.tu_wbptr++);
		tu.tu_wcnt--;
		return;
	}

	/*
	 * Last message byte was sent out.
	 * Switch on tu_state of transfer.
	 */
	if (tudebug) {
		printf("tuxintr: state=");
		printstate(tu.tu_state);
	}
	switch(tu.tu_state) {

	/*
	 * Two nulls have been sent, remove break, and send inits
	 */
	case TUS_INIT1:	
		mtpr(CSTS, IE);
		printd("ON2 ");
		tu.tu_state = TUS_INIT2;
		tu.tu_wbptr = tuinit;
		tu.tu_wcnt = sizeof (tuinit);
		goto top;

	/*
	 * Inits have been sent, wait for a continue msg.
	 */
	case TUS_INIT2:	
		(void) mfpr(CSRD);
		mtpr(CSRS, IE);
		tu.tu_flag = 1;
		break;

	case TUS_IDLE:		/* stray interrupt? */
		break;

	/*
	 * Read cmd packet sent, get ready for data
	 */
	case TUS_SENDR:
		tu.tu_state = TUS_GETH;
		tu.tu_rbptr = (u_char *)&tudata;
		tu.tu_rcnt = 2;
		tu.tu_flag = 1;
		mtpr(CSTS, 0);	/* disable transmitter interrupts */
		printd("OFF ");
		break;

	/*
	 * Write cmd packet sent, wait for continue
	 */
	case TUS_SENDW:	
		tu.tu_state = TUS_WAIT;
		tu.tu_flag = 1;
		if ((mfpr(CSRS)&IE) == 0) {
			printf("NO IE\n");
			mtpr(CSRS, IE);
		}
		break;

	/*
	 * Header sent, send data.
	 */
	case TUS_SENDH:
		tu.tu_state = TUS_SENDD;
		tu.tu_wbptr = (u_char *)tu.tu_addr;
		tu.tu_wcnt = tudata.pk_mcount;
		goto top;

	/*
	 * Data sent, follow with checksum.
	 */
	case TUS_SENDD:	
		tu.tu_state = TUS_SENDC;
		tu.tu_wbptr = (u_char *)&tudata.pk_chksum;
		tu.tu_wcnt = sizeof tudata.pk_chksum;
		goto top;

	/* 
	 * Checksum sent, wait for continue.
	 */
	case TUS_SENDC:
		/*
		 * Updata buffer address and count.
		 */
		tu.tu_addr += tudata.pk_mcount;
		tu.tu_count -= tudata.pk_mcount;
		if (tu.tu_count) {
			tu.tu_state = TUS_WAIT;
			tu.tu_flag = 1;
			break;
		}

		/*
		 * End of transmission, get ready for end packet.
		 */
		tu.tu_state = TUS_GET;
		tu.tu_rbptr = (u_char *)&tudata;
		tu.tu_rcnt = sizeof (tudata);
		tu.tu_flag = 1;
		mtpr(CSTS, 0);
		printd("OFF2 ");
		break;

	/*
	 * Random interrupt, probably from MRSP ACK
	 */
	default:
		break;
	}
	if (tudebug) {
		printd("  new tu_state=");
		printstate(tu.tu_state);
	}
}

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
		tu.tu_flag = 0;
		return;
	}
	if (tu.tu_flag)
		tu.tu_flag++;
	if (tu.tu_flag <= 40) {
		timeout(tuwatch, (caddr_t)0, hz);
		return;
	}
	printf("tu: read stalled\n");
	printf("%X %X %X %X %X %X %X %X\n", tu.tu_rbptr, tu.tu_rcnt,
		tu.tu_wbptr, tu.tu_wcnt, tu.tu_state, tu.tu_flag,
		tu.tu_addr, tu.tu_count);
	tu.tu_flag = 0;
	s = splx(TUIPL);
	(void) mfpr(CSRD);
	mtpr(CSRS, IE);		/* in case we were flushing */
	mtpr(CSTS, IE);
	tu.tu_state = TUS_IDLE;
	if (!tutab.b_active) {
		wakeup((caddr_t)&tu);
		goto retry;
	}
	if (++tutab.b_errcnt <= 1) {
		tustart();
		goto retry;
	}
	if (bp = tutab.b_actf) {
		bp->b_flags |= B_ERROR;
		if ((bp->b_flags&B_READ) == 0)
			tu_vee(&pcnt[minor(bp->b_dev)&DNUM]);
		iodone(bp);
	}
retry:
	splx(s);
	timeout(tuwatch, (caddr_t)0, hz);
}

tu_pee(cp)
	char *cp;
{
	register int s;

	s = splx(TUIPL);
	if (++(*cp) > NTUQ)
		sleep(cp, PRIBIO);
	splx(s);
}

tu_vee(cp)
	char *cp;
{
	register int s;

	s = splx(TUIPL);
	if (--(*cp) <= NTUQ)
		wakeup(cp);
	splx(s);
}
#endif
