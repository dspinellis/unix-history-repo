/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tu.c	7.1 (Berkeley) 6/5/86
 */

#if defined(VAX750) || defined(VAX730)
/*
 * TU58 DECtape II device driver
 *
 * TU58 console cassette driver (for VAX-11/750 or VAX-11/730).
 * The TU58 is treated as a block device (only).  Error detection and
 * recovery is not extensive, but sufficient for most situations. It is 
 * assumed that the TU58 will follow the RSP (or MRSP) protocol exactly,
 * very few protocol errors are checked for.  It is also assumed that
 * the 730 uses Modified RSP (MRSP), while the 750 may use either RSP
 * or MRSP depending on whether defined(MRSP) is true or not.
 * In the case of a 750 without MRSP, the only way for the CPU to
 * keep up with the tu58 is to lock out virtually everything else.
 *
 * NOTE: Reading large amounts of data from the tu58 is likely
 *	 to crash your system if you are running multiuser.
 *	 	******FOR SINGLE USER USE ONLY*****
 */
#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"

#include "cpu.h"
#include "mtpr.h"
#include "rsp.h"

#define	printd	if(tudebug) printf
#ifdef	printd
int	tudebug;	/* printd */
#endif	printd

#define	NTU	((cpu == VAX_750) ? 1 : 2)
#define DNUM    01		/* mask for drive number (should match NTU) */
#define	NTUBLK	512		/* number of blocks on a TU58 cassette */
#define WRV     02              /* bit in minor dev => write w. read verify */
#define NTUQ    2               /* # of blocks which can be queued up */
#define	spltu()	((cpu == VAX_750) ? spl7() : spl4())

#ifndef MRSP
#define MRSP (cpu != VAX_750)
#endif

/*
 * State information 
 */
struct tu {
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
} tu;


/*
 * Device register bits
 */
#define	READY	0200		/* transmitter ready */
#define	DONE	0200		/* receiver done */
#define	IE	0100		/* interrupt enable */
#define	BREAK	1		/* send break */

struct packet tucmd;		/* a command sent to the TU58 */
struct packet tudata;		/* a command or data returned from TU58 */

char *tustates[TUS_NSTATES] = {
	"INIT1", "INIT2", "IDLE", "SENDH", "SENDD", "SENDC", "SENDR",
	"SENDW", "GETH", "GETD", "GETC", "GET", "WAIT", "RCVERR", "CHKERR"
};

u_char	tunull[2] = { 0, 0 };	/* nulls to send for initialization */
u_char	tuinit[2] = { TUF_INITF, TUF_INITF };	/* inits to send */
static char tu_pcnt[2];            		/* pee/vee counters */
int	tutimer = 0;
int	tuwake();
struct buf tutab;				/* I/O queue header */

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
	if ((minor(dev)&DNUM) >= NTU)
		return (ENXIO);
	if (tu.tu_dopen[minor(dev)&DNUM])
		return (EBUSY);
	if (tutimer++ == 0)
		timeout(tuwatch, (caddr_t)0, hz);

	s = spltu();
	tu.tu_dopen[minor(dev)&DNUM]++;
	/*
	 * If the cassette's already initialized,
	 * just enable interrupts and return.
	 */
	if (tu.tu_state == TUS_IDLE) {
		mtpr(CSRS, IE);
		goto ok;
	}

	/* 
	 * Must initialize, reset the cassette
	 * and wait for things to settle down.
	 */
	tureset();
	sleep((caddr_t)&tu, PZERO+1);
	tutab.b_active = NULL;
	if (tu.tu_state != TUS_IDLE) {
		tu.tu_state = TUS_INIT1;
		tu.tu_dopen[minor(dev)&DNUM] = 0;
		tu.tu_rcnt = tu.tu_wcnt = 0;
		mtpr(CSTS, 0);
		mtpr(CSRS, 0);
		splx(s);
		return (EIO);
	}
ok:
	splx(s);
	return (0);
}

/*
 * Close the TU58, but make sure all
 * outstanding i/o is complete first..
 */
/* ARGSUSED */
tuclose(dev, flag)
	dev_t dev;
	int flag;
{
	int s, unit = minor(dev);
	struct buf *bp, *last = NULL;

	s = spltu();
	while (tu_pcnt[unit])
		sleep(&tu_pcnt[unit], PRIBIO);
	/*
	 * No more writes are pending, scan the 
	 * buffer queue for oustanding reads from
	 * this unit.
	 */
	for (bp = tutab.b_actf; bp; bp = bp->b_actf) {
		if (bp->b_dev == dev)
			last = bp;
	}
	if (last) {
		last->b_flags |= B_CALL;
		last->b_iodone = tuwake;
		sleep((caddr_t)last, PRIBIO);
	}
	tu.tu_dopen[unit&DNUM] = 0;
	if (!tu.tu_dopen[0] && !tu.tu_dopen[1]) {
		tutimer = 0;
		mtpr(CSRS, 0);
		tu.tu_flag = 0;
	}
	splx(s);
}

tuwake(bp)
	struct buf *bp;
{
	wakeup((caddr_t)bp);
}

/*
 * Reset the TU58
 */
tureset()
{

	mtpr(CSRS, 0);
	tu.tu_state = TUS_INIT1;
	tu.tu_wbptr = tunull;
	tu.tu_wcnt = sizeof (tunull);
	tucmd.pk_flag = TUF_CMD;
	tucmd.pk_mcount = sizeof (tucmd) - 4;
	tucmd.pk_mod = 0;
	tucmd.pk_seq = 0;
	tucmd.pk_sw = MRSP ? TUSW_MRSP : 0;
	tutab.b_active++;
	mtpr(CSTS, IE | BREAK);
	tuxintr();			/* start output */
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
		tu_pee(&tu_pcnt[minor(bp->b_dev)&DNUM]);
	bp->av_forw = NULL;
	s = spltu();
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
	int s;

	if ((bp = tutab.b_actf) == NULL)
		return;
	s = spltu();
	if (tu.tu_state != TUS_IDLE) {
		tureset();
		splx(s);
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
	tu.tu_wbptr = (u_char *)&tucmd;
	tu.tu_wcnt = sizeof (tucmd);
	tuxintr();
	splx(s);
}

/*
 * TU58 receiver interrupt
 */
turintr()
{
	register struct buf *bp;
	register int c;

	c = mfpr(CSRD)&0xff;
	if (MRSP) {
		while ((mfpr(CSTS)&READY) == 0)
			;
		mtpr(CSTD, TUF_CONT);			/* ACK */
		if (tu.tu_rcnt) {
			*tu.tu_rbptr++ = c;
			if (--tu.tu_rcnt)
				return;
		}
	}

	/*
	 * Switch on the state of the transfer.
	 */
	switch(tu.tu_state) {

	/*
	 * Probably an overrun error,
	 * cannot happen if MRSP is used
	 */
	case TUS_RCVERR:
		mtpr(CSRS, 0);					/* flush */
		printf("overrun error, transfer restarted\n");	/* DEBUG */
		tu.tu_serrs++;
		tu_restart();
		break;

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
		switch(c) {
		case TUF_CONT:  /* got the expected continue */
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

		case TUF_CMD:   /* sending us an END packet...error */
			tu.tu_state = TUS_GET;
			tu.tu_rbptr = (u_char *) &tudata;
			tu.tu_rcnt = sizeof (tudata) - 1;
			tu.tu_flag = 1;
			mtpr (CSTS, 0);
			*tu.tu_rbptr = c;
			break;

		case TUF_INITF:
			tureset();
			break;

		default:        /* something random...bad news */
			tu.tu_state = TUS_INIT1;
			break;
		}
		break;

	case TUS_SENDW:
		if (c != TUF_CONT && c != TUF_INITF) 
			goto bad;
		tureset();
		break;

	/*
	 * Got header, now get data; amount to
	 * fetch is included in packet.
	 */
	case TUS_GETH:
		if (MRSP && (tudata.pk_flag == TUF_DATA))
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

	case TUS_CHKERR:		/* from tudma only */
		tu.tu_cerrs++;
		goto tus_get;

	case TUS_GET:
		if (MRSP)
			/* 
		 	 * The checksum has already been calculated and
		 	 * verified in the pseudo DMA routine
		 	 */
			goto tus_get;

	case TUS_GETC:
		/* got entire packet */
		if (tudata.pk_chksum !=
		    tuchk(*((short *)&tudata), (u_short *)
		     (tudata.pk_flag == TUF_DATA ? 
		     (u_short *) tu.tu_addr : (u_short *)&tudata.pk_op),
		     (int)tudata.pk_mcount))
			tu.tu_cerrs++;
tus_get:
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
				printf("tu%d: no bp, active %d\n",
					tudata.pk_unit, tutab.b_active);
				tustart();
				return;
			}
			if (tudata.pk_mod > 1) {        /* hard error */
				bp->b_flags |= B_ERROR;
				tu.tu_herrs++;
				printf("tu%d: hard error bn%d,", 
					minor(bp->b_dev)&DNUM, bp->b_blkno);
				printf("  pk_mod %o\n", tudata.pk_mod&0377);
			} else if (tudata.pk_mod != 0)	/* soft error */
				tu.tu_serrs++;
			tutab.b_active = NULL;
			tutab.b_actf = bp->av_forw;
			bp->b_resid = tu.tu_count;
			if ((bp->b_flags&B_READ) == 0)
				tu_vee(&tu_pcnt[minor(bp->b_dev)&DNUM]);
			iodone(bp);
			tustart();
		} else {
			/*
			 * Neither data nor end: data was lost
			 * somehow, restart the transfer
			 */
			mtpr(CSRS, 0);		/* flush the rest */
			tu_restart();
			tu.tu_serrs++;
		}
		break;

	case TUS_IDLE:
	case TUS_INIT1:
		break;

	default:
bad:
		if (c == TUF_INITF) {
			printf("tu%d protocol error, state=", 
							(int)tudata.pk_unit);
			printstate(tu.tu_state);
			printf(", op=%x, cnt=%d, block=%d\n",
			    tucmd.pk_op, tucmd.pk_count, tucmd.pk_block);
			tutab.b_active = NULL;
			if (bp = tutab.b_actf) {
				bp->b_flags |= B_ERROR;
				tutab.b_actf = bp->av_forw;
				if ((bp->b_flags&B_READ) == 0)
					tu_vee(&tu_pcnt[minor(bp->b_dev)&DNUM]);
				iodone(bp);
			}
			tu.tu_state = TUS_INIT1;
		} else {
			printf("tu%d: receive state error, state=",
						(int)tudata.pk_unit);
			printstate(tu.tu_state);
			printf(", byte=%x\n", c & 0xff);
			if (tutab.b_actf)
				tu_restart();
			else
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
	 * Switch on state of transfer.
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
	case TUS_IDLE:

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
	register u_short *wp;	/* r10 */
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

	if (tutimer == 0)
		return;

	if (tu.tu_flag == 0) {		/* if no read in progress - skip */
		timeout(tuwatch, (caddr_t)0, hz);
		return;
	}
	if (tu.tu_flag++ <= 40) {
		timeout(tuwatch, (caddr_t)0, hz);
		return;
	}
	printf("tu%d: read stalled\n", tudata.pk_unit);
#ifdef TUDEBUG
	printf("%X %X %X %X %X %X %X %X\n", tu.tu_rbptr, tu.tu_rcnt,
		tu.tu_wbptr, tu.tu_wcnt, tu.tu_state, tu.tu_flag,
		tu.tu_addr, tu.tu_count);
#endif
	s = spltu();
	tu.tu_flag = 0;
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
			tu_vee(&tu_pcnt[minor(bp->b_dev)&DNUM]);
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

	s = spltu();
	if (++(*cp) > NTUQ)
		sleep(cp, PRIBIO);
	splx(s);
}

tu_vee(cp)
	char *cp;
{
	register int s;

	s = spltu();
	if (--(*cp) <= NTUQ)
		wakeup(cp);
	splx(s);
}

tu_restart()
{
	tureset();
	timeout(tustart, (caddr_t)0, hz * 3);
}

#endif
