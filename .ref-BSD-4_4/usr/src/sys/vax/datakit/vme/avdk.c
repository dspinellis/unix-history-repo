#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/edt.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/sbd.h"

#include "dkctlr.h"
#include "dkio.h"
#include "dkmod.h"
#include "dkstat.h"

static int open(queue_t *, dev_t, int, int);
static int close(queue_t *);
static int wput(queue_t *, mblk_t *);

static struct module_info minfo = {
	0, "avdk", 0, 1024, 10240, 2048,
};

static struct qinit rinit = {
	NULL, NULL, open, close, NULL, &minfo, NULL
};

static struct qinit winit = {
	wput, NULL, NULL, NULL, NULL, &minfo, NULL
};

struct streamtab avdkinfo = {
	&rinit, &winit, NULL, NULL
};

extern int avdkctlrcnt;
extern struct dkctlr avdkctlr[];
extern int avdkmaxchno;
extern int avdkemajor[];
extern struct dkctlrstat avdkstat;

extern struct dkstat dkstat;
extern struct dkmodule dkmod[];

struct device {
	volatile unsigned short	timer0;
	volatile unsigned short	timer2;
	volatile unsigned short	pad0x004[2];
	volatile unsigned short	ctl;
	volatile unsigned short	pad0x00A[3];
	volatile unsigned short	stat;
	volatile unsigned short	pad0x012[3];
	volatile unsigned short	rxfifo;
	volatile unsigned short	pad0x01A[3];
	volatile unsigned short	txfifo;
	volatile unsigned short	pad0x022[3];
	volatile unsigned char	pad0x028;
	volatile unsigned char	vector;
	volatile unsigned short	pad0x2A[107];
	volatile unsigned short	dmac[128];
};

#define IENABLE		0x0001		/* ctl */
#define FAIL		0x0004
#define RXIE		0x1000

#define ALIVE		0x0001		/* stat */
#define RXFULL		0x0008
#define TXRDY		0x0040
#define CPMT		0x0080

#define	CTL		0x0100		/* rxfifo/txfifo */
#define CHNO		0x0200
#define TXEOD		0x0400
#define PERR		0X0800
#define RXMT		0x1000

#define MAXTXBURST	1024	

static struct ptr {
	unsigned char	dead;
	unsigned char	strange_but_true;
} ptr[4];					/* urk */

static int	
reset(struct dkctlr *ctlr)
{
	struct device *addr = ctlr->addr;

	addr->ctl = FAIL;
	while ((addr->rxfifo & RXMT) == 0)
		;
	addr->txfifo = CHNO|511;
	addr->txfifo = 0;
	addr->txfifo = TXEOD;
	wbflush();
	if ((addr->stat & ALIVE) == 0) {
		if (ctlr->ptr->dead)
			printf("avdk%d: restarted\n", ctlr - avdkctlr);
		return 1;
	}
	ctlr->ptr->dead = 1;
	return 0;
}

avdkedtinit(struct edt *e)
{
	struct dkctlr *ctlr;

	if (e->e_intr_info->v_unit >= avdkctlrcnt)
		return;
	ctlr = &avdkctlr[e->e_intr_info->v_unit];
	ctlr->addr = (struct device *)e->e_base;
	ctlr->ptr = 0;
	if (badaddr(&ctlr->addr->ctl, sizeof(unsigned short)))
		return;
	/*
	if ((ctlr->ptr = (struct ptr *)kern_malloc(sizeof(struct ptr))) == 0)
		return;
	 */
	ctlr->ptr = &ptr[e->e_intr_info->v_unit];
	ctlr->ptr->dead = 0;
	for (ctlr->maxchno = 0; ctlr->maxchno < avdkmaxchno; ctlr->maxchno++) {
		ctlr->chan[ctlr->maxchno].chno = ctlr->maxchno;
		ctlr->chan[ctlr->maxchno].ctlr = ctlr;
	}
	ctlr->curchan = 0;
	ctlr->rxbp = 0;
	ctlr->addr->vector = e->e_intr_info->v_vec;
	reset(ctlr);
}

static int
alive(struct dkctlr *ctlr)
{
	struct device *addr = ctlr->addr;

	if ((addr->stat & ALIVE) == 0)
		return 1;
	if ((addr->ctl & IENABLE) == 0) {
		while ((addr->rxfifo & RXMT) == 0)
			;
		addr->ctl = RXIE|FAIL|IENABLE;
		wbflush();
	}
	return 0;
}

static struct dkctlr *
dev2ctlr(dev_t dev)
{
	int i;

	for (i = 0; i < avdkctlrcnt; i++) {
		if (avdkemajor[i] == emajor(dev))
			return avdkctlr[i].ptr ? &avdkctlr[i]: 0;
	}
	return 0;
}

static int
open(queue_t *q, dev_t dev, int flag, int sflag)
{
	struct dkctlr *ctlr;
	mblk_t *bp;
	unsigned chno;

	if (sflag || (ctlr = dev2ctlr(dev)) == 0)
		return OPENFAIL;
	if (minor(dev) >= ctlr->maxchno)
		return OPENFAIL;
	if (alive(ctlr) && reset(ctlr))
		return OPENFAIL;
	if (ctlr->module == 0) {
		if ((ctlr->module = dkmodall(dev, 0, ctlr->maxchno)) == 0)
			return OPENFAIL;
		ctlr->module->dkstate = ctlr->state;
	}
	chno = minor(dev);
	if (ctlr->state[chno] != DKCLOSED) {
		if (ctlr->chan[chno].exclusive)
			return OPENFAIL;
		if (ctlr->state[chno] != DKOPEN)
			return OPENFAIL;
		return chno;
	}
	ctlr->chan[chno].q = q;
	q->q_ptr = (char *)&ctlr->chan[chno];
	WR(q)->q_ptr = q->q_ptr;
	ctlr->chan[chno].exclusive = 1;
	if (bp = allocb(sizeof(struct stroptions), BPRI_LO)) {
		struct stroptions *s = (struct stroptions *)bp->b_rptr;

		bp->b_datap->db_type = M_SETOPTS;
		bp->b_wptr += sizeof(struct stroptions);
		s->so_flags = SO_READOPT|SO_HIWAT|SO_LOWAT;
		s->so_readopt = RMSGN;
		s->so_hiwat = 32*1024;
		s->so_lowat = 8*1024;
		putnext(q, bp);
	}
	ctlr->state[chno] = DKOPEN;
	return chno;
}

static int
close(queue_t *q)
{
	struct dkctlr *ctlr = ((struct chan *)q->q_ptr)->ctlr;
	unsigned chno = ((struct chan *)q->q_ptr)->chno;

	ctlr->chan[chno].q = 0;
	ctlr->chan[chno].exclusive = 0;
	if (ctlr->state[chno] == DKRCLOSE || ctlr->module->listnrq == 0)
		ctlr->state[chno] = DKCLOSED;
	else if (ctlr->state[chno] == DKOPEN)
		ctlr->state[chno] = DKLCLOSE;
	if (ctlr->module->listnrq)
		putctl2(RD(ctlr->module->listnrq), M_PCPROTO, DKMCLOSE, chno);
}

#define TXSPIN(thing, ctr)	{	int x = 50000;			\
									\
					wbflush();			\
					while ((thing) && x--)		\
						avdkstat.spin[ctr]++;	\
				}

#define TXSTUFF(where, data)	{	where = data;			}

static int
wput(queue_t *q, mblk_t *mp)
{
	struct dkctlr *ctlr = ((struct chan *)q->q_ptr)->ctlr;
	unsigned chno = ((struct chan *)q->q_ptr)->chno;
	struct device *addr = ctlr->addr;
	mblk_t *bp;
	int n, xhf;

	switch ((int)mp->b_datap->db_type) {

	case M_CTL:
	case M_DATA:
		TXSPIN((addr->stat & CPMT) == 0, 0);
		TXSTUFF(addr->txfifo, CHNO|chno);
		n = MAXTXBURST;
		for (bp = mp; bp; bp = bp->b_cont) {
			if(bp->b_rptr >= bp->b_wptr)
				continue;
			dkstat.output += bp->b_wptr - bp->b_rptr;
			if (n <= 0) {
				TXSPIN((addr->stat & TXRDY) == 0, 1);
				TXSTUFF(addr->txfifo, TXEOD);
				TXSPIN((addr->stat & CPMT) == 0, 2);
				TXSTUFF(addr->txfifo, CHNO|chno);
				n = MAXTXBURST;
			}
			n--;
			TXSPIN((addr->stat & TXRDY) == 0, 3);
			if (bp->b_datap->db_type == M_CTL) {
				TXSTUFF(addr->txfifo, CTL|*bp->b_rptr++);
			}
			else {
				TXSTUFF(addr->txfifo, *bp->b_rptr++);
			}
			while (bp->b_rptr < bp->b_wptr) {
				if (n <= 0) {
					TXSPIN((addr->stat & TXRDY) == 0, 4);
					TXSTUFF(addr->txfifo, TXEOD);
					TXSPIN((addr->stat & CPMT) == 0, 5);
					TXSTUFF(addr->txfifo, CHNO|chno);
					n = MAXTXBURST;
				}
				n--;
				TXSPIN((addr->stat & TXRDY) == 0, 6);
				TXSTUFF(addr->txfifo, *bp->b_rptr++);
			}
		}
		TXSPIN((addr->stat & TXRDY) == 0, 7);
		TXSTUFF(addr->txfifo, TXEOD);
		freemsg(mp);
		break;

	case M_PCPROTO:
		switch (*mp->b_rptr) {

		case DKMCLOSE:
			if ((chno = mp->b_rptr[1]) < ctlr->maxchno) {
				if (ctlr->state[chno] == DKOPEN) {
					ctlr->state[chno] = DKRCLOSE;
					putctl(ctlr->chan[chno].q->q_next, M_HANGUP);
				}
				else if (ctlr->state[chno] == DKLCLOSE)
					ctlr->state[chno] = DKCLOSED;
			}
			break;

		case DKMXINIT:
			chno = mp->b_rptr[1];
			if (chno < ctlr->maxchno && ctlr->state[chno] == DKOPEN) {
				putnext(ctlr->chan[chno].q, mp);
				return;
			}
			break;

		case DKMBUFFER:
			ctlr->chan[chno].buffer = mp->b_rptr[1];
			break;
		}
		freemsg(mp);
		break;

	case M_IOCTL:
		if (((struct iocblk *)mp->b_rptr)->ioc_cmd == DIOCNXCL) {
			ctlr->chan[chno].exclusive = 0;
			((struct iocblk *)mp->b_rptr)->ioc_count = 0;
			mp->b_datap->db_type = M_IOCACK;
		}
		else
			mp->b_datap->db_type = M_IOCNAK;
		qreply(q, mp);
		break;

	default:
		printf("avdkwput: type %d\n", mp->b_datap->db_type);
		/*FALLTHROUGH*/
	case M_FLUSH:
		freemsg(mp);
		break;
	}
}


static void
__putnext(queue_t *q, mblk_t *mp)
{
	if (q && canput(q->q_next)) {
		putnext(q, mp);
		return;
	}
	freemsg(mp);
	if (q == 0)
		avdkstat.noqueue++;
	else
		avdkstat.blocked++;
}

static mblk_t *
__allocb(unsigned data)
{
	mblk_t *bp;
	unsigned size = /*(data & CTL) ? 16:*/ 256;

	if ((bp = allocb(size, BPRI_MED)) == 0) {
		if ((bp = allocb(4*size, BPRI_MED)) == 0)
			avdkstat.noallocb++;
	}
	return bp;
}

static void
interrupt(struct dkctlr *ctlr)
{
	struct device *addr;
	struct chan *chan;
	unsigned data, input;
	mblk_t *bp;

	addr = ctlr->addr;
	avdkstat.rintr++;
	addr->ctl &= ~RXIE;
	bp = ctlr->rxbp;
	chan = ctlr->curchan;
	input = 0;
	while (((data = addr->rxfifo) & RXMT) == 0) {
		if (data & PERR) {
			dkstat.parity++;
			continue;
		}
		if ((data & CHNO) == 0) {
			if (chan == 0) {
				dkstat.markflt++;
				continue;
			}
			if ((data & (CTL+0xFF)) == CTL)
				continue;
			if (bp == 0 && (bp = __allocb(data)) == 0)
				continue;
			if (data & CTL) {
				if (bp->b_wptr > bp->b_rptr) {
					input += bp->b_wptr - bp->b_rptr;
					__putnext(chan->q, bp);
					if ((bp = __allocb(data)) == 0)
						continue;
				}
				bp->b_datap->db_type = M_CTL;
			}
			else if (bp->b_wptr >= bp->b_datap->db_lim) {
				avdkstat.extrab++;
				input += bp->b_wptr - bp->b_rptr;
				__putnext(chan->q, bp);
				if ((bp = allocb(data)) == 0)
					continue;
			}
			*bp->b_wptr++ = data & 0xFF;
			continue;
		}
		data &= 0x1FF;
		if (chan && chan->chno == data)
			continue;
		if (bp) {
			input += bp->b_wptr - bp->b_rptr;
			__putnext(chan->q, bp);
			bp = 0;
		}
		chan = 0;
		if (data == 0)
			dkstat.pack0++;
		else if (data >= ctlr->maxchno)
			dkstat.packstrange++;
		else if (ctlr->chan[data].q == 0) {
			if (ctlr->module && ctlr->module->listnrq)
				putctl2(RD(ctlr->module->listnrq),
					M_PCPROTO, DKMCLOSE, data);
			dkstat.closepack++;
		}
		else
			chan = &ctlr->chan[data];
	}
	if (bp && (chan->buffer == 0 || bp->b_datap->db_type == M_CTL)) {
		input += bp->b_wptr - bp->b_rptr;
		__putnext(chan->q, bp);
		bp = 0;
	}
	dkstat.input += input;
	ctlr->rxbp = bp;
	ctlr->curchan = chan;
	addr->ctl |= RXIE;
}

avdkintr0(ctlr)
{
}

avdkintr1(ctlr)
{
}

avdkintr2(ctlr)
{
}

void
avdkintr3(int ctlrno)
{
	if (ctlrno >= avdkctlrcnt)
		return;
	interrupt(&avdkctlr[ctlrno]);
}
