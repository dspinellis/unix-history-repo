/*	if_dmc.c	6.3	84/09/27	*/

#include "dmc.h"
#if NDMC > 0
#define printd if(dmcdebug)printf
int dmcdebug = 0;
/*
 * DMC11 device driver, internet version
 *
 *	Bill Nesheim	(bill@cornell.arpa or {vax135,uw-beaver,ihnp4}!bill)
 *	Cornell University
 *	Department of Computer Science
 *
 *	Based loosly on 4.2BSD release
 *	The UNIBUS support routines were taken from Lou Salkind's DEUNA driver
 *
 * TO DO:
 *	generalize unibus routines
 *	add timeout to mark interface down when other end of link dies
 *	figure out better way to check for completed buffers
 *	(not critical with DMC, only 7 bufs, but may cause problems
 *		on a DMR)
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "ioctl.h"		/* must precede tty.h */
#include "tty.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_uba.h"
#include "if_dmc.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

/*
 * Driver information for auto-configuration stuff.
 */
int	dmcprobe(), dmcattach(), dmcinit(), dmcioctl();
int	dmcoutput(), dmcreset();
struct	uba_device *dmcinfo[NDMC];
u_short	dmcstd[] = { 0 };
struct	uba_driver dmcdriver =
	{ dmcprobe, 0, dmcattach, 0, dmcstd, "dmc", dmcinfo };

/* as long as we use clists for command queues, we only have 28 bytes to use! */
/* DMC-11 only has 7 buffers; DMR-11 has 64 */
#define NRCV 7
#define NXMT (NRCV - 2)	/* avoid running out of buffers on recv end */ 
#define NTOT (NRCV + NXMT)

/* error reporting intervals */
#define DMC_RPNBFS	50
#define DMC_RPDSC	1
#define DMC_RPTMO	20
#define DMC_RPDCK	5

struct  dmc_command {
	char	qp_cmd;		/* command */
	short	qp_ubaddr;	/* buffer address */
	short	qp_cc;		/* character count || XMEM */
	struct	dmc_command *qp_next;	/* next command on queue */
};

/*
 * The dmcuba structures generalize the ifuba structure
 * to an arbitrary number of recieve and transmit buffers.
 */
struct	ifxmt {
	struct	ifrw x_ifrw;		/* mapping imfo */
	struct	pte x_map[IF_MAXNUBAMR];	/* output base pages */
	short 	x_xswapd;		/* mask of clusters swapped */
	struct	mbuf *x_xtofree;	/* pages being dma'd out */
};
struct	dmcuba {
	short	ifu_uban;		/* uba number */
	short	ifu_hlen;		/* local net header length */
	struct	uba_regs *ifu_uba;	/* uba regs, in vm */
	struct	ifrw ifu_r[NRCV];	/* receive information */
	struct	ifxmt ifu_w[NXMT];	/* transmit information */
				/* these should only be pointers */
	short	ifu_flags;		/* used during uballoc's */
};

struct dmcbufs {
	int	ubinfo;		/* from uballoc */
	short	cc;		/* buffer size */
	short	flags;		/* access control */
};
#define	DBUF_OURS	0	/* buffer is available */
#define	DBUF_DMCS	1	/* buffer claimed by somebody */
#define	DBUF_XMIT	4	/* transmit buffer */
#define	DBUF_RCV	8	/* recieve buffer */

struct mbuf *dmc_get();

/*
 * DMC software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * sc_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a  set of 7 UBA interface structures
 * for each, which
 * contain information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct dmc_softc {
	short	sc_oused;		/* output buffers currently in use */
	short	sc_iused;		/* input buffers given to DMC */
	short	sc_flag;		/* flags */
	struct	ifnet sc_if;		/* network-visible interface */
	struct	dmcbufs sc_rbufs[NRCV];	/* recieve buffer info */
	struct	dmcbufs sc_xbufs[NXMT];	/* transmit buffer info */
	struct	dmcuba sc_ifuba;	/* UNIBUS resources */
	int	sc_ubinfo;		/* UBA mapping info for base table */
	int	sc_errors[4];		/* non-fatal error counters */
#define sc_datck sc_errors[0]
#define sc_timeo sc_errors[1]
#define sc_nobuf sc_errors[2]
#define sc_disc  sc_errors[3]
	/* command queue stuff */
	struct	dmc_command sc_cmdbuf[NTOT+3];
	struct	dmc_command *sc_qhead;	/* head of command queue */
	struct	dmc_command *sc_qtail;	/* tail of command queue */
	struct	dmc_command *sc_qactive;	/* command in progress */
	struct	dmc_command *sc_qfreeh;	/* head of list of free cmd buffers */
	struct	dmc_command *sc_qfreet;	/* tail of list of free cmd buffers */
	/* end command queue stuff */
} dmc_softc[NDMC];

/* flags */
#define	DMC_ALLOC	01	/* unibus resources allocated */
#define	DMC_BMAPPED	02	/* base table mapped */

struct	dmc_base {
	short	d_base[128];	/* DMC base table */
} dmc_base[NDMC];

/* queue manipulation macros */
#define	QUEUE_AT_HEAD(qp, head, tail) \
	(qp)->qp_next = (head); \
	(head) = (qp); \
	if ((tail) == (struct dmc_command *) 0) \
		(tail) = (head) 

#define QUEUE_AT_TAIL(qp, head, tail) \
	if ((tail)) \
		(tail)->qp_next = (qp); \
	else \
		(head) = (qp); \
	(qp)->qp_next = (struct dmc_command *) 0; \
	(tail) = (qp)

#define DEQUEUE(head, tail) \
	(head) = (head)->qp_next;\
	if ((head) == (struct dmc_command *) 0)\
		(tail) = (head)

dmcprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct dmcdevice *addr = (struct dmcdevice *)reg;
	register int i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dmcrint(0); dmcxint(0);
#endif
	addr->bsel1 = DMC_MCLR;
	for (i = 100000; i && (addr->bsel1 & DMC_RUN) == 0; i--)
		;
	if ((addr->bsel1 & DMC_RUN) == 0)
		return (0);
	/* MCLR is self clearing */
	addr->bsel0 = DMC_RQI|DMC_IEI;
	DELAY(100000);
	addr->bsel1 = DMC_MCLR;
	for (i = 100000; i && (addr->bsel1 & DMC_RUN) == 0; i--)
		;
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
dmcattach(ui)
	register struct uba_device *ui;
{
	register struct dmc_softc *sc = &dmc_softc[ui->ui_unit];
	register struct dmc_command *qp;

	sc->sc_if.if_unit = ui->ui_unit;
	sc->sc_if.if_name = "dmc";
	sc->sc_if.if_mtu = DMCMTU;
	sc->sc_if.if_init = dmcinit;
	sc->sc_if.if_output = dmcoutput;
	sc->sc_if.if_ioctl = dmcioctl;
	sc->sc_if.if_reset = dmcreset;
	sc->sc_if.if_flags = IFF_POINTOPOINT;
	sc->sc_ifuba.ifu_flags = UBA_CANTWAIT;

	/* set up command queues */
	sc->sc_qfreeh = sc->sc_qfreet =
		sc->sc_qhead = sc->sc_qtail = sc->sc_qactive =
		(struct dmc_command *) 0;
	/* set up free command buffer list */
	for (qp = &sc->sc_cmdbuf[0]; qp < &sc->sc_cmdbuf[NTOT+2]; qp++ ) {
		QUEUE_AT_HEAD( qp, sc->sc_qfreeh, sc->sc_qfreet);
	}
	if_attach(&sc->sc_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified UBA, reset it's state.
 */
dmcreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
	register struct dmc_softc *sc = &dmc_softc[unit];

	if (unit >= NDMC || (ui = dmcinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" dmc%d", unit);
	sc->sc_flag = 0;	/* previous unibus resources no longer valid */
	dmcinit(unit);
}

/*
 * Initialization of interface; reinitialize UNIBUS usage.
 */
dmcinit(unit)
	int unit;
{
	register struct dmc_softc *sc = &dmc_softc[unit];
	register struct uba_device *ui = dmcinfo[unit];
	register struct dmcdevice *addr;
	register struct ifnet *ifp = &sc->sc_if;
	register struct ifrw *ifrw;
	register struct ifxmt *ifxp;
	register struct dmcbufs *rp;
	int base;
	struct sockaddr_in *sin;

	printd("dmcinit\n");
	addr = (struct dmcdevice *)ui->ui_addr;

	sin = (struct sockaddr_in *) &ifp->if_addr;
	if (sin->sin_addr.s_addr == 0)	/* if address still unknown */
		return;
	sin = (struct sockaddr_in *) &ifp->if_dstaddr;
	if (sin->sin_addr.s_addr == 0)	/* if address still unknown */
		return;

	if ((addr->bsel1&DMC_RUN) == 0) {
		printf("dmcinit: DMC not running\n");
		ifp->if_flags &= ~(IFF_RUNNING|IFF_UP);
		return;
	}
	/* map base table */
	if ((sc->sc_flag&DMC_BMAPPED) == 0) {
		sc->sc_ubinfo = uballoc(ui->ui_ubanum,
			(caddr_t)&dmc_base[unit], sizeof (struct dmc_base), 0);
		sc->sc_flag |= DMC_BMAPPED;
	}
	/* initialize UNIBUS resources */
	sc->sc_iused = sc->sc_oused = 0;
	if ((sc->sc_flag&DMC_ALLOC) == 0) {
		if (dmc_ubainit(&sc->sc_ifuba, ui->ui_ubanum, 0,
				(int)btoc(DMCMTU)) == 0) {
			printf("dmc%d: can't initialize\n", unit);
			ifp->if_flags &= ~IFF_UP;
			return;
		}
		sc->sc_flag |= DMC_ALLOC;
	}

	/* initialize buffer pool */
	/* recieves */
	ifrw = &sc->sc_ifuba.ifu_r[0];
	for (rp = &sc->sc_rbufs[0]; rp < &sc->sc_rbufs[NRCV]; rp++) {
		rp->ubinfo = ifrw->ifrw_info & 0x3ffff;
		rp->cc = DMCMTU;
		rp->flags = DBUF_OURS|DBUF_RCV;
		printd("rcv: 0x%x\n",rp->ubinfo);
		ifrw++; 
	}
	/* transmits */
	ifxp = &sc->sc_ifuba.ifu_w[0];
	for (rp = &sc->sc_xbufs[0]; rp < &sc->sc_xbufs[NXMT]; rp++) {
		rp->ubinfo = ifxp->x_ifrw.ifrw_info & 0x3ffff;
		rp->cc = 0;
		rp->flags = DBUF_OURS|DBUF_XMIT;
		printd("xmit: 0x%x\n",rp->ubinfo);
		ifxp++; 
	}
	/* base in */
	base = sc->sc_ubinfo & 0x3ffff;
	printd("  base 0x%x\n", base);
	dmcload(sc, DMC_BASEI, base, (base>>2)&DMC_XMEM);
	/* specify half duplex operation, flags tell if primary */
	/* or secondary station */
	if (ui->ui_flags == 0)
		       /* use DDMCP mode in full duplex */
			dmcload(sc, DMC_CNTLI, 0, 0);
	else if (ui->ui_flags == 1)
		       /* use MAINTENENCE mode */
		       dmcload(sc, DMC_CNTLI, 0, DMC_MAINT );
	else if (ui->ui_flags == 2)
		/* use DDCMP half duplex as primary station */
		dmcload(sc, DMC_CNTLI, 0, DMC_HDPLX);
	else if (ui->ui_flags == 3)
		/* use DDCMP half duplex as secondary station */
		dmcload(sc, DMC_CNTLI, 0, DMC_HDPLX | DMC_SEC);
	
	/* queue first NRCV buffers for DMC to fill */
	for (rp = &sc->sc_rbufs[0]; rp < &sc->sc_rbufs[NRCV]; rp++) {
		rp->flags |= DBUF_DMCS;
		dmcload(sc, DMC_READ, rp->ubinfo,
			(((rp->ubinfo>>2)&DMC_XMEM)|rp->cc));
		sc->sc_iused++;
	}

	/* enable output interrupts */
	while ((addr->bsel2&DMC_IEO) == 0)
		addr->bsel2 |= DMC_IEO;
	ifp->if_flags |= IFF_UP|IFF_RUNNING;
}

/*
 * Start output on interface.  Get another datagram
 * to send from the interface queue and map it to
 * the interface before starting output.
 *
 * Must be called at spl 5
 */
dmcstart(dev)
	dev_t dev;
{
	int unit = minor(dev);
	register struct dmc_softc *sc = &dmc_softc[unit];
	struct mbuf *m;
	register struct dmcbufs *rp;
	register int n;

	if ((sc->sc_flag & DMC_ALLOC) == 0) {
		printf("dmcstart: no unibus resources!!\n");
		return;
	}
	/*
	 * Dequeue up to NXMT requests and map them to the UNIBUS.
	 * If no more requests, or no dmc buffers available, just return.
	 */
	n = 0;
	for (rp = &sc->sc_xbufs[0]; rp < &sc->sc_xbufs[NXMT]; rp++ ) {
		/* find an available buffer */
		if ((rp->flags&DBUF_DMCS) == 0){
			IF_DEQUEUE(&sc->sc_if.if_snd, m);
			if (m == 0)
				return;
			if ((rp->flags&DBUF_XMIT) == 0)
				printf("dmcstart: not xmit buf\n");
			/* mark it dmcs */
			rp->flags |= (DBUF_DMCS);
			/*
			 * Have request mapped to UNIBUS for transmission
			 * and start the output.
			 */
			rp->cc = (dmcput(&sc->sc_ifuba, n, m))&DMC_CCOUNT;
			sc->sc_oused++;
			dmcload(sc, DMC_WRITE, rp->ubinfo, 
				rp->cc | ((rp->ubinfo>>2)&DMC_XMEM));
		}
		n++;
	}
}

/*
 * Utility routine to load the DMC device registers.
 */
dmcload(sc, type, w0, w1)
	register struct dmc_softc *sc;
	int type, w0, w1;
{
	register struct dmcdevice *addr;
	register int unit, sps;
	register struct dmc_command *qp;

	unit = (sc - dmc_softc)/ sizeof (struct dmc_softc);
	addr = (struct dmcdevice *)dmcinfo[unit]->ui_addr;
	sps = spl5();

	/* grab a command buffer from the free list */
	if ((qp = sc->sc_qfreeh) == (struct dmc_command *)0)
		panic("dmc command queue overflow");
	DEQUEUE(sc->sc_qfreeh, sc->sc_qfreet);

	/* fill in requested info */
	qp->qp_cmd = (type | DMC_RQI);
	qp->qp_ubaddr = w0;
	qp->qp_cc = w1;
	
	if (sc->sc_qactive) {	/* command in progress */
		if (type == DMC_READ) {
			QUEUE_AT_HEAD(qp, sc->sc_qhead, sc->sc_qtail);
		} else {
			QUEUE_AT_TAIL(qp, sc->sc_qhead, sc->sc_qtail);
		}
	} else {	/* command port free */
		sc->sc_qactive = qp;
		addr->bsel0 = qp->qp_cmd;
		dmcrint(unit);
	}
	splx(sps);
}

/*
 * DMC interface receiver interrupt.
 * Ready to accept another command,
 * pull one off the command queue.
 */
dmcrint(unit)
	int unit;
{
	register struct dmc_softc *sc;
	register struct dmcdevice *addr;
	register struct dmc_command *qp;
	register int n;

	addr = (struct dmcdevice *)dmcinfo[unit]->ui_addr;
	sc = &dmc_softc[unit];
	if ((qp = sc->sc_qactive) == (struct dmc_command *) 0) {
		printf("dmcrint: no command\n");
		return;
	}
	while (addr->bsel0&DMC_RDYI) {
		addr->sel4 = qp->qp_ubaddr;
		addr->sel6 = qp->qp_cc;
		addr->bsel0 &= ~(DMC_IEI|DMC_RQI);
		printd("load done, cmd 0x%x, ubaddr 0x%x, cc 0x%x\n",
			qp->qp_cmd, qp->qp_ubaddr, qp->qp_cc);
		/* free command buffer */
		QUEUE_AT_HEAD(qp, sc->sc_qfreeh, sc->sc_qfreet);
		while (addr->bsel0 & DMC_RDYI) {
			/*
			 * Can't check for RDYO here 'cause
			 * this routine isn't reentrant!
			 */
			DELAY(5);
		}
		/* move on to next command */
		if ((sc->sc_qactive = sc->sc_qhead)==(struct dmc_command *) 0) 
			/* all done */
			break;
		/* more commands to do, start the next one */
		qp = sc->sc_qactive;
		DEQUEUE(sc->sc_qhead, sc->sc_qtail);
		addr->bsel0 = qp->qp_cmd;
		n = RDYSCAN;
		while (n-- && (addr->bsel0&DMC_RDYI) == 0)
			DELAY(5);
	}
	if (sc->sc_qactive) {
		addr->bsel0 |= DMC_IEI|DMC_RQI;
		/* VMS does it twice !*$%@# */
		addr->bsel0 |= DMC_IEI|DMC_RQI;
	}
}

/*
 * DMC interface transmitter interrupt.
 * A transfer may have completed, check for errors.
 * If it was a read, notify appropriate protocol.
 * If it was a write, pull the next one off the queue.
 */
dmcxint(unit)
	int unit;
{
	register struct dmc_softc *sc;
	register struct ifnet *ifp;
	struct uba_device *ui = dmcinfo[unit];
	struct dmcdevice *addr;
	struct mbuf *m;
	register struct ifqueue *inq;
	int arg, pkaddr, cmd, len;
	register struct ifrw *ifrw;
	register struct dmcbufs *rp;

	addr = (struct dmcdevice *)ui->ui_addr;
	sc = &dmc_softc[unit];
	ifp = &sc->sc_if;

	cmd = addr->bsel2 & 0xff;
	arg = addr->sel6 & 0xffff;
	if ((cmd&DMC_RDYO) == 0)  {
		printf("dmc%d: bogus xmit intr\n", unit);
		return; 
	}
	/* reconstruct UNIBUS address of buffer returned to us */
	pkaddr = ((arg&DMC_XMEM)<<2)|(addr->sel4 & 0xffff);
	/* release port */
	addr->bsel2 &= ~DMC_RDYO;
	switch (cmd & 07) {

	case DMC_OUR:
		/*
		 * A read has completed.  
		 * Pass packet to type specific
		 * higher-level input routine.
		 */
		ifp->if_ipackets++;
		len = arg & DMC_CCOUNT;
		/* find location in dmcuba struct */
		ifrw = &sc->sc_ifuba.ifu_r[0];
		rp = &sc->sc_rbufs[0];
		for (; rp < &sc->sc_rbufs[NRCV]; rp++) {
			if (rp->ubinfo == pkaddr)
				goto foundrcv;
			ifrw++;
		}
		printf("bad rcv pkt addr 0x%x len 0x%x\n", pkaddr, len);
		goto setup;
		
	foundrcv:
		if ((rp->flags&DBUF_DMCS) == 0) {
			printf("dmcxint: done unalloc rbuf\n");
		}
		switch (ifp->if_addr.sa_family) {
#ifdef INET
		case AF_INET:
			schednetisr(NETISR_IP);
			inq = &ipintrq;
			break;
#endif

		default:
			printf("dmc%d: unknown address type %d\n", unit,
			    ifp->if_addr.sa_family);
			goto setup;
		}

		m = dmc_get(&sc->sc_ifuba, ifrw, len, 0);
		if (m == (struct mbuf *)0)
			goto setup;
		if (IF_QFULL(inq)) {
			IF_DROP(inq);
			m_freem(m);
		} else
			IF_ENQUEUE(inq, m);
setup:
		arg = ifrw->ifrw_info & 0x3ffff;
		dmcload(sc, DMC_READ, arg, ((arg >> 2) & DMC_XMEM) | DMCMTU);
		break;

	case DMC_OUX:
		/*
		 * A write has completed, start another
		 * transfer if there is more data to send.
		 */
		ifp->if_opackets++;
		printd("OUX pkaddr 0x%x\n",pkaddr);
		/* find associated dmcbuf structure */
		rp = &sc->sc_xbufs[0];
		for (; rp < &sc->sc_xbufs[NXMT]; rp++) {
			if (rp->ubinfo == pkaddr)
				goto found;
		}
		printf("dmc%d: bad packet address 0x%x\n",
			unit, pkaddr);
		break;
	found:
		if ((rp->flags&DBUF_DMCS) == 0)
			printf("dmc returned unallocated packet 0x%x\n",
				pkaddr);
		/* mark buffer free */
		rp->flags &= ~(DBUF_DMCS);
		sc->sc_oused--;
		dmcstart(unit);
		break;

	case DMC_CNTLO:
		arg &= DMC_CNTMASK;
		if (arg&DMC_FATAL) {
			register int i;

			printf("dmc%d: fatal error, flags=%b\n",
			    unit, arg, CNTLO_BITS);
			ifp->if_flags &= ~(IFF_RUNNING|IFF_UP);
			/* master clear device */
			addr->bsel1 = DMC_MCLR;
			for (i = 100000; i && (addr->bsel1 & DMC_RUN) == 0; i--)
				;
			dmcinit(unit);
			ifp->if_ierrors++;
			break;
		} else {
                        /* ACCUMULATE STATISTICS */
			switch(arg) {
			case DMC_NOBUFS:
                        	ifp->if_ierrors++;
				if((sc->sc_nobuf++ % DMC_RPNBFS) != 0)
					break;
				goto report;
			case DMC_DISCONN:
				if((sc->sc_disc++ % DMC_RPDSC) != 0)
					break;
				goto report;
			case DMC_TIMEOUT:
				if((sc->sc_timeo++ % DMC_RPTMO) != 0)
					break;
				goto report;
			case DMC_DATACK:
                        	ifp->if_oerrors++;
				if((sc->sc_datck++ % DMC_RPDCK) != 0)
					break;
				goto report;
			default:
				goto report;
			}
			break;
		report:
                        printf("dmc%d: soft error, flags=%b\n",
                            unit, arg, CNTLO_BITS);
		}
		break;

	default:
		printf("dmc%d: bad control %o\n", unit, cmd);
	}
	return;
}

/*
 * DMC output routine.
 * Just send the data, header was supplied by
 * upper level protocol routines.
 */
dmcoutput(ifp, m, dst)
	register struct ifnet *ifp;
	register struct mbuf *m;
	struct sockaddr *dst;
{
	int s;

	if (dst->sa_family != ifp->if_addr.sa_family) {
		printf("dmc%d: af%d not supported\n", ifp->if_unit,
		    dst->sa_family);
		m_freem(m);
		return (EAFNOSUPPORT);
	}
	s = spl5();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		m_freem(m);
		splx(s);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	dmcstart(ifp->if_unit);
	splx(s);
	return (0);
}

/*
 * Process an ioctl request.
 */
dmcioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct ifreq *ifr = (struct ifreq *)data;
	struct sockaddr_in *sin;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifp->if_flags & IFF_RUNNING)
			if_rtinit(ifp, -1);     /* delete previous route */
		sin = (struct sockaddr_in *)&ifr->ifr_addr;
		ifp->if_addr = *(struct sockaddr *)sin;
		ifp->if_net = in_netof(sin->sin_addr);
		ifp->if_flags |= IFF_UP;
		/* set up routing table entry */
		if ((ifp->if_flags & IFF_ROUTE) == 0) {
			rtinit(&ifp->if_dstaddr, &ifp->if_addr, RTF_HOST|RTF_UP);
			ifp->if_flags |= IFF_ROUTE;
		}
		break;

	case SIOCSIFDSTADDR:
		ifp->if_dstaddr = ifr->ifr_dstaddr;
		break;
		
	default:
		error = EINVAL;
	}
	if ((ifp->if_flags & IFF_RUNNING) == 0)
		dmcinit(ifp->if_unit); 
	splx(s);
	return (error);
}

/*
 * Routines supporting UNIBUS network interfaces.
 */

/*
 * Init UNIBUS for interface on uban whose headers of size hlen are to
 * end on a page boundary.  We allocate a UNIBUS map register for the page
 * with the header, and nmr more UNIBUS map registers for i/o on the adapter,
 * doing this for each receive and transmit buffer.  We also
 * allocate page frames in the mbuffer pool for these pages.
 */
dmc_ubainit(ifu, uban, hlen, nmr)
	register struct dmcuba *ifu;
	int uban, hlen, nmr;
{
	register caddr_t cp, dp;
	register struct ifrw *ifrw;
	register struct ifxmt *ifxp;
	int i, ncl;

	ncl = clrnd(nmr + CLSIZE) / CLSIZE;
	if (ifu->ifu_r[0].ifrw_addr) {
		/*
		 * If the first read buffer has a non-zero
		 * address, it means we have already allocated core
		 */
		cp = ifu->ifu_r[0].ifrw_addr - (CLBYTES - hlen);
	} else {
		cp = m_clalloc(NTOT * ncl, MPG_SPACE);
		if (cp == 0)
			return (0);
		ifu->ifu_hlen = hlen;
		ifu->ifu_uban = uban;
		ifu->ifu_uba = uba_hd[uban].uh_uba;
		dp = cp + CLBYTES - hlen;
		for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++) {
			ifrw->ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
		for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[NXMT]; ifxp++) {
			ifxp->x_ifrw.ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
	}
	/* allocate for receive ring */
	for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++) {
		if (dmc_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifrw *rw;

			for (rw = ifu->ifu_r; rw < ifrw; rw++)
				ubarelse(ifu->ifu_uban, &rw->ifrw_info);
			goto bad;
		}
	}
	/* and now transmit ring */
	for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[NXMT]; ifxp++) {
		ifrw = &ifxp->x_ifrw;
		if (dmc_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifxmt *xp;

			for (xp = ifu->ifu_w; xp < ifxp; xp++)
				ubarelse(ifu->ifu_uban, &xp->x_ifrw.ifrw_info);
			for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++)
				ubarelse(ifu->ifu_uban, &ifrw->ifrw_info);
			goto bad;
		}
		for (i = 0; i < nmr; i++)
			ifxp->x_map[i] = ifrw->ifrw_mr[i];
		ifxp->x_xswapd = 0;
	}
	return (1);
bad:
	m_pgfree(cp, NTOT * ncl);
	ifu->ifu_r[0].ifrw_addr = 0;
	return (0);
}

/*
 * Setup either a ifrw structure by allocating UNIBUS map registers,
 * possibly a buffered data path, and initializing the fields of
 * the ifrw structure to minimize run-time overhead.
 */
static
dmc_ubaalloc(ifu, ifrw, nmr)
	struct dmcuba *ifu;
	register struct ifrw *ifrw;
	int nmr;
{
	register int info;

	info =
	    uballoc(ifu->ifu_uban, ifrw->ifrw_addr, nmr*NBPG + ifu->ifu_hlen,
		ifu->ifu_flags);
	if (info == 0)
		return (0);
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | (UBAI_BDP(info) << UBAMR_DPSHIFT);
	ifrw->ifrw_mr = &ifu->ifu_uba->uba_map[UBAI_MR(info) + 1];
	return (1);
}

/*
 * Pull read data off a interface.
 * Len is length of data, with local net header stripped.
 * Off is non-zero if a trailer protocol was used, and
 * gives the offset of the trailer information.
 * We copy the trailer information and then all the normal
 * data into mbufs.  When full cluster sized units are present
 * on the interface on cluster boundaries we can get them more
 * easily by remapping, and take advantage of this here.
 */
struct mbuf *
dmc_get(ifu, ifrw, totlen, off0)
	register struct dmcuba *ifu;
	register struct ifrw *ifrw;
	int totlen, off0;
{
	struct mbuf *top, **mp, *m;
	int off = off0, len;
	register caddr_t cp = ifrw->ifrw_addr + ifu->ifu_hlen;

	top = 0;
	mp = &top;
	while (totlen > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
			cp = ifrw->ifrw_addr + ifu->ifu_hlen + off;
		} else
			len = totlen;
		if (len >= CLBYTES) {
			struct mbuf *p;
			struct pte *cpte, *ppte;
			int x, *ip, i;

			MCLGET(p, 1);
			if (p == 0)
				goto nopage;
			len = m->m_len = CLBYTES;
			m->m_off = (int)p - (int)m;
			if (!claligned(cp))
				goto copy;

			/*
			 * Switch pages mapped to UNIBUS with new page p,
			 * as quick form of copy.  Remap UNIBUS and invalidate.
			 */
			cpte = &Mbmap[mtocl(cp)*CLSIZE];
			ppte = &Mbmap[mtocl(p)*CLSIZE];
			x = btop(cp - ifrw->ifrw_addr);
			ip = (int *)&ifrw->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte++ = *cpte; *cpte = t;
				*ip++ =
				    cpte++->pg_pfnum|ifrw->ifrw_proto;
				mtpr(TBIS, cp);
				cp += NBPG;
				mtpr(TBIS, (caddr_t)p);
				p += NBPG / sizeof (*p);
			}
			goto nocopy;
		}
nopage:
		m->m_len = MIN(MLEN, len);
		m->m_off = MMINOFF;
copy:
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
		cp += m->m_len;
nocopy:
		*mp = m;
		mp = &m->m_next;
		if (off) {
			/* sort of an ALGOL-W style for statement... */
			off += m->m_len;
			if (off == totlen) {
				cp = ifrw->ifrw_addr + ifu->ifu_hlen;
				off = 0;
				totlen = off0;
			}
		} else
			totlen -= m->m_len;
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Map a chain of mbufs onto a network interface
 * in preparation for an i/o operation.
 * The argument chain of mbufs includes the local network
 * header which is copied to be in the mapped, aligned
 * i/o space.
 */
dmcput(ifu, n, m)
	struct dmcuba *ifu;
	int n;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register caddr_t cp;
	register struct ifxmt *ifxp;
	register struct ifrw *ifrw;
	register int i;
	int xswapd = 0;
	int x, cc, t;
	caddr_t dp;

	ifxp = &ifu->ifu_w[n];
	ifrw = &ifxp->x_ifrw;
	cp = ifrw->ifrw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp) && m->m_len == CLBYTES) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)*CLSIZE];
			x = btop(cp - ifrw->ifrw_addr);
			ip = (int *)&ifrw->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++)
				*ip++ = ifrw->ifrw_proto | pte++->pg_pfnum;
			xswapd |= 1 << (x>>(CLSHIFT-PGSHIFT));
			mp = m->m_next;
			m->m_next = ifxp->x_xtofree;
			ifxp->x_xtofree = m;
			cp += m->m_len;
		} else {
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
			cp += m->m_len;
			MFREE(m, mp);
		}
		m = mp;
	}

	/*
	 * Xswapd is the set of clusters we just mapped out.  Ifxp->x_xswapd
	 * is the set of clusters mapped out from before.  We compute
	 * the number of clusters involved in this operation in x.
	 * Clusters mapped out before and involved in this operation
	 * should be unmapped so original pages will be accessed by the device.
	 */
	cc = cp - ifrw->ifrw_addr;
	x = ((cc - ifu->ifu_hlen) + CLBYTES - 1) >> CLSHIFT;
	ifxp->x_xswapd &= ~xswapd;
	while (i = ffs(ifxp->x_xswapd)) {
		i--;
		if (i >= x)
			break;
		ifxp->x_xswapd &= ~(1<<i);
		i *= CLSIZE;
		for (t = 0; t < CLSIZE; t++) {
			ifrw->ifrw_mr[i] = ifxp->x_map[i];
			i++;
		}
	}
	ifxp->x_xswapd |= xswapd;
	return (cc);
}
#endif
