/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_dmc.c	7.5 (Berkeley) %G%
 */

#include "dmc.h"
#if NDMC > 0

/*
 * DMC11 device driver, internet version
 *
 *	Bill Nesheim
 *	Cornell University
 *
 *	Lou Salkind
 *	New York University
 */

/* #define DEBUG	/* for base table dump on fatal error */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "ioctl.h"		/* must precede tty.h */
#include "tty.h"
#include "protosw.h"
#include "socket.h"
#include "syslog.h"
#include "vmmac.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef	INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#endif

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_uba.h"
#include "if_dmc.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#include "../h/time.h"
#include "../h/kernel.h"

/*
 * output timeout value, sec.; should depend on line speed.
 */
int	dmc_timeout = 20;

/*
 * Driver information for auto-configuration stuff.
 */
int	dmcprobe(), dmcattach(), dmcinit(), dmcioctl();
int	dmcoutput(), dmcreset(), dmctimeout();
struct	uba_device *dmcinfo[NDMC];
u_short	dmcstd[] = { 0 };
struct	uba_driver dmcdriver =
	{ dmcprobe, 0, dmcattach, 0, dmcstd, "dmc", dmcinfo };

#define NRCV 7
#define NXMT 3 
#define NCMDS	(NRCV+NXMT+4)	/* size of command queue */

#define printd if(dmcdebug)printf
int dmcdebug = 0;

/* error reporting intervals */
#define DMC_RPNBFS	50
#define DMC_RPDSC	1
#define DMC_RPTMO	10
#define DMC_RPDCK	10

struct  dmc_command {
	char	qp_cmd;		/* command */
	short	qp_ubaddr;	/* buffer address */
	short	qp_cc;		/* character count || XMEM */
	struct	dmc_command *qp_next;	/* next command on queue */
};

struct dmcbufs {
	int	ubinfo;		/* from uballoc */
	short	cc;		/* buffer size */
	short	flags;		/* access control */
};
#define	DBUF_OURS	0	/* buffer is available */
#define	DBUF_DMCS	1	/* buffer claimed by somebody */
#define	DBUF_XMIT	4	/* transmit buffer */
#define	DBUF_RCV	8	/* receive buffer */


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
	struct	ifnet sc_if;		/* network-visible interface */
	short	sc_oused;		/* output buffers currently in use */
	short	sc_iused;		/* input buffers given to DMC */
	short	sc_flag;		/* flags */
	int	sc_ubinfo;		/* UBA mapping info for base table */
	int	sc_errors[4];		/* non-fatal error counters */
#define sc_datck sc_errors[0]
#define sc_timeo sc_errors[1]
#define sc_nobuf sc_errors[2]
#define sc_disc  sc_errors[3]
	struct	dmcbufs sc_rbufs[NRCV];	/* receive buffer info */
	struct	dmcbufs sc_xbufs[NXMT];	/* transmit buffer info */
	struct	ifubinfo sc_ifuba;	/* UNIBUS resources */
	struct	ifrw sc_ifr[NRCV];	/* UNIBUS receive buffer maps */
	struct	ifxmt sc_ifw[NXMT];	/* UNIBUS receive buffer maps */
	/* command queue stuff */
	struct	dmc_command sc_cmdbuf[NCMDS];
	struct	dmc_command *sc_qhead;	/* head of command queue */
	struct	dmc_command *sc_qtail;	/* tail of command queue */
	struct	dmc_command *sc_qactive;	/* command in progress */
	struct	dmc_command *sc_qfreeh;	/* head of list of free cmd buffers */
	struct	dmc_command *sc_qfreet;	/* tail of list of free cmd buffers */
	/* end command queue stuff */
} dmc_softc[NDMC];

/* flags */
#define DMC_RUNNING	0x01		/* device initialized */
#define DMC_BMAPPED	0x02		/* base table mapped */
#define DMC_RESTART	0x04		/* software restart in progress */
#define DMC_ONLINE	0x08		/* device running (had a RDYO) */

struct dmc_base {
	short	d_base[128];		/* DMC base table */
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
	if ((addr->bsel1 & DMC_RUN) == 0) {
		printf("dmcprobe: can't start device\n" );
		return (0);
	}
	addr->bsel0 = DMC_RQI|DMC_IEI;
	/* let's be paranoid */
	addr->bsel0 |= DMC_RQI|DMC_IEI;
	DELAY(1000000);
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

	sc->sc_if.if_unit = ui->ui_unit;
	sc->sc_if.if_name = "dmc";
	sc->sc_if.if_mtu = DMCMTU;
	sc->sc_if.if_init = dmcinit;
	sc->sc_if.if_output = dmcoutput;
	sc->sc_if.if_ioctl = dmcioctl;
	sc->sc_if.if_reset = dmcreset;
	sc->sc_if.if_watchdog = dmctimeout;
	sc->sc_if.if_flags = IFF_POINTOPOINT;
	sc->sc_ifuba.iff_flags = UBA_CANTWAIT;

	if_attach(&sc->sc_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified UBA, reset its state.
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
	sc->sc_flag = 0;
	sc->sc_if.if_flags &= ~IFF_RUNNING;
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
	register struct dmc_command *qp;
	struct ifaddr *ifa;
	int base;
	int s;

	addr = (struct dmcdevice *)ui->ui_addr;

	/*
	 * Check to see that an address has been set
	 * (both local and destination for an address family).
	 */
	for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next)
		if (ifa->ifa_addr.sa_family && ifa->ifa_dstaddr.sa_family)
			break;
	if (ifa == (struct ifaddr *) 0)
		return;

	if ((addr->bsel1&DMC_RUN) == 0) {
		printf("dmcinit: DMC not running\n");
		ifp->if_flags &= ~IFF_UP;
		return;
	}
	/* map base table */
	if ((sc->sc_flag & DMC_BMAPPED) == 0) {
		sc->sc_ubinfo = uballoc(ui->ui_ubanum,
			(caddr_t)&dmc_base[unit], sizeof (struct dmc_base), 0);
		sc->sc_flag |= DMC_BMAPPED;
	}
	/* initialize UNIBUS resources */
	sc->sc_iused = sc->sc_oused = 0;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		if (if_ubaminit(&sc->sc_ifuba, ui->ui_ubanum,
		    sizeof(struct dmc_header), (int)btoc(DMCMTU),
		    sc->sc_ifr, NRCV, sc->sc_ifw, NXMT) == 0) {
			printf("dmc%d: can't allocate uba resources\n", unit);
			ifp->if_flags &= ~IFF_UP;
			return;
		}
		ifp->if_flags |= IFF_RUNNING;
	}
	sc->sc_flag &= ~DMC_ONLINE;
	sc->sc_flag |= DMC_RUNNING;
	/*
	 * Limit packets enqueued until we see if we're on the air.
	 */
	ifp->if_snd.ifq_maxlen = 3;

	/* initialize buffer pool */
	/* receives */
	ifrw = &sc->sc_ifr[0];
	for (rp = &sc->sc_rbufs[0]; rp < &sc->sc_rbufs[NRCV]; rp++) {
		rp->ubinfo = UBAI_ADDR(ifrw->ifrw_info);
		rp->cc = DMCMTU + sizeof (struct dmc_header);
		rp->flags = DBUF_OURS|DBUF_RCV;
		ifrw++; 
	}
	/* transmits */
	ifxp = &sc->sc_ifw[0];
	for (rp = &sc->sc_xbufs[0]; rp < &sc->sc_xbufs[NXMT]; rp++) {
		rp->ubinfo = UBAI_ADDR(ifxp->ifw_info);
		rp->cc = 0;
		rp->flags = DBUF_OURS|DBUF_XMIT;
		ifxp++; 
	}

	/* set up command queues */
	sc->sc_qfreeh = sc->sc_qfreet
		 = sc->sc_qhead = sc->sc_qtail = sc->sc_qactive =
		(struct dmc_command *)0;
	/* set up free command buffer list */
	for (qp = &sc->sc_cmdbuf[0]; qp < &sc->sc_cmdbuf[NCMDS]; qp++) {
		QUEUE_AT_HEAD(qp, sc->sc_qfreeh, sc->sc_qfreet);
	}

	/* base in */
	base = UBAI_ADDR(sc->sc_ubinfo);
	dmcload(sc, DMC_BASEI, (u_short)base, (base>>2) & DMC_XMEM);
	/* specify half duplex operation, flags tell if primary */
	/* or secondary station */
	if (ui->ui_flags == 0)
		/* use DDCMP mode in full duplex */
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

	/* enable operation done interrupts */
	while ((addr->bsel2 & DMC_IEO) == 0)
		addr->bsel2 |= DMC_IEO;
	s = spl5();
	/* queue first NRCV buffers for DMC to fill */
	for (rp = &sc->sc_rbufs[0]; rp < &sc->sc_rbufs[NRCV]; rp++) {
		rp->flags |= DBUF_DMCS;
		dmcload(sc, DMC_READ, rp->ubinfo,
			(((rp->ubinfo>>2)&DMC_XMEM) | rp->cc));
		sc->sc_iused++;
	}
	splx(s);
}

/*
 * Start output on interface.  Get another datagram
 * to send from the interface queue and map it to
 * the interface before starting output.
 *
 * Must be called at spl 5
 */
dmcstart(unit)
	int unit;
{
	register struct dmc_softc *sc = &dmc_softc[unit];
	struct mbuf *m;
	register struct dmcbufs *rp;
	register int n;

	/*
	 * Dequeue up to NXMT requests and map them to the UNIBUS.
	 * If no more requests, or no dmc buffers available, just return.
	 */
	n = 0;
	for (rp = &sc->sc_xbufs[0]; rp < &sc->sc_xbufs[NXMT]; rp++ ) {
		/* find an available buffer */
		if ((rp->flags & DBUF_DMCS) == 0) {
			IF_DEQUEUE(&sc->sc_if.if_snd, m);
			if (m == 0)
				return;
			/* mark it dmcs */
			rp->flags |= (DBUF_DMCS);
			/*
			 * Have request mapped to UNIBUS for transmission
			 * and start the output.
			 */
			rp->cc = if_ubaput(&sc->sc_ifuba, &sc->sc_ifw[n], m);
			rp->cc &= DMC_CCOUNT;
			if (++sc->sc_oused == 1)
				sc->sc_if.if_timer = dmc_timeout;
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
	int type;
	u_short w0, w1;
{
	register struct dmcdevice *addr;
	register int unit, sps;
	register struct dmc_command *qp;

	unit = sc - dmc_softc;
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
		printf("dmc%d: dmcrint no command\n", unit);
		return;
	}
	while (addr->bsel0&DMC_RDYI) {
		addr->sel4 = qp->qp_ubaddr;
		addr->sel6 = qp->qp_cc;
		addr->bsel0 &= ~(DMC_IEI|DMC_RQI);
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
		if ((sc->sc_qactive = sc->sc_qhead) == (struct dmc_command *)0)
			break;		/* all done */
		/* more commands to do, start the next one */
		qp = sc->sc_qactive;
		DEQUEUE(sc->sc_qhead, sc->sc_qtail);
		addr->bsel0 = qp->qp_cmd;
		n = RDYSCAN;
		while (n-- > 0)
			if ((addr->bsel0&DMC_RDYI) || (addr->bsel2&DMC_RDYO))
				break;
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
	struct ifqueue *inq;
	int arg, pkaddr, cmd, len, s;
	register struct ifrw *ifrw;
	register struct dmcbufs *rp;
	register struct ifxmt *ifxp;
	struct dmc_header *dh;
	int off, resid;

	addr = (struct dmcdevice *)ui->ui_addr;
	sc = &dmc_softc[unit];
	ifp = &sc->sc_if;

	while (addr->bsel2 & DMC_RDYO) {

		cmd = addr->bsel2 & 0xff;
		arg = addr->sel6 & 0xffff;
		/* reconstruct UNIBUS address of buffer returned to us */
		pkaddr = ((arg&DMC_XMEM)<<2) | (addr->sel4 & 0xffff);
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
			/* find location in dmcuba struct */
			ifrw= &sc->sc_ifr[0];
			for (rp = &sc->sc_rbufs[0]; rp < &sc->sc_rbufs[NRCV]; rp++) {
				if(rp->ubinfo == pkaddr)
					break;
				ifrw++;
			}
			if (rp >= &sc->sc_rbufs[NRCV])
				panic("dmc rcv");
			if ((rp->flags & DBUF_DMCS) == 0)
				printf("dmc%d: done unalloc rbuf\n", unit);

			len = (arg & DMC_CCOUNT) - sizeof (struct dmc_header);
			if (len < 0 || len > DMCMTU) {
				ifp->if_ierrors++;
				printd("dmc%d: bad rcv pkt addr 0x%x len 0x%x\n",
				    unit, pkaddr, len);
				goto setup;
			}
			/*
			 * Deal with trailer protocol: if type is trailer
			 * get true type from first 16-bit word past data.
			 * Remember that type was trailer by setting off.
			 */
			dh = (struct dmc_header *)ifrw->ifrw_addr;
			dh->dmc_type = ntohs((u_short)dh->dmc_type);
#define dmcdataaddr(dh, off, type)	((type)(((caddr_t)((dh)+1)+(off))))
			if (dh->dmc_type >= DMC_TRAILER &&
			    dh->dmc_type < DMC_TRAILER+DMC_NTRAILER) {
				off = (dh->dmc_type - DMC_TRAILER) * 512;
				if (off >= DMCMTU)
					goto setup;		/* sanity */
				dh->dmc_type = ntohs(*dmcdataaddr(dh, off, u_short *));
				resid = ntohs(*(dmcdataaddr(dh, off+2, u_short *)));
				if (off + resid > len)
					goto setup;		/* sanity */
				len = off + resid;
			} else
				off = 0;
			if (len == 0)
				goto setup;

			/*
			 * Pull packet off interface.  Off is nonzero if
			 * packet has trailing header; dmc_get will then
			 * force this header information to be at the front,
			 * but we still have to drop the type and length
			 * which are at the front of any trailer data.
			 */
			m = if_ubaget(&sc->sc_ifuba, ifrw, len, off, ifp);
			if (m == 0)
				goto setup;
			if (off) {
				ifp = *(mtod(m, struct ifnet **));
				m->m_off += 2 * sizeof (u_short);
				m->m_len -= 2 * sizeof (u_short);
				*(mtod(m, struct ifnet **)) = ifp;
			}
			switch (dh->dmc_type) {

#ifdef INET
			case DMC_IPTYPE:
				schednetisr(NETISR_IP);
				inq = &ipintrq;
				break;
#endif
			default:
				m_freem(m);
				goto setup;
			}

			s = splimp();
			if (IF_QFULL(inq)) {
				IF_DROP(inq);
				m_freem(m);
			} else
				IF_ENQUEUE(inq, m);
			splx(s);

	setup:
			/* is this needed? */
			rp->ubinfo = UBAI_ADDR(ifrw->ifrw_info);

			dmcload(sc, DMC_READ, rp->ubinfo, 
			    ((rp->ubinfo >> 2) & DMC_XMEM) | rp->cc);
			break;

		case DMC_OUX:
			/*
			 * A write has completed, start another
			 * transfer if there is more data to send.
			 */
			ifp->if_opackets++;
			/* find associated dmcbuf structure */
			ifxp = &sc->sc_ifw[0];
			for (rp = &sc->sc_xbufs[0]; rp < &sc->sc_xbufs[NXMT]; rp++) {
				if(rp->ubinfo == pkaddr)
					break;
				ifxp++;
			}
			if (rp >= &sc->sc_xbufs[NXMT]) {
				printf("dmc%d: bad packet address 0x%x\n",
				    unit, pkaddr);
				break;
			}
			if ((rp->flags & DBUF_DMCS) == 0)
				printf("dmc%d: unallocated packet 0x%x\n",
				    unit, pkaddr);
			/* mark buffer free */
			if (ifxp->ifw_xtofree) {
				(void)m_freem(ifxp->ifw_xtofree);
				ifxp->ifw_xtofree = 0;
			}
			rp->flags &= ~DBUF_DMCS;
			if (--sc->sc_oused == 0)
				sc->sc_if.if_timer = 0;
			else
				sc->sc_if.if_timer = dmc_timeout;
			if ((sc->sc_flag & DMC_ONLINE) == 0) {
				extern int ifqmaxlen;

				/*
				 * We're on the air.
				 * Open the queue to the usual value.
				 */
				sc->sc_flag |= DMC_ONLINE;
				ifp->if_snd.ifq_maxlen = ifqmaxlen;
			}
			break;

		case DMC_CNTLO:
			arg &= DMC_CNTMASK;
			if (arg & DMC_FATAL) {
				if (arg != DMC_START)
					log(LOG_ERR,
					    "dmc%d: fatal error, flags=%b\n",
					    unit, arg, CNTLO_BITS);
				dmcrestart(unit);
				break;
			}
			/* ACCUMULATE STATISTICS */
			switch(arg) {
			case DMC_NOBUFS:
				ifp->if_ierrors++;
				if ((sc->sc_nobuf++ % DMC_RPNBFS) == 0)
					goto report;
				break;
			case DMC_DISCONN:
				if ((sc->sc_disc++ % DMC_RPDSC) == 0)
					goto report;
				break;
			case DMC_TIMEOUT:
				if ((sc->sc_timeo++ % DMC_RPTMO) == 0)
					goto report;
				break;
			case DMC_DATACK:
				ifp->if_oerrors++;
				if ((sc->sc_datck++ % DMC_RPDCK) == 0)
					goto report;
				break;
			default:
				goto report;
			}
			break;
		report:
			printd("dmc%d: soft error, flags=%b\n", unit,
			    arg, CNTLO_BITS);
			if ((sc->sc_flag & DMC_RESTART) == 0) {
				/*
				 * kill off the dmc to get things
				 * going again by generating a
				 * procedure error
				 */
				sc->sc_flag |= DMC_RESTART;
				arg = UBAI_ADDR(sc->sc_ubinfo);
				dmcload(sc, DMC_BASEI, arg, (arg>>2)&DMC_XMEM);
			}
			break;

		default:
			printf("dmc%d: bad control %o\n", unit, cmd);
			break;
		}
	}
	dmcstart(unit);
	return;
}

/*
 * DMC output routine.
 * Encapsulate a packet of type family for the dmc.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
dmcoutput(ifp, m0, dst)
	register struct ifnet *ifp;
	register struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, error, s;
	register struct mbuf *m = m0;
	register struct dmc_header *dh;
	register int off;

	if ((ifp->if_flags & IFF_UP) == 0) {
		error = ENETDOWN;
		goto bad;
	}

	switch (dst->sa_family) {
#ifdef	INET
	case AF_INET:
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if ((ifp->if_flags & IFF_NOTRAILERS) == 0)
		if (off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = DMC_TRAILER + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = htons((u_short)DMC_IPTYPE);
			*(mtod(m, u_short *) + 1) = htons((u_short)m->m_len);
			goto gottrailertype;
		}
		type = DMC_IPTYPE;
		off = 0;
		goto gottype;
#endif

	case AF_UNSPEC:
		dh = (struct dmc_header *)dst->sa_data;
		type = dh->dmc_type;
		goto gottype;

	default:
		printf("dmc%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottrailertype:
	/*
	 * Packet to be sent as a trailer; move first packet
	 * (control information) to end of chain.
	 */
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;
	m0 = m;

gottype:
	/*
	 * Add local network header
	 * (there is space for a uba on a vax to step on)
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof(struct dmc_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct dmc_header);
	} else {
		m->m_off -= sizeof (struct dmc_header);
		m->m_len += sizeof (struct dmc_header);
	}
	dh = mtod(m, struct dmc_header *);
	dh->dmc_type = htons((u_short)type);

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
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

bad:
	m_freem(m0);
	return (error);
}


/*
 * Process an ioctl request.
 */
/* ARGSUSED */
dmcioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	int s = splimp(), error = 0;
	register struct dmc_softc *sc = &dmc_softc[ifp->if_unit];

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		if ((ifp->if_flags & IFF_RUNNING) == 0)
			dmcinit(ifp->if_unit); 
		break;

	case SIOCSIFDSTADDR:
		if ((ifp->if_flags & IFF_RUNNING) == 0)
			dmcinit(ifp->if_unit); 
		break;
		
	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    sc->sc_flag & DMC_RUNNING)
			dmcdown(ifp->if_unit);
		else if (ifp->if_flags & IFF_UP &&
		    (sc->sc_flag & DMC_RUNNING) == 0)
			dmcrestart(ifp->if_unit);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 * Restart after a fatal error.
 * Clear device and reinitialize.
 */
dmcrestart(unit)
	int unit;
{
	register struct dmc_softc *sc = &dmc_softc[unit];
	register struct dmcdevice *addr;
	register int i;
	int s;
	
#ifdef DEBUG
	/* dump base table */
	printf("dmc%d base table:\n", unit);
	for (i = 0; i < sizeof (struct dmc_base); i++)
		printf("%o\n" ,dmc_base[unit].d_base[i]);
#endif

	dmcdown(unit);

	/*
	 * Let the DMR finish the MCLR.	 At 1 Mbit, it should do so
	 * in about a max of 6.4 milliseconds with diagnostics enabled.
	 */
	addr = (struct dmcdevice *)(dmcinfo[unit]->ui_addr);
	for (i = 100000; i && (addr->bsel1 & DMC_RUN) == 0; i--)
		;
	/* Did the timer expire or did the DMR finish? */
	if ((addr->bsel1 & DMC_RUN) == 0) {
		log(LOG_ERR, "dmc%d: M820 Test Failed\n", unit);
		return;
	}

	/* restart DMC */
	dmcinit(unit);
	sc->sc_flag &= ~DMC_RESTART;
	s = spl5();
	dmcstart(unit);
	splx(s);
	sc->sc_if.if_collisions++;	/* why not? */
}

/*
 * Reset a device and mark down.
 * Flush output queue and drop queue limit.
 */
dmcdown(unit)
	int unit;
{
	register struct dmc_softc *sc = &dmc_softc[unit];
	register struct ifxmt *ifxp;

	((struct dmcdevice *)(dmcinfo[unit]->ui_addr))->bsel1 = DMC_MCLR;
	sc->sc_flag &= ~(DMC_RUNNING | DMC_ONLINE);

	for (ifxp = sc->sc_ifw; ifxp < &sc->sc_ifw[NXMT]; ifxp++) {
		if (ifxp->ifw_xtofree) {
			(void) m_freem(ifxp->ifw_xtofree);
			ifxp->ifw_xtofree = 0;
		}
	}
	if_qflush(&sc->sc_if.if_snd);
}

/*
 * Watchdog timeout to see that transmitted packets don't
 * lose interrupts.  The device has to be online (the first
 * transmission may block until the other side comes up).
 */
dmctimeout(unit)
	int unit;
{
	register struct dmc_softc *sc;
	struct dmcdevice *addr;

	sc = &dmc_softc[unit];
	if (sc->sc_flag & DMC_ONLINE) {
		addr = (struct dmcdevice *)(dmcinfo[unit]->ui_addr);
		log(LOG_ERR, "dmc%d: output timeout, bsel0=%b bsel2=%b\n",
		    unit, addr->bsel0 & 0xff, DMC0BITS,
		    addr->bsel2 & 0xff, DMC2BITS);
		dmcrestart(unit);
	}
}
#endif
