/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_apx.c	7.11 (Berkeley) %G%
 */

/*
 * Driver for SGS-THOMSON MK5025 based Link level controller.
 * The chip will do LAPB in hardware, although this driver only
 * attempts to use it for HDLC framing.
 *
 * Driver written by Keith Sklower, based on lance AMD7990 
 * driver by Van Jacobsen, and information graciously supplied
 * by the ADAX corporation of Berkeley, CA.
 */

#include "apx.h"
#if NAPX > 0

#include <sys/param.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/syslog.h>

#include <net/if.h>
#include <net/netisr.h>
#include <net/if_types.h>
#ifdef CCITT
#include <netccitt/x25.h>
int x25_rtrequest(), x25_ifoutput();
#endif

#include <i386/isa/if_apxreg.h>

int	apxprobe(), apxattach(), apxstart(), apx_uprim(), apx_meminit();
int	apxinit(), apxoutput(), apxioctl(), apxreset(), apxdebug = 0;
void	apx_ifattach(), apxtest(), apxinput(), apxintr(), apxtint(), apxrint();

struct apx_softc {
	struct	ifnet apx_if;
	caddr_t	apx_device;		/* e.g. isa_device, vme_device, etc. */
	struct	apc_reg *apx_reg;	/* control regs for both subunits */
	struct	apc_mem *apx_hmem;	/* Host addr for shared memory */
	struct	apc_mem *apx_dmem;	/* Device (chip) addr for shared mem */
	struct	sgcp *apx_sgcp;		/* IO control port for this subunit */
	int	apx_flags;		/* Flags specific to this driver */
#define APXF_CHIPHERE	0x01		/* mk5025 present */
	int	apx_rxnum;		/* Last receiver dx we looked at */
	int	apx_txnum;		/* Last tranmistter dx we stomped on */
	int	apx_txcnt;		/* Number of packets queued for tx*/
	u_int	apx_msize;
	struct	sgae apx_csr23;		/* 24 bit init addr, as seen by chip */
	u_short	apx_csr4;		/* byte gender, set in mach dep code */
	struct	apc_modes apx_modes;	/* Parameters, as amended by ioctls */
} apx_softc[2 * NAPX];

struct apxstat {
	int	rxnull;			/* no rx bufs ready this interrupt */
	int	rxnrdy;			/* expected rx buf not ready */
	int	rx2big;			/* expected rx buf not ready */
	int	txnull;
	int	pint;			/* new primitive available interrupt */
	int	rint;			/* receive interrupts */
	int	tint;			/* transmit interrupts */
	int	anyint;			/* note all interrupts */
	int	queued;			/* got through apxinput */
	int	nxpctd;			/* received while if was down */
	int	rstfld;			/* reset didn't work */
} apxstat;

/* default operating paramters for devices */
struct	apc_modes apx_default_modes = {
 { 1,		/* apm_sgob.lsaddr; */
   3,		/* apm_sgob.rsaddr; */
   -SGMTU,	/* apm_sgob.n1; */
   ((-10)<<8),	/* apm_sgob.n2_scale; */
   -1250,	/* apm_sgob.t1; */
   -10000,	/* apm_sgob.t3; */
   -80,		/* apm_sgob.tp; */
 },
 2,		/* apm_txwin; */
 1,		/* apm_apxmode: RS_232 connector and modem clock; */
 0,		/* apm_apxaltmode: enable dtr, disable X.21 connector; */
 IFT_X25,	/* apm_iftype; */
};

/* Begin bus & endian dependence */

#include <i386/isa/isa_device.h>

struct	isa_driver apxdriver = {
	apxprobe, apxattach, "apx",
};

#define SG_RCSR(apx, csrnum) \
	 (outw(&(apx->apx_sgcp->sgcp_rap), csrnum << 1), \
	  inw(&(apx->apx_sgcp->sgcp_rdp)))

#define SG_WCSR(apx, csrnum, data) \
	 (outw(&(apx->apx_sgcp->sgcp_rap), csrnum << 1), \
	  outw(&(apx->apx_sgcp->sgcp_rdp), data))

#define APX_RCSR(apx, csrname) inb(&(apx->apx_reg->csrname))
#define APX_WCSR(apx, csrname, data) outb(&(apx->apx_reg->csrname), data)

#define TIMO 10000 /* used in apx_uprim */

apxprobe(id)
	register struct	isa_device *id;
{
	int	moffset = 0, nchips = 2, unit = id->id_unit << 1, subunit;
	struct	apc_reg *reg = (struct apc_reg *)id->id_iobase;
	register struct	apx_softc *apx = apx_softc + unit;

	/*
	 * Probing for the second MK5025 on all ISA/EISA adax boards
	 * manufactured prior to July 1992 (and some time following)
	 * will hang the bus and the system.  Thus, it is essential
	 * not to probe for the second mk5025 if it is known not to be there.
	 * As the current config scheme for 386BSD does not have a flags
	 * field, we adopt the convention of using the low order bit of
	 * the memsize to warn us that we have a single chip board.
	 */
	if (id->id_msize & 1)
		nchips = 1;
	for (subunit = 0; subunit < nchips; subunit++) {
		apx->apx_msize	= id->id_msize >> 1;
		apx->apx_hmem	= (struct apc_mem *) (id->id_maddr + moffset);
		apx->apx_dmem	= (struct apc_mem *) moffset;
		apx->apx_device	= (caddr_t) id;
		apx->apx_reg	= reg;
		apx->apx_sgcp	= reg->axr_sgcp + subunit;
		apx->apx_csr4	= 0x0210;	/* no byte swapping for PC-AT */
		apx->apx_modes	= apx_default_modes;
		apx->apx_if.if_unit = unit++;
		moffset = apx->apx_msize;
		apxtest(apx++);
	}
	return 1;
}

apxattach(id)
	struct	isa_device *id;
{
	register struct	apx_softc *apx = apx_softc + (id->id_unit << 1);

	apx_ifattach(&((apx++)->apx_if));
	apx_ifattach(&(apx->apx_if));
	return 0;
}
/* End bus & endian dependence */

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
void
apx_ifattach(ifp)
	register struct ifnet *ifp;
{
	/*
	 * Initialize ifnet structure
	 */
	ifp->if_name	= "apc";
	ifp->if_mtu	= SGMTU;
	ifp->if_init	= apxinit;
	ifp->if_output	= apxoutput;
	ifp->if_start	= apxstart;
	ifp->if_ioctl	= apxioctl;
	ifp->if_reset	= apxreset;
	ifp->if_type	= apx_default_modes.apm_iftype;
	ifp->if_hdrlen	= 5;
	ifp->if_addrlen	= 8;
	if_attach(ifp);
}
/*
 * Initialization of interface
 */
apxinit(unit)
	int unit;
{
	struct ifnet *ifp = &apx_softc[unit].apx_if;
	int s = splimp();

	ifp->if_flags &= ~(IFF_RUNNING|IFF_OACTIVE);
	if (apxreset(unit) && (ifp->if_flags & IFF_UP)) {
		ifp->if_flags |= IFF_RUNNING;
		(void)apxstart(ifp);
	}
	splx(s);
	return 0;
}

apxctr(apx)
	register struct apx_softc *apx;
{
	APX_WCSR(apx, axr_ccr, 0xB0); /* select ctr 2, write lsb+msb, mode 0 */
	APX_WCSR(apx, axr_cnt2, 0x1);
	APX_WCSR(apx, axr_cnt2, 0x0);
	DELAY(50);
	APX_WCSR(apx, axr_ccr, 0xE8); /* latch status, ctr 2; */
	return (APX_RCSR(apx, axr_cnt2));
}

void
apxtest(apx)
	register struct apx_softc *apx;
{
	int i =  0;

	if ((apx->apx_if.if_unit & 1) == 0 && (i = apxctr(apx)) == 0)
		apxerror(apx, "no response from timer chip", 0);
	if (SG_RCSR(apx, 1) & 0x8000)
		SG_WCSR(apx, 1, 0x8040);
	SG_WCSR(apx, 4, apx->apx_csr4);
	SG_WCSR(apx, 5, 0x08);		/* Set DTR mode in SGS thompson chip */
	if (((i = SG_RCSR(apx, 5)) & 0xff08) != 0x08)
		apxerror(apx, "no mk5025, csr5 high bits are", i);
	else
		apx->apx_flags |= APXF_CHIPHERE;
	(void) apx_uprim(apx, SG_STOP, "stop after probing");
}

apxreset(unit)
	int	unit;
{
	register struct apx_softc *apx = &apx_softc[unit ^ 1];
	u_char apm_apxmode = 0, apm_apxaltmode = 0;
#define MODE(m) (m |= apx->apx_modes.m << ((apx->apx_if.if_unit & 1) ? 1 : 0))

	MODE(apm_apxmode);
	MODE(apm_apxaltmode);
	apx = apx_softc + unit;
	MODE(apm_apxmode);
	MODE(apm_apxaltmode);
	APX_WCSR(apx, axr_mode, apm_apxmode);
	APX_WCSR(apx, axr_altmode, apm_apxaltmode);
	(void) apxctr(apx);
	(void) apx_uprim(apx, SG_STOP, "stop to reset");
	if ((apx->apx_if.if_flags & IFF_UP) == 0)
		return 0;
	apx_meminit(apx->apx_hmem, apx);
	SG_WCSR(apx, 4, apx->apx_csr4);
	SG_WCSR(apx, 2, apx->apx_csr23.f_hi);
	SG_WCSR(apx, 3, apx->apx_csr23.lo);
	if (apx_uprim(apx, SG_INIT, "init request") ||
	    apx_uprim(apx, SG_STAT, "status request") ||
	    apx_uprim(apx, SG_TRANS, "transparent mode"))
		return 0;
	SG_WCSR(apx, 0, SG_INEA);
	return 1;
}

apx_uprim(apx, request, ident)
	register struct apx_softc *apx;
	char *ident;
{
	register int timo = 0;
	int reply;

	if ((apx->apx_flags & APXF_CHIPHERE) == 0)
		return 1;	/* maybe even should panic . . . */
	if ((reply = SG_RCSR(apx, 1)) & 0x8040)
		SG_WCSR(apx, 1, 0x8040); /* Magic! */
	if (request == SG_STOP && (SG_RCSR(apx, 0) & SG_STOPPED))
		return 0;
	SG_WCSR(apx, 1, request | SG_UAV);
	do {
		reply = SG_RCSR(apx, 1);
		if (timo++ >= TIMO || (reply & 0x8000)) {
				apxerror(apx, ident, reply);
				return 1;
		}
	} while (reply & SG_UAV);
	return 0;
}

apx_meminit(apc, apx)
	register struct apc_mem *apc;
	struct apx_softc *apx;
{
	register struct apc_mem *apcbase = apx->apx_dmem;
	register int i;
#define LOWADDR(e) (((u_long)&(apcbase->e)) & 0xffff)
#define HIADDR(e) ((((u_long)&(apcbase->e)) >> 16) & 0xff)
#define SET_SGAE(d, f, a) {(d).lo = LOWADDR(a); (d).f_hi = (f) | HIADDR(a);}
#define SET_SGDX(d, f, a, b) \
	{SET_SGAE((d).sgdx_ae, f, a); (d).sgdx_mcnt = (d).sgdx_bcnt = (b);}

	apx->apx_txnum = apx->apx_rxnum = apx->apx_txcnt = 0;
	bzero((caddr_t)apc, ((caddr_t)(&apc->apc_rxmd[0])) - (caddr_t)apc);
	apc->apc_mode = 0x0108;	/* 2 flag spacing, leave addr & ctl, do CRC16 */
	apc->apc_sgop = apx->apx_modes.apm_sgop;
	SET_SGAE(apx->apx_csr23, SG_UIE | SG_PROM, apc_mode);
	SET_SGAE(apc->apc_rxdd, SG_RLEN, apc_rxmd[0]);
	i = SG_TLEN | ((apx->apx_modes.apm_txwin)<< 8);
	SET_SGAE(apc->apc_txdd, i, apc_txmd[0]);
	SET_SGAE(apc->apc_stdd, 0, apc_sgsb);
	SET_SGDX(apc->apc_rxtid, SG_OWN, apc_rxidbuf[0], -SGMTU);
	SET_SGDX(apc->apc_txtid, 0, apc_txidbuf[0], 0);
	for (i = 0; i < SGRBUF; i++)
		 SET_SGDX(apc->apc_rxmd[i], SG_OWN, apc_rbuf[i][0], -SGMTU)
	for (i = 0; i < SGTBUF; i++)
		 SET_SGDX(apc->apc_txmd[i], 0, apc_tbuf[i][0], 0)
}

/*
 * Start output on interface.  Get another datagram to send
 * off of the interface queue, and copy it to the interface
 * before starting the output.
 */
apxstart(ifp)
	struct ifnet *ifp;
{
	register struct apx_softc *apx = &apx_softc[ifp->if_unit];
	register struct sgdx *dx;
	struct apc_mem *apc = apx->apx_hmem;
	struct mbuf *m;
	int len;

	if ((ifp->if_flags & IFF_RUNNING) == 0)
		return (0);
	do {
		dx = apc->apc_txmd + apx->apx_txnum;
		if (dx->sgdx_flags & SG_OWN)
			return (0);
		IF_DEQUEUE(&ifp->if_snd, m);
		if (m == 0)
			return (0);
		len = min(m->m_pkthdr.len, SGMTU);
		m_copydata(m, 0, len, apc->apc_tbuf[apx->apx_txnum]);
		m_freem(m);
		dx->sgdx_mcnt = -len;
		dx->sgdx_flags = (SG_OWN|SG_TUI|SG_SLF|SG_ELF) |
			(0xff & dx->sgdx_flags);
		SG_WCSR(apx, 0, SG_INEA | SG_TDMD);
		DELAY(20);
		if (++apx->apx_txnum >= SGTBUF)
			apx->apx_txnum = 0;
	} while (++apx->apx_txcnt < SGTBUF);
	apx->apx_txcnt = SGTBUF; /* in case txcnt > SGTBUF by mistake */
	ifp->if_flags |= IFF_OACTIVE;
	return (0);
}

void
apxintr()
{
	register struct apx_softc *apx;
	int reply;

	apxstat.anyint++;
	for (apx = apx_softc + NAPX + NAPX; --apx >= apx_softc;) {
		if (apx->apx_flags & APXF_CHIPHERE)
		    /* Try to turn off interrupt cause */
		    while ((reply = SG_RCSR(apx, 0)) & 0xff) {
			SG_WCSR(apx, 0, SG_INEA | 0xfe);
			if (reply & (SG_MERR|SG_TUR|SG_ROR)) {
				apxerror(apx, "mem, rx, or tx error", reply);
				apxinit(apx->apx_if.if_unit);
				break;
			}
			if (reply & SG_RINT)
				apxrint(apx);
			if (reply & SG_TINT)
				apxtint(apx);
			if (reply & SG_PINT)
				apxstat.pint++;
		}
	}
}

void
apxtint(apx)
	register struct apx_softc *apx;
{
	register struct apc_mem *apc = apx->apx_hmem;
	int i, loopcount = 0;

	apxstat.tint++;
	do {
		if ((i = apx->apx_txnum - apx->apx_txcnt) < 0)
			i += SGTBUF;
		if (apc->apc_txmd[i].sgdx_flags & SG_OWN) {
			if (loopcount)
				break;
			apxstat.txnull++;
			return;
		}
		loopcount++;
		apx->apx_if.if_flags &= ~IFF_OACTIVE;
	} while (--apx->apx_txcnt > 0);
	apxstart(&apx->apx_if);
}

void
apxrint(apx)
	register struct apx_softc *apx;
{
	register struct apc_mem *apc = apx->apx_hmem;
	register struct sgdx *dx = apc->apc_rxmd + apx->apx_rxnum;
	int i = 0;
#define SGNEXTRXMD \
dx = ++apx->apx_rxnum == SGRBUF ? &apc->apc_rxmd[apx->apx_rxnum = 0] : dx + 1;

	apxstat.rint++;
	/*
	 * Out of sync with hardware, should never happen?
	 */
	while (dx->sgdx_flags & SG_OWN) {
		apxstat.rxnrdy++;
		if (++i == SGRBUF) {
			apxstat.rxnull++;
			return;
		}
		SGNEXTRXMD;
	}
	/*
	 * Process all buffers with valid data
	 */
	while ((dx->sgdx_flags & SG_OWN) == 0) {
		if ((dx->sgdx_flags & (SG_SLF|SG_ELF)) != (SG_SLF|SG_ELF)) {
			/*
			 * Find the end of the packet so we synch up.
			 * We throw the data away.
			 */
			apxerror(apx, "chained buffer", dx->sgdx_flags);
			do {
				apxstat.rx2big++;
				dx->sgdx_bcnt = 0;
				dx->sgdx_flags = SG_OWN | (0xff&dx->sgdx_flags);
				SGNEXTRXMD;
			} while (!(dx->sgdx_flags & (SG_OWN|SG_SLF|SG_ELF)));
			/*
			 * If search terminated without successful completion
			 * we reset the hardware (conservative).
			 */
			if ((dx->sgdx_flags & (SG_OWN|SG_SLF|SG_ELF)) !=
			    SG_ELF) {
				apxreset(apx->apx_if.if_unit);
				return;
			}
		} else
			apxinput(&apx->apx_if, apc->apc_rbuf[apx->apx_rxnum],
					-dx->sgdx_mcnt);
		dx->sgdx_bcnt = 0;
		dx->sgdx_flags = SG_OWN | (0xff & dx->sgdx_flags);
		SGNEXTRXMD;
	}
}

void
apxinput(ifp, buffer, len)
	register struct ifnet *ifp;
	caddr_t buffer;
{
	extern struct ifqueue hdintrq, ipintrq;
	register struct ifqueue *inq;
	register u_char *cp = (u_char *)buffer;
	struct mbuf *m, *m_devget();
	int isr;

	ifp->if_ipackets++;
	if ((ifp->if_flags & IFF_UP) == 0) {
		apxstat.nxpctd++;
		return;
	}
	if (cp[0] == 0xff && cp[1] == 0x3) {
		/* This is a UI HDLC Packet, so we'll assume PPP
		   protocol.  for now, IP only. */
		buffer += 4;
		len -= 4;
		inq = &ipintrq;
		isr = NETISR_IP;
	} else {
#ifdef CCITT
		inq = &hdintrq;
		isr = NETISR_CCITT;
	}
	if (len <= 0) {
#endif
		return;
	}
	m = m_devget(buffer, len, 0, ifp, (void (*)())0);
	if (m == 0)
		return;
	if(IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else {
		apxstat.queued++;
		IF_ENQUEUE(inq, m);
		schednetisr(isr);
	}
}

/*
 * Process an ioctl request.
 */
apxioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;
	struct apx_softc *apx = &apx_softc[ifp->if_unit];

	switch (cmd) {

	case SIOCSIFADDR:
#ifdef CCITT
		ifa->ifa_rtrequest = x25_rtrequest;
		break;

	case SIOCSIFCONF_X25:
		ifp->if_output = x25_ifoutput;
		ifp->if_flags |= IFF_UP;
		error = hd_ctlinput(PRC_IFUP, ifa->ifa_addr);
		if (error == 0)
			apxinit(ifp->if_unit);
#endif
		break;

	case SIOCSIFFLAGS:
		if (((ifp->if_flags & IFF_UP) == 0 &&
		     (ifp->if_flags & IFF_RUNNING)) ||
		    (ifp->if_flags & IFF_UP) &&
		     (ifp->if_flags & IFF_RUNNING) == 0)
			apxinit(ifp->if_unit);
		break;

	case SIOCSIFMODE:
		if ((ifp->if_flags & IFF_UP) == 0)
			apx->apx_modes = *(struct apc_modes *)data;
		else
	default:
			error = EINVAL;

	}
	splx(s);
	return (error);
}

apxerror(apx, msg, data)
	register struct	apx_softc *apx;	
	char	*msg;
{
	log(LOG_WARNING, "apc%d: %s, stat=0x%x\n",
		apx->apx_if.if_unit, msg, data);
}

/*
 * For debugging loopback activity.
 */
apxoutput(ifp, m, dst, rt)
register struct ifnet *ifp;
register struct mbuf *m;
struct sockaddr *dst;
struct rtentry *rt;
{
	int s = splimp(), error = 0;
	static char pppheader[4] = { -1, 3, 0, 0x21 };
	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	ifp->if_opackets++;
	M_PREPEND(m, sizeof pppheader, M_DONTWAIT);
	if (m == 0) {
		splx(s);
		return ENOBUFS;
	}
	bcopy(pppheader, mtod(m, caddr_t), sizeof pppheader);
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		m_freem(m);
		error = ENOBUFS;
	} else {
		IF_ENQUEUE(&ifp->if_snd, m);
		if ((ifp->if_flags & IFF_OACTIVE) == 0)
			(*ifp->if_start)(ifp);
	}
	splx(s);
	return (error);
}
#endif /* NAPX */
