/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_apx.c	7.2 (Berkeley) %G%
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

#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/if_types.h"
#include "netccitt/x25.h"

#include "if_apxreg.h"

int	apxprobe(), apxattach(), apxstart(), apx_uprim(), apx_meminit();
int	apxinit(), x25_ifoutput(), x25_rtrequest(), apxioctl(), apxreset();
void	apx_ifattach(), apxinput(), apxintr(), apxtint(), apaxrint();

struct apx_softc {
	struct	ifnet apx_if;
	caddr_t	apx_device;		/* e.g. isa_device */
	u_short	apx_csr4;		/* byte gender, set in mach dep code */
	struct	apc_reg *apx_reg;	/* control regs for both subunits */
	struct	apc_mem *apx_hmem;	/* Host addr for shared memory */
	struct	apc_mem *apx_dmem;	/* Device (chip) addr for shared mem */
	struct	sgcp *apx_sgcp;		/* IO control port for this subunit */
	struct	apc_modes apx_modes;	/* Parameters, as amended by ioctls */
	int	apx_rxnum;		/* Last receiver dx we looked at */
	int	apx_txnum;		/* Last tranmistter dx we stomped on */
	int	apx_txcnt;		/* Number of packets queued for tx*/
} apx_softc[2 * NAPX], *apx_lastsoftc = apx_softc;

struct apxstat {
	int	nulltx;
	int	pint;
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
 5,		/* apm_apxmode; */
 0,		/* apm_apxaltmode; */
 IFT_X25,	/* apm_iftype; */
};

/* Begin bus & endian dependence */

#include "isa_device.h"

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
	int	moffset, subunit, unit = id->id_unit << 1;
	struct	apc_reg *reg = (struct apc_reg *)id->id_iobase;
	register struct	apx_softc *apx = apx_softc + unit;

	/* Set and read DTR defeat in channel 0 to test presence of apc */
	outb(&reg->axr_altmode, 4);
	if (inb(&reg->axr_altmode) == 0)
		return 0;			/* No board present */

	for (subunit = 0; subunit < 2; subunit++, apx++) {
		/* Set and read DTR mode to test present of SGS thompson chip */
		apx->apx_if.if_unit = unit++;
		apx->apx_sgcp = reg->axr_sgcp + subunit;
		SG_WCSR(apx, 5, 0x08);
		if (((SG_RCSR(apx, 5) & 0xff08) != 0x08)) {
			apxerror(apx, "no mk5025 for channel", subunit);
			continue;
		}
		moffset = subunit ? id->id_msize >> 1 : 0;
		apx->apx_hmem	= (struct apc_mem *) (id->id_maddr + moffset);
		apx->apx_dmem	= (struct apc_mem *) (moffset);
		apx->apx_modes	= apx_default_modes;
		apx->apx_device = (caddr_t) id;
		apx->apx_reg	= reg;
		apx->apx_csr4	= 0x0110;	/* no byte swapping for PC-AT */
	}
	return 1;
}

apxattach(id)
	register struct isa_device *id;
{
	int	unit = id->id_unit + id->id_unit;

	apx_ifattach(unit);
	apx_ifattach(unit + 1);
	return (0);
}

/* End bus & endian dependence */

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
void
apx_ifattach(unit)
{
	register struct ifnet *ifp = &(apx_softc[unit].apx_if);
	/*
	 * Initialize ifnet structure
	 */
	if (apx_softc[unit].apx_device == 0)
		return;
	ifp->if_name = "apc";
	ifp->if_mtu = SGMTU;
	ifp->if_init = apxinit;
	ifp->if_output = x25_ifoutput;
	ifp->if_start = apxstart;
	ifp->if_ioctl = apxioctl;
	ifp->if_reset = apxreset;
	ifp->if_type = apx_default_modes.apm_iftype;
	ifp->if_hdrlen = 5;
	ifp->if_addrlen = 8;
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
	apx->apx_txnum = apx->apx_rxnum = apx->apx_txcnt = 0;

	if (apx_uprim(apx, SG_STOP, "stop") ||
	    !(apx->apx_if.if_flags & IFF_UP))
		return 0;
	apx_meminit(apx->apx_hmem, apx); /* also sets CSR2 */
	SG_WCSR(apx, 3, (int)apx->apx_dmem);
	SG_WCSR(apx, 4, apx->apx_csr4);
	if (apx_uprim(apx, SG_INIT, "init request") ||
	    apx_uprim(apx, SG_STAT, "status request") ||
	    apx_uprim(apx, SG_TRANS, "transparent mode"))
		return 0;
	SG_WCSR(apx, 0, SG_INEA);
	return 1;
}

apx_uprim(apx, request, ident)
	int request;
	char *ident;
	register struct apx_softc *apx;
{
	register int timo = 0;
	int reply = SG_RCSR(apx, 1);

	if (reply & 0x8040)
		SG_WCRS(1, 0x8040); /* Magic! */
	SG_WCSR(apx, 1, request | SG_UAV);
	do {
		reply = SG_RCRS(1);
		if (timo >= TIMO | reply & 0x8000) {
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
#define SET_SGDX(dx, f, a, b, m) \
	{ (dx).sgdx_addr = LOWADDR(a); (dx).sgdx_bcnt = (b);\
	  (dx).sgdx_mcnt = (m); (dx).sgdx_flags = (f) | HIADDR(a); }

	bzero((caddr_t)apc, LOWADDR(apc_rxmd[0]));
	apc->apc_mode = 0x8040;	/* 2 flag spacing, trans mode, 16bit FCS */
	apc->apc_sgop = apx->apx_modes.apm_sgop;
	apc->apc_rlen = SG_RLEN | HIADDR(apc_rxmd[0]);
	apc->apc_rdra = LOWADDR(apc_rxmd[0]);
	apc->apc_rlen = SG_TLEN | apx->apx_modes.apm_txwin |HIADDR(apc_txmd[0]);
	apc->apc_tdra = LOWADDR(apc_txmd[0]);
	SET_SGDX(apc->apc_rxtid, SG_OWN, apc_rxidbuf, -SGMTU, 0);
	SET_SGDX(apc->apc_txtid, 0, apc_txidbuf, -SGMTU, 0);
	apc->apc_stathi = HIADDR(apc_sgsb);
	apc->apc_statlo = LOWADDR(apc_sgsb);
	for (i = 0; i < SGRBUF; i++)
		 SET_SGDX(apc->apc_rxmd[i], SG_OWN, apc_rbuf[i][0], -SGMTU, 0)
	for (i = 0; i < SGTBUF; i++)
		 SET_SGDX(apc->apc_txmd[i], SG_TUI, apc_tbuf[i][0], 0, 0)
	SG_WCSR(apx, 2, SG_UIE | SG_PROM | HIADDR(apc_mode));
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
		dx->sgdx_mcnt = -len;
		dx->sgdx_flags = SG_OWN | SG_TUI | (0xff & dx->sgdx_flags);
		SG_WCSR(apx, 0, SG_INEA | SG_TDMD);
		if (++apx->apx_txnum >= SGTBUF)
			apx->apx_txnum = 0;
	} while (++apx->apx_txcnt < SGTBUF);
	apx->apx_txcnt = SGTBUF;
	ifp->if_flags |= IFF_OACTIVE;
	return (0);
}

void
apxintr()
{
	register struct apx_softc *apx = apx_lastsoftc;
	struct apx_softc *apxlim = apx_softc + NAPX + NAPX;
	int reply;

	do {
		if (apx->apx_if.if_flags & IFF_UP)
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
		if (++apx >= apxlim)
			apx = apx_softc;
	} while (apx != apx_lastsoftc);
}

void
apxtint(apx)
	register struct apx_softc *apx;
{
	register struct apc_mem *apc = apx->apx_hmem;
	int i, loopcount = 0;

	do {
		if ((i = apx->apx_txnum - apx->apx_txcnt) < 0)
			i += SGTBUF;
		if (apc->apc_txmd[i].sgdx_flags & SG_OWN) {
			if (loopcount)
				break;
			apxstat.nulltx++;
			return;
		}
		loopcount++;
		apx->apx_if.if_flags &= ~IFF_OACTIVE;
	} while (--apx->apx_txcnt > 0);
	apxstart(&apx->apx_if);
}

apxrint(apx)
	register struct apx_softc *apx;
{
	register struct apc_mem *apc = apx->apx_hmem;
	register struct sgdx *dx = apc->apc_rxmd + apx->apx_rxnum;
#define SGNEXTRXMD \
dx = ++apx->apx_rxnum == SGRBUF ? &apc->apc_rxmd[apx->apx_rxnum = 0] : dx + 1;

	/*
	 * Out of sync with hardware, should never happen?
	 */
	if (dx->sgdx_flags & SG_OWN) {
		apxerror(apx, "out of sync");
		return;
	}
	/*
	 * Process all buffers with valid data
	 */
	while ((dx->sgdx_flags & SG_OWN) == 0) {
		if ((dx->sgdx_flags & (SG_SLF|SG_ELF)) != (SG_SLF|SG_ELF)) {
			/*
			 * Find the end of the packet so we can see how long
			 * it was.  We still throw it away.
			 */
			apxerror(apx, "chained buffer", dx->sgdx_flags);
			do {
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
					-dx->sgdx_bcnt);
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
	register struct ifqueue *inq;
	struct mbuf *m, *apxget();
	extern struct ifqueue hdintrq, ipintrq;
	int isr;

	ifp->if_ipackets++;
    {
	register u_char *cp = (u_char *)buffer;

	if (cp[0] == 0xff && cp[1] == 0x3) {
		/* This is a UI HDLC Packet, so we'll assume PPP
		   protocol.  for now, IP only. */
		buffer += 4;
		len -= 4;
		inq = &ipintrq;
		isr = NETISR_IP;
	} else {
		inq = &hdintrq;
		isr = NETISR_CCITT;
	}
    }
	if (len <= 0)
		return;

	m = apxget(buffer, len , 0, ifp);
	if (m == 0)
		return;

	if(IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else {
		IF_ENQUEUE(inq, m);
		schednetisr(isr);
	}
}

/*
 * Routine to copy from board local memory into mbufs.
 */
struct mbuf *
apxget(buf, totlen, off0, ifp)
	char *buf;
	int totlen, off0;
	struct ifnet *ifp;
{
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	register int off = off0, len;
	register char *cp;
	char *epkt;

	cp = buf;
	epkt = cp + totlen;
	if (off) {
		cp += off + 2 * sizeof(u_short);
		totlen -= 2 * sizeof(u_short);
	}

	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return (0);
	m->m_pkthdr.rcvif = ifp;
	m->m_pkthdr.len = totlen;
	m->m_len = MHLEN;

	while (totlen > 0) {
		if (top) {
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == 0) {
				m_freem(top);
				return (0);
			}
			m->m_len = MLEN;
		}
		len = min(totlen, epkt - cp);
		if (len >= MINCLSIZE) {
			MCLGET(m, M_DONTWAIT);
			if (m->m_flags & M_EXT)
				m->m_len = len = min(len, MCLBYTES);
			else
				len = m->m_len;
		} else {
			/*
			 * Place initial small packet/header at end of mbuf.
			 */
			if (len < m->m_len) {
				if (top == 0 && len + max_linkhdr <= m->m_len)
					m->m_data += max_linkhdr;
				m->m_len = len;
			} else
				len = m->m_len;
		}
		bcopy(cp, mtod(m, caddr_t), (unsigned)len);
		cp += len;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		if (cp == epkt)
			cp = buf;
	}
	return (top);
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
	case SIOCSIFCONF_X25:
		ifp->if_flags |= IFF_UP;
		error = hd_ctlinput(PRC_IFUP, ifa->ifa_addr);
		if (error == 0)
			apxinit(ifp->if_unit);
		break;

	case SIOCSIFADDR:
		ifa->ifa_rtrequest = x25_rtrequest;
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
#endif /* NAPX */
