/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
 *	@(#)if_enp.c	7.5 (Berkeley) %G%
 */

#include "enp.h"
#if NENP > 0
/*
 * CMC ENP-20 Ethernet Controller.
 */
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "ioctl.h"
#include "errno.h"
#include "vmparam.h"
#include "syslog.h"
#include "uio.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"
#endif
#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#include "../tahoe/cpu.h"
#include "../tahoe/pte.h"
#include "../tahoe/mtpr.h"

#include "../tahoevba/vbavar.h"
#include "../tahoeif/if_enpreg.h"

#define ENPSTART	0xf02000	/* standard enp start addr */
#define	ENPUNIT(dev)	(minor(dev))	/* for enp ram devices */
/* macros for dealing with longs in i/o space */
#define	ENPGETLONG(a)	((((u_short *)(a))[0] << 16)|(((u_short *)(a))[1]))
#define	ENPSETLONG(a,v) \
   { register u_short *wp = (u_short *)(a); \
     wp[0] = ((u_short *)&(v))[0]; wp[1] = ((u_short *)&(v))[1];}

int	enpprobe(), enpattach(), enpintr();
long	enpstd[] = { 0xfff41000, 0xfff61000, 0 };
struct  vba_device *enpinfo[NENP];
struct  vba_driver enpdriver = 
    { enpprobe, 0, enpattach, 0, enpstd, "enp", enpinfo, "enp-20", 0 };

int	enpinit(), enpioctl(), enpreset(), enpoutput(), enpstart();
struct  mbuf *enpget();

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * es_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct  enp_softc {
	struct  arpcom es_ac;           /* common ethernet structures */
#define es_if		es_ac.ac_if
#define es_addr	es_ac.ac_enaddr
	short	es_ivec;		/* interrupt vector */
} enp_softc[NENP]; 
extern	struct ifnet loif;

enpprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register br, cvec;		/* must be r12, r11 */
	register struct enpdevice *addr = (struct enpdevice *)reg;
	struct enp_softc *es = &enp_softc[vi->ui_unit];

#ifdef lint
	br = 0; cvec = br; br = cvec;
	enpintr(0);
#endif
	if (badaddr((caddr_t)addr, 2) || badaddr((caddr_t)&addr->enp_ram[0], 2))
		return (0);
	es->es_ivec = --vi->ui_hd->vh_lastiv;
	addr->enp_state = S_ENPRESET;		/* reset by VERSAbus reset */
	br = 0x14, cvec = es->es_ivec;		/* XXX */
	return (sizeof (struct enpdevice));
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets. 
 */
enpattach(ui)
	register struct vba_device *ui;
{
	struct enp_softc *es = &enp_softc[ui->ui_unit];
	register struct ifnet *ifp = &es->es_if;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "enp";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = enpinit;
	ifp->if_ioctl = enpioctl;
	ifp->if_output = ether_output;
	ifp->if_start = enpstart;
	ifp->if_reset = enpreset;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
	if_attach(ifp);
}

/*
 * Reset of interface after "system" reset.
 */
enpreset(unit, vban)
	int unit, vban;
{
	register struct vba_device *ui;

	if (unit >= NENP || (ui = enpinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_vbanum != vban)
		return;
	printf(" enp%d", unit);
	enpinit(unit);
}

/*
 * Initialization of interface; clear recorded pending operations.
 */
enpinit(unit)
	int unit;
{
	struct enp_softc *es = &enp_softc[unit];
	register struct vba_device *ui = enpinfo[unit];
	struct enpdevice *addr;
	register struct ifnet *ifp = &es->es_if;
	int s;

	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		addr = (struct enpdevice *)ui->ui_addr;
		s = splimp();
		RESET_ENP(addr);
		DELAY(200000);
		es->es_if.if_flags |= IFF_RUNNING;
		splx(s);
	}
}

/*
 * Ethernet interface interrupt.
 */
enpintr(unit)
	int unit;
{
	register struct enpdevice *addr;
	register BCB *bcbp;

	addr = (struct enpdevice *)enpinfo[unit]->ui_addr;
#if ENP == 30
	if (!IS_ENP_INTR(addr))
		return;
	ACK_ENP_INTR(addr);
#endif
	while ((bcbp = (BCB *)ringget((RING *)&addr->enp_tohost )) != 0) {
		enpread(&enp_softc[unit], bcbp);
		(void) ringput((RING *)&addr->enp_enpfree, bcbp); 
	}
}

/*
 * Read input packet, examine its packet type, and enqueue it.
 */
enpread(es, bcbp)
	struct enp_softc *es;
	register BCB *bcbp;
{
	register struct ether_header *enp;
	struct mbuf *m;
	int s, len, off, resid;

	es->es_if.if_ipackets++; 
	/*
	 * Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	len = bcbp->b_msglen - sizeof (struct ether_header);
	enp = (struct ether_header *)ENPGETLONG(&bcbp->b_addr);
#define enpdataaddr(enp, off, type) \
    ((type)(((caddr_t)(((char *)enp)+sizeof (struct ether_header))+(off))))
	enp->ether_type = ntohs((u_short)enp->ether_type);
	if (enp->ether_type >= ETHERTYPE_TRAIL &&
	    enp->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (enp->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;
		enp->ether_type = ntohs(*enpdataaddr(enp, off, u_short *));
		resid = ntohs(*(enpdataaddr(enp, off+2, u_short *)));
		if (off + resid > len)
			return;
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; enpget will then force this header
	 * information to be at the front.
	 */
	m = enpget((u_char *)enp, len, off, &es->es_if);
	if (m == 0)
		return;
	ether_input(&es->es_if, enp, m);
}

enpstart(ifp)
	struct ifnet *ifp;
{

	if (enpput(ifp))
		return (ENOBUFS);
	else
		return (0);
}

/*
 * Routine to copy from mbuf chain to transmitter buffer on the VERSAbus.
 */
enpput(ifp)
struct ifnet *ifp;
{
	register BCB *bcbp;
	register struct enpdevice *addr;
	register struct mbuf *mp;
	register u_char *bp;
	register u_int len;
	int unit = ifp->if_unit, ret = 1;
	struct mbuf *m;

	addr = (struct enpdevice *)enpinfo[unit]->ui_addr;
again:
	if (ringempty((RING *)&addr->enp_hostfree))  {
	/*	ifp->if_flags |= IFF_OACTIVE; */
		return (ret);
	}
	IF_DEQUEUE(&ifp->if_snd, m);
	if (m == 0) {
		ifp->if_flags &= ~IFF_OACTIVE;
		return (0);
	}
	bcbp = (BCB *)ringget((RING *)&addr->enp_hostfree);
	bcbp->b_len = 0;
	bp = (u_char *)ENPGETLONG(&bcbp->b_addr);
	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		enpcopy(mtod(mp, u_char *), bp, len);
		bp += len;
		bcbp->b_len += len;
	}
	bcbp->b_len = max(ETHERMIN+sizeof (struct ether_header), bcbp->b_len);
	bcbp->b_reserved = 0;
	if (ringput((RING *)&addr->enp_toenp, bcbp) == 1)
		INTR_ENP(addr);
	m_freem(m);
	ret = 0;
	goto again;
}

/*
 * Routine to copy from VERSAbus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
enpget(rxbuf, totlen, off, ifp)
	u_char *rxbuf;
	int totlen, off;
	struct ifnet *ifp;
{
	register u_char *cp;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len;
	u_char *packet_end;

	rxbuf += sizeof (struct ether_header);
	cp = rxbuf;
	packet_end = cp + totlen;
	if (off) {
		off += 2 * sizeof(u_short);
		totlen -= 2 *sizeof(u_short);
		cp = rxbuf + off;
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
		len = min(totlen, (packet_end - cp));
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
		enpcopy(cp, mtod(m, u_char *), (u_int)len);
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		cp += len;
		if (cp == packet_end)
			cp = rxbuf;
	}
	return (top);
}

enpcopy(from, to, cnt)
	register u_char *from, *to;
	register u_int cnt;
{
	register c;
	register short *f, *t;

	if (((int)from&01) && ((int)to&01)) {
		/* source & dest at odd addresses */
		*to++ = *from++;
		--cnt;
	}
	if (cnt > 1 && (((int)to&01) == 0) && (((int)from&01) == 0)) {
		t = (short *)to;
		f = (short *)from;
		for (c = cnt>>1; c; --c)	/* even address copy */
			*t++ = *f++;
		cnt &= 1;
		if (cnt) {			/* odd len */
			from = (u_char *)f;
			to = (u_char *)t;
			*to = *from;
		}
	}
	while ((int)cnt-- > 0)	/* one of the address(es) must be odd */
		*to++ = *from++;
}

/*
 * Process an ioctl request.
 */
enpioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct enpdevice *addr;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			enpinit(ifp->if_unit);
			((struct arpcom *)ifp)->ac_ipaddr =
			    IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS: {
			struct ns_addr *ina = &IA_SNS(ifa)->sns_addr;
			struct enp_softc *es = &enp_softc[ifp->if_unit];

			if (!ns_nullhost(*ina)) {
				ifp->if_flags &= ~IFF_RUNNING;
				addr = (struct enpdevice *)
				    enpinfo[ifp->if_unit]->ui_addr;
				enpsetaddr(ifp->if_unit, addr,
				    ina->x_host.c_host);
			} else
				ina->x_host = *(union ns_host *)es->es_addr;
			enpinit(ifp->if_unit);
			break;
		}
#endif
		default:
			enpinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags&IFF_UP) == 0 && ifp->if_flags&IFF_RUNNING) {
			enpinit(ifp->if_unit);		/* reset board */
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags&IFF_UP &&
		     (ifp->if_flags&IFF_RUNNING) == 0)
			enpinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

enpsetaddr(unit, addr, enaddr)
	int unit;
	struct enpdevice *addr;
	u_char *enaddr;
{

	enpcopy(enaddr, addr->enp_addr.e_baseaddr.ea_addr,
	    sizeof (struct ether_addr));
	enpinit(unit);
	enpgetaddr(unit, addr);
}

enpgetaddr(unit, addr)
	int unit;
	struct enpdevice *addr;
{
	struct enp_softc *es = &enp_softc[unit];

	enpcopy(addr->enp_addr.e_baseaddr.ea_addr, es->es_addr,
	    sizeof (struct ether_addr));
	printf("enp%d: hardware address %s\n",
	    unit, ether_sprintf(es->es_addr));
}

/* 
 * Routines to synchronize enp and host.
 */
#ifdef notdef
static
ringinit(rp, size)
	register RING *rp;
{

	rp->r_rdidx = rp->r_wrtidx = 0;
	rp->r_size = size;
}

static
ringfull(rp)
	register RING *rp;
{
	register short idx;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	return (idx == rp->r_rdidx);
}

static
fir(rp)
	register RING *rp;
{

	return (rp->r_rdidx != rp->r_wrtidx ? rp->r_slot[rp->r_rdidx] : 0);
}
#endif

static
ringempty(rp)
	register RING *rp;
{

	return (rp->r_rdidx == rp->r_wrtidx);
}

static
ringput(rp, v)
	register RING *rp;
	BCB *v;
{
	register int idx;

	idx = (rp->r_wrtidx + 1) & (rp->r_size-1);
	if (idx != rp->r_rdidx) {
		ENPSETLONG(&rp->r_slot[rp->r_wrtidx], v);
		rp->r_wrtidx = idx;
		if ((idx -= rp->r_rdidx) < 0)
			idx += rp->r_size;
		return (idx);			/* num ring entries */
	}
	return (0);
}

static
ringget(rp)
	register RING *rp;
{
	register int i = 0;

	if (rp->r_rdidx != rp->r_wrtidx) {
		i = ENPGETLONG(&rp->r_slot[rp->r_rdidx]);
		rp->r_rdidx = (++rp->r_rdidx) & (rp->r_size-1);
	}
	return (i);
}

/*
 * ENP Ram device.
 */
enpr_open(dev)
	dev_t dev;
{
	register int unit = ENPUNIT(dev);
	struct vba_device *ui;
	struct enpdevice *addr;

	if (unit >= NENP || (ui = enpinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    (addr = (struct enpdevice *)ui->ui_addr) == 0)
		return (ENODEV);
	if (addr->enp_state != S_ENPRESET)
		return (EACCES);  /* enp is not in reset state, don't open  */
	return (0);
}

/*ARGSUSED*/
enpr_close(dev)
	dev_t dev;
{

	return (0);
}

enpr_read(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	register struct iovec *iov;
	struct enpdevice *addr;

	if (uio->uio_offset > RAM_SIZE)
		return (ENODEV);
	iov = uio->uio_iov;
	if (uio->uio_offset + iov->iov_len > RAM_SIZE)
		iov->iov_len = RAM_SIZE - uio->uio_offset;
	addr = (struct enpdevice *)enpinfo[ENPUNIT(dev)]->ui_addr;
	if (useracc(iov->iov_base, (unsigned)iov->iov_len, 0) == 0)
		return (EFAULT);
	enpcopy((u_char *)&addr->enp_ram[uio->uio_offset],
	    (u_char *)iov->iov_base, (u_int)iov->iov_len);
	uio->uio_resid -= iov->iov_len;
	iov->iov_len = 0;
	return (0);
}

enpr_write(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	register struct enpdevice *addr;
	register struct iovec *iov;

	addr = (struct enpdevice *)enpinfo[ENPUNIT(dev)]->ui_addr;
	iov = uio->uio_iov;
	if (uio->uio_offset > RAM_SIZE)
		return (ENODEV);
	if (uio->uio_offset + iov->iov_len > RAM_SIZE)
		iov->iov_len = RAM_SIZE - uio->uio_offset;
	if (useracc(iov->iov_base, (unsigned)iov->iov_len, 1) == 0)
		return (EFAULT);
	enpcopy((u_char *)iov->iov_base,
	    (u_char *)&addr->enp_ram[uio->uio_offset], (u_int)iov->iov_len);
	uio->uio_resid -= iov->iov_len;
	iov->iov_len = 0;
	return (0);
}

/*ARGSUSED*/
enpr_ioctl(dev, cmd, data)
	dev_t dev;
	caddr_t data;
{
	register unit = ENPUNIT(dev);
	struct enpdevice *addr;

	addr = (struct enpdevice *)enpinfo[unit]->ui_addr;
	switch(cmd) {

	case ENPIOGO:
		ENPSETLONG(&addr->enp_base, addr);
		addr->enp_intrvec = enp_softc[unit].es_ivec;
		ENP_GO(addr, ENPSTART);
		DELAY(200000);
		enpinit(unit);
		/*
		 * Fetch Ethernet address after link level
		 * is booted (firmware copies manufacturer's
		 * address from on-board ROM).
		 */
		enpgetaddr(unit, addr);
		addr->enp_state = S_ENPRUN;
		break;

	case ENPIORESET:
		RESET_ENP(addr);
		addr->enp_state = S_ENPRESET;
		DELAY(100000);
		break;
	default:
		return (EINVAL);
	}
	return (0);
}
#endif
