/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
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
 *	@(#)if_ethersubr.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "if.h"
#include "netisr.h"
#include "route.h"
#include "if_llc.h"

#include "machine/mtpr.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_var.h"
#include "../netinet/if_ether.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#ifdef ISO
#include "../netiso/argo_debug.h"
#include "../netiso/iso.h"
#include "../netiso/iso_var.h"
#endif

u_char	etherbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
extern	struct ifnet loif;

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 * Assumes that ifp is actually pointer to arpcom structure.
 */
ether_output(ifp, m0, dst)
	register struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	short type;
	int s, error = 0;
 	u_char edst[6];
	struct in_addr idst;
	register struct mbuf *m = m0;
	struct mbuf *mcopy = (struct mbuf *)0;
	register struct ether_header *eh;
	int usetrailers, off;
#define	ac ((struct arpcom *)ifp)

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}
	if (ifp->if_flags & IFF_SIMPLEX && dst->sa_family != AF_UNSPEC &&
	    !bcmp((caddr_t)edst, (caddr_t)etherbroadcastaddr, sizeof (edst)))
		mcopy = m_copy(m, 0, (int)M_COPYALL);
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
 		if (!arpresolve(ac, m, &idst, edst, &usetrailers))
			return (0);	/* if not yet resolved */
		if ((ifp->if_flags & IFF_SIMPLEX) && (*edst & 1))
		    mcopy = m_copy(m, 0, (int)M_COPYALL);
		off = m->m_pkthdr.len - m->m_len;
		if (usetrailers && off > 0 && (off & 0x1ff) == 0 &&
		    (m->m_flags & M_EXT) == 0 &&
		    m->m_data >= m->m_pktdat + 2 * sizeof (u_short)) {
			type = ETHERTYPE_TRAIL + (off>>9);
			m->m_data -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = htons((u_short)ETHERTYPE_IP);
			*(mtod(m, u_short *) + 1) = htons((u_short)m->m_len);
			goto gottrailertype;
		}
		type = ETHERTYPE_IP;
		goto gottype;
#endif
#ifdef NS
	case AF_NS:
		type = ETHERTYPE_NS;
		if (!bcmp((caddr_t)edst, (caddr_t)&ns_thishost, sizeof(edst)))
			return(looutput(&loif, m, dst));
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		    (caddr_t)edst, sizeof (edst));
		if ((ifp->if_flags & IFF_SIMPLEX) && (*edst & 1))
		    mcopy = m_copy(m, 0, (int)M_COPYALL);
		goto gottype;
#endif
#ifdef	ISO
	case AF_ISO: {
		int	len;
		int	ret;
		struct	llc *l;

		if ((ret = iso_tryloopback(m, (struct sockaddr_iso *)dst)) >= 0)
			return (ret);
		ret = iso_snparesolve(ifp, (struct sockaddr_iso *)dst,
					(char *)edst, &len);
		if (ret > 0) {
			m_freem(m); /* Not Resolved */
			return(ret);
		}
		M_PREPEND(m, 3, M_DONTWAIT);
		if (m == NULL)
			return(0);
		type = m->m_pkthdr.len;
		l = mtod(m, struct llc *);
		l->llc_dsap = l->llc_ssap = LLC_ISO_LSAP;
		l->llc_control = LLC_UI;
		IFDEBUG(D_ETHER)
			int i;
			printf("unoutput: sending pkt to: ");
			for (i=0; i<6; i++)
				printf("%x ", edst[i] & 0xff);
			printf("\n");
		ENDDEBUG
		} goto gottype;
#endif	ISO
	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		goto gottype;

	default:
		printf("%s%d: can't handle af%d\n", ifp->if_name, ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottrailertype:
	/*
	 * Packet to be sent as trailer: move first packet
	 * (control information) to end of chain.
	 */
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;

gottype:
	/*
	 * Add local net header.  If no space in first mbuf,
	 * allocate another.
	 */
	M_PREPEND(m, sizeof (struct ether_header), M_DONTWAIT);
	if (m == 0) {
		error = ENOBUFS;
		goto bad;
	}
	eh = mtod(m, struct ether_header *);
	type = htons((u_short)type);
	bcopy((caddr_t)&type,(caddr_t)&eh->ether_type,
		sizeof(eh->ether_type));
 	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof (edst));
 	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)eh->ether_shost,
	    sizeof(eh->ether_shost));
	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		error = ENOBUFS;
		goto bad;
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if ((ifp->if_flags & IFF_OACTIVE) == 0)
		error = (*ifp->if_start)(ifp);
	splx(s);
	if (mcopy)
		(void) looutput(&loif, mcopy, dst);
	return (error);

bad:
	if (mcopy)
		m_freem(mcopy);
	if (m)
		m_freem(m);
	return (error);
}

/*
 * Pull packet off interface.  Off is nonzero if packet
 * has trailing header; we still have to drop
 * the type and length which are at the front of any trailer data.
 */
ether_input(ifp, eh, m)
	struct ifnet *ifp;
	register struct ether_header *eh;
	struct mbuf *m;
{
	register struct ifqueue *inq;
	register struct llc *l;
	int s;

	if (bcmp((caddr_t)etherbroadcastaddr, (caddr_t)eh->ether_dhost,
	    sizeof(etherbroadcastaddr)) == 0)
		m->m_flags |= M_BCAST;
	else if (eh->ether_dhost[0] & 1)
		m->m_flags |= M_MCAST;

	switch (eh->ether_type) {
#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput((struct arpcom *)ifp, m);
		return;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif
	default:
		if (eh->ether_type > ETHERMTU)
			goto dropanyway;
		l = mtod(m, struct llc *);
		switch (l->llc_control) {
		case LLC_UI:
		/* LLC_UI_P forbidden in class 1 service */
		    if ((l->llc_dsap == LLC_ISO_LSAP) &&
			(l->llc_ssap == LLC_ISO_LSAP)) {
#ifdef	ISO
				/* LSAP for ISO */
			M_PREPEND(m, sizeof *eh, M_DONTWAIT);
			if (m == 0)
				return;
			*mtod(m, struct ether_header *) = *eh;
			IFDEBUG(D_ETHER)
			    printf("clnp packet");
			ENDDEBUG
			schednetisr(NETISR_ISO);
			inq = &clnlintrq;
			if (IF_QFULL(inq)){
				IFDEBUG(D_ETHER)
				    printf(" qfull\n");
				ENDDEBUG
				IF_DROP(inq);
				m_freem(m);
			} else {
				IF_ENQUEUE(inq, m);
				IFDEBUG(D_ETHER)
				    printf(" queued\n");
				ENDDEBUG
			}
			return;
#endif	ISO
		    }
		    break;
		case LLC_XID:
		case LLC_XID_P:
		    if(m->m_len < 6)
			goto dropanyway;
		    l->llc_window = 0;
		    l->llc_fid = 9;
		    l->llc_class = 1;
		    l->llc_dsap = l->llc_ssap = 0;
		    /* Fall through to */
		case LLC_TEST:
		case LLC_TEST_P:
		{
		    struct sockaddr sa;
		    register struct ether_header *eh2;
		    int i;
		    u_char c = l->llc_dsap;
		    l->llc_dsap = l->llc_ssap;
		    l->llc_ssap = c;
		    if (m->m_flags & (M_BCAST | M_MCAST))
			bcopy((caddr_t)ac->ac_enaddr,
			      (caddr_t)eh->ether_dhost, 6);
		    sa.sa_family = AF_UNSPEC;
		    sa.sa_len = sizeof(sa);
		    eh2 = (struct ether_header *)sa.sa_data;
		    for (i = 0; i < 6; i++) {
			eh2->ether_shost[i] = c = eh->ether_dhost[i];
			eh2->ether_dhost[i] = 
				eh->ether_dhost[i] = eh->ether_shost[i];
			eh->ether_shost[i] = c;
		    }
		    ifp->if_output(ifp, m, &sa);
		    return;
		}
		dropanyway:
		default:
		    m_freem(m);
		    return;
	    }
	}

	s = splimp();
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);
	splx(s);
}

/*
 * Convert Ethernet address to printable (loggable) representation.
 */
static char digits[] = "0123456789abcdef";
char *
ether_sprintf(ap)
	register u_char *ap;
{
	register i;
	static char etherbuf[18];
	register char *cp = etherbuf;

	for (i = 0; i < 6; i++) {
		*cp++ = digits[*ap >> 4];
		*cp++ = digits[*ap++ & 0xf];
		*cp++ = ':';
	}
	*--cp = 0;
	return (etherbuf);
}
