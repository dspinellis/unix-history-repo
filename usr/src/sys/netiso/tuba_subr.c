/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_subr.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "proc.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"

#include "../net/route.h"
#include "../net/if.h"

#include "in.h"
#include "in_systm.h"
#include "ip.h"
#include "in_pcb.h"
#include "ip_var.h"
#include "ip_icmp.h"
#include "tcp.h"
#include "tcp_fsm.h"
#include "tcp_seq.h"
#include "tcp_timer.h"
#include "tcp_var.h"
#include "tcpip.h"

#include "netiso/argo_debug.h"
#include "netiso/iso.h"
#include "netiso/clnp.h"
#include "netiso/iso_pcb.h"
#include "netiso/iso_var.h"

#include "tuba_addr.h"
/*
 * Tuba initialization
 */
tuba_init()
{
#define TUBAHDRSIZE (3 /*LLC*/ + 9 /*CLNP Fixed*/ + 42 /*Addresses*/ \
		     + 6 /*CLNP Segment*/ + 20 /*TCP*/)
	extern struct isopcb tuba_isopcb;

	tuba_isopcb.isop_next = tuba_isopcb.isop_prev = &tuba_isopcb;
	if (max_protohdr < TUBAHDRSIZE)
		max_protohdr = TUBAHDRSIZE;
	if (max_linkhdr + TUBAHDRSIZE > MHLEN)
		panic("tuba_init");
	tuba_timer_init();
}

tuba_output(tp, m)
	struct tcpcb *tp;
	register struct mbuf *m;
{
	struct isopcb *isop = (struct isopcb *)tp->t_tuba_pcb;
	register struct tcpiphdr *n = tp->tp_template;

	if (n->ni_sum == 0) {
		register struct inpcb *inp = tp->tp_inpcb;
		register struct tuba_cache *tc;
		u_long cksum_fixup;

		if ((tc = tuba_table[inp->in_faddr.s_addr]) == 0)
			return (ENOBUFS);
		cksum_fixup = tc->tc_sum_out; /* includes -index */
		if ((tc = tuba_table[inp->in_laddr.s_addr]) == 0)
			return (ENOBUFS);
		ICKSUM(cksum_fixup, cksum_fixup + tc->tc_sum_out);
		n->ti_sum = cksum_fixup;
		n = mtod(m, struct tcpiphdr *);
		ICKSUM(n->ti_sum, cksum_fixup + n->ti_sum);
	}
	m->m_len -= sizeof (struct ip);
	m->m_pkthdr.len -= sizeof (struct ip);
	m->m_data += sizeof (struct ip);
	return (clnp_output(m, isop, m->m_pkthdr.len, 0));
}

tuba_refcnt(isop, delta)
	struct isopcb *isop;
{
	register struct tuba_cache *tc;
	unsigned index;

	if (delta != 1)
		delta = -1;
	if (isop == 0 || isop->isop_faddr == 0 || isop->isop_laddr == 0 ||
	    (delta == -1 && isop->isop_tuba_cached == 0) ||
	    (delta == 1 && isop->isop_tuba_cached != 0))
		return;
	isop->isop_tuba_cached = (delta == 1);
	if ((index = tuba_lookup(&isop->isop_faddr.siso_addr)) != 0 &&
	    (tc = tuba_cache[index]) != 0 && (delta == 1 || tc->tc_refcnt > 0))
		tc->tc_refcnt += delta;
	if ((index = tuba_lookup(&isop->isop_laddr.siso_addr)) != 0 &&
	    (tc = tuba_cache[index]) != 0 && (delta == 1 || tc->tc_refcnt > 0))
		tc->tc_refcnt += delta;
}

tuba_pcbdetach(isop)
	struct isopcb *isop;
{
	if (isop == 0)
		return;
	tuba_refcnt(isop, -1);
	isop->isop_socket = 0;
	iso_pcbdetach(isop);
}

static struct sockaddr_iso null_siso = { sizeof(null_siso), AF_ISO, };
/*
 * Avoid  in_pcbconnect in faked out tcp_input()
 */
tuba_pcbconnect(inp, nam)
	register struct inpcb *inp;
	struct mbuf *nam;
{
	register struct sockaddr_iso *siso = mtod(m, struct sockaddr_iso *);
	struct sockaddr_in *sin = mtod(m, struct sockaddr_in *);
	struct tcpcb *tp = intotcpcb(inp);
	unsigned index = sin->sin_addr.s_addr;
	struct tuba_cache *tc = tuba_table[index];
	struct isopcb *isop = (struct isopcb *)tp->tp_tuba_pcb;
	int error;

	inp->inp_faddr.s_addr = index;
	inp->inp_fport = sin->sin_port;
	*siso = null_siso;
	siso->siso_addr = tc->tc_addr;
	siso->siso_tlen = sizeof(inp->inp_fport);
	bcopy((caddr_t)&inp->inp_fport, TSEL(siso), sizeof(inp->inp_fport));
	nam->m_len = sizeof(*siso);
	if ((error = iso_pcbconnect(isop, nam)) == 0)
		tuba_refcnt(isop, 1);
	return (error);
}

/*
 * CALLED FROM:
 * 	clnp's input routine, indirectly through the protosw.
 * FUNCTION and ARGUMENTS:
 * Take a packet (m) from clnp, strip off the clnp header and give it to tcp
 * or udp.
 * No return value.  
 */
ProtoHook
tpclnp_input(m, src, dst, clnp_len, ce_bit)
	register struct mbuf *m;
	struct sockaddr_iso *src, *dst;
	int clnp_len, ce_bit;
{
	int s = splnet();
	unsigned long fix_csum, lindex, findex;
	register struct tcpiphdr *ti;
	register struct inpcb *inp;
	struct mbuf *om = 0;
	int len, tlen, off;
	register struct tcpcb *tp = 0;
	int tiflags;
	struct socket *so;
	int todrop, acked, ourfinisacked, needoutput = 0;
	short ostate;
	struct in_addr laddr;
	int dropsocket = 0, iss = 0;

	if ((m->m_flags & M_PKTHDR) == 0)
		panic("tuba_input");
	/*
	 * Do some housekeeping looking up CLNP addresses.
	 * If we are out of space might as well drop the packet now.
	 */
	tcpstat.tcps_rcvtotal++;
	if ((lindex = tuba_lookup(&dst->siso_addr) == 0 ||
	    (findex = tuba_lookup(&dst->siso_addr) == 0))
		goto drop;
	/*
	 * Get CLNP and TCP header together in first mbuf.
	 * CLNP gave us an mbuf chain WITH the clnp header pulled up,
	 * and the length of the clnp header.
	 */
	len = clnp_len + sizeof(struct tcphdr);
	if (m->m_len < len) {
		if ((m = m_pullup(m, len)) == 0) {
			tcpstat.tcps_rcvshort++;
			return;
		}
	}
	/*
	 * Calculate checksum of extended TCP header and data,
	 * by adjusting the checksum for missing parts of the header.
	 */
	m->m_data += clnp_len;
	m->m_len -= clnp_len;
	tlen = m->m_pkthdr.len -= clnp_len;
	ICKSUM(fix_cksum, tuba_table[findex]->tc_sum_in + htons((u_short)tlen)
		+ tuba_table[lindex]->tc_sum_in + in_cksum(m, tlen));
	if (fix_cksum != 0xffff) {
		tcpstat.tcps_rcvbadsum++;
		goto drop;
	}
	m->m_data -= sizeof(struct ip);
	m->m_len += sizeof(struct ip);
	m->m_pkthdr.len += sizeof(struct ip);
	/*
	 * The reassembly code assumes it will be overwriting a useless
	 * part of the packet, which is why we need to have ti point
	 * into the packet itself.
	 *
	 * Check to see if the data is properly alligned
	 * so that we can save copying the tcp header.
	 * This code knows way too much about the structure of mbufs!
	 */
	off = ((sizeof (long) - 1) & ((m->m_flags & EXT) ?
		(m->m_data - m->m_ext.ext_buf) :  (m->m_data - m_pktdat)));
	if (off) {
		struct mbuf *m0 = m_gethdr(M_DONTWAIT, MT_DATA)
		if (m0 == 0) {
			goto drop;
		}
		bcopy(mtod(m, caddr_t) + sizeof(struct ip),
		      mtod(m0, caddr_t) + sizeof(struct ip),
		      sizeof(struct tcphdr));
		m->m_data += sizeof(struct tcpiphdr);
		m0->m_next = m;
		m0->m_pkthdr = m->m_pkthdr;
		m0->m_flags = m->m_pkthdr & M_COPYFLAGS;
		m0->m_len = sizeof(struct tcpiphdr);
		m = m0;
	}
	ti = mtod(m, struct tcpiphdr *);
	ti->ti_src.s_addr = findex;
	ti->ti_dst.s_addr = lindex;
	ti->ti_len = tlen;
	/*
	 * Now include the rest of TCP input
	 */
#define TUBA_INPUT
#define in_pcbconnect	tuba_pcbconnect

#include "../netinet/tcp_input.c"
}
