/*	ns_input.c	6.1	85/05/30	*/

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "ns.h"
#include "ns_if.h"
#include "ns_pcb.h"
#include "idp.h"
#include "idp_var.h"
#include "ns_error.h"

/*
 * NS initialization.
 */
union ns_host	ns_thishost;
union ns_host	ns_zerohost;
union ns_host	ns_broadhost;

static char allones[] = {-1, -1, -1, -1, -1, -1};

struct nspcb nspcb;
struct nspcb nsrawpcb;

struct ifqueue	nsintrq;
int	nsqmaxlen = IFQ_MAXLEN;

int	idpcksum = 0;

ns_init()
{
	ns_broadhost = * (union ns_host *) allones;
	nspcb.nsp_next = nspcb.nsp_prev = &nspcb;
	nsrawpcb.nsp_next = nsrawpcb.nsp_prev = &nsrawpcb;
	nsintrq.ifq_maxlen = nsqmaxlen;
}

/*
 * Idp input routine.  Pass to next level.
 */
int nsintr_getpck = 0;
int nsintr_swtch = 0;
nsintr()
{
	register struct idp *idp;
	register struct mbuf *m;
	struct nspcb *nsp;
	struct mbuf *m0;
	register int i;
	int len, s, error;
	char oddpacketp;

next:
	/*
	 * Get next datagram off input queue and get IDP header
	 * in first mbuf.
	 */
	s = splimp();
	IF_DEQUEUE(&nsintrq, m);
	splx(s);
	nsintr_getpck++;
	if (m == 0)
		return;
	if ((m->m_off > MMAXOFF || m->m_len < sizeof (struct idp)) &&
	    (m = m_pullup(m, sizeof (struct idp))) == 0) {
		idpstat.idps_toosmall++;
		goto next;
	}

	/*
	 * Give any raw listeners a crack at the packet
	 */
	for (nsp = nsrawpcb.nsp_next; nsp != &nsrawpcb; nsp = nsp->nsp_next) {
		struct mbuf *m1 = m_copy(m, 0, M_COPYALL);
		if (m1) idp_input(m1, nsp);
	}

	idp = mtod(m, struct idp *);
	len = ntohs(idp->idp_len);
	if (oddpacketp = len & 1) {
		len++;		/* If this packet is of odd length,
				   preserve garbage byte for checksum */
	}

	/*
	 * Check that the amount of data in the buffers
	 * is as at least much as the IDP header would have us expect.
	 * Trim mbufs if longer than we expect.
	 * Drop packet if shorter than we expect.
	 */
	i = -len;
	m0 = m;
	for (;;) {
		i += m->m_len;
		if (m->m_next == 0)
			break;
		m = m->m_next;
	}
	if (i != 0) {
		if (i < 0) {
			idpstat.idps_tooshort++;
			m = m0;
			goto bad;
		}
		if (i <= m->m_len)
			m->m_len -= i;
		else
			m_adj(m0, -i);
	}
	m = m0;
	if (idpcksum && ((i = idp->idp_sum)!=0xffff)) {
		idp->idp_sum = 0;
		if (i != (idp->idp_sum = ns_cksum(m,len))) {
			idpstat.idps_badsum++;
			if (ns_hosteqnh(ns_thishost, idp->idp_dna.x_host))
				error = NS_ERR_BADSUM;
			else
				error = NS_ERR_BADSUM_T;
			ns_error(m, error, 0);
			goto next;
		}
	}
	/*
	 * Is this a directed broadcast?
	 */
	if (ns_hosteqnh(ns_broadhost,idp->idp_dna.x_host)) {
		if ((ns_netof(idp->idp_dna)!=ns_netof(idp->idp_sna)) &&
		   (ns_netof(idp->idp_dna)!=-1) && (ns_netof(idp->idp_sna)!=0)
		   && (ns_netof(idp->idp_dna)!=0)) {
			/*
			 * Look to see if I need to eat this packet as well.
			 */
			struct ns_ifaddr *ia =
					ns_iaonnetof(idp->idp_dna.x_net);
			if (ia) {
				m = m_copy(m, 0, M_COPYALL);
			} else
				m = NULL;
			idp_forward(idp);
			if (m==NULL)
				goto next;
			idp = mtod(m, struct idp *);
		}
	/*
	 * Is this our packet? If not, forward.
	 */
	} else if (!ns_hosteqnh(ns_thishost,idp->idp_dna.x_host)) {
		idp_forward(idp);
		goto next;
	}

	/*
	 * Locate pcb for datagram.
	 */
	nsp = ns_pcblookup(&idp->idp_sna, idp->idp_dna.x_port, NS_WILDCARD);


	 /*
	  * Switch out to protocol's input routine.
	  */

	nsintr_swtch++;
	if (nsp) {
		if (oddpacketp) {
			m_adj(m0, -1);
		}
		switch (idp->idp_pt) {
			case NSPROTO_SPP:
				spp_input(m,nsp);
				break;
			case NSPROTO_ERROR:
				ns_err_input(m);
				break;
			default:
				idp_input(m,nsp);
		}
	} else {
		/* don't send ERROR response for multicast packet */
		if (idp->idp_dna.x_host.c_host[0] & 1)
			goto bad;
		ns_error(m, NS_ERR_NOSOCK, 0);
	}
	goto next;

bad:
	m_freem(m);
	goto next;
}

u_char nsctlerrmap[PRC_NCMDS] = {
	ECONNABORTED,	ECONNABORTED,	0,		0,
	0,		0,		EHOSTDOWN,	EHOSTUNREACH,
	ENETUNREACH,	EHOSTUNREACH,	ECONNREFUSED,	ECONNREFUSED,
	EMSGSIZE,	0,		0,		0,
	0,		0,		0,		0
};

idp_ctlinput(cmd, arg)
	int cmd;
	caddr_t arg;
{
	struct ns_addr *ns;
	int idp_abort();
	int type;

	if (cmd < 0 || cmd > PRC_NCMDS)
		return;
	if (nsctlerrmap[cmd] == 0)
		return;		/* XXX */
	type = NS_ERR_UNREACH_HOST;
	if (cmd == PRC_IFDOWN)
		ns = &((struct sockaddr_ns *)arg)->sns_addr;
	else if (cmd == PRC_HOSTDEAD || cmd == PRC_HOSTUNREACH)
		ns = (struct ns_addr *)arg;
	else {
		ns = &((struct ns_errp *)arg)->ns_err_idp.idp_dna;
		type = ((struct ns_errp *)arg)->ns_err_num;
		type = ntohs(type);
	}
	switch (type) {
	case NS_ERR_UNREACH_HOST:
	case NS_ERR_NOSOCK:
		ns_pcbnotify(ns, (int)nsctlerrmap[cmd], idp_abort, 0);
	}
}

int	idpprintfs = 0;
int	idpforwarding = 1;
/*
 * Forward a packet.  If some error occurs return the sender
 * an error packet.  Note we can't always generate a meaningful
 * error message because the NS errors don't have a large enough repetoire
 * of codes and types.
 */
struct route idp_droute;
struct route idp_sroute;

idp_forward(idp)
	register struct idp *idp;
{
	register int error, type, code;
	struct mbuf *mopt, *mcopy;

	if (idpprintfs) {
		printf("forward: src ");
		ns_printhost(&idp->idp_sna);
		printf(", dst ");
		ns_printhost(&idp->idp_dna);
		printf("hop count %d\n", idp->idp_tc);
	}
	if (idpforwarding == 0) {
		/* can't tell difference between net and host */
		type = NS_ERR_UNREACH_HOST, code = 0;
		goto senderror;
	}
	idp->idp_tc++;
	if (idp->idp_tc > NS_MAXHOPS) {
		type = NS_ERR_TOO_OLD, code = 0;
		goto senderror;
	}
	/* need to adjust checksum */
	if (idp->idp_sum!=0xffff) {
		union bytes {
			u_char c[4];
			u_short s[2];
			long l;
		} x;
		register int shift;
		x.l = 0; x.c[0] = 1;
		shift = (((((int)ntohs(idp->idp_len))+1)>>1)-2) & 0xf;
		x.l = idp->idp_sum + (x.l << shift);
		x.l = x.s[0] + x.s[1];
		x.l = x.s[0] + x.s[1];
		if (x.l==0xffff) idp->idp_sum = 0; else idp->idp_sum = x.l;
	}
	mopt = m_get(M_DONTWAIT, MT_DATA);
	if (mopt == NULL) {
		m_freem(dtom(idp));
		return;
	}

	/*
	 * Save at most 42 bytes of the packet in case
	 * we need to generate an NS error message to the src.
	 */
	mcopy = m_copy(dtom(idp), 0, imin(ntohs(idp->idp_len), 42));

	if ((idp->idp_dna.x_host.c_host[0] & 0x1) == 0)
		error = ns_output(dtom(idp), (struct route *)0, NS_FORWARDING);
	/*
	 * Here we are about to forward a broadcast packet,
	 * so we try to insure that it doesn't go back out
	 * on the interface it came in on.
	 */
	else if (idp_do_route(&idp->idp_dna,&idp_droute)) {
		if (idp_do_route(&idp->idp_sna,&idp_sroute)) {
		    struct ifnet *ifp;

		    if (idp_droute.ro_rt &&
			(ifp=idp_droute.ro_rt->rt_ifp) &&
		        idp_sroute.ro_rt &&
			(ifp!=idp_sroute.ro_rt->rt_ifp)) {
				error = ns_output(dtom(idp), &idp_droute,
					      NS_FORWARDING|NS_ALLOWBROADCAST);
		    }
		    idp_undo_route(&idp_sroute);
		}
		idp_undo_route(&idp_droute);
	}

	if (error == 0) {
		if (mcopy)
			m_freem(mcopy);
		return;
	}
	if (mcopy == NULL)
		return;
	idp = mtod(mcopy, struct idp *);
	type = NS_ERR_UNSPEC_T, code = 0;
	switch (error) {

	case ENETUNREACH:
	case EHOSTDOWN:
	case EHOSTUNREACH:
	case ENETDOWN:
	case EPERM:
		type = NS_ERR_UNREACH_HOST;
		break;

	case EMSGSIZE:
		type = NS_ERR_TOO_BIG;
		code = 576; /* too hard to figure out mtu here */
		break;

	case ENOBUFS:
		type = NS_ERR_UNSPEC_T;
		break;
	}
senderror:
	ns_error(dtom(idp), type, code);
}

idp_do_route(src, ro)
struct ns_addr *src;
struct route *ro;
{
	
	struct sockaddr_ns *dst;

	bzero((caddr_t)ro, sizeof (*ro));
	dst = (struct sockaddr_ns *)&ro->ro_dst;

	dst->sns_family = AF_NS;
	dst->sns_addr = *src;
	rtalloc(ro);
	if (ro->ro_rt == 0 || ro->ro_rt->rt_ifp == 0) {
		return(0);
	}
	ro->ro_rt->rt_use++;
	return(1);
}

idp_undo_route(ro)
register struct route *ro;
{
	if (ro->ro_rt) {RTFREE(ro->ro_rt);}
}
ns_watch_output(m)
struct mbuf *m;
{
	register struct nspcb *nsp;
	/*
	 * Give any raw listeners a crack at the packet
	 */
	for (nsp = nsrawpcb.nsp_next; nsp != &nsrawpcb; nsp = nsp->nsp_next) {
		struct mbuf *m1 = m_copy(m, 0, M_COPYALL);
		if (m1) idp_input(m1, nsp);
	}
}
