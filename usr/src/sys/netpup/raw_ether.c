/*	%M%	%I%	%E%	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../netpup/ether.h"
#include "../vaxif/if_en.h"

/*
 * Raw interface to 3Mb/s Ethernet.
 */

struct	sockaddr etherlink = { AF_ETHERLINK };
/*
 * Generate Ethernet header and pass packet to output
 * routine.  The user must create a skeletal header
 * in order to message type.  Source and destination
 * field are filled in from send parameters.
 */
raw_enoutput(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	register struct en_header *en;
	register struct sockaddr_en *sen;
	register struct rawcb *rp = sotorawcb(so);
	struct ifnet *ifp;
	int error = 0;

	/*
	 * Verify user has supplied necessary space for the header.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof (struct en_header)) &&
	    (m = m_pullup(m, sizeof (struct en_header))) == 0) {
		error = EMSGSIZE;	/* XXX */
		goto bad;
	}
	en = mtod(m, struct en_header *);
	sen = (struct sockaddr_en *)&rp->rcb_faddr;
	en->en_dhost = sen->sen_host;
	if (rp->rcb_route.ro_rt == 0)
		ifp = if_ifonnetof(sen->sen_net);
	else {
		rp->rcb_route.ro_rt->rt_use++;
		ifp = rp->rcb_route.ro_rt->rt_ifp;
	}
	if (ifp == 0) {
		error = ENETUNREACH;
		goto bad;
	}
	en->en_shost = ifp->if_host[0];
	return ((*ifp->if_output)(ifp, m, &etherlink));
bad:
	m_freem(m);
	return (error);
}
