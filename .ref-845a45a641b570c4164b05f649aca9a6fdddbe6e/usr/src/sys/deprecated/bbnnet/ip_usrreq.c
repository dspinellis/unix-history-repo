/**************************************************************************/
/*                                                                        */
/*                    miscellaneous ip routines                           */
/*                                                                        */
/**************************************************************************/


#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"
#include "../net/netisr.h"

#include "../vax/mtpr.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"


ip_ioctl (inp, command, data)
struct inpcb *inp;
int command;
caddr_t	data;
{
    /* no IP ioctls */
    return in_ioctl (command, data);
}

ip_ctloutput(req, so, level, optname, optval)
int req;
struct socket *so;
int level, optname;
struct mbuf **optval;
{
    struct inpcb *inp;

    /*
     * O.K., with the berkeley method of using the protocol number for the level,
     * what magic cookie should we use to distinguish between IP and the interfaces?
     */
    inp = sotoinpcb(so);

    switch(req)
    {
	case PRCO_GETOPT:
	    return(ip_getopt(inp,optname,optval));

	case PRCO_SETOPT:
	    return(ip_setopt(inp,optname,optval));

	default:
	    panic("ip_ctloutput");
    }
    /*NOTREACHED*/
}

ip_setopt (inpcb, command, data)
struct inpcb	*inpcb;
struct mbuf	**data;
{
    register int error = 0;
    register struct mbuf *m = *data;

    switch (command)
    {
      case SO_IPROUTE:
	/* this probably breaks!! */
	if (m->m_len == 0)
	{
	    /* turns off use of options */
	    inpcb->inp_optlen = 0;
	    break;
	}
	if ((m->m_len < (2 * sizeof(struct in_addr))) ||
	    (m->m_len > (MAX_IPOPTLEN - sizeof(struct in_addr))) ||
	    (m->m_len % sizeof(struct in_addr)))
	{
	    error = EINVAL;
	    break;
	}
	/*
	 * O.K., user process specifies it as:
	 *      ->A->B->C->D
	 * D must be our final destination (but we can't
	 * check that since we haven't connected yet).
	 * Convert this into a form for ip_output.
	 */
	inpcb->inp_optlen = m->m_len;
	bcopy (mtod(m, caddr_t), inpcb->inp_options, (unsigned)m->m_len);

	/*
	 * Following could be moved to ip_send(), but let's
	 * do it once for efficiency even though user may
	 * retrieve different from what stored.
	 */
	{
	    char	*p;
	    struct in_addr *ipa;

	    p = inpcb->inp_options;
	    ipa = (struct in_addr *) p;
	    ipa[m->m_len / sizeof(struct in_addr)] = ipa[0];
	    p[0] = IP_NOP_OPT;
	    p[1] = IP_LRTE_OPT;
	    p[2] = m->m_len -1;
	    p[3] = 4; /* offset: counting one based */
	}
	/*
	 * Now we have a correct IP source route recorded,
	 * and the first hop comes after the source route.
	 */
	break;
      default:
	error = EINVAL;
    }

    /* they can futz with m */
    if (*data)
	m_freem(*data);

    return (error);
}

ip_getopt (inpcb, command, data)
struct inpcb	*inpcb;
struct mbuf	**data;
{
    register error = 0;
    register struct mbuf *m = NULL;

    *data = NULL;	/* o.k. (no data sent on getsockopt) */

    switch (command)
    {
      case SO_IPROUTE:
	if (!inpcb->inp_optlen)
	    break;

	m = m_get(M_WAIT, MT_SOOPTS);

	if (m == 0)
	    return(ENOBUFS);

	m->m_len = inpcb->inp_optlen;

	bcopy (inpcb->inp_options, mtod(m, caddr_t), (unsigned)m->m_len);
	break;

      default:
	error = EINVAL;
    }
    *data = m;
    return (error);
}

u_char inetctlerrmap[PRC_NCMDS] = 
{
    ENETUNREACH,	/* PRC_IFDOWN: connection oriented protocols use
			 * interface with their local address.  Can't re-route.
			 */

	ECONNABORTED,	0,		0,
	0,		0,		EHOSTDOWN,	EHOSTUNREACH,
	ENETUNREACH,	EHOSTUNREACH,	ECONNREFUSED,	ECONNREFUSED,
	EMSGSIZE,	0,		0,		0,
	0,		0,		0,		0
} ;
