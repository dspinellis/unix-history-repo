/**************************************************************************/
/*                                                                        */
/*        HMP protocol: see RFC 869, Dec. 1983 for details                */
/*                                                                        */
/*   users are warned that this code has not been extensively tested:     */
/*   it was written for use on SUNs in Jan of 1985, by Craig Partridge    */
/*   and has been used a bit by some by local projects.  It was ported    */
/*   to 4.3 to replace some HMP code that had what now goes into a user   */
/*   level server process in the kernel.  This has not been               */
/*   heavily tested.  Think of this as alpha test code...                 */
/*                                                                        */
/**************************************************************************/

#ifdef HMP

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/hmp.h"
#include "../bbnnet/hmp_var.h"

/* hmp pcb queue */
struct inpcb hmp;

/**************************************************************************/
/*                                                                        */
/*     called to initialize hmp control structures                        */
/*                                                                        */
/**************************************************************************/

hmp_init()
{
    hmp.inp_next = hmp.inp_prev = &hmp;
}

/**************************************************************************/
/*                                                                        */
/*   called to handle input packet by IP level.  Does checksum, finds if  */
/*   destination port exists and does any screening if port was bound by  */
/*   application.  Note that it also re-routes poll requests to HOST_PORT */
/*                                                                        */
/**************************************************************************/

hmp_input(m0)
struct mbuf *m0;
{
    struct ip *ih;
    struct mbuf *m;
    struct hmp_hdr *hh;
    struct hmpcb *hp;
    struct inpcb *inp;
    struct in_addr dst, src;
    u_short sav_cksum, port;
    int len;
    static struct sockaddr_hmp hmp_hmp;

    /* grab header */
    m = m0;
    if (((m->m_off > MMAXOFF) || (m->m_len < HMPIPHDR)) &&
	(m = m_pullup(m,HMPIPHDR)) == (struct mbuf *)0)
	return;

    ih = mtod(m,struct ip *);

    /* save what we need from IP header */
    dst = ih->ip_dst;
    src = ih->ip_src;

    len = ih->ip_len;

    /* get rid of IP header */
    m->m_len -= sizeof(struct ip);
    m->m_off += sizeof(struct ip);
    len -= sizeof(struct ip);

    hh = mtod(m,struct hmp_hdr *);

    /* do checksum */
    sav_cksum = hh->hh_cksum;
    hh->hh_cksum = 0;
    if (sav_cksum != in_cksum(m,len))
	goto bad;

    /* are we being polled ?*/
    if (hh->hh_msgtyp == HM_POLL)
	port = HOST_PORT;
    else
	port = hh->hh_port;

    /* find associated pcb if any */
    inp = in_pcblookup(&hmp,ih->ip_src,(u_short)0, dst, port,1);

    if (inp == (struct inpcb *)0)
    {
	goto bad;
    }

    hp = intohmpcb(inp);

    /* is this packet of a type we accept? */

    if (port == HOST_PORT)
    {
	/* check system type and password */

	if ((hp->hp_lsystyp) && (hh->hh_systyp != hp->hp_lsystyp))
	    goto bad;

	if ((hp->hp_lpasswd) && (hh->hh_passwd != hp->hp_lpasswd))
	    goto bad;

    }
    else if ((hp->hp_flags & HM_SEQOPT) && (hh->hh_passwd != hp->hp_lseq))
	goto bad;	/* out of sequence */

    hp->hp_lseq = htons(ntohs(hp->hp_lseq)+1);

    /* censoring message types? */
    /* also see if letting traps thru */

    if ((hp->hp_lmsgtyp) && (hh->hh_msgtyp != hp->hp_lmsgtyp) &&
	(!(hp->hp_flags & HM_TRAPOPT) || (hh->hh_msgtyp != HM_TRAP)))
	goto bad;

    /* construct sockaddr_hmp */

    hmp_hmp.sin_port = hh->hh_port;
    hmp_hmp.sin_addr = src;
    hmp_hmp.sih_seqno = hh->hh_seq;
    hmp_hmp.sih_passwd  = hh->hh_passwd;
    hmp_hmp.sih_systype = hh->hh_systyp;
    hmp_hmp.sih_msgtype = hh->hh_msgtyp;
    hmp_hmp.sih_ctlflgs = hh->hh_ctlflg;
    hmp_hmp.sih_options = 0;

    /* drop HMP header */
    m->m_len -= sizeof(struct hmp_hdr);
    m->m_off += sizeof(struct hmp_hdr);

    if (sbappendaddr(&inp->inp_socket->so_rcv,(struct sockaddr *)&hmp_hmp,
	m, (struct mbuf *)0) == 0)
	goto bad;
    else
	sorwakeup(inp->inp_socket);
    return;

bad :
    m_freem(m);
}

/**************************************************************************/
/*                                                                        */
/*   sends packet.  Checks for some wild values and does some magic if    */
/*   the sender is the HOST_PORT.                                         */
/*                                                                        */
/**************************************************************************/

hmp_output(inp, m0)
register struct inpcb *inp;
struct mbuf *m0;
{
    register struct mbuf *m;
    register struct hmp_hdr *hh;
    register struct ip *ip;
    register struct hmpcb *hp = intohmpcb(inp);
    int len, error;

    /* check for wild message types */

    if ((hp->hp_rmsgtyp <=0) || (hp->hp_rmsgtyp > 102) ||
	((hp->hp_rmsgtyp > 7) && (hp->hp_rmsgtyp < 100)))
    {
	m = m0;
	error = EINVAL;
	goto dropit;
    }

    /* compute length and grab mbuf for HMP/IP header */
    for(m=m0,len=0; m != (struct mbuf *)0; m = m->m_next)
	len += m->m_len;

    if ((m = m_get(M_DONTWAIT,MT_HEADER)) == (struct mbuf *)0)
    {
	m = m0;
	error = ENOBUFS;
	goto dropit;
    }

    /* do HMP header */
    m->m_off = MMAXOFF - sizeof(struct hmp_hdr);
    m->m_len = sizeof(struct hmp_hdr);
    m->m_next = m0;

    hh = mtod(m,struct hmp_hdr *);

    /* traps and responding to polls we are host sending to appl */
    if ((inp->inp_lport == HOST_PORT) || (hp->hp_rmsgtyp == HM_TRAP))
    {
	hh->hh_systyp = hp->hp_lsystyp;
	hh->hh_port = (u_char) inp->inp_fport;
    }
    else
    {
	hh->hh_systyp = hp->hp_rsystyp;
	hh->hh_port = (u_char) inp->inp_lport;
    }

    /* give it the present sequence number */
    if (hp->hp_flags & HM_NUMOPT)
	hh->hh_seq = hp->hp_rseq;
    else
	hh->hh_seq = hp->hp_lseq;

    len += sizeof(struct hmp_hdr);
    /* fill in the rest */
    hh->hh_passwd = hp->hp_rpasswd;
    hh->hh_msgtyp = hp->hp_rmsgtyp;
    hh->hh_ctlflg = hp->hp_ctlflg;
    hh->hh_cksum = 0;
    hh->hh_cksum = in_cksum(m,len);

    /* IP header */
    m->m_off -= sizeof(struct ip);
    m->m_len += sizeof(struct ip);

    ip = mtod(m,struct ip *);

    ip->ip_hl = ip->ip_v = 0;
    ip->ip_tos = 0;
    ip->ip_id = ip->ip_off = 0;
    ip->ip_p = IPPROTO_HMP;
    ip->ip_len = htons(len);
    ip->ip_src = inp->inp_laddr;
    ip->ip_dst = inp->inp_faddr;
    ip->ip_ttl = MAXTTL;

    return(ip_send(inp,m,(int)len,0));

dropit :
    m_freem(m);
    return(error);
}

/**************************************************************************/
/*                                                                        */
/*              basically one large dispatch table.                       */
/*                                                                        */
/**************************************************************************/

/* ARGSUSED */
hmp_usrreq(so,req,m,nam,rights)
struct socket *so;
int req;
struct mbuf *m, *nam, *rights;
{
    register struct inpcb *inp = sotoinpcb(so);
    register int error = 0;

    if ((rights != (struct mbuf *)0) && (rights->m_len))
    {
	error = EINVAL;
	goto release;
    }

    switch(req)
    {
      case PRU_ATTACH:
	error = hmp_attach(so,&hmp);
	break;

      case PRU_DETACH:
	error = hmp_detach(inp);
	break;

      case PRU_BIND:
	error = hmp_bind(inp,nam);
	break;

      case PRU_CONNECT:
	error = hmp_connect(inp,nam);
	if (error == 0)
	    soisconnected(so);
	break;

      case PRU_DISCONNECT:
	error = hmp_disconnect(inp);
	if (error == 0)
	    soisdisconnected(so);
	break;

      case PRU_SHUTDOWN:
	socantsendmore(so);
	break;

      case PRU_SEND:
	{
	    struct in_addr sav_addr;

	    /* if nam not 0 then doing sendto(), else send() */
	    if (nam != (struct mbuf *)0)
	    {
		sav_addr = inp->inp_laddr;
		if (inp->inp_faddr.s_addr != INADDR_ANY)
		{
		    error = EISCONN;
		    break;
		}
		if (error = hmp_connect(inp,nam))
		    break;
	    }
	    else if (inp->inp_faddr.s_addr == INADDR_ANY)
	    {
		error = ENOTCONN;
		break;
	    }

	    error = hmp_output(inp,m);
	    m = (struct mbuf *)0;

	    if (nam != (struct mbuf *)0)
	    {
		hmp_disconnect(inp);
		inp->inp_laddr = sav_addr;
	    }
	}
	break;

      case PRU_ABORT:
	hmp_detach(inp);
	hmp_disconnect(inp);
	sofree(so);
	break;

      case PRU_SOCKADDR:
	in_setsockaddr(inp,nam);
	break;

      case PRU_PEERADDR:
	in_setpeeraddr(inp,nam);
	break;

      case PRU_CONTROL:
	/* not our ioctl, let lower level try ioctl */
	error = ip_ioctl (inp, (int) m, (caddr_t) nam);
	m = (struct mbuf *) NULL;	/* don't want it freed */
	break;

      case PRU_SENSE:
	m = (struct mbuf *)0;
	/* fall thru.... */

      case PRU_LISTEN:
      case PRU_RCVD:
      case PRU_RCVOOB:
      case PRU_FASTTIMO:
      case PRU_SLOWTIMO:
      case PRU_PROTORCV:
      case PRU_PROTOSEND:
      case PRU_SENDOOB:
      case PRU_CONNECT2:
      case PRU_ACCEPT:
	error = EOPNOTSUPP;
	break;

      default:
	panic("hmp_usrreq");
    }

release :
    if (m != (struct mbuf *)0)
	m_freem(m);

    return(error);
}

/**************************************************************************/
/*                                                                        */
/*                                                                        */
/**************************************************************************/

hmp_abort(inp)
register struct inpcb *inp;
{
    register struct socket *so = inp->inp_socket;

    hmp_disconnect(inp);
    in_pcbdisconnect(inp);
    soisdisconnected(so);
}

/**************************************************************************/
/*                                                                        */
/**************************************************************************/

hmp_ctloutput(req, so, level, optname, optval)
int req;
struct socket *so;
int level, optname;
struct mbuf **optval;
{
    int s = splnet(); /* like PRU/packet/timer entry into net code */
    int error;

    /*
     * see comments by tcp_ctloutput()
     */
    if (level == HMPROTO)
    {
	struct inpcb *inp;

	inp = sotoinpcb(so);

	switch(req)
	{
	  case PRCO_GETOPT:
	    error = hmp_getopt(inp,optname,optval);
	    break;

	  case PRCO_SETOPT:
	    error = hmp_setopt(inp,optname,optval);
	    break;

	  default:
	    panic("hmp_ctloutput");
	}
    } else
        error = ip_ctloutput(req,so,level,optname,optval);

    splx(s);
    return (error);
}

/**************************************************************************/
/*                                                                        */
/**************************************************************************/

hmp_getopt(inp,optname,optval)
struct inpcb *inp;
int optname;
struct mbuf **optval;
{
    int error = 0;

    switch (optname)
    {
#ifdef HMPTRAPS
      case SOI_MONHOST:
	error = getmonhosts(optval);
	break;
#endif

      default:
	error = EOPNOTSUPP;
	break;
    }

    return(error);
}

/**************************************************************************/
/*                                                                        */
/**************************************************************************/

hmp_setopt(inp, optname, optval)
struct inpcb *inp;
int optname;
struct mbuf **optval;
{
    register error = 0;

    switch(optname)
    {
#ifdef HMPTRAP
      case SOI_MONHOST:
	error = setmonhosts(*optval);
	break;
#endif


      default:
	error = EOPNOTSUPP;
    }

    /* clean up for setsockopt */
    if (*optval  != 0)
	m_freem(*optval);

    return(error);
}

hmp_ctlinput (prc_code, arg)
caddr_t arg;
{
}
#endif HMP
