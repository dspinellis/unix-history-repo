/**************************************************************************/
/*                                                                        */
/*        HMP protocol: see RFC 869, Dec. 1983 for details                */
/*        original version by Craig Partridge (craig@bbn-unix)            */
/*                    January 1985                                        */
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

extern hmp_binding_used();

struct pr_advice hmp_advice =
{
	HM_MAXPORTS,
	HM_MAXPORTS,
	HM_MAXPORTS,
	HM_MAXPORTS,
	sizeof(u_char),
	hmp_binding_used,
} ;

/**************************************************************************/
/*                                                                        */
/*   generate a new HMP inpcb                                             */
/*                                                                        */
/**************************************************************************/

hmp_attach(so,head)
struct socket *so;
struct inpcb *head;
{
    register struct inpcb *inp;
    register struct mbuf *m;
    register struct hmpcb *hp;
    int error = 0;

    if (!(error = soreserve(so,2048,2048)))
    {
	if (!(error = in_pcballoc(so,head)))
	{
	    inp = sotoinpcb(so);
	    inp->inp_lport = inp->inp_fport = 0;

	    if ((m = m_getclr(M_WAIT,MT_PCB)) != (struct mbuf *)0)
	    {
		hp = mtod(m,struct hmpcb *);
		inp->inp_ppcb = (caddr_t) hp;
		return(0);
	    }
	    error = ENOBUFS;
	    in_pcbdetach(inp);
	}
    }

    return(error);
}

/**************************************************************************/
/*                                                                        */
/*    get rid of a no longer used HMP inpcb                               */
/*                                                                        */
/**************************************************************************/

hmp_detach(inp)
register struct inpcb *inp;
{
    register struct hmpcb *hp;
    register int error;

    if (inp == (struct inpcb *)0)
	error = ENOTCONN;
    else
    {
	error = 0;
	hp = intohmpcb(inp);
	(void) m_free(dtom(hp));
	inp->inp_ppcb = (caddr_t)0;

	in_pcbdetach(inp);
    }
    return(error);
}

/**************************************************************************/
/*                                                                        */
/**************************************************************************/

int hmp_binding_used(inp, lport, lsaddr, reuselocal)
struct inpcb   *inp;
u_short	lport;
u_long	lsaddr;
{
    register struct inpcb *i;

    if (reuselocal)
	return(1);

    for (i = hmp.inp_next; i != &hmp; i = i->inp_next)
    {
	/* don't want to find ourself */
	if ((i != inp) && (i->inp_lport == lport))
	    if ((i->inp_laddr.s_addr == lsaddr) ||
	    (i->inp_laddr.s_addr == INADDR_ANY) ||
	    (lsaddr == INADDR_ANY))
	    break;
    }
    return (i != &hmp);
}

/**************************************************************************/
/*                                                                        */
/*     binds and stuffs all hmp garbage into its pcb                      */
/*                                                                        */
/**************************************************************************/

hmp_bind(inp,nam)
struct inpcb *inp;
struct mbuf *nam;
{
    int error;

    register struct hmpcb *hp = intohmpcb(inp);
    register struct sockaddr_hmp *sinh;
    
    sinh = mtod(nam,struct sockaddr_hmp *);

    if (error = in_pcbbind(inp,nam,&hmp_advice))
	return(error);

    /* now do hmp stuff */

    hp->hp_inpcb = inp;
    hp->hp_lsystyp = sinh->sih_systype;
    hp->hp_lmsgtyp = sinh->sih_msgtype;
    hp->hp_lseq = sinh->sih_seqno;
    hp->hp_lpasswd = sinh->sih_passwd;
    hp->hp_flags = sinh->sih_options & (HM_BINDOPTS);

    return(0);
}

/**************************************************************************/
/*                                                                        */
/*   connect to remote end.  All this does is semi-permanently set the    */
/*   destination address.....                                             */
/*                                                                        */
/**************************************************************************/

hmp_connect(inp,nam)
struct inpcb *inp;
struct mbuf *nam;
{
    register struct socket *so = inp->inp_socket;
    register struct sockaddr_hmp *sinh;
    register struct hmpcb *hp;
    register int error = 0;

    if (inp->inp_faddr.s_addr != INADDR_ANY)
	return(EISCONN);

    /* not bound? */
    if (inp->inp_lport==0)
    {
        if (error = in_pcbbind(inp,(struct mbuf *)0),&hmp_advice)
	    return(error);
    }

    if ((nam == (struct mbuf *)0) || (nam->m_len != sizeof(*sinh)))
	return(EINVAL);

    sinh = mtod(nam,struct sockaddr_hmp *);

    if ((sinh->sin_port & 0xff) != sinh->sin_port)
	return(EINVAL);

    inp->inp_fport = sinh->sin_port;
    inp->inp_faddr = sinh->sin_addr;

    /* now do hmp stuff */

    hp = intohmpcb(inp);

    hp->hp_rsystyp = sinh->sih_systype;
    hp->hp_rmsgtyp = sinh->sih_msgtype;
    hp->hp_rseq = sinh->sih_seqno;
    hp->hp_rpasswd = sinh->sih_passwd;
    hp->hp_ctlflg = sinh->sih_ctlflgs;
    hp->hp_flags = sinh->sih_options & HM_CONNOPTS;

    return(0);
}

/**************************************************************************/
/*                                                                        */
/*     break association with a remote address                            */
/*                                                                        */
/**************************************************************************/

hmp_disconnect(inp)
register struct inpcb *inp;
{
    register struct hmpcb *hp;

    if (inp->inp_faddr.s_addr == INADDR_ANY)
	return(ENOTCONN);

    hp = intohmpcb(inp);

    /* clean up the hmpcb */

    hp->hp_rsystyp = 0;
    hp->hp_rmsgtyp = 0;
    hp->hp_rseq = 0;
    hp->hp_rpasswd = 0;
    hp->hp_ctlflg = 0;
    hp->hp_flags &= ~HM_CONNOPTS;

    in_pcbdisconnect(inp);

    return(0);
}
#endif HMP
