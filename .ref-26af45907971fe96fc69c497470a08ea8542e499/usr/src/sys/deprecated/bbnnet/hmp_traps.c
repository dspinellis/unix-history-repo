/**************************************************************************/
/*                                                                        */
/*  code to support kernel generated traps -- user's can send their own   */
/*  thru normal channels.                                                 */
/*                                                                        */
/**************************************************************************/

#if HMP && HMPTRAPS

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
#include "../bbnnet/hmp_traps.h"

/* 
 * list of monitoring hosts that want to get traps
 * uses a sockaddr_hmp so you can specify address & port. doesn't use
 * other fields but is the most common structure for saving port/host info.
 */

static struct sockaddr_hmp mon_hosts[MAX_MONHOSTS];
static int num_monhosts = 0;

/*
 * learn who gets traps
 */

getmonhosts(optval)
struct mbuf **optval;
{
    struct mbuf *m;

    if ((m = m_getclr(M_WAIT,MT_DATA)) == 0)
	return(ENOBUFS);

    if (num_monhosts)
    {
	m->m_len = num_monhosts * sizeof(struct sockaddr_hmp);
	bcopy((caddr_t)mon_hosts,mtod(m,caddr_t),(unsigned)m->m_len);
    }
    else
    {
	/* would like to return nothing, but can't */
	m->m_len = sizeof(struct sockaddr_hmp);
	bzero(mtod(m,caddr_t),m->m_len);
    }

    *optval = m;
    return(0);
}

/* 
 * changing who gets our traps 
 */

setmonhosts(optval)
struct mbuf *optval;
{
    register unsigned len;

    if (!suser())
	return(u.u_error);

    if (optval == 0)
    {
	/* clearing table */
	num_monhosts = 0;
	return(0);
    }

    len = (optval)->m_len;

    /* rational data ? */
    if (((len/sizeof(struct sockaddr_hmp)) == 0) || 
	((len % sizeof(struct sockaddr_hmp)) != 0))
	return(EFAULT);

    if (len > sizeof(mon_hosts))
	return(ENOBUFS);	/* close enough to true error */

    bcopy(mtod(optval,caddr_t),(caddr_t)mon_hosts,len);

    num_monhosts = len / sizeof(struct sockaddr_hmp);

    return(0);
}

/*
 * what kernel calls to send stuff 
 */

/* nobody does anything but copy this */
static struct hmpcb trappcb =
{
    	0,		0,		HM_43HOST,	HM_43HOST,
	HM_TRAP,	HM_TRAP,	0,		0,
	0,		0,		0,
} ;

hmp_trap(code, data, len)
int code;
caddr_t data;
unsigned len;
{
    register int i;
    struct mbuf *m;
    struct inpcb inp;
    struct hmpcb hp;
    struct hmp_trap ht;

    if (num_monhosts == 0)
	return;

    if (len > (MLEN - sizeof(ht)))
    {
	printf("hmp_trap: trap size too big\n");
	return;
    }
    bcopy((caddr_t)&trappcb,(caddr_t)&hp,sizeof(hp));
    bzero((caddr_t)&inp,sizeof(inp));

    /* this may not be necessary but careful never hurts */
    inp.inp_ppcb = &hp;
    hp.hp_inpcb = &inp;

    /* fill in trap data leader */
    ht.ht_time = iptime();
    ht.ht_type = htonl((u_long)code);


    for(i=0; i < num_monhosts; i++)
    {
	/* get mbuf for packet */
	m = m_getclr(M_DONTWAIT,MT_DATA);

	if (m == 0)
	    return;

	bcopy((caddr_t)&ht,mtod(m,caddr_t),sizeof(ht));
	bcopy(data,mtod(m,caddr_t)+sizeof(ht),len);

	m->m_len = len + sizeof(ht);

	/* stuff remote address and port */
	inp.inp_faddr = mon_hosts[i].sin_addr;
	inp.inp_fport = mon_hosts[i].sin_port;

	(void) hmp_output(&inp,m);
    }
}

#endif
