/* tcp.c - MIB realization of the TCP group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/tcp.c,v 7.12 91/02/22 09:44:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/tcp.c,v 7.12 91/02/22 09:44:42 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	tcp.c,v $
 * Revision 7.12  91/02/22  09:44:42  mrose
 * Interim 6.8
 * 
 * Revision 7.11  91/01/08  12:48:45  mrose
 * update
 * 
 * Revision 7.10  90/12/18  10:14:14  mrose
 * update
 * 
 * Revision 7.9  90/10/17  14:33:18  mrose
 * update
 * 
 * Revision 7.8  90/10/02  09:55:05  mrose
 * normalize again
 * 
 * Revision 7.7  90/07/09  14:49:43  mrose
 * sync
 * 
 * Revision 7.6  90/06/05  20:47:27  mrose
 * touch-up
 * 
 * Revision 7.5  90/04/23  00:08:16  mrose
 * touch-up
 * 
 * Revision 7.4  90/04/18  08:52:06  mrose
 * oid_normalize
 * 
 * Revision 7.3  90/02/27  18:50:07  mrose
 * unix stuff
 * 
 * Revision 7.2  90/01/27  08:22:09  mrose
 * touch-up
 * 
 * Revision 7.1  89/12/11  10:40:40  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:23:35  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "mib.h"

#include "internet.h"
#if defined(BSD44) || defined (BSD43)
#include <sys/param.h>
#endif
#include <sys/protosw.h>
#include <sys/socketvar.h>
#include <net/route.h>
#include <netinet/in_systm.h>
#ifdef	BSD44
#include <netinet/ip.h>
#endif
#include <netinet/in_pcb.h>
#include <netinet/tcp.h>
#include <netinet/tcp_fsm.h>
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>

/*  */

#define	RTOA_OTHER	1		/* tcpRtoAlgorithm */
#define	RTOA_VANJ	4		/*   ..  */

#define	MXCN_NONE	(-1)		/* tcpMaxConn */


static struct tcpstat tcpstat;

static int tcpConnections;

/*  */

#define	tcpRtoAlgorithm	0
#define	tcpRtoMin	1
#define	tcpRtoMax	2
#define	tcpMaxConn	3
#define	tcpActiveOpens	4
#define	tcpPassiveOpens	5
#define	tcpAttemptFails	6
#define	tcpEstabResets	7
#define	tcpCurrEstab	8
#define	tcpInSegs	9
#define	tcpOutSegs	10
#define	tcpRetransSegs	11
#ifndef	SUNOS4
#define	tcpInErrs	12
#else
#undef	tcpInErrs	12
#endif
#undef	tcpOutRsts	13		/* NOT IMPLEMENTED */


static int  o_tcp (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register struct tcpstat *tcps = &tcpstat;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    static   int lastq = -1;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1
		    || oid -> oid_elements[oid -> oid_nelem - 1] != 0)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = 0;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else
		return NOTOK;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case tcpCurrEstab:
	    if (get_connections (type_SNMP_PDUs_get__request) == NOTOK)
		return generr (offset);
	    break;

	default:
	    if (quantum != lastq) {
		lastq = quantum;

		if (getkmem (nl + N_TCPSTAT, (caddr_t) tcps, sizeof *tcps)
		        == NOTOK)
		    return generr (offset);
	    }
	    break;
    }

    switch (ifvar) {
	case tcpRtoAlgorithm:
#ifdef	TCPTV_REXMTMAX
	    return o_integer (oi, v, RTOA_VANJ);
#else
	    return o_integer (oi, v, RTOA_OTHER);
#endif

	case tcpRtoMin:
	    return o_integer (oi, v, TCPTV_MIN * 100);	/* milliseconds */

#ifdef	TCPTV_REXMTMAX
	case tcpRtoMax:
	    return o_integer (oi, v, TCPTV_REXMTMAX * 100); /*   .. */
#endif

	case tcpMaxConn:
	    return o_integer (oi, v, MXCN_NONE);

#ifdef	TCPTV_REXMTMAX
	case tcpActiveOpens:
	    return o_integer (oi, v, tcps -> tcps_connattempt);

	case tcpPassiveOpens:
	    return o_integer (oi, v, tcps -> tcps_accepts);

	case tcpAttemptFails:
	    return o_integer (oi, v, tcps -> tcps_conndrops);

	case tcpEstabResets:
	    return o_integer (oi, v, tcps -> tcps_drops);
#endif

	case tcpCurrEstab:
	    return o_integer (oi, v, tcpConnections);

#ifdef	TCPTV_REXMTMAX
	case tcpInSegs:
	    return o_integer (oi, v, tcps -> tcps_rcvtotal);

	case tcpOutSegs:
	    return o_integer (oi, v, tcps -> tcps_sndtotal
			           - tcps -> tcps_sndrexmitpack);

	case tcpRetransSegs:
	    return o_integer (oi, v, tcps -> tcps_sndrexmitpack);
#endif

#ifdef	tcpInErrs
	case tcpInErrs:
#if	!defined(BSD44) && !(defined(BSD43) && defined(ultrix))
	    return o_integer (oi, v, tcps -> tcps_badsegs);
#else
	    return o_integer (oi, v, tcps -> tcps_rcvbadsum
				   + tcps -> tcps_rcvbadoff
				   + tcps -> tcps_rcvshort);
#endif
#endif

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static	int	tcp_states[TCP_NSTATES];


struct tcptab {
#define	TT_SIZE	10			/* object instance */
    unsigned int   tt_instance[TT_SIZE];

    struct inpcb   tt_pcb;		/* protocol control block */

    struct socket  tt_socb;		/* socket info */

    struct tcpcb   tt_tcpb;		/* TCP info */

    struct tcptab *tt_next;
};

static struct tcptab *tts = NULL;

static	int	flush_tcp_cache = 0;


struct tcptab *get_tcpent ();

/*  */

#define	tcpConnState	0
#define	tcpConnLocalAddress 1
#define	tcpConnLocalPort 2
#define	tcpConnRemAddress 3
#define	tcpConnRemPort	4
#define	unixTcpConnSendQ 5
#define	unixTcpConnRecvQ 6


static int  o_tcp_conn (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register int    i;
    register unsigned int *ip,
			  *jp;
    register struct tcptab *tt;
    struct sockaddr_in netaddr;
    register OID    oid = oi -> oi_name;
    OID	    new;
    register OT	    ot = oi -> oi_type;

    if (get_connections (offset) == NOTOK)
	return generr (offset);

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + TT_SIZE)
		return int_SNMP_error__status_noSuchName;
	    if ((tt = get_tcpent (oid -> oid_elements + oid -> oid_nelem
				      - TT_SIZE, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if ((i = oid -> oid_nelem - ot -> ot_name -> oid_nelem) != 0
		    && i < TT_SIZE) {
		for (jp = (ip = oid -> oid_elements + ot -> ot_name -> oid_nelem - 1) + i;
		         jp > ip;
		         jp--)
		    if (*jp != 0)
			break;
		if (jp == ip)
		    oid -> oid_nelem = ot -> ot_name -> oid_nelem;
		else {
		    if ((new = oid_normalize (oid, TT_SIZE - i, 65536))
			    == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = oid = new;
		}
	    }
	    else
		if (i > TT_SIZE)
		    oid -> oid_nelem = ot -> ot_name -> oid_nelem + TT_SIZE;

	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		if ((tt = tts) == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, TT_SIZE)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - TT_SIZE;
		jp = tt -> tt_instance;
		for (i = TT_SIZE; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		if ((tt = get_tcpent (ip = oid -> oid_elements
				         + oid -> oid_nelem - TT_SIZE, 1))
		        == NULL)
		    return NOTOK;

		jp = tt -> tt_instance;
		for (i = TT_SIZE; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case tcpConnState:
	    if ((i = tt -> tt_tcpb.t_state) < 0
		    || i >= sizeof tcp_states / sizeof tcp_states[0])
		return generr (offset);
	    return o_integer (oi, v, tcp_states[i]);
	
	case tcpConnLocalAddress:
	    netaddr.sin_addr = tt -> tt_pcb.inp_laddr;	/* struct copy */
	    return o_ipaddr (oi, v, &netaddr);
	    
	case tcpConnLocalPort:
	    return o_integer (oi, v, ntohs (tt -> tt_pcb.inp_lport) & 0xffff);

	case tcpConnRemAddress:
	    netaddr.sin_addr = tt -> tt_pcb.inp_faddr;	/* struct copy */
	    return o_ipaddr (oi, v, &netaddr);

	case tcpConnRemPort:
	    return o_integer (oi, v, ntohs (tt -> tt_pcb.inp_fport) & 0xffff);

	case unixTcpConnSendQ:
	    return o_integer (oi, v, tt -> tt_socb.so_snd.sb_cc);
	
	case unixTcpConnRecvQ:
	    return o_integer (oi, v, tt -> tt_socb.so_rcv.sb_cc);
	
	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static int  tt_compar (a, b)
struct tcptab **a,
	      **b;
{
    return elem_cmp ((*a) -> tt_instance, TT_SIZE,
		     (*b) -> tt_instance, TT_SIZE);
}


static int  get_connections (offset)
int	offset;
{
    register int    i;
    register unsigned int  *cp;
    register struct tcptab *ts,
			   *tp,
			  **tsp;
    register struct inpcb  *ip;
    struct inpcb *head,
		  tcb;
    struct nlist nzs;
    register struct nlist *nz = &nzs;
    static   int first_time = 1;
    static   int lastq = -1;

    if (quantum == lastq)
	return OK;
    if (!flush_tcp_cache
	    && offset == type_SNMP_PDUs_get__next__request
	    && quantum == lastq + 1) {			/* XXX: caching! */
	lastq = quantum;
	return OK;
    }
    lastq = quantum, flush_tcp_cache = 0;

    for (ts = tts; ts; ts = tp) {
	tp = ts -> tt_next;

	free ((char *) ts);
    }
    tts = NULL;

    if (getkmem (nl + N_TCB, (char *) &tcb, sizeof tcb) == NOTOK)
	return NOTOK;
    head = (struct inpcb *) nl[N_TCB].n_value;

    tsp = &tts, i = 0;
    ip = &tcb;
    while (ip -> inp_next != head) {
	if ((ts = (struct tcptab *) calloc (1, sizeof *ts)) == NULL)
	    adios (NULLCP, "out of memory");
			    /* no check needed for duplicate connections... */
	*tsp = ts, tsp = &ts -> tt_next, i++;

	nz -> n_name = "struct inpcb",
	nz -> n_value = (unsigned long) ip -> inp_next;
	if (getkmem (nz, (caddr_t) &ts -> tt_pcb, sizeof ts -> tt_pcb)
	        == NOTOK)
	    return NOTOK;
	ip = &ts -> tt_pcb;

	nz ->n_name = "struct socket",
	nz -> n_value = (unsigned long) ip -> inp_socket;
	if (getkmem (nz, (caddr_t) &ts -> tt_socb, sizeof ts -> tt_socb)
	        == NOTOK)
	    return NOTOK;

	nz ->n_name = "struct tcb",
	nz -> n_value = (unsigned long) ip -> inp_ppcb;
	if (getkmem (nz, (caddr_t) &ts -> tt_tcpb, sizeof ts -> tt_tcpb)
	        == NOTOK)
	    return NOTOK;

	cp = ts -> tt_instance;
	cp += ipaddr2oid (cp, &ip -> inp_laddr);
	*cp++ = ntohs (ip -> inp_lport) & 0xffff;
	cp += ipaddr2oid (cp, &ip -> inp_faddr);
	*cp++ = ntohs (ip -> inp_fport) & 0xffff;

	if (debug && first_time) {
	    OIDentifier	oids;

	    oids.oid_elements = ts -> tt_instance;
	    oids.oid_nelem = TT_SIZE;
	    advise (LLOG_DEBUG, NULLCP,
		    "add connection: %s", sprintoid (&oids));
	}
    }
    first_time = 0;

    if ((tcpConnections = i) > 1) {
	register struct tcptab **base,
			       **tse;

	if ((base = (struct tcptab **) malloc ((unsigned) (i * sizeof *base)))
	        == NULL)
	    adios (NULLCP, "out of memory");

	tse = base;
	for (ts = tts; ts; ts = ts -> tt_next)
	    *tse++ = ts;

	qsort ((char *) base, i, sizeof *base, tt_compar);

	tsp = base;
	ts = tts = *tsp++;

	while (tsp < tse) {
	    ts -> tt_next = *tsp;
	    ts = *tsp++;
	}
	ts -> tt_next = NULL;

	free ((char *) base);
    }

    return OK;    
}

/*  */

static struct tcptab *get_tcpent (ip, isnext)
register unsigned int *ip;
int	isnext;
{
    register struct tcptab *tt;

    for (tt = tts; tt; tt = tt -> tt_next)
	switch (elem_cmp (tt -> tt_instance, TT_SIZE, ip, TT_SIZE)) {
	    case 0:
	        if (!isnext)
		    return tt;
		if ((tt = tt -> tt_next) == NULL)
		    goto out;
		/* else fall... */

	    case 1:
		return (isnext ? tt : NULL);
	}

out: ;
    flush_tcp_cache = 1;

    return NULL;
}

/*  */

init_tcp () {
    register OT	    ot;

    if (ot = text2obj ("tcpRtoAlgorithm"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpRtoAlgorithm;
    if (ot = text2obj ("tcpRtoMin"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpRtoMin;
#ifdef	TCPTV_REXMTMAX
    if (ot = text2obj ("tcpRtoMax"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpRtoMax;
#endif
    if (ot = text2obj ("tcpMaxConn"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpMaxConn;
#ifdef	TCPTV_REXMTMAX
    if (ot = text2obj ("tcpActiveOpens"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpActiveOpens;
    if (ot = text2obj ("tcpPassiveOpens"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpPassiveOpens;
    if (ot = text2obj ("tcpAttemptFails"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpAttemptFails;
    if (ot = text2obj ("tcpEstabResets"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpEstabResets;
#endif
    if (ot = text2obj ("tcpCurrEstab"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpCurrEstab;
#ifdef	TCPTV_REXMTMAX
    if (ot = text2obj ("tcpInSegs"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpInSegs;
    if (ot = text2obj ("tcpOutSegs"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpOutSegs;
    if (ot = text2obj ("tcpRetransSegs"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpRetransSegs;
#endif
#ifdef	tcpInErrs
    if (ot = text2obj ("tcpInErrs"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpInErrs;
#endif
#ifdef	tcpOutRsts
    if (ot = text2obj ("tcpOutRsts"))
	ot -> ot_getfnx = o_tcp,
	ot -> ot_info = (caddr_t) tcpOutRsts;
#endif

    if (ot = text2obj ("tcpConnState"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) tcpConnState;
    if (ot = text2obj ("tcpConnLocalAddress"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) tcpConnLocalAddress;
    if (ot = text2obj ("tcpConnLocalPort"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) tcpConnLocalPort;
    if (ot = text2obj ("tcpConnRemAddress"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) tcpConnRemAddress;
    if (ot = text2obj ("tcpConnRemPort"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) tcpConnRemPort;

    if (ot = text2obj ("unixTcpConnSendQ"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) unixTcpConnSendQ;
    if (ot = text2obj ("unixTcpConnRecvQ"))
	ot -> ot_getfnx = o_tcp_conn,
	ot -> ot_info = (caddr_t) unixTcpConnRecvQ;

    tcp_states[TCPS_CLOSED] = 1;
    tcp_states[TCPS_LISTEN] = 2;
    tcp_states[TCPS_SYN_SENT] = 3;
    tcp_states[TCPS_SYN_RECEIVED] = 4;
    tcp_states[TCPS_ESTABLISHED] = 5;
    tcp_states[TCPS_CLOSE_WAIT] = 8;
    tcp_states[TCPS_FIN_WAIT_1] = 6;
    tcp_states[TCPS_CLOSING] = 10;
    tcp_states[TCPS_LAST_ACK] = 9;
    tcp_states[TCPS_FIN_WAIT_2] = 7;
    tcp_states[TCPS_TIME_WAIT] = 11;
}
