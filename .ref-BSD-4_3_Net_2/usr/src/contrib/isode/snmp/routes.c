/* routes.c - MIB support of the routing tables */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/routes.c,v 7.5 91/02/22 09:43:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/routes.c,v 7.5 91/02/22 09:43:51 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	routes.c,v $
 * Revision 7.5  91/02/22  09:43:51  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/18  10:13:45  mrose
 * update
 * 
 * Revision 7.3  90/10/17  14:33:22  mrose
 * update
 * 
 * Revision 7.2  90/05/22  20:30:31  mrose
 * cache
 * 
 * Revision 7.1  89/12/01  08:25:48  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:23:21  mrose
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
#include "interfaces.h"
#include "routes.h"

/*  */

static int routeNumber;
static struct rtetab  *rts = NULL;
static struct rtetab **rtp;

struct rtetab *rts_inet = NULL;
#ifdef	BSD44
struct rtetab *rts_iso = NULL;
#endif

static	int	first_time = 1;
static	int	flush_rt_cache = 0;

/*  */

static int  rt_compar (a, b)
register struct rtetab **a,
		       **b;
{
    int	    i;

    if ((i = (*a) -> rt_dst.sa.sa_family - (*b) -> rt_dst.sa.sa_family))
	return (i > 0 ? 1 : -1);

    return elem_cmp ((*a) -> rt_instance, (*a) -> rt_insize,
		     (*b) -> rt_instance, (*b) -> rt_insize);
}


int	get_routes (offset)
int	offset;
{
    register int   i;
    int	    rthashsize,
	    tblsize;
#ifdef	ultrix
    struct rtentry **rtaddr,
		**rtnet,
		**rthost;
#else
    struct mbuf **rtaddr,
		**rtnet,
		**rthost;
#endif
    register struct rtetab  *rt,
			    *rp;
    struct nlist nzs;
    register struct nlist *nz = &nzs;
    static   int lastq = -1;

    if (quantum == lastq)
	return OK;
    if (!flush_rt_cache
	    && offset == type_SNMP_PDUs_get__next__request
	    && quantum == lastq + 1) {			/* XXX: caching! */
	lastq = quantum;
	return OK;
    }
    lastq = quantum, flush_rt_cache = 0;

    for (rt = rts; rt; rt = rp) {
	rp = rt -> rt_next;

	free ((char *) rt);
    }
    rts = rts_inet = NULL;
#ifdef	BSD44
    rts_iso = NULL;
#endif

    rtp = &rts, routeNumber = 0;
#ifdef	BSD44
    if (nl[N_RADIX_NODE_HEAD].n_value) {
	if (get_radix_nodes () == NOTOK)
	    goto out1;

	goto sort_routes;
    }
#endif

    if (getkmem (nl + N_RTHASHSIZE, (caddr_t) &rthashsize, sizeof rthashsize)
	    == NOTOK)
	return NOTOK;
    if (rthashsize == 0)		/* XXX: why is this? */
	rthashsize = 8;
    tblsize = rthashsize * sizeof *rtaddr;
#ifdef	ultrix
    if ((rtnet = (struct rtentry **) malloc ((unsigned) (tblsize))) == NULL
	    || (rthost = (struct rtentry **) malloc ((unsigned) (tblsize)))
		    == NULL)
#else
    if ((rtnet = (struct mbuf **) malloc ((unsigned) (tblsize))) == NULL
	    || (rthost = (struct mbuf **) malloc ((unsigned) (tblsize)))
		    == NULL)
#endif
	adios (NULLCP, "out of memory");
    if (getkmem (nl + N_RTNET, (caddr_t) rtnet, tblsize) == NOTOK
	    || getkmem (nl + N_RTHOST, (caddr_t) rthost, tblsize) == NOTOK)
	goto out2;

    nz -> n_name = "struct route";
    for (rtaddr = rtnet; rtaddr; rtaddr = rthost, rthost = NULL) {
	for (i = 0; i < rthashsize; i++) {
#ifdef	ultrix
	    struct rtentry ree;
	    register struct rtentry *re;

	    for (re = rtaddr[i];
		    nz -> n_value = (unsigned long) re;
		    re = ree.rt_next) {
		if (getkmem (nz, (char *) &ree, sizeof ree) == NOTOK)
		    goto out2;
		if (get_route (&ree) == NOTOK)
		    goto out2;
#else
	    register struct mbuf *m;
	    struct mbuf    ms;
	    register struct rtentry *re;

	    for (m = rtaddr[i];
		     nz -> n_value = (unsigned long) m;
		     m = ms.m_next) {
		if (getkmem (nz, (char *) &ms, sizeof ms) == NOTOK)
		    goto out2;

#ifndef	BSD44
		re = mtod (&ms, struct rtentry *);
#else
		re = (struct rtentry *) ms.m_dat;
#endif
		if (get_route (re) == NOTOK)
		    goto out2;
#endif
	    }
	}

        free ((char *) rtaddr);
    }


#ifdef	BSD44
sort_routes: ;
#endif
    if (routeNumber > 1) {
	register struct rtetab **base,
			       **rte;

	if ((base = (struct rtetab **)
		    	    malloc ((unsigned) (routeNumber * sizeof *base)))
		== NULL)
	    adios (NULLCP, "out of memory");

	rte = base;
	for (rt = rts; rt; rt = rt -> rt_next)
	    *rte++ = rt;

	qsort ((char *) base, routeNumber, sizeof *base, rt_compar);

	rtp = base;
	rt = rts = *rtp++;
	rts_inet = NULL;
#ifdef	BSD44
	rts_iso = NULL;
#endif
	while (rtp < rte) {
	    switch (rt -> rt_dst.sa.sa_family) {
	        case AF_INET:
	            if (rts_inet == NULL)
			rts_inet = rt;
		    break;

#ifdef	BSD44
		case AF_ISO:
		    if (rts_iso == NULL)
			rts_iso = rt;
		    break;
#endif
	    }

	    rt -> rt_next = *rtp;
	    rt = *rtp++;
	}
	switch (rt -> rt_dst.sa.sa_family) {
	    case AF_INET:
	        if (rts_inet == NULL)
		    rts_inet = rt;
		break;

#ifdef	BSD44
	    case AF_ISO:
		if (rts_iso == NULL)
		    rts_iso = rt;
		break;
#endif
	}
	rt -> rt_next = NULL;

	free ((char *) base);
    }

    first_time = 0;
    return OK;

out2: ;
    free ((char *) rtnet);
    free ((char *) rthost);

#ifdef	BSD44
out1: ;
#endif
    for (rt = rts; rt; rt = rp) {
	rp = rt -> rt_next;

	free ((char *) rt);
    }
    rts = rts_inet = NULL;
#ifdef	BSD44
    rts_iso = NULL;
#endif

    return NOTOK;
}

/* */

static int  get_route (re)
register struct rtentry *re;
{
    register struct rtetab *rt,
			   *rz;
#ifdef	BSD44
    union sockaddr_un rtsock;
    struct nlist nzs;
    register struct nlist *nz = &nzs;
#endif
    OIDentifier	oids;

#ifdef	BSD44
    nz -> n_name = "union sockaddr_un",
    nz -> n_value = (unsigned long) rt_key (re);
    if (getkmem (nz, (caddr_t) &rtsock, sizeof rtsock) == NOTOK)
	return NOTOK;
#endif

    if ((rt = (struct rtetab *) calloc (1, sizeof *rt)) == NULL)
	adios (NULLCP, "out of memory");
    rt -> rt_rt = *re;	    /* struct copy */

#ifndef	BSD44
    rt -> rt_dst.sa = re -> rt_dst;		/* struct copy */

    rt -> rt_gateway.sa = re -> rt_gateway;	/*   .. */
#else
    rt -> rt_dst = rtsock;			/* struct copy */

    nz -> n_name = "union sockaddr_un",
    nz -> n_value = (unsigned long) re -> rt_gateway;
    if (getkmem (nz, (caddr_t) &rt -> rt_gateway, sizeof rt -> rt_gateway)
	    == NOTOK)
	return NOTOK;
#endif

    switch (rt -> rt_dst.sa.sa_family) {
	case AF_INET:
	    rt -> rt_insize =
		ipaddr2oid (rt -> rt_instance, &rt -> rt_dst.un_in.sin_addr);
	    if (rts_inet == NULL)	/* in case routeNumber == 1 */
		rts_inet = rt;
	    break;

#ifdef	BSD44
       case AF_ISO:
	    rt -> rt_insize =
		clnpaddr2oid (rt -> rt_instance,
			      &rt -> rt_dst.un_iso.siso_addr);
	    if (rts_iso == NULL)	/* in case routeNumber == 1 */
		rts_iso = rt;
	    break;
#endif

	default:
	    bzero ((char *) rt -> rt_instance, sizeof rt -> rt_instance);
	    rt -> rt_insize = 0;
	    break;
    }
    
    for (rz = rts; rz; rz = rz -> rt_next)
	if (rz -> rt_dst.sa.sa_family == rt -> rt_dst.sa.sa_family
	        && elem_cmp (rz -> rt_instance, rz -> rt_insize,
			     rt -> rt_instance, rt -> rt_insize) == 0)
	    break;
    if (rz) {
	if (first_time) {
	    oids.oid_elements = rt -> rt_instance;
	    oids.oid_nelem = rt -> rt_insize;
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "duplicate routes for destination %d/%s",
		    rt -> rt_dst.sa.sa_family, sprintoid (&oids));
	}

	rt -> rt_instance[rt -> rt_insize++] = ++rz -> rt_magic;
    }

    *rtp = rt, rtp = &rt -> rt_next, routeNumber++;

    if (debug && first_time) {
	oids.oid_elements = rt -> rt_instance;
	oids.oid_nelem = rt -> rt_insize;
	advise (LLOG_DEBUG, NULLCP,
		"add route: %d/%s on interface 0x%x with flags %d",
		rt -> rt_dst.sa.sa_family, sprintoid (&oids), re -> rt_ifp,
		re -> rt_flags);
    }

    return OK;
}

/*  */

#ifdef	BSD44
static int  get_radix_nodes () {
    struct radix_node_head *rnh,
			    head;
    struct nlist nzs;
    register struct nlist *nz = &nzs;

    if (getkmem (nl + N_RADIX_NODE_HEAD, (caddr_t) &rnh, sizeof rnh) == NOTOK)
	return NOTOK;

    while (rnh) {
	nz -> n_name = "struct radix_node_head",
	nz -> n_value = (unsigned long) rnh;
	if (getkmem (nz, (caddr_t) &head, sizeof head) == NOTOK)
	    return NOTOK;
	rnh = head.rnh_next;

	if (head.rnh_af == AF_UNSPEC)
	    continue;

	if (get_radix_node (head.rnh_treetop) == NOTOK)
	    return NOTOK;
    }

    return OK;
}

/*  */

static int  get_radix_node (rn)
struct radix_node *rn;
{
    struct radix_node rnode;
    struct rtentry rtentry;
    struct nlist nzs;
    register struct nlist *nz = &nzs;

    for (;;) {
	nz -> n_name = "struct radix_node",
	nz -> n_value = (unsigned long) rn;
	if (getkmem (nz, (caddr_t) &rnode, sizeof rnode) == NOTOK)
	    return NOTOK;

	if (rnode.rn_b < 0) {
	    if (!(rnode.rn_flags & RNF_ROOT)) {
		nz -> n_name = "struct rtentry",
		nz -> n_value = (unsigned long) rn;
		if (getkmem (nz, (caddr_t) &rtentry, sizeof rtentry) == NOTOK)
		    return NOTOK;

		if (get_route (&rtentry) == NOTOK)
		    return NOTOK;
	    }

	    if (rn = rnode.rn_dupedkey)
		continue;
	}
	else {
	    if (get_radix_node (rnode.rn_l) == NOTOK
		    || get_radix_node (rnode.rn_r) == NOTOK)
		return NOTOK;
	}

	return OK;
    }
}
#endif

/*  */

struct rtetab *get_rtent (ip, len, head, isnext)
register unsigned int *ip;
int	len;
struct rtetab *head;
int	isnext;
{
    int	    family;
    register struct rtetab *rt;

    if (head)
	family = head -> rt_dst.sa.sa_family;
    for (rt = head; rt; rt = rt -> rt_next)
	if (rt -> rt_dst.sa.sa_family != family)
	    break;
	else
	    switch (elem_cmp (rt -> rt_instance, rt -> rt_insize, ip, len)) {
	        case 0:
		    if (!isnext)
			return rt;
		    if ((rt = rt -> rt_next) == NULL
			    || rt -> rt_dst.sa.sa_family != family)
			goto out;
		    /* else fall... */

		case 1:
		    return (isnext ? rt : NULL);
	    }

out: ;
    flush_rt_cache = 1;

    return NULL;
}
