/* ip.c - MIB realization of the IP (and Address Translation) group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/ip.c,v 7.20 91/02/22 09:43:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/ip.c,v 7.20 91/02/22 09:43:27 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	ip.c,v $
 * Revision 7.20  91/02/22  09:43:27  mrose
 * Interim 6.8
 * 
 * Revision 7.19  91/01/11  15:34:23  mrose
 * sets
 * 
 * Revision 7.18  90/12/18  10:13:36  mrose
 * update
 * 
 * Revision 7.17  90/10/15  18:21:00  mrose
 * tables
 * 
 * Revision 7.16  90/10/02  09:54:59  mrose
 * normalize again
 * 
 * Revision 7.15  90/09/03  12:57:25  mrose
 * update
 * 
 * Revision 7.14  90/08/16  16:50:32  mrose
 * again
 * 
 * Revision 7.13  90/08/08  14:00:58  mrose
 * stuff
 * 
 * Revision 7.12  90/07/09  14:48:44  mrose
 * sync
 * 
 * Revision 7.11  90/05/22  20:30:26  mrose
 * cache
 * 
 * Revision 7.10  90/05/12  17:02:02  mrose
 * sync
 * 
 * Revision 7.9  90/04/18  08:51:47  mrose
 * oid_normalize
 * 
 * Revision 7.8  90/03/24  10:54:09  mrose
 * update
 * 
 * Revision 7.7  90/03/06  13:50:55  mrose
 * jch
 * 
 * Revision 7.6  90/02/27  18:52:11  mrose
 * unix stuff
 * 
 * Revision 7.5  90/02/27  18:49:40  mrose
 * unix stuff
 * 
 * Revision 7.4  90/02/23  17:47:43  mrose
 * update
 * 
 * Revision 7.3  90/02/17  10:38:14  mrose
 * smux
 * 
 * Revision 7.2  90/01/27  08:21:52  mrose
 * touch-up
 * 
 * Revision 7.1  90/01/11  18:34:10  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:06  mrose
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

#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_var.h>
#include <sys/ioctl.h>

/*  */

#define	FORW_GATEWAY	1		/* ipForwarding */
#define	FORW_HOST	2
static int	ipforwarding;

static struct ipstat ipstat;

/*  */

#define	ipForwarding	0
#define	ipDefaultTTL	1
#ifdef	BSD43
#define	ipInReceives	2
#endif
#define	ipInHdrErrors	3
#undef	ipInAddrErrors	4		/* NOT IMPLEMENTED */
#ifdef	BSD43
#define	ipForwDatagrams	5
#endif
#ifdef	BSD44
#define	ipInUnknownProtos 6
#endif
#undef	ipInDiscards	7		/* NOT IMPLEMENTED */
#ifdef	BSD44
#define	ipInDelivers	8
#define	ipOutRequests	9
#define	ipOutDiscards	10
#endif
#ifdef	BSD43
#define	ipOutNoRoutes	11
#endif	
#define	ipReasmTimeout	12
#ifdef	BSD43
#define	ipReasmReqds	13
#endif
#ifdef	BSD44
#define	ipReasmOKs	14
#endif
#ifdef	BSD43
#define	ipReasmFails	15
#endif
#ifdef	BSD44
#undef	ipFragOKs	16
#undef	ipFragFails	17
#undef	ipFragCreates	18
#endif


static int  o_ip (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register struct ipstat *ips = &ipstat;
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
	case ipDefaultTTL:
	case ipReasmTimeout:
	    break;
	    
	default:
	    if (quantum != lastq) {
		lastq = quantum;

		if (getkmem (nl + N_IPFORWARDING, (caddr_t) &ipforwarding,
			     sizeof ipforwarding) == NOTOK
			|| getkmem (nl + N_IPSTAT, (caddr_t) ips, sizeof *ips)
		    		== NOTOK)
		    return generr (offset);
	    }
	    break;
    }

    switch (ifvar) {
	case ipForwarding:
	    return o_integer (oi, v, ipforwarding ? FORW_GATEWAY : FORW_HOST);
	    
	case ipDefaultTTL:
	    return o_integer (oi, v, MAXTTL);

#ifdef	ipInReceives
	case ipInReceives:
	    return o_integer (oi, v, ips -> ips_total);
#endif

	case ipInHdrErrors:
	    return o_integer (oi, v, ips -> ips_badsum
			           + ips -> ips_tooshort
				   + ips -> ips_toosmall
				   + ips -> ips_badhlen
				   + ips -> ips_badlen);

#ifdef	ipForwDatagrams
	case ipForwDatagrams:
	    return o_integer (oi, v, ips -> ips_forward);
#endif

#ifdef	ipInUnknownProtos
	case ipInUnknownProtos:
	    return o_integer (oi, v, ips -> ips_noproto);
#endif

#ifdef	ipInDelivers
	case ipInDelivers:
	    return o_integer (oi, v, ips -> ips_delivered);
#endif

#ifdef	ipOutRequests
	case ipOutRequests:
	    return o_integer (oi, v, ips -> ips_localout);
#endif

#ifdef	ipOutDiscards
	case ipOutDiscards:
	    return o_integer (oi, v, ips -> ips_odropped);
#endif

#ifdef	ipOutNoRoutes
	case ipOutNoRoutes:
	    return o_integer (oi, v, ips -> ips_cantforward);
#endif

	case ipReasmTimeout:
	    return o_integer (oi, v, IPFRAGTTL);

#ifdef	ipReasmReqds
	case ipReasmReqds:
	    return o_integer (oi, v, ips -> ips_fragments);
#endif

#ifdef	ipReasmOKs
	case ipReasmOKs:
	    return o_integer (oi, v, ips -> ips_reassembled);
#endif

#ifdef	ipReasmFails
	case ipReasmFails:
	    return o_integer (oi, v, ips -> ips_fragdropped
			           + ips -> ips_fragtimeout);
#endif

#ifdef	ipFragOKs
	case ipFragOKs:
	    return o_integer (oi, v, ips -> ips_fragmented);
#endif

#ifdef	ipFragFails
	case ipFragFails:
	    return o_integer (oi, v, ips -> ips_cantfrag);
#endif

#ifdef	ipFragCreates
	case ipFragCreates:
	    return o_integer (oi, v, ips -> ips_ofragments);
#endif

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

#ifndef	IP_MAXPACKET
#define	IP_MAXPACKET	65535		/* ipAdEntReasmMaxSize */
#endif


#define	IFN_SIZE	4

/*  */

#define	ipAdEntAddr	0
#define	ipAdEntIfIndex	1
#define	ipAdEntNetMask	2
#define	ipAdEntBcastAddr 3
#define	ipAdEntReasmMaxSize 4


static int  o_ip_addr (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    register int   i;
    int	    ifvar;
    register unsigned int *ip,
			  *jp;
    register struct address   *as;
    register OID    oid = oi -> oi_name;
    OID	    new;
    register OT	    ot = oi -> oi_type;

    if (get_interfaces (offset) == NOTOK)
	return generr (offset);

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + IFN_SIZE)
		return int_SNMP_error__status_noSuchName;
	    if ((as = get_addrent (oid -> oid_elements + oid -> oid_nelem
				   - IFN_SIZE, IFN_SIZE, afs_inet, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if ((i = oid -> oid_nelem - ot -> ot_name -> oid_nelem) != 0
		    && i < IFN_SIZE) {
		for (jp = (ip = oid -> oid_elements + ot -> ot_name -> oid_nelem - 1) + i;
		         jp > ip;
		         jp--)
		    if (*jp != 0)
			break;
		if (jp == ip)
		    oid -> oid_nelem = ot -> ot_name -> oid_nelem;
		else {
		    if ((new = oid_normalize (oid, IFN_SIZE - i, 256))
			    == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = oid = new;
		}
	    }
	    else
		if (i > IFN_SIZE)
		    oid -> oid_nelem = ot -> ot_name -> oid_nelem + IFN_SIZE;

	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		if ((as = afs_inet) == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, IFN_SIZE)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - IFN_SIZE;
		jp = as -> adr_instance;
		for (i = IFN_SIZE; i> 0; i--)
		    *ip++ = *jp++;
		
		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		if ((as = get_addrent (ip = oid -> oid_elements
				      	    + oid -> oid_nelem - IFN_SIZE,
				       IFN_SIZE, afs_inet, 1)) == NULL)
		    return NOTOK;

		jp = as -> adr_instance;
		for (i = IFN_SIZE; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case ipAdEntAddr:
	    return o_ipaddr (oi, v, (struct sockaddr_in *) &as -> adr_address);

	case ipAdEntIfIndex:
	    return o_integer (oi, v, ffs (as -> adr_indexmask));

	case ipAdEntNetMask:
	    return o_ipaddr (oi, v, (struct sockaddr_in *) &as -> adr_netmask);

	case ipAdEntBcastAddr:	/* beyond belief! */
	    {
		u_long a =  (((struct sockaddr_in *) &as -> adr_netmask)
			       				    -> sin_addr.s_addr)
			          & ~(((struct sockaddr_in *) &as
					-> adr_broadaddr) -> sin_addr.s_addr);

		return o_integer (oi, v, a ? 1 : 0);
	    }

	case ipAdEntReasmMaxSize:
	    return o_integer (oi, v, IP_MAXPACKET);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

#define	ipRouteDest	0
#define	ipRouteIfIndex	1
#define	ipRouteMetric1	2
#define	ipRouteMetric2	3
#define	ipRouteMetric3	4
#define	ipRouteMetric4	5
#define	ipRouteNextHop	6
#define	ipRouteType	7
#define	ipRouteProto	8
#undef	ipRouteAge	9		/* NOT IMPLEMENTED */
#define	ipRouteMask	10
#define	ipRouteMetric5	11
#define	ipRouteInfo	12
#define	unixIpRouteFlags 13
#define	unixIpRouteRefCnt 14
#define	unixIpRouteUses	15


static int  o_ip_route (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register int    i;
    register unsigned int *ip,
			  *jp;
    register struct rtetab *rt;
    register OID    oid = oi -> oi_name;
    OID	    new;
    register OT	    ot = oi -> oi_type;

    if (get_routes (offset) == NOTOK)
	return generr (offset);

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem < ot -> ot_name -> oid_nelem + IFN_SIZE)
		return int_SNMP_error__status_noSuchName;
	    if ((rt = get_rtent (oid -> oid_elements
				     + ot -> ot_name -> oid_nelem,
				 oid -> oid_nelem
				     - ot -> ot_name -> oid_nelem,
				 rts_inet, 0))
		    == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if ((i = oid -> oid_nelem - ot -> ot_name -> oid_nelem) != 0
		    && i < IFN_SIZE) {
		for (jp = (ip = oid -> oid_elements + ot -> ot_name -> oid_nelem - 1) + i;
		         jp > ip;
		         jp--)
		    if (*jp != 0)
			break;
		if (jp == ip)
		    oid -> oid_nelem = ot -> ot_name -> oid_nelem;
		else {
		    if ((new = oid_normalize (oid, IFN_SIZE - i, 256))
			    == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = oid = new;
		}
	    }

	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		if ((rt = rts_inet) == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, rt -> rt_insize)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - rt -> rt_insize;
		jp = rt -> rt_instance;
		for (i = rt -> rt_insize; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;

		if ((rt = get_rtent (ip = oid -> oid_elements
				      	    + ot -> ot_name -> oid_nelem,
				     j = oid -> oid_nelem
				     	     - ot -> ot_name -> oid_nelem,
				     rts_inet, 1)) == NULL)
		    return NOTOK;

		if ((i = j - rt -> rt_insize) < 0) {
		    if ((new = oid_extend (oid, -i)) == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = new;

		    oid = new;
		}
		else
		    if (i > 0)
			oid -> oid_nelem -= i;

		ip = oid -> oid_elements + ot -> ot_name -> oid_nelem;
		jp = rt -> rt_instance;
		for (i = rt -> rt_insize; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case ipRouteDest:
	    return o_ipaddr (oi, v, (struct sockaddr_in *) &rt -> rt_dst);

	case ipRouteIfIndex:
	    {
		register struct interface *is;

		if (get_interfaces (type_SNMP_PDUs_get__request) == NOTOK)
		    return generr (offset);

		for (is = ifs; is; is = is -> ifn_next)
		    if ((caddr_t) is -> ifn_offset
			        == (caddr_t) rt -> rt_rt.rt_ifp) {
			if (is -> ifn_ready)
			    return o_integer (oi, v, is -> ifn_index);
			break;
		    }

		if (offset == type_SNMP_PDUs_get__next__request)
		    return NOTOK;
		return int_SNMP_error__status_noSuchName;
	    }
	    
	case ipRouteMetric1:
	case ipRouteMetric2:
	case ipRouteMetric3:
	case ipRouteMetric4:
	case ipRouteMetric5:
	    return o_integer (oi, v, METRIC_NONE);

	case ipRouteNextHop:
	    return o_ipaddr (oi, v, (struct sockaddr_in *) &rt -> rt_gateway);

	case ipRouteType:
	    switch (rt -> rt_rt.rt_flags & (RTF_GATEWAY | RTF_HOST)) {
		case RTF_GATEWAY:
		case RTF_HOST:
		    return o_integer (oi, v, TYPE_REMOTE);

		case 0:
		    return o_integer (oi, v, TYPE_DIRECT);

		default:
		    return o_integer (oi, v, TYPE_OTHER);
	    }

	case ipRouteProto:
#ifdef	RTF_DYNAMIC
#ifndef	RTF_MODIFIED
	    if (rt -> rt_rt.rt_flags & RTF_DYNAMIC)
#else
	    if (rt -> rt_rt.rt_flags & (RTF_DYNAMIC | RTF_MODIFIED))
#endif
		return o_integer (oi, v, PROTO_ICMP);
	    else
#endif
		return o_integer (oi, v, PROTO_OTHER);

	case ipRouteMask:
	    {
		struct sockaddr_in mask;
		struct sockaddr_in *sin = (struct sockaddr_in *) &rt -> rt_dst;
#ifndef	BSD44
		register struct interface *is;
		register struct address *as;
#endif

		bzero ((char *) &mask, sizeof mask);
		
		if (rt->rt_rt.rt_flags & RTF_HOST)
		    mask.sin_addr.s_addr = (u_long) 0xffffffff;
		else
		    if (sin -> sin_addr.s_addr != 0L) {
#ifndef	BSD44
			/* XXX - BSD44 shouldn't use this code, it has a */
			/* mask associated with each route, but I don't */
			/* know how to locate it at the moment (jch) */

			if (get_interfaces (type_SNMP_PDUs_get__request)
			        == NOTOK)
			    return generr (offset);

			for (is = ifs; is; is = is -> ifn_next)
			    if ((caddr_t) is -> ifn_offset
				    == (caddr_t) rt -> rt_rt.rt_ifp)
				break;

			if (!is || !is -> ifn_ready)
			    return int_SNMP_error__status_noSuchName;

			for (as = afs_inet; as; as = as -> adr_next)
			    if (is -> ifn_indexmask & as -> adr_indexmask)
				break;

			if (!as)
			    return int_SNMP_error__status_noSuchName;

			mask.sin_addr.s_addr = as -> adr_netmask.un_in.sin_addr.s_addr;
#else
			if (IN_CLASSA (sin -> sin_addr.s_addr))
			    mask.sin_addr.s_addr = IN_CLASSA_NET;
			else
			    if (IN_CLASSB (sin -> sin_addr.s_addr))
				mask.sin_addr.s_addr = IN_CLASSB_NET;
			    else
				mask.sin_addr.s_addr = IN_CLASSC_NET;
#endif
		}

		return o_ipaddr (oi, v, &mask);
	    }

	case ipRouteInfo:
	    return o_specific (oi, v, (caddr_t) nullSpecific);

	case unixIpRouteFlags:
	    return o_integer (oi, v, rt -> rt_rt.rt_flags & 0xffff);

	case unixIpRouteRefCnt:
	    return o_integer (oi, v, rt -> rt_rt.rt_refcnt & 0xffff);

	case unixIpRouteUses:
	    return o_integer (oi, v, rt -> rt_rt.rt_use);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static struct rtstat rtstat;

/*  */

#define	unixRouteBadRedirects 0
#define	unixRouteCreatedByRedirects 1
#define	unixRouteModifiedByRedirects 2
#define	unixRouteLookupFails 3
#define	unixRouteWildcardUses 4


static int  o_ip_routing_stats (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register struct rtstat *rts = &rtstat;
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

    if (quantum != lastq) {
	lastq = quantum;

	if (getkmem (nl + N_RTSTAT, (caddr_t) rts, sizeof *rts) == NOTOK)
	    return generr (offset);
    }

    switch (ifvar) {
	case unixRouteBadRedirects:
	    return o_integer (oi, v, rts -> rts_badredirect & 0xffff);

	case unixRouteCreatedByRedirects:
	    return o_integer (oi, v, rts -> rts_dynamic & 0xffff);

	case unixRouteModifiedByRedirects:
	    return o_integer (oi, v, rts -> rts_newgateway & 0xffff);

	case unixRouteLookupFails:
	    return o_integer (oi, v, rts -> rts_unreach & 0xffff);

	case unixRouteWildcardUses:
	    return o_integer (oi, v, rts -> rts_wildcard & 0xffff);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

struct adrtab {
#define	ADN_SIZE	(IFN_SIZE + 1)		/* IpAddress instance */
    unsigned int    adn_instance[ADN_SIZE];	
    int	    adn_insize;

    struct in_addr adn_address;			/* IpAddress */


#define	ADM_SIZE	ADR_SIZE		/* PhysAddress instance */
    unsigned int    adm_instance[ADM_SIZE];	
    int	    adm_insize;

    u_char	adm_address[ADM_SIZE];		/* PhysAddress */
    u_char	adm_addrlen;			/*   .. */


#define	ADA_SIZE	(IFN_SIZE + 2)		/* AtEntry instance */
    unsigned int    ada_instance[ADA_SIZE];	
    int	    ada_insize;


    int	    adr_index;				/* ifIndex */

    int	    adr_type;				/* ipNetToMediaType */
#define	OTHER_MAPPING	1
#define	DYNAMIC_MAPPING	3
#define	STATIC_MAPPING	4

    struct adrtab *adn_next;			/* next IpAddress */
    struct adrtab *adm_next;    		/* next PhysAddress */
    struct adrtab *ada_next;			/* next AtEntry */
};

static	struct adrtab *ada = NULL;
static	struct adrtab *adn = NULL;
static	struct adrtab *adm = NULL;

static	int	flush_arp_cache = 0;


static struct adrtab *get_arpent ();

/*  */

#define	atIfIndex	0
#define	atPhysAddress	1
#define	atNetAddress	2

#define	ipNetToMediaIfIndex 3
#define	ipNetToMediaPhysAddress 4
#define	ipNetToMediaNetAddress 5
#define	ipNetToMediaType 6


static int  o_address (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    register int    i;
    int	    ifvar,
	    isnpa;
    register unsigned int *ip,
			  *jp;
    register struct adrtab *at;
    struct sockaddr_in netaddr;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    if (get_arptab (offset) == NOTOK)
	return generr (offset);

    switch (ifvar = (int) ot -> ot_info) {
	case ipNetToMediaIfIndex:
	case ipNetToMediaPhysAddress:
	case ipNetToMediaNetAddress:
	case ipNetToMediaType:
	    isnpa = 0;
	    break;

	case atIfIndex:
	case atPhysAddress:
	case atNetAddress:
	    isnpa = -1;
	    break;

	default:
	    return generr (offset);
    }
    
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((at = get_arpent (oid -> oid_elements
				  	+ ot -> ot_name -> oid_nelem,
				  oid -> oid_nelem
				  	- ot -> ot_name -> oid_nelem,
				  isnpa, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		switch (isnpa) {
		    case 0:
		        if (at = adn)
			    jp = at -> adn_instance, i = at -> adn_insize;
			break;

		    case 1:
		        if (at = adm)
			    jp = at -> adm_instance, i = at -> adm_insize;
			break;

		    case -1:
		        if (at = ada)
			    jp = at -> ada_instance, i = at -> ada_insize;
			break;
		}
		if (at == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, i)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - i;
		for (; i > 0; i--)
		    *ip++ = *jp++;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;

		if ((at = get_arpent (oid -> oid_elements
				          + ot -> ot_name -> oid_nelem,
				      j = oid -> oid_nelem
				      	      - ot -> ot_name -> oid_nelem,
				      isnpa, 1)) == NULL)
		    return NOTOK;
		i = isnpa > 0 ? at -> adm_insize
		    	      : isnpa == 0 ? at -> adn_insize
				  	   : at -> ada_insize;

		if ((i = j - i) < 0) {
		    OID	    new;

		    if ((new = oid_extend (oid, -i)) == NULLOID)
			return NOTOK;
		    if (v -> name)
			free_SNMP_ObjectName (v -> name);
		    v -> name = new;

		    oid = new;
		}
		else
		    if (i > 0)
			oid -> oid_nelem -= i;

		ip = oid -> oid_elements + ot -> ot_name -> oid_nelem;
		switch (isnpa) {
		    case 0:
			jp = at -> adn_instance, i = at -> adn_insize;
			break;

		    case 1:
			jp = at -> adm_instance, i = at -> adm_insize;
			break;

		    case -1:
		        jp = at -> ada_instance, i = at -> ada_insize;
			break;

		}
		for (; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case atIfIndex:
	case ipNetToMediaIfIndex:
	    return o_integer (oi, v, at -> adr_index);

	case atPhysAddress:
	case ipNetToMediaPhysAddress:
	    return o_string (oi, v, (char *) at -> adm_address,
			     (int) at -> adm_addrlen);

	case atNetAddress:
	case ipNetToMediaNetAddress:
	    netaddr.sin_addr = at -> adn_address;	/* struct copy */
	    return o_ipaddr (oi, v, &netaddr);

	case ipNetToMediaType:
	    return o_integer (oi, v, at -> adr_type);
	    
	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static int  adn_compar (a, b)
register struct adrtab **a,
		       **b;
{
    return elem_cmp ((*a) -> adn_instance, (*a) -> adn_insize,
		     (*b) -> adn_instance, (*b) -> adn_insize);
}


static int  adm_compar (a, b)
register struct adrtab **a,
		       **b;
{
    return elem_cmp ((*a) -> adm_instance, (*a) -> adm_insize,
		     (*b) -> adm_instance, (*b) -> adm_insize);
}

static int  ada_compar (a, b)
register struct adrtab **a,
		       **b;
{
    return elem_cmp ((*a) -> ada_instance, (*a) -> ada_insize,
		     (*b) -> ada_instance, (*b) -> ada_insize);
}


static int  get_arptab (offset)
int	offset;
{
    int	    adrNumber = 0,
	    arptab_size,
	    tblsize;
    register struct arptab *ac,
			   *ae;
    struct arptab *arptab;
    register struct adrtab *at,
			   *ap,
			  **base,
			  **afe,
			  **afp;
    register struct interface *is;
    static  int first_time = 1;
    static  int lastq = -1;

    if (quantum == lastq)
	return OK;
    if (!flush_arp_cache
	    && offset == type_SNMP_PDUs_get__next__request
	    && quantum == lastq + 1) {			/* XXX: caching! */
	lastq = quantum;
	return OK;
    }
    lastq = quantum, flush_arp_cache = 0;

    for (at = adn; at; at = ap) {
	ap = at -> adn_next;

	free ((char *) at);
    }
    adn = adm = ada = NULL;

    if (getkmem (nl + N_ARPTAB_SIZE, (caddr_t) &arptab_size,
		 sizeof arptab_size) == NOTOK)
	return NOTOK;
    tblsize = arptab_size * sizeof *arptab;
    if ((arptab = (struct arptab *) malloc ((unsigned) (tblsize))) == NULL)
	adios (NULLCP, "out of memory");
    if (getkmem (nl + N_ARPTAB, (caddr_t) arptab, tblsize) == NOTOK) {
	free ((char *) arptab);
	return NOTOK;
    }

    afp = &adn;
    for (ae = (ac = arptab) + arptab_size; ac < ae; ac++) {
	int	type;

	if (!(ac -> at_iaddr.s_addr) || !(ac -> at_flags & ATF_COM))
	    continue;
	type = ac -> at_flags & ATF_PERM ? OTHER_MAPPING
	     : ac -> at_flags & ATF_PUBL ? STATIC_MAPPING
	     : DYNAMIC_MAPPING;

/* there appears to be no way to gather per-interface address translation
   tables, so we simply duplicate the arptable for each interface... */
	for (is = ifs; is; is = is -> ifn_next) {
	    if (!is -> ifn_ready)
		continue;

	    if ((at = (struct adrtab *) calloc (1, sizeof *at)) == NULL)
		adios (NULLCP, "out of memory");
	    *afp = at, afp = &at -> adn_next, adrNumber++;

	    at -> adr_index = is -> ifn_index;
	    at -> adr_type = type;

	    at -> adn_address = ac -> at_iaddr;	/* struct copy */
	    at -> adn_instance[0] = at -> adr_index, at -> adn_insize = 1;
	    at -> adn_insize += ipaddr2oid (at -> adn_instance + 1,
					    &at -> adn_address);

#ifdef	NEW_AT
	    bcopy ((char *) ac -> at_enaddr,
		   (char *) at -> adm_address,
		   (int) (at -> adm_addrlen = sizeof ac -> at_enaddr));
#else
	    bcopy ((char *) ac -> at_enaddr.ether_addr_octet,
		   (char *) at -> adm_address,
		   (int) (at -> adm_addrlen =
			  sizeof ac -> at_enaddr.ether_addr_octet));
#endif
	    at -> adm_instance[0] = at -> adr_index, at -> adm_insize = 1;
	    at -> adm_insize += mediaddr2oid (at -> adm_instance + 1,
					      at -> adm_address,
					      (int) at -> adm_addrlen, 0);

	    at -> ada_instance[0] = at -> adr_index;
	    at -> ada_instance[1] = 1;
	    at -> ada_insize = 2;
	    at -> ada_insize += ipaddr2oid (at -> ada_instance + 2,
					    &at -> adn_address);

	    if (debug && first_time) {
		char    buffer[BUFSIZ];
		OIDentifier	oids;

		oids.oid_elements = at -> adn_instance;
		oids.oid_nelem = at -> adn_insize;
		(void) strcpy (buffer, sprintoid (&oids));
		oids.oid_elements = at -> adm_instance;
		oids.oid_nelem = at -> adm_insize;
		advise (LLOG_DEBUG, NULLCP,
			"add mapping on %d: %s -> %s",
			at -> adr_index, buffer, sprintoid (&oids));
	    }
	}
    }
    first_time = 0;
    free ((char *) arptab);

    if (adrNumber <= 1) {
	adm = ada = adn;
	return OK;
    }

    if ((base = (struct adrtab **)
		    malloc ((unsigned) (adrNumber * sizeof *base))) == NULL)
	adios (NULLCP, "out of memory");

    afe = base;
    for (at = adn; at; at = at -> adn_next)
	*afe++ = at;

    qsort ((char *) base, adrNumber, sizeof *base, adn_compar);

    afp = base;
    at = adn = *afp++;
    while (afp < afe) {
	at -> adn_next = *afp;
	at = *afp++;
    }
    at -> adn_next = NULL;

    qsort ((char *) base, adrNumber, sizeof *base, adm_compar);

    afp = base;
    at = adm = *afp++;
    while (afp < afe) {
	at -> adm_next = *afp;
	at = *afp++;
    }
    at -> adm_next = NULL;

    qsort ((char *) base, adrNumber, sizeof *base, ada_compar);

    afp = base;
    at = ada = *afp++;
    while (afp < afe) {
	at -> ada_next = *afp;
	at = *afp++;
    }
    at -> ada_next = NULL;

    free ((char *) base);

    return OK;
}

/*  */

static struct adrtab *get_arpent (ip, len, isnpa, isnext)
register unsigned int *ip;
int	len;
int	isnpa,
	isnext;
{
    register struct adrtab *at;

    switch (isnpa) {
	case 0:
	    for (at = adn; at; at = at -> adn_next)
		switch (elem_cmp (at -> adn_instance, at -> adn_insize,
				  ip, len)) {
		    case 0:
		        if (!isnext)
			    return at;
			if ((at = at -> adn_next) == NULL)
			    goto out;
			/* else fall... */

		    case 1:
			return (isnext ? at : NULL);
		}
	    break;

	case 1:
	    for (at = adm; at; at = at -> adm_next)
		switch (elem_cmp (at -> adm_instance, at -> adm_insize,
				  ip, len)) {
		    case 0:
		        if (!isnext)
			    return at;
			if ((at = at -> adm_next) == NULL)
			    goto out;
			/* else fall... */

		    case 1:
			return (isnext ? at : NULL);
		}
	    break;

	case -1:
	    for (at = ada; at; at = at -> ada_next)
		switch (elem_cmp (at -> ada_instance, at -> ada_insize,
				  ip, len)) {
		    case 0:
		        if (!isnext)
			    return at;
			if ((at = at -> ada_next) == NULL)
			    goto out;
			/* else fall... */

		    case 1:
			return (isnext ? at : NULL);
		}
	    break;
    }

out: ;
    flush_arp_cache = 1;

    return NULL;
}

/*  */

init_ip () {
    register OT	    ot;

    if (ot = text2obj ("ipForwarding"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipForwarding;
    if (ot = text2obj ("ipDefaultTTL"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipDefaultTTL;
#ifdef	ipInReceives
    if (ot = text2obj ("ipInReceives"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInReceives;
#endif
    if (ot = text2obj ("ipInHdrErrors"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInHdrErrors;
#ifdef	ipInAddrErrors
    if (ot = text2obj ("ipInAddrErrors"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInAddrErrors;
#endif
#ifdef	ipForwDatagrams
    if (ot = text2obj ("ipForwDatagrams"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipForwDatagrams;
#endif
#ifdef	ipInUnknownProtos
    if (ot = text2obj ("ipInUnknownProtos"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInUnknownProtos;
#endif
#ifdef	ipInDiscards
    if (ot = text2obj ("ipInDiscards"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInDiscards;
#endif
#ifdef	ipInDelivers
    if (ot = text2obj ("ipInDelivers"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipInDelivers;
#endif
#ifdef	ipOutRequests
    if (ot = text2obj ("ipOutRequests"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipOutRequests;
#endif
#ifdef	ipOutDiscards
    if (ot = text2obj ("ipOutDiscards"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipOutDiscards;
#endif
#ifdef	ipOutNoRoutes
    if (ot = text2obj ("ipOutNoRoutes"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipOutNoRoutes;
#endif
    if (ot = text2obj ("ipReasmTimeout"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipReasmTimeout;
#ifdef	ipReasmReqds
    if (ot = text2obj ("ipReasmReqds"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipReasmReqds;
#endif
#ifdef	ipReasmOKs
    if (ot = text2obj ("ipReasmOKs"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipReasmOKs;
#endif
#ifdef	ipReasmFails
    if (ot = text2obj ("ipReasmFails"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipReasmFails;
#endif
#ifdef	ipFragOKs
    if (ot = text2obj ("ipFragOKs"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipFragOKs;
#endif
#ifdef	ipFragFails
    if (ot = text2obj ("ipFragFails"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipFragFails;
#endif
#ifdef	ipFragCreates
    if (ot = text2obj ("ipFragCreates"))
	ot -> ot_getfnx = o_ip,
	ot -> ot_info = (caddr_t) ipFragCreates;
#endif

    if (ot = text2obj ("ipAdEntAddr"))
	ot -> ot_getfnx = o_ip_addr,
	ot -> ot_info = (caddr_t) ipAdEntAddr;
    if (ot = text2obj ("ipAdEntIfIndex"))
	ot -> ot_getfnx = o_ip_addr,
	ot -> ot_info = (caddr_t) ipAdEntIfIndex;
    if (ot = text2obj ("ipAdEntNetMask"))
	ot -> ot_getfnx = o_ip_addr,
	ot -> ot_info = (caddr_t) ipAdEntNetMask;
    if (ot = text2obj ("ipAdEntBcastAddr"))
	ot -> ot_getfnx = o_ip_addr,
	ot -> ot_info = (caddr_t) ipAdEntBcastAddr;
    if (ot = text2obj ("ipAdEntReasmMaxSize"))
	ot -> ot_getfnx = o_ip_addr,
	ot -> ot_info = (caddr_t) ipAdEntReasmMaxSize;

    if (ot = text2obj ("ipRouteDest"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteDest;
    if (ot = text2obj ("ipRouteIfIndex"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteIfIndex;
    if (ot = text2obj ("ipRouteMetric1"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMetric1;
    if (ot = text2obj ("ipRouteMetric2"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMetric2;
    if (ot = text2obj ("ipRouteMetric3"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMetric3;
    if (ot = text2obj ("ipRouteMetric4"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMetric4;
    if (ot = text2obj ("ipRouteNextHop"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteNextHop;
    if (ot = text2obj ("ipRouteType"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteType;
    if (ot = text2obj ("ipRouteProto"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteProto;
#ifdef	ipRouteAge
    if (ot = text2obj ("ipRouteAge"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteAge;
#endif
    if (ot = text2obj ("ipRouteMask"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMask;
    if (ot = text2obj ("ipRouteMetric5"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteMetric5;
    if (ot = text2obj ("ipRouteInfo"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) ipRouteInfo;
    
    if (ot = text2obj ("unixIpRouteFlags"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) unixIpRouteFlags;
    if (ot = text2obj ("unixIpRouteRefCnt"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) unixIpRouteRefCnt;
    if (ot = text2obj ("unixIpRouteUses"))
	ot -> ot_getfnx = o_ip_route,
	ot -> ot_info = (caddr_t) unixIpRouteUses;

    if (ot = text2obj ("unixRouteBadRedirects"))
	ot -> ot_getfnx = o_ip_routing_stats,
	ot -> ot_info = (caddr_t) unixRouteBadRedirects;
    if (ot = text2obj ("unixRouteCreatedByRedirects"))
	ot -> ot_getfnx = o_ip_routing_stats,
	ot -> ot_info = (caddr_t) unixRouteCreatedByRedirects;
    if (ot = text2obj ("unixRouteModifiedByRedirects"))
	ot -> ot_getfnx = o_ip_routing_stats,
	ot -> ot_info = (caddr_t) unixRouteModifiedByRedirects;
    if (ot = text2obj ("unixRouteLookupFails"))
	ot -> ot_getfnx = o_ip_routing_stats,
	ot -> ot_info = (caddr_t) unixRouteLookupFails;
    if (ot = text2obj ("unixRouteWildcardUses"))
	ot -> ot_getfnx = o_ip_routing_stats,
	ot -> ot_info = (caddr_t) unixRouteWildcardUses;

    if (ot = text2obj ("atIfIndex"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) atIfIndex;
    if (ot = text2obj ("atPhysAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) atPhysAddress;
    if (ot = text2obj ("atNetAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) atNetAddress;

    if (ot = text2obj ("ipNetToMediaIfIndex"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) ipNetToMediaIfIndex;
    if (ot = text2obj ("ipNetToMediaPhysAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) ipNetToMediaPhysAddress;
    if (ot = text2obj ("ipNetToMediaNetAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) ipNetToMediaNetAddress;
    if (ot = text2obj ("ipNetToMediaType"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) ipNetToMediaType;
}
