/* clns.c - MIB realization of the experimental CLNS group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/clns.c,v 7.10 91/02/22 09:42:55 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/clns.c,v 7.10 91/02/22 09:42:55 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	clns.c,v $
 * Revision 7.10  91/02/22  09:42:55  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/08  12:48:23  mrose
 * update
 * 
 * Revision 7.8  90/12/18  10:13:07  mrose
 * update
 * 
 * Revision 7.7  90/10/15  18:21:04  mrose
 * tables
 * 
 * Revision 7.6  90/09/07  11:21:27  mrose
 * update
 * 
 * Revision 7.5  90/07/09  14:48:34  mrose
 * sync
 * 
 * Revision 7.4  90/05/22  20:30:21  mrose
 * cache
 * 
 * Revision 7.3  90/02/27  18:49:28  mrose
 * unix stuff
 * 
 * Revision 7.2  90/02/17  10:37:37  mrose
 * smux
 * 
 * Revision 7.1  90/01/11  18:33:54  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:22:55  mrose
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

#ifdef	BSD44
#include <sys/kinfo.h>
#include <net/if_dl.h>
#include <netiso/iso_snpac.h>
#define	CLNP_ER_CODES
#include <netiso/clnp.h>
#include <netiso/clnp_stat.h>
#include <netiso/esis.h>

/*  */

#define	FORW_IS		1		/* clnpForwarding */
#define	FORW_ES		2
static	int	iso_systype;

struct clnp_stat clnp_stat;

/*  */

#define	clnpForwarding	0
#define	clnpDefaultLifeTime 1
#define	clnpInReceives	2
#define	clnpInHdrErrors	3
#define	clnpInAddrErrors 4
#define	clnpForwPDUs	5
#undef	clnpInUnknownNLPs 6		/* NOT IMPLEMENTED */
#define	clnpInUnknownULPs 7
#undef	clnpInDiscards	8		/* NOT IMPLEMENTED */
#define	clnpInDelivers	9
#define	clnpOutRequests	10
#define	clnpOutDiscards	11
#define	clnpOutNoRoutes	12
#define	clnpReasmTimeout 13
#define	clnpReasmReqds	14
#define	clnpReasmOKs	15
#define	clnpReasmFails	16
#define	clnpSegOKs	17
#define	clnpSegFails	18
#define	clnpSegCreates	19
#undef	clnpInOpts	20		/* NOT IMPLEMENTED */
#undef	clnpInOpts	21		/* NOT IMPLEMENTED */

#define	clnpInErrors	(100 + 0)
#define	clnpOutErrors	(100 + 1)
#define	clnpInErrUnspecs (100 + 2)
#define	clnpInErrProcs	(100 + 3)
#define	clnpInErrCksums	(100 + 4)
#define	clnpInErrCongests (100 + 5)
#define	clnpInErrHdrs	(100 + 6)
#define	clnpInErrSegs	(100 + 7)
#define	clnpInErrIncomps (100 + 8)
#define	clnpInErrDups	(100 + 9)
#define	clnpInErrUnreachDsts (100 + 10)
#define	clnpInErrUnknownDsts (100 + 11)
#define	clnpInErrSRUnspecs (100 + 12)
#define	clnpInErrSRSyntaxes (100 + 13)
#define	clnpInErrSRUnkAddrs (100 + 14)
#define	clnpInErrSRBadPaths (100 + 15)
#define	clnpInErrHops	(100 + 16)
#define	clnpInErrHopReassms (100 + 17)
#define	clnpInErrUnsOptions (100 + 18)
#define	clnpInErrUnsVersions (100 + 19)
#define	clnpInErrUnsSecurities (100 + 20)
#define	clnpInErrUnsSRs	(100 + 21)
#define	clnpInErrUnsRRs	(100 + 22)
#define	clnpInErrInterferences (100 + 23)
#define	clnpOutErrUnspecs	(100 + 24)
#define	clnpOutErrProcs	(100 + 25)
#define	clnpOutErrCksums (100 + 26)
#define	clnpOutErrCongests (100 + 27)
#define	clnpOutErrHdrs	(100 + 28)
#define	clnpOutErrSegs	(100 + 29)
#define	clnpOutErrIncomps (100 + 30)
#define	clnpOutErrDups	(100 + 31)
#define	clnpOutErrUnreachDsts (100 + 32)
#define	clnpOutErrUnknownDsts (100 + 33)
#define	clnpOutErrSRUnspecs (100 + 34)
#define	clnpOutErrSRSyntaxes (100 + 35)
#define	clnpOutErrSRUnkAddrs (100 + 36)
#define	clnpOutErrSRBadPaths (100 + 37)
#define	clnpOutErrHops	(100 + 38)
#define	clnpOutErrHopReassms (100 + 39)
#define	clnpOutErrUnsOptions (100 + 40)
#define	clnpOutErrUnsVersions (100 + 41)
#define	clnpOutErrUnsSecurities	(100 + 42)
#define	clnpOutErrUnsSRs (100 + 43)
#define	clnpOutErrUnsRRs (100 + 44)
#define	clnpOutErrInterferences	(100 + 45)


static int  o_clnp (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    register int   *dp,
		   *ep,
		    j;
    int	    ifvar;
    register struct clnp_stat *cns = &clnp_stat;
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
	case clnpDefaultLifeTime:
	case clnpReasmTimeout:
	    break;
	    
	default:
	    if (quantum != lastq) {
		lastq = quantum;

		if (getkmem (nl + N_ISO_SYSTYPE, (caddr_t) &iso_systype,
			     sizeof iso_systype) == NOTOK
		        || getkmem (nl + N_CLNP_STAT, (caddr_t) cns,
				    sizeof *cns) == NOTOK)
		    return generr (offset);
	    }
	    break;
    }

    switch (ifvar) {
	case clnpForwarding:
	    return o_integer (oi, v,
			      iso_systype & SNPA_ES ? FORW_ES : FORW_IS);

	case clnpDefaultLifeTime:
	    return o_integer (oi, v, CLNP_TTL);

	case clnpInReceives:
	    return o_integer (oi, v, cns -> cns_total);

	case clnpInHdrErrors:
	    return o_integer (oi, v, cns -> cns_toosmall
			           + cns -> cns_badhlen
				   + cns -> cns_badcsum
			           + cns -> cns_noseg
				   + cns -> cns_badvers);

	case clnpInAddrErrors:
	    return o_integer (oi, v, cns -> cns_badaddr);

	case clnpForwPDUs:
	    return o_integer (oi, v, cns -> cns_forward);

	case clnpInUnknownULPs:
	    return o_integer (oi, v, cns -> cns_noproto);

	case clnpInDelivers:
	    return o_integer (oi, v, cns -> cns_delivered);

	case clnpOutRequests:
	    return o_integer (oi, v, cns -> cns_sent - cns -> cns_forward);

	case clnpOutDiscards:
	    return o_integer (oi, v, cns -> cns_odropped);

	case clnpOutNoRoutes:
	    return o_integer (oi, v, cns -> cns_cantforward);

	case clnpReasmTimeout:
	    return o_integer (oi, v, CLNP_TTL);

	case clnpReasmReqds:
	    return o_integer (oi, v, cns -> cns_fragments);

	case clnpReasmOKs:
	    return o_integer (oi, v, cns -> cns_reassembled);

	case clnpReasmFails:
	    return o_integer (oi, v, cns -> cns_fragdropped
				   + cns -> cns_fragtimeout);

	case clnpSegOKs:
	    return o_integer (oi, v, cns -> cns_fragmented);

	case clnpSegFails:
	    return o_integer (oi, v, cns -> cns_cantfrag);

	case clnpSegCreates:
	    return o_integer (oi, v, cns -> cns_ofragments);

	case clnpInErrors:
	    j = 0;
	    for (ep = (dp = cns -> cns_er_inhist) + CLNP_ERRORS; dp <= ep; )
		j += *dp++;
	    return o_integer (oi, v, j);

	case clnpOutErrors:
	    j = 0;
	    for (ep = (dp = cns -> cns_er_outhist) + CLNP_ERRORS; dp <= ep; )
		j += *dp++;
	    return o_integer (oi, v, j);

#define	clnpInputError(r)	cns -> cns_er_inhist[clnp_er_index (r)]

	case clnpInErrUnspecs:
	    return o_integer (oi, v, clnpInputError (GEN_NOREAS));

	case clnpInErrProcs:
	    return o_integer (oi, v, clnpInputError (GEN_PROTOERR));

	case clnpInErrCksums:
	    return o_integer (oi, v, clnpInputError (GEN_BADCSUM));

	case clnpInErrCongests:
	    return o_integer (oi, v, clnpInputError (GEN_CONGEST));

	case clnpInErrHdrs:
	    return o_integer (oi, v, clnpInputError (GEN_HDRSYNTAX));

	case clnpInErrSegs:
	    return o_integer (oi, v, clnpInputError (GEN_SEGNEEDED));

	case clnpInErrIncomps:
	    return o_integer (oi, v, clnpInputError (GEN_INCOMPLETE));

	case clnpInErrDups:
	    return o_integer (oi, v, clnpInputError (GEN_DUPOPT));

	case clnpInErrUnreachDsts:
	    return o_integer (oi, v, clnpInputError (ADDR_DESTUNREACH));

	case clnpInErrUnknownDsts:
	    return o_integer (oi, v, clnpInputError (ADDR_DESTUNKNOWN));

	case clnpInErrSRUnspecs:
	    return o_integer (oi, v, clnpInputError (SRCRT_UNSPECERR));

	case clnpInErrSRSyntaxes:
	    return o_integer (oi, v, clnpInputError (SRCRT_SYNTAX));

	case clnpInErrSRUnkAddrs:
	    return o_integer (oi, v, clnpInputError (SRCRT_UNKNOWNADDR));

	case clnpInErrSRBadPaths:
	    return o_integer (oi, v, clnpInputError (SRCRT_BADPATH));

	case clnpInErrHops:
	    return o_integer (oi, v, clnpInputError (TTL_EXPTRANSIT));

	case clnpInErrHopReassms:
	    return o_integer (oi, v, clnpInputError (TTL_EXPREASS));

	case clnpInErrUnsOptions:
	    return o_integer (oi, v, clnpInputError (DISC_UNSUPPOPT));

	case clnpInErrUnsVersions:
	    return o_integer (oi, v, clnpInputError (DISC_UNSUPPVERS));

	case clnpInErrUnsSecurities:
	    return o_integer (oi, v, clnpInputError (DISC_UNSUPPSECURE));

	case clnpInErrUnsSRs:
	    return o_integer (oi, v, clnpInputError (DISC_UNSUPPSRCRT));

	case clnpInErrUnsRRs:
	    return o_integer (oi, v, clnpInputError (DISC_UNSUPPRECRT));

	case clnpInErrInterferences:
	    return o_integer (oi, v, clnpInputError (REASS_INTERFERE));

#undef	clnpInputError
#define	clnpOutputError(r)	cns -> cns_er_outhist[clnp_er_index (r)]

	case clnpOutErrUnspecs:
	    return o_integer (oi, v, clnpOutputError (GEN_NOREAS));

	case clnpOutErrProcs:
	    return o_integer (oi, v, clnpOutputError (GEN_PROTOERR));

	case clnpOutErrCksums:
	    return o_integer (oi, v, clnpOutputError (GEN_BADCSUM));

	case clnpOutErrCongests:
	    return o_integer (oi, v, clnpOutputError (GEN_CONGEST));

	case clnpOutErrHdrs:
	    return o_integer (oi, v, clnpOutputError (GEN_HDRSYNTAX));

	case clnpOutErrSegs:
	    return o_integer (oi, v, clnpOutputError (GEN_SEGNEEDED));

	case clnpOutErrIncomps:
	    return o_integer (oi, v, clnpOutputError (GEN_INCOMPLETE));

	case clnpOutErrDups:
	    return o_integer (oi, v, clnpOutputError (GEN_DUPOPT));

	case clnpOutErrUnreachDsts:
	    return o_integer (oi, v, clnpOutputError (ADDR_DESTUNREACH));

	case clnpOutErrUnknownDsts:
	    return o_integer (oi, v, clnpOutputError (ADDR_DESTUNKNOWN));

	case clnpOutErrSRUnspecs:
	    return o_integer (oi, v, clnpOutputError (SRCRT_UNSPECERR));

	case clnpOutErrSRSyntaxes:
	    return o_integer (oi, v, clnpOutputError (SRCRT_SYNTAX));

	case clnpOutErrSRUnkAddrs:
	    return o_integer (oi, v, clnpOutputError (SRCRT_UNKNOWNADDR));

	case clnpOutErrSRBadPaths:
	    return o_integer (oi, v, clnpOutputError (SRCRT_BADPATH));

	case clnpOutErrHops:
	    return o_integer (oi, v, clnpOutputError (TTL_EXPTRANSIT));

	case clnpOutErrHopReassms:
	    return o_integer (oi, v, clnpOutputError (TTL_EXPREASS));

	case clnpOutErrUnsOptions:
	    return o_integer (oi, v, clnpOutputError (DISC_UNSUPPOPT));

	case clnpOutErrUnsVersions:
	    return o_integer (oi, v, clnpOutputError (DISC_UNSUPPVERS));

	case clnpOutErrUnsSecurities:
	    return o_integer (oi, v, clnpOutputError (DISC_UNSUPPSECURE));

	case clnpOutErrUnsSRs:
	    return o_integer (oi, v, clnpOutputError (DISC_UNSUPPSRCRT));

	case clnpOutErrUnsRRs:
	    return o_integer (oi, v, clnpOutputError (DISC_UNSUPPRECRT));

	case clnpOutErrInterferences:
	    return o_integer (oi, v, clnpOutputError (REASS_INTERFERE));

#undef	clnpOutputError

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

static int  clnp_er_index (p)
u_char p;
{
    register u_char *cp = clnp_er_codes + CLNP_ERRORS;

    while (cp-- > clnp_er_codes)
	if (*cp == p)
	    return (cp - clnp_er_codes);

    return (CLNP_ERRORS + 1);
}

/*  */

#define	CLNP_MAXPACKET	65535		/* clnpAdEntReasmMaxSize */
					/* equivalent of IP_MAXPACKET */


#define	clnpAdEntAddr	0
#define	clnpAdEntIfIndex 1
#define	clnpAdEntNetMask 2
#define	clnpAdEntReasmMaxSize 3


static int  o_clnp_addr (oi, v, offset)
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
    register OT	    ot = oi -> oi_type;

    if (get_interfaces (offset) == NOTOK)
	return generr (offset);

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((as = get_addrent (oid -> oid_elements
				   	+ ot -> ot_name -> oid_nelem,
				   oid -> oid_nelem
				   	- ot -> ot_name -> oid_nelem,
				   afs_iso, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem < ot -> ot_name -> oid_nelem)
		return NOTOK;
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((as = afs_iso) == NULL)
		    return NOTOK;

		if ((new = oid_extend (oid, as -> adr_insize)) == NULLOID)
		    return NOTOK;
		ip = new -> oid_elements + new -> oid_nelem - as -> adr_insize;
		jp = as -> adr_instance;
		for (i = as -> adr_insize; i > 0; i--)
		    *ip++ = *jp++;
		
		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	j;
		
		if ((as = get_addrent (oid -> oid_elements
				           + ot -> ot_name -> oid_nelem,
				       j = oid -> oid_nelem
				               - ot -> ot_name -> oid_nelem,
				       afs_iso, 1)) == NULL)
		    return NOTOK;

		if ((i = j - as -> adr_insize) < 0) {
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
		jp = as -> adr_instance;
		for (i = as -> adr_insize; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case clnpAdEntAddr:
	    return o_clnpaddr (oi, v,
			       (struct sockaddr_iso *) &as -> adr_address);

	case clnpAdEntIfIndex:
	    return o_integer (oi, v, ffs (as -> adr_indexmask));

	case clnpAdEntNetMask:
	    return o_clnpaddr (oi, v,
			       (struct sockaddr_iso *) &as -> adr_netmask);

	case clnpAdEntReasmMaxSize:
	    return o_integer (oi, v, CLNP_MAXPACKET);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

#define	clnpRouteDest	0
#define	clnpRouteIfIndex 1
#define	clnpRouteMetric1 2
#define	clnpRouteMetric2 3
#define	clnpRouteMetric3 4
#define	clnpRouteMetric4 5
#define	clnpRouteNextHop 6
#define	clnpRouteType	7
#define	clnpRouteProto	8
#define	clnpRouteAge	9
#define	unixClnpRouteFlags 10
#define	unixClnpRouteRefCnt 11
#define	unixClnpRouteUses	12


static int  o_clnp_route (oi, v, offset)
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
	    if (oid -> oid_nelem <= ot -> ot_name -> oid_nelem)
		return int_SNMP_error__status_noSuchName;
	    if ((rt = get_rtent (oid -> oid_elements
				     + ot -> ot_name -> oid_nelem,
				 oid -> oid_nelem
				     - ot -> ot_name -> oid_nelem,
				 rts_iso, 0)) == NULL)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		if ((rt = rts_iso) == NULL)
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

		if ((rt = get_rtent (oid -> oid_elements
				         + ot -> ot_name -> oid_nelem,
				     j = oid -> oid_nelem
				             - ot -> ot_name -> oid_nelem,
				     rts_iso, 1)) == NULL)
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
	case clnpRouteDest:
	    return o_clnpaddr (oi, v,
			     (struct sockaddr_iso *) &rt -> rt_dst);

	case clnpRouteIfIndex:
	    {
		register struct interface *is;

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
	    
	case clnpRouteMetric1:
	case clnpRouteMetric2:
	case clnpRouteMetric3:
	case clnpRouteMetric4:
	    return o_integer (oi, v, METRIC_NONE);

	case clnpRouteNextHop:
	    return o_clnpaddr (oi, v,
			       (struct sockaddr_iso *) &rt -> rt_gateway);

	case clnpRouteType:
	    switch (rt -> rt_rt.rt_flags & (RTF_GATEWAY | RTF_HOST)) {
		case RTF_GATEWAY:
		case RTF_HOST:
		    return o_integer (oi, v, TYPE_REMOTE);

		case 0:
		    return o_integer (oi, v, TYPE_DIRECT);

		default:
		    return o_integer (oi, v, TYPE_OTHER);
	    }

	case clnpRouteProto:
	    if (rt -> rt_rt.rt_flags & (RTF_DYNAMIC | RTF_MODIFIED))
		return o_integer (oi, v, PROTO_ESIS);
	    else
		return o_integer (oi, v, PROTO_OTHER);

	case clnpRouteAge:
	    return o_integer (oi, v, 0);

	case unixClnpRouteFlags:
	    return o_integer (oi, v, rt -> rt_rt.rt_flags & 0xffff);

	case unixClnpRouteRefCnt:
	    return o_integer (oi, v, rt -> rt_rt.rt_refcnt & 0xffff);

	case unixClnpRouteUses:
	    return o_integer (oi, v, rt -> rt_rt.rt_use);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

struct adrtab {
#define	ADN_SIZE	ADR_SIZE		/* ClnpAddress instance */
    unsigned int    adn_instance[ADN_SIZE];	
    int	    adn_insize;				

    struct iso_addr adn_address;		/* ClnpAddress */


#define	ADM_SIZE	ADR_SIZE		/* PhysAddress instance */
    unsigned int    adm_instance[ADM_SIZE];	
    int	    adm_insize;				

    u_char	adm_address[ADM_SIZE];		/* PhysAddress */
    u_char	adm_addrlen;			/*   .. */


    int	    adr_index;				/* ifIndex */

    int	    adr_type;				/* clnpNetToMediaType */
						/* clnpMediaToNetType */
#define	DYNAMIC_MAPPING	3
#define	STATIC_MAPPING	4


    struct adrtab *adn_next;			/* next ClnpAddress */
    struct adrtab *adm_next;    		/* next PhysAddress */
};

static	struct adrtab *adn = NULL;
static	struct adrtab *adm = NULL;

static	int	flush_arp_cache = 0;


static struct adrtab *get_arpent ();

/*  */

#define	clnpNetToMediaIfIndex 0
#define	clnpNetToMediaPhysAddress 1
#define	clnpNetToMediaNetAddress 2
#define	clnpNetToMediaType 3
#undef	clnpNetToMediaAge 4		/* NOT IMPLEMENTED */
#undef	clnpNetToMediaHoldTime 5	/* NOT IMPLEMENTED */

#define	clnpMediaToNetIfIndex 6
#define	clnpMediaToNetNetAddress 7
#define	clnpMediaToNetPhysAddress 8
#define	clnpMediaToNetType 9
#undef	clnpMediaToNetAge 10		/* NOT IMPLEMENTED */
#undef	clnpMediaToNetHoldTime 11	/* NOT IMPLEMENTED */


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
    struct sockaddr_iso netaddr;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    if (get_arptab (offset) == NOTOK)
	return generr (offset);

    switch (ifvar = (int) ot -> ot_info) {
	case clnpNetToMediaIfIndex:
	case clnpNetToMediaPhysAddress:
	case clnpNetToMediaNetAddress:
	case clnpNetToMediaType:
	    isnpa = 0;
	    break;

	case clnpMediaToNetIfIndex:
	case clnpMediaToNetNetAddress:
	case clnpMediaToNetPhysAddress:
	case clnpMediaToNetType:
	    isnpa = 1;
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
	    if (oid -> oid_nelem < ot -> ot_name -> oid_nelem)
		return NOTOK;
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((at = isnpa ? adm : adn) == NULL)
		    return NOTOK;
		if (isnpa)
		    jp = at -> adm_instance, i = at -> adm_insize;
		else
		    jp = at -> adn_instance, i = at -> adn_insize;

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
		i = isnpa ? at -> adm_insize : at -> adn_insize;

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
		if (isnpa)
		    jp = at -> adm_instance, i = at -> adm_insize;
		else
		    jp = at -> adn_instance, i = at -> adn_insize;
		for (; i > 0; i--)
		    *ip++ = *jp++;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    switch (ifvar) {
	case clnpNetToMediaIfIndex:
	case clnpMediaToNetIfIndex:
	    return o_integer (oi, v, at -> adr_index);

	case clnpNetToMediaPhysAddress:
	case clnpMediaToNetPhysAddress:
	    return o_string (oi, v, (char *) at -> adm_address,
			     (int) at -> adm_addrlen);

	case clnpNetToMediaNetAddress:
	case clnpMediaToNetNetAddress:
	    netaddr.siso_addr = at -> adn_address;	/* struct copy */
	    return o_clnpaddr (oi, v, &netaddr);

	case clnpNetToMediaType:
	case clnpMediaToNetType:
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


#define	ROUND(a)	(1 + (((a) - 1) | (sizeof (long) - 1)))

static int  get_arptab (offset)
int	offset;
{
    int	    adrNumber = 0,
	    rlen,
	    tblsize;
    char   *snpac;
    register char *sc,
		  *se;
    register struct adrtab *at,
			   *ap,
			  **base,
			  **afe,
			  **afp;
    register struct interface *is;
    register struct rt_msghdr *rtm;
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
    adn = adm = NULL;

    if ((tblsize = getkerninfo (KINFO_RT_DUMP, NULLCP, NULLIP, 0)) == NOTOK)
	return NOTOK;
    if ((snpac = malloc ((unsigned) tblsize)) == NULL)
	adios (NULLCP, "out of memory");
    if ((rlen = getkerninfo (KINFO_RT_DUMP, snpac, &tblsize, 0)) == NOTOK) {
	free (snpac);
	return NOTOK;
    }

    afp = &adn;
    for (se = (sc = snpac) + rlen; sc < se; sc += rtm -> rtm_msglen) {
	register struct sockaddr_dl *sdl;
	register struct sockaddr *sa;
	struct iso_addr nsap;

	rtm = (struct rt_msghdr *) sc;
	sa = (struct sockaddr *) (rtm + 1);
	if (sa -> sa_family != AF_ISO)
	    continue;

	sdl = (struct sockaddr_dl *) (((caddr_t) sa) + ROUND (sa -> sa_len));
	if (sdl -> sdl_family != AF_LINK)
	    continue;

	nsap = ((struct sockaddr_iso *) sa) -> siso_addr;    /* struct copy */
	if (nsap.isoa_len == 0)
	    continue;

	for (is = ifs; is; is = is -> ifn_next)
	    if (is -> ifn_interface.ac_if.if_index == rtm -> rtm_index)
		break;
	if (!is) {
	    if (first_time)
		advise (LLOG_EXCEPTIONS, NULLCP,
			"unable to find interface for SNPA in cache");
	    continue;
	}

	if ((at = (struct adrtab *) calloc (1, sizeof *at)) == NULL)
	    adios (NULLCP, "out of memory");
	*afp = at, afp = &at -> adn_next, adrNumber++;

	at -> adr_index = is -> ifn_index;

	at -> adr_type = rtm -> rtm_flags & (RTF_DYNAMIC | RTF_MODIFIED)
				? DYNAMIC_MAPPING : STATIC_MAPPING;

	at -> adn_address = nsap;	/* struct copy */
	at -> adn_instance[0] = at -> adr_index, at -> adn_insize = 1;
	at -> adn_insize += clnpaddr2oid (at -> adn_instance + 1,
					  &at -> adn_address);

	bcopy ((char *) LLADDR (sdl), (char *) at -> adm_address,
	       (int) (at -> adm_addrlen = sdl -> sdl_alen));
	at -> adm_instance[0] = at -> adr_index, at -> adm_insize = 1;
	at -> adm_insize += mediaddr2oid (at -> adm_instance + 1,
					  at -> adm_address,
					  (int) at -> adm_addrlen, 0);

	if (debug && first_time) {
	    char    buffer[BUFSIZ];
	    OIDentifier	oids;

	    oids.oid_elements = at -> adn_instance;
	    oids.oid_nelem = at -> adn_insize;
	    (void) strcpy (buffer, sprintoid (&oids));
	    oids.oid_elements = at -> adm_instance;
	    oids.oid_nelem = at -> adm_insize;
	    advise (LLOG_DEBUG, NULLCP,
		    "add mapping on interface %d: %s -> %s",
		    at -> adr_index, buffer, sprintoid (&oids));
	}
    }
    first_time = 0;
    free ((char *) snpac);

    if (adrNumber <= 1) {
	adm = adn;
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

    free ((char *) base);

    return OK;
}
#undef	ROUND

/*  */

static struct adrtab *get_arpent (ip, len, isnpa, isnext)
register unsigned int *ip;
int	len;
int	isnpa,
	isnext;
{
    register struct adrtab *at;

    if (isnpa)
	for (at = adm; at; at = at -> adm_next)
	    switch (elem_cmp (at -> adm_instance, at -> adm_insize, ip, len)) {
		case 0:
		    if (!isnext)
			return at;
		    if ((at = at -> adm_next) == NULL)
			goto out;
		    /* else fall... */

		case 1:
		    return (isnext ? at : NULL);
	    }
    else
	for (at = adn; at; at = at -> adn_next)
	    switch (elem_cmp (at -> adn_instance, at -> adn_insize, ip, len)) {
		case 0:
		    if (!isnext)
			return at;
		    if ((at = at -> adn_next) == NULL)
			goto out;
		    /* else fall... */

		case 1:
		    return (isnext ? at : NULL);
	    }

out: ;
    flush_arp_cache = 1;

    return NULL;
}

/*  */

static	struct esis_stat esis_stat;

/*  */

#define	esisESHins	0
#define	esisESHouts	1
#define	esisISHins	2
#define	esisISHouts	3
#define	esisRDUins	4
#define	esisRDUouts	5


static int  o_esis (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register struct esis_stat *es = &esis_stat;
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

	if (getkmem (nl + N_ESIS_STAT, (caddr_t) &esis_stat, sizeof esis_stat)
	        == NOTOK)
	    return generr (offset);
    }

    switch (ifvar) {
	case esisESHins:
	    return o_integer (oi, v, es -> es_eshrcvd);

	case esisESHouts:
	    return o_integer (oi, v, es -> es_eshsent);

	case esisISHins:
	    return o_integer (oi, v, es -> es_ishrcvd);

	case esisISHouts:
	    return o_integer (oi, v, es -> es_ishsent);

	case esisRDUins:
	    return o_integer (oi, v, es -> es_rdrcvd);

	case esisRDUouts:
	    return o_integer (oi, v, es -> es_rdsent);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

init_clns () {
    register OT	    ot;

    if (nl[N_ISO_SYSTYPE].n_value == 0)
	return;

    if (ot = text2obj ("clnpForwarding"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpForwarding;
    if (ot = text2obj ("clnpDefaultLifeTime"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpDefaultLifeTime;
    if (ot = text2obj ("clnpInReceives"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInReceives;
    if (ot = text2obj ("clnpInHdrErrors"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInHdrErrors;
    if (ot = text2obj ("clnpInAddrErrors"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInAddrErrors;
    if (ot = text2obj ("clnpForwPDUs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpForwPDUs;
#ifdef	clnpInUnknownNLPs
    if (ot = text2obj ("clnpInUnknownNLPs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInUnknownNLPs;
#endif
    if (ot = text2obj ("clnpInUnknownULPs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInUnknownULPs;
#ifdef	clnpInDiscards
    if (ot = text2obj ("clnpInDiscards"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInDiscards;
#endif
    if (ot = text2obj ("clnpInDelivers"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInDelivers;
    if (ot = text2obj ("clnpOutRequests"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutRequests;
    if (ot = text2obj ("clnpOutDiscards"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutDiscards;
    if (ot = text2obj ("clnpOutNoRoutes"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutNoRoutes;
    if (ot = text2obj ("clnpReasmTimeout"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpReasmTimeout;
    if (ot = text2obj ("clnpReasmReqds"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpReasmReqds;
    if (ot = text2obj ("clnpReasmOKs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpReasmOKs;
    if (ot = text2obj ("clnpReasmFails"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpReasmFails;
    if (ot = text2obj ("clnpSegOKs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpSegOKs;
    if (ot = text2obj ("clnpSegFails"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpSegFails;
    if (ot = text2obj ("clnpSegCreates"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpSegCreates;
#ifdef	clnpInOpts
    if (ot = text2obj ("clnpInOpts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInOpts;
#endif
#ifdef	clnpOutOpts
    if (ot = text2obj ("clnpOutOpts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutOpts;
#endif

    if (ot = text2obj ("clnpAdEntAddr"))
	ot -> ot_getfnx = o_clnp_addr,
	ot -> ot_info = (caddr_t) clnpAdEntAddr;
    if (ot = text2obj ("clnpAdEntIfIndex"))
	ot -> ot_getfnx = o_clnp_addr,
	ot -> ot_info = (caddr_t) clnpAdEntIfIndex;
    if (ot = text2obj ("clnpAdEntNetMask"))
	ot -> ot_getfnx = o_clnp_addr,
	ot -> ot_info = (caddr_t) clnpAdEntNetMask;
    if (ot = text2obj ("clnpAdEntReasmMaxSize"))
	ot -> ot_getfnx = o_clnp_addr,
	ot -> ot_info = (caddr_t) clnpAdEntReasmMaxSize;

    if (ot = text2obj ("clnpRouteDest"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteDest;
    if (ot = text2obj ("clnpRouteIfIndex"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteIfIndex;
    if (ot = text2obj ("clnpRouteMetric1"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteMetric1;
    if (ot = text2obj ("clnpRouteMetric2"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteMetric2;
    if (ot = text2obj ("clnpRouteMetric3"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteMetric3;
    if (ot = text2obj ("clnpRouteMetric4"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteMetric4;
    if (ot = text2obj ("clnpRouteNextHop"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteNextHop;
    if (ot = text2obj ("clnpRouteType"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteType;
    if (ot = text2obj ("clnpRouteProto"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteProto;
    if (ot = text2obj ("clnpRouteAge"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) clnpRouteAge;

    if (ot = text2obj ("unixClnpRouteFlags"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) unixClnpRouteFlags;
    if (ot = text2obj ("unixClnpRouteRefCnt"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) unixClnpRouteRefCnt;
    if (ot = text2obj ("unixClnpRouteUses"))
	ot -> ot_getfnx = o_clnp_route,
	ot -> ot_info = (caddr_t) unixClnpRouteUses;

    if (ot = text2obj ("clnpNetToMediaIfIndex"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaIfIndex;
    if (ot = text2obj ("clnpNetToMediaPhysAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaPhysAddress;
    if (ot = text2obj ("clnpNetToMediaNetAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaNetAddress;
    if (ot = text2obj ("clnpNetToMediaType"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaType;
#ifdef	clnpNetToMediaAge
    if (ot = text2obj ("clnpNetToMediaAge"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaAge;
#endif
#ifdef	clnpNetToMediaHoldTime
    if (ot = text2obj ("clnpNetToMediaHoldTime"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpNetToMediaHoldTime;
#endif
    if (ot = text2obj ("clnpMediaToNetIfIndex"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetIfIndex;
    if (ot = text2obj ("clnpMediaToNetNetAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetNetAddress;
    if (ot = text2obj ("clnpMediaToNetPhysAddress"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetPhysAddress;
    if (ot = text2obj ("clnpMediaToNetType"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetType;
#ifdef	clnpMediaToNetAge
    if (ot = text2obj ("clnpMediaToNetAge"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetAge;
#endif
#ifdef	clnpMediaToNetHoldTime
    if (ot = text2obj ("clnpMediaToNetHoldTime"))
	ot -> ot_getfnx = o_address,
	ot -> ot_info = (caddr_t) clnpMediaToNetHoldTime;
#endif

    if (ot = text2obj ("clnpInErrors"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrors;
    if (ot = text2obj ("clnpOutErrors"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrors;
    if (ot = text2obj ("clnpInErrUnspecs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnspecs;
    if (ot = text2obj ("clnpInErrProcs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrProcs;
    if (ot = text2obj ("clnpInErrCksums"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrCksums;
    if (ot = text2obj ("clnpInErrCongests"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrCongests;
    if (ot = text2obj ("clnpInErrHdrs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrHdrs;
    if (ot = text2obj ("clnpInErrSegs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrSegs;
    if (ot = text2obj ("clnpInErrIncomps"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrIncomps;
    if (ot = text2obj ("clnpInErrDups"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrDups;
    if (ot = text2obj ("clnpInErrUnreachDsts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnreachDsts;
    if (ot = text2obj ("clnpInErrUnknownDsts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnknownDsts;
    if (ot = text2obj ("clnpInErrSRUnspecs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrSRUnspecs;
    if (ot = text2obj ("clnpInErrSRSyntaxes"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrSRSyntaxes;
    if (ot = text2obj ("clnpInErrSRUnkAddrs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrSRUnkAddrs;
    if (ot = text2obj ("clnpInErrSRBadPaths"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrSRBadPaths;
    if (ot = text2obj ("clnpInErrHops"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrHops;
    if (ot = text2obj ("clnpInErrHopReassms"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrHopReassms;
    if (ot = text2obj ("clnpInErrUnsOptions"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnsOptions;
    if (ot = text2obj ("clnpInErrUnsVersions"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnsVersions;
    if (ot = text2obj ("clnpInErrUnsSecurities"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnsSecurities;
    if (ot = text2obj ("clnpInErrUnsSRs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnsSRs;
    if (ot = text2obj ("clnpInErrUnsRRs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrUnsRRs;
    if (ot = text2obj ("clnpInErrInterferences"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpInErrInterferences;
    if (ot = text2obj ("clnpOutErrUnspecs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnspecs;
    if (ot = text2obj ("clnpOutErrProcs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrProcs;
    if (ot = text2obj ("clnpOutErrCksums"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrCksums;
    if (ot = text2obj ("clnpOutErrCongests"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrCongests;
    if (ot = text2obj ("clnpOutErrHdrs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrHdrs;
    if (ot = text2obj ("clnpOutErrSegs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrSegs;
    if (ot = text2obj ("clnpOutErrIncomps"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrIncomps;
    if (ot = text2obj ("clnpOutErrDups"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrDups;
    if (ot = text2obj ("clnpOutErrUnreachDsts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnreachDsts;
    if (ot = text2obj ("clnpOutErrUnknownDsts"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnknownDsts;
    if (ot = text2obj ("clnpOutErrSRUnspecs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrSRUnspecs;
    if (ot = text2obj ("clnpOutErrSRSyntaxes"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrSRSyntaxes;
    if (ot = text2obj ("clnpOutErrSRUnkAddrs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrSRUnkAddrs;
    if (ot = text2obj ("clnpOutErrSRBadPaths"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrSRBadPaths;
    if (ot = text2obj ("clnpOutErrHops"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrHops;
    if (ot = text2obj ("clnpOutErrHopReassms"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrHopReassms;
    if (ot = text2obj ("clnpOutErrUnsOptions"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnsOptions;
    if (ot = text2obj ("clnpOutErrUnsVersions"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnsVersions;
    if (ot = text2obj ("clnpOutErrUnsSecurities"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnsSecurities;
    if (ot = text2obj ("clnpOutErrUnsSRs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnsSRs;
    if (ot = text2obj ("clnpOutErrUnsRRs"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrUnsRRs;
    if (ot = text2obj ("clnpOutErrInterferences"))
	ot -> ot_getfnx = o_clnp,
	ot -> ot_info = (caddr_t) clnpOutErrInterferences;

    if (ot = text2obj ("esisESHins"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisESHins;
    if (ot = text2obj ("esisESHouts"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisESHouts;
    if (ot = text2obj ("esisISHins"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisISHins;
    if (ot = text2obj ("esisISHouts"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisISHouts;
    if (ot = text2obj ("esisRDUins"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisRDUins;
    if (ot = text2obj ("esisRDUouts"))
	ot -> ot_getfnx = o_esis,
	ot -> ot_info = (caddr_t) esisRDUouts;
}
#else

init_clns () {}

#endif
