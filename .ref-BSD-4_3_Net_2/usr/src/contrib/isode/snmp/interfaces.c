/* interfaces.c - MIB realization of the Interfaces group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/interfaces.c,v 7.12 91/02/22 09:43:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/interfaces.c,v 7.12 91/02/22 09:43:23 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	interfaces.c,v $
 * Revision 7.12  91/02/22  09:43:23  mrose
 * Interim 6.8
 * 
 * Revision 7.11  91/01/11  15:34:19  mrose
 * sets
 * 
 * Revision 7.10  91/01/07  12:40:46  mrose
 * update
 * 
 * Revision 7.9  90/12/18  10:13:28  mrose
 * update
 * 
 * Revision 7.8  90/10/23  20:39:57  mrose
 * update
 * 
 * Revision 7.7  90/10/23  20:36:16  mrose
 * update
 * 
 * Revision 7.6  90/03/24  10:54:02  mrose
 * update
 * 
 * Revision 7.5  90/02/27  18:49:35  mrose
 * unix stuff
 * 
 * Revision 7.4  90/02/23  17:47:38  mrose
 * update
 * 
 * Revision 7.3  90/02/17  10:38:10  mrose
 * smux
 * 
 * Revision 7.2  90/01/27  08:21:47  mrose
 * touch-up
 * 
 * Revision 7.1  90/01/11  18:34:04  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:03  mrose
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
#ifdef	BSD44
#include <net/if_types.h>
#endif
#include <sys/ioctl.h>

/*  */

#define	TYPE_MIN	1		/* ifType */
#define	TYPE_OTHER	1
#define	TYPE_ETHER	6
#define	TYPE_P10	12
#define	TYPE_P80	13
#define	TYPE_MAX	28


#define	ADMIN_MIN	1		/* ifAdminStatus */
#define	ADMIN_MAX	3

#define	OPER_UP		1		/* ifOperStatus */
#define	OPER_DOWN	2


/* we assume that all interfaces are present at startup time and that they
   don't move around in memory... */

int	ifNumber = 0;

struct interface *ifs = NULL;

static struct address	 *afs = NULL;
struct address *afs_inet = NULL;
#ifdef	BSD44
struct address *afs_iso = NULL;
#endif

static	int	flush_if_cache = 0;

/*  */

#define	ifIndex		0
#define	ifDescr		1
#define	ifType		2		/* SEMI IMPLEMENTED */
#define	ifMtu		3
#define	ifSpeed		4		/* SEMI IMPLEMENTED */
#define	ifPhysAddress	5
#define	ifAdminStatus	6
#define	ifOperStatus	7
#ifdef	BSD44
#define	ifLastChange	8
#endif
#ifdef	BSD44
#define	ifInOctets	9
#endif
#define	ifInUcastPkts	10
#ifdef	BSD44
#define	ifInNUcastPkts	11
#define	ifInDiscards	12
#endif
#define	ifInErrors	13
#ifdef	BSD44
#define	ifInUnknownProtos 14
#define	ifOutOctets	15
#endif
#define	ifOutUcastPkts	16
#ifdef	BSD44
#define	ifOutNUcastPkts	17
#endif
#define	ifOutDiscards	18
#define	ifOutErrors	19
#define	ifOutQLen	20
#define	ifSpecific	21


static int  o_interfaces (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifnum,
	    ifvar;
    register struct interface *is;
    register struct ifnet *ifn;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
#ifdef	ifLastChange
    static   int lastq = -1;
    static   integer diff;
#endif

    if (get_interfaces (offset) == NOTOK)
	return generr (offset);

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1)
		return int_SNMP_error__status_noSuchName;
	    ifnum = oid -> oid_elements[oid -> oid_nelem - 1];
	    for (is = ifs; is; is = is -> ifn_next)
		if (is -> ifn_index == ifnum)
		    break;
	    if (is == NULL || !is -> ifn_ready)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		for (is = ifs; is; is = is -> ifn_next)
		    if (is -> ifn_ready)
			break;
		if (!is)
		    return NOTOK;
		ifnum = is -> ifn_index;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = ifnum;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else {
		int	i = ot -> ot_name -> oid_nelem;
		register struct interface *iz;

		if ((ifnum = oid -> oid_elements[i]) == 0) {
		    if ((is = ifs) == NULL)
			return NOTOK;
		    if (is -> ifn_ready)
			goto stuff_ifnum;
		    ifnum = 1;
		}
		for (is = iz = ifs; is; is = is -> ifn_next)
		    if ((iz = is) -> ifn_index == ifnum)
			break;
		for (is = iz -> ifn_next; is; is = is -> ifn_next)
		    if (is -> ifn_ready)
			break;
		if (!is)
		    return NOTOK;
stuff_ifnum: ;
		ifnum = is -> ifn_index;

		oid -> oid_elements[i] = ifnum;
		oid -> oid_nelem = i + 1;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }
    ifn = &is -> ifn_interface.ac_if;

    switch (ifvar) {
	case ifIndex:
	    return o_integer (oi, v, is -> ifn_index);

	case ifDescr:
	    return o_string (oi, v, is -> ifn_descr, strlen (is -> ifn_descr));

	case ifType:
	    if (is -> ifn_type < TYPE_MIN || is -> ifn_type > TYPE_MAX)
		is -> ifn_type = TYPE_OTHER;
	    return o_integer (oi, v, is -> ifn_type);

	case ifMtu:
	    return o_integer (oi, v, ifn -> if_mtu);

	case ifSpeed:
	    return o_integer (oi, v, is -> ifn_speed);

	case ifPhysAddress:
#ifdef	NEW_AT
	    return o_string (oi, v,
			     (char *) is -> ifn_interface.ac_enaddr,
			     sizeof is -> ifn_interface.ac_enaddr);
#else
	    return o_string (oi, v,
		       (char *) is -> ifn_interface.ac_enaddr.ether_addr_octet,
			sizeof is -> ifn_interface.ac_enaddr.ether_addr_octet);
#endif

	case ifAdminStatus:
	    return o_integer (oi, v, is -> ifn_admin);

	case ifOperStatus:
	    return o_integer (oi, v, ifn -> if_flags & IFF_UP ? OPER_UP
							      : OPER_DOWN);

#ifdef	ifLastChange
	case ifLastChange:
	    if ((diff = (ifn -> if_lastchange.tv_sec - my_boottime.tv_sec)
                                                                        * 100
		 	+ ((ifn -> if_lastchange.tv_usec - my_boottime.tv_usec)
                                                                    / 10000))
		    < 0)
		diff = 0;
	    return o_number (oi, v, (caddr_t) &diff);
#endif

#ifdef	ifInOctets
	case ifInOctets:
	    return o_integer (oi, v, ifn -> if_ibytes);
#endif

	case ifInUcastPkts:
#ifndef	BSD44
	    return o_integer (oi, v, ifn -> if_ipackets);
#else
	    return o_integer (oi, v, ifn -> if_ipackets - ifn -> if_imcasts);
#endif

#ifdef	ifInNUcastPkts
	case ifInNUcastPkts:
	    return o_integer (oi, v, ifn -> if_imcasts);
#endif

#ifdef	ifInDiscards
	case ifInDiscards:
	    return o_integer (oi, v, ifn -> if_iqdrops);
#endif

	case ifInErrors:
	    return o_integer (oi, v, ifn -> if_ierrors);

#ifdef	ifInUnknownProtos
	case ifInUnknownProtos:
	    return o_integer (oi, v, ifn -> if_noproto);
#endif

#ifdef	ifOutOctets
	case ifOutOctets:
	    return o_integer (oi, v, ifn -> if_obytes);
#endif

	case ifOutUcastPkts:
#ifndef	BSD44
	    return o_integer (oi, v, ifn -> if_opackets);
#else
	    return o_integer (oi, v, ifn -> if_opackets - ifn -> if_omcasts);
#endif

#ifdef	ifOutNUcastPkts
	case ifOutNUcastPkts:
	    return o_integer (oi, v, ifn -> if_omcasts);
#endif

	case ifOutDiscards:
	    return o_integer (oi, v, ifn -> if_snd.ifq_drops);

	case ifOutErrors:
	    return o_integer (oi, v, ifn -> if_oerrors);

	case ifOutQLen:
	    return o_integer (oi, v, ifn -> if_snd.ifq_len);

	case ifSpecific:
	    return o_specific (oi, v, (caddr_t) nullSpecific);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

set_interface (name, ava)
char   *name,
       *ava;
{
    int	    i;
    u_long  l;
    register char   *cp;
    register struct interface *is;

    for (is = ifs; is; is = is -> ifn_next)
	if (strcmp (is -> ifn_descr, name) == 0)
	    break;
    if (!is) {
	advise (LLOG_DEBUG, NULLCP, "no such interface as \"%s\"", name);
	return;
    }

    if ((cp = index (ava, '=')) == NULL)
	return;
    *cp++ = NULL;

    if (lexequ (ava, "ifType") == 0) {
	if (sscanf (cp, "%d", &i) != 1 || i < TYPE_MIN || i > TYPE_MAX) {
malformed: ;
	    advise (LLOG_EXCEPTIONS, NULLCP, "malformed attribute \"%s=%s\"",
		    ava, cp);
	    return;
	}

	switch (is -> ifn_type = i) {
	    case TYPE_ETHER:
	    case TYPE_P10:
	        is -> ifn_speed = 10000000;
		break;

	    case TYPE_P80:
	        is -> ifn_speed = 80000000;
		break;

	    default:
	        break;
	}
	return;
    }

    if (lexequ (ava, "ifSpeed") == 0) {
	if (sscanf (cp, "%U", &l) != 1)
	    goto malformed;

	is -> ifn_speed = l;
	return;
    }
    
    if (lexequ (ava, "ifAdminStatus") == 0) {
	if (sscanf (cp, "%d", &i) != 1 || i < ADMIN_MIN || i > ADMIN_MAX)
	    goto malformed;

	is -> ifn_admin = i;
	return;
    }

    advise (LLOG_EXCEPTIONS, NULLCP, "unknown attribute \"%s=%s\"", ava, cp);
}

/*  */

init_interfaces () {
    int	    i;
    struct ifnet *ifnet;
    register OT	    ot;
    register struct interface  *is,
			      **ifp;
    struct nlist nzs;
    register struct nlist *nz = &nzs;

    if (getkmem (nl + N_IFNET, (caddr_t) &ifnet, sizeof ifnet) == NOTOK) {
	register struct interface *ip;

disabled: ;
	advise (LLOG_EXCEPTIONS, NULLCP, "interfaces group disabled!");
        for (is = ifs; is; is = ip) {
	    ip = is -> ifn_next;

	    free ((char *) is);
	}
	ifs = NULL;
    
	return;
    }

    ifp = &ifs;
    for (i = 0; ifnet; i++) {
	register struct ifnet *ifn;

	if ((is = (struct interface *) calloc (1, sizeof *is)) == NULL)
	    adios (NULLCP, "out of memory");
	is -> ifn_index = i + 1;
	is -> ifn_indexmask = 1 << i;

	ifn = &is -> ifn_interface.ac_if;

	is -> ifn_offset = (unsigned long) ifnet;

	nz -> n_name = "struct ifnet", nz -> n_value = is -> ifn_offset;
	if (getkmem (nz, (caddr_t) ifn, sizeof is -> ifn_interface) == NOTOK)
	    goto disabled;
	ifnet = ifn -> if_next;

	nz -> n_name = "if_name",
	nz -> n_value = (unsigned long) ifn -> if_name;
	if (getkmem (nz, (caddr_t) is -> ifn_descr, sizeof is -> ifn_descr - 1)
		 == NOTOK)
	    goto disabled;
	is -> ifn_descr[sizeof is -> ifn_descr - 1] = NULL;
	(void) sprintf (is -> ifn_descr + strlen (is -> ifn_descr), "%d",
			ifn -> if_unit);

#ifdef	BSD44
	switch (is -> ifn_type = ifn -> if_type) {
	    case IFT_ETHER:
	    case IFT_P10:
	        is -> ifn_speed = 10000000;
		break;

	    case IFT_P80:
	        is -> ifn_speed = 80000000;
		break;

	    default:
	        break;
	}
#endif
	if (is -> ifn_type != TYPE_ETHER)
#ifdef	NEW_AT
	    bzero ((char *) is -> ifn_interface.ac_enaddr,
		   sizeof is -> ifn_interface.ac_enaddr);
#else
	    bzero ((char *) is -> ifn_interface.ac_enaddr.ether_addr_octet,
		   sizeof is -> ifn_interface.ac_enaddr.ether_addr_octet);
#endif

	is -> ifn_admin = OPER_UP;

	*ifp = is, ifp = &is -> ifn_next;

	if (debug)
	    advise (LLOG_DEBUG, NULLCP,
		    "add interface %d: %s 0x%x",
		    is -> ifn_index, is -> ifn_descr, is -> ifn_offset);
    }

    if (ot = text2obj ("ifNumber")) {
	ot -> ot_getfnx = o_generic;
	if ((ot -> ot_info = (caddr_t) malloc (sizeof (integer))) == NULL)
	    adios (NULLCP, "out of memory");
	*((integer *) ot -> ot_info) = ifNumber = i;
    }

    (void) get_interfaces (type_SNMP_PDUs_get__request);

    if (ot = text2obj ("ifIndex"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifIndex;
    if (ot = text2obj ("ifDescr"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifDescr;
    if (ot = text2obj ("ifType"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifType;
    if (ot = text2obj ("ifMtu"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifMtu;
    if (ot = text2obj ("ifSpeed"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifSpeed;
    if (ot = text2obj ("ifPhysAddress"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifPhysAddress;
    if (ot = text2obj ("ifAdminStatus"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifAdminStatus;
    if (ot = text2obj ("ifOperStatus"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOperStatus;
#ifdef	ifLastChange
    if (ot = text2obj ("ifLastChange"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifLastChange;
#endif
#ifdef	ifInOctets
    if (ot = text2obj ("ifInOctets"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInOctets;
#endif
    if (ot = text2obj ("ifInUcastPkts"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInUcastPkts;
#ifdef	ifInNUcastPkts
    if (ot = text2obj ("ifInNUcastPkts"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInNUcastPkts;
#endif
#ifdef	ifInDiscards
    if (ot = text2obj ("ifInDiscards"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInDiscards;
#endif
    if (ot = text2obj ("ifInErrors"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInErrors;
#ifdef	ifInUnknownProtos
    if (ot = text2obj ("ifInUnknownProtos"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifInUnknownProtos;
#endif
#ifdef	ifOutOctets
    if (ot = text2obj ("ifOutOctets"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutOctets;
#endif
    if (ot = text2obj ("ifOutUcastPkts"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutUcastPkts;
#ifdef	ifOutNUcastPkts
    if (ot = text2obj ("ifOutNUcastPkts"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutNUcastPkts;
#endif
    if (ot = text2obj ("ifOutDiscards"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutDiscards;
    if (ot = text2obj ("ifOutErrors"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutErrors;
    if (ot = text2obj ("ifOutQLen"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifOutQLen;
    if (ot = text2obj ("ifSpecific"))
	ot -> ot_getfnx = o_interfaces,
	ot -> ot_info = (caddr_t) ifSpecific;
}

/*  */

static int  adr_compar (a, b)
register struct address **a,
			**b;
{
    int    i;

    if ((i = (*a) -> adr_address.sa.sa_family
	 	- (*b) -> adr_address.sa.sa_family))
	return (i > 0 ? 1 : -1);

    return elem_cmp ((*a) -> adr_instance, (*a) -> adr_insize,
		     (*b) -> adr_instance, (*b) -> adr_insize);
}


int	get_interfaces (offset)
int	offset;
{
    int	    adrNumber = 0;
    register OT	    ot;
    register struct interface  *is;
    register struct address    *as,
			       *ap,
			      **base,
			      **afe,
			      **afp;
    static   int first_time = 1;
    static   int lastq = -1;

    if (quantum == lastq)
	return OK;
    if (!flush_if_cache
	    && offset == type_SNMP_PDUs_get__next__request
	    && quantum == lastq + 1) {			/* XXX: caching! */
	lastq = quantum;
	return OK;
    }
    lastq = quantum, flush_if_cache = 0;

    for (as = afs; as; as = ap) {
	ap = as -> adr_next;

	free ((char *) as);
    }
    afs = afs_inet = NULL;
#ifdef	BSD44
    afs_iso = NULL;
#endif

    afp = &afs;
    for (is = ifs; is; is = is -> ifn_next) {
	struct arpcom ifns;
	register struct ifnet *ifn = &ifns.ac_if;
#ifdef	BSD43
	struct ifaddr ifaddr;
	register struct ifaddr *ifa;
#ifdef	BSD44
	union sockaddr_un ifsocka,
			  ifsockb;
#endif
	union sockaddr_un ifsockc;
	register union sockaddr_un *ia,
				   *ib;
	register union sockaddr_un *ic = &ifsockc;
#endif
#ifndef	BSD44
	struct ifreq ifreq;
#endif
	struct nlist nzs;
	register struct nlist *nz = &nzs;

	nz -> n_name = "struct ifnet", nz -> n_value = is -> ifn_offset;
	if (getkmem (nz, (caddr_t) ifn, sizeof ifns) == NOTOK)
	    return NOTOK;

#ifndef	BSD43
	if (ifn -> if_addr.sa_family == AF_UNSPEC)
	    continue;

	if (nd != NOTOK) {
	    (void) strcpy (ifreq.ifr_name, is -> ifn_descr);
	    if (ioctl (nd, SIOCGIFNETMASK, (char *) &ifreq) == NOTOK) {
		if (debug)
		    advise (LLOG_EXCEPTIONS, "failed", "SIOCGIFNETMASK on %s",
			    is -> ifn_descr);
		bzero ((char *) &ifreq, sizeof ifreq);
	    }
	}
	else
	    bzero ((char *) &ifreq, sizeof ifreq);
	if (ifn -> if_addr.sa_family == AF_INET) {
	    struct sockaddr_in *sx = (struct sockaddr_in *) &ifreq.ifr_addr;
	    struct sockaddr_in *sy = (struct sockaddr_in *) &ifn -> if_addr;

	    if (sx -> sin_addr.s_addr == 0) {
		if (IN_CLASSA (sy -> sin_addr.s_addr))
		    sx -> sin_addr.s_addr = IN_CLASSA_NET;
		else
		    if (IN_CLASSB (sy -> sin_addr.s_addr))
			sx -> sin_addr.s_addr = IN_CLASSB_NET;
		    else
			sx -> sin_addr.s_addr = IN_CLASSC_NET;
	    }
	}

	if (as = find_address ((union sockaddr_un *) &ifn -> if_addr))
	    as -> adr_indexmask |= is -> ifn_indexmask;
	else {
	    if ((as = (struct address *) calloc (1, sizeof *as)) == NULL)
		adios (NULLCP, "out of memory");
	    *afp = as, afp = &as -> adr_next, adrNumber++;

	    as -> adr_address.sa = ifn -> if_addr;	      /* struct copy */
	    if (ifn -> if_addr.sa_family == AF_INET)
		as -> adr_broadaddr.sa = ifn -> if_broadaddr; /*   .. */
	    as -> adr_netmask.sa = ifreq.ifr_addr;	      /*   .. */
	    as -> adr_indexmask = is -> ifn_indexmask;

	    switch (ifn -> if_addr.sa_family) {
		case AF_INET:
		    as -> adr_insize =
			ipaddr2oid (as -> adr_instance,
				    &((struct sockaddr_in *) &ifn -> if_addr)
								-> sin_addr);
		    if (afs_inet == NULL)	/* needed for find_address */
			afs_inet = as;
		    break;

		default:
		    bzero ((char *) as -> adr_instance,
			   sizeof as -> adr_instance);
		    as -> adr_insize = 0;
		    break;
	    }
	}
#else
#ifndef	BSD44
	ia = (union sockaddr_un *) &ifaddr.ifa_addr,
	ib = (union sockaddr_un *) &ifaddr.ifa_broadaddr;
#else
	ia = &ifsocka, ib = &ifsockb;
#endif

	for (ifa = ifn -> if_addrlist; ifa; ifa = ifaddr.ifa_next) {
	    nz -> n_name = "struct ifaddr",
	    nz -> n_value = (unsigned long) ifa;
	    if (getkmem (nz, (caddr_t) &ifaddr, sizeof ifaddr) == NOTOK)
		continue;
#ifndef	BSD44
	    if (ia -> sa.sa_family == AF_UNSPEC)
		continue;

	    if (nd != NOTOK) {
		(void) strcpy (ifreq.ifr_name, is -> ifn_descr);
		if (ioctl (nd, SIOCGIFNETMASK, (char *) &ifreq) == NOTOK) {
		    if (debug)
			advise (LLOG_EXCEPTIONS, "failed",
				"SIOCGIFNETMASK on %s", is -> ifn_descr);
		    bzero ((char *) ic, sizeof *ic);
		}
		ic -> sa = ifreq.ifr_addr;	/* struct copy */
	    }
	    else
		bzero ((char *) ic, sizeof *ic);
#else
	    nz -> n_name = "union sockaddr_un",
	    nz -> n_value = (unsigned long) ifaddr.ifa_addr;
	    if (getkmem (nz, (caddr_t) ia, sizeof *ia) == NOTOK)
		continue;

	    if (ia -> sa.sa_family == AF_UNSPEC)
		continue;

	    if (ia -> sa.sa_family == AF_INET) {
		nz -> n_value = (unsigned long) ifaddr.ifa_broadaddr;
		if (getkmem (nz, (caddr_t) ib, sizeof *ib) == NOTOK)
		    continue;
	    }

	    nz -> n_value = (unsigned long) ifaddr.ifa_netmask;
	    if (getkmem (nz, (caddr_t) ic, sizeof *ic) == NOTOK)
		continue;
#endif
	    if (ia -> sa.sa_family == AF_INET
		    && ic -> un_in.sin_addr.s_addr == 0) {
		if (IN_CLASSA (ia -> un_in.sin_addr.s_addr))
		    ic -> un_in.sin_addr.s_addr = IN_CLASSA_NET;
		else
		    if (IN_CLASSB (ia -> un_in.sin_addr.s_addr))
			ic -> un_in.sin_addr.s_addr = IN_CLASSB_NET;
		    else
			ic -> un_in.sin_addr.s_addr = IN_CLASSC_NET;
	    }

	    if (as = find_address (ia))
		as -> adr_indexmask |= is -> ifn_indexmask;
	    else {
		if ((as = (struct address *) calloc (1, sizeof *as)) == NULL)
		    adios (NULLCP, "out of memory");
		*afp = as, afp = &as -> adr_next, adrNumber++;

		as -> adr_address = *ia;		/* struct copy */
		if (ia -> sa.sa_family == AF_INET)
		    as -> adr_broadaddr = *ib;		/* struct copy */
		as -> adr_netmask = *ic;		/*   .. */

		as -> adr_indexmask = is -> ifn_indexmask;

		switch (ia -> sa.sa_family) {
		    case AF_INET:
			as -> adr_insize =
			    ipaddr2oid (as -> adr_instance,
					&ia -> un_in.sin_addr);
			if (afs_inet == NULL)	/* needed for find_address */
			    afs_inet = as;
			break;

#ifdef	BSD44
		    case AF_ISO:
			as -> adr_insize =
			    clnpaddr2oid (as -> adr_instance,
					  &ia -> un_iso.siso_addr);
			if (afs_iso == NULL)	/* needed for find_address */
			    afs_iso = as;
			break;
#endif

		    default:
			bzero ((char *) as -> adr_instance,
			       sizeof as -> adr_instance);
			as -> adr_insize = 0;
			break;
		}
	    }
	}
#endif

	is -> ifn_interface = ifns;	/* struct copy */

	if (is -> ifn_type != TYPE_ETHER)
#ifdef	NEW_AT
	    bzero ((char *) is -> ifn_interface.ac_enaddr,
		   sizeof is -> ifn_interface.ac_enaddr);
#else
	    bzero ((char *) is -> ifn_interface.ac_enaddr.ether_addr_octet,
		   sizeof is -> ifn_interface.ac_enaddr.ether_addr_octet);
#endif
    }

    ifNumber = 0;
    for (is = ifs; is; is = is -> ifn_next) {
	is -> ifn_ready = 0;

	for (as = afs; as; as = as -> adr_next)
	    if (as -> adr_indexmask & is -> ifn_indexmask)
		break;
	if (as)
	    ifNumber += (is -> ifn_ready = 1);
    }
    if (ot = text2obj ("ifNumber"))
	*((integer *) ot -> ot_info) = ifNumber;

    if (debug && first_time) {
	first_time = 0;

	for (as = afs; as; as = as -> adr_next) {
	    OIDentifier	oids;

	    oids.oid_elements = as -> adr_instance;
	    oids.oid_nelem = as -> adr_insize;
	    advise (LLOG_DEBUG, NULLCP,
		    "add address: %d/%s with mask 0x%x",
		    as -> adr_address.sa.sa_family, sprintoid (&oids),
		    as -> adr_indexmask);
	}
    }

    if (adrNumber <= 1)
	return OK;

    if ((base = (struct address **)
		    malloc ((unsigned) (adrNumber * sizeof *base))) == NULL)
	adios (NULLCP, "out of memory");

    afe = base;
    for (as = afs; as; as = as -> adr_next)
	*afe++ = as;

    qsort ((char *) base, adrNumber, sizeof *base, adr_compar);

    afp = base;
    as = afs = *afp++;
    afs_inet = NULL;
#ifdef	BSD44
    afs_iso = NULL;
#endif
    while (afp < afe) {
	switch (as -> adr_address.sa.sa_family) {
	    case AF_INET:
	        if (afs_inet == NULL)
		    afs_inet = as;
	        break;

#ifdef	BSD44
	    case AF_ISO:
	        if (afs_iso == NULL)
		    afs_iso = as;
	        break;
#endif
	}

	as -> adr_next = *afp;
	as = *afp++;
    }
    switch (as -> adr_address.sa.sa_family) {
        case AF_INET:
	    if (afs_inet == NULL)
		afs_inet = as;
	    break;

#ifdef	BSD44
	case AF_ISO:
	    if (afs_iso == NULL)
		afs_iso = as;
	    break;
#endif
    }
    as -> adr_next = NULL;

    free ((char *) base);

    return OK;
}

/*  */

struct address *find_address (addr)
register union sockaddr_un *addr;
{
    register struct address *as;
    register struct in_addr *in;
#ifdef	BSD44
    register struct iso_addr *iso;
#endif

    switch (addr -> sa.sa_family) {
	case AF_INET:
	    in = &addr -> un_in.sin_addr;
	    for (as = afs_inet; as; as = as -> adr_next)
		if (as -> adr_address.sa.sa_family != AF_INET)
		    break;
		else
		    if (bcmp ((char *) in,
			      (char *) &as -> adr_address.un_in.sin_addr,
			      sizeof *in) == 0)
			return as;
	    break;

#ifdef	BSD44
	case AF_ISO:
	    iso = &addr -> un_iso.siso_addr;
	    for (as = afs_iso; as; as = as -> adr_next)
		if (as -> adr_address.sa.sa_family != AF_ISO)
		    break;
		else
		    if (bcmp ((char *) iso,
			      (char *) &as -> adr_address.un_iso.siso_addr,
			      sizeof *iso) == 0)
			return as;
	    
	    break;
#endif

	default:
	    break;
    }

    return NULL;
}

/*  */

struct address *get_addrent (ip, len, head, isnext)
register unsigned int *ip;
int	len;
struct address *head;
int	isnext;
{
    int	    family;
    register struct address *as;

    if (head)
	family = head -> adr_address.sa.sa_family;
    for (as = head; as; as = as -> adr_next)
	if (as -> adr_address.sa.sa_family != family)
	    break;
	else
	    switch (elem_cmp (as -> adr_instance, as -> adr_insize, ip, len)) {
		case 0:
		    if (!isnext)
			return as;
		    if ((as = as -> adr_next) == NULL
		            || as -> adr_address.sa.sa_family != family)
			goto out;
		    /* else fall... */

		case 1:
		    return (isnext ? as : NULL);
	    }

out: ;
    flush_if_cache = 1;

    return NULL;
}
