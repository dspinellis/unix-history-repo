/* acsapstub.c - application entity titles -- stub directory service */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapstub.c,v 7.6 91/02/22 09:14:19 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapstub.c,v 7.6 91/02/22 09:14:19 mrose Interim $
 *
 * RFC1085 (LPP) support contributed by the Wollongong Group, Inc.
 *
 *
 * $Log:	acsapstub.c,v $
 * Revision 7.6  91/02/22  09:14:19  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/11/21  11:29:36  mrose
 * sun
 * 
 * Revision 7.4  90/11/04  19:14:37  mrose
 * update
 * 
 * Revision 7.3  90/11/04  14:22:31  mrose
 * update
 * 
 * Revision 7.2  90/10/15  18:19:28  mrose
 * zap-AET
 * 
 * Revision 7.1  90/07/09  14:30:41  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:22:00  mrose
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


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "psap.h"
#include "isoaddrs.h"
#include "internet.h"
#include "tailor.h"


#define	fetch_aet	str2aet_seq

/*    DATA */

static struct isoentity ies;

/*  */

AEI	str2aei_stub (designator, qualifier)
char   *designator,
       *qualifier;
{
#ifdef	LPP
    register int    n;
    int	    tcp_port,
	    udp_port;
    register struct servent *sp;
#endif
    register char  *cp;
    register struct NSAPaddr *na;
    register struct isoentity  *ie = &ies;

    if (fetch_aet (designator, qualifier, &ies) == NOTOK) {
#ifdef	LPP
	if (lpp_aet (designator, qualifier, &ies) != NOTOK)
	    goto found_it;
#endif

	return NULLAEI;
    }

    if (ie -> ie_addr.pa_addr.sa_addr.ta_naddr <= 0) {
	register int	i;
	register struct TSAPaddr *ta = &ie -> ie_addr.pa_addr.sa_addr;

	na = ta -> ta_addrs, ta -> ta_naddr = 1;
	bzero ((char *) na, sizeof *na);

	/* guess at what sort of address we have on our hands */

	if ((i = strlen (cp = designator)) > 6
	        && cp[0] == '4' && cp[2] == '0' && cp[3] == '0') {
	    na -> na_stack = NA_NSAP;
	    na -> na_community = ts_comm_nsap_default;
	    na -> na_addrlen = implode ((u_char *) na -> na_address,
					cp, strlen (cp));
	    goto found_it;
	}

	if (i <= NSAP_DTELEN + 1)
	   for (; *cp; cp++)
	       if (!isxdigit ((u_char) *cp))
		   break;
	if (*cp) {
	    register struct hostent *hp = gethostbystring (designator);

	    if (index (cp, '@') || index (cp, '=') || index (cp, ','))
		return NULLAEI;

	    na -> na_stack = NA_TCP;
	    na -> na_community = ts_comm_tcp_default;
	    if (hp) {
#ifdef	h_addr
		register char **ap;
#endif

		(void) strcpy (na -> na_domain,
			       inet_ntoa (*(struct in_addr *) hp -> h_addr));
#ifdef	h_addr
		for (ap = hp -> h_addr_list + 1; *ap; ap++) {
		    ta -> ta_naddr++, na++;

		    if (ta -> ta_naddr >= NTADDR)
			break;

		    bzero ((char *) na, sizeof *na);
		    na -> na_stack = NA_TCP;
		    na -> na_community = ts_comm_tcp_default;
		    (void) strcpy (na -> na_domain,
				   inet_ntoa (*(struct in_addr *) *ap));
		}
#endif
	    }
	    else
		/* hostname not in /etc/hosts or NIS */
		return (NULLAEI);
	}
	else {
	   (void) strcpy (na -> na_dte, designator);
	   na -> na_dtelen = strlen (na -> na_dte);
#ifdef	BRIDGE_X25
	   na -> na_stack = bridgediscrim (na) ? NA_BRG : NA_X25;
#else
	   na -> na_stack = NA_X25;
#endif
	   na -> na_community = ts_comm_x25_default;
	}
    }

#ifdef	LPP
    for (na = ie -> ie_addr.pa_addr.sa_addr.ta_addrs,
	 	n = ie -> ie_addr.pa_addr.sa_addr.ta_naddr;
	     n > 0;
	     na++, n--)
	if (na -> na_stack == NA_TCP && na -> na_port == 0) {
	    cp = strncmp (qualifier, "isode ", 6) ? qualifier : qualifier + 6;
	    if (sp = getservbyname (cp, "lpp"))
		tcp_port = udp_port = sp -> s_port;
	    else {
		tcp_port = (sp = getservbyname (cp, "tcp")) ? sp -> s_port : 0;
		udp_port = (sp = getservbyname (cp, "udp")) ? sp -> s_port : 0;
	    }
	    break;
	}

    for (na = ie -> ie_addr.pa_addr.sa_addr.ta_addrs,
	 	n = ie -> ie_addr.pa_addr.sa_addr.ta_naddr;
	     n > 0;
	     na++, n--)
	if (na -> na_stack == NA_TCP && na -> na_port == 0) {
	    register struct TSAPaddr *ta;

	    if (tcp_port) {
		na -> na_port = (u_short) tcp_port;
		na -> na_tset = NA_TSET_TCP;

		if (udp_port
		        && (ta = &ie -> ie_addr.pa_addr.sa_addr) -> ta_naddr
		    		    < NTADDR) {
		    register struct NSAPaddr *ua =
					    ta -> ta_addrs + ta -> ta_naddr++;

		    bzero ((char *) ua, sizeof *ua);
		    ua -> na_stack = NA_TCP;
		    ua -> na_community = ts_comm_tcp_default;
		    (void) strncpy (ua -> na_domain, na -> na_domain,
				    sizeof ua -> na_domain);
		    ua -> na_port = (u_short) udp_port;
		    ua -> na_tset = NA_TSET_UDP;
		}
	    }
	    else
		if (udp_port) {
		    na -> na_port = (u_short) udp_port;
		    na -> na_tset = NA_TSET_UDP;
		}
	}
#endif

found_it: ;
    if (addr_log -> ll_events & LLOG_DEBUG)
	_printent (ie);

    return oid2aei (&ie -> ie_identifier);
}

/*  */

#ifdef	LPP
static unsigned int elements[NELEM + 1];
static char objent[BUFSIZ];


static int  lpp_aet (designator, qualifier, ie)
char   *designator,
       *qualifier;
register struct isoentity *ie;
{
    register int    nelem;
    int	    tcp_port,
	    udp_port;
    char   *cp;
    register struct servent *sp;
    register OID	oid = &ie -> ie_identifier;
    register struct PSAPaddr *pa = &ie -> ie_addr;
    register struct TSAPaddr *ta = &pa -> pa_addr.sa_addr;
    register struct NSAPaddr *na;

    cp = strncmp (qualifier, "isode ", 6) ? qualifier : qualifier + 6;
    if (sp = getservbyname (cp, "lpp"))
	tcp_port = udp_port = sp -> s_port;
    else
	if ((tcp_port = (sp = getservbyname (cp, "tcp")) ? sp -> s_port : 0)
	    	    == 0
		&& (udp_port = (sp = getservbyname (cp, "udp")) ? sp -> s_port 
								: 0) == 0)
	    return NOTOK;

    nelem = 0;
    elements[nelem++] = 1, elements[nelem++] = 17, elements[nelem++] = 4,
    elements[nelem++] = 0, elements[nelem++] = 0;

    oid -> oid_elements = elements;
    oid -> oid_nelem = nelem;
    (void) sprintf (ie -> ie_descriptor = objent, "%s-%s", designator,
		    qualifier);

    (void) bzero ((char *) pa, sizeof *pa);
    na = ta -> ta_addrs;
    if (tcp_port) {
	na -> na_stack = NA_TCP;
	na -> na_community = ts_comm_tcp_default;
	(void) strncpy (na -> na_domain, designator, sizeof na -> na_domain);
	na -> na_port = (u_short) tcp_port;
	na -> na_tset = NA_TSET_TCP;
	na++;
    }
    if (udp_port) {
	na -> na_stack = NA_TCP;
	na -> na_community = ts_comm_tcp_default;
	(void) strncpy (na -> na_domain, designator, sizeof na -> na_domain);
	na -> na_port = (u_short) udp_port;
	na -> na_tset = NA_TSET_UDP;
	na++;
    }
    ta -> ta_naddr = na - ta -> ta_addrs;
    
    return OK;
}
#endif

/*  */

struct PSAPaddr *aei2addr_stub (aei)
AEI	aei;
{
    register struct isoentity  *ie = &ies;
    register struct PSAPaddr   *pa;

    if (aei -> aei_ap_title
	    ? oid_cmp (&ie -> ie_identifier, prim2oid (aei -> aei_ap_title))
	    : ie -> ie_identifier.oid_nelem > 0) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("aei2addr_stub cache miss on %s", sprintaei (aei)));

	return NULLPA;
    }
    pa = &ie -> ie_addr;
    if (addr_log -> ll_events & LLOG_DEBUG)
	_printent (ie);

    return (pa -> pa_addr.sa_addr.ta_naddr > 0 ? pa : NULLPA);
}

/*  */

#ifdef	LPP
/* ARGSUSED */

AEI	str2aei_dse (designator, qualifier, context, interactive)
char   *designator,
       *qualifier,
       *context;
int	interactive;
{
    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	  ("DSE lookup not supported for LPP"));
    return NULLAEI;
}


/* ARGSUSED */

struct PSAPaddr *aei2addr_dse (aei)
AEI	aei;
{
    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	  ("DSE lookup not supported for LPP"));

    return NULLPA;
}
#endif
