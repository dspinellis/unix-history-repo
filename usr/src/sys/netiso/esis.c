/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*	@(#)esis.c	7.8 (Berkeley) %G% */
#ifndef lint
static char *rcsid = "$Header: esis.c,v 4.10 88/09/15 18:57:03 hagens Exp $";
#endif

#ifdef ISO

#include "types.h"
#include "param.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "user.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "kernel.h"

#include "../net/if.h"
#include "../net/if_dl.h"
#include "../net/route.h"

#include "iso.h"
#include "iso_pcb.h"
#include "iso_var.h"
#include "iso_snpac.h"
#include "clnl.h"
#include "clnp.h"
#include "clnp_stat.h"
#include "esis.h"
#include "argo_debug.h"

/*
 *	Global variables to esis implementation
 *
 *	esis_holding_time - the holding time (sec) parameter for outgoing pdus
 *	esis_config_time  - the frequency (sec) that hellos are generated
 *
 */
struct isopcb	esis_pcb;
int				esis_sendspace = 2048;
int				esis_recvspace = 2048;
short			esis_holding_time = ESIS_HT;
short			esis_config_time = ESIS_CONFIG;
extern int		iso_systype;
extern char		all_es_snpa[], all_is_snpa[];

#define EXTEND_PACKET(m, mhdr, cp)\
	if (((m)->m_next = m_getclr(M_DONTWAIT, MT_HEADER)) == NULL) {\
		esis_stat.es_nomem++;\
		m_freem(mhdr);\
		return;\
	} else {\
		(m) = (m)->m_next;\
		(cp) = mtod((m), caddr_t);\
	}
/*
 * FUNCTION:		esis_init
 *
 * PURPOSE:			Initialize the kernel portion of esis protocol
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_init()
{
	extern struct clnl_protosw clnl_protox[256];
	int esis_input();
	int snpac_age();
	int	esis_config();
#ifdef	ISO_X25ESIS
	x25esis_input();
#endif	ISO_X25ESIS

	esis_pcb.isop_next = esis_pcb.isop_prev = &esis_pcb;

	clnl_protox[ISO9542_ESIS].clnl_input = esis_input;
	llinfo_llc.lc_next = llinfo_llc.lc_prev = &llinfo_llc;
	timeout(snpac_age, (caddr_t)0, hz);
	timeout(esis_config, (caddr_t)0, hz);

#ifdef	ISO_X25ESIS
	clnl_protox[ISO9542X25_ESIS].clnl_input = x25esis_input;
#endif	ISO_X25ESIS
}

/*
 * FUNCTION:		esis_usrreq
 *
 * PURPOSE:			Handle user level esis requests
 *
 * RETURNS:			0 or appropriate errno
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This is here only so esis gets initialized.
 */
/*ARGSUSED*/
esis_usrreq(so, req, m, nam, control)
struct socket	*so;		/* socket: used only to get to this code */
int				req;		/* request */
struct mbuf		*m;			/* data for request */
struct mbuf		*nam;		/* optional name */
struct mbuf		*control;	/* optional control */
{
	if (m != NULL)
		m_freem(m);

	return(EOPNOTSUPP);
}

/*
 * FUNCTION:		esis_input
 *
 * PURPOSE:			Process an incoming esis packet
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_input(m0, shp)
struct mbuf		*m0;		/* ptr to first mbuf of pkt */
struct snpa_hdr	*shp;	/* subnetwork header */
{
	struct esis_fixed	*pdu = mtod(m0, struct esis_fixed *);
	register int type;

	IFDEBUG(D_ESISINPUT)
		int i;

		printf("esis_input: pdu on ifp x%x (%s%d): from:", shp->snh_ifp, 
			shp->snh_ifp->if_name, shp->snh_ifp->if_unit);
		for (i=0; i<6; i++)
			printf("%x%c", shp->snh_shost[i]&0xff, (i<5) ? ':' : ' ');
		printf(" to:");
		for (i=0; i<6; i++)
			printf("%x%c", shp->snh_dhost[i]&0xff, (i<5) ? ':' : ' ');
		printf("\n");
	ENDDEBUG

	/*
	 *	check checksum if necessary
	 */
	if (ESIS_CKSUM_REQUIRED(pdu) && iso_check_csum(m0, (int)pdu->esis_hdr_len)) {
		esis_stat.es_badcsum++;
		goto bad;
	}

	/* check version */
	if (pdu->esis_vers != ESIS_VERSION) {
		esis_stat.es_badvers++;
		goto bad;
	}

	type = pdu->esis_type & 0x1f;
	switch (type) {
		case ESIS_ESH:
			esis_eshinput(m0, shp);
			return;

		case ESIS_ISH:
			esis_ishinput(m0, shp);
			return;

		case ESIS_RD:
			esis_rdinput(m0, shp);
			return;

		default: {
			esis_stat.es_badtype++;
			goto bad;
		}
	}

bad:
	m_freem(m0);
}

/*
 * FUNCTION:		esis_rdoutput
 *
 * PURPOSE:			Transmit a redirect pdu
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Assumes there is enough space for fixed part of header,
 *					DA, BSNPA and NET in first mbuf.
 */
esis_rdoutput(inbound_shp, inbound_m, inbound_oidx, rd_dstnsap, rt)
struct snpa_hdr		*inbound_shp;	/* snpa hdr from incoming packet */
struct mbuf			*inbound_m;		/* incoming pkt itself */
struct clnp_optidx	*inbound_oidx;	/* clnp options assoc with incoming pkt */
struct iso_addr		*rd_dstnsap;	/* ultimate destination of pkt */
struct rtentry		*rt;			/* snpa cache info regarding next hop of
										pkt */
{
	struct mbuf			*m, *m0;
	caddr_t				cp;
	struct esis_fixed	*pdu;
	int					len, total_len = 0;
	struct sockaddr_iso	siso;
	struct ifnet 		*ifp = inbound_shp->snh_ifp;
	struct sockaddr_dl *sdl;
	struct iso_addr *rd_gwnsap;

	if (rt->rt_flags & RTF_GATEWAY) {
		rd_gwnsap = &((struct sockaddr_iso *)rt->rt_gateway)->siso_addr;
		rt = rtalloc1(rt->rt_gateway, 0);
	} else
		rd_gwnsap = &((struct sockaddr_iso *)rt_key(rt))->siso_addr;
	if (rt == 0 || (sdl = (struct sockaddr_dl *)rt->rt_gateway) == 0 ||
		sdl->sdl_family != AF_LINK) {
		/* maybe we should have a function that you
		   could put in the iso_ifaddr structure
		   which could translate iso_addrs into snpa's
		   where there is a known mapping for that address type */
		esis_stat.es_badtype++;
		return;
	}
	esis_stat.es_rdsent++;
	IFDEBUG(D_ESISOUTPUT)
		printf("esis_rdoutput: ifp x%x (%s%d), ht %d, m x%x, oidx x%x\n",
			ifp, ifp->if_name, ifp->if_unit, esis_holding_time, inbound_m,
			inbound_oidx);
		printf("\tdestination: %s\n", clnp_iso_addrp(rd_dstnsap));
		printf("\tredirected toward:%s\n", clnp_iso_addrp(rd_gwnsap));
	ENDDEBUG

	if ((m0 = m = m_gethdr(M_DONTWAIT, MT_HEADER)) == NULL) {
		esis_stat.es_nomem++;
		return;
	}
	bzero(mtod(m, caddr_t), MHLEN);

	pdu = mtod(m, struct esis_fixed *);
	cp = (caddr_t)(pdu + 1); /*pointer arith.; 1st byte after header */
	len = sizeof(struct esis_fixed);

	/*
	 *	Build fixed part of header
	 */
	pdu->esis_proto_id = ISO9542_ESIS;
	pdu->esis_vers = ESIS_VERSION;
	pdu->esis_type = ESIS_RD;
	HTOC(pdu->esis_ht_msb, pdu->esis_ht_lsb, esis_holding_time);

	/* Insert destination address */
	(void) esis_insert_addr(&cp, &len, rd_dstnsap, m, 0);

	/* Insert the snpa of better next hop */
	*cp++ = sdl->sdl_alen;
	bcopy(LLADDR(sdl), cp, sdl->sdl_alen);
	cp += sdl->sdl_alen;
	len += (sdl->sdl_alen + 1);

	/* 
	 *	If the next hop is not the destination, then it ought to be
	 *	an IS and it should be inserted next. Else, set the
	 *	NETL to 0
	 */
	/* PHASE2 use mask from ifp of outgoing interface */
	if (!iso_addrmatch1(rd_dstnsap, rd_gwnsap)) {
		/* this should not happen: 
		if ((nhop_sc->sc_flags & SNPA_IS) == 0) {
			printf("esis_rdoutput: next hop is not dst and not an IS\n");
			m_freem(m0);
			return;
		} */
		(void) esis_insert_addr(&cp, &len, rd_gwnsap, m, 0);
	} else {
		*cp++ = 0;	/* NETL */
		len++;
	}
	m->m_len = len;

	/*
	 *	PHASE2
	 *	If redirect is to an IS, add an address mask. The mask to be
	 *	used should be the mask present in the routing entry used to
	 *	forward the original data packet.
	 */
	
	/*
	 *	Copy Qos, priority, or security options present in original npdu
	 */
	if (inbound_oidx) {
		/* THIS CODE IS CURRENTLY (mostly) UNTESTED */
		int optlen = 0;
		if (inbound_oidx->cni_qos_formatp)
			optlen += (inbound_oidx->cni_qos_len + 2);
		if (inbound_oidx->cni_priorp)	/* priority option is 1 byte long */
			optlen += 3;
		if (inbound_oidx->cni_securep)
			optlen += (inbound_oidx->cni_secure_len + 2);
		if (M_TRAILINGSPACE(m) < optlen) {
			EXTEND_PACKET(m, m0, cp);
			m->m_len = 0;
			/* assumes MLEN > optlen */
		}
		/* assume MLEN-len > optlen */
		/* 
		 *	When copying options, copy from ptr - 2 in order to grab
		 *	the option code and length
		 */
		if (inbound_oidx->cni_qos_formatp) {
			bcopy(mtod(inbound_m, caddr_t) + inbound_oidx->cni_qos_formatp - 2,
				cp, (unsigned)(inbound_oidx->cni_qos_len + 2));
			cp += inbound_oidx->cni_qos_len + 2;
		}
		if (inbound_oidx->cni_priorp) {
			bcopy(mtod(inbound_m, caddr_t) + inbound_oidx->cni_priorp - 2,
					cp, 3);
			cp += 3;
		}
		if (inbound_oidx->cni_securep) {
			bcopy(mtod(inbound_m, caddr_t) + inbound_oidx->cni_securep - 2, cp, 
				(unsigned)(inbound_oidx->cni_secure_len + 2));
			cp += inbound_oidx->cni_secure_len + 2;
		}
		m->m_len += optlen;
		len += optlen;
	}

	pdu->esis_hdr_len = m0->m_pkthdr.len = len;
	iso_gen_csum(m0, ESIS_CKSUM_OFF, (int)pdu->esis_hdr_len);

	bzero((caddr_t)&siso, sizeof(siso));
	siso.siso_len = 12;
	siso.siso_family = AF_ISO;
	siso.siso_data[0] = AFI_SNA;
	siso.siso_nlen = 6 + 1;	/* should be taken from snpa_hdr */
										/* +1 is for AFI */
	bcopy(inbound_shp->snh_shost, siso.siso_data + 1, 6);
	(ifp->if_output)(ifp, m0, &siso, 0);
}

/*
 * FUNCTION:		esis_insert_addr
 *
 * PURPOSE:			Insert an iso_addr into a buffer
 *
 * RETURNS:			true if buffer was big enough, else false
 *
 * SIDE EFFECTS:	Increment buf & len according to size of iso_addr
 *
 * NOTES:			Plus 1 here is for length byte
 */
esis_insert_addr(buf, len, isoa, m, nsellen)
register caddr_t			*buf;		/* ptr to buffer to put address into */
int							*len;		/* ptr to length of buffer so far */
register struct iso_addr	*isoa;		/* ptr to address */
register struct mbuf		*m;			/* determine if there remains space */
int							nsellen;
{
	register int newlen, result = 0;

	isoa->isoa_len -= nsellen;
	newlen = isoa->isoa_len + 1;
	if (newlen <=  M_TRAILINGSPACE(m)) {
		bcopy((caddr_t)isoa, *buf, newlen);
		*len += newlen;
		*buf += newlen;
		m->m_len += newlen;
		result = 1;
	}
	isoa->isoa_len += nsellen;
	return (result);
}

#define ESIS_EXTRACT_ADDR(d, b) { d = (struct iso_addr *)(b); b += (1 + *b); \
	    if (b > buflim) {esis_stat.es_toosmall++; goto bad;}}
#define ESIS_NEXT_OPTION(b)	{ b += (2 + b[1]); \
	    if (b > buflim) {esis_stat.es_toosmall++; goto bad;}}
int ESHonly = 1;
/*
 
/*
 * FUNCTION:		esis_eshinput
 *
 * PURPOSE:			Process an incoming ESH pdu
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_eshinput(m, shp)
struct mbuf		*m;	/* esh pdu */
struct snpa_hdr	*shp;	/* subnetwork header */
{
	struct	esis_fixed	*pdu = mtod(m, struct esis_fixed *);
	u_short				ht;		/* holding time */
	struct	iso_addr	*nsap;
	int					naddr;
	u_char				*buf = (u_char *)(pdu + 1);
	u_char				*buflim = pdu->esis_hdr_len + (u_char *)pdu;
	int					new_entry = 0;

	esis_stat.es_eshrcvd++;

	CTOH(pdu->esis_ht_msb, pdu->esis_ht_lsb, ht);

	naddr = *buf++;
	if (buf >= buflim)
		goto bad;
	if (naddr == 1) {
		ESIS_EXTRACT_ADDR(nsap, buf);
		new_entry = snpac_add(shp->snh_ifp,
								 nsap, shp->snh_shost, SNPA_ES, ht, 0);
	} else {
		int nsellength = 0, nlen = 0;
		{
		/* See if we want to compress out multiple nsaps differing
		   only by nsel */
			register struct ifaddr *ifa = shp->snh_ifp->if_addrlist;
			for (; ifa; ifa = ifa->ifa_next)
				if (ifa->ifa_addr->sa_family == AF_ISO) {
					nsellength = ((struct iso_ifaddr *)ifa)->ia_addr.siso_tlen;
					break;
			}
		}
		IFDEBUG(D_ESISINPUT)
			printf("esis_eshinput: esh: ht %d, naddr %d nsellength %d\n",
					ht, naddr, nsellength);
		ENDDEBUG
		while (naddr-- > 0) {
			struct iso_addr *nsap2; u_char *buf2;
			ESIS_EXTRACT_ADDR(nsap, buf);
			/* see if there is at least one more nsap in ESH differing
			   only by nsel */
			if (nsellength != 0) for (buf2 = buf; buf2 < buflim;) {
				ESIS_EXTRACT_ADDR(nsap2, buf2);
				IFDEBUG(D_ESISINPUT)
					printf("esis_eshinput: comparing %s ", 
						clnp_iso_addrp(nsap));
					printf("and %s\n", clnp_iso_addrp(nsap2));
				ENDDEBUG
				if (Bcmp(nsap->isoa_genaddr, nsap2->isoa_genaddr,
						 nsap->isoa_len - nsellength) == 0) {
					nlen = nsellength;
					break;
				}
			}
			new_entry |= snpac_add(shp->snh_ifp,
									nsap, shp->snh_shost, SNPA_ES, ht, nlen);
			nlen = 0;
		}
	}
	IFDEBUG(D_ESISINPUT)
		printf("esis_eshinput: nsap %s is %s\n", 
			clnp_iso_addrp(nsap), new_entry ? "new" : "old");
	ENDDEBUG
	if (new_entry && (iso_systype & SNPA_IS))
		esis_shoutput(shp->snh_ifp, ESIS_ISH, esis_holding_time,
						shp->snh_shost, 6, (struct iso_addr *)0);
bad:
	m_freem(m);
	return;
}

/*
 * FUNCTION:		esis_ishinput
 *
 * PURPOSE:			process an incoming ISH pdu
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_ishinput(m, shp)
struct mbuf		*m;	/* esh pdu */
struct snpa_hdr	*shp;	/* subnetwork header */
{
	struct esis_fixed	*pdu = mtod(m, struct esis_fixed *);
	u_short				ht;					/* holding time */
	struct iso_addr		*nsap; 				/* Network Entity Title */
	register u_char		*buf = (u_char *) (pdu + 1);
	register u_char		*buflim = pdu->esis_hdr_len + (u_char *)pdu;
	int					new_entry;

	esis_stat.es_ishrcvd++;
	CTOH(pdu->esis_ht_msb, pdu->esis_ht_lsb, ht);

	IFDEBUG(D_ESISINPUT)
		printf("esis_ishinput: ish: ht %d\n", ht);
	ENDDEBUG
	if (ESHonly)
		goto bad;

	ESIS_EXTRACT_ADDR(nsap, buf);

	while (buf < buflim) {
		switch (*buf) {
		case ESISOVAL_ESCT:
			if (buf[1] != 2)
				goto bad;
			CTOH(buf[2], buf[3], esis_config_time);
			break;
		
		default:
			printf("Unknown ISH option: %x\n", *buf);
		}
		ESIS_NEXT_OPTION(buf);
	}
	new_entry = snpac_add(shp->snh_ifp, nsap, shp->snh_shost, SNPA_IS, ht, 0);
	IFDEBUG(D_ESISINPUT)
		printf("esis_ishinput: nsap %s is %s\n", 
			clnp_iso_addrp(nsap), new_entry ? "new" : "old");
	ENDDEBUG

	if (new_entry)
		esis_shoutput(shp->snh_ifp, 
			iso_systype & SNPA_ES ? ESIS_ESH : ESIS_ISH,
			esis_holding_time, shp->snh_shost, 6, (struct iso_addr *)0);
bad:
	m_freem(m);
	return;
}

/*
 * FUNCTION:		esis_rdinput
 *
 * PURPOSE:			Process an incoming RD pdu
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_rdinput(m0, shp)
struct mbuf		*m0;	/* esh pdu */
struct snpa_hdr	*shp;	/* subnetwork header */
{
	struct esis_fixed	*pdu = mtod(m0, struct esis_fixed *);
	u_short				ht;		/* holding time */
	struct iso_addr		*da, *net = 0, *netmask = 0, *snpamask = 0;
	register struct iso_addr *bsnpa;
	register u_char		*buf = (u_char *)(pdu + 1);
	register u_char		*buflim = pdu->esis_hdr_len + (u_char *)pdu;

	esis_stat.es_rdrcvd++;

	/* intermediate systems ignore redirects */
	if (iso_systype & SNPA_IS)
		goto bad;
	if (ESHonly)
		goto bad;

	CTOH(pdu->esis_ht_msb, pdu->esis_ht_lsb, ht);
	if (buf >= buflim)
		goto bad;

	/* Extract DA */
	ESIS_EXTRACT_ADDR(da, buf);

	/* Extract better snpa */
	ESIS_EXTRACT_ADDR(bsnpa, buf);

	/* Extract NET if present */
	if (buf < buflim) {
		if (*buf == 0)
			buf++; /* no NET present, skip NETL anyway */
		else
			ESIS_EXTRACT_ADDR(net, buf);
	}

	/* process options */
	while (buf < buflim) {
		switch (*buf) {
		case ESISOVAL_SNPAMASK:
			if (snpamask) /* duplicate */
				goto bad;
			snpamask = (struct iso_addr *)(buf + 1);
			break;

		case ESISOVAL_NETMASK:
			if (netmask) /* duplicate */
				goto bad;
			netmask = (struct iso_addr *)(buf + 1);
			break;

		default:
			printf("Unknown option in ESIS RD (0x%x)\n", buf[-1]);
		}
		ESIS_NEXT_OPTION(buf);
	}

	IFDEBUG(D_ESISINPUT)
		printf("esis_rdinput: rd: ht %d, da %s\n", ht, clnp_iso_addrp(da));
		if (net)
			printf("\t: net %s\n", clnp_iso_addrp(net));
	ENDDEBUG
	/*
	 *	If netl is zero, then redirect is to an ES. We need to add an entry
	 *	to the snpa cache for (destination, better snpa).
	 *	If netl is not zero, then the redirect is to an IS. In this
	 *	case, add an snpa cache entry for (net, better snpa).
	 *
	 *	If the redirect is to an IS, add a route entry towards that
	 *	IS.
	 */
	if (net == 0 || net->isoa_len == 0 || snpamask) {
		/* redirect to an ES */
		snpac_add(shp->snh_ifp, da,
				bsnpa->isoa_genaddr, SNPA_ES, ht, 0);
	} else {
		snpac_add(shp->snh_ifp, net,
				bsnpa->isoa_genaddr, SNPA_IS, ht, 0);
		snpac_addrt(shp->snh_ifp, da, net, netmask);
	}
bad:
	m_freem(m0);
}

/*
 * FUNCTION:		esis_config
 *
 * PURPOSE:			Report configuration
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Called every esis_config_time seconds
 */
esis_config()
{
	register struct ifnet	*ifp;

	timeout(esis_config, (caddr_t)0, hz * esis_config_time);

	/* 
	 *	Report configuration for each interface that 
	 *	- is UP
	 *	- is not loopback
	 *	- has broadcast capabilities
	 *	- has an ISO address
	 */
	
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if ((ifp->if_flags & IFF_UP) &&
			(ifp->if_flags & IFF_BROADCAST) &&
			((ifp->if_flags & IFF_LOOPBACK) == 0)) {
			/* search for an ISO address family */
			struct ifaddr	*ia;

			for (ia = ifp->if_addrlist; ia; ia = ia->ifa_next) {
				if (ia->ifa_addr->sa_family == AF_ISO) {
					esis_shoutput(ifp, 
						iso_systype & SNPA_ES ? ESIS_ESH : ESIS_ISH,
						esis_holding_time,
						(caddr_t)(iso_systype & SNPA_ES ? all_is_snpa : 
						all_es_snpa), 6, (struct iso_addr *)0);
					break;
				}
			}
		}
	}
}

/*
 * FUNCTION:		esis_shoutput
 *
 * PURPOSE:			Transmit an esh or ish pdu
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
esis_shoutput(ifp, type, ht, sn_addr, sn_len, isoa)
struct ifnet	*ifp;
int				type;
short			ht;
caddr_t 		sn_addr;
int				sn_len;
struct	iso_addr *isoa;
{
	struct mbuf			*m, *m0;
	caddr_t				cp, naddrp;
	int					naddr = 0;
	struct esis_fixed	*pdu;
	struct iso_ifaddr	*ia;
	int					len;
	struct sockaddr_iso	siso;

	if (type == ESIS_ESH)
		esis_stat.es_eshsent++;
	else if (type == ESIS_ISH) 
		esis_stat.es_ishsent++;
	else {
		printf("esis_shoutput: bad pdu type\n");
		return;
	}

	IFDEBUG(D_ESISOUTPUT)
		int	i;
		printf("esis_shoutput: ifp x%x (%s%d), %s, ht %d, to: [%d] ",
			ifp, ifp->if_name, ifp->if_unit, type == ESIS_ESH ? "esh" : "ish",
			ht, sn_len);
		for (i=0; i<sn_len; i++)
			printf("%x%c", *(sn_addr+i), i < (sn_len-1) ? ':' : ' ');
		printf("\n");
	ENDDEBUG

	if ((m0 = m = m_gethdr(M_DONTWAIT, MT_HEADER)) == NULL) {
		esis_stat.es_nomem++;
		return;
	}
	bzero(mtod(m, caddr_t), MHLEN);

	pdu = mtod(m, struct esis_fixed *);
	naddrp = cp = (caddr_t)(pdu + 1);
	len = sizeof(struct esis_fixed);

	/*
	 *	Build fixed part of header
	 */
	pdu->esis_proto_id = ISO9542_ESIS;
	pdu->esis_vers = ESIS_VERSION;
	pdu->esis_type = type;
	HTOC(pdu->esis_ht_msb, pdu->esis_ht_lsb, ht);

	if (type == ESIS_ESH) {
		cp++;
		len++;
	}

	m->m_len = len;
	if (isoa) {
		/*
		 * Here we are responding to a clnp packet sent to an NSAP
		 * that is ours which was sent to the MAC addr all_es's.
		 * It is possible that we did not specifically advertise this
		 * NSAP, even though it is ours, so we will respond
		 * directly to the sender that we are here.  If we do have
		 * multiple NSEL's we'll tack them on so he can compress them out.
		 */
		(void) esis_insert_addr(&cp, &len, isoa, m, 0);
		naddr = 1;
	}
	for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
		int nsellen = (type == ESIS_ISH ? ia->ia_addr.siso_tlen : 0); 
		int n = ia->ia_addr.siso_nlen;
		register struct iso_ifaddr *ia2;

		if (type == ESIS_ISH && naddr > 0)
			break;
		for (ia2 = iso_ifaddr; ia2 != ia; ia2 = ia2->ia_next)
			if (Bcmp(ia->ia_addr.siso_data, ia2->ia_addr.siso_data, n) == 0)
					break;
		if (ia2 != ia)
			continue;	/* Means we have previously copied this nsap */
		if (isoa && Bcmp(ia->ia_addr.siso_data, isoa->isoa_genaddr, n) == 0) {
			isoa = 0;
			continue;	/* Ditto */
		}
		IFDEBUG(D_ESISOUTPUT)
			printf("esis_shoutput: adding NSAP %s\n", 
				clnp_iso_addrp(&ia->ia_addr.siso_addr));
		ENDDEBUG
		if (!esis_insert_addr(&cp, &len,
							  &ia->ia_addr.siso_addr, m, nsellen)) {
			EXTEND_PACKET(m, m0, cp);
			(void) esis_insert_addr(&cp, &len, &ia->ia_addr.siso_addr, m,
									nsellen);
		}
		naddr++;
	}

	if (type == ESIS_ESH)
		*naddrp = naddr;

	m0->m_pkthdr.len = len;
	pdu->esis_hdr_len = len;
	iso_gen_csum(m0, ESIS_CKSUM_OFF, (int)pdu->esis_hdr_len);

	bzero((caddr_t)&siso, sizeof(siso));
	siso.siso_family = AF_ISO;
	siso.siso_data[0] = AFI_SNA;
	siso.siso_nlen = sn_len + 1;
	siso.siso_len  = sn_len + 6;
	bcopy(sn_addr, siso.siso_data + 1, (unsigned)sn_len);
	(ifp->if_output)(ifp, m0, &siso, 0);
}

/*
 * FUNCTION:		esis_ctlinput
 *
 * PURPOSE:			Handle the PRC_IFDOWN transition
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Calls snpac_flush for interface specified.
 *					The loop through iso_ifaddr is stupid because
 *					back in if_down, we knew the ifp...
 */
esis_ctlinput(req, siso)
int						req;		/* request: we handle only PRC_IFDOWN */
struct sockaddr_iso		*siso;		/* address of ifp */
{
	register struct iso_ifaddr *ia;	/* scan through interface addresses */

	if (req == PRC_IFDOWN)
		for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
			if (iso_addrmatch(IA_SIS(ia), siso))
				snpac_flushifp(ia->ia_ifp);
		}
}

#endif	ISO
