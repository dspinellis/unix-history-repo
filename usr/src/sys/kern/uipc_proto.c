#include "../h/param.h"
#include "../inet/protocol.h"
#include "../inet/protocolsw.h"
/* should include a header file giving desired protocols */
#define	NTCP	1

/*
 * Protocol configuration table and routines to search it.
 */

/*
 * Generic (local or not yet specified) protocol.
 */
int	gen_usrreq();

#if NTCP > 0
/*
 * TCP/IP protocol family: IP, ICMP, UDP, TCP.
 */
int	ip_input(),ip_output(),ip_advise(),ip_slowtimo(),ip_drain();
int	icmp_input(),icmp_output();
int	udp_input(),udp_advise(),udp_usrreq(),udp_sense();
int	tcp_input(),tcp_advise(),tcp_fasttimo(),tcp_slowtimo(),
	    tcp_drain(),tcp_sense();
int	rawip_input(),rawip_advise(),rawip_advise(),rawip_usrreq(),
	    rawip_sense();
#endif

#if NPUP > 0
/*
 * Pup protocols: PUP, BSP.
 */
int	pup1_input(),pup1_output(),pup1_advise(),pup1_slowtimo(),
	    pup1_drain(),pup1_usrreq(),pup1_sense();
int	bsp_input(),bsp_advise(),bsp_fasttimo(),bsp_slowtimo(),
	    bsp_drain(),bsp_usrreq(),bsp_sense();
int	rawpup_input(),rawpup_usrreq(),rawpup_sense();
#endif

#if NCHAOS > 0
/*
 * Chaosnet protocols.
 */
/* ... */
#endif

#if NOISCP > 0
/*
 * Office information system communcation protocols.
 */
/* ... */
#endif

#if NNBS > 0
/*
 * NBS protocols.
 */
/* ... */
#endif

#if NECMA > 0
/*
 * ECMA protocols.
 */
/* ... */
#endif

#if NDATAKIT > 0
/*
 * Datakit protocols.
 */
/* ... */
#endif

struct protosw protosw[] = {
{ SOCK_STREAM,	PF_GENERIC,	0,		0,
  0,		0,		0,
  0,		0,		0,		gen_usrreq,	0,
  0 },
{ SOCK_DGRAM,	PF_GENERIC,	0,		PR_ATOMIC|PR_PROVIDEADDR,
  0,		0,		0,
  0,		0,		0,		gen_usrreq,	0,
  0 },
{ SOCK_RDM,	PF_GENERIC,	0,		PR_ATOMIC|PR_PROVIDEADDR,
  0,		0,		0,
  0,		0,		0,		gen_usrreq,	0,
  0 },
{ SOCK_RAW,	PF_GENERIC,	0,		PR_ATOMIC|PR_PROVIDEADDR,
  0,		0,		0,
  0,		0,		0,		gen_usrreq,	0,
#if NTCP > 0
{ 0,		0,		0,		0,
  ip_input,	ip_output,	0,
  0,		ip_slowtimo,	ip_drain,	0,		0,
  0 },
{ 0,		0,		IPPROTO_ICMP,	0,
  icmp_input,	icmp_output,	0,
  0,		0,		icmp_drain,	0,		0,
  0 },
{ SOCK_DGRAM,	PF_INET,	IPPROTO_UDP,	PR_ATOMIC|PR_PROVIDEADDR,
  udp_input,	0,		udp_advise,
  0,		0,		0,		udp_usrreq,	udp_sense,
  MLEN },
{ SOCK_STREAM,	PF_INET,	IPPROTO_TCP,	0,
  tcp_input,	0,		tcp_advise,
  tcp_fasttimo,	tcp_slowtimo,	tcp_drain,	tcp_usrreq,	tcp_sense,
  MLEN },
{ SOCK_RAW,	PF_INET,	IPPROTO_RAW,	PR_ATOMIC|PR_PROVIDEADDR,
  ri_input,	0,		ri_advise,
  ri_fasttimo,	ri_slowtimo,	ri_drain,	ri_usrreq,	ri_sense,
  MLEN },
#endif
#if NPUP > 0
{ SOCK_DGRAM,	PF_PUP,		0,		PR_ATOMIC|PR_PROVIDEADDR,
  pup_input,	pup_output,	pup_advise,
  0,		pup_slowtimo,	pup_drain,	pup_usrreq,	pup_sense,
  MLEN },
{ SOCK_STREAM,	PF_PUP1,	PUPPROTO_BSP,	0,
  bsp_input,	0,		bsp_advise,
  bsp_fasttimo,	bsp_slowtimo,	bsp_drain,	bsp_usrreq,	bsp_sense,
  MLEN },
{ SOCK_RAW,	PF_PUP1,	PUPPROTO_RAW,	PR_ATOMIC|PR_PROVIDEADDR,
  rp_input,	0,		rp_advise,
  rp_fasttimo,	rp_slowtimo,	rp_drain,	rp_usrreq,	rp_sense,
  MLEN	},
#endif
#if NCHAOS > 0
/* ... */
#endif
#if NOISCP > 0
/* ... */
#endif
#if NNBS > 0
/* ... */
#endif
#if NECMA > 0
/* ... */
#endif
#if NDATAKIT > 0
/* ... */
#endif
};
#define	protoswEND	&protosw[sizeof (protosw)/sizeof (protosw[0])]

/*
 * Operations on protocol table and protocol families.
 */

/*
 * Find a standard protocol in a protocol family
 * of a specific type.
 */
pf_stdproto(family, type)
	int family, type;
{
	register struct protosw *pr;

	if (family == 0)
		return (0);
	for (pr = protosw; pr < protoswEND; pr++)
		if (pr->pr_family == family && pr->pr_type == type)
			return (pr);
	return (0);
}

/*
 * Find a specified protocol in a specified protocol family.
 */
pf_findproto(family, proto)
	int family, proto;
{
	register struct protosw *pr;

	if (family == 0)
		return (0);
	for (pr = protosw; pr < protoswEND; pr++)
		if (pr->pr_family == family && pr->pr_proto == proto)
			return (pr);
	return (0);
}
