
#include "../h/param.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/domain.h"
#include "../h/mbuf.h"

#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_pcb.h"

/*
 * Internet protocols
 */

int	 raw_init(), raw_usrreq();

/* IP */
int	ip_init(),ip_timeo(),ip_drain();
int	ip_send(), ip_ctloutput();
int	raw_ip_input(), raw_ip_output();

/* ICMP */
int	icmp(), ic_timeo();
int	raw_icmp_output();

/* UDP */
int	udp_input();
int	udp_usrreq(), udp_init(), udp_ctlinput();
int	raw_udp_output();

/* TCP */
int	tcp_input();
int	tcp_usrreq(), tcp_ctlinput(), tcp_ctloutput();
int	tcp_init(), tcp_timeo(), tcp_drain();
int	raw_tcp_output();

/* RDP */
#ifdef RDP
int	rdp_input(), rdp_init(), rdp_timeo(), rdp_usrreq();
int	rdp_ctlinput(), rdp_ctloutput();
int	raw_rdp_output();
#endif

/* HMP */
#ifdef HMP
int	hmp_input(), hmp_init(), hmp_usrreq();
int 	hmp_ctlinput(), hmp_ctloutput();
int	raw_hmp_output();
#endif

/*
 * IMP protocol family: raw interface.
 * Using the raw interface entry to get the timer routine
 * in is a kludge.
 */
#include "imp.h"
#if NIMP > 0
int	rimp_output(), hostslowtimo();
#endif

#ifdef NSIP
int	idpip_input(), rip_output();
#endif

extern	struct domain inetdomain;

struct protosw inetsw[] = {
{ 0,		&inetdomain,	0,		0,
  0,		ip_send,	0,		ip_ctloutput,
  0,
  ip_init,	0,		ip_timeo,	ip_drain
},

/*
 * put frequently used protocols early.  Table is beginning to get large..
 */

/* UDP */
{ SOCK_DGRAM,	&inetdomain,	IPPROTO_UDP,	PR_ATOMIC|PR_ADDR,
  udp_input,	0,		udp_ctlinput,	ip_ctloutput,
  udp_usrreq,
  udp_init,	0,		0,		0
},

/* TCP */
{ SOCK_STREAM,	&inetdomain,	IPPROTO_TCP,	PR_CONNREQUIRED|PR_WANTRCVD,
  tcp_input,	0,		tcp_ctlinput,	tcp_ctloutput,
  tcp_usrreq,
  tcp_init,	0,		tcp_timeo,	tcp_drain
},

/* RDP */
#ifdef RDP
{ SOCK_SEQPACKET, &inetdomain,	IPPROTO_RDP,	PR_CONNREQUIRED|PR_WANTRCVD,
  rdp_input,	0,		rdp_ctlinput,	rdp_ctloutput,
  rdp_usrreq,
  rdp_init,	0,		rdp_timeo,	0
},
#endif

/* HMP */
#ifdef HMP
{ SOCK_DGRAM,	&inetdomain,	IPPROTO_HMP,	PR_ATOMIC|PR_ADDR,
  hmp_input,	0,		hmp_ctlinput,	hmp_ctloutput,
  hmp_usrreq,
  hmp_init,	0,		0,		0
},
#endif

#ifdef NSIP
{ SOCK_RAW,	&inetdomain,	IPPROTO_IDP,	PR_ATOMIC|PR_ADDR,
  idpip_input,	rip_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},
#endif

/*
 * to get all incoming IP packets:
 *	socket(AF_INET,SOCK_RAW,0)
 */
{ SOCK_RAW,	&inetdomain,	0,		PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_ip_output,	0,		0,
  raw_usrreq,
  raw_init,	0,		0,		0
},

/*
 * to get all incoming ICMP packets:
 * 	socket(AF_INET,SOCK_RAW, IPPROTO_ICMP)
 */
{ SOCK_RAW,	&inetdomain,	IPPROTO_ICMP,	PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_icmp_output,0,		0,
  raw_usrreq,
  0,		0,		ic_timeo,	0
},

#ifdef RDP
/*
 * to get all incoming RDP packets:
 *	socket(AF_INET,SOCK_RAW, IPPROTO_RDP)
 */
{ SOCK_RAW,	&inetdomain,	IPPROTO_RDP,	PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_rdp_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},
#endif

/*
 * to get all incoming UDP packets:
 *	socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
 */
{ SOCK_RAW,	&inetdomain,	IPPROTO_UDP,	PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_udp_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},

#ifdef HMP
/*
 * to get all incoming HMP packets:
 *	socket(AF_INET,SOCK_RAW, IPPROTO_HMP);
 */
{ SOCK_RAW,	&inetdomain,	IPPROTO_HMP,	PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_hmp_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},
#endif

/*
 * to get all TCP packets:
 *	socket(AF_INET, SOCK_RAW, IPPROTO_TCP);
 */
{ SOCK_RAW,	&inetdomain,	IPPROTO_TCP,	PR_ATOMIC|PR_ADDR,
  raw_ip_input,	raw_tcp_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},
};

struct domain inetdomain =
    { AF_INET, "internet", 0, 0, 0, 
      inetsw, &inetsw[sizeof(inetsw)/sizeof(inetsw[0])] };

#if NIMP > 0
extern	struct domain impdomain;

struct protosw impsw[] = {
{ SOCK_RAW,	&impdomain,	0,		PR_ATOMIC|PR_ADDR,
  0,		rimp_output,	0,		0,
  raw_usrreq,
  0,		0,		hostslowtimo,	0
},
};

struct domain impdomain =
    { AF_IMPLINK, "imp", 0, 0, 0,
      impsw, &impsw[sizeof (impsw)/sizeof(impsw[0])] };
#endif

#include "hy.h"
#if NHY > 0
/*
 * HYPERchannel protocol family: raw interface.
 */
int	rhy_output();

struct protosw hysw[] = {
{ SOCK_RAW,	PF_HYLINK,	0,		PR_ATOMIC|PR_ADDR,
  0,		rhy_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0
},
};

struct domain hydomain =
    { AF_HYLINK, "hy", hysw, &hysw[sizeof (hysw)/sizeof(hysw[0])] };
#endif

#define N_ELEMENTS(x) (sizeof(x)/sizeof((x)[0]))

#ifdef AF_RDPDEBUG
struct protosw rdpdebugsw[] = {
	{ SOCK_RAW, PF_RDPDEBUG, 0, PR_ATOMIC|PR_ADDR,
	  0,0,0,0,
	  raw_usrreq,
	  0,0,0,0}
};
struct domain rdpdebugdomain = 
{ AF_RDPDEBUG, "rdpdebug", rdpdebugsw, &rdpdebugsw[N_ELEMENTS(rdpdebugsw)] };
#endif

#ifdef AF_TCPDEBUG
struct protosw tcpdebugsw[] = {
	{ SOCK_RAW, PF_TCPDEBUG, 0, PR_ATOMIC|PR_ADDR,
	  0,0,0,0,
	  raw_usrreq,
	  0,0,0,0}
};
struct domain tcpdebugdomain = 
{ AF_TCPDEBUG, "tcpdebug", tcpdebugsw, &tcpdebugsw[N_ELEMENTS(tcpdebugsw)] };
#endif
