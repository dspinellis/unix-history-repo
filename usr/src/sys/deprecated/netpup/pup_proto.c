/*	pup_proto.c	6.3	84/08/29	*/

#include "param.h"
#include "socket.h"
#include "protosw.h"
#include "domain.h"

/*
 * PUP-I protocol family: raw interface
 */
int	rpup_output();
extern	int raw_usrreq();
extern	struct domain pupdomain;		/* or at least forward */

struct protosw pupsw[] = {
{ SOCK_RAW,	&pupdomain,	0,		PR_ATOMIC|PR_ADDR,
  0,		rpup_output,	0,		0,
  raw_usrreq,
  0,		0,		0,		0,
},
};

struct domain pupdomain =
    { AF_PUP, "pup", 0, 0, 0,
      pupsw, &pupsw[sizeof(pupsw)/sizeof(pupsw[0])] };

#ifdef notdef
/*
 * 3 Mb/s Ethernet link protocol family: raw interface
 */
int	raw_enoutput();
extern	int raw_usrreq();

struct protosw ensw[] = {
{ SOCK_RAW,	&endomain,	0,		PR_ATOMIC|PR_ADDR,
  0,		raw_enoutput,	0,		0,
  raw_usrreq,
  0,		0,		0,		0,
},
};

struct domain endomain =
    { AF_ETHERLINK "ether", 0, 0, 0,
      ensw, &ensw[sizeof(ensw)/sizeof(ensw[0])] };
#endif
