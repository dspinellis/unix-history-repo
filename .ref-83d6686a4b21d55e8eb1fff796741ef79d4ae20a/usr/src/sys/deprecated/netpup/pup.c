/*	pup.c	6.2	84/08/29	*/

#include "param.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "../net/af.h"
#include "../netpup/pup.h"

#ifdef PUP
pup_hash(spup, hp)
	struct sockaddr_pup *spup;
	struct afhash *hp;
{

	hp->afh_nethash = spup->spup_net;
	hp->afh_hosthash = spup->spup_host;
}

pup_netmatch(spup1, spup2)
	struct sockaddr_pup *spup1, *spup2;
{

	return (spup1->spup_net == spup2->spup_net);
}
#endif
