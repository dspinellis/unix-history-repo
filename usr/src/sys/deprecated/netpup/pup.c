/*	pup.c	4.1	82/06/13	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/af.h"
#include "../net/pup.h"

#ifdef PUP
pup_hash(spup, hp)
	struct sockaddr_pup *spup;
	struct afhash *hp;
{
COUNT(PUP_HASH);
	hp->afh_nethash = spup->spup_addr.pp_net;
	hp->afh_hosthash = spup->spup_addr.pp_host;
	if (hp->afh_hosthash < 0)
		hp->afh_hosthash = -hp->afh_hosthash;
}

pup_netmatch(spup1, spup2)
	struct sockaddr_pup *spup1, *spup2;
{
COUNT(PUP_NETMATCH);
	return (spup1->spup_addr.pp_net == spup2->spup_addr.pp_net);
}
#endif
