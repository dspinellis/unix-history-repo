
/*
 * send an IP datagram.  Used when don't have any Internet protocol
 * control block associated with the action.  Dummies one up.
 */

#define NOPCB_IPSEND(mp, len, asis, retval)     \
	{ struct inpcb inp;                     \
	bzero ((caddr_t) &inp, sizeof(inp));	\
	retval = ip_send (&inp, mp, len, asis); \
	}
