/*	if_ether.c	6.2	83/08/28	*/

/*
 * Ethernet address resolution protocol.
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../netinet/in.h"
#include "../netinet/if_ether.h"


/*
 * Internet to ethernet address resolution table.
 */
struct	arptab {
	struct	in_addr at_iaddr;	/* internet address */
	u_char	at_enaddr[6];		/* ethernet address */
	struct	mbuf *at_hold;		/* last packet until resolved/timeout */
	u_char	at_timer;		/* minutes since last reference */
	u_char	at_flags;		/* flags */
};
/* at_flags field values */
#define	ATF_INUSE	1		/* entry in use */
#define ATF_COM		2		/* completed entry (enaddr valid) */

#define	ARPTAB_BSIZ	5		/* bucket size */
#define	ARPTAB_NB	19		/* number of buckets */
#define	ARPTAB_SIZE	(ARPTAB_BSIZ * ARPTAB_NB)
struct	arptab arptab[ARPTAB_SIZE];

#define	ARPTAB_HASH(a) \
	((short)((((a) >> 16) ^ (a)) & 0x7fff) % ARPTAB_NB)

#define	ARPTAB_LOOK(at,addr) { \
	register n; \
	at = &arptab[ARPTAB_HASH(addr) * ARPTAB_BSIZ]; \
	for (n = 0 ; n < ARPTAB_BSIZ ; n++,at++) \
		if (at->at_iaddr.s_addr == addr) \
			break; \
	if (n >= ARPTAB_BSIZ) \
		at = 0; }

struct	arpcom *arpcom;		/* chain of active ether interfaces */
int	arpt_age;		/* aging timer */

/* timer values */
#define	ARPT_AGE	(60*1)	/* aging timer, 1 min. */
#define	ARPT_KILLC	20	/* kill completed entry in 20 mins. */
#define	ARPT_KILLI	3	/* kill incomplete entry in 3 minutes */

u_char	etherbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
extern struct ifnet loif;

/*
 * Local addresses in the range oldmap to infinity are
 * mapped according to the old mapping scheme.  That is,
 * mapping of Internet to Ethernet addresses is performed
 * by taking the high three bytes of the network interface's
 * address and the low three bytes of the local address part.
 * This only allows boards from the same manufacturer to
 * communicate unless the on-board address is overridden
 * (not possible in many manufacture's hardware).
 *
 * NB: setting oldmap to zero completely disables ARP
 *     (i.e. identical to setting IFF_NOARP with an ioctl).
 */
int	oldmap = 1024;

/*
 * Attach an ethernet interface to the list "arpcom" where
 * arptimer() can find it.  If first time 
 * initialization, start arptimer().
 */
arpattach(ac)
	register struct arpcom *ac;
{
	register struct arpcom *acp;

	for (acp = arpcom; acp != (struct arpcom *)0; acp = acp->ac_ac)
		if (acp == ac)		/* if already on list */
			return;
	ac->ac_ac = arpcom;
	arpcom = ac;
	if (arpcom->ac_ac == 0)		/* very first time */
		arptimer();
}

/*
 * Timeout routine.  Age arp_tab entries once a minute.
 */
arptimer()
{
	register struct arptab *at;
	register i;

	timeout(arptimer, (caddr_t)0, hz);
#ifdef notdef
	if (++arpt_sanity > ARPT_SANITY) {
		register struct arpcom *ac;

		/*
		 * Randomize sanity timer based on my host address.
		 * Ask who has my own address;  if someone else replies,
		 * then they are impersonating me.
		 */
		arpt_sanity = arpcom->ac_enaddr[5] & 0x3f;
		for (ac = arpcom; ac != (struct arpcom *)-1; ac = ac->ac_ac)
			arpwhohas(ac, &((struct sockaddr_in *)
			    &ac->ac_if.if_addr)->sin_addr);
	}
#endif
	if (++arpt_age > ARPT_AGE) {
		arpt_age = 0;
		at = &arptab[0];
		for (i = 0; i < ARPTAB_SIZE; i++, at++) {
			if (at->at_flags == 0)
				continue;
			if (++at->at_timer < ((at->at_flags&ATF_COM) ?
			    ARPT_KILLC : ARPT_KILLI))
				continue;
			/* timer has expired, clear entry */
			arptfree(at);
		}
	}
}

/*
 * Broadcast an ARP packet, asking who has addr on interface ac.
 */
arpwhohas(ac, addr)
	register struct arpcom *ac;
	struct in_addr *addr;
{
	register struct mbuf *m;
	register struct ether_header *eh;
	register struct ether_arp *ea;
	struct sockaddr sa;

	if ((m = m_get(M_DONTWAIT, MT_DATA)) == NULL)
		return;
	m->m_len = sizeof *ea + sizeof *eh;
	m->m_off = MMAXOFF - m->m_len;
	ea = mtod(m, struct ether_arp *);
	eh = (struct ether_header *)sa.sa_data;
	bzero((caddr_t)ea, sizeof (*ea));
	bcopy((caddr_t)etherbroadcastaddr, (caddr_t)eh->ether_dhost,
	   sizeof (etherbroadcastaddr));
	eh->ether_type = ETHERPUP_ARPTYPE;	/* if_output will swap */
	ea->arp_hrd = htons(ARPHRD_ETHER);
	ea->arp_pro = htons(ETHERPUP_IPTYPE);
	ea->arp_hln = sizeof ea->arp_sha;	/* hardware address length */
	ea->arp_pln = sizeof ea->arp_spa;	/* protocol address length */
	ea->arp_op = htons(ARPOP_REQUEST);
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)&((struct sockaddr_in *)&ac->ac_if.if_addr)->sin_addr,
	   (caddr_t)ea->arp_spa, sizeof (ea->arp_spa));
	bcopy((caddr_t)addr, (caddr_t)ea->arp_tpa, sizeof (ea->arp_tpa));
	sa.sa_family = AF_UNSPEC;
	(void) (*ac->ac_if.if_output)(&ac->ac_if, m, &sa);
}

/*
 * Resolve an IP address into an ethernet address.  If success, 
 * desten is filled in and 1 is returned.  If there is no entry
 * in arptab, set one up and broadcast a request 
 * for the IP address;  return 0.  Hold onto this mbuf and 
 * resend it once the address is finally resolved.
 *
 * We do some (conservative) locking here at splimp, since
 * arptab is also altered from input interrupt service (ecintr/ilintr
 * calls arpinput when ETHERPUP_ARPTYPE packets come in).
 */
arpresolve(ac, m, destip, desten)
	register struct arpcom *ac;
	struct mbuf *m;
	register struct in_addr *destip;
	register u_char *desten;
{
	register struct arptab *at;
	register struct ifnet *ifp;
	struct sockaddr_in sin;
	int s, lna;

	lna = in_lnaof(*destip);
	if (lna == INADDR_ANY) {	/* broadcast address */
		bcopy((caddr_t)etherbroadcastaddr, (caddr_t)desten,
		   sizeof (etherbroadcastaddr));
		return (1);
	}
	ifp = &ac->ac_if;
	/* if for us, then use software loopback driver */
	if (destip->s_addr ==
	    ((struct sockaddr_in *)&ifp->if_addr)-> sin_addr.s_addr) {
		sin.sin_family = AF_INET;
		sin.sin_addr = *destip;
		return (looutput(&loif, m, (struct sockaddr *)&sin));
	}
	if ((ifp->if_flags & IFF_NOARP) || lna >= oldmap) {
		bcopy((caddr_t)ac->ac_enaddr, (caddr_t)desten, 3);
		desten[3] = (lna >> 16) & 0x7f;
		desten[4] = (lna >> 8) & 0xff;
		desten[5] = lna & 0xff;
		return (1);
	}
	s = splimp();
	ARPTAB_LOOK(at, destip->s_addr);
	if (at == 0) {			/* not found */
		at = arptnew(destip);
		at->at_hold = m;
		arpwhohas(ac, destip);
		splx(s);
		return (0);
	}
	at->at_timer = 0;		/* restart the timer */
	if (at->at_flags & ATF_COM) {	/* entry IS complete */
		bcopy((caddr_t)at->at_enaddr, (caddr_t)desten, 6);
		splx(s);
		return (1);
	}
	/*
	 * There is an arptab entry, but no ethernet address
	 * response yet.  Replace the held mbuf with this
	 * latest one.
	 */
	if (at->at_hold)
		m_freem(at->at_hold);
	at->at_hold = m;
	arpwhohas(ac, destip);		/* ask again */
	splx(s);
	return (0);
}

/*
 * Find my own IP address.  It will either be waiting for us in
 * monitor RAM, or can be obtained via broadcast to the file/boot
 * server (not necessarily using the ARP packet format).
 *
 * Unimplemented at present, return 0 and assume that the host
 * will set his own IP address via the SIOCSIFADDR ioctl.
 */
/*ARGSUSED*/
struct in_addr
arpmyaddr(ac)
	register struct arpcom *ac;
{
	static struct in_addr addr;

#ifdef lint
	ac = ac;
#endif
	addr.s_addr = 0;
	return (addr);
}

/*
 * Called from ecintr/ilintr when ether packet type ETHERPUP_ARP
 * is received.  Algorithm is exactly that given in RFC 826.
 * In addition, a sanity check is performed on the sender
 * protocol address, to catch impersonators.
 */
arpinput(ac, m)
	register struct arpcom *ac;
	struct mbuf *m;
{
	register struct ether_arp *ea;
	struct ether_header *eh;
	register struct arptab *at = 0;  /* same as "merge" flag */
	struct sockaddr_in sin;
	struct sockaddr sa;
	struct mbuf *mhold;
	struct in_addr isaddr,itaddr,myaddr;

	if (m->m_len < sizeof *ea)
		goto out;
	myaddr = ((struct sockaddr_in *)&ac->ac_if.if_addr)->sin_addr;
	ea = mtod(m, struct ether_arp *);
	if (ntohs(ea->arp_pro) != ETHERPUP_IPTYPE)
		goto out;
	isaddr.s_addr = ((struct in_addr *)ea->arp_spa)->s_addr;
	itaddr.s_addr = ((struct in_addr *)ea->arp_tpa)->s_addr;
	if (!bcmp((caddr_t)ea->arp_sha, (caddr_t)ac->ac_enaddr,
	  sizeof (ac->ac_enaddr)))
		goto out;	/* it's from me, ignore it. */
	if (isaddr.s_addr == myaddr.s_addr) {
		printf("duplicate IP address!! sent from ethernet address: ");
		printf("%x %x %x %x %x %x\n", ea->arp_sha[0], ea->arp_sha[1],
		    ea->arp_sha[2], ea->arp_sha[3],
		    ea->arp_sha[4], ea->arp_sha[5]);
		if (ntohs(ea->arp_op) == ARPOP_REQUEST)
			goto reply;
		goto out;
	}
	ARPTAB_LOOK(at, isaddr.s_addr);
	if (at) {
		bcopy((caddr_t)ea->arp_sha, (caddr_t)at->at_enaddr,
		   sizeof (ea->arp_sha));
		at->at_flags |= ATF_COM;
		if (at->at_hold) {
			mhold = at->at_hold;
			at->at_hold = 0;
			sin.sin_family = AF_INET;
			sin.sin_addr = isaddr;
			(*ac->ac_if.if_output)(&ac->ac_if, 
			    mhold, (struct sockaddr *)&sin);
		}
	}
	if (itaddr.s_addr != myaddr.s_addr)
		goto out;	/* if I am not the target */
	if (at == 0) {		/* ensure we have a table entry */
		at = arptnew(&isaddr);
		bcopy((caddr_t)ea->arp_sha, (caddr_t)at->at_enaddr,
		   sizeof (ea->arp_sha));
		at->at_flags |= ATF_COM;
	}
	if (ntohs(ea->arp_op) != ARPOP_REQUEST)
		goto out;
reply:
	bcopy((caddr_t)ea->arp_sha, (caddr_t)ea->arp_tha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)ea->arp_spa, (caddr_t)ea->arp_tpa,
	   sizeof (ea->arp_spa));
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof (ea->arp_sha));
	bcopy((caddr_t)&myaddr, (caddr_t)ea->arp_spa,
	   sizeof (ea->arp_spa));
	ea->arp_op = htons(ARPOP_REPLY);
	eh = (struct ether_header *)sa.sa_data;
	bcopy((caddr_t)ea->arp_tha, (caddr_t)eh->ether_dhost,
	   sizeof (eh->ether_dhost));
	eh->ether_type = ETHERPUP_ARPTYPE;
	sa.sa_family = AF_UNSPEC;
	(*ac->ac_if.if_output)(&ac->ac_if, m, &sa);
	return;
out:
	m_freem(m);
	return;
}

/*
 * Free an arptab entry.
 */
arptfree(at)
	register struct arptab *at;
{
	int s = splimp();

	if (at->at_hold)
		m_freem(at->at_hold);
	at->at_hold = 0;
	at->at_timer = at->at_flags = 0;
	at->at_iaddr.s_addr = 0;
	splx(s);
}

/*
 * Enter a new address in arptab, pushing out the oldest entry 
 * from the bucket if there is no room.
 */
struct arptab *
arptnew(addr)
	struct in_addr *addr;
{
	register n;
	int oldest = 0;
	register struct arptab *at, *ato;

	ato = at = &arptab[ARPTAB_HASH(addr->s_addr) * ARPTAB_BSIZ];
	for (n = 0 ; n < ARPTAB_BSIZ ; n++,at++) {
		if (at->at_flags == 0)
			goto out;	 /* found an empty entry */
		if (at->at_timer > oldest) {
			oldest = at->at_timer;
			ato = at;
		}
	}
	at = ato;
	arptfree(at);
out:
	at->at_iaddr = *addr;
	at->at_flags = ATF_INUSE;
	return (at);
}
