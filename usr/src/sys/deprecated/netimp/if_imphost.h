/*	if_imphost.h	4.3	82/02/16	*/

/*
 * Host structure used with IMP's.
 * Used to hold outgoing packets which
 * would exceed allowed RFNM count.
 *
 * These structures are packed into
 * mbuf's and kept as small as possible.
 */
struct host {
	struct	mbuf *h_q;		/* holding queue */
	struct	in_addr h_addr;		/* host's address */
	u_short	h_status;          	/* host status */
	u_char	h_rfnm;			/* # outstanding rfnm's */
	u_char	h_refcnt;		/* reference count */
};

#define	HOSTS_DOWN	0		/* host believed down */
#define HOSTS_UP	128 		/* host up */

/*
 * Host structures, as seen inside an mbuf.
 * Hashing on the host address is used to
 * select an index into the first mbuf.  Collisions
 * are then resolved by searching successive
 * mbuf's at the same index.  Reclamation is done
 * automatically at the time a structure is free'd.
 */
#define	HPMBUF	((MLEN - sizeof(int)) / sizeof(struct host))
#define	HOSTHASH(a)	(((a).s_addr&~0x80000000) % HPMBUF)

struct hmbuf {
	int	hm_count;		/* # of struct's in use */
	struct	host hm_hosts[HPMBUF];	/* data structures proper */
};

#ifdef KERNEL
struct host *hostlookup();
struct host *hostenter();
#endif
