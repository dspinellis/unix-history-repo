/*	raw_cb.h	4.4	82/04/10	*/

/*
 * Raw protocol interface control block.  Used
 * to tie a socket to the generic raw interface.
 */
struct rawcb {
	struct	rawcb *rcb_next;	/* doubly linked list */
	struct	rawcb *rcb_prev;
	struct	socket *rcb_socket;	/* back pointer to socket */
	struct	sockaddr rcb_faddr;	/* destination address */
	struct	sockaddr rcb_laddr;	/* socket's address */
	caddr_t	rcb_pcb;		/* protocol specific stuff */
	short	rcb_flags;
};

/*
 * Since we can't interpret canonical addresses,
 * we mark an address present in the flags field.
 */
#define	RAW_LADDR	01
#define	RAW_FADDR	02

#define	sotorawcb(so)		((struct rawcb *)(so)->so_pcb)

/*
 * Nominal space allocated to a raw socket.
 */
#define	RAWSNDQ		2048
#define	RAWRCVQ		2048

/*
 * Format of raw interface header prepended by
 * raw_input after call from protocol specific
 * input routine.
 */
struct raw_header {
	struct	sockproto raw_proto;	/* format of packet */
	struct	sockaddr raw_dst;	/* dst address for rawintr */
	struct	sockaddr raw_src;	/* src address for sbappendaddr */
};

#ifdef KERNEL
struct rawcb rawcb;			/* head of list */
#endif
