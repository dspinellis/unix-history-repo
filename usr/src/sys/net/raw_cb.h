/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)raw_cb.h	7.2 (Berkeley) %G%
 */

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
	struct	sockproto rcb_proto;	/* protocol family, protocol */
	caddr_t	rcb_pcb;		/* protocol specific stuff */
	struct	mbuf *rcb_options;	/* protocol specific options */
	struct	route rcb_route;	/* routing information */
	short	rcb_flags;
};

/*
 * Since we can't interpret canonical addresses,
 * we mark an address present in the flags field.
 */
#define	RAW_LADDR	01
#define	RAW_FADDR	02
#define	RAW_DONTROUTE	04		/* no routing, default */

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
