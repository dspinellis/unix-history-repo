/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)raw_cb.h	7.7 (Berkeley) %G%
 */

/*
 * Raw protocol interface control block.  Used
 * to tie a socket to the generic raw interface.
 */
struct rawcb {
	struct	rawcb *rcb_next;	/* doubly linked list */
	struct	rawcb *rcb_prev;
	struct	socket *rcb_socket;	/* back pointer to socket */
	struct	sockaddr *rcb_faddr;	/* destination address */
	struct	sockaddr *rcb_laddr;	/* socket's address */
	struct	sockproto rcb_proto;	/* protocol family, protocol */
};

#define	sotorawcb(so)		((struct rawcb *)(so)->so_pcb)

/*
 * Nominal space allocated to a raw socket.
 */
#define	RAWSNDQ		8192
#define	RAWRCVQ		8192

#ifdef KERNEL
struct rawcb rawcb;			/* head of list */

int	 raw_attach __P((struct socket *, int));
void	 raw_ctlinput __P((int, struct sockaddr *));
void	 raw_detach __P((struct rawcb *));
void	 raw_disconnect __P((struct rawcb *));
void	 raw_init __P((void));
void	 raw_input __P((struct mbuf *,
	    struct sockproto *, struct sockaddr *, struct sockaddr *));
int	 raw_usrreq __P((struct socket *,
	    int, struct mbuf *, struct mbuf *, struct mbuf *));
#endif
