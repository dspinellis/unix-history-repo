/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *      @(#)ns_pcb.h	7.2 (Berkeley) %G%
 */

/*
 * Ns protocol interface control block.
 */
struct nspcb {
	struct	nspcb *nsp_next;	/* doubly linked list */
	struct	nspcb *nsp_prev;
	struct	nspcb *nsp_head;
	struct	socket *nsp_socket;	/* back pointer to socket */
	struct	ns_addr nsp_faddr;	/* destination address */
	struct	ns_addr nsp_laddr;	/* socket's address */
	caddr_t	nsp_pcb;		/* protocol specific stuff */
	struct	route nsp_route;	/* routing information */
	struct	ns_addr nsp_lastdst;	/* validate cached route for dg socks*/
	long	nsp_notify_param;	/* extra info passed via ns_pcbnotify*/
	short	nsp_flags;
	u_char	nsp_dpt;		/* default packet type for idp_output*/
	u_char	nsp_rpt;		/* last received packet type by
								idp_input() */
};

/* possible flags */

#define NSP_IN_ABORT	0x1		/* calling abort through socket */
#define NSP_RAWIN	0x2		/* show headers on input */
#define NSP_RAWOUT	0x4		/* show header on output */
#define NSP_ALL_PACKETS	0x8		/* Turn off higher proto processing */

#define	NS_WILDCARD	1

#define nsp_lport nsp_laddr.x_port
#define nsp_fport nsp_faddr.x_port

#define	sotonspcb(so)		((struct nspcb *)((so)->so_pcb))

/*
 * Nominal space allocated to a ns socket.
 */
#define	NSSNDQ		2048
#define	NSRCVQ		2048


#ifdef KERNEL
struct	nspcb nspcb;			/* head of list */
struct	nspcb *ns_pcblookup();
#endif
