#define RCSINPCBHDR "$Header: in_pcb.h,v 1.7 85/07/31 09:31:17 walsh Exp $"

/*
 * Common structure pcb for internet protocol implementation.
 * Here are stored pointers to local and foreign host table
 * entries, local and foreign socket numbers, and pointers
 * up (to a socket structure) and down (to a protocol-specific)
 * control block.
 */
#define MAX_IPOPTLEN	60

struct inpcb {
	struct	inpcb *inp_next,*inp_prev;
					/* pointers to other pcb's */
	struct	in_addr inp_faddr;	/* foreign host table entry */
	struct	in_addr inp_laddr;	/* local host table entry */
	u_short		inp_fport;	/* foreign port */
	u_short		inp_lport;	/* local port */
	struct	socket *inp_socket;	/* back pointer to socket */
	caddr_t		inp_ppcb;	/* pointer to per-protocol pcb */
	struct	route	inp_route;	/* routing entry */
	char		inp_optlen;
	char		inp_options[MAX_IPOPTLEN];
};

/*
 * protocol specific structure passed to some pcb controlling routines.
 * new in 4.3 because addition of new protocols required a more generalized
 * in_pcbbind().  rootport <= resvport <= maxport.  non-superuser can
 * bind rootport -> maxport, but kernel will bind resvport -> maxport so
 * that projects can test things without being root and without fearing
 * someone may accidentally take their port.
 */

struct pr_advice {
	u_short rootport;	/* ports reserved exclusively for root */
	u_short resvport;	/* ports reserved from random allocation */
	u_short maxport;	/* absolute max port */
	u_short nowport;	/* port last used, initialize to resvport! */
	u_short portsize;	/* size of port (in bytes) */
	int (*bind_used)();	/* routine called to check before binding */
};

/*
 * protocol specific setsockopt/getsockopt calls.
 */
#define SO_IPROUTE		2

#define	sotoinpcb(so)	((struct inpcb *)(so)->so_pcb)


#ifdef KERNEL

extern struct inpcb *in_pcblookup();

#define uwake(inp)				\
{						\
	sbwakeup(&inp->inp_socket->so_rcv);	\
	sbwakeup(&inp->inp_socket->so_snd);	\
}

#endif
