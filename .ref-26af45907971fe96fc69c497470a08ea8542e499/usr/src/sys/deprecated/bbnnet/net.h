#define RCSNETHDR	"$Header: net.h,v 1.14 85/07/31 09:32:54 walsh Exp $"

/*
 * dummy protocol number(s) for raw input.  Not real like UDPROTO/TCPROTO/...
 */
#define NETLOG_PROTO	62


#define TRUE  1
#define FALSE 0

/*
 * Software interrupt version
 *
 * This version of the network code runs off software interrupts rather than
 * as a separate process.  The software interrupt level for the network is
 * higher than the software level for the clock (so you can enter the network
 * in routines called from timeouts).  User process routines switch into
 * network code by doing splnet().
 */

/* splnet is defined in ../sys/asm.sed */
#define setsoftnet()	mtpr(SIRR, 12)

#ifndef LOCORE

typedef unsigned short n_short; /* short as received from the net */
typedef u_long n_long;   	/* long as received from the net */
typedef u_long sequence; 	/* sequence numbers in tcp */
typedef	u_long net_t;		/* network number */

/*
 * This structure describes the work that the finite state machine
 * procedure is supposed to do.  At one time, these were chained
 * together and batch processing performed on them.  Code would need
 * reworking to go back to that method since work entry has pointer
 * to tcpcb, which may have been closed and freed.
 */
struct work {
	short w_type;                   /* what to do with entry */
	short w_stype;                  /* subtype for timer names */
	struct tcpcb *w_tcb;            /* -> tcb for entry */
	char *w_dat;                    /* -> work data chain */
};

/*
 * No need to spl when enter network via action, since network is only
 * accessed via softare interrupt [splnet], tcp_usrreq(), and tcp_timeo()
 * which splnet().
 */
#define w_alloc(type, stype, tp, m)					\
{									\
	struct work w;							\
									\
	w.w_type = type; w.w_stype = stype; w.w_tcb = tp; 		\
	w.w_dat = (char *)m;						\
	(void) action(&w);						\
}

#define NETHASH(x)  ((u_long) iptonet(x))
#define HOSTHASH(x) ((u_long) htonl((u_long) (x)))

/* make an ip addr */
#define ipaddr(w,x,y,z) ((long)(((z)<<24)|((y)<<16)|((x)<<8)|(w)))


struct dfilter {
	struct in_addr	foreign_host;
	struct in_addr	local_host;
	u_short		foreign_port;
	u_short		local_port;
	int		matches;
};

#ifdef KERNEL
u_long iptime();

extern struct dfilter tcp_dfilter;

extern struct in_addr icmp_addr();
extern struct in_addr redir_addr();

#endif KERNEL

/* replace with dynamic estimate of remote send buffering */
#define MINTCPBUF	256
#endif LOCORE
