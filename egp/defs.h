/*	@(#)defs.h	1.3 (Berkeley) 12/13/88 */

/* EGP User Process, ISI 23-Jun-84 */

/* compiler switches */

#define INSTALL 1		/* 0 => debugging - dont install routes in 
					kernel. Reference in main() */

#define SAMENET 0		/* 1 => all egp neighbors must have the same
					shared net in common. Referenced in
					init_egpngh() */

#define ALLOWNONNEIGHBORS 0	/* 1 => allows non-neighbors not on shared net
					to conduct egp for testing,
					referenced in init_egp(), init_egp2(),
					also require SAMENET = 0, and 
					DEFAULTIF to specify source interface
					*/

/* initialization file */

#define EGPINITFILE	"/etc/egp.conf"

/* general definitions for EGP user process */

#define TRUE	 1
#define FALSE	 0
#define ERROR	-1			/* used in rt_mknr() and rt_NRnets()*/
#define NOERROR -2			/* used in egppoll() */

#ifndef NULL
#define NULL	 0
#endif

#define MAXHOSTNAMELENGTH 64		/*used in init_egpngh & rt_dumb_init*/
#define MAXPACKETSIZE 8192

#if ALLOWNONNEIGHBORS
#define DEFAULTIF "10.1.0.52"	/* source interface (in quoted dot notation)
				   to use for non-neighbor EGP peers,
				   host specific, for testing only */
#endif

/* macros to select internet address given pointer to a struct sockaddr */

/* result is u_long */
#define sock_inaddr(x) (((struct sockaddr_in *)(x))->sin_addr.s_addr)

/* result is struct in_addr */
#define in_addr_ofs(x) (((struct sockaddr_in *)(x))->sin_addr)

/* definitions from C-gateway */

#define reg register
#define ext extern

#define	AMSK	0200		/* Mask values used to decide on which */
#define	AVAL	0000		/* class of address we have */
#define	BMSK	0300
#define	BVAL	0200
#define	CMSK	0340		/* The associated macros take an arg */
#define	CVAL	0300		/* of the form in_addr.i_aaddr.i_anet */

#define	in_isa(x)	(((x) & AMSK) == AVAL)
#define	in_isb(x)	(((x) & BMSK) == BVAL)
#define	in_isc(x)	(((x) & CMSK) == CVAL)

/* definitions from routed/defs.h */

#define equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)


/* system definitions */

extern	char *sys_errlist[];
extern	int errno;


/* external definitions */

extern int tracing;			/* trace packets and route changes */
extern int n_interfaces;		/* # internet interfaces */
extern int n_remote_nets;		/* # remote nets via internal 
						non-routing gateways */
extern struct rthash rt_interior;	/* routes interior to my autonomous 
						system */
extern struct rthash nethash[];	   /* exterior routes advised by EGP neighbor
					declared in table2.h */
extern struct interface *ifnet;	   /* direct internet interface list */
extern int terminate;		   /* terminate EGP process - set by
					egpallcease(); tested by 
					egpstunacq() and egpacq() */
extern int	s;		/* socket for ioctl calls installing routes,
				   set in main() */
extern int	install;	/* if TRUE install route in kernelcall kernel,
				 * it is set by main() after kernel routes
				 * initially read and tested in table2.c */
extern int  rt_default_status;	/* NULL (no default) | INSTALLED (install 
				   needs to be true also) | NOTINSTALLED */
extern u_short	mysystem;		/* autonomous system number */
extern	int	nneigh;			/* number of trusted neighbors in 
						egpnn[] */
extern	int	maxacq;		/* maximum number neighbors to be acquired */
extern  int	n_acquired;	/* number neighbors acquired */
extern	int	egpsleep;	/* No. seconds between egpjob wakeups.
				   Time computed when neigh. (re)acquired 
				   or dropped */
extern	struct egpngh *egpngh;	/* start of linked list of egp neighbor state
				   tables */

extern	u_short	egprid_h;	/* sequence number of received egp packet
				   in host byte order - all ids in internal
				   tables are in host byte order */
extern  int	rt_maxage;	/* maximum allowed age of any route since last
				   updated by an NR message */
extern	int	maxpollint;	/* maximum poll interval of acquired neighbors
				   set in egpstime(), used in rt_NRupdate() */



/* function type declarations */

char *malloc();
char *inet_ntoa();
struct in_addr inet_makeaddr();
int timeout();
int egpallcease();
struct rt_entry *rt_ext_lookup();
struct rt_entry *rt_int_lookup();
struct	interface *if_withnet();
