/*	tp_var.h	1.3	82/10/09	*/

/*
 * Kernel data structures for DECnet Transport layer.
 */

/*
 * DECnet node number of this host.
 */
int	tp_host;

/*
 * Transport Statistics
 */
struct tpstat {
	int	tps_badinit;		/* bad initialization message */
	int	tps_init;		/* good initialization message */
	int	tps_p2hdr;		/* saw Phase II route header */
	int	tps_returned;		/* got a returned packet */
	int	tps_shortpacket;	/* packet was too short */
	int	tps_notforme;		/* dstnode not this system */
	int	tps_verif;		/* saw a verification message */
	int	tps_badtest;		/* bad hello and test message */
	int	tps_unknown;		/* unknown message type */
	int	tps_badsrc;		/* source number out of range */
	int	tps_route;		/* got routing message */
};

/*
 * Transport State
 * (should be per interface when routing)
 */
int	tpstate;

#define	TPS_HALT	0		/* line not initialized */
#define	TPS_TIS		1		/* transport initialize sent */
#define	TPS_RUN		2		/* line ready for use */

/*
 * Routing Parameters
 * (Transport spec, page 23)
 */
struct tprp {
	short	tprp_nn;	/* number of nodes in network (max addr) */
	/* NEED MORE FOR FULL ROUTING */
};

#ifdef KERNEL
struct	tpstat	tpstat;
struct	tprp	tprp;
struct	ifqueue	tpintrq;
struct	ifnet	*tpifp;		/* The DECnet hardware interface */
#endif
