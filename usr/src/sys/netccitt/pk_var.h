/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_var.h	7.3 (Berkeley) %G%
 */

/*
 * Protocol-Protocol Packet Buffer.
 * (Eventually will be replace by system-wide structure).
 */

struct	pq	{
	int	(*pq_put)();	/* How to process data */
	struct	mbuf *pq_data;	/* Queued data */
	int	pq_space;	/* For accounting */
	int	pq_flags;
	int	(*pq_unblock)();/* called & cleared when unblocking */
	caddr_t pq_proto;	/* for other service entries */
	caddr_t pq_next;	/* next q, or route, or pcb */
};

/*
 *
 *  X.25 Logical Channel Descriptor
 *
 */

struct pklcd {
	struct	pq lcd_downq, lcd_upq;	/* protocol glue for datagram service */
	short   lcd_lcn;		/* Logical channel number */
	short   lcd_state;		/* Logical Channel state */
        bool	lcd_intrconf_pending;	/* Interrupt confirmation pending */
	octet	lcd_intrdata;		/* Octet of incoming intr data */
	short   lcd_timer;		/* Various timer values */
	short   lcd_dg_timer;		/* to reclaim idle datagram circuits */
	char	lcd_retry;		/* Timer retry count */
	char	lcd_rsn;		/* Seq no of last received packet */
	char	lcd_ssn;		/* Seq no of next packet to send */
	char	lcd_output_window;	/* Output flow control window */
	char	lcd_input_window;	/* Input flow control window */
	char	lcd_last_transmitted_pr;/* Last Pr value transmitted */
        bool	lcd_rnr_condition;	/* Remote in busy condition */
        bool	lcd_window_condition;	/* Output window size exceeded */
        bool	lcd_reset_condition;	/* True, if waiting reset confirm */
	char	lcd_packetsize;		/* Maximum packet size */
	char	lcd_windowsize;		/* Window size - both directions */
        octet	lcd_closed_user_group;	/* Closed user group specification */
	char	lcd_flags;		/* copy of sockaddr_x25 op_flags */
	struct	x25_packet *lcd_template;/* Address of current packet */
	struct	socket *lcd_so;		/* Socket addr for connection */
	struct	sockaddr_x25 *lcd_craddr;/* Calling address */
	struct	sockaddr_x25 *lcd_ceaddr;/* Called address */
	time_t	lcd_stime;		/* time circuit established */
	long    lcd_txcnt;		/* Data packet transmit count */
	long    lcd_rxcnt;		/* Data packet receive count */
	short   lcd_intrcnt;		/* Interrupt packet transmit count */
	struct	pklcd *lcd_listen;	/* Next lcd on listen queue */
	struct	pkcb *lcd_pkp;		/* network this lcd is attached to */
};

#define X25_DG_CIRCUIT	0x10		/* lcd_flag: used for datagrams */
#define X25_DG_ROUTING	0x20		/* lcd_flag: peer addr not yet known */

/*
 * Per network information, allocated dynamically
 * when a new network is configured.
 */

struct	pkcb {
	struct	pkcb *pk_next;
	struct	x25_ifaddr *pk_ia;	/* backpointer to ifaddr */
	short	pk_state;		/* packet level status */
	int	(*pk_output) ();	/* link level output procedure */
	struct	x25config *pk_xcp;	/* network specific configuration */
	struct	x25config pk_xc;	/* network specific configuration */
	struct	pklcd **pk_chan;	/* actual size == xc_maxlcn+1 */
#define	pk_maxlcn pk_xc.xc_maxlcn	/* local copy of xc_maxlcn */
};
/*
 *	Interface address, x25 version. Exactly one of these structures is 
 *	allocated for each interface with an x25 address.
 *
 *	The ifaddr structure conatins the protocol-independent part
 *	of the structure, and is assumed to be first.
 */
struct x25_ifaddr {
	struct	ifaddr ia_ifa;		/* protocol-independent info */
#define ia_ifp		ia_ifa.ifa_ifp
#define	ia_flags	ia_ifa.ifa_flags
	struct	pkcb	ia_pkcb;	/* per network information */
#define ia_maxlcn	ia_pkcb.pk_maxlcn
#define ia_chan		ia_pkcb.pk_chan
#define	ia_addr		ia_pkcb.pk_xc.xc_addr
	struct	sockaddr_x25 ia_sockmask; /* reserve space for netmask */
};

/*
 * ``Link-Level'' extension to Routing Entry for upper level
 * packet switching via X.25 virtual circuits.
 */
struct rtextension_x25 {
	struct	pklcd *rtx_lcd;		/* local connection block */
	struct	rtentry *rtx_rt;	/* back pointer to route */
	struct	x25_ifaddr *rtx_ia;	/* may not be same as rt_ifa */
	int	rtx_state;		/* can't trust lcd->lcd_state */
	int	rtx_flags;
	int	rtx_timer;		/* for idle timeout */
};

/* States for rtx_state */
#define XRS_NEWBORN		0
#define XRS_RESOLVING		1
#define XRS_FREE		2
#define XRS_CONNECTED		3
#define XRS_CONNECTING		4
#define XRS_DISCONNECTING 	5

/* flags */
#define XRF_VALID	0x1		/* Circuit is live, etc. */
#define XRF_RTHELD	0x2		/* this lcb references rtentry */

#ifdef KERNEL
struct	pkcb *pkcbhead;		/* head of linked list of networks */
struct	pklcd *pk_listenhead;

char	*pk_name[], *pk_state[];
int	pk_t20, pk_t21, pk_t22, pk_t23;
#endif
