/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)x25_var.h	7.1 (Berkeley) %G%
 */


/*
 * Device-independent x25 driver data.
 */
struct	x25com {
	struct 	ifnet xc_if;		/* network-visible interface */
/*	int	(*xc_if.if_start)()	/* connect, confirm procedure */
	u_char	xc_addrlen;		/* length of X.121 address */
	u_char	xc_addr[16];		/* X.121 address */
	u_short xc_flags;		/* X.25 specific flags */
	u_short	xc_nchan;		/* number of logical channels */
	u_short xc_nactive;		/* number of live logical channels */
	u_short xc_npvc;		/* # of permanent virt. circuits */
	u_short xc_pvcx;		/* index of first pcv */
	u_short xc_svcx;		/* index of first svc */
	u_short xc_dg_idletimo;		/* timeout to close idle dg channel */
	u_short xc_rslvtimo;		/* if name translation fails, rid */
	struct	x25lcb **xc_lcbvec;	/* where to dispatch data */
	int	(*xc_disconnect)();	/* to delete idle dg circuits */
};
#define	XCF_HSL3	0x1		/* Hardware support for level 3 */
#define XCF_HSL2	0x2		/* Hardware support for level 2 */


/*
 * Local Connection Block.
 */

struct x25lcb {
	struct	xq	{
		int	(*xq_put)();	/* How to process data */
		struct	mbuf *xq_data;	/* Queued data */
		int	xq_space;	/* For accounting */
		int	xq_flags;
		int	(*xq_unblock)();/* called & cleared when unblocking */
		caddr_t xq_proto;	/* for other service entries */
		caddr_t xq_next;	/* next q, or route, or pcb */
	}	xl_downq, xl_upq;
/*	(*xl_downq.xq_next->xq_put)()	/* LAPB put when using pk_output() */
	int	xl_flags;		/* valid, c-o versus dg */
	int	xl_timer;		/* figure out what to close */
	int	xl_state;		/* connecting, connected, resolving */
	int	xl_index;		/* which entry we are in device table */
	struct	x25com *xl_xc;		/* back pointer to device */
	/*
	 * x25pkb (packet-level control block) would follow immediately
	 * for devices only supplying LAPB or less.
	 */
};
/* flags */
#define XL_VALID	0x1		/* Circuit is live, etc. */
#define XL_DGRAM	0x2		/* not connection oriented, can close */
#define XL_RTHELD	0x4		/* this lcb references rtentry */

#define XQ_UP		0x1		/* I am an up queue */
#define XQ_DOWN		0x2		/* I am a down queue */

/* states for LCB */
#define XLS_NEWBORN	0x0
#define XLS_CONNECTED	0x1
#define XLS_CONNECTING	0x2
#define XLS_RESOLVING	0x3
#define XLS_DISCONNECTING 0x4
#define XLS_FREE	0x5

#ifdef KERNEL
int	x25_ifinput();
#endif
