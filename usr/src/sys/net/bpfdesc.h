/*
 * Copyright (c) 1990, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from the Stanford/CMU enet packet filter,
 * (net/enet.c) distributed as part of 4.3BSD, and code contributed
 * to Berkeley by Steven McCanne and Van Jacobson both of Lawrence
 * Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)bpfdesc.h	7.6 (Berkeley) %G%
 *
 * @(#) $Header: bpfdesc.h,v 1.9 91/10/27 21:22:38 mccanne Exp $ (LBL)
 */

/*
 * Descriptor associated with each open bpf file.
 */
struct bpf_d {
	struct bpf_d	*bd_next;	/* Linked list of descriptors */
	/*
	 * Buffer slots: two mbuf clusters buffer the incoming packets.
	 *   The model has three slots.  Sbuf is always occupied.
	 *   sbuf (store) - Receive interrupt puts packets here.
	 *   hbuf (hold) - When sbuf is full, put cluster here and
	 *                 wakeup read (replace sbuf with fbuf).
	 *   fbuf (free) - When read is done, put cluster here.
	 * On receiving, if sbuf is full and fbuf is 0, packet is dropped.
	 */
	caddr_t		bd_sbuf;	/* store slot */
	caddr_t		bd_hbuf;	/* hold slot */
	caddr_t		bd_fbuf;	/* free slot */
	int 		bd_slen;	/* current length of store buffer */
	int 		bd_hlen;	/* current length of hold buffer */

	int		bd_bufsize;	/* absolute length of buffers */

	struct bpf_if *	bd_bif;		/* interface descriptor */
	u_long		bd_rtout;	/* Read timeout in 'ticks' */
	struct bpf_insn *bd_filter; 	/* filter code */
	u_long		bd_rcount;	/* number of packets received */
	u_long		bd_dcount;	/* number of packets dropped */

	u_char		bd_promisc;	/* true if listening promiscuously */
	u_char		bd_state;	/* idle, waiting, or timed out */
	u_char		bd_immediate;	/* true to return on packet arrival */
#if BSD < 199103
	u_char		bd_selcoll;	/* true if selects collide */
	int		bd_timedout;
	struct proc *	bd_selproc;	/* process that last selected us */
#else
	u_char		bd_pad;		/* explicit alignment */
	struct selinfo	bd_sel;		/* bsd select info */
#endif
};

/*
 * Descriptor associated with each attached hardware interface.
 */
struct bpf_if {
	struct bpf_if *bif_next;	/* list of all interfaces */
	struct bpf_d *bif_dlist;	/* descriptor list */
	struct bpf_if **bif_driverp;	/* pointer into softc */
	u_int bif_dlt;			/* link layer type */
	u_int bif_hdrlen;		/* length of header (with padding) */
	struct ifnet *bif_ifp;		/* correspoding interface */
};

#ifdef KERNEL
int	 bpf_setf __P((struct bpf_d *, struct bpf_program *));
#endif
