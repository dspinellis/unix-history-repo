/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that: (1) source code distributions
 * retain the above copyright notice and this paragraph in its entirety, (2)
 * distributions including binary code include the above copyright notice and
 * this paragraph in its entirety in the documentation or other materials
 * provided with the distribution, and (3) all advertising materials mentioning
 * features or use of this software display the following acknowledgement:
 * ``This product includes software developed by the University of California,
 * Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
 * the University nor the names of its contributors may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * @(#) $Header: bpfdesc.h,v 1.7 90/12/04 01:05:01 mccanne Exp $ (LBL)
 *
 * This code is derived from the Stanford/CMU enet packet filter,
 * (net/enetdefs.h) distributed in 4.3BSD Unix.
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
	struct mbuf *	bd_sbuf;	/* store slot */
	struct mbuf *	bd_hbuf;	/* hold slot */
	struct mbuf *	bd_fbuf;	/* free slot */

	struct bpf_if *	bd_bif;		/* interface descriptor */
	u_long		bd_rtout;	/* Read timeout in 'ticks' */
	struct mbuf *	bd_filterm;	/* Packet filter mbuf */
	struct bpf_insn *bd_filter; 	/* precomputed pointer to fcode */
	u_long		bd_rcount;	/* number of packets received */
	u_long		bd_dcount;	/* number of packets dropped */
	struct proc *	bd_SelProc;	/* process that last selected us */

	u_char		bd_promisc;	/* true if listening promiscuously */
	u_char		bd_state;	/* idle, waiting, or timed out */
	u_char		bd_SelColl;	/* true if selects collide */
	u_char		bd_immediate;	/* true to return on packet arrival */
};

/*
 * Descriptor associated with each attached hardware interface.
 */
struct bpf_if {
	/* List of descriptors listening on this interface. */
	struct bpf_d *bif_dlist;

	/* Pointer to the device driver's softc bpf field. */
	struct bpf_if **bif_driverp;

	/* Device parameters, see bpf.h. */
	struct bpf_devp bif_devp;

	/* Length of bpf header (bpf_hdr + padding). */
	u_int bif_hdrlen;

	/* 'ifnet' of associated interface. */
	struct ifnet *bif_ifp;
};
