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
 *	@(#)pk_input.c	7.4 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"

#include "../netccitt/x25.h"
#include "../netccitt/pk.h"
#include "../netccitt/pk_var.h"

/* 
 *  This procedure is called by the link level whenever the link
 *  becomes operational, is reset, or when the link goes down. 
 */

pk_ctlinput (code, pkp)
register struct pkcb *pkp;
{

	if (pkp == 0)
		return (EINVAL);
	switch (code) {
	case PRC_LINKUP: 
		if (pkp -> pk_state == DTE_WAITING)
			pk_restart (pkp, X25_RESTART_NETWORK_CONGESTION);
		break;

	case PRC_LINKDOWN: 
		pk_restart (pkp, -1);	/* Clear all active circuits */
		pkp -> pk_state = DTE_WAITING;
		break;

	case PRC_LINKRESET: 
		pk_restart (pkp, X25_RESTART_NETWORK_CONGESTION);
		break;

	}
	return (0);
}

/* 
 *  X.25 PACKET INPUT
 *
 *  This procedure is called by a link level procedure whenever
 *  an information frame is received. It decodes the packet and
 *  demultiplexes based on the logical channel number.
 *
 */

pk_input (m, xcp)
register struct mbuf *m;
struct x25config *xcp;
{
	register struct x25_packet *xp;
	register struct pklcd *lcp;
	register struct socket *so = 0;
	register struct pkcb *pkp;
	int  ptype, lcn, lcdstate = LISTEN;
	static struct x25config *lastxcp;
	static struct pkcb *lastpkp;

	if (xcp == lastxcp)
		pkp = lastpkp;
	else {
		for (pkp = pkcbhead; ; pkp = pkp -> pk_next) {
			if (pkp == 0) {
				pk_message (0, xcp, "pk_input: unknown network");
				m_freem (m);
				return;
			}
			if (pkp -> pk_xcp == xcp)
				break;
		}
		lastxcp = xcp;
		lastpkp = pkp;
	}

	xp = mtod (m, struct x25_packet *);
	ptype = pk_decode (xp);
	lcn = xp -> logical_channel_number;
	lcp = pkp -> pk_chan[lcn];

	/* 
	 *  If the DTE is in Restart  state, then it will ignore data, 
	 *  interrupt, call setup and clearing, flow control and reset 
	 *  packets.
	 */
	if (lcn < 0 || lcn > pkp -> pk_maxlcn) {
		pk_message (lcn, pkp -> pk_xcp, "illegal lcn");
		m_freem (m);
		return;
	}

	pk_trace (pkp -> pk_xcp, xp, "P-In");

	if (pkp -> pk_state != DTE_READY && ptype != RESTART && ptype != RESTART_CONF) {
		m_freem (m);
		return;
	}
	if (lcp) {
		so = lcp -> lcd_so;
		lcdstate = lcp -> lcd_state;
	} else {
		if (ptype == CLEAR) {	/* idle line probe (Datapac specific) */
			/* send response on lcd 0's output queue */
			lcp -> lcd_template = pk_template (lcn, X25_CLEAR_CONFIRM);
			pk_output (lcp);
			m_freem (m);
			return;
		}
		if (ptype != CALL)
			ptype = INVALID_PACKET;
	}

	if (lcn == 0 && ptype != RESTART && ptype != RESTART_CONF) {
		pk_message (0, pkp -> pk_xcp, "illegal ptype (%s) on lcn 0",
			pk_name[ptype / MAXSTATES]);
		m_freem (m);
		return;
	}

	switch (ptype + lcdstate) {
	/* 
	 *  Incoming Call packet received. 
	 */
	case CALL + LISTEN: 
		incoming_call (pkp, xp, m -> m_len);
		break;

	/* 	
	 *  Call collision: Just throw this "incoming call" away since 
	 *  the DCE will ignore it anyway. 
	 */
	case CALL + SENT_CALL: 
		pk_message ((int)xp -> logical_channel_number, pkp -> pk_xcp, 
			"incoming call collision");
		break;

	/* 
	 *  Call confirmation packet received. This usually means our
	 *  previous connect request is now complete.
	 */
	case CALL_ACCEPTED + SENT_CALL: 
		call_accepted (lcp, xp, m -> m_len);
		break;

	/* 
	 *  This condition can only happen if the previous state was
	 *  SENT_CALL. Just ignore the packet, eventually a clear 
	 *  confirmation should arrive.
	 */
	case CALL_ACCEPTED + SENT_CLEAR: 
		break;

	/* 
	 *  Clear packet received. This requires a complete tear down
	 *  of the virtual circuit.  Free buffers and control blocks.
	 *  and send a clear confirmation.
	 */
	case CLEAR + READY:
	case CLEAR + RECEIVED_CALL: 
	case CLEAR + SENT_CALL: 
	case CLEAR + DATA_TRANSFER: 
		lcp -> lcd_state = RECEIVED_CLEAR;
		lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_CLEAR_CONFIRM);
		pk_output (lcp);
		pk_clearcause (pkp, xp);
		pk_close (lcp);
		break;

	/* 
	 *  Clear collision: Treat this clear packet as a confirmation.
	 */
	case CLEAR + SENT_CLEAR: 
		pk_close (lcp);
		break;

	/* 
	 *  Clear confirmation received. This usually means the virtual
	 *  circuit is now completely removed.
	 */
	case CLEAR_CONF + SENT_CLEAR: 
		pk_close (lcp);
		break;

	/* 
	 *  A clear confirmation on an unassigned logical channel - just
	 *  ignore it. Note: All other packets on an unassigned channel
	 *  results in a clear.
	 */
	case CLEAR_CONF + READY:
		break;

	/* 
	 *  Data packet received. Pass on to next level. Move the Q and M
	 *  bits into the data portion for the next level.
	 */
	case DATA + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition) {
			ptype = DELETE_PACKET;
			break;
		}

		/* 
		 *  Process the P(S) flow control information in this Data packet. 
		 *  Check that the packets arrive in the correct sequence and that 
		 *  they are within the "lcd_input_window". Input window rotation is 
		 *  initiated by the receive interface.
		 */

		if (PS(xp) != ((lcp -> lcd_rsn + 1) % MODULUS) ||
			PS(xp) == ((lcp -> lcd_input_window + lcp->lcd_windowsize) % MODULUS)) {
			m_freem (m);
			pk_procerror (RESET, lcp, "p(s) flow control error");
			break;
		}
		lcp -> lcd_rsn = PS(xp);

		if (pk_ack (lcp, PR(xp)) != PACKET_OK) {
			m_freem (m);
			break;
		}

		m -> m_data += PKHEADERLN;
		m -> m_len -= PKHEADERLN;
		if (lcp -> lcd_flags & X25_MQBIT) {
			octet *t;

			m -> m_data -= 1;
			m -> m_len += 1;
			t = mtod (m, octet *);
			*t = 0x00;
			if (xp -> q_bit)
				*t |= 0x80;
			if (MBIT(xp))
				*t |= 0x40;
		}

		/*
		 * Discard Q-BIT packets if the application
		 * doesn't want to be informed of M and Q bit status
		 */
		if (xp -> q_bit && (lcp -> lcd_flags & X25_MQBIT) == 0) {
			m_freem (m);
			lcp -> lcd_rxcnt++;
			/*
			 * NB.  This is dangerous: sending a RR here can
			 * cause sequence number errors if a previous data
			 * packet has not yet been passed up to the application
			 * (RR's are normally generated via PRU_RCVD).
			 */
			lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_RR);
			pk_output (lcp);
		} else {
#ifdef BSD4_3
			sbappendrecord (&so -> so_rcv, m);
#else
			sbappend (&so -> so_rcv, m);
#endif
			sorwakeup (so);
		}
		break;

	/* 
	 *  Interrupt packet received.
	 */
	case INTERRUPT + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition)
			break;
		lcp -> lcd_intrdata = xp -> packet_data;
		sohasoutofband (so);
		lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_INTERRUPT_CONFIRM);
		pk_output (lcp);
		break;

	/* 
	 *  Interrupt confirmation packet received.
	 */
	case INTERRUPT_CONF + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition)
			break;
		if (lcp -> lcd_intrconf_pending == TRUE)
			lcp -> lcd_intrconf_pending = FALSE;
		else
			pk_procerror (RESET, lcp, "unexpected packet");
		break;

	/* 
	 *  Receiver ready received. Rotate the output window and output
	 *  any data packets waiting transmission.
	 */
	case RR + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition)
			break;
		if (pk_ack (lcp, PR(xp)) != PACKET_OK)
			break;
		if (lcp -> lcd_rnr_condition == TRUE)
			lcp -> lcd_rnr_condition = FALSE;
		pk_output (lcp);
		break;

	/* 
	 *  Receiver Not Ready received. Packets up to the P(R) can be
	 *  be sent. Condition is cleared with a RR.
	 */
	case RNR + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition)
			break;
		if (pk_ack (lcp, PR(xp)) != PACKET_OK)
			break;
		lcp -> lcd_rnr_condition = TRUE;
		break;

	/* 
	 *  Reset packet received. Set state to FLOW_OPEN.  The Input and
	 *  Output window edges ar set to zero. Both the send and receive
	 *  numbers are reset. A confirmation is returned.
	 */
	case RESET + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition)
			/* Reset collision. Just ignore packet. */
			break;

		pk_resetcause (pkp, xp);
		sbflush (&so -> so_snd);
		sbflush (&so -> so_rcv);

		wakeup ((caddr_t) & so -> so_timeo);
		sorwakeup (so);
		sowwakeup (so);

		lcp -> lcd_window_condition = lcp -> lcd_rnr_condition =
			lcp -> lcd_intrconf_pending = FALSE;
		lcp -> lcd_output_window = lcp -> lcd_input_window =
			lcp -> lcd_last_transmitted_pr = 0;
		lcp -> lcd_ssn = 0;
		lcp -> lcd_rsn = MODULUS - 1;

		lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_RESET_CONFIRM);
		pk_output (lcp);
		break;

	/* 
	 *  Reset confirmation received.
	 */
	case RESET_CONF + DATA_TRANSFER: 
		if (lcp -> lcd_reset_condition) {
			lcp -> lcd_reset_condition = FALSE;
			pk_output (lcp);
		}
		else
			pk_procerror (RESET, lcp, "unexpected packet");
		break;

	case DATA + SENT_CLEAR: 
		ptype = DELETE_PACKET;
	case RR + SENT_CLEAR: 
	case RNR + SENT_CLEAR: 
	case INTERRUPT + SENT_CLEAR: 
	case INTERRUPT_CONF + SENT_CLEAR: 
	case RESET + SENT_CLEAR: 
	case RESET_CONF + SENT_CLEAR: 
		/* Just ignore packet if we have sent a CLEAR already.
		   */
		break;

	/* 
	 *  Restart sets all the permanent virtual circuits to the "Data
	 *  Transfer" stae and  all the switched virtual circuits to the
	 *  "Ready" state.
	 */
	case RESTART + READY: 
		switch (pkp -> pk_state) {
		case DTE_SENT_RESTART: 
			/* Restart collision. */
			pkp -> pk_state = DTE_READY;
			pk_message (0, pkp -> pk_xcp,
				"Packet level operational");
			break;

		default: 
			pk_restart (pkp, -1);
			pk_restartcause (pkp, xp);
			pkp -> pk_chan[0] -> lcd_template = pk_template (0,
				X25_RESTART_CONFIRM);
			pk_output (pkp -> pk_chan[0]);
		}
		break;

	/* 
	 *  Restart confirmation received. All logical channels are set
	 *  to READY. 
	 */
	case RESTART_CONF + READY: 
		switch (pkp -> pk_state) {
		case DTE_SENT_RESTART: 
			pkp -> pk_state = DTE_READY;
			pk_message (0, pkp -> pk_xcp,
				"Packet level operational");
			break;

		default: 
			/* Restart local procedure error. */
			pk_restart (pkp, X25_RESTART_LOCAL_PROCEDURE_ERROR);
			pkp -> pk_state = DTE_SENT_RESTART;
		}
		break;

	default: 
		if (lcp) {
			pk_procerror (CLEAR, lcp, "unknown packet error");
			pk_message (lcn, pkp -> pk_xcp,
				"\"%s\" unexpected in \"%s\" state",
				pk_name[ptype/MAXSTATES], pk_state[lcdstate]);
		}
		else	/* Packets arrived on an unassigned channel. 
			*/
			pk_message ((int)xp->logical_channel_number, pkp -> pk_xcp,
				"packet arrived on unassigned lcn");
		break;
	}
	if (ptype != DATA)
		m_freem (m);
}


/* 
 * This routine handles incoming call packets. It matches the protocol
 * field on the Call User Data field (usually the first four bytes) with 
 * sockets awaiting connections.
 */

static
incoming_call (pkp, xp, len)
struct pkcb *pkp;
struct x25_packet *xp;
{
	register struct pklcd *lcp = 0, *l;
	register struct sockaddr_x25 *sa;
	register struct x25_calladdr *a;
	register struct socket *so = 0;
	struct mbuf *m;
	register int l1, l2;
	char *e, *errstr = "server unavailable";
	octet *u;
	int lcn = xp -> logical_channel_number;

	/* First, copy the data from the incoming call packet to a X25_socket
	   descriptor. */

	a = (struct x25_calladdr *) &xp -> packet_data;
	l1 = a -> calling_addrlen;
	l2 = a -> called_addrlen;
	if ((m = m_getclr (M_DONTWAIT, MT_HEADER)) == 0)
		return;
	sa = mtod (m, struct sockaddr_x25 *);
	u = (octet *) (a -> address_field + l2 / 2);
	e = sa -> x25_addr;
	if (l2 & 0x01) {
		*e++ = *u++ & 0x0f;
		l1--;
	}
	from_bcd (e, &u, l1);
	if (l1 & 0x01)
		u++;

	parse_facilities (u, sa);
	u += *u + 1;
	sa -> x25_udlen = min (16, ((octet *)xp) + len - u);
	if (sa -> x25_udlen < 0)
		sa -> x25_udlen = 0;
	bcopy ((caddr_t)u, sa -> x25_udata, (unsigned)sa -> x25_udlen);

	/*
	 * Now, loop through the  listen sockets looking for a match on the
	 * PID. That is  the first  four octets  of the user data field.  This
	 * is the closest thing to a port number for X.25 packets. What it
	 * does provide is away of  multiplexing  services at the user level. 
	 */

	for (l = pk_listenhead; l; l = l -> lcd_listen) {
		struct sockaddr_x25 *sxp = l -> lcd_ceaddr;

		if (bcmp (sxp -> x25_udata, sa -> x25_udata, sxp->x25_udlen))
			continue;
		if (sxp -> x25_net && sxp -> x25_net != pkp->pk_xcp->xc_net)
			continue;
		/*
		 * don't accept incoming collect calls unless
		 * the server sets the reverse charging option.
		 */
		if ((sxp -> x25_opts.op_flags & (X25_OLDSOCKADDR|X25_REVERSE_CHARGE)) == 0 &&
			sa -> x25_opts.op_flags & X25_REVERSE_CHARGE) {
			errstr = "incoming collect call refused";
			break;
		}
		if (l -> lcd_so) {
			if (so = sonewconn (l -> lcd_so, SO_ISCONNETED))
				    lcp = (struct pklcd *) so -> so_pcb;
		} else 
			lcp = pk_attach((struct socket *) 0);
		if (lcp == 0) {
			/*
			 * Insufficient space or too many unaccepted
			 * connections.  Just throw the call away.
			 */
			errstr = "server malfunction";
			break;
		}
		lcp -> lcd_upper = l -> lcd_upper;
		lcp -> lcd_upnext = l -> lcd_upnext;
		lcp -> lcd_lcn = lcn;
		lcp -> lcd_state = RECEIVED_CALL;
		lcp -> lcd_craddr = sa;
		sa -> x25_opts.op_flags |= sxp -> x25_opts.op_flags &
			~X25_REVERSE_CHARGE;
		pk_assoc (pkp, lcp, sa);
		lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_CALL_ACCEPTED);
		if (so) {
			pk_output (lcp);
			soisconnected (so);
		} else if (lcp->lcd_upper)
			(*lcp->lcd_upper)(lcp);
		return;
	}

	/*
	 * If the call fails for whatever reason, we still need to build a
	 * skeleton LCD in order to be able to properly  receive the CLEAR
	 * CONFIRMATION.
	 */
#ifdef WATERLOO		/* be explicit */
	if (l == 0 && bcmp(sa->x25_udata, "ean", 3) == 0)
		pk_message (lcn, pkp -> pk_xcp, "host=%s ean%c: %s",
			sa->x25_addr, sa->x25_udata[3] & 0xff, errstr);
	else if (l == 0 && bcmp(sa->x25_udata, "\1\0\0\0", 4) == 0)
		pk_message (lcn, pkp -> pk_xcp, "host=%s x29d: %s",
			sa->x25_addr, errstr);
	else
#endif
	pk_message (lcn, pkp -> pk_xcp, "host=%s pid=%x %x %x %x: %s",
		sa -> x25_addr, sa -> x25_udata[0] & 0xff,
		sa -> x25_udata[1] & 0xff, sa -> x25_udata[2] & 0xff,
		sa -> x25_udata[3] & 0xff, errstr);
	if ((m = m_getclr (M_DONTWAIT, MT_HEADER)) == 0) {
		(void) m_free (dtom (sa));
		return;
	}
	lcp = mtod (m, struct pklcd *);
	lcp -> lcd_lcn = lcn;
	lcp -> lcd_state = RECEIVED_CALL;
	pk_assoc (pkp, lcp, sa);
	(void) m_free (dtom (sa));
	pk_clear (lcp);
}

static
call_accepted (lcp, xp, len)
struct pklcd *lcp;
struct x25_packet *xp;
{
	register struct x25_calladdr *ap;
	register octet *fcp;

	lcp -> lcd_state = DATA_TRANSFER;
	soisconnected (lcp -> lcd_so);
	if (len > 3) {
		ap = (struct x25_calladdr *) &xp -> packet_data;
		fcp = (octet *) ap -> address_field + (ap -> calling_addrlen +
			ap -> called_addrlen + 1) / 2;
		if (fcp + *fcp <= ((octet *)xp) + len)
			parse_facilities (fcp, lcp -> lcd_ceaddr);
	}
	pk_assoc (lcp -> lcd_pkp, lcp, lcp -> lcd_ceaddr);
}

static
parse_facilities (fcp, sa)
register octet *fcp;
register struct sockaddr_x25 *sa;
{
	register octet *maxfcp;

	maxfcp = fcp + *fcp;
	fcp++;
	while (fcp < maxfcp) {
		/*
		 * Ignore national DCE or DTE facilities
		 */
		if (*fcp == 0 || *fcp == 0xff)
			break;
		switch (*fcp) {
		case FACILITIES_WINDOWSIZE:
			sa -> x25_opts.op_wsize = fcp[1];
			fcp += 3;
			break;

		case FACILITIES_PACKETSIZE:
			sa -> x25_opts.op_psize = fcp[1];
			fcp += 3;
			break;

		case FACILITIES_THROUGHPUT:
			sa -> x25_opts.op_speed = fcp[1];
			fcp += 2;
			break;

		case FACILITIES_REVERSE_CHARGE:
			if (fcp[1] & 01)
				sa -> x25_opts.op_flags |= X25_REVERSE_CHARGE;
			/*
			 * Datapac specific: for a X.25(1976) DTE, bit 2
			 * indicates a "hi priority" (eg. international) call.
			 */
			if (fcp[1] & 02 && sa -> x25_opts.op_psize == 0)
				sa -> x25_opts.op_psize = X25_PS128;
			fcp += 2;
			break;

		default:
/*printf("unknown facility %x, class=%d\n", *fcp, (*fcp & 0xc0) >> 6);*/
			switch ((*fcp & 0xc0) >> 6) {
			case 0:			/* class A */
				fcp += 2;
				break;

			case 1:
				fcp += 3;
				break;

			case 2:
				fcp += 4;
				break;

			case 3:
				fcp++;
				fcp += *fcp;
			}
		}
	}
}

from_bcd (a, x, len)
register char *a;
register octet **x;
register int len;
{
	register int posn = 0;

	while (--len >= 0) {
		if (posn++ & 0x01)
			*a = *(*x)++ & 0x0f;
		else
			*a = (**x >> 4) & 0x0F;
		*a++ |= 0x30;
	}
}
