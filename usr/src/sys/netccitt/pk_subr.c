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
 *	@(#)pk_subr.c	7.11 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "protosw.h"
#include "socketvar.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"

#include "../net/if.h"

#include "x25.h"
#include "pk.h"
#include "pk_var.h"
#include "x25err.h"

int     pk_sendspace = 1024 * 2 + 8;
int     pk_recvspace = 1024 * 2 + 8;

struct pklcd_q pklcd_q = {&pklcd_q, &pklcd_q};

/* 
 *  Attach X.25 protocol to socket, allocate logical channel descripter
 *  and buffer space, and enter LISTEN state if we are to accept
 *  IN-COMMING CALL packets.  
 *
 */

struct pklcd *
pk_attach (so)
struct socket *so;
{
	register struct pklcd *lcp;
	register int error = ENOBUFS;

	MALLOC(lcp, struct pklcd *, sizeof (*lcp), M_PCB, M_NOWAIT);
	if (lcp) {
		bzero ((caddr_t)lcp, sizeof (*lcp));
		insque (&lcp -> lcd_q, &pklcd_q);
		if (so) {
			error = soreserve (so, pk_sendspace, pk_recvspace);
			lcp -> lcd_so = so;
			if (so -> so_options & SO_ACCEPTCONN)
				lcp -> lcd_state = LISTEN;
			else
				lcp -> lcd_state = READY;
		} else
			sbreserve (&lcp -> lcd_sb, pk_sendspace);
	}
	if (so) {
		so -> so_pcb = (caddr_t) lcp;
		so -> so_error = error;
	}
	return (lcp);
}

/* 
 *  Disconnect X.25 protocol from socket.
 */

pk_disconnect (lcp)
register struct pklcd *lcp;
{
	register struct socket *so = lcp -> lcd_so;
	register struct pklcd *l, *p;

	switch (lcp -> lcd_state) {
	case LISTEN: 
		for (p = 0, l = pk_listenhead; l && l != lcp; p = l, l = l -> lcd_listen);
		if (p == 0) {
			if (l != 0)
				pk_listenhead = l -> lcd_listen;
		}
		else
		if (l != 0)
			p -> lcd_listen = l -> lcd_listen;
		pk_close (lcp);
		break;

	case READY: 
		pk_acct (lcp);
		pk_close (lcp);
		break;

	case SENT_CLEAR: 
	case RECEIVED_CLEAR: 
		break;

	default: 
		pk_acct (lcp);
		if (so) {
			soisdisconnecting (so);
			sbflush (&so -> so_rcv);
		}
		pk_clear (lcp, 241, 0); /* Normal Disconnect */

	}
}

/* 
 *  Close an X.25 Logical Channel. Discard all space held by the
 *  connection and internal descriptors. Wake up any sleepers.
 */

pk_close (lcp)
struct pklcd *lcp;
{
	register struct socket *so = lcp -> lcd_so;

	pk_freelcd (lcp);

	if (so == NULL)
		return;

	so -> so_pcb = 0;
	soisdisconnected (so);
	/* sofree (so);	/* gak!!! you can't do that here */
}

/* 
 *  Create a template to be used to send X.25 packets on a logical
 *  channel. It allocates an mbuf and fills in a skeletal packet
 *  depending on its type. This packet is passed to pk_output where
 *  the remainer of the packet is filled in.
*/

struct mbuf *
pk_template (lcn, type)
int lcn, type;
{
	register struct mbuf *m;
	register struct x25_packet *xp;

	MGETHDR (m, M_DONTWAIT, MT_HEADER);
	if (m == 0)
		panic ("pk_template");
	m -> m_act = 0;

	/*
	 * Efficiency hack: leave a four byte gap at the beginning
	 * of the packet level header with the hope that this will
	 * be enough room for the link level to insert its header.
	 */
	m -> m_data += max_linkhdr;
	m -> m_len = PKHEADERLN;

	xp = mtod (m, struct x25_packet *);
	*(long *)xp = 0;		/* ugly, but fast */
/*	xp -> q_bit = 0;*/
	xp -> fmt_identifier = 1;
/*	xp -> lc_group_number = 0;*/

	SET_LCN(xp, lcn);
	xp -> packet_type = type;

	return (m);
}

/* 
 *  This routine restarts all the virtual circuits. Actually,
 *  the virtual circuits are not "restarted" as such. Instead,
 *  any active switched circuit is simply returned to READY
 *  state.
 */

pk_restart (pkp, restart_cause)
register struct pkcb *pkp;
int restart_cause;
{
	register struct mbuf *m;
	register struct pklcd *lcp;
	register int i;

	/* Restart all logical channels. */
	if (pkp -> pk_chan == 0)
		return;
	for (i = 1; i <= pkp -> pk_maxlcn; ++i)
		if ((lcp = pkp -> pk_chan[i]) != NULL) {
			if (lcp -> lcd_so) {
				lcp -> lcd_so -> so_error = ENETRESET;
				pk_close (lcp);
			} else {
				pk_flush (lcp);
				lcp -> lcd_state = READY;
				if (lcp -> lcd_upper)
					lcp -> lcd_upper (lcp, 0);
			}
		}

	if (restart_cause < 0)
		return;

	pkp -> pk_state = DTE_SENT_RESTART;
	lcp = pkp -> pk_chan[0];
	m = lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_RESTART);
	m -> m_len += 2;
	mtod (m, struct x25_packet *) -> packet_data = 0;	/* DTE only */
	mtod (m, octet *)[4]  = restart_cause;
	pk_output (lcp);
}


/* 
 *  This procedure frees up the Logical Channel Descripter.
 */

pk_freelcd (lcp)
register struct pklcd *lcp;
{
	if (lcp == NULL)
		return;

	if (lcp -> lcd_lcn > 0)
		lcp -> lcd_pkp -> pk_chan[lcp -> lcd_lcn] = NULL;

	pk_flush (lcp);
	remque (&lcp -> lcd_q);
	free ((caddr_t)lcp, M_PCB);
}


/* 
 *  Bind a address and protocol value to a socket.  The important
 *  part is the protocol value - the first four characters of the 
 *  Call User Data field.
 */

pk_bind (lcp, nam)
struct pklcd *lcp;
struct mbuf *nam;
{
	register struct pkcb *pkp;
	register struct pklcd *pp;
	register struct sockaddr_x25 *sa;

	if (nam == NULL)
		return (EADDRNOTAVAIL);
	if (lcp -> lcd_ceaddr)				/* XXX */
		return (EADDRINUSE);
	if (pk_checksockaddr (nam))
		return (EINVAL);
	sa = mtod (nam, struct sockaddr_x25 *);

	/*
	 * If the user wishes to accept calls only from a particular
	 * net (net != 0), make sure the net is known
	 */

	if (sa -> x25_net)
		for (pkp = pkcbhead; ; pkp = pkp -> pk_next) {
			if (pkp == 0)
				return (ENETUNREACH);
			if (pkp -> pk_xcp -> xc_addr.x25_net == sa -> x25_net)
				break;
		}

	/*
	 * For ISO's sake permit default listeners, but only one such . . .
	 */
	for (pp = pk_listenhead; pp; pp = pp -> lcd_listen) {
		register struct sockaddr_x25 *sa2 = pp -> lcd_ceaddr;
		if ((sa2 -> x25_udlen == sa -> x25_udlen) &&
		    (sa2 -> x25_udlen == 0 ||
		     (bcmp (sa2 -> x25_udata, sa -> x25_udata,
			    min (sa2 -> x25_udlen, sa -> x25_udlen)) == 0)))
				return (EADDRINUSE);
	}
	lcp -> lcd_laddr = *sa;
	lcp -> lcd_ceaddr = &lcp -> lcd_laddr;
	return (0);
}

/*
 * Include a bound control block in the list of listeners.
 */
pk_listen (lcp)
register struct pklcd *lcp;
{
	register struct pklcd **pp;

	if (lcp -> lcd_ceaddr == 0)
		return (EDESTADDRREQ);

	lcp -> lcd_state = LISTEN;
	/*
	 * Add default listener at end, any others at start.
	 */
	if (lcp -> lcd_ceaddr -> x25_udlen == 0) {
		for (pp = &pk_listenhead; *pp; )
			pp = &((*pp) -> lcd_listen);
		*pp = lcp;
	} else {
		lcp -> lcd_listen = pk_listenhead;
		pk_listenhead = lcp;
	}
	return (0);
}
/*
 * Include a listening control block for the benefit of other protocols.
 */
pk_protolisten (spi, spilen, callee)
int (*callee) ();
{
	register struct pklcd *lcp = pk_attach ((struct socket *)0);
	register struct mbuf *nam;
	register struct sockaddr_x25 *sa;
	int error = ENOBUFS;

	if (lcp) {
		if (nam = m_getclr (MT_SONAME, M_DONTWAIT)) {
			sa = mtod (nam, struct sockaddr_x25 *);
			sa -> x25_family = AF_CCITT;
			sa -> x25_len = nam -> m_len = sizeof (*sa);
			sa -> x25_udlen = spilen;
			sa -> x25_udata[0] = spi;
			lcp -> lcd_upper = callee;
			lcp -> lcd_flags = X25_MBS_HOLD;
			error = pk_bind (lcp, nam) || pk_listen (lcp);
			(void) m_free (nam);
		}
		if (error)
			pk_freelcd (lcp);
	}
	return error; /* Hopefully Zero !*/
}

/*
 * Associate a logical channel descriptor with a network.
 * Fill in the default network specific parameters and then
 * set any parameters explicitly specified by the user or
 * by the remote DTE.
 */

pk_assoc (pkp, lcp, sa)
register struct pkcb *pkp;
register struct pklcd *lcp;
register struct sockaddr_x25 *sa;
{

	lcp -> lcd_pkp = pkp;
	lcp -> lcd_packetsize = pkp -> pk_xcp -> xc_psize;
	lcp -> lcd_windowsize = pkp -> pk_xcp -> xc_pwsize;
	lcp -> lcd_rsn = MODULUS - 1;
	pkp -> pk_chan[lcp -> lcd_lcn] = lcp;

	if (sa -> x25_opts.op_psize)
		lcp -> lcd_packetsize = sa -> x25_opts.op_psize;
	else
		sa -> x25_opts.op_psize = lcp -> lcd_packetsize;
	if (sa -> x25_opts.op_wsize)
		lcp -> lcd_windowsize = sa -> x25_opts.op_wsize;
	else
		sa -> x25_opts.op_wsize = lcp -> lcd_windowsize;
	sa -> x25_net = pkp -> pk_xcp -> xc_addr.x25_net;
	lcp -> lcd_flags = sa -> x25_opts.op_flags;
	lcp -> lcd_stime = time.tv_sec;
}

pk_connect (lcp, sa)
register struct pklcd *lcp;
register struct sockaddr_x25 *sa;
{
	register struct pkcb *pkp;

	if (sa -> x25_addr[0] == '\0')
		return (EDESTADDRREQ);
	if (lcp -> lcd_pkp == 0)
	    for (pkp = pkcbhead; ; pkp = pkp -> pk_next) {
		if (pkp == 0)
			return (ENETUNREACH);
		/*
		 * use first net configured (last in list
		 * headed by pkcbhead) if net is zero
		 */
		if (sa -> x25_net == 0 && pkp -> pk_next == 0)
			break;
		if (sa -> x25_net == pkp -> pk_xcp -> xc_addr.x25_net)
			break;
	}

	if (pkp -> pk_state != DTE_READY)
		return (ENETDOWN);
	if ((lcp -> lcd_lcn = pk_getlcn (pkp)) == 0)
		return (EMFILE);
	lcp -> lcd_faddr = *sa;
	lcp -> lcd_ceaddr = & lcp -> lcd_faddr;
	pk_assoc (pkp, lcp, lcp -> lcd_ceaddr);
	if (lcp -> lcd_so)
		soisconnecting (lcp -> lcd_so);
	lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_CALL);
	pk_callrequest (lcp, lcp -> lcd_ceaddr, pkp -> pk_xcp);
	return (*pkp -> pk_start) (lcp);
}

/* 
 *  Build the rest of the CALL REQUEST packet. Fill in calling
 *  address, facilities fields and the user data field.
 */

pk_callrequest (lcp, sa, xcp)
struct pklcd *lcp;
register struct sockaddr_x25 *sa;
register struct x25config *xcp;
{
	register struct x25_calladdr *a;
	register struct mbuf *m = lcp -> lcd_template;
	register struct x25_packet *xp = mtod (m, struct x25_packet *);
	unsigned posn = 0;
	octet *cp;

	if (lcp -> lcd_flags & X25_DBIT)
		xp -> d_bit = 1;
	a = (struct x25_calladdr *) &xp -> packet_data;
	a -> calling_addrlen = strlen (xcp -> xc_addr.x25_addr);
	a -> called_addrlen = strlen (sa -> x25_addr);
	cp = (octet *) a -> address_field;
	to_bcd (&cp, (int)a -> called_addrlen, sa -> x25_addr, &posn);
	to_bcd (&cp, (int)a -> calling_addrlen, xcp -> xc_addr.x25_addr, &posn);
	if (posn & 0x01)
		*cp++ &= 0xf0;
	m -> m_len += cp - (octet *) a;

	if (lcp -> lcd_facilities) {
		m -> m_pkthdr.len += 
			(m -> m_next = lcp -> lcd_facilities) -> m_len;
		lcp -> lcd_facilities = 0;
	} else
		build_facilities (m, sa, (int)xcp -> xc_type);

	m_copyback (m, m -> m_pkthdr.len, sa -> x25_udlen, sa -> x25_udata);
#ifdef ANDREW
	printf ("call: ");
	for (cp = mtod (m, octet *), posn = 0; posn < m -> m_len; ++posn)
		printf ("%x ", *cp++);
	printf ("\n");
#endif
}

static
build_facilities (m, sa, type)
register struct mbuf *m;
struct sockaddr_x25 *sa;
{
	register octet *cp;
	register octet *fcp;
	register int revcharge;

	cp = mtod (m, octet *) + m -> m_len;
	fcp = cp + 1;
	revcharge = sa -> x25_opts.op_flags & X25_REVERSE_CHARGE ? 1 : 0;
	/*
	 * This is specific to Datapac X.25(1976) DTEs.  International
	 * calls must have the "hi priority" bit on.
	 */
	if (type == X25_1976 && sa -> x25_opts.op_psize == X25_PS128)
		revcharge |= 02;
	if (revcharge) {
		*fcp++ = FACILITIES_REVERSE_CHARGE;
		*fcp++ = revcharge;
	}
	switch (type) {
	case X25_1980:
	case X25_1984:
		*fcp++ = FACILITIES_PACKETSIZE;
		*fcp++ = sa -> x25_opts.op_psize;
		*fcp++ = sa -> x25_opts.op_psize;

		*fcp++ = FACILITIES_WINDOWSIZE;
		*fcp++ = sa -> x25_opts.op_wsize;
		*fcp++ = sa -> x25_opts.op_wsize;
	}
	*cp = fcp - cp - 1;
	m -> m_pkthdr.len = (m -> m_len += *cp + 1);
}

to_bcd (a, len, x, posn)
register octet **a;
register char *x;
register int len;
register unsigned *posn;
{
	while (--len >= 0)
		if ((*posn)++ & 0x01)
			*(*a)++ |= *x++ & 0x0F;
		else
			**a = *x++ << 4;
}

/* 
 *  This routine gets the  first available logical channel number.  The
 *  search is from the highest number to lowest number (DTE).
 */

pk_getlcn (pkp)
register struct pkcb *pkp;
{
	register int i;

	if (pkp -> pk_chan == 0)
		return (0);
	for (i = pkp -> pk_maxlcn; i > 0; --i)
		if (pkp -> pk_chan[i] == NULL)
			break;
	return (i);

}

/* 
 *  This procedure sends a CLEAR request packet. The lc state is
 *  set to "SENT_CLEAR". 
 */

pk_clear (lcp, diagnostic, abortive)
register struct pklcd *lcp;
{
	register struct mbuf *m = pk_template (lcp -> lcd_lcn, X25_CLEAR);

	m -> m_len += 2;
	mtod (m, struct x25_packet *) -> packet_data = 0;
	mtod (m, octet *)[4] = diagnostic;
	if (lcp -> lcd_facilities) {
		m -> m_next = lcp -> lcd_facilities;
		m -> m_pkthdr.len += m -> m_next -> m_len;
		lcp -> lcd_facilities = 0;
	}
	if (abortive)
		lcp -> lcd_template = m;
	else {
		struct socket *so = lcp -> lcd_so;
		struct sockbuf *sb = so ? & so -> so_snd : & lcp -> lcd_sb;
		sbappendrecord (sb, m);
	}
	pk_output (lcp);

}

/*
 * This procedure generates RNR's or RR's to inhibit or enable
 * inward data flow, if the current state changes (blocked ==> open or
 * vice versa), or if forced to generate one.  One forces RNR's to ack data.  
 */
pk_flowcontrol (lcp, inhibit, forced)
register struct pklcd *lcp;
{
	inhibit = (inhibit != 0);
	if (lcp == 0 || lcp -> lcd_state != DATA_TRANSFER ||
	    (forced == 0 && lcp -> lcd_rxrnr_condition == inhibit))
		return;
	lcp -> lcd_rxrnr_condition = inhibit;
	lcp -> lcd_template = pk_template (lcp -> lcd_lcn, inhibit ? RNR : RR);
	pk_output (lcp);
}

/* 
 *  This procedure sends a RESET request packet. It re-intializes
 *  virtual circuit.
 */

static
pk_reset (lcp, diagnostic)
register struct pklcd *lcp;
{
	register struct mbuf *m;
	register struct socket *so = lcp -> lcd_so;

	if (lcp -> lcd_state != DATA_TRANSFER)
		return;

	if (so)
		so -> so_error = ECONNRESET;
	lcp -> lcd_reset_condition = TRUE;

	/* Reset all the control variables for the channel. */
	pk_flush (lcp);
	lcp -> lcd_window_condition = lcp -> lcd_rnr_condition =
		lcp -> lcd_intrconf_pending = FALSE;
	lcp -> lcd_rsn = MODULUS - 1;
	lcp -> lcd_ssn = 0;
	lcp -> lcd_output_window = lcp -> lcd_input_window =
		lcp -> lcd_last_transmitted_pr = 0;
	m = lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_RESET);
	m -> m_len += 2;
	mtod (m, struct x25_packet *) -> packet_data = 0;
	mtod (m, octet *)[4] = diagnostic;
	pk_output (lcp);

}

/*
 * This procedure frees all data queued for output or delivery on a
 *  virtual circuit.
 */

pk_flush (lcp)
register struct pklcd *lcp;
{
	register struct socket *so;

	if (lcp -> lcd_template)
		m_freem (lcp -> lcd_template);

	if (lcp -> lcd_cps) {
		m_freem (lcp -> lcd_cps);
		lcp -> lcd_cps = 0;
	}
	if (lcp -> lcd_facilities) {
		m_freem (lcp -> lcd_facilities);
		lcp -> lcd_facilities = 0;
	}
	if (so = lcp -> lcd_so) {
		sbflush (&so -> so_rcv);
		sbflush (&so -> so_snd);
	} else 
		sbflush (&lcp -> lcd_sb);
}

/* 
 *  This procedure handles all local protocol procedure errors.
 */

pk_procerror (error, lcp, errstr, diagnostic)
register struct pklcd *lcp;
char *errstr;
{

	pk_message (lcp -> lcd_lcn, lcp -> lcd_pkp -> pk_xcp, errstr);

	switch (error) {
	case CLEAR: 
		if (lcp -> lcd_so) {
			lcp -> lcd_so -> so_error = ECONNABORTED;
			soisdisconnecting (lcp -> lcd_so);
		}
		pk_clear (lcp, diagnostic, 1);
		break;

	case RESET: 
		pk_reset (lcp, diagnostic);
	}
}

/* 
 *  This procedure is called during the DATA TRANSFER state to check 
 *  and  process  the P(R) values  received  in the DATA,  RR OR RNR
 *  packets.
 */

pk_ack (lcp, pr)
struct pklcd *lcp;
unsigned pr;
{
	register struct socket *so = lcp -> lcd_so;

	if (lcp -> lcd_output_window == pr)
		return (PACKET_OK);
	if (lcp -> lcd_output_window < lcp -> lcd_ssn) {
		if (pr < lcp -> lcd_output_window || pr > lcp -> lcd_ssn) {
			pk_procerror (RESET, lcp,
				"p(r) flow control error", 2);
			return (ERROR_PACKET);
		}
	}
	else {
		if (pr < lcp -> lcd_output_window && pr > lcp -> lcd_ssn) {
			pk_procerror (RESET, lcp,
				"p(r) flow control error #2", 2);
			return (ERROR_PACKET);
		}
	}

	lcp -> lcd_output_window = pr;		/* Rotate window. */
	if (lcp -> lcd_window_condition == TRUE)
		lcp -> lcd_window_condition = FALSE;

	if (so && ((so -> so_snd.sb_flags & SB_WAIT) || so -> so_snd.sb_sel))
		sowwakeup (so);
	if (lcp -> lcd_upper)
		(*lcp -> lcd_upper) (lcp, 0);

	return (PACKET_OK);
}

/* 
 *  This procedure decodes the X.25 level 3 packet returning a 
 *  code to be used in switchs or arrays.
 */

pk_decode (xp)
register struct x25_packet *xp;
{
	register int type;

	if (xp -> fmt_identifier != 1)
		return (INVALID_PACKET);
#ifdef ancient_history
	/* 
	 *  Make sure that the logical channel group number is 0.
	 *  This restriction may be removed at some later date.
	 */
	if (xp -> lc_group_number != 0)
		return (INVALID_PACKET);
#endif
	/* 
	 *  Test for data packet first.
	 */
	if (!(xp -> packet_type & DATA_PACKET_DESIGNATOR))
		return (DATA);

	/* 
	 *  Test if flow control packet (RR or RNR).
	 */
	if (!(xp -> packet_type & RR_OR_RNR_PACKET_DESIGNATOR))
		switch (xp -> packet_type & 0x1f) {
		case X25_RR:
			return (RR);
		case X25_RNR:
			return (RNR);
		case X25_REJECT:
			return (REJECT);
		}

	/* 
	 *  Determine the rest of the packet types.
	 */
	switch (xp -> packet_type) {
	case X25_CALL: 
		type = CALL;
		break;

	case X25_CALL_ACCEPTED: 
		type = CALL_ACCEPTED;
		break;

	case X25_CLEAR: 
		type = CLEAR;
		break;

	case X25_CLEAR_CONFIRM: 
		type = CLEAR_CONF;
		break;

	case X25_INTERRUPT: 
		type = INTERRUPT;
		break;

	case X25_INTERRUPT_CONFIRM: 
		type = INTERRUPT_CONF;
		break;

	case X25_RESET: 
		type = RESET;
		break;

	case X25_RESET_CONFIRM: 
		type = RESET_CONF;
		break;

	case X25_RESTART: 
		type = RESTART;
		break;

	case X25_RESTART_CONFIRM: 
		type = RESTART_CONF;
		break;

	case X25_DIAGNOSTIC:
		type = DIAG_TYPE;
		break;

	default: 
		type = INVALID_PACKET;
	}
	return (type);
}

/* 
 *  A restart packet has been received. Print out the reason
 *  for the restart.
 */

pk_restartcause (pkp, xp)
struct pkcb *pkp;
register struct x25_packet *xp;
{
	register struct x25config *xcp = pkp -> pk_xcp;
	register int lcn = LCN(xp);

	switch (xp -> packet_data) {
	case X25_RESTART_LOCAL_PROCEDURE_ERROR: 
		pk_message (lcn, xcp, "restart: local procedure error");
		break;

	case X25_RESTART_NETWORK_CONGESTION: 
		pk_message (lcn, xcp, "restart: network congestion");
		break;

	case X25_RESTART_NETWORK_OPERATIONAL: 
		pk_message (lcn, xcp, "restart: network operational");
		break;

	default: 
		pk_message (lcn, xcp, "restart: unknown cause");
	}
}

#define MAXRESETCAUSE	7

int     Reset_cause[] = {
	EXRESET, EXROUT, 0, EXRRPE, 0, EXRLPE, 0, EXRNCG
};

/* 
 *  A reset packet has arrived. Return the cause to the user.
 */

pk_resetcause (pkp, xp)
struct pkcb *pkp;
register struct x25_packet *xp;
{
	register struct pklcd *lcp =
				pkp -> pk_chan[LCN(xp)];
	register int code = xp -> packet_data;

	if (code > MAXRESETCAUSE)
		code = 7;	/* EXRNCG */

	pk_message(LCN(xp), lcp -> lcd_pkp, "reset code 0x%x, diagnostic 0x%x",
			xp -> packet_data, 4[(u_char *)xp]);
			
	lcp -> lcd_so -> so_error = Reset_cause[code];
}

#define MAXCLEARCAUSE	25

int     Clear_cause[] = {
	EXCLEAR, EXCBUSY, 0, EXCINV, 0, EXCNCG, 0,
	0, 0, EXCOUT, 0, EXCAB, 0, EXCNOB, 0, 0, 0, EXCRPE,
	0, EXCLPE, 0, 0, 0, 0, 0, EXCRRC
};

/* 
 *  A clear packet has arrived. Return the cause to the user.
 */

pk_clearcause (pkp, xp)
struct pkcb *pkp;
register struct x25_packet *xp;
{
	register struct pklcd *lcp =
		pkp -> pk_chan[LCN(xp)];
	register int code = xp -> packet_data;

	if (code > MAXCLEARCAUSE)
		code = 5;	/* EXRNCG */
	lcp -> lcd_so -> so_error = Clear_cause[code];
}

char *
format_ntn (xcp)
register struct x25config *xcp;
{

	return (xcp -> xc_addr.x25_addr);
}

/* VARARGS1 */
pk_message (lcn, xcp, fmt, a1, a2, a3, a4, a5, a6)
struct x25config *xcp;
char *fmt;
{

	if (lcn)
		if (pkcbhead -> pk_next)
			printf ("X.25(%s): lcn %d: ", format_ntn (xcp), lcn);
		else
			printf ("X.25: lcn %d: ", lcn);
	else
		if (pkcbhead -> pk_next)
			printf ("X.25(%s): ", format_ntn (xcp));
		else
			printf ("X.25: ");

	printf (fmt, a1, a2, a3, a4, a5, a6);
	printf ("\n");
}

pk_ifattach (ia, lloutput, llnext)
register struct x25_ifaddr *ia;
int (*lloutput) ();
caddr_t llnext;
{
	/* this is here because you can't include both pk_var and hd_var */
	/* this will probably be replace by a streams gluing mechanism */
	ia -> ia_pkcb.pk_lloutput = lloutput;
	ia -> ia_pkcb.pk_llnext = llnext;
}

pk_fragment (lcp, m0, qbit, mbit, wait)
struct mbuf *m0;
register struct pklcd *lcp;
{
	register struct mbuf *m = m0;
	register struct x25_packet *xp;
	register struct sockbuf *sb;
	struct mbuf *head = 0, *next, **mp = &head, *m_split ();
	int totlen, psize = 1 << (lcp -> lcd_packetsize);

	if (m == 0)
		return;
	if (m -> m_flags & M_PKTHDR == 0)
		panic ("pk_fragment");
	totlen = m -> m_pkthdr.len;
	m -> m_act = 0;
	sb = lcp -> lcd_so ? &lcp -> lcd_so -> so_snd : & lcp -> lcd_sb;
	do {
		if (totlen > psize) {
			if ((next = m_split (m, psize, wait)) == 0)
				goto abort;
			totlen -= psize;
		} else
			next = 0;
		M_PREPEND(m, PKHEADERLN, wait);
		if (m == 0)
			goto abort;
		*mp = m;
		mp = & m -> m_act;
		*mp = 0;
		xp = mtod (m, struct x25_packet *);
		0[(char *)xp] = 0;
		if (qbit)
			xp -> q_bit = 1;
		if (lcp -> lcd_flags & X25_DBIT)
			xp -> d_bit = 1;
		xp -> fmt_identifier = 1;
		xp -> packet_type = X25_DATA;
		SET_LCN(xp, lcp -> lcd_lcn);
		if (next || (mbit && (totlen == psize ||
				      (lcp -> lcd_flags & X25_DBIT))))
			MBIT(xp) = 1;
	} while (m = next);
	for (m = head; m; m = next) {
		next = m -> m_act;
		m -> m_act = 0;
		sbappendrecord (sb, m);
	}
	return 0;
abort:
	if (wait)
		panic ("pk_fragment null mbuf after wait");
	if (next)
		m_freem (next);
	for (m = head; m; m = next) {
		next = m -> m_act;
		m_freem (m);
	}
	return ENOBUFS;
}

struct mbuf *
m_split (m0, len0, wait)
register struct mbuf *m0;
int len0;
{
	register struct mbuf *m, *n;
	unsigned len = len0;

	for (m = m0; m && len > m -> m_len; m = m -> m_next)
		len -= m -> m_len;
	if (m == 0)
		return (0);
	if (m0 -> m_flags & M_PKTHDR) {
		MGETHDR(n, wait, m0 -> m_type);
		if (n == 0)
			return (0);
		n -> m_pkthdr.rcvif = m0 -> m_pkthdr.rcvif;
		n -> m_pkthdr.len = m0 -> m_pkthdr.len - len0;
		m0 -> m_pkthdr.len = len0;
		if (m -> m_flags & M_EXT)
			goto extpacket;
		if (len > MHLEN) {
			/* m can't be the lead packet */
			MH_ALIGN(n, 0);
			n -> m_next = m_split (m, len, wait);
			if (n -> m_next == 0) {
				(void) m_free (n);
				return (0);
			} else
				return (n);
		} else
			MH_ALIGN(n, len);
	} else if (len == m -> m_len) {
		n = m -> m_next;
		m -> m_next = 0;
		return (n);
	}
extpacket:
	len = m -> m_len - len;		/* remainder to be copied */
	m -> m_len -= len;		/* now equals original len */
	if (m -> m_flags & M_EXT) {
		n -> m_flags |= M_EXT;
		n -> m_ext = m -> m_ext;
		mclrefcnt[mtocl (m -> m_ext.ext_buf)]++;
		n -> m_data = m -> m_data + m -> m_len;
	} else {
		MGET(n, wait, m -> m_type);
		if (n == 0) {
			m -> m_len += len;
			return (0);
		}
		M_ALIGN(n, len);
		bcopy (mtod (m, caddr_t), mtod (n, caddr_t), len);
	}
	n -> m_len = len;
	n -> m_next = m -> m_next;
	m -> m_next = 0;
	return (n);
}
