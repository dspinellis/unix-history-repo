/*	nsp_usrreq.c	1.5	82/12/18	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../netdecnet/decnet.h"
#include "../netdecnet/dn_systm.h"
#include "../net/if.h"
#include "../netdecnet/nsp.h"
#include "../netdecnet/nsp_var.h"
#include <errno.h>

/*
 * NSP protocol interface to socket abstraction.
 */
struct	nspcb *nsp_newnspcb();

/*
 * Process an NSP user request for NSP np.  If this is a send request
 * then m is the mbuf chain of send data.  If this is a timer expiration
 * (called from the software clock routine), then timertype tells which timer.
 */
nsp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	register struct nspcb *np = sotonspcb(so);
	int s = splnet();
	int error = 0;
	int ostate;
COUNT(NSP_USRREQ);

	/*
	 * When an NSP is attached to a socket, then there will be
	 * a (struct nspcb) pointed at by the socket.
	 * The normal sequence of events is:
	 *	PRU_ATTACH		creating these structures
	 *	PRU_CONNECT		connecting to a remote peer
	 *	(PRU_SEND|PRU_RCVD)*	exchanging data
	 *	PRU_DISCONNECT		disconnecting from remote peer
	 *	PRU_DETACH		deleting the structures
	 * With the operations from PRU_CONNECT through PRU_DISCONNECT
	 * possible repeated several times.
	 *
	 * MULTIPLE CONNECTS ARE NOT YET IMPLEMENTED.
	 */
	if (np == 0 && req != PRU_ATTACH) {
		splx(s);
		return (EINVAL);		/* XXX */
	}
	if (np) {
		ostate = np->n_state;
	}
	switch (req) {

	/*
	 * NSP attaches to socket via PRU_ATTACH, reserving space
	 * and NSP control block.
	 **** If the socket is to receive connections,
	 **** then the LISTEN state is entered.
	 */
	case PRU_ATTACH:
		if (np) {
			error = EISCONN;
			break;
		}
		error = nsp_attach(so, (struct sockaddr *)addr);
		if (error)
			break;
		np = sotonspcb(so);
		break;

	/*
	 * PRU_DETACH detaches the NSP protocol from the socket.
	 * If the protocol state is non-embryonic, then can't
	 * do this directly: have to initiate a PRU_DISCONNECT,
	 * which may finish later; embryonic nspcb's can just
	 * be discarded here.
	 */
	case PRU_DETACH:
		if (np->n_state != NS_O && np->n_state != NS_CL
		    && np->n_state != NS_LI)
			nsp_disconnect(np, <reason>);
		else {
			nsp_close(np);
			np = 0;
		}
		break;

	/*
	 * Initiate connection to peer.
	 * Enter CI state, and mark socket as connecting.
	 **** Start keep-alive timer, and seed output sequence space.
	 **** Send initial segment on connection.
	 */
	case PRU_CONNECT:
		error = dn_pcbconnect(np, (struct sockaddr_dn *)addr);
		if (error)
			break;
		soisconnecting(so);
		nsp_connect(np);
		break;

	/*
	 * Initiate disconnect from peer.
	 * If connection never passed embryonic stage, just drop;
	 * else if don't need to let data drain, then can just drop anyways,
	 * else have to begin NSP shutdown process: mark socket disconnecting,
	 * drain unread data, state switch to reflect user close, and
	 * send segment (e.g. DI) to peer.  Socket will be really disconnected
	 * when peer sends DC to ack our DI.
	 *
	 * SHOULD IMPLEMENT LATER PRU_CONNECT VIA REALLOC NSPCB.
	 */
	case PRU_DISCONNECT:
		nsp_disconnect(np);
		break;

	/*
	 * Accept a connection.  Essentially all the work is
	 * done at higher levels; just return the address
	 * of the peer, storing through addr.
	 */
	case PRU_ACCEPT:
		dn_pcbconnaddr(np, (struct sockaddr *)addr);
		break;

/*** BEGIN NOT MODIFIED FOR NSP ***/
	/*
	 * Mark the connection as being incapable of further output.
	 */
	case PRU_SHUTDOWN:
		socantsendmore(so);
		nsp_usrclosed(np);
		(void) nsp_output(np);
		break;

	/*
	 * After a receive, possibly send window update to peer.
	 */
	case PRU_RCVD:
		(void) nsp_output(np);
		break;
/*** END NOT MODIFIED FOR NSP ***/

	/*
	 * Do a send by putting data in output queue and
	 * calling output processor.
	 */
	case PRU_SEND:
		sbpappend(&so->so_snd, m);
		(void) nsp_output(np);
		break;

/*** BEGIN NOT MODIFIED FOR NSP ***/
	/*
	 * Abort the NSP.
	 */
	case PRU_ABORT:
		nsp_drop(np, ECONNABORTED);
		break;

/* SOME AS YET UNIMPLEMENTED HOOKS */
	case PRU_CONTROL:
		error = EOPNOTSUPP;
		break;

	case PRU_SENSE:
		error = EOPNOTSUPP;
		break;
/* END UNIMPLEMENTED HOOKS */

	case PRU_RCVOOB:
		if (so->so_oobmark == 0 &&
		    (so->so_state & SS_RCVATMARK) == 0) {
			error = EINVAL;
			break;
		}
		if ((np->n_flags & NSP_RCVINTR) == 0) {
			error = EWOULDBLOCK;
			break;
		}
		/* RETURN THE DATA */
		break;

	case PRU_SENDOOB:
		/*
		if interrupt data present return error (can't queue)
		if len > 16 return error
		put in xmt mbuf
		mark interrupt data available
		call nsp_output
		*/
		break;

	/*
	 * NSP slow timer went off; going through this
	 * routine for tracing's sake.
	 */
	case PRU_SLOWTIMO:
		nsp_timers(np, (int)addr);
		req |= (int)addr << 8;		/* for debug's sake */
		break;
/*** END NOT MODIFIED FOR NSP ***/

	default:
		panic("nsp_usrreq");
	}
	if (np && (so->so_options & SO_DEBUG))
		nsp_trace(NA_USER, ostate, np, (struct XXXXXXXX *)0, req);
	splx(s);
	return (error);
}

/*
 * Attach NSP protocol to socket, allocating NSP control block,
 * bufer space, and entering LISTEN state if to accept connections.
 */
nsp_attach(so, sa)
	struct socket *so;
	struct sockaddr *sa;
{
	register struct nspcb *np;
	struct sockaddr_dn *sdn = (struct sockaddr_dn *)sa;
	struct mbuf *m;
	int error;

	if (sdn) {
		if (sdn->sdn_family != AF_DECNET)
			return (EAFNOSUPPORT);
		/* the user has specified a sockaddr with a socreate.
		all this can do is allow the user to specify an object
		type or other info if he is going to wait for a connection.
		figure this out later. */
	} else {
		/* nothing specified, will expect a connect request soon */
	}
	m = m_getclr(MT_CANTWAIT, MT_PCB);
	if (m == 0)
		return (ENOBUFS);
	if (sbreserve(&so->so_snd, 1024) == 0) {
bad:
		(void) m_free(m);
		return (ENOBUFS);
	}
	if (sbreserve(&so->so_rcv, 1024) == 0) {
		sbrelease(&so->so_snd);
		goto bad;
	}
	np = mtod(m, struct nspcb *);
	np->n_head = &ncb;
	insque(np, &ncb);
	sp->so_pcb = (caddr_t)np;
	sdn = (struct sockaddr_dn *)&so->so_addr;
	sdn->sdn_family == AF_DECNET;
	sdn->sdn_addr = WHAT ELSE NEEDS TO BE FILLED IN HERE?
	if (so->so_options & SO_ACCEPTCONN) {
		np->n_state = NS_LI;
	} else
		np->n_state = NS_O;
	return (0);
}

/*** BEGIN NOT MODIFIED FOR NSP ***/
/*
 * Initiate (or continue) disconnect.
 * If embryonic state, just send reset (once).
 * If not in ``let data drain'' option, just drop.
 * Otherwise (hard), mark socket disconnecting and drop
 * current input data; switch states based on user close, and
 * send segment to peer (with FIN).
 */
nsp_disconnect(np)
	struct nspcb *np;
{
	struct socket *so = np->n_socket;

	if (np->n_state < NSPS_ESTABLISHED)
		nsp_close(np);
	else if (so->so_linger == 0)
		nsp_drop(np, 0);
	else {
		soisdisconnecting(so);
		sbflush(&so->so_rcv);
		nsp_usrclosed(np);
		(void) nsp_output(np);
	}
}

/*
 * User issued close, and wish to trail through shutdown states:
 * if never received SYN, just forget it.  If got a SYN from peer,
 * but haven't sent FIN, then go to FIN_WAIT_1 state to send peer a FIN.
 * If already got a FIN from peer, then almost done; go to LAST_ACK
 * state.  In all other cases, have already sent FIN to peer (e.g.
 * after PRU_SHUTDOWN), and just have to play tedious game waiting
 * for peer to send FIN or not respond to keep-alives, etc.
 */
nsp_usrclosed(np)
	struct nspcb *np;
{

	switch (np->n_state) {

	case NSPS_LISTEN:
	case NSPS_SYN_SENT:
		np->n_state = NSPS_CLOSED;
		nsp_close(np);
		break;

	case NSPS_SYN_RECEIVED:
	case NSPS_ESTABLISHED:
		np->n_state = NSPS_FIN_WAIT_1;
		break;

	case NSPS_CLOSE_WAIT:
		np->n_state = NSPS_LAST_ACK;
		break;
	}
}
