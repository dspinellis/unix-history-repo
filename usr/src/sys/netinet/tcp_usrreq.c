/* tcp_usrreq.c 1.3 81/10/18 */
#include "../h/param.h"
#include "../bbnnet/net.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/imp.h"
#include "../bbnnet/ucb.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp_pred.h"

lis_cls(wp)             /* passive open (1) */
struct work *wp;
{
	
COUNT(LIS_CLS);
	t_open(wp->w_tcb, PASSIVE);

	return(LISTEN);
}

sys_cls(wp)             /* active open (6) */
register struct work *wp;
{

COUNT(SYS_CLS);
	t_open(wp->w_tcb, ACTIVE);
	send_ctl(wp->w_tcb);            /* send SYN */

	return(SYN_SENT);
}

cls_opn(wp)             /* close request before receiving foreign SYN (10) */
struct work *wp;                           
{

COUNT(CLS_OPN);
	t_close(wp->w_tcb, UCLOSED);

	return(CLOSED);
}

cl2_clw(wp)             /* close request after receiving foreign FIN (13) */
struct work *wp;
{
	register struct tcb *tp;

COUNT(CL2_CLW);
	tp = wp->w_tcb;

	tp->snd_fin = TRUE;             /* send our own FIN */
	send_ctl(tp);                   
	tp->usr_closed = TRUE;

	return(CLOSING2);
}

cls_rwt(wp)             /* rcv request after foreign close (20) */
struct work *wp;
{
	register struct tcb *tp;

COUNT(CLS_RWT);
	tp = wp->w_tcb;

	present_data(tp);       /* present any remaining data */

	if (rcv_empty(tp)) {
        	t_close(tp, UCLOSED);
         	return(CLOSED);
	} else
		return(RCV_WAIT);

}

fw1_syr(wp)             /* close request on synced connection (24,25) */
struct work *wp;
{
	register struct tcb *tp;

COUNT(FW1_SYR);
	tp = wp->w_tcb;

	tp->snd_fin = TRUE;                     /* send FIN */
	send_ctl(tp);
	tp->usr_closed = TRUE;

	return(FIN_W1);
}

sss_snd(wp)             /* send request on open connection (40,41) */
struct work *wp;
{
	register struct tcb *tp;
	register struct mbuf *m, *n;
	register struct ucb *up;
	register off;
	sequence last;

COUNT(SSS_SND);
	tp = wp->w_tcb;
	up = tp->t_ucb;

	last = tp->snd_off;

	/* count number of mbufs in send data */

	for (m = n = (struct mbuf *)wp->w_dat; m != NULL; m = m->m_next) {
		up->uc_ssize++;
		last += m->m_len;
	}

	/* find end of send buffer and append data */

	if ((m = up->uc_sbuf) != NULL) {        /* something in send buffer */
		while (m->m_next != NULL) {             /* find the end */
			m = m->m_next;
			last += m->m_len;
		}
		last += m->m_len;

		/* if there's room in old buffer for new data, consolidate */

		off = m->m_off + m->m_len;
		while (n != NULL && (MSIZE - off) >= n->m_len) {
			bcopy((caddr_t)((int)n + n->m_off), 
			      (caddr_t)((int)m + off), n->m_len);
			m->m_len += n->m_len;
			off += n->m_len;
			up->uc_ssize--;
			n = m_free(n);
		}
		m->m_next = n;

	} else                                  /* nothing in send buffer */
		up->uc_sbuf = n;

	if (up->uc_flags & UEOL) {               /* set EOL */
		tp->snd_end = last;
	}

	if (up->uc_flags & UURG) {              /* urgent data */
		tp->snd_urp = last+1;
		tp->snd_urg = TRUE;
	} 

	send(tp);

	return(SAME);
}

sss_rcv(wp)             /* rcv request on open connection (42) */
struct work *wp;
{
	register struct tcb *tp;

COUNT(SSS_RCV);
	tp = wp->w_tcb;

	send_ctl(tp);                   /* send new window */
	present_data(tp);

	return(SAME);
}

cls_nsy(wp)                  /* abort request on unsynced connection (44) */
struct work *wp;
{

COUNT(CLS_NSY);
	t_close(wp->w_tcb, UABORT);

	return(CLOSED);
}

cls_syn(wp)             /* abort request on synced connection (45) */
struct work *wp;
{
	register struct tcb *tp;

COUNT(CLS_SYN);
	tp = wp->w_tcb;

	tp->snd_rst = TRUE;            /* send reset */
	send_null(tp);
	t_close(tp, UABORT);

	return(CLOSED);
}

cls_act(wp)             /* net closing open connection (47) */
struct work *wp;
{

COUNT(CLS_ACT);
	t_close(wp->w_tcb, UNETDWN);

	return(CLOSED);
}

cls_err(wp)             /* invalid user request in closing states */
struct work *wp;
{
COUNT(CLS_ERR);
	to_user(wp->w_tcb->t_ucb, UCLSERR);

	return(SAME);
}

timers(wp)              /* timer processor (14,17,34,35,36,37,38) */
struct work *wp;
{
	register struct tcb *tp;
	register type;

COUNT(TIMERS);
	tp = wp->w_tcb;
	type = wp->w_stype;

	switch (type) {

	case TINIT:             /* initialization timer */

		if (!tp->syn_acked) {           /* haven't got ACK of our SYN (35) */

			t_close(tp, UINTIMO);
			return(CLOSED);
		}
		break;

	case TFINACK:           /* fin-ack timer */   

		if (tp->t_state == TIME_WAIT) {

			/* can be sure our ACK of for FIN was rcvd,
			   can close if no data left for user */

			if (rcv_empty(tp)) {            /* 14 */
				t_close(tp, UCLOSED);
				return(CLOSED);
			} else                          /* 17 */
				return(RCV_WAIT);

		} else if (tp->t_state == CLOSING1)     /* 37 */

			/* safe to close */

			tp->waited_2_ml = TRUE;

	        break;

	case TREXMT:            /* retransmission timer */

		/* set up for a retransmission, increase rexmt time
		   in case of multiple retransmissions. */

	        if (tp->t_rexmt_val > tp->snd_una) {   /* 34 */
	        	tp->snd_nxt = tp->snd_una;
	        	tp->rexmt = TRUE;
			tp->t_xmtime = tp->t_xmtime << 1;                  
        		if (tp->t_xmtime > T_REMAX)
        			tp->t_xmtime = T_REMAX;
	        	send(tp);
	        }
		break;

	case TREXMTTL:          /* retransmit too long */

		/* tell user */

        	if (tp->t_rtl_val > tp->snd_una)        /* 36 */
        		to_user(tp->t_ucb, URXTIMO);

		/* if user has already closed, abort the connection */

		if (tp->usr_closed) {
			t_close(tp, URXTIMO);
			return(CLOSED);
		}
		break;

	case TPERSIST:          /* persist timer */

		/* force a byte send through closed window */

        	tp->force_one = TRUE;                   /* 38 */
        	send(tp);
		break;
	}

	return(SAME);
}
