/*	uipc_mu_msg.c	Melb 4.3	82/12/28	*/

#ifdef	MUSH
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"

/*
 * Melbourne Trivial IPC mechanism
 *
 *	This is intended solely to serve for the 4.1bsd MUSH implementation
 *	until a better IPC scheme arrives from somewhere
 *
 *	If it happens to be useful for other purposes, OK, but users
 *	should be prepared to change at short (no) notice.
 *	This is purposely kept as small as possible, with the expectation
 *	than anything done using this scheme should be able to be
 *	done easily in any other, with possibly a little more legwork.
 *
 *	NB: we don't go fooling with spl's in here, as it is deliberately
 *	not intended that interrupt level code will ever call this
 */

mu_msg()
{
	register struct proc *p, *pp;
	mmsgbuf mb;
	register struct a {
		msg_type	cmd;
		int		wait;	/* bit mask - see mu_msg.h */
		mmsgbuf	*	msgp;
	} *uap;

	uap = (struct a *)u.u_ap;
	p = u.u_procp;

	for (;;) {	/* only loop if cmd == MSG_SNDW */
		switch (uap->cmd) {

		case MSG_ENAB:
			p->p_msgflgs |= MSGENAB;
			return;

		case MSG_DISB:
			p->p_msgflgs &= ~MSGENAB;
				/*
				 * Q: what should be done with a pending msg
				 * - for now we will just leave it, proc should
				 * do a MSG_RECV w/o waiting after MSG_DISB
				 */
			return;

		case MSG_RECV:
			while (!p->p_mb.msg_val && (uap->wait & MSG_W_RCV)) {
				p->p_msgflgs |= MSGOK;
				sleep((caddr_t) &p->p_mb, MSGPRI);
			}
			u.u_error = copyout((caddr_t)&p->p_mb,
				(caddr_t)uap->msgp, sizeof(mmsgbuf));
			p->p_msgflgs &= ~(MSGOK|MSGWRPLY);
			if (p->p_mb.msg_rply)
				p->p_msgflgs |= MSGRPLY;
			else {
				p->p_mb.msg_val = 0;
				if (p->p_msgflgs & MSGWAIT) {
					p->p_msgflgs &= ~MSGWAIT;
					wakeup((caddr_t)&p->p_mb);
				}
			}
			return;

		case MSG_SEND:
		case MSG_SNDW:
		case MSG_RPLY:
			u.u_error = copyin((caddr_t)uap->msgp, (caddr_t)&mb,
			    sizeof(mmsgbuf));
			if (u.u_error)
				return;
			if (uap->cmd == MSG_RPLY) {
				if (!(p->p_msgflgs & MSGRPLY) ||
				    mb.msg_pid != p->p_mb.msg_pid) {
					u.u_error = EINVAL;
					return;
				}
				p->p_mb.msg_val = 0;
				if (!mb.msg_rply && p->p_msgflgs & MSGWAIT) {
					p->p_msgflgs &= ~MSGWAIT;
					wakeup((caddr_t)&p->p_mb);
				}
			} else {
				if (p->p_msgflgs & MSGRPLY) {
					while (pp = mu_send(&p->p_mb,
					    (int)p->p_mb.msg_pid, 0)) {
						pp->p_msgflgs |= MSGWAIT;
						sleep((caddr_t)&pp->p_mb,
						    MSGPRI);
					}
					u.u_error = 0; /* not err if exited */
					p->p_mb.msg_val = 0;
					if (!mb.msg_rply &&
					    p->p_msgflgs & MSGWAIT) {
						p->p_msgflgs &= ~MSGWAIT;
						wakeup((caddr_t)&p->p_mb);
					}
				}
			}
			p->p_msgflgs &= ~MSGRPLY;
			if (mb.msg_rply) {
				if (p->p_mb.msg_val) {
					u.u_error = ENOSPC;
					return;
				}
				p->p_mb = mb;
				p->p_mb.msg_rply = 0;
				p->p_mb.msg_val = 0;
				p->p_msgflgs |= MSGWRPLY;
			}
			mb.msg_uid = u.u_uid;
			while ((pp = mu_send(&mb, (int)mb.msg_pid, p->p_pid)) &&
			    uap->wait & MSG_W_POST) {
				pp->p_msgflgs |= MSGWAIT;
				sleep((caddr_t)&pp->p_mb, MSGPRI);
			}
			if (pp)
				u.u_error = EBUSY;
			if (u.u_error) {
				if (mb.msg_rply)
					p->p_msgflgs &= ~MSGWRPLY;
				return;
			}
			if (uap->cmd == MSG_SNDW) {
				uap->cmd = MSG_RECV;
				break;
			}
			return;
		
		default:
			u.u_error = EINVAL;
			return;
		}
	}
}

struct proc *
mu_send(mp, pid, from)
	register mmsgbuf *mp;
	register int pid, from;
{
	register struct proc *p;

	p = pfind(pid);

	if (p == NULL || p->p_stat == SZOMB ||
	    !(p->p_msgflgs & (MSGOK|MSGENAB|MSGWRPLY))) {
		u.u_error = ESRCH;
		return((struct proc *)0);
	}

	if (p->p_mb.msg_val ||
	    from != 0 && p->p_msgflgs&MSGWRPLY && from != p->p_mb.msg_pid)
		return(p);
	
	p->p_mb = *mp;
	p->p_mb.msg_val = 1;
	p->p_mb.msg_pid = from;
	if (from == 0)
		p->p_mb.msg_rply = 0;
	if (p->p_msgflgs & MSGOK)
		wakeup((caddr_t)&p->p_mb);
	else if (p->p_msgflgs & MSGENAB)
		psignal(p, SIGMESG);
	return((struct proc *)0);
}

msgto(pid, data)
int pid;
DATA_T data;
{
	register struct proc *p;
	mmsgbuf mb;

	mb.msg_uid = 0;		/* all msgs from system are from root */
	mb.msg_data = data;
	mb.msg_rply = 0;
	while (p = mu_send(&mb, pid, u.u_procp->p_pid)) {
		p->p_msgflgs |= MSGWAIT;
		sleep((caddr_t)&p->p_mb, MSGPRI);
	}
}
#endif
