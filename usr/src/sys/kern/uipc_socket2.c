/*	uipc_socket2.c	4.4	81/11/21	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"

/*
 * Primitive routines for operating on sockets and socket buffers
 */

/*
 * Procedures to manipulate state flags of socket
 * and do appropriate wakeups.
 */
soisconnecting(so)
	struct socket *so;
{

	so->so_state &= ~(SS_ISCONNECTED|SS_ISDISCONNECTING);
	so->so_state |= SS_ISCONNECTING;
	wakeup((caddr_t)&so->so_timeo);
}

soisconnected(so)
	struct socket *so;
{

	so->so_state &= ~(SS_ISCONNECTING|SS_ISDISCONNECTING);
	so->so_state |= SS_ISCONNECTED;
	wakeup((caddr_t)&so->so_timeo);
}

soisdisconnecting(so)
	struct socket *so;
{

	so->so_state &= ~(SS_ISCONNECTED|SS_ISCONNECTING);
	so->so_state |= (SS_ISDISCONNECTING|SS_CANTRCVMORE|SS_CANTSENDMORE);
	wakeup((caddr_t)&so->so_timeo);
}

soisdisconnected(so)
	struct socket *so;
{

	so->so_state &= ~(SS_ISCONNECTING|SS_ISCONNECTED|SS_ISDISCONNECTING);
	so->so_state |= (SS_CANTRCVMORE|SS_CANTSENDMORE);
	wakeup((caddr_t)&so->so_timeo);
	sowwakeup(so);
	sorwakeup(so);
}

socantsendmore(so)
	struct socket *so;
{

	so->so_state |= SS_CANTSENDMORE;
	sowwakeup(so);
}

socantrcvmore(so)
	struct socket *so;
{

	so->so_state |= SS_CANTRCVMORE;
	sorwakeup(so);
}

/*
 * Select a socket.
 */
soselect(so, flag)
	register struct socket *so;
	int flag;
{

	if (flag & FREAD) {
		if (soreadable(so))
			return (1);
		sbselqueue(&so->so_rcv);
	}
	if (flag & FWRITE) {
		if (sowriteable(so))
			return (1);
		sbselqueue(&so->so_snd);
	}
	return (0);
}

/*
 * Queue a process for a select on a socket buffer.
 */
sbselqueue(sb)
	struct sockbuf *sb;
{
	register struct proc *p;

	if ((p = sb->sb_sel) && p->p_wchan == (caddr_t)&selwait)
		sb->sb_flags |= SB_COLL;
	else
		sb->sb_sel = u.u_procp;
}

/*
 * Wait for data to arrive at/drain from a socket buffer.
 */
sbwait(sb)
	struct sockbuf *sb;
{

	sb->sb_flags |= SB_WAIT;
	sleep((caddr_t)&sb->sb_cc, PZERO+1);
}

/*
 * Wakeup processes waiting on a socket buffer.
 */
sbwakeup(sb)
	struct sockbuf *sb;
{

	if (sb->sb_sel) {
		selwakeup(sb->sb_sel, sb->sb_flags & SB_COLL);
		sb->sb_sel = 0;
		sb->sb_flags &= ~SB_COLL;
	}
	if (sb->sb_flags & SB_WAIT) {
		sb->sb_flags &= ~SB_WAIT;
		wakeup((caddr_t)sb->sb_cc);
	}
}

/*
 * Allot mbufs to a sockbuf.
 */
sbreserve(sb, cc)
	struct sockbuf *sb;
{

	if (m_reserve(cc) == 0)
		return (0);
	sb->sb_hiwat = cc;
	sb->sb_mbmax = (cc*2)/MSIZE;
	return (1);
}

/*
 * Free mbufs held by a socket, and reserved mbuf space.
 */
sbrelease(sb)
	struct sockbuf *sb;
{

	sbflush(sb);
	m_release(sb->sb_hiwat);
	sb->sb_hiwat = sb->sb_mbmax = 0;
}

/*
 * Routines to add (at the end) and remove (from the beginning)
 * data from a mbuf queue.
 */

/*
 * Append mbuf queue m to sockbuf sb.
 */
sbappend(sb, m)
	register struct mbuf *m;
	register struct sockbuf *sb;
{
	register struct mbuf **np, *n;

	np = &sb->sb_mb;
	while ((n = *np) && n->m_next)
		np = &n->m_next;
	while (m) {
		if (n && n->m_off <= MMAXOFF && m->m_off <= MMAXOFF &&
		   (int)n->m_act == 0 && (int)m->m_act == 0 &&
		   (n->m_off + n->m_len + m->m_off) <= MMAXOFF) {
			bcopy(mtod(m, caddr_t), mtod(n, caddr_t),
			    (unsigned)m->m_len);
			n->m_len += m->m_len;
			sb->sb_cc += m->m_len;
			m = m_free(m);
			continue;
		}
		sballoc(sb, m);
		*np = m;
		n = m;
		np = &n->m_next;
		m = m->m_next;
	}
}

sbappendaddr(sb, asa, m0)
	struct sockbuf *sb;
	struct sockaddr *asa;
	struct mbuf *m0;
{
	struct sockaddr *msa;
	register struct mbuf *m;
	register int len = sizeof (struct sockaddr);

	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	if (len > sbspace(sb))
		return (0);
	m = m_get(0);
	if (m == 0)
		return (0);
	m->m_off = MMINOFF;
	m->m_len = sizeof (struct sockaddr);
	msa = mtod(m, struct sockaddr *);
	*msa = *asa;
	m->m_act = (struct mbuf *)1;
	sbappend(sb, m);
	m0->m_act = (struct mbuf *)1;
	sbappend(sb, m0);
	return (1);
}

/*
 * Free all mbufs on a sockbuf mbuf chain.
 * Check that resource allocations return to 0.
 */
sbflush(sb)
	struct sockbuf *sb;
{

	if (sb->sb_flags & SB_LOCK)
		panic("sbflush");
	sbdrop(sb, sb->sb_cc);
	if (sb->sb_cc || sb->sb_mbcnt || sb->sb_mb)
		panic("sbflush 2");
}

/*
 * Drop data from (the front of) a sockbuf chain.
 */
sbdrop(sb, len)
	register struct sockbuf *sb;
	register int len;
{
	register struct mbuf *m = sb->sb_mb, *mn;

	while (len > 0) {
		if (m == 0)
			panic("sbdrop");
		if (m->m_len <= len) {
			len -= m->m_len;
			sbfree(sb, m);
			MFREE(m, mn);
			m = mn;
		} else {
			m->m_len -= len;
			m->m_off += len;
			sb->sb_cc -= len;
			break;
		}
	}
	sb->sb_mb = m;
}
