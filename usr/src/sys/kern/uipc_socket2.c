/*	uipc_socket2.c	4.1	81/11/15	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/mbuf.h"
#include "../h/protocol.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/inaddr.h"
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

	if ((p = sb->sb_sel) && p->p_wchan == (caddr_t)select)
		sb->sb_flags |= SB_COLL;
	else
		sb->sb_sel = u.u_procp;
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
	sb->sb_cc = cc;
	sb->sb_mbcnt = (cc*2)/MSIZE;
}

/*
 * Free mbufs held by a socket, and reserved mbuf space.
 */
sbrelease(sb)
	struct sockbuf *sb;
{

	sbflush(sb);
	m_release(sb->sb_cc);
	sb->sb_cc = sb->sb_mbcnt = 0;
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
			bcopy(mtod(m, caddr_t), mtod(n, caddr_t), m->m_len);
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

struct mbuf *
sb_copy(sb, off, len)
	struct sockbuf *sb;
	int off;
	register int len;
{
	register struct mbuf *m, *n, **np;
	struct mbuf *top, *p;
COUNT(SB_COPY);

	if (len == 0)
		return (0);
	if (off < 0 || len < 0)
		panic("sb_copy");
	m = sb->sb_mb;
	while (off > 0) {
		if (m == 0)
			panic("sb_copy");
		if (off < m->m_len)
			break;
		off -= m->m_len;
		m = m->m_next;
	}
	np = &top;
	top = 0;
	while (len > 0) {
		MGET(n, 1);
		*np = n;
		if (n == 0)
			goto nospace;
		if (m == 0)
			panic("sb_copy");
		n->m_len = MIN(len, m->m_len - off);
		if (m->m_off > MMAXOFF) {
			p = mtod(m, struct mbuf *);
			n->m_off = ((int)p - (int)n) + off;
			mprefcnt[mtopf(p)]++;
		} else {
			n->m_off = MMINOFF;
			bcopy(mtod(m, caddr_t)+off, mtod(n, caddr_t),
			    n->m_len);
		}
		len -= n->m_len;
		off = 0;
		m = m->m_next;
		np = &n->m_next;
	}
	return (top);
nospace:
	printf("snd_copy: no space\n");
	m_freem(top);
	return (0);
}
