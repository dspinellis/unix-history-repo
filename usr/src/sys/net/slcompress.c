/*
 *	@(#)slcompress.c	7.4 (Berkeley) %G%
 *
 *			THIS CODE IS NOT FOR DISTRIBUTION!
 *	KEEP YOUR GRUBBY HANDS OFF UNLESS AUTHORIZED BY VAN JACOBSON TO COPY!
 *			ASK SAM, MIKE, OR BILL ABOUT IT.
 *
 * Routines to compress and uncompess tcp packets (for transmission
 * over low speed serial lines.
 *
 * Copyright (c) 1988, 1989 by Van Jacobson, Lawrence Berkeley Laboratory
 * All rights reserved.
 */

#include "sl.h"
#if NSL > 0
#ifndef lint
static char rcsid[] = "$Header: slcompress.c,v 1.10 89/06/05 08:28:52 van Exp $";
#endif

#include <sys/types.h>
#include <sys/param.h>
#include <sys/mbuf.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>

#include "slcompress.h"

#ifndef NO_SL_STATS
#define INCR(counter) ++comp->counter;
#else
#define INCR(counter)
#endif

#define BCMP(p1, p2, n) bcmp((char *)(p1), (char *)(p2), (int)(n))
#define BCOPY(p1, p2, n) bcopy((char *)(p1), (char *)(p2), (int)(n))


void
sl_compress_init(comp)
	struct slcompress *comp;
{
	register u_int i;
	register struct cstate *tstate = comp->tstate;

	bzero((char *)comp, sizeof(*comp));
	for (i = MAX_STATES - 1; i > 0; --i) {
		tstate[i].cs_id = i;
		tstate[i].cs_next = &tstate[i - 1];
	}
	tstate[0].cs_next = &tstate[MAX_STATES - 1];
	tstate[0].cs_id = 0;
	comp->last_cs = &tstate[0];
	comp->last_recv = 255;
	comp->last_xmit = 255;
}


/* ENCODE encodes a number that is known to be non-zero.  ENCODEZ
 * checks for zero (since zero has to be encoded in the long, 3 byte
 * form).
 */
#define ENCODE(n) { \
	if ((u_short)(n) >= 256) { \
		*cp++ = 0; \
		cp[1] = (n); \
		cp[0] = (n) >> 8; \
		cp += 2; \
	} else { \
		*cp++ = (n); \
	} \
}
#define ENCODEZ(n) { \
	if ((u_short)(n) >= 256 || (u_short)(n) == 0) { \
		*cp++ = 0; \
		cp[1] = (n); \
		cp[0] = (n) >> 8; \
		cp += 2; \
	} else { \
		*cp++ = (n); \
	} \
}

#define DECODEL(f) { \
	if (*cp == 0) {\
		(f) = htonl(ntohl(f) + ((cp[1] << 8) | cp[2])); \
		cp += 3; \
	} else { \
		(f) = htonl(ntohl(f) + (u_long)*cp++); \
	} \
}

#define DECODES(f) { \
	if (*cp == 0) {\
		(f) = htons(ntohs(f) + ((cp[1] << 8) | cp[2])); \
		cp += 3; \
	} else { \
		(f) = htons(ntohs(f) + (u_long)*cp++); \
	} \
}


u_char
sl_compress_tcp(m, ip, comp)
	struct mbuf *m;
	register struct ip *ip;
	struct slcompress *comp;
{
	register struct cstate *cs = comp->last_cs->cs_next;
	register u_int hlen = ip->ip_hl;
	register struct tcphdr *oth;
	register struct tcphdr *th;
	register u_int deltaS, deltaA;
	register u_int changes = 0;
	u_char new_seq[16];
	register u_char *cp = new_seq;

	/*
	 * Bail if this is an ip fragment or if we don't have
	 * a complete ip & tcp header in the first mbuf.  Otherwise,
	 * check flags to see if this is a packet we might compress 
	 * and, if so, try to locate the connection state.
	 * special case the most recently used connection since
	 * it's most likely to be used again & we don't have to
	 * do any reordering if it's used.
	 */
	if ((ip->ip_off & 0x3fff) || m->m_len < 40)
		return (TYPE_IP);

	th = (struct tcphdr *)&((int *)ip)[hlen];
	if ((th->th_flags & (TH_SYN|TH_FIN|TH_RST|TH_ACK)) != TH_ACK)
		return (TYPE_IP);

	INCR(sls_packets)
	if (ip->ip_src.s_addr != cs->cs_ip.ip_src.s_addr ||
	    ip->ip_dst.s_addr != cs->cs_ip.ip_dst.s_addr ||
	    *(int *)th != ((int *)&cs->cs_ip)[cs->cs_ip.ip_hl]) {
		/*
		 * Wasn't the first -- search for it.
		 *
		 * States are kept in a circularly linked list with
		 * first_cs pointing to the head of the list.  The
		 * list is kept in lru order by moving a state to the
		 * head of the list whenever it is referenced.  Since
		 * the list is short and, empirically, the connection
		 * we want is almost always near the front, we locate
		 * states via linear search.  If we don't find a state
		 * for the datagram, the oldest state is used.
		 */
		register struct cstate *lcs;

		do {
			lcs = cs; cs = cs->cs_next;
			INCR(sls_searches)
			if (*(int *)th == ((int *)&cs->cs_ip)[cs->cs_ip.ip_hl]
			    && ip->ip_src.s_addr == cs->cs_ip.ip_src.s_addr
			    && ip->ip_dst.s_addr == cs->cs_ip.ip_dst.s_addr)
				goto found;
		} while (cs != comp->last_cs);
		INCR(sls_misses)

		/*
		 * Didn't find it -- re-use oldest cstate.
		 * Send an uncompressed packet that tells
		 * the other side what connection number
		 * we're using for this conversation.  Note
		 * that since the state list is circular, the
		 * oldest state points to the newest and we only
		 * need to set last_cs to update the lru linkage.
		 */
		comp->last_cs = lcs;
		hlen += th->th_off;
		hlen <<= 2;
		goto uncompressed;

	found:
		/*
		 * Found it -- move to the front on the connection list.
		 */
		if (comp->last_cs == cs)
			comp->last_cs = lcs;
		else {
			lcs->cs_next = cs->cs_next;
			cs->cs_next = comp->last_cs->cs_next;
			comp->last_cs->cs_next = cs;
		}
	}

	/*
	 * Make sure that only what we expect to change changed.
	 */
	oth = (struct tcphdr *)&((int *)&cs->cs_ip)[hlen];
	deltaS = hlen;
	hlen += th->th_off;
	hlen <<= 2;

	if (((u_short *)ip)[0] != ((u_short *)&cs->cs_ip)[0] ||
	    ((u_short *)ip)[4] != ((u_short *)&cs->cs_ip)[4] ||
	    th->th_off != oth->th_off ||
	    (deltaS > 5 &&
	     BCMP(ip + 1, &cs->cs_ip + 1, (deltaS - 5) << 2)) ||
	    (th->th_off > 5 &&
	     BCMP(th + 1, oth + 1, (th->th_off - 5) << 2)))
		goto uncompressed;

	/*
	 * Figure out which of the changing fields changed.  The
	 * receiver expects changes in the order: urgent, window,
	 * ack, seq (the order minimizes the number of temporaries
	 * needed in this section of code).
	 */
	if (th->th_flags & TH_URG) {
		deltaS = ntohs(th->th_urp);
		ENCODEZ(deltaS);
		changes |= NEW_U;
	} else if (th->th_urp != oth->th_urp)
		/* argh! URG not set but urp changed -- a sensible
		 * implementation should never do this but RFC793
		 * doesn't prohibit the change so we have to deal
		 * with it.  */
		 goto uncompressed;

	if (deltaS = (u_short)(ntohs(th->th_win) - ntohs(oth->th_win))) {
		ENCODE(deltaS);
		changes |= NEW_W;
	}

	if (deltaA = ntohl(th->th_ack) - ntohl(oth->th_ack)) {
		if (deltaA > 0xffff)
			goto uncompressed;
		ENCODE(deltaA);
		changes |= NEW_A;
	}

	if (deltaS = ntohl(th->th_seq) - ntohl(oth->th_seq)) {
		if (deltaS > 0xffff)
			goto uncompressed;
		ENCODE(deltaS);
		changes |= NEW_S;
	}

	switch(changes) {

	case 0:
		if (ip->ip_len != cs->cs_ip.ip_len && ntohs(ip->ip_len) != hlen)
			break;
		/*
		 * Nothing changed and this packet looks like a duplicate
		 * of the last or contains no data -- this is probably a
		 * retransmitted ack or window probe.  Send it
		 * uncompressed in case the other side missed the
		 * compressed version.
		 *
		 * (fall through)
		 */

	case SPECIAL_I:
	case SPECIAL_D:
		/*
		 * actual changes match one of our special case encodings --
		 * send packet uncompressed.
		 */
		goto uncompressed;

	case NEW_S|NEW_A:
		if (deltaS == deltaA &&
		    deltaS == ntohs(cs->cs_ip.ip_len) - hlen) {
			/* special case for echoed terminal traffic */
			changes = SPECIAL_I;
			cp = new_seq;
		}
		break;

	case NEW_S:
		if (deltaS == ntohs(cs->cs_ip.ip_len) - hlen) {
			/* special case for data xfer */
			changes = SPECIAL_D;
			cp = new_seq;
		}
		break;
	}

	deltaS = ntohs(ip->ip_id) - ntohs(cs->cs_ip.ip_id);
	if (deltaS != 1) {
		ENCODEZ(deltaS);
		changes |= NEW_I;
	}
	if (th->th_flags & TH_PUSH)
		changes |= TCP_PUSH_BIT;
	/*
	 * Grab the cksum before we overwrite it below.  Then update our
	 * state with this packet's header.
	 */
	deltaA = ntohs(th->th_sum);
	BCOPY(ip, &cs->cs_ip, hlen);

	/*
	 * We want to use the original packet as our compressed packet.
	 * (cp - new_seq) is the number of bytes we need for compressed
	 * sequence numbers.  In addition we need one byte for the change
	 * mask, one for the connection id and two for the tcp checksum.
	 * So, (cp - new_seq) + 4 bytes of header are needed.  hlen is how
	 * many bytes of the original packet to toss so subtract the two to
	 * get the new packet size.
	 */
	deltaS = cp - new_seq;
	cp = (u_char *)ip;
	if (comp->last_xmit != cs->cs_id) {
		comp->last_xmit = cs->cs_id;
		hlen -= deltaS + 4;
		cp += hlen;
		m->m_len -= hlen;
		m->m_data += hlen;
		*cp++ = changes | NEW_C;
		*cp++ = cs->cs_id;
	} else {
		hlen -= deltaS + 3;
		cp += hlen;
		m->m_len -= hlen;
		m->m_data += hlen;
		*cp++ = changes;
	}
	*cp++ = deltaA >> 8;
	*cp++ = deltaA;
	BCOPY(new_seq, cp, deltaS);
	INCR(sls_compressed)
	return (TYPE_COMPRESSED_TCP);

	/*
	 * Update connection state cs & send uncompressed packet ('uncompressed'
	 * means a regular ip/tcp packet but with the 'conversation id' we hope
	 * to use on future compressed packets in the protocol field).
	 */
uncompressed:
	BCOPY(ip, &cs->cs_ip, hlen);
	ip->ip_p = cs->cs_id;
	comp->last_xmit = cs->cs_id;
	return (TYPE_UNCOMPRESSED_TCP);
}


int
sl_uncompress_tcp(bufp, len, type, comp)
	u_char **bufp;
	int len;
	u_int type;
	struct slcompress *comp;
{
	register u_char *cp;
	register u_int hlen, changes;
	register struct tcphdr *th;
	register struct cstate *cs;
	register struct ip *ip;

	switch (type) {

	case TYPE_UNCOMPRESSED_TCP:
		ip = (struct ip *) *bufp;
		if (ip->ip_p >= MAX_STATES) {
			INCR(sls_errorin)
			return (0);
		}
		cs = &comp->rstate[comp->last_recv = ip->ip_p];
		comp->flags &=~ SLF_TOSS;
		ip->ip_p = IPPROTO_TCP;
		hlen = ip->ip_hl;
		hlen += ((struct tcphdr *)&((int *)ip)[hlen])->th_off;
		hlen <<= 2;
		BCOPY(ip, &cs->cs_ip, hlen);
		cs->cs_ip.ip_sum = 0;
		cs->cs_hlen = hlen;
		INCR(sls_uncompressedin)
		return (len);

	case TYPE_ERROR:
		comp->flags |= SLF_TOSS;
	default:
		INCR(sls_errorin)
		return (0);

	case TYPE_COMPRESSED_TCP:
		break;
	}
	/* We've got a compressed packet. */
	INCR(sls_compressedin)
	cp = *bufp;
	changes = *cp++;
	if (changes & NEW_C) {
		/* Make sure the state index is in range, then grab the state.
		 * If we have a good state index, clear the 'discard' flag. */
		if (*cp >= MAX_STATES) {
			INCR(sls_errorin)
			return (0);
		}
		comp->flags &=~ SLF_TOSS;
		comp->last_recv = *cp++;
	} else {
		/* this packet has an implicit state index.  If we've
		 * had a line error since the last time we got an
		 * explicit state index, we have to toss the packet. */
		if (comp->flags & SLF_TOSS) {
			INCR(sls_tossed)
			return (0);
		}
	}
	cs = &comp->rstate[comp->last_recv];
	hlen = cs->cs_ip.ip_hl << 2;
	th = (struct tcphdr *)&((u_char *)&cs->cs_ip)[hlen];
	th->th_sum = htons((*cp << 8) | cp[1]);
	cp += 2;
	if (changes & TCP_PUSH_BIT)
		th->th_flags |= TH_PUSH;
	else
		th->th_flags &=~ TH_PUSH;

	switch (changes & SPECIALS_MASK) {
	case SPECIAL_I:
		{
		register u_int i = ntohs(cs->cs_ip.ip_len) - cs->cs_hlen;
		th->th_ack = htonl(ntohl(th->th_ack) + i);
		th->th_seq = htonl(ntohl(th->th_seq) + i);
		}
		break;

	case SPECIAL_D:
		th->th_seq = htonl(ntohl(th->th_seq) + ntohs(cs->cs_ip.ip_len)
				   - cs->cs_hlen);
		break;

	default:
		if (changes & NEW_U) {
			th->th_flags |= TH_URG;
			DECODES(th->th_urp)
		} else
			th->th_flags &=~ TH_URG;
		if (changes & NEW_W)
			DECODES(th->th_win)
		if (changes & NEW_A)
			DECODEL(th->th_ack)
		if (changes & NEW_S)
			DECODEL(th->th_seq)
		break;
	}
	if (changes & NEW_I) {
		DECODES(cs->cs_ip.ip_id)
	} else
		cs->cs_ip.ip_id = htons(ntohs(cs->cs_ip.ip_id) + 1);

	/*
	 * At this point, cp points to the first byte of data in the
	 * packet.  If we're not aligned on a 4-byte boundary, copy the
	 * data down so the ip & tcp headers will be aligned.  Then back up
	 * cp by the tcp/ip header length to make room for the reconstructed
	 * header (we assume the packet we were handed has enough space to
	 * prepend 128 bytes of header).  Adjust the lenth to account for
	 * the new header & fill in the IP total length.
	 */
	len -= (cp - *bufp);
	if (len < 0) {
		/* we must have dropped some characters (crc should detect
		 * this but the old slip framing won't) */
		INCR(sls_errorin)
		return (0);
	}
	if ((int)cp & 3) {
		if (len > 0)
			BCOPY(cp, (int)cp &~ 3, len);
		cp = (u_char *)((int)cp &~ 3);
	}
	cp -= cs->cs_hlen;
	len += cs->cs_hlen;
	cs->cs_ip.ip_len = htons(len);
	BCOPY(&cs->cs_ip, cp, cs->cs_hlen);
	*bufp = cp;

	/* recompute the ip header checksum */
	{
		register u_short *bp = (u_short *)cp;
		for (changes = 0; hlen > 0; hlen -= 2)
			changes += *bp++;
		changes = (changes & 0xffff) + (changes >> 16);
		changes = (changes & 0xffff) + (changes >> 16);
		((struct ip *)cp)->ip_sum = ~ changes;
	}
	return (len);
}
#endif
