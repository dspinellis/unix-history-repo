/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_sl.c	7.15 (Berkeley) %G%
 */

/*
 * Serial Line interface
 *
 * Rick Adams
 * Center for Seismic Studies
 * 1300 N 17th Street, Suite 1450
 * Arlington, Virginia 22209
 * (703)276-7900
 * rick@seismo.ARPA
 * seismo!rick
 *
 * Pounded on heavily by Chris Torek (chris@mimsy.umd.edu, umcp-cs!chris).
 * N.B.: this belongs in netinet, not net, the way it stands now.
 * Should have a link-layer type designation, but wouldn't be
 * backwards-compatible.
 *
 * Converted to 4.3BSD Beta by Chris Torek.
 * Other changes made at Berkeley, based in part on code by Kirk Smith.
 * W. Jolitz, added slip abort & time domain window
 * also added Van Jacobson's hdr compression code
 */

/* $Header: if_sl.c,v 1.12 85/12/20 21:54:55 chris Exp $ */
/* from if_sl.c,v 1.11 84/10/04 12:54:47 rick Exp */

#include "sl.h"
#if NSL > 0

#include "param.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "buf.h"
#include "dk.h"
#include "socket.h"
#include "ioctl.h"
#include "file.h"
#include "tty.h"
#include "kernel.h"
#include "conf.h"
#include "errno.h"

#include "if.h"
#include "netisr.h"
#include "route.h"
#if INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "slcompress.h"
#endif
#include "if_slvar.h"

#include "machine/mtpr.h"

/*
 * N.B.: SLMTU is now a hard limit on input packet size.
 * SLMTU must be <= MCLBYTES - sizeof(struct ifnet *).
 */
#define	SLMTU	1006
#define	SLIP_HIWAT	1000	/* don't start a new packet if HIWAT on queue */
#define	CLISTRESERVE	1000	/* Can't let clists get too low */

/*
 * SLIP ABORT ESCAPE MECHANISM:
 *	(inspired by HAYES modem escape arrangement)
 *	1sec escape 1sec escape 1sec escape { 1sec escape 1sec escape }
 *	signals a "soft" exit from slip mode by usermode process
 *	(hard exit unimplemented -- currently system dependant)
 */

#define	ABT_ESC		'\033'	/* can't be t_intr - distant host must know it*/
#define ABT_WAIT	1	/* in seconds - idle before an escape & after */
#define ABT_RECYCLE	(5*2+2)	/* in seconds - time window processing abort */

/* a "soft" abort means to pass a suggestion to user code to abort slip */
#define ABT_SOFT	3	/* count of escapes */

/* a "hard" abort means to force abort slip within the kernel -- process jam? */
#define ABT_HARD	5	/* count of escapes */

/*
 * SLIP TIME WINDOW:
 *	Only accept packets with octets that come at least this often.
 *	With non-reliable but fast modems (FAX, Packet Radio), we assume that
 *	packets come in groups (time domain), and that fractional groups that
 *	come erratically are just noise that will foul subsequent packets.
 *	We reject them on a time filter basis.
 *
 *	This is a very coarse filter, because error correcting modems like the
 *	telebit take there own sweet time encoding/decoding packets. If you
 *	are using an MNP,PEP or other such arrangement, this won't help much.
 *	If you are using packet radio, use the millisecond time val with
 *	as small a resolution as possible. In any case, the coarse filter
 *	saves noisey lines about 50 % of the time.
 */

#define	TIME_WINDOW	2	/* max seconds between valid packet chars */

struct sl_softc	 sl_softc[NSL];

#define FRAME_END	 	0300		/* Frame End */
#define FRAME_ESCAPE		0333		/* Frame Esc */
#define TRANS_FRAME_END	 	0334		/* transposed frame end */
#define TRANS_FRAME_ESCAPE 	0335		/* transposed frame esc */

#define t_sc T_LINEP

int sloutput(), slioctl(), ttrstrt();

/*
 * Called from boot code to establish sl interfaces.
 */
slattach()
{
	register struct sl_softc *sc;
	register int i = 0;

	for (sc = sl_softc; i < NSL; sc++) {
		sc->sc_if.if_name = "sl";
		sc->sc_if.if_unit = i++;
		sc->sc_if.if_mtu = SLMTU;
		sc->sc_if.if_flags = IFF_POINTOPOINT;
		sc->sc_if.if_ioctl = slioctl;
		sc->sc_if.if_output = sloutput;
		sc->sc_if.if_snd.ifq_maxlen = IFQ_MAXLEN;
		if_attach(&sc->sc_if);
	}
}

/*
 * Line specific open routine.
 * Attach the given tty to the first available sl unit.
 */
/* ARGSUSED */
slopen(dev, tp)
	dev_t dev;
	register struct tty *tp;
{
	register struct sl_softc *sc;
	register int nsl;
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	if (tp->t_line == SLIPDISC)
		return (EBUSY);

	for (nsl = 0, sc = sl_softc; nsl < NSL; nsl++, sc++)
		if (sc->sc_ttyp == NULL) {
			sc->sc_flags = 0;
			sc->sc_ilen = 0;
			if (slinit(sc) == 0)
				return (ENOBUFS);
#ifdef INET
			sl_compress_init(&sc->sc_comp);
#endif
			tp->t_sc = (caddr_t)sc;
			sc->sc_ttyp = tp;
			ttyflush(tp, FREAD | FWRITE);
			return (0);
		}

	return (ENXIO);
}

/*
 * Line specific close routine.
 * Detach the tty from the sl unit.
 * Mimics part of ttyclose().
 */
slclose(tp)
	struct tty *tp;
{
	register struct sl_softc *sc;
	int s;

	ttywflush(tp);
	tp->t_line = 0;
	s = splimp();		/* paranoid; splnet probably ok */
	sc = (struct sl_softc *)tp->t_sc;
	if (sc != NULL) {
		if_down(&sc->sc_if);
		sc->sc_ttyp = NULL;
		tp->t_sc = NULL;
		MCLFREE((struct mbuf *)sc->sc_buf);
		sc->sc_buf = 0;
		sc->sc_mp = (char *) 4; /*XXX!?! */
	}
	splx(s);
}

/*
 * Line specific (tty) ioctl routine.
 * Provide a way to get the sl unit number.
 */
/* ARGSUSED */
sltioctl(tp, cmd, data, flag)
	struct tty *tp;
	caddr_t data;
{
	struct sl_softc *sc = (struct sl_softc *)tp->t_sc;
	int s;

	switch (cmd) {
	case TIOCGETD:
		*(int *)data = sc->sc_if.if_unit;
		break;
	case TIOCMGET:
		if (tp->t_state&TS_CARR_ON)
			*(int *)data = TIOCM_CAR ;
		else	*(int *)data = 0 ;

		if (sc->sc_flags&SC_ABORT)
			*(int *)data |= TIOCM_DTR ;
		break;
	case SLIOCGFLAGS:
		*(int *)data = sc->sc_flags;
		break;
	case SLIOCSFLAGS:
#define	SC_MASK	(SC_COMPRESS|SC_NOICMP)
		s = splimp();
		sc->sc_flags =
		    (sc->sc_flags &~ SC_MASK) | ((*(int *)data) & SC_MASK);
		splx(s);
		break;
	default:
		return (-1);
	}
	return (0);
}

/*
 * Queue a packet.  Start transmission if not active.
 */
sloutput(ifp, m, dst)
	register struct ifnet *ifp;
	register struct mbuf *m;
	struct sockaddr *dst;
{
	register struct sl_softc *sc;
	int s;

	/*
	 * `Cannot happen' (see slioctl).  Someday we will extend
	 * the line protocol to support other address families.
	 */
	if (dst->sa_family != AF_INET) {
		printf("sl%d: af%d not supported\n", ifp->if_unit,
			dst->sa_family);
		m_freem(m);
		return (EAFNOSUPPORT);
	}

	sc = &sl_softc[ifp->if_unit];
	if (sc->sc_ttyp == NULL) {
		m_freem(m);
		return (ENETDOWN);	/* sort of */
	}
	if ((sc->sc_ttyp->t_state & TS_CARR_ON) == 0) {
		m_freem(m);
		return (EHOSTUNREACH);
	}
	s = splimp();
#ifdef INET
	if (sc->sc_flags & (SC_COMPRESS|SC_NOICMP)) {
		register struct ip *ip = mtod(m, struct ip *);
		if (ip->ip_p == IPPROTO_TCP) {
			/* add stuff to TOS routing */
			if (sc->sc_flags & SC_COMPRESS)
				(void) sl_compress_tcp(m, ip, &sc->sc_comp);
		} else if ((sc->sc_flags & SC_NOICMP) &&
		    ip->ip_p == IPPROTO_ICMP) {
			m_freem(m);
			splx(s);
			return (0);
		}
	}
#endif
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		m_freem(m);
		sc->sc_if.if_oerrors++;
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if (sc->sc_ttyp->t_outq.c_cc == 0) {
		splx(s);
		slstart(sc->sc_ttyp);
	} else
		splx(s);
	return (0);
}

/*
 * Start output on interface.  Get another datagram
 * to send from the interface queue and map it to
 * the interface before starting output.
 */
slstart(tp)
	register struct tty *tp;
{
	register struct sl_softc *sc = (struct sl_softc *)tp->t_sc;
	register struct mbuf *m;
	register int len;
	register u_char *cp;
	int nd, np, n, s;
	struct mbuf *m2;
	extern int cfreecount;

	for (;;) {
		/*
		 * If there is more in the output queue, just send it now.
		 * We are being called in lieu of ttstart and must do what
		 * it would.
		 */
		if (tp->t_outq.c_cc > 0)
			ttstart(tp);
		if (tp->t_outq.c_cc > SLIP_HIWAT)
			return;

		/*
		 * This happens briefly when the line shuts down.
		 */
		if (sc == NULL)
			return;

		/*
		 * If system is getting low on clists
		 * and we have something running already, stop here.
		 */
		if (cfreecount < CLISTRESERVE + SLMTU && tp->t_outq.c_cc)
			return;

		/*
		 * Get a packet and send it to the interface.
		 */
		s = splimp();
		IF_DEQUEUE(&sc->sc_if.if_snd, m);
		splx(s);
		if (m == NULL)
			return;

		/*
		 * The extra FRAME_END will start up a new packet, and thus
		 * will flush any accumulated garbage.  We do this whenever
		 * the line may have been idle for some time.
		 */
		if (tp->t_outq.c_cc == 0)
			(void) putc(FRAME_END, &tp->t_outq);

		while (m) {
			cp = mtod(m, u_char *);
			len = m->m_len;
			while (len > 0) {
				/*
				 * Find out how many bytes in the string we can
				 * handle without doing something special.
				 */
				nd = locc(FRAME_ESCAPE, len, cp);
				np = locc(FRAME_END, len, cp);
				n = len - MAX(nd, np);
				if (n) {
					/*
					 * Put n characters at once
					 * into the tty output queue.
					 */
					if (b_to_q((char *)cp, n, &tp->t_outq))
						break;
					len -= n;
					cp += n;
				}
				/*
				 * If there are characters left in the mbuf,
				 * the first one must be special..
				 * Put it out in a different form.
				 */
				if (len) {
					if (putc(FRAME_ESCAPE, &tp->t_outq))
						break;
					if (putc(*cp == FRAME_ESCAPE ?
					   TRANS_FRAME_ESCAPE : TRANS_FRAME_END,
					   &tp->t_outq)) {
						(void) unputc(&tp->t_outq);
						break;
					}
					cp++;
					len--;
				}
			}
			MFREE(m, m2);
			m = m2;
		}

		if (putc(FRAME_END, &tp->t_outq)) {
			/*
			 * Not enough room.  Remove a char to make room
			 * and end the packet normally.
			 * If you get many collisions (more than one or two
			 * a day) you probably do not have enough clists
			 * and you should increase "nclist" in param.c.
			 */
			(void) unputc(&tp->t_outq);
			(void) putc(FRAME_END, &tp->t_outq);
			sc->sc_if.if_collisions++;
		} else
			sc->sc_if.if_opackets++;
	}
}

slinit(sc)
	register struct sl_softc *sc;
{
	register caddr_t p;

	if (sc->sc_buf == (char *) 0) {
		MCLALLOC(p, M_WAIT);
		if (p) {
			sc->sc_buf = p;
			sc->sc_mp = p;
		} else {
			printf("sl%d: can't allocate buffer\n", sc - sl_softc);
			sc->sc_if.if_flags &= ~IFF_UP;
			return (0);
		}
	}
	return (1);
}

/*
 * Copy data buffer to mbuf chain; add ifnet pointer ifp.
 */
struct mbuf *
sl_btom(sc, len, ifp)
	struct sl_softc *sc;
	register int len;
	struct ifnet *ifp;
{
	register caddr_t cp;
	register struct mbuf *m, **mp;
	register unsigned count;
	struct mbuf *top = NULL;

	cp = sc->sc_buf;
	mp = &top;

	while (len > 0) {
		if (top == NULL) {
			MGETHDR(m, M_DONTWAIT, MT_DATA);
		} else {
			MGET(m, M_DONTWAIT, MT_DATA);
		}
		if (m == NULL) {
			m_freem(top);
			return (NULL);
		}
		if (top == NULL) {
			m->m_pkthdr.rcvif = ifp;
			m->m_pkthdr.len = len;
			m->m_len = MHLEN;
		} else
			m->m_len = MLEN;
		*mp = m;
		/*
		 * If we have at least MINCLSIZE bytes,
		 * allocate a new page.  Swap the current
		 * buffer page with the new one.
		 */
		if (len >= MINCLSIZE) {
			MCLGET(m, M_DONTWAIT);
			if (m->m_flags & M_EXT) {
				cp = mtod(m, char *);
				m->m_data = sc->sc_buf;
				sc->sc_buf = cp;
				count = MIN(len, MCLBYTES);
				goto nocopy;
			}
		}
		count = MIN(len, m->m_len);
		bcopy(cp, mtod(m, caddr_t), count);
nocopy:
		m->m_len = count;
		cp += count;
		len -= count;
		mp = &m->m_next;
	}
	return (top);
}

/*
 * tty interface receiver interrupt.
 */
slinput(c, tp)
	register int c;
	register struct tty *tp;
{
	register struct sl_softc *sc;
	register struct mbuf *m;
	int s;

	tk_nin++;
	sc = (struct sl_softc *)tp->t_sc;
	if (sc == NULL)
		return;
	if (!(tp->t_state&TS_CARR_ON))	/*XXX*/
		return;

	c &= 0xff;

	/* if we see an abort after "idle" time, count it */
	if ((c&0x7f) == ABT_ESC && time.tv_sec >= sc->sc_lasttime + ABT_WAIT) {
		sc->sc_abortcount++;
		/* record when the first abort escape arrived */
		if (sc->sc_abortcount == 1) sc->sc_starttime = time.tv_sec;
	}

	/* if we have an abort, see that we have not run out of time, or
	   that we have an "idle" time after the complete escape sequence */
	if (sc->sc_abortcount) {
		if (time.tv_sec >= sc->sc_starttime + ABT_RECYCLE)
			sc->sc_abortcount = 0;
		if (sc->sc_abortcount >= ABT_SOFT
		&& time.tv_sec >= sc->sc_lasttime + ABT_WAIT)
			sc->sc_flags |= SC_ABORT;
	}

	if (sc->sc_ilen && time.tv_sec >= sc->sc_lasttime + TIME_WINDOW) {
		sc->sc_flags &= ~SC_ESCAPED;
		sc->sc_mp = sc->sc_buf;
		sc->sc_ilen = 0;
		sc->sc_if.if_ierrors++;
		return;
	}

	sc->sc_lasttime = time.tv_sec;

	if (sc->sc_flags & SC_ESCAPED) {
		sc->sc_flags &= ~SC_ESCAPED;
		switch (c) {

		case TRANS_FRAME_ESCAPE:
			c = FRAME_ESCAPE;
			break;

		case TRANS_FRAME_END:
			c = FRAME_END;
			break;

		default:
			sc->sc_if.if_ierrors++;
			sc->sc_mp = sc->sc_buf;
			sc->sc_ilen = 0;
			return;
		}
	} else {
		switch (c) {

		case FRAME_END:
			if (sc->sc_ilen == 0)	/* ignore */
				return;
			m = sl_btom(sc, sc->sc_ilen, &sc->sc_if);
			sc->sc_mp = sc->sc_buf;
			sc->sc_ilen = 0;
			if (m == NULL) {
				sc->sc_if.if_ierrors++;
				return;
			}
			sc->sc_if.if_ipackets++;
#ifdef INET
			{ u_char type = *mtod(m, u_char *);
			  if (!(m = sl_uncompress_tcp(m, type&0xf0, &sc->sc_comp)))
				return;
			}
#endif
			s = splimp();
			if (IF_QFULL(&ipintrq)) {
				IF_DROP(&ipintrq);
				sc->sc_if.if_ierrors++;
				m_freem(m);
			} else {
				IF_ENQUEUE(&ipintrq, m);
				schednetisr(NETISR_IP);
			}
			splx(s);
			return;

		case FRAME_ESCAPE:
			sc->sc_flags |= SC_ESCAPED;
			return;
		}
	}
	if (++sc->sc_ilen > SLMTU) {
		sc->sc_if.if_ierrors++;
		sc->sc_mp = sc->sc_buf;
		sc->sc_ilen = 0;
		return;
	}
	*sc->sc_mp++ = c;
}

/*
 * Process an ioctl request.
 */
slioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifa->ifa_addr->sa_family == AF_INET)
			ifp->if_flags |= IFF_UP;
		else
			error = EAFNOSUPPORT;
		break;

	case SIOCSIFDSTADDR:
		if (ifa->ifa_addr->sa_family != AF_INET)
			error = EAFNOSUPPORT;
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
#endif
