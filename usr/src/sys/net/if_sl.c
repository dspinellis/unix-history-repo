/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)if_sl.c	7.7 (Berkeley) %G%
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
 */

/* $Header: if_sl.c,v 1.12 85/12/20 21:54:55 chris Exp $ */
/* from if_sl.c,v 1.11 84/10/04 12:54:47 rick Exp */

#include "sl.h"
#if NSL > 0

#include "param.h"
#include "mbuf.h"
#include "buf.h"
#include "dk.h"
#include "socket.h"
#include "ioctl.h"
#include "file.h"
#include "tty.h"
#include "errno.h"

#include "if.h"
#include "netisr.h"
#include "route.h"
#if INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#endif

#include "../machine/mtpr.h"

/*
 * N.B.: SLMTU is now a hard limit on input packet size.
 * SLMTU must be <= MCLBYTES - sizeof(struct ifnet *).
 */
#define	SLMTU	1006
#define	SLIP_HIWAT	1000	/* don't start a new packet if HIWAT on queue */
#define	CLISTRESERVE	1000	/* Can't let clists get too low */

struct sl_softc {
	struct	ifnet sc_if;	/* network-visible interface */
	short	sc_flags;	/* see below */
	short	sc_ilen;	/* length of input-packet-so-far */
	struct	tty *sc_ttyp;	/* pointer to tty structure */
	char	*sc_mp;		/* pointer to next available buf char */
	char	*sc_buf;	/* input buffer */
} sl_softc[NSL];

/* flags */
#define	SC_ESCAPED	0x0001	/* saw a FRAME_ESCAPE */

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

	if (!suser())
		return (EPERM);
	if (tp->t_line == SLIPDISC)
		return (EBUSY);

	for (nsl = 0, sc = sl_softc; nsl < NSL; nsl++, sc++)
		if (sc->sc_ttyp == NULL) {
			sc->sc_flags = 0;
			sc->sc_ilen = 0;
			if (slinit(sc) == 0)
				return (ENOBUFS);
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

	if (cmd == TIOCGETD) {
		*(int *)data = ((struct sl_softc *)tp->t_sc)->sc_if.if_unit;
		return (0);
	}
	return (-1);
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
		if (cfreecount < CLISTRESERVE + SLMTU && tp->t_outq.c_cc == 0)
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
	struct mbuf *p;

	if (sc->sc_buf == (char *) 0) {
		MCLALLOC(p, 1);
		if (p) {
			sc->sc_buf = (char *)p;
			sc->sc_mp = sc->sc_buf + sizeof(struct ifnet *);
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

	cp = sc->sc_buf + sizeof(struct ifnet *);
	mp = &top;
	while (len > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if ((*mp = m) == NULL) {
			m_freem(top);
			return (NULL);
		}
		if (ifp)
			m->m_off += sizeof(ifp);
		/*
		 * If we have at least NBPG bytes,
		 * allocate a new page.  Swap the current buffer page
		 * with the new one.  We depend on having a space
		 * left at the beginning of the buffer
		 * for the interface pointer.
		 */
		if (len >= NBPG) {
			MCLGET(m);
			if (m->m_len == MCLBYTES) {
				cp = mtod(m, char *);
				m->m_off = (int)sc->sc_buf - (int)m;
				sc->sc_buf = cp;
				if (ifp) {
					m->m_off += sizeof(ifp);
					count = MIN(len,
					    MCLBYTES - sizeof(struct ifnet *));
				} else
					count = MIN(len, MCLBYTES);
				goto nocopy;
			}
		}
		if (ifp)
			count = MIN(len, MLEN - sizeof(ifp));
		else
			count = MIN(len, MLEN);
		bcopy(cp, mtod(m, caddr_t), count);
nocopy:
		m->m_len = count;
		if (ifp) {
			m->m_off -= sizeof(ifp);
			m->m_len += sizeof(ifp);
			*mtod(m, struct ifnet **) = ifp;
			ifp = NULL;
		}
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

	c &= 0xff;
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
			sc->sc_mp = sc->sc_buf + sizeof(struct ifnet *);
			sc->sc_ilen = 0;
			return;
		}
	} else {
		switch (c) {

		case FRAME_END:
			if (sc->sc_ilen == 0)	/* ignore */
				return;
			m = sl_btom(sc, sc->sc_ilen, &sc->sc_if);
			if (m == NULL) {
				sc->sc_if.if_ierrors++;
				return;
			}
			sc->sc_mp = sc->sc_buf + sizeof(struct ifnet *);
			sc->sc_ilen = 0;
			sc->sc_if.if_ipackets++;
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
		sc->sc_mp = sc->sc_buf + sizeof(struct ifnet *);
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
		if (ifa->ifa_addr.sa_family == AF_INET)
			ifp->if_flags |= IFF_UP;
		else
			error = EAFNOSUPPORT;
		break;

	case SIOCSIFDSTADDR:
		if (ifa->ifa_addr.sa_family != AF_INET)
			error = EAFNOSUPPORT;
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
#endif
