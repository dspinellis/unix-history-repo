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
 * Some things done here could obviously be done in a better way,
 * but they were done this way to minimize the number of files
 * that had to be changed to accomodate this device.
 * A lot of this code belongs in the tty driver.
 *
 * Pounded on heavily by Chris Torek (chris@mimsy.umd.edu, umcp-cs!chris).
 * N.B.: this belongs in netinet, not vaxif, the way it stands now.
 *
 * Converted to 4.3BSD Beta by Chris Torek.
 */

/* $Header: if_sl.c,v 1.12 85/12/20 21:54:55 chris Exp $ */
/* from if_sl.c,v 1.11 84/10/04 12:54:47 rick Exp */

#include "sl.h"
#if NSL > 0

#include "param.h"
#include "mbuf.h"
#include "buf.h"
#include "socket.h"
#include "ioctl.h"
#include "tty.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif vax

/*
 * N.B.: SLMTU is now a hard limit on input packet size.  Some limit
 * is required, lest we use up all mbufs in the case of deleterious data
 * dribbling down the line.
 */
#define	SLMTU	1006

struct sl_softc {
	struct	ifnet sc_if;	/* network-visible interface */
	short	sc_flags;	/* see below */
	short	sc_ilen;	/* length of input-packet-so-far */
	struct	tty *sc_ttyp;	/* pointer to tty structure */
	char	*sc_mp;		/* pointer to next available buf char */
	char	sc_buf[SLMTU];	/* input buffer */
} sl_softc[NSL];

/* flags */
#define	SC_ESCAPED	0x0001	/* saw a FRAME_ESCAPE */
#define	SC_OACTIVE	0x0002	/* output tty is active */

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
slopen(dev, tp)
	dev_t dev;
	register struct tty *tp;
{
	register struct sl_softc *sc;
	register int nsl;

	if (tp->t_sc != NULL)
		return (EBUSY);

	for (nsl = 0, sc = sl_softc; nsl < NSL; nsl++, sc++)
		if (sc->sc_ttyp == NULL) {
			sc->sc_flags = 0;
			sc->sc_ilen = 0;
			sc->sc_mp = sc->sc_buf;
			tp->t_sc = (caddr_t)sc;
			sc->sc_ttyp = tp;
			return (0);
		}

	return (ENOSPC);
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
	}
	splx(s);
}

/*
 * Line specific (tty) ioctl routine.
 * Provide a way to get the sl unit number.
 */
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
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		m_freem(m);
		sc->sc_if.if_collisions++;
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if ((sc->sc_flags & SC_OACTIVE) == 0) {
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
	register int c, len;
	register u_char *mcp;
	int flush;

	/*
	 * If there is more in the output queue, just send it now.
	 * We are being called in lieu of ttstart and must do what
	 * it would.
	 */
	if (tp->t_outq.c_cc > 0) {
		ttstart(tp);
		return;
	}

	/*
	 * This happens briefly when the line shuts down.
	 */
	if (sc == NULL)
		return;

	/*
	 * Get a packet and map it to the interface.
	 */
	c = splimp();
	IF_DEQUEUE(&sc->sc_if.if_snd, m);
	if (m == NULL) {
		sc->sc_flags &= ~SC_OACTIVE;
		splx(c);
		return;
	}
	flush = !(sc->sc_flags & SC_OACTIVE);
	sc->sc_flags |= SC_OACTIVE;
	splx(c);

	/*
	 * The extra FRAME_END will start up a new packet, and thus
	 * will flush any accumulated garbage.  We do this whenever
	 * the line may have been idle for some time.
	 */
	if (flush)
		(void) putc(FRAME_END, &tp->t_outq);

	while (m != NULL) {
		len = m->m_len;
		mcp = mtod(m, u_char *);
		while (--len >= 0) {
			c = *mcp++;
			if (c == FRAME_ESCAPE || c == FRAME_END) {
				if (putc(FRAME_ESCAPE, &tp->t_outq))
					goto full;
				c = c == FRAME_ESCAPE ? TRANS_FRAME_ESCAPE :
							TRANS_FRAME_END;
				if (putc(c, &tp->t_outq)) {
					(void) unputc(&tp->t_outq);
					goto full;
				}
			} else
				if (putc(c, &tp->t_outq))
					goto full;
		}
		m = m_free(m);
	}

	if (putc(FRAME_END, &tp->t_outq)) {
full:
		/*
		 * If you get many oerrors (more than one or two a day)
		 * you probably do not have enough clists and you should 
		 * increase "nclist" in param.c.
		 */
		(void) unputc(&tp->t_outq);	/* make room */
		putc(FRAME_END, &tp->t_outq);	/* end the packet */
		sc->sc_if.if_oerrors++;
	} else
		sc->sc_if.if_opackets++;

	/*
	 * Start transmission.  Note that slstart, not ttstart, will be
	 * called when the transmission completes, be that after a single
	 * piece of what we have mapped, or be it after the entire thing
	 * has been sent.  That is why we need to check the output queue
	 * count at the top.
	 */
	ttstart(tp);
}

/*
 * Copy data buffer to mbuf chain; add ifnet pointer ifp.
 */
struct mbuf *
sl_btom(addr, len, ifp)
	register caddr_t addr;
	register int len;
	struct ifnet *ifp;
{
	register struct mbuf *m, **mp;
	register int count;
	struct mbuf *top = NULL;

	mp = &top;
	while (len > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if ((*mp = m) == NULL) {
			m_freem(top);
			return (NULL);
		}
		if (ifp) {
			m->m_off += sizeof(ifp);
			count = MIN(len, MLEN - sizeof(ifp));
		} else {
			if (len >= NBPG) {
				struct mbuf *p;

				MCLGET(p, 1);
				if (p != NULL) {
					count = MIN(len, CLBYTES);
					m->m_off = (int)p - (int)m;
				} else
					count = MIN(len, MLEN);
			} else
				count = MIN(len, MLEN);
		}
		bcopy(addr, mtod(m, caddr_t), count);
		m->m_len = count;
		if (ifp) {
			m->m_off -= sizeof(ifp);
			m->m_len += sizeof(ifp);
			*mtod(m, struct ifnet **) = ifp;
			ifp = NULL;
		}
		addr += count;
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
			sc->sc_mp = sc->sc_buf;
			sc->sc_ilen = 0;
			return;
		}
	} else {
		switch (c) {

		case FRAME_END:
			if (sc->sc_ilen == 0)	/* ignore */
				return;
			m = sl_btom(sc->sc_buf, sc->sc_ilen, &sc->sc_if);
			if (m == NULL) {
				sc->sc_if.if_ierrors++;
				return;
			}
			sc->sc_mp = sc->sc_buf;
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
	if (++sc->sc_ilen >= SLMTU) {
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
