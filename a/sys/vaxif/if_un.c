/*	if_un.c	4.27	82/12/18	*/

#include "un.h"
#if NUN > 0
/*
 * Ungermann-Bass network/DR11-W interface driver
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/vmmac.h"
#include <errno.h>
#include <time.h>
#include "../h/kernel.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vaxif/if_un.h"
#include "../vaxif/if_unreg.h"
#include "../vaxif/if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#define	UNMTU		(600-sizeof (struct un_header))

#define	US_NULL		0	/* not doing anything state */
#define	US_IDLE 	1	/* waiting to transfer state */
#define	US_READ		2	/* reading state */
#define	US_WRITE	3	/* writing state */
#define	US_RESET	4	/* waiting for reset state */

int	unprobe(), unattach(), unintr();
struct	uba_device *uninfo[NUN];
u_short	unstd[] = { 0 };
struct	uba_driver undriver =
	{ unprobe, 0, unattach, 0, unstd, "un", uninfo };
#define	UNUNIT(dev)	(minor(dev))

int	uninit(), unoutput(), unreset();
int	unrestart();

/*
 * Ungermann-Bass software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * us_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address,
 * etc.  We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct un_softc {
	struct	ifnet us_if;		/* network-visible interface */
	struct	ifuba us_ifuba;		/* UNIBUS resources */
	short	us_state;		/* device state */
	short	us_errcnt;		/* number of errors since time set */
	short	us_restart;		/* restart interval */
	u_char	us_maxtime;		/* interval for error counting */
	u_char	us_maxerr;		/* errors allowed in interval */
	time_t	us_errtime;		/* time for error counting */
} un_softc[NUN];

/*
 * Cause an interrupt to determine interface presence and
 * interrupt vector.
 */
unprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct undevice *addr = (struct undevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	unintr(0);
#endif
	addr->csr = IE|UNRESET;
	addr->csr = IE|UNRESET|GO;
	DELAY(100000);
	addr->csr = 0;
#ifdef ECHACK
	br = 0x16;
#endif
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
unattach(ui)
	struct uba_device *ui;
{
	register struct un_softc *us = &un_softc[ui->ui_unit];
	struct sockaddr_in *sin;

	us->us_if.if_unit = ui->ui_unit;
	us->us_if.if_name = "un";
	us->us_if.if_mtu = UNMTU;
	us->us_if.if_net = ui->ui_flags;
	sin = (struct sockaddr_in *)&us->us_if.if_addr;
	sin->sin_family = AF_INET;
	/* host number will be filled in later. */
	sin->sin_addr = if_makeaddr(us->us_if.if_net, 0);
	sin = (struct sockaddr_in *)&us->us_if.if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(us->us_if.if_net, INADDR_ANY);
	us->us_if.if_flags |= IFF_BROADCAST;
	us->us_if.if_init = uninit;
	us->us_if.if_output = unoutput;
	us->us_if.if_reset = unreset;
	us->us_if.if_watchdog = unrestart;
	us->us_maxtime = 3;
	us->us_maxerr = 10;
	us->us_restart = 5 * 60;
	us->us_ifuba.ifu_flags = UBA_CANTWAIT;
#ifdef notdef
	us->us_ifuba.ifu_flags |= UBA_NEEDBDP;
#endif
	if_attach(&us->us_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
unreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NUN || (ui = uninfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" un%d", unit);
	uninit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
uninit(unit)
	int unit;
{
	register struct un_softc *us = &un_softc[unit];
	register struct uba_device *ui = uninfo[unit];
	register struct undevice *addr;
	int s;

	if (if_ubainit(&us->us_ifuba, ui->ui_ubanum,
	    sizeof (struct un_header), (int)btoc(UNMTU)) == 0) {
		printf("un%d: can't initialize\n", unit);
		us->us_if.if_flags &= ~IFF_UP;
		return;
	}
	us->us_errcnt = 0;
	us->us_errtime = time.tv_sec;
	unwhoami(unit);

	/*
	 * Reset U-B interface, thus causing an interrupt which
	 * will start things going.
	 */
	addr = (struct undevice *)ui->ui_addr;
	s = splimp();
	addr->csr = IE|UNRESET;
	addr->csr = IE|UNRESET|GO;
	us->us_state = US_RESET;
	splx(s);
}

/*
 * Try to start a write operation.
 * If interface is busy, it must be in idle state, so issue a reset.
 * Otherwise, get the datagram from the output queue, map it onto
 * the UNIBUS, and start the write.  This routine should not be
 * called if the output queue is empty.
 */
unstart(dev)
	dev_t dev;
{
	int unit = UNUNIT(dev);
	struct uba_device *ui = uninfo[unit];
	register struct un_softc *us = &un_softc[unit];
	register struct undevice *addr = (struct undevice *)ui->ui_addr;
	struct mbuf *m;
	int dataaddr, datalen;
	register short cmdcsr;

	if (us->us_state != US_NULL) {
		addr->csr = IE|UNRESET;
		addr->csr = IE|UNRESET|GO;
		us->us_state = US_RESET;
	} else {
		IF_DEQUEUE(&us->us_if.if_snd, m);
		if (m == 0)
			return;
		us->us_state = US_WRITE;
		datalen = if_wubaput(&us->us_ifuba, m);
		if (us->us_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(us->us_ifuba.ifu_uba,
				us->us_ifuba.ifu_w.ifrw_bdp);
		dataaddr = us->us_ifuba.ifu_w.ifrw_info;
		addr->bar = dataaddr & 0xffff;
		addr->wcr = -(((datalen + 1) >> 1) + 1);
		cmdcsr = ((dataaddr >> 12) & 0x30) | IE | UNOUT;
		addr->csr = cmdcsr;
		addr->csr = cmdcsr | GO;
	}
}

/*
 * Ungermann-Bass interface interrupt handler.
 * Determines reason for interrupt and acts accordingly.
 */
unintr(unit)
	int unit;
{
	register struct un_softc *us = &un_softc[unit];
	struct undevice *addr = (struct undevice *)uninfo[unit]->ui_addr;
	register struct un_header *un;
	struct mbuf *m;
	int len;
	register struct ifqueue *inq;
	int cmdcsr;

	if ((addr->dar & RESETACK) && us->us_state != US_RESET) {
		if ((us->us_if.if_flags & IFF_UP) == 0)
			return;
		printf("un%d: unexpected reset\n", unit);
		unerror(unit);
	}
		
	switch (us->us_state) {

	case US_NULL:
		printf("un%d: stray interrupt\n", unit);
		break;

	case US_RESET:
		if (!(addr->dar & RESETACK)) {
			addr->csr = IE|UNRESET;
			addr->csr = IE|UNRESET|GO;
			return;
		}
		break;

	case US_IDLE:
		break;

	case US_READ:
		us->us_if.if_ipackets++;
		if (us->us_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(us->us_ifuba.ifu_uba,
				us->us_ifuba.ifu_r.ifrw_bdp);
		if (addr->csr & STATA) {
			if ((us->us_if.if_flags & IFF_UP) == 0)
				return;
			printf("un%d: input error csr=%b\n", unit,
				addr->csr&0xffff, UNBITS);
			us->us_if.if_ierrors++;
			unerror(unit);
			break;
		}
		un = (struct un_header *)(us->us_ifuba.ifu_r.ifrw_addr);
		switch (un->un_ptype) {
#ifdef INET
		case UNTYPE_IP:
			len = htons((u_short)((struct ip *) (un+1))->ip_len);
			schednetisr(NETISR_IP);
			inq = &ipintrq;
			break;
#endif
		case UNTYPE_INQUIRE: {
			struct sockaddr_in *sin;

			us->us_if.if_host[0] =
			    un->un_dport << 16 | htons(un->un_dniu);
			sin = (struct sockaddr_in *)&us->us_if.if_addr;
			sin->sin_addr = if_makeaddr(us->us_if.if_net,
				us->us_if.if_host[0]);
			us->us_if.if_flags |= IFF_UP;
			if_rtinit(&us->us_if, RTF_UP);
			goto setup;
		}

		default:
			printf("un%d: bad packet type %d\n", un->un_ptype);
			goto setup;
		}

		m = if_rubaget(&us->us_ifuba, len, 0);
		if (m != 0)
			if (IF_QFULL(inq)) {
				IF_DROP(inq);
				m_freem(m);
			} else
				IF_ENQUEUE(inq, m);
		break;

	case US_WRITE:
		us->us_if.if_opackets++;
		if (addr->csr & STATA) {
			if ((us->us_if.if_flags & IFF_UP) == 0)
				return;
			printf("un%d: output error csr=%b\n",
			    unit, addr->csr, UNBITS);
			us->us_if.if_oerrors++;
			unerror(unit);
		}
		if (us->us_ifuba.ifu_xtofree) {
			m_freem(us->us_ifuba.ifu_xtofree);
			us->us_ifuba.ifu_xtofree = 0;
		}
		break;

	default:
		printf("un%d: invalid state %d csr=%b\n",
		    us->us_state, addr->csr, UNBITS);
	}

setup:
	us->us_state = US_NULL;
	if (addr->csr & STATB) {
		us->us_state = US_READ;
		addr->wcr = -((sizeof (struct un_header) + UNMTU + 1)/2+1);
		addr->bar = us->us_ifuba.ifu_r.ifrw_info & 0xffff;
		cmdcsr = ((us->us_ifuba.ifu_r.ifrw_info >> 12) & 0x30);
		cmdcsr |= IE|UNRDDG;
		addr->csr = cmdcsr;
		addr->csr = cmdcsr | GO;
	} else if (us->us_if.if_snd.ifq_head != 0 && (addr->csr & STATC))
		unstart(unit);
	
	if (us->us_state == US_NULL) {
		us->us_state = US_IDLE;
		addr->csr = IE|UNIDLE;
		addr->csr = IE|UNIDLE|GO;
	}
}

/*
 * Ungermann-Bass output routine.
 * Encapsulate a packet destined for dst for the local net.
 */
unoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, destniu, destport, len;
	register struct mbuf *m = m0;
	register struct un_header *un;
	register struct un_softc *us = &un_softc[ifp->if_unit];
	int s;

	if ((us->us_if.if_flags & IFF_UP) == 0)
		return (ENETDOWN);
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET: {
		struct sockaddr_in *sin = (struct sockaddr_in *)dst;
		struct ip *ip = mtod(m, struct ip *);

		if (sin->sin_addr.s_addr & 0xffffff00) {
			destniu = sin->sin_addr.s_addr >> 24;
			destport = (sin->sin_addr.s_addr >> 8) & 0xff;
		} else {
			destniu = 0xffff;
			destport = 0xff;
		}
		len = htons((u_short) ip->ip_len);
		type = UNTYPE_IP;
		break;
	}
#endif
	default:
		printf("un%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		m_freem(m0);
		return (EAFNOSUPPORT);
	}
	
	/*
	 * Add local net header.  If no space in first mbuf,
	 * allocate another.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof (struct un_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m_freem(m0);
			return (ENOBUFS);
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct un_header);
	} else {
		m->m_off -= sizeof (struct un_header);
		m->m_len += sizeof (struct un_header);
	}
	un = mtod(m, struct un_header *);
	bzero((caddr_t)un, sizeof (struct un_header));
	un->un_length = htons((u_short)(len + sizeof (struct un_header)));
	un->un_dniu = htons((u_short)destniu);
	un->un_dport = destport;
	un->un_dtype = 5;
	un->un_sniu = htons((u_short)(ifp->if_host[0] >> 24));
	un->un_sport = (ifp->if_host[0] >> 8) & 0xff;
	un->un_stype = 5;
	un->un_ptype = type;

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		m_freem(m);
		splx(s);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if (us->us_state == US_IDLE)
		unstart(ifp->if_unit);
	splx(s);
	return (0);
}

/*
 * U-B error handler, if maxerr errors have occured
 * in maxtime seconds, disable the interface.
 */
unerror(unit)
	int unit;
{
	register struct un_softc *us = &un_softc[unit];
	struct undevice *addr = (struct undevice *)uninfo[unit]->ui_addr;

	if (time.tv_sec - us->us_errtime > us->us_maxtime) {
		us->us_errtime = time.tv_sec;
		us->us_errcnt = 1;
	} else if (++us->us_errcnt >= us->us_maxerr) {
		printf("un%d: error limit exceeded\n", unit);
		us->us_if.if_flags &= ~IFF_UP;
		addr->csr = 0;
		us->us_if.if_timer = us->us_restart;
	}
}

unrestart(unit)
	int unit;
{
	register struct un_softc *us = &un_softc[unit];
	struct undevice *addr = (struct undevice *)uninfo[unit]->ui_addr;
	int s;

	us->us_if.if_flags |= IFF_UP;
	printf("un%d: restarting\n", unit);
	unwhoami(unit);
	s = splimp();
	addr->csr = IE|UNRESET;
	addr->csr = IE|UNRESET|GO;
	us->us_state = US_RESET;
	splx(s);
}

/*
 * Send a "Who am I?" message to the interface. 
 * Interface should respond with an copy of the
 * packet with its real address filled in.  The
 * message is placed at the head of the output queue.
 * An interface reset should be done next to start
 * things rolling.
 */
unwhoami(unit)             
	int unit;
{
	register struct mbuf *m;
	register struct un_softc *us = &un_softc[unit];
	register struct un_header *un;
	int s;

	if ((m = m_get(M_DONTWAIT, MT_HEADER)) == 0) 
		return;
	m->m_off = MMINOFF;
	m->m_len = sizeof(struct un_header);
	un = mtod(m, struct un_header *);
	bzero((caddr_t)un, sizeof (struct un_header));
	un->un_length = htons(sizeof (struct un_header));
	un->un_dtype = un->un_stype = 5;
	un->un_ptype = UNTYPE_INQUIRE;
	s = splimp();
	IF_PREPEND(&us->us_if.if_snd, m);
	splx(s);
}
