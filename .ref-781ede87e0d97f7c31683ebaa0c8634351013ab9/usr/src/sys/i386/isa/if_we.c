/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tim L. Tucker
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)if_we.c	5.4 (Berkeley) %G%
 */

/*
 * Modification history
 *
 * 8/28/89 - Initial version(if_wd.c), Tim L Tucker
 */
 
#include "we.h"
#if	NWE > 0
/*
 * Western Digital 8003 ethernet/starlan adapter
 *
 * Supports the following interface cards:
 * WD8003E, WD8003EBT, WD8003S, WD8003SBT, WD8013EBT
 *
 * The Western Digital card is one of many AT/MCA ethernet interfaces
 * based on the National DS8390 Network Interface chip set.
 */
#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/netisr.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/if_ether.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#include "machine/isa/if_wereg.h"
#include "machine/isa/isa_device.h"
 
/*
 * This constant should really be 60 because the we adds 4 bytes of crc.
 * However when set to 60 our packets are ignored by deuna's , 3coms are
 * okay ??????????????????????????????????????????
 */
#define ETHER_MIN_LEN 64
#define	ETHER_ADDR_LEN 6
#define ETHER_HDR_SIZE 14
 
/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * qe_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	we_softc {
	struct	arpcom we_ac;		/* Ethernet common part 	*/
#define	we_if	we_ac.ac_if		/* network-visible interface 	*/
#define	we_addr	we_ac.ac_enaddr		/* hardware Ethernet address 	*/

	u_char	we_flags;		/* software state		*/
#define	WDF_RUNNING	0x01
#define WDF_TXBUSY	0x02

	u_char	we_type;		/* interface type code		*/
	u_short	we_vector;		/* interrupt vector 		*/
	caddr_t	we_io_ctl_addr;		/* i/o bus address, control	*/
	caddr_t	we_io_nic_addr;		/* i/o bus address, DS8390	*/

	caddr_t	we_vmem_addr;		/* card RAM virtual memory base */
	u_long	we_vmem_size;		/* card RAM bytes		*/
	caddr_t	we_vmem_ring;		/* receive ring RAM vaddress	*/
} we_softc[NWE];

int	weprobe(), weattach(), weintr();
int	weinit(), weoutput(), weioctl(), wereset();

struct	isa_driver wedriver = {
	weprobe, weattach, "we",
};
 
/*
 * Probe the WD8003 to see if it's there
 */
weprobe(is)
	struct isa_device *is;
{
	register int i;
	register struct we_softc *sc = &we_softc[is->id_unit];
	union we_mem_sel wem;
	u_char sum;
 
	/*
	 * Here we check the card ROM, if the checksum passes, and the
	 * type code and ethernet address check out, then we know we have
	 * a wd8003 card.
	 *
	 * Autoconfiguration: No warning message is printed on error.
	 */
	for (sum = 0, i = 0; i < 8; ++i)
	    sum += inb(is->id_iobase + WD_ROM_OFFSET + i);
	if (sum != WD_CHECKSUM)
            return (0);
	sc->we_type = inb(is->id_iobase + WD_ROM_OFFSET + 6);
	if ((sc->we_type != WD_ETHER) && (sc->we_type != WD_STARLAN)
	&& (sc->we_type != WD_ETHER2))
            return (0);

	/*
	 * Setup card RAM area and i/o addresses
	 * Kernel Virtual to segment C0000-DFFFF?????
	 */
	sc->we_io_ctl_addr = is->id_iobase;
	sc->we_io_nic_addr = sc->we_io_ctl_addr + WD_NIC_OFFSET;
	sc->we_vector = is->id_irq;
	sc->we_vmem_addr = (caddr_t)is->id_maddr;
	sc->we_vmem_size = is->id_msize;
	sc->we_vmem_ring = sc->we_vmem_addr + (WD_PAGE_SIZE * WD_TXBUF_SIZE);

	/*
	 * Save board ROM station address
	 */
	for (i = 0; i < ETHER_ADDR_LEN; ++i)
	    sc->we_addr[i] = inb(sc->we_io_ctl_addr + WD_ROM_OFFSET + i);

	/*
	 * Mapin interface memory, setup memory select register
	 */
	/* wem.ms_addr = (u_long)sc->we_vmem_addr >> 13;  */
	wem.ms_addr = (u_long)(0xd0000)>> 13; 
	wem.ms_enable = 1;
	wem.ms_reset = 0;
	outb(sc->we_io_ctl_addr, wem.ms_byte);

	/*
	 * clear interface memory, then sum to make sure its valid
	 */
	for (i = 0; i < sc->we_vmem_size; ++i)
	    sc->we_vmem_addr[i] = 0x0;
	for (sum = 0, i = 0; i < sc->we_vmem_size; ++i)
	    sum += sc->we_vmem_addr[i];
	if (sum != 0x0) {
            printf("we%d: wd8003 dual port RAM address error\n", is->id_unit);
	    return (0);
	}

	return (WD_IO_PORTS);
}
 
/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
weattach(is)
	struct isa_device *is;
{
	register struct we_softc *sc = &we_softc[is->id_unit];
	register struct ifnet *ifp = &sc->we_if;
 
	/*
	 * Initialize ifnet structure
	 */
	ifp->if_unit = is->id_unit;
	ifp->if_name = "we";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST|IFF_NOTRAILERS;
	ifp->if_init = weinit;
	ifp->if_output = weoutput;
	ifp->if_ioctl = weioctl;
	ifp->if_reset = wereset;
	ifp->if_watchdog = 0;
	if_attach(ifp);
 
	/*
	 * Banner...
	 */
	printf(" %s address %s",
		((sc->we_type != WD_STARLAN) ? "ethernet" : "starlan"),
		ether_sprintf(sc->we_addr));
}
 
/*
 * Reset of interface.
 */
wereset(unit, uban)
	int unit, uban;
{
	if (unit >= NWE)
		return;
	printf("we%d: reset\n", unit);
	we_softc[unit].we_flags &= ~WDF_RUNNING;
	weinit(unit);
}
 
/*
 * Take interface offline.
 */
westop(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];
	union we_command wecmd;
	int s;
 
	/*
	 * Shutdown DS8390
	 */
	s = splimp();
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_stp = 1;
	wecmd.cs_sta = 0;
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	(void) splx(s);
}

/*
 * Initialization of interface (really just DS8390). 
 */
weinit(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];
	register struct ifnet *ifp = &sc->we_if;
	union we_command wecmd;
	int i, s;
 
	/* address not known */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;

	/* already running */
	if (sc->we_flags & WDF_RUNNING)
		return;

	/*
	 * Initialize DS8390 in order given in NSC NIC manual.
	 * this is stock code...please see the National manual for details.
	 */
	s = splhigh();
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_stp = 1;
	wecmd.cs_sta = 0;
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_DCR, WD_D_CONFIG);
	outb(sc->we_io_nic_addr + WD_P0_RBCR0, 0);
	outb(sc->we_io_nic_addr + WD_P0_RBCR1, 0);
	outb(sc->we_io_nic_addr + WD_P0_RCR, WD_R_MON);
	outb(sc->we_io_nic_addr + WD_P0_TCR, WD_T_CONFIG);
	outb(sc->we_io_nic_addr + WD_P0_TPSR, 0);
	outb(sc->we_io_nic_addr + WD_P0_PSTART, WD_TXBUF_SIZE);
	outb(sc->we_io_nic_addr + WD_P0_PSTOP,
		sc->we_vmem_size / WD_PAGE_SIZE);
	outb(sc->we_io_nic_addr + WD_P0_BNRY, WD_TXBUF_SIZE);
	outb(sc->we_io_nic_addr + WD_P0_ISR, 0xff);
	outb(sc->we_io_nic_addr + WD_P0_IMR, WD_I_CONFIG);
	wecmd.cs_ps = 1;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	for (i = 0; i < ETHER_ADDR_LEN; ++i)
	    outb(sc->we_io_nic_addr + WD_P1_PAR0 + i, sc->we_addr[i]);
	for (i = 0; i < ETHER_ADDR_LEN; ++i)	/* == broadcast addr */
	    outb(sc->we_io_nic_addr + WD_P1_MAR0 + i, 0xff);
	outb(sc->we_io_nic_addr + WD_P1_CURR, WD_TXBUF_SIZE);
	wecmd.cs_ps = 0;
	wecmd.cs_stp = 0;
	wecmd.cs_sta = 1;
	wecmd.cs_rd = 0x4;
	outb(sc->we_io_nic_addr + WD_P1_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_RCR, WD_R_CONFIG);

	/*
	 * Take the interface out of reset, program the vector, 
	 * enable interrupts, and tell the world we are up.
	 */
	ifp->if_flags |= IFF_UP | IFF_RUNNING;
	sc->we_flags |= WDF_RUNNING;
	sc->we_flags &= ~WDF_TXBUSY;
	(void) splx(s);
	westart(unit);
}
 
/*
 * Start output on interface.
 */
westart(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];
	struct mbuf *m0, *m;
	register caddr_t buffer;
	int len = 0, s;
	union we_command wecmd;
 
	/*
	 * The DS8390 has only one transmit buffer, if it is busy we
	 * must wait until the transmit interrupt completes.
	 */
	s = splhigh();
	if (sc->we_flags & WDF_TXBUSY) {
		(void) splx(s);
		return;
	}
	IF_DEQUEUE(&sc->we_if.if_snd, m);
	if (m == 0) {
		(void) splx(s);
		return;
	}
	sc->we_flags |= WDF_TXBUSY; 
	(void) splx(s);

	/*
	 * Copy the mbuf chain into the transmit buffer
	 */
	buffer = sc->we_vmem_addr;
	for (m0 = m; m != 0; m = m->m_next) {
		bcopy(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
        	len += m->m_len;
	}

	/*
	 * If this was a broadcast packet loop it
	 * back because the hardware can't hear its own
	 * transmits.
	 */
	if (bcmp((caddr_t)(mtod(m0, struct ether_header *)->ether_dhost),
	   (caddr_t)etherbroadcastaddr,
	   sizeof(etherbroadcastaddr)) == 0) {
		weread(sc, m0);
	} else {
		m_freem(m0);
	}

	/*
	 * Init transmit length registers, and set transmit start flag.
	 */
	s = splhigh();
	len = MAX(len, ETHER_MIN_LEN);
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_TBCR0, len & 0xff);
	outb(sc->we_io_nic_addr + WD_P0_TBCR1, len >> 8);
	wecmd.cs_txp = 1;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	(void) splx(s);
}
 
/*
 * Ethernet interface interrupt processor
 */
weintr(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[0];
	union we_command wecmd;
	union we_interrupt weisr;
	int s;
	unit =0;
 
	/* disable onboard interrupts, then get interrupt status */
	s = splhigh();
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_IMR, 0);
	weisr.is_byte = inb(sc->we_io_nic_addr + WD_P0_ISR);
	outb(sc->we_io_nic_addr + WD_P0_ISR, 0xff);
	(void) splx(s);

	/* transmit error */
	if (weisr.is_txe) {
		/* need to read these registers to clear status */
		sc->we_if.if_collisions +=
		    inb(sc->we_io_nic_addr + WD_P0_TBCR0);
		++sc->we_if.if_oerrors;
	}

	/* receiver error */
	if (weisr.is_rxe) {
		/* need to read these registers to clear status */
		(void) inb(sc->we_io_nic_addr + 0xD);
		(void) inb(sc->we_io_nic_addr + 0xE);
		(void) inb(sc->we_io_nic_addr + 0xF);
		++sc->we_if.if_ierrors;
	}

	/* normal transmit complete */
	if (weisr.is_ptx)
		wetint (unit);

	/* normal receive notification */
	if (weisr.is_prx)
		werint (unit);

	/* try to start transmit */
	westart(unit);

	/* re-enable onboard interrupts */
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_IMR, WD_I_CONFIG);
}
 
/*
 * Ethernet interface transmit interrupt.
 */
wetint(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];

	/*
	 * Do some statistics (assume page zero of NIC mapped in)
	 */
	sc->we_flags &= ~WDF_TXBUSY; 
	sc->we_if.if_timer = 0;
	++sc->we_if.if_opackets;
	sc->we_if.if_collisions += inb(sc->we_io_nic_addr + WD_P0_TBCR0);
}
 
/*
 * Ethernet interface receiver interrupt.
 */
werint(unit)
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];
	register struct mbuf **m;
	int mlen, len, count;
	u_char bnry, curr;
	union we_command wecmd;
	struct we_ring *wer;
	struct mbuf *m0;
	caddr_t pkt, endp;
static Bdry;
 
	/*
	 * Traverse the receive ring looking for packets to pass back.
	 * The search is complete when we find a descriptor not in use.
	 */
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	bnry = inb(sc->we_io_nic_addr + WD_P0_BNRY);
	wecmd.cs_ps = 1;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	curr = inb(sc->we_io_nic_addr + WD_P1_CURR);
if(Bdry && Bdry > bnry)
	bnry =Bdry;
	while (bnry != curr)
	{
		/* get pointer to this buffer header structure */
		wer = (struct we_ring *)(sc->we_vmem_addr + (bnry << 8));
        	len = wer->we_count - 4;	/* count includes CRC */
		pkt = (caddr_t)(wer + 1) /*- 2*/;	/* 2 - word align pkt data */
        	count = len /*+ 2*/;		/* copy two extra bytes */
		endp = (caddr_t)(sc->we_vmem_addr + sc->we_vmem_size);
		++sc->we_if.if_ipackets;

		/* pull packet out of dual ported RAM */
		m = &m0; m0 = 0;
		while (count > 0)
		{
		    /* drop chain if can't get another buffer */
		    MGET(*m, M_DONTWAIT, MT_DATA);
		    if (*m == 0)
		    {
			m_freem(m0);
			goto outofbufs;
		    }

		    /* fill mbuf and attach to packet list */
		    mlen = MIN(MLEN, count);
		    mlen = MIN(mlen, endp - pkt);
		    bcopy(pkt, mtod(*m, caddr_t), mlen);
		    (*m)->m_len = mlen;
		    m = &((*m)->m_next);
		    pkt += mlen;
		    count -= mlen;

		    /* wrap memory pointer around circ buffer */
		    if (pkt == endp)
			pkt = (caddr_t)sc->we_vmem_ring;
		}

		/* skip aligment bytes, send packet up to higher levels */
		if (m0 != 0)
		{
/*		    m0->m_off += 2*/;
		    weread(sc, m0);
		}

outofbufs:
		/* advance on chip Boundry register */
		bnry = wer->we_next_packet;
		wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
		wecmd.cs_ps = 0;
		outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);

		/* watch out for NIC overflow, reset Boundry if invalid */
		if ((bnry - 1) < WD_TXBUF_SIZE) {
#ifdef notdef
		    wereset(unit, 0);
		    break;
#else
		    outb(sc->we_io_nic_addr + WD_P0_BNRY,
			(sc->we_vmem_size / WD_PAGE_SIZE) - 1);
#endif
		}
		outb(sc->we_io_nic_addr + WD_P0_BNRY, bnry-1);

		/* refresh our copy of CURR */
		wecmd.cs_ps = 1;
		outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
		curr = inb(sc->we_io_nic_addr + WD_P1_CURR);
	}
Bdry = bnry;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 */
weoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
	u_char edst[6];
	struct in_addr idst;
	register struct we_softc *sc = &we_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	int usetrailers;
 
	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}

	switch (dst->sa_family) {
 
#ifdef INET
	case AF_INET:
		/* Note: we ignore usetrailers */
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		if (!arpresolve(&sc->we_ac, m, &idst, edst, &usetrailers))
			return (0);	/* if not yet resolved */
		type = ETHERTYPE_IP;
		break;
#endif
#ifdef NS
	case AF_NS:
		type = ETHERTYPE_NS;
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		    (caddr_t)edst, sizeof (edst));
		break;
#endif

 
	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		break;
 
	default:
		printf("we%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}
 
	/*
	 * Add local net header.  If no space in first mbuf,
	 * allocate another.
	 */
	if (m->m_off > MMAXOFF || MMINOFF + ETHER_HDR_SIZE > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = ETHER_HDR_SIZE;
	} else {
		m->m_off -= ETHER_HDR_SIZE;
		m->m_len += ETHER_HDR_SIZE;
	}
	eh = mtod(m, struct ether_header *);
	eh->ether_type = htons((u_short)type);
 	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof (edst));
 	bcopy((caddr_t)sc->we_addr, (caddr_t)eh->ether_shost,
		sizeof (sc->we_addr));
 
	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		(void) splx(s);
		m_freem(m);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	(void) splx(s);
	westart(ifp->if_unit);
	return (0);
 
bad:
	m_freem(m0);
	return (error);
}
 
/*
 * Process an ioctl request.
 */
weioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct we_softc *sc = &we_softc[ifp->if_unit];
	struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;
 
	switch (cmd) {
 
	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		weinit(ifp->if_unit);
		switch(ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);
			
			if (ns_nullhost(*ina))
				ina->x_host = *(union ns_host *)(sc->we_addr);
			else
				wesetaddr(ina->x_host.c_host, ifp->if_unit);
			break;
		    }
#endif
		}
		break;

	case SIOCSIFFLAGS:
		if (((ifp->if_flags & IFF_UP) == 0) &&
		   (sc->we_flags & WDF_RUNNING)) {
			westop(ifp->if_unit);
		} else if (((ifp->if_flags & IFF_UP) == IFF_UP) &&
		   ((sc->we_flags & WDF_RUNNING) == 0))
			weinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
 
	}
	(void) splx(s);
	return (error);
}
 
/*
 * set ethernet address for unit
 */
wesetaddr(physaddr, unit)
	u_char *physaddr;
	int unit;
{
	register struct we_softc *sc = &we_softc[unit];
	register int i;

	/*
	 * Rewrite ethernet address, and then force restart of NIC
	 */
	for (i = 0; i < ETHER_ADDR_LEN; i++)
		sc->we_addr[i] = physaddr[i];
	sc->we_flags &= ~WDF_RUNNING;
	weinit(unit);
}
 
/*
 * Pass a packet to the higher levels.
 * NO TRAILER PROTOCOL!
 */
weread(sc, m)
	register struct we_softc *sc;
    	struct mbuf *m;
{
	struct ether_header *eh;
	int scn, type, s;
	struct ifqueue *inq;
 
	/*
	 * Get ethernet protocol type out of ether header
	 */
	eh = mtod(m, struct ether_header *);
	type = ntohs((u_short)eh->ether_type);

	/*
	 * Drop ethernet header
	 */
	m->m_off += ETHER_HDR_SIZE;
	m->m_len -= ETHER_HDR_SIZE;
	
	/*
	 * Insert ifp pointer at start of packet
	 */
	m->m_off -= sizeof (struct ifnet *);
	m->m_len += sizeof (struct ifnet *);
	*(mtod(m, struct ifnet **)) = &sc->we_if;

	switch (type) {

#ifdef INET
	case ETHERTYPE_IP:
		scn = NETISR_IP;
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput(&sc->we_ac, m);
		return;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		scn = NETISR_NS;
		inq = &nsintrq;
		break;

#endif
 
	default:
		m_freem(m);
		return;
	}
 
	s = splimp();
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);
	schednetisr(scn);
	(void) splx(s);
}

#endif
