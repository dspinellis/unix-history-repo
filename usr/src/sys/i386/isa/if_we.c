/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tim L. Tucker.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_we.c	8.1 (Berkeley) 6/11/93
 */

/*
 * Modification history
 *
 * 8/28/89 - Initial version(if_wd.c), Tim L Tucker
 */
 
#include "we.h"
#if NWE > 0
/*
 * Western Digital 8003 ethernet/starlan adapter
 *
 * Supports the following interface cards:
 * WD8003E, WD8003EBT, WD8003S, WD8003SBT, WD8013EBT
 *
 * The Western Digital card is one of many AT/MCA ethernet interfaces
 * based on the National DS8390 Network Interface chip set.
 */
#include <sys/param.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/syslog.h>

#include <net/if.h>
#include <net/netisr.h>

#ifdef INET
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_var.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#endif

#ifdef NS
#include <netns/ns.h>
#include <netns/ns_if.h>
#endif

#include <i386/isa/if_wereg.h>
#include <i386/isa/isa_device.h>
 
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
	short	we_io_ctl_addr;		/* i/o bus address, control	*/
	short	we_io_nic_addr;		/* i/o bus address, DS8390	*/

	caddr_t	we_vmem_addr;		/* card RAM virtual memory base */
	u_long	we_vmem_size;		/* card RAM bytes		*/
	caddr_t	we_vmem_ring;		/* receive ring RAM vaddress	*/
	caddr_t	we_vmem_end;		/* receive ring RAM end	*/
} we_softc[NWE];

int	weprobe(), weattach(), weintr(), westart();
int	weinit(), ether_output(), weioctl(), wereset(), wewatchdog();

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
	sc->we_vmem_end = sc->we_vmem_addr + is->id_msize;

	/*
	 * Save board ROM station address
	 */
	for (i = 0; i < ETHER_ADDR_LEN; ++i)
	    sc->we_addr[i] = inb(sc->we_io_ctl_addr + WD_ROM_OFFSET + i);

	/*
	 * Mapin interface memory, setup memory select register
	 */
	/* wem.ms_addr = (u_long)sc->we_vmem_addr >> 13; */
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
	union we_command wecmd;
 
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_stp = 1;
	wecmd.cs_sta = 0;
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	/*
	 * Initialize ifnet structure
	 */
	ifp->if_unit = is->id_unit;
	ifp->if_name = "we" ;
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS ;
	ifp->if_init = weinit;
	ifp->if_output = ether_output;
	ifp->if_start = westart;
	ifp->if_ioctl = weioctl;
	ifp->if_reset = wereset;
	ifp->if_watchdog = wewatchdog;
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
/*	we_softc[unit].we_flags &= ~WDF_RUNNING; */
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

wewatchdog(unit) {

	log(LOG_WARNING,"we%d: soft reset\n", unit);
	westop(unit);
	weinit(unit);
}

static Bdry;
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
	/*if (ifp->if_flags & IFF_RUNNING) return; */

	/*
	 * Initialize DS8390 in order given in NSC NIC manual.
	 * this is stock code...please see the National manual for details.
	 */
	s = splhigh();
Bdry = 0;
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
	ifp->if_flags |= IFF_RUNNING;
	sc->we_flags &= ~WDF_TXBUSY;
	(void) splx(s);
	westart(ifp);
}
 
/*
 * Start output on interface.
 */
westart(ifp)
	struct ifnet *ifp;
{
	register struct we_softc *sc = &we_softc[ifp->if_unit];
	struct mbuf *m0, *m;
	register caddr_t buffer;
	int len, s;
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
	len = 0;
	for (m0 = m; m != 0; m = m->m_next) {
		bcopy(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
        	len += m->m_len;
	}

	m_freem(m0);

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
	register struct we_softc *sc = &we_softc[unit];
	union we_command wecmd;
	union we_interrupt weisr;
	int nloops = 10;

	unit =0;
 
	/* disable onboard interrupts, then get interrupt status */
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	weisr.is_byte = inb(sc->we_io_nic_addr + WD_P0_ISR);
loop:
	outb(sc->we_io_nic_addr + WD_P0_ISR, weisr.is_byte);

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
	if (weisr.is_ptx || weisr.is_txe)
		wetint (unit);

	/* normal receive notification */
	if (weisr.is_prx || weisr.is_rxe)
		werint (unit);

	/* try to start transmit */
	westart(&sc->we_if);

	/* re-enable onboard interrupts */
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
	outb(sc->we_io_nic_addr + WD_P0_IMR, 0xff/*WD_I_CONFIG*/);
	weisr.is_byte = inb(sc->we_io_nic_addr + WD_P0_ISR);
	if (weisr.is_byte) {
		/*
		 * I caught it looping forever here a couple of times,
		 * but I haven't had time to figure out why.  Just
		 * returning seems to be safe, and it does not appear
		 * to interfere with future packets.    - Pace 5/19/92
		 */
		if (--nloops <= 0) {
			printf ("we0: weintr is looping\n");
			return;
		}
		goto loop;
	}
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
	u_char bnry, curr;
	long len;
	union we_command wecmd;
	struct we_ring *wer;
 
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
if(Bdry)
	bnry =Bdry;

	while (bnry != curr)
	{
		/* get pointer to this buffer header structure */
		wer = (struct we_ring *)(sc->we_vmem_addr + (bnry << 8));

		/* count includes CRC */
		len = wer->we_count - 4;
		if (len > 30 && len <= ETHERMTU+100
			/*&& (*(char *)wer  == 1 || *(char *) wer == 0x21)*/)
			weread(sc, (caddr_t)(wer + 1), len);
		else printf("reject %d", len);

outofbufs:
		wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
		wecmd.cs_ps = 0;
		outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);

		/* advance on chip Boundry register */
		if((caddr_t) wer + WD_PAGE_SIZE - 1 > sc->we_vmem_end) {
			bnry = WD_TXBUF_SIZE;
			outb(sc->we_io_nic_addr + WD_P0_BNRY,
					sc->we_vmem_size / WD_PAGE_SIZE-1);
	
		} else {
			if (len > 30 && len <= ETHERMTU+100)
				bnry = wer->we_next_packet;
			else bnry = curr;

			/* watch out for NIC overflow, reset Boundry if invalid */
			if ((bnry - 1) < WD_TXBUF_SIZE) {
		    		outb(sc->we_io_nic_addr + WD_P0_BNRY,
					(sc->we_vmem_size / WD_PAGE_SIZE) - 1);
				bnry = WD_TXBUF_SIZE;
			} else
				outb(sc->we_io_nic_addr + WD_P0_BNRY, bnry-1);
		}

		/* refresh our copy of CURR */
		wecmd.cs_ps = 1;
		outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
		curr = inb(sc->we_io_nic_addr + WD_P1_CURR);
	}
Bdry = bnry;
}

#ifdef shit
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
		switch(ifa->ifa_addr->sa_family) {
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
#endif
 
/*
 * Process an ioctl request.
 */
weioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct we_softc *sc = &we_softc[ifp->if_unit];
	struct ifreq *ifr = (struct ifreq *)data;
	int s = splimp(), error = 0;


	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;

		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			weinit(ifp->if_unit);	/* before arpwhohas */
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
				ina->x_host = *(union ns_host *)(sc->ns_addr);
			else {
				/* 
				 * The manual says we can't change the address 
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING; 
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)sc->ns_addr, sizeof(sc->ns_addr));
			}
			weinit(ifp->if_unit); /* does ne_setaddr() */
			break;
		    }
#endif
		default:
			weinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			ifp->if_flags &= ~IFF_RUNNING;
			westop(ifp->if_unit);
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			weinit(ifp->if_unit);
		break;

#ifdef notdef
	case SIOCGHWADDR:
		bcopy((caddr_t)sc->sc_addr, (caddr_t) &ifr->ifr_data,
			sizeof(sc->sc_addr));
		break;
#endif

	default:
		error = EINVAL;
	}
	splx(s);
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
 
#define	wedataaddr(sc, eh, off, type) \
	((type) ((caddr_t)((eh)+1)+(off) >= (sc)->we_vmem_end) ? \
		(((caddr_t)((eh)+1)+(off))) - (sc)->we_vmem_end \
		+ (sc)->we_vmem_ring: \
		((caddr_t)((eh)+1)+(off)))
/*
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
weread(sc, buf, len)
	register struct we_softc *sc;
	char *buf;
	int len;
{
	register struct ether_header *eh;
    	struct mbuf *m, *weget();
	int off, resid;

	/*
	 * Deal with trailer protocol: if type is trailer type
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	eh = (struct ether_header *)buf;
	eh->ether_type = ntohs((u_short)eh->ether_type);
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU) return;		/* sanity */
		eh->ether_type = ntohs(*wedataaddr(sc, eh, off, u_short *));
		resid = ntohs(*(wedataaddr(sc, eh, off+2, u_short *)));
		if (off + resid > len) return;		/* sanity */
		len = off + resid;
	} else	off = 0;

	len -= sizeof(struct ether_header);
	if (len <= 0) return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; neget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = weget(buf, len, off, &sc->we_if, sc);
	if (m == 0) return;
	ether_input(&sc->we_if, eh, m);
}

/*
 * Supporting routines
 */

/*
 * Pull read data off a interface.
 * Len is length of data, with local net header stripped.
 * Off is non-zero if a trailer protocol was used, and
 * gives the offset of the trailer information.
 * We copy the trailer information and then all the normal
 * data into mbufs.  When full cluster sized units are present
 * we copy into clusters.
 */
struct mbuf *
weget(buf, totlen, off0, ifp, sc)
	caddr_t buf;
	int totlen, off0;
	struct ifnet *ifp;
	struct we_softc *sc;
{
	struct mbuf *top, **mp, *m, *p;
	int off = off0, len;
	register caddr_t cp = buf;
	char *epkt;
	int tc =totlen;

	buf += sizeof(struct ether_header);
	cp = buf;
	epkt = cp + totlen;

	if (off) {
		cp += off + 2 * sizeof(u_short);
		totlen -= 2 * sizeof(u_short);
	}

	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return (0);
	m->m_pkthdr.rcvif = ifp;
	m->m_pkthdr.len = totlen;
	m->m_len = MHLEN;

	top = 0;
	mp = &top;
	while (totlen > 0) {
		if (top) {
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == 0) {
				m_freem(top);
				return (0);
			}
			m->m_len = MLEN;
		}
		len = min(totlen, epkt - cp);
		if (len >= MINCLSIZE) {
			MCLGET(m, M_DONTWAIT);
			if (m->m_flags & M_EXT)
				m->m_len = len = min(len, MCLBYTES);
			else
				len = m->m_len;
		} else {
			/*
			 * Place initial small packet/header at end of mbuf.
			 */
			if (len < m->m_len) {
				if (top == 0 && len + max_linkhdr <= m->m_len)
					m->m_data += max_linkhdr;
				m->m_len = len;
			} else
				len = m->m_len;
		}

		totlen -= len;
		/* only do up to end of buffer */
		if (cp+len > sc->we_vmem_end) {
			unsigned toend = sc->we_vmem_end - cp;

			bcopy(cp, mtod(m, caddr_t), toend);
			cp = sc->we_vmem_ring;
			bcopy(cp, mtod(m, caddr_t)+toend, len - toend);
			cp += len - toend;
			epkt = cp + totlen;
		} else {
			bcopy(cp, mtod(m, caddr_t), (unsigned)len);
			cp += len;
		}
		*mp = m;
		mp = &m->m_next;
		if (cp == epkt) {
			cp = buf;
			epkt = cp + tc;
		}
	}
	return (top);
}
#endif
