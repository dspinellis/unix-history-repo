/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)if_we.c	7.3 (Berkeley) 5/21/91
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         2       00112
 * --------------------         -----   ----------------------
 *
 * 09 Sep 92	Mike Durkin		Fix Danpex EW-2016 & other 8013 clones
 *					enable with "options WECOMPAT"
 * 19 Sep 92	Michael Galassi		Fixed multiboard routing
 * 20 Sep 92	Barry Lustig		WD8013 16 bit mode -- enable
 *						with "options WD8013".
 * 14 Mar 93	Marc Frajola		BPF packet filter support
 * 14 Mar 93	David Greenman		Input and other routines re-written
 * 14 Mar 93	Rodney W. Grimes	Added link level address to we_attach
 */

/*
 * Modification history
 *
 * 8/28/89 - Initial version(if_wd.c), Tim L Tucker
 *
 * 92.09.19 - Changes to allow multiple we interfaces in one box.
 *          Allowed interupt handler to look at unit other than 0
 *            Bdry was static, made it into an array w/ one entry per
 *          interface.  nerd@percival.rain.com (Michael Galassi)
 *
 * BPF Packet Filter Support added by Marc Frajola, 12/30/92
 * Input & other routines re-written by David Greenman, 1/2/93
 * BPF trailer support added by David Greenman, 1/7/93
 * we_attach enhanced with link level address by Rodney W. Grimes, 1/30/93
 *
 * $Log:	if_we.c,v $
 * Revision 1.2  93/02/18  17:21:57  davidg
 * Bugs in mbuf cluster allocation fixed
 * Problem with nfs wanting mbufs aligned on longword boundries fixed
 * 
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

#include "net/if.h"
#include "net/if_types.h"
#include "net/if_dl.h"
#include "net/netisr.h"

#ifdef INET
#include "netinet/in.h"
#include "netinet/in_systm.h"
#include "netinet/in_var.h"
#include "netinet/ip.h"
#include "netinet/if_ether.h"
#endif

#ifdef NS
#include "netns/ns.h"
#include "netns/ns_if.h"
#endif

#include "bpfilter.h"
#if NBPFILTER > 0
#include "net/bpf.h"
#include "net/bpfdesc.h"
#endif

#include "i386/isa/isa.h"
#include "i386/isa/if_wereg.h"
#include "i386/isa/isa_device.h"
#include "i386/isa/icu.h"
#include "i386/include/pio.h"

static inline char *we_ring_copy();
 
/*
 * This constant should really be 60 because the we adds 4 bytes of crc.
 * However when set to 60 our packets are ignored by deunas , 3coms are
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
#if NBPFILTER > 0
#define	WDF_ATTACHED	0x80
#endif

	u_char	we_type;		/* interface type code		*/
	u_short	we_vector;		/* interrupt vector 		*/
	short	we_io_ctl_addr;		/* i/o bus address, control	*/
	short	we_io_nic_addr;		/* i/o bus address, DS8390	*/

	caddr_t	we_vmem_addr;		/* card RAM virtual memory base */
	u_long	we_vmem_size;		/* card RAM bytes		*/
	caddr_t	we_vmem_ring;		/* receive ring RAM vaddress	*/
	caddr_t	we_vmem_end;		/* receive ring RAM end	*/
	caddr_t	we_bpf;			/* Magic Cookie for BPF */
} we_softc[NWE];

int	weprobe(), weattach(), weintr(), westart();
int	weinit(), ether_output(), weioctl(), wereset(), wewatchdog();

struct	isa_driver wedriver = {
	weprobe, weattach, "we",
};
 
static unsigned short wemask[] =
	{ IRQ9, IRQ3, IRQ5, IRQ7, IRQ10, IRQ11, IRQ15, IRQ4 };
	
/*
 * Probe the WD8003 to see if its there
 */
weprobe(is)
	struct isa_device *is;
{
	register int i;
	register struct we_softc *sc = &we_softc[is->id_unit];
	union we_mem_sel wem;
	u_char sum;
#ifdef WD8013						/* 20 Sep 92*/
	union we_laar laar;

	laar.laar_byte = 0;
#endif	/* WD8013*/

	wem.ms_byte = 0;				/* 20 Sep 92*/
 
	/* reset card to force it into a known state. */
	outb(is->id_iobase, 0x80);
	DELAY(100);
	outb(is->id_iobase, 0x00);
	/* wait in the case this card is reading it's EEROM */
	DELAY(5000);

#ifdef WD8013						/* 20 Sep 92*/
	/* allow the NIC to access the shared RAM 16 bits at a time */

	laar.addr_l19 = 1;
	laar.lan_16_en = 1;
	laar.mem_16_en = 1;
	outb(is->id_iobase+5, laar.laar_byte);	/* Write a 0xc1 */
#endif	/* WD8013*/

	/*
	 * Here we check the card ROM, if the checksum passes, and the
	 * type code and ethernet address check out, then we know we have
	 * a wd8003 card.
	 *
	 * Autoconfiguration: No warning message is printed on error.
	 */
	for (sum = 0, i = 0; i < 8; ++i)
	    sum += inb(is->id_iobase + WD_ROM_OFFSET + i);
	if (sum != WD_CHECKSUM) {		/* 09 Sep 92*/
#ifdef WECOMPAT
	    printf( "we: probe: checksum failed... installing anyway\n");
	    printf( "we: Danpex EW-2016 or other 8013 clone card?\n");
#else	/* !WECOMPAT*/
            return (0);
#endif	/* !WECOMPAT*/
	}
	sc->we_type = inb(is->id_iobase + WD_ROM_OFFSET + 6);
#ifdef nope
	if ((sc->we_type & WD_REVMASK) != 2		/* WD8003E or WD8003S */
		&& (sc->we_type & WD_REVMASK) != 4	/* WD8003EBT */
		&& (sc->we_type & WD_REVMASK) != 6)        /* WD8003ELB? */
            return (0);
#endif
/*printf("type %x ", sc->we_type);*/
	if (sc->we_type & WD_SOFTCONFIG) {
		int iv = inb(is->id_iobase + 1) & 4 |
			((inb(is->id_iobase+4) & 0x60) >> 5);
/*printf("iv %d ", iv);*/
		if (wemask[iv] != is->id_irq)
			return(0);
		outb(is->id_iobase+4, inb(is->id_iobase+4) | 0x80);
	}

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
        wem.ms_addr = kvtop(sc->we_vmem_addr) >> 13;
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
	struct ifaddr *ifa;
	struct sockaddr_dl *sdl;
 
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
	/* Search down the ifa address list looking for the AF_LINK type entry */
 	ifa = ifp->if_addrlist;
	while ((ifa != 0) &&
	       (ifa->ifa_addr != 0) &&
	       (ifa->ifa_addr->sa_family != AF_LINK))
		{
		ifa = ifa->ifa_next;
		}
	/* If we find an AF_LINK type entry, we well fill in the hardware addr */
	if ((ifa != 0) &&
	    (ifa->ifa_addr != 0))
		{
		/* Fill in the link level address for this interface */
		sdl = (struct sockaddr_dl *)ifa->ifa_addr;
		sdl->sdl_type = IFT_ETHER;
		sdl->sdl_alen = ETHER_ADDR_LEN;
		sdl->sdl_slen = 0;
		bcopy(sc->we_addr, LLADDR(sdl), ETHER_ADDR_LEN);
		}

#if NBPFILTER > 0
	sc->we_flags &= ~WDF_ATTACHED;	/* Make sure BPF attach flag clear */
#endif
 
	/*
	 * Banner...
	 */
	printf(" %saddr %s",
		(sc->we_type & WD_ETHERNET) ? "enet" : "slan",
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

	weintr(unit);
	/*log(LOG_WARNING,"we%d: soft reset\n", unit);
	westop(unit);
	weinit(unit);*/
}

static Bdry[NWE];					/* 19 Sep 92*/
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

#if NBPFILTER > 0
	if ((sc->we_flags & WDF_ATTACHED) == 0) {
		bpfattach(&sc->we_bpf, ifp, DLT_EN10MB,
			sizeof(struct ether_header));
		sc->we_flags |= WDF_ATTACHED;
	}
#endif

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
	Bdry[unit] = 0;					/* 19 Sep 92*/
	wecmd.cs_byte = inb(sc->we_io_nic_addr + WD_P0_COMMAND);
	wecmd.cs_stp = 1;
	wecmd.cs_sta = 0;
	wecmd.cs_ps = 0;
	outb(sc->we_io_nic_addr + WD_P0_COMMAND, wecmd.cs_byte);
#ifdef WD8013						/* 20 Sep 92*/
	/* enable 16 bit access if 8013 card */
	outb(sc->we_io_nic_addr + WD_P0_DCR, WD_D_CONFIG16);
#else	/* !WD8013*/
	outb(sc->we_io_nic_addr + WD_P0_DCR, WD_D_CONFIG);
#endif	/* !WD8013*/
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
#if NBPFILTER > 0
	if (sc->we_if.if_flags & IFF_PROMISC) {
		outb(sc->we_io_nic_addr + WD_P0_RCR,
			 WD_R_PRO | WD_R_SEP | WD_R_AR | WD_R_CONFIG);
	} else
#endif
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

#if NBPFILTER > 0
	if (sc->we_bpf) {
		u_short etype;
		int off, datasize, resid;
		struct ether_header *eh;
		struct trailer_header {
			u_short ether_type;
			u_short ether_residual;
		} trailer_header;
		char ether_packet[ETHERMTU+100];
		char *ep;

		ep = ether_packet;

		/*
		 * We handle trailers below:
		 * Copy ether header first, then residual data,
		 * then data. Put all this in a temporary buffer
		 * 'ether_packet' and send off to bpf. Since the
		 * system has generated this packet, we assume
		 * that all of the offsets in the packet are
		 * correct; if they're not, the system will almost
		 * certainly crash in m_copydata.
		 * We make no assumptions about how the data is
		 * arranged in the mbuf chain (i.e. how much
		 * data is in each mbuf, if mbuf clusters are
		 * used, etc.), which is why we use m_copydata
		 * to get the ether header rather than assume
		 * that this is located in the first mbuf.
		 */
		/* copy ether header */
		m_copydata(m, 0, sizeof(struct ether_header), ep);
		eh = (struct ether_header *) ep;
		ep += sizeof(struct ether_header);
		etype = ntohs(eh->ether_type);
		if (etype >= ETHERTYPE_TRAIL &&
		    etype < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
			datasize = ((etype - ETHERTYPE_TRAIL) << 9);
			off = datasize + sizeof(struct ether_header);

			/* copy trailer_header into a data structure */
			m_copydata(m, off, sizeof(struct trailer_header),
				&trailer_header.ether_type);

			/* copy residual data */
			m_copydata(m, off+sizeof(struct trailer_header),
				resid = ntohs(trailer_header.ether_residual) -
				sizeof(struct trailer_header), ep);
			ep += resid;

			/* copy data */
			m_copydata(m, sizeof(struct ether_header), datasize, ep);
			ep += datasize;

			/* restore original ether packet type */
			eh->ether_type = trailer_header.ether_type;

			bpf_tap(sc->we_bpf, ether_packet, ep - ether_packet);
		} else
			bpf_mtap(sc->we_bpf, m);
	}
#endif

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
	sc->we_if.if_timer = 3;
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
	if (weisr.is_byte) goto loop;
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
	if(Bdry[unit])					/* 19 Sep 92*/
		bnry = Bdry[unit];

	while (bnry != curr)
	{
		/* get pointer to this buffer header structure */
		wer = (struct we_ring *)(sc->we_vmem_addr + (bnry << 8));

		/* count includes CRC */
		len = wer->we_count - 4;
		if (len > 30 && len <= ETHERMTU+100)
			weread(sc, (caddr_t)(wer + 1), len);
		else printf("we%d: reject - bad length %d", unit, len);

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
	Bdry[unit] = bnry;				/* 19 Sep 92*/
}

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
				 * The manual says we cant change the address 
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
#if NBPFILTER > 0
		if (sc->we_if.if_flags & IFF_PROMISC) {
			outb(sc->we_io_nic_addr + WD_P0_RCR,
				 WD_R_PRO | WD_R_SEP | WD_R_AR | WD_R_CONFIG);
		} else
#endif
			outb(sc->we_io_nic_addr + WD_P0_RCR, WD_R_CONFIG);
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
 
#define	ringoffset(sc, eh, off, type) \
	((type)( ((caddr_t)(eh)+(off) >= (sc)->we_vmem_end) ? \
		(((caddr_t)(eh)+(off))) - (sc)->we_vmem_end \
		+ (sc)->we_vmem_ring: \
		((caddr_t)(eh)+(off)) ))
/*
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
weread(sc, buf, len)
	register struct we_softc *sc;
	char *buf;
	int len;
{
	caddr_t we_ring_copy();
	struct ether_header *eh;
    	struct mbuf *m, *head, *we_ring_to_mbuf();
	int off, resid;
	u_short etype;
	struct trailer_header {
		u_short	trail_type;
		u_short trail_residual;
	} trailer_header;

	++sc->we_if.if_ipackets;

	/* Allocate a header mbuf */
	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		goto bad;
	m->m_pkthdr.rcvif = &sc->we_if;
	m->m_pkthdr.len = len;
	m->m_len = 0;
	head = m;

	eh = (struct ether_header *)buf;

#define EROUND	((sizeof(struct ether_header) + 3) & ~3)
#define EOFF	(EROUND - sizeof(struct ether_header))

	/*
	 * The following assumes there is room for
	 * the ether header in the header mbuf
	 */
	head->m_data += EOFF;
	bcopy(buf, mtod(head, caddr_t), sizeof(struct ether_header));
	buf += sizeof(struct ether_header);
	head->m_len += sizeof(struct ether_header);
	len -= sizeof(struct ether_header);

	etype = ntohs((u_short)eh->ether_type);

	/*
	 * Deal with trailer protocol:
	 * If trailer protocol, calculate the datasize as 'off',
	 * which is also the offset to the trailer header.
	 * Set resid to the amount of packet data following the
	 * trailer header.
	 * Finally, copy residual data into mbuf chain.
	 */
	if (etype >= ETHERTYPE_TRAIL &&
	    etype < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {

		off = (etype - ETHERTYPE_TRAIL) << 9;
		if ((off + sizeof(struct trailer_header)) > len)
			goto bad;	/* insanity */

		eh->ether_type = *ringoffset(sc, buf, off, u_short *);
		resid = ntohs(*ringoffset(sc, buf, off+2, u_short *));

		if ((off + resid) > len) goto bad;	/* insanity */

		resid -= sizeof(struct trailer_header);
		if (resid < 0) goto bad;	/* insanity */

		m = we_ring_to_mbuf(sc, ringoffset(sc, buf, off+4, char *), head, resid);
		if (m == 0) goto bad;

		len = off;
		head->m_pkthdr.len -= 4; /* subtract trailer header */
	}

	/*
	 * Pull packet off interface. Or if this was a trailer packet,
	 * the data portion is appended.
	 */
	m = we_ring_to_mbuf(sc, buf, m, len);
	if (m == 0) goto bad;

#if NBPFILTER > 0
	/*
	 * Check if there's a bpf filter listening on this interface.
	 * If so, hand off the raw packet to bpf. 
	 */
	if (sc->we_bpf) {
		bpf_mtap(sc->we_bpf, head);
	}

	/*
	 * Note that the interface cannot be in promiscuous mode if
	 * there are no bpf listeners.  And if we are in promiscuous
	 * mode, we have to check if this packet is really ours.
	 *
	 * XXX This test does not support multicasts.
	 */
	if ((sc->we_if.if_flags & IFF_PROMISC) &&
		bcmp(eh->ether_dhost, sc->we_addr,
			sizeof(eh->ether_dhost)) != 0 &&
		bcmp(eh->ether_dhost, etherbroadcastaddr,
			sizeof(eh->ether_dhost)) != 0) {

		m_freem(head);
		return;
	}
#endif

	/*
	 * Fix up data start offset in mbuf to point past ether header
	 */
	m_adj(head, sizeof(struct ether_header));

	/*
	 * silly ether_input routine needs 'type' in host byte order
	 */
	eh->ether_type = ntohs(eh->ether_type);

	ether_input(&sc->we_if, eh, head);
	return;

bad:	if (head)
		m_freem(head);
	return;
}

/*
 * Supporting routines
 */

/*
 * Copy data from receive buffer to end of mbuf chain
 * allocate additional mbufs as needed. return pointer
 * to last mbuf in chain.
 * sc = we info
 * src = pointer in we ring buffer
 * dst = pointer to last mbuf in mbuf chain to copy to
 * amount = amount of data to copy
 */
struct mbuf *
we_ring_to_mbuf(sc,src,dst,total_len)
	struct we_softc *sc;
	char *src;
	struct mbuf *dst;
	int total_len;
{
	register struct mbuf *m = dst;

	while (total_len > 0) {
		register int amount = min(total_len, M_TRAILINGSPACE(m));

		if (amount == 0) { /* no more data in this mbuf, alloc another */
			/*
			 * if there is enough data for an mbuf cluster, attempt
			 * to allocate one of those, otherwise, a regular mbuf
			 * will do.
			 */ 
			dst = m;
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == 0)
				return (0);

			if (total_len >= MINCLSIZE)
				MCLGET(m, M_DONTWAIT);

			m->m_len = 0;
			dst->m_next = m;
			amount = min(total_len, M_TRAILINGSPACE(m));
		}

		src = we_ring_copy(sc, src, mtod(m, caddr_t) + m->m_len, amount);

		m->m_len += amount;
		total_len -= amount;

	}
	return (m);
}

static inline char *
we_ring_copy(sc,src,dst,amount)
	struct we_softc *sc;
	char	*src;
	char	*dst;
	int	amount;
{
	int	tmp_amount;

	/* does copy wrap to lower addr in ring buffer? */
	if (src + amount > sc->we_vmem_end) {
		tmp_amount = sc->we_vmem_end - src;
		bcopy(src,dst,tmp_amount); /* copy amount up to end */
		amount -= tmp_amount;
		src = sc->we_vmem_ring;
		dst += tmp_amount;
	}

	bcopy(src, dst, amount);

	return(src + amount);
}
#endif

