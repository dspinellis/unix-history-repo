#include "ec.h"
#if NEC > 0
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
 */
/*
 * A driver for the 3Com 3C503 (Etherlink II) ethernet adaptor.
 *
 *   Written by Herb Peyerl (hpeyerl@novatel.cuc.ab.ca) on 04/25/92.
 *   (This is my first ever device driver and I couldn't have done
 *    it without the consumption of many "Brock Gummy Bears" so a
 *    big thanx to the "Brock Candy Company" of Chattanooga TN)
 *
 *   This driver uses the Western Digital 8003 driver for a template
 *   since the two cards use the DP8390 chip by National.  Everything
 *   is fairly similar except that the nic on the wd8003 appears to
 *   to see shared memory at 0x0000 and on the 3com, the nic sees it 
 *   at 0x2000.  Also, the 3c503 has it's own ASIC for controlling 
 *   things like the IRQ level in software and whether to use the
 *   onboard xceiver or not.  Since the Clarkson drivers do a very
 *   good rendition of a 3c503, I also scavenged a lot of ideas from
 *   there.
 *
 * Kludges:
 *   Since I couldn't really think of any non-creative way (other than
 *   using a #define) of configuring the board to use the onboard xceiver,
 *   I kludged the isa_device->unit to contain this information.  Simply
 *   put, if bit-7 of isa_device->unit is set (>127) then the driver
 *   configures that unit to onboard-xceiver (BNC) and if <128 it assumes
 *   AUI.  ec_attach informs the user of this on bootup.  Also, ec_probe
 *   repairs this bit after obtaining it's information since I didn't know
 *   what else within the depths of the kernel would freak out if I left it.
 */
#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"
#include "net/if.h"
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
#include "i386/isa/isa_device.h"
#include "i386/isa/if_ec.h"

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * qe_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	ec_softc {
	struct	arpcom ec_ac;		/* Ethernet common part 	*/
#define	ec_if	ec_ac.ac_if		/* network-visible interface 	*/
#define	ec_addr	ec_ac.ac_enaddr		/* hardware Ethernet address 	*/

	u_char	ec_flags;		/* software state		*/
#define	EC_RUNNING	0x01
#define EC_TXBUSY	0x02

	u_char	ec_type;		/* interface type code		*/
	u_short	ec_vector;		/* interrupt vector 		*/
	short	ec_io_ctl_addr;		/* i/o bus address, control	*/
	short	ec_io_nic_addr;		/* i/o bus address, DS8390	*/
	short	thick_or_thin;		/* thick=0;thin=2		*/

	caddr_t	ec_vmem_addr;		/* card RAM virtual memory base */
	u_long	ec_vmem_size;		/* card RAM bytes		*/
	caddr_t	ec_vmem_ring;		/* receive ring RAM vaddress	*/
	caddr_t	ec_vmem_end;		/* receive ring RAM end	*/
} ec_softc[NEC];

#define PAGE0  outb(sc->ec_io_nic_addr + EN_CCMD, ENC_NODMA|ENC_PAGE0);
#define PAGE1  outb(sc->ec_io_nic_addr + EN_CCMD, ENC_NODMA|ENC_PAGE1);
static Bdry;

int ether_output(),
    ecprobe(),
    ecattach(),
    ecintr(),
    ec_init(),
    ec_ioctl(),
    ec_reset(),
    ec_watchdog(),
    ec_start_output();

struct isa_driver ecdriver = {
	ecprobe, 
        ecattach,
        "ec",
};

ecprobe(is)
struct isa_device *is;
{
	register struct ec_softc *sc = &ec_softc[is->id_unit&127];
	int i, sum; 
/*
 * Set up the softc structure with card specific info.
 * The 3Com asic is at base+0x400
 */
	sc->ec_io_ctl_addr = is->id_iobase + 0x400;
	sc->ec_io_nic_addr = is->id_iobase;
	sc->ec_vector = is->id_irq;
	sc->ec_vmem_addr = (caddr_t)is->id_maddr;
	sc->ec_vmem_size = is->id_msize;
	sc->ec_vmem_ring = sc->ec_vmem_addr + (EC_PAGE_SIZE * EC_TXBUF_SIZE);
	sc->ec_vmem_end = sc->ec_vmem_addr + is->id_msize;
/* 
 * Now we get the MAC address. Assume thin ethernet unless told otherwise later.
 */
	outb(sc->ec_io_ctl_addr + E33G_CNTRL, ECNTRL_RESET|2);	/* Toggle reset bit on*/
	DELAY(100);
	outb(sc->ec_io_ctl_addr + E33G_CNTRL, 2); 	/* Toggle reset bit off */
	DELAY(100);
	outb(sc->ec_io_ctl_addr + E33G_CNTRL, ECNTRL_SAPROM|2);	/* Map SA_PROM */
	for (i=0;i<ETHER_ADDR_LEN; ++i)
		sc->ec_addr[i] = inb(sc->ec_io_nic_addr + i);
	outb(sc->ec_io_ctl_addr + E33G_CNTRL, 2);   /* Disable SA_PROM */
	outb(sc->ec_io_ctl_addr + E33G_GACFR, EGACFR_IRQOFF);   /* tcm, rsel, mbs0, nim */
/*
 * Stop the chip just in case.
 */
	DELAY(1000);
	PAGE0
	outb(sc->ec_io_nic_addr + EN_CCMD, ENC_NODMA|ENC_STOP);
	DELAY(1000);

	/* Check cmd reg and fail if not right */
	if ((i=inb(sc->ec_io_nic_addr + EN_CCMD)) != (ENC_NODMA|ENC_STOP))
		return(0);

/*
 * Test the shared memory.
 */
	for(i = 0 ; i < sc->ec_vmem_size ; ++i)
		sc->ec_vmem_addr[i] = 0x0;
	for(sum=0, i=0; i<sc->ec_vmem_size; ++i)
		sum += sc->ec_vmem_addr[i];
	if(sum)
	{
		printf("ec%d: shared memory error (device conflict?)\n",
			is->id_unit);
		return(0);
	}
/*
 *  All done. 
 */
	return(1);
}

ecattach(is)
struct isa_device *is;
{
	register struct ec_softc *sc = &ec_softc[is->id_unit];
	register struct ifnet *ifp = &sc->ec_if;

/**
 ** Initialize the ASIC in same order as Clarkson driver.
 **/


/*
 * Point vector pointer registers off into boonies.
 */
	outb(sc->ec_io_ctl_addr + E33G_VP2, 0xff);
	outb(sc->ec_io_ctl_addr + E33G_VP1, 0xff);
	outb(sc->ec_io_ctl_addr + E33G_VP0, 0x0);
/*
 * Set up control of shared memory, buffer ring, etc.
 */
	outb(sc->ec_io_ctl_addr + E33G_STARTPG, EC_RXBUF_OFFSET);
	outb(sc->ec_io_ctl_addr + E33G_STOPPG, EC_RXBUF_END);
/*
 * Set up the IRQ  and NBURST on the board.
 *  ( Not sure why we set up NBURST since we don't use DMA.) 
 *
 * Normally we would is->id_irq<<2 but IRQ2 is defined as 0x200
 * in icu.h so it's a special case.
 */
	if(is->id_irq == 0x200)
		outb(sc->ec_io_ctl_addr + E33G_IDCFR, 0x10);
	else
		outb(sc->ec_io_ctl_addr + E33G_IDCFR, is->id_irq << 2);
        outb(sc->ec_io_ctl_addr + E33G_NBURST, 0x08);     /* Set Burst to 8 */
	outb(sc->ec_io_ctl_addr + E33G_DMAAH, 0x20);
	outb(sc->ec_io_ctl_addr + E33G_DMAAL, 0x0);
/*
 * Fill in the ifnet structure.
 */
	ifp->if_unit = is->id_unit;
	ifp->if_name = "ec" ;
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS;
	ifp->if_init = ec_init;
	ifp->if_output = ether_output;
	ifp->if_start = ec_start_output;
	ifp->if_ioctl = ec_ioctl;
	ifp->if_reset = ec_reset;
	ifp->if_watchdog = ec_watchdog;
/*
 * Attach the interface to something. Have to figure this out later.
 */
	if_attach(ifp);
/*
 * Weeee.. We get to tell people we exist...
 */
	printf(" address %s", ether_sprintf(sc->ec_addr));
}

ec_init(unit)
int unit;
{
	register struct ec_softc *sc = &ec_softc[unit];
	register struct ifnet *ifp = &sc->ec_if;
	int i, s;
	u_short   ax, cx;

	Bdry=0;
printf("ecinit");
/*
 * Address not known.
 */
	if(ifp->if_addrlist == (struct ifaddr *) 0)
		return;

	/*
	 * XXX (untested)
	 * select thick (e.g. AUI connector) if LLC0 bit is set
	 */
	if (ifp->if_flags & IFF_LLC0)
		outb(sc->ec_io_ctl_addr + E33G_CNTRL, 0);
	else
		outb(sc->ec_io_ctl_addr + E33G_CNTRL, 2);

/*
 * Set up the 8390 chip.
 *   (Use sequence recommended by 3Com. )
 */
	s=splhigh();
	PAGE0
	outb(sc->ec_io_nic_addr + EN_CCMD, ENC_NODMA|ENC_PAGE0|ENC_STOP);
	outb(sc->ec_io_nic_addr + EN0_DCFG, ENDCFG_BM8);
	outb(sc->ec_io_nic_addr + EN0_RCNTLO, 0x0);
	outb(sc->ec_io_nic_addr + EN0_RCNTHI, 0x0);
	outb(sc->ec_io_nic_addr + EN0_RXCR, ENRXCR_MON );
	outb(sc->ec_io_nic_addr + EN0_TXCR, 0x02);
	outb(sc->ec_io_nic_addr + EN0_BOUNDARY, EC_RXBUF_OFFSET);
	outb(sc->ec_io_nic_addr + EN0_TPSR, 0x20);
	outb(sc->ec_io_nic_addr + EN0_STARTPG, EC_RXBUF_OFFSET);
	outb(sc->ec_io_nic_addr + EN0_STOPPG, EC_RXBUF_END);
	outb(sc->ec_io_nic_addr + EN0_ISR, 0xff);
	outb(sc->ec_io_nic_addr + EN0_IMR, 0x3f);
/*
 * Copy Ethernet address from SA_PROM into 8390 chip registers.
 */
	PAGE1
	for(i=0;i<6;i++)
		outb(sc->ec_io_nic_addr + EN1_PHYS+i, sc->ec_addr[i]);
/*
 * Set multicast filter mask bits in case promiscuous rcv wanted (???)
 *   (set to 0xff as in if_we.c)
 */
	for(i=0;i<8;i++)
		outb(sc->ec_io_nic_addr + EN1_MULT+i, 0xff);
/*
 * Set current shared page for RX to work on.
 */
	outb(sc->ec_io_nic_addr + EN1_CURPAG, EC_RXBUF_OFFSET);
/*
 * Start the 8390. Clear Interrupt Status reg, and accept Broadcast
 * packets.
 */
	outb(sc->ec_io_nic_addr + EN_CCMD, ENC_START|ENC_PAGE0|ENC_NODMA);
	outb(sc->ec_io_nic_addr + EN0_ISR, 0xff);
	outb(sc->ec_io_nic_addr + EN0_TXCR, 0x0);
	outb(sc->ec_io_nic_addr + EN0_RXCR, ENRXCR_BCST);
/*
 * Take interface out of reset, program the vector,
 * enable interrupts, and tell the world we are up.
 */
	ifp->if_flags |= IFF_RUNNING;
	outb(sc->ec_io_ctl_addr + E33G_GACFR, EGACFR_NORM);  /* tcm, rsel, mbs0 */
	(void) splx(s);
	sc->ec_flags &= ~EC_TXBUSY;
	ec_start_output(ifp);
}

ec_start_output(ifp)
struct ifnet *ifp;
{
	register struct ec_softc *sc = &ec_softc[ifp->if_unit];
	struct mbuf *m0, *m;
	register caddr_t buffer;
	int len, s;
	int ec_cmd_reg;

/*
 * The DS8390 only has one transmit buffer, if it is busy we
 * must wait until the transmit interrupt completes.
 */
	s=splhigh();
	if(sc->ec_flags & EC_TXBUSY)
	{
		(void) splx(s);
		return;
	}
	IF_DEQUEUE(&sc->ec_if.if_snd, m);
	if(m == 0)
	{
		(void) splx(s);
		return;
	}
	sc->ec_flags |= EC_TXBUSY;
	(void) splx(s);

/*
 * Copy the mbuf chain into the transmit buffer
 */
	buffer = sc->ec_vmem_addr;
	len = 0;
	for(m0 = m; m!= 0; m = m->m_next)
	{
		bcopy(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
		len  += m->m_len;
	}
	m_freem(m0);

/*
 * Init transmit length registers and set transmit start flag.
 */
	s=splhigh();
	len = MAX(len, ETHER_MIN_LEN);
	PAGE0
	outb(sc->ec_io_nic_addr + EN0_TCNTLO, len & 0xff);
	outb(sc->ec_io_nic_addr + EN0_TCNTHI, len >> 8);
	ec_cmd_reg = inb(sc->ec_io_nic_addr + EN_CCMD);
	outb(sc->ec_io_nic_addr + EN_CCMD, ec_cmd_reg|ENC_TRANS);
	(void) splx(s);
}

	int ec_cmd_reg, ec_sts_reg;
/*
 * Interrupt handler.
 */
ecintr(unit)
int unit;
{
	register struct ec_softc *sc = &ec_softc[unit];

	unit = 0;

/*
 * Get current command register and interrupt status.
 * Turn off interrupts while we take care of things.
 */
	ec_cmd_reg = inb(sc->ec_io_nic_addr + EN_CCMD);
	PAGE0
	ec_sts_reg = inb(sc->ec_io_nic_addr + EN0_ISR);
	outb(sc->ec_io_nic_addr + EN0_IMR, 0x0);
loop:
	outb(sc->ec_io_nic_addr + EN0_ISR, ec_sts_reg);
	/*
	 * have we lost ourselves somewhere?
	 */
	if (ec_sts_reg == 0xff)
		ec_watchdog(unit);
/*
 * Transmit error
 */
 	if(ec_sts_reg & ENISR_TX_ERR)
	{
		sc->ec_if.if_collisions += inb(sc->ec_io_nic_addr + EN0_TCNTLO);
		++sc->ec_if.if_oerrors;
	}
/*
 * Receive Error
 */
	if(ec_sts_reg & ENISR_RX_ERR)
	{
		(void) inb(sc->ec_io_nic_addr + 0xD);
		(void) inb(sc->ec_io_nic_addr + 0xE);
		(void) inb(sc->ec_io_nic_addr + 0xF);
		++sc->ec_if.if_ierrors;
	}
/*
 * Normal transmit complete.
 */
	if(ec_sts_reg&ENISR_TX || ec_sts_reg&ENISR_TX_ERR) 
		ectint(unit);
/*
 * Normal receive notification
 */
	if(ec_sts_reg&(ENISR_RX|ENISR_RX_ERR)) 
		ecrint(unit);

/*
 * Try to start transmit
 */
	ec_start_output(&sc->ec_if);
/*
 * Reenable  onboard interrupts.
 */
	/*PAGE0*/
	outb(sc->ec_io_nic_addr + EN_CCMD, ec_cmd_reg);
	outb(sc->ec_io_nic_addr + EN0_IMR, 0x3f);
	if(ec_sts_reg=inb(sc->ec_io_nic_addr + EN0_ISR))
		goto loop;
}

/*
 * Transmit interrupt
 */
ectint(unit)
int unit;
{
	register struct ec_softc *sc = &ec_softc[unit];
/*
 * Do some statistics.
 */
	PAGE0
	sc->ec_flags &= ~EC_TXBUSY;
	sc->ec_if.if_timer = 0;
	++sc->ec_if.if_opackets;
	sc->ec_if.if_collisions += inb(sc->ec_io_nic_addr + EN0_TCNTLO);
}

/*
 * Receive interrupt.
 * (This gave me the most trouble so excuse the mess.)
 */
ecrint(unit)
int unit;
{
	register struct ec_softc *sc = &ec_softc[unit];
	u_char bnry, curr;
	int len;
	struct ec_ring *ecr;
 
	/*
	 * Traverse the receive ring looking for packets to pass back.
	 * The search is complete when we find a descriptor not in use.
	 */
	PAGE0
	bnry = inb(sc->ec_io_nic_addr + EN0_BOUNDARY);
	PAGE1
	curr = inb(sc->ec_io_nic_addr + EN1_CURPAG);
if(Bdry)
	bnry =Bdry;

	while (bnry != curr)
	{
		/* get pointer to this buffer header structure */
		ecr = (struct ec_ring *)(sc->ec_vmem_addr + ((bnry-EC_VMEM_OFFSET) << 8));

		/* count includes CRC */
		len = ecr->ec_count - 4;
		/*if (len > 30 && len <= ETHERMTU+100) */
			ecread(sc, (caddr_t)(ecr + 1), len);
		/*else printf("reject:%x bnry:%x curr:%x", len, bnry, curr);*/
outofbufs:
		PAGE0
		/* advance on chip Boundry register */
		if((caddr_t) ecr + EC_PAGE_SIZE - 1 > sc->ec_vmem_end) {
			bnry = EC_RXBUF_OFFSET;
			outb(sc->ec_io_nic_addr + EN0_BOUNDARY,
				(sc->ec_vmem_size / EC_PAGE_SIZE) - 1 - EC_VMEM_OFFSET); 
		} else {
			if (len > 30 && len <= ETHERMTU+100)
				bnry = ecr->ec_next_packet;
			else bnry = curr;

			/* watch out for NIC overflow, reset Boundry if invalid */
			if ((bnry - 1) < EC_RXBUF_OFFSET) {
		    		outb(sc->ec_io_nic_addr + EN0_BOUNDARY, 
					(sc->ec_vmem_size / EC_PAGE_SIZE) - 1 - EC_VMEM_OFFSET);
				bnry = EC_RXBUF_OFFSET;
			} else
				outb(sc->ec_io_nic_addr + EN0_BOUNDARY, bnry-1);
		}

		/* refresh our copy of CURR */
		PAGE1
		curr = inb(sc->ec_io_nic_addr + EN1_CURPAG);
	}
	Bdry = bnry;
	PAGE0
}

#define	ecdataaddr(sc, eh, off, type) \
	((type) ((caddr_t)((eh)+1)+(off) >= (sc)->ec_vmem_end) ? \
		(((caddr_t)((eh)+1)+(off))) - (sc)->ec_vmem_end \
		+ (sc)->ec_vmem_ring: \
		((caddr_t)((eh)+1)+(off)))

ecread(sc, buf, len)
register struct ec_softc *sc;
char *buf;
int len;
{
	register struct ether_header *eh;
    	struct mbuf *m, *ecget();
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
		eh->ether_type = ntohs(*ecdataaddr(sc, eh, off, u_short *));
		resid = ntohs(*(ecdataaddr(sc, eh, off+2, u_short *)));
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
	m = ecget(buf, len, off, &sc->ec_if, sc);
	if (m == 0) return;
	ether_input(&sc->ec_if, eh, m);
}
ec_ioctl(ifp, cmd, data)
register struct ifnet *ifp;
int cmd;
caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct ec_softc *sc = &ec_softc[ifp->if_unit];
	struct ifreq *ifr = (struct ifreq *)data;
	int s=splimp(), error=0;
	switch(cmd){
	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			ec_init(ifp->if_unit);	/* before arpwhohas */
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
			ec_init(ifp->if_unit); /* does ne_setaddr() */
			break;
		    }
#endif
		default:
			ec_init(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			ifp->if_flags &= ~IFF_RUNNING;
			ec_stop(ifp->if_unit);
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			ec_init(ifp->if_unit);
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

ec_reset(unit)
int unit;
{
	if(unit >= NEC)
		return;
	printf("ec%d: reset\n", unit);
	ec_init(unit);
}

ec_watchdog(unit)
int unit;
{
	log(LOG_WARNING, "ec%d: soft reset\n", unit);
	ec_stop(unit);
	ec_init(unit);
}

ec_stop(unit)
int unit;
{
	register struct ec_softc *sc = &ec_softc[unit];
	int s;

	s=splimp();
	PAGE0
	outb(sc->ec_io_nic_addr + EN_CCMD, ENC_NODMA|ENC_STOP);
	outb(sc->ec_io_nic_addr + EN0_IMR, 0x0);
	sc->ec_flags &= ~EC_RUNNING;
/*
 * Shutdown the 8390.
 */
	(void) splx(s);
}

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
ecget(buf, totlen, off0, ifp, sc)
	caddr_t buf;
	int totlen, off0;
	struct ifnet *ifp;
	struct ec_softc *sc;
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
		if (cp+len > sc->ec_vmem_end) {
			unsigned toend = sc->ec_vmem_end - cp;

			bcopy(cp, mtod(m, caddr_t), toend);
			cp = sc->ec_vmem_ring;
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
#endif /* NEC > 0 */
