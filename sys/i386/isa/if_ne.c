/*-
 * Copyright (c) 1990, 1991 William F. Jolitz.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)if_ne.c	7.4 (Berkeley) 5/21/91
 */

/*
 * NE2000/NE1000 Ethernet driver
 *
 * Parts inspired from Tim Tucker's if_wd driver for the wd8003,
 * insight on the ne2000 gained from Robert Clements PC/FTP driver.
 *
 * Corrected for NE1000 by Andrew A. Chernov
 */

#include "ne.h"
#if NNE > 0

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"

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
#include "i386/isa/if_nereg.h"
#include "i386/isa/icu.h"

int	neprobe(), neattach(), neintr();
int	nestart(),neinit(), ether_output(), neioctl();

struct	isa_driver nedriver = {
	neprobe, neattach, "ne",
};

struct	mbuf *neget();

#define ETHER_MIN_LEN 64
#define ETHER_MAX_LEN 1536

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * ns_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	ne_softc {
	struct	arpcom ns_ac;		/* Ethernet common part */
#define	ns_if	ns_ac.ac_if		/* network-visible interface */
#define	ns_addr	ns_ac.ac_enaddr		/* hardware Ethernet address */
	int	ns_flags;
#define	DSF_LOCK	1		/* block re-entering enstart */
	int	ns_oactive ;
	int	ns_mask ;
	int	ns_ba;			/* byte addr in buffer ram of inc pkt */
	int	ns_cur;			/* current page being filled */
	struct	prhdr	ns_ph;		/* hardware header of incoming packet*/
	struct	ether_header ns_eh;	/* header of incoming packet */
	u_char	ns_pb[2048 /*ETHERMTU+sizeof(long)*/];
	short	ns_txstart;		/* transmitter buffer start */
	u_short ns_rxend;               /* recevier buffer end */
	short	ns_port;		/* i/o port base */
	short	ns_mode;		/* word/byte mode */
} ne_softc[NNE] ;
#define	ENBUFSIZE	(sizeof(struct ether_header) + ETHERMTU + 2 + ETHER_MIN_LEN)

#define	PAT(n)	(0xa55a + 37*(n))

u_short boarddata[16];
 
neprobe(dvp)
	struct isa_device *dvp;
{
	int val, i, s, sum, bytemode = 1, pat;
	register struct ne_softc *ns = &ne_softc[0];
	register nec;

#ifdef lint
	neintr(0);
#endif

	nec = ns->ns_port = dvp->id_iobase;
	s = splimp();

	if (bytemode) {
		/* Byte Transfers, Burst Mode Select, Fifo at 8 bytes */
		ns->ns_mode = DSDC_BMS|DSDC_FT1;
		ns->ns_txstart = TBUF8;
		ns->ns_rxend = RBUFEND8;
	} else {
word:
		/* Word Transfers, Burst Mode Select, Fifo at 8 bytes */
		ns->ns_mode = DSDC_WTS|DSDC_BMS|DSDC_FT1;
		ns->ns_txstart = TBUF16;
		ns->ns_rxend = RBUFEND16;
		bytemode = 0;
	}

	/* Reset the bastard */
	val = inb(nec + ne_reset);
	DELAY(200);
	outb(nec + ne_reset, val);
	DELAY(200);

	outb(nec + ds_cmd, DSCM_STOP|DSCM_NODMA);
	
	i = 10000;
	while ((inb(nec + ds0_isr) & DSIS_RESET) == 0 && i-- > 0);
	if (i < 0) return (0);

	outb(nec + ds0_isr, 0xff);
	outb(nec + ds0_dcr, ns->ns_mode);
	outb(nec + ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	DELAY(1000);

	/* Check cmd reg and fail if not right */
	if ((i = inb(nec + ds_cmd)) != (DSCM_NODMA|DSCM_PG0|DSCM_STOP))
		return(0);

	outb(nec + ds0_tcr, 0);
	outb(nec + ds0_rcr, DSRC_MON);
	outb(nec + ds0_pstart, (ns->ns_txstart+PKTSZ)/DS_PGSIZE);
	outb(nec + ds0_pstop, ns->ns_rxend/DS_PGSIZE);
	outb(nec + ds0_bnry, ns->ns_rxend/DS_PGSIZE);
	outb(nec + ds0_imr, 0);
	outb(nec + ds0_isr, 0);
	outb(nec + ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(nec + ds1_curr, (ns->ns_txstart+PKTSZ)/DS_PGSIZE);
	outb(nec + ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);


#ifdef NEDEBUG
#define	RCON	37
	{	int i, rom;

		rom=1;
		printf("ne ram ");
		
		for (i = 0; i < 0xfff0; i+=4) {
			pat = PAT(i);
			neput(ns, &pat,i,4);
			nefetch(ns, &pat,i,4);
			if (pat == PAT(i)) {
				if (rom) {
					rom=0;
					printf(" %x", i);
				}
			} else {
				if (!rom) {
					rom=1;
					printf("..%x ", i);
				}
			}
			pat=0;
			neput(ns, &pat,i,4);
		}
		printf("\n");
	}
#endif

	/*
	 * <groan> detect difference between units
	 * solely by where the RAM is decoded.
	 */
	pat = PAT(0);
	neput(ns, &pat, ns->ns_txstart, 4);
	nefetch(ns, &pat, ns->ns_txstart, 4);
	if (pat != PAT(0)) {
		if (bytemode)
			goto word;
		else return (0);
	}


	/* Extract board address */
	nefetch (ns, (caddr_t)boarddata, 0, sizeof(boarddata));

	for(i=0; i < 6; i++)
		ns->ns_addr[i] = boarddata[i];
	splx(s);
	return (1);
}

/*
 * Fetch from onboard ROM/RAM
 */
nefetch (ns, up, ad, len) struct ne_softc *ns; caddr_t up; {
	u_char cmd;
	register nec = ns->ns_port;
	int counter = 100000;

	cmd = inb (nec + ds_cmd);
	outb (nec + ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);

	/* Setup remote dma */
	outb (nec + ds0_isr, DSIS_RDC);

	if ((ns->ns_mode & DSDC_WTS) && len&1)
		len++;		/* roundup to words */

	outb (nec+ds0_rbcr0, len);
	outb (nec+ds0_rbcr1, len>>8);
	outb (nec+ds0_rsar0, ad);
	outb (nec+ds0_rsar1, ad>>8);

	/* Execute & extract from card */
	outb (nec+ds_cmd, DSCM_RREAD|DSCM_PG0|DSCM_START);

	if (ns->ns_mode & DSDC_WTS)
		insw (nec+ne_data, up, len/2);
	else
		insb (nec+ne_data, up, len);

	/* Wait till done, then shutdown feature */
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0 && counter-- > 0)
		;
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);
}

/*
 * Put to onboard RAM
 */
neput (ns, up, ad, len) struct ne_softc *ns; caddr_t up; {
	u_char cmd;
	register nec = ns->ns_port;
	int counter = 100000;

	cmd = inb(nec+ds_cmd);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);

	/* Setup for remote dma */
	outb (nec+ds0_isr, DSIS_RDC);

	if ((ns->ns_mode & DSDC_WTS) && len&1)
		len++;		/* roundup to words */

	outb (nec+ds0_rbcr0, len);
	outb (nec+ds0_rbcr1, len>>8);
	outb (nec+ds0_rsar0, ad);
	outb (nec+ds0_rsar1, ad>>8);

	/* Execute & stuff to card */
	outb (nec+ds_cmd, DSCM_RWRITE|DSCM_PG0|DSCM_START);
	if (ns->ns_mode & DSDC_WTS)
		outsw (nec+ne_data, up, len/2);
	else
		outsb (nec+ne_data, up, len);
	
	/* Wait till done, then shutdown feature */
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0 && counter-- > 0)
		;
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);
}

/*
 * Reset of interface.
 */
nereset(unit, uban)
	int unit, uban;
{
	if (unit >= NNE)
		return;
	printf("ne%d: reset\n", unit);
	ne_softc[unit].ns_flags &= ~DSF_LOCK;
	neinit(unit);
}
 
/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.  We get the ethernet address here.
 */
neattach(dvp)
	struct isa_device *dvp;
{
	int unit = dvp->id_unit;
	register struct ne_softc *ns = &ne_softc[unit];
	register struct ifnet *ifp = &ns->ns_if;

	ifp->if_unit = unit;
	ifp->if_name = nedriver.name ;
	ifp->if_mtu = ETHERMTU;
	printf ("ne%d: NE%s address %s", unit,
		  (ns->ns_mode & DSDC_WTS) ? "2000" : "1000",
		  ether_sprintf(ns->ns_addr)) ;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS;
	ifp->if_init = neinit;
	ifp->if_output = ether_output;
	ifp->if_start = nestart;
	ifp->if_ioctl = neioctl;
	ifp->if_reset = nereset;
	ifp->if_watchdog = 0;
	if_attach(ifp);
}

/*
 * Initialization of interface; set up initialization block
 * and transmit/receive descriptor rings.
 */
neinit(unit)
	int unit;
{
	register struct ne_softc *ns = &ne_softc[unit];
	struct ifnet *ifp = &ns->ns_if;
	int s;
	int i; char *cp;
	register nec = ns->ns_port;

 	if (ifp->if_addrlist == (struct ifaddr *)0) return;
	if (ifp->if_flags & IFF_RUNNING) return;

	s = splimp();

	/* set physical address on ethernet */
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	for (i=0 ; i < 6 ; i++) outb(nec+ds1_par0+i,ns->ns_addr[i]);

	/* clr logical address hash filter for now */
	for (i=0 ; i < 8 ; i++) outb(nec+ds1_mar0+i,0xff);

	/* init regs */
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	outb (nec+ds0_rbcr0, 0);
	outb (nec+ds0_rbcr1, 0);
	outb (nec+ds0_imr, 0);
	outb (nec+ds0_isr, 0xff);

	/* Word Transfer select, Burst Mode Select, Fifo at 8 bytes */
	outb(nec+ds0_dcr, ns->ns_mode);

	outb(nec+ds0_tcr, 0);
	outb (nec+ds0_rcr, DSRC_MON);
	outb (nec+ds0_tpsr, 0);
	outb(nec+ds0_pstart, (ns->ns_txstart+PKTSZ)/DS_PGSIZE);
	outb(nec+ds0_pstop, ns->ns_rxend/DS_PGSIZE);
	outb(nec+ds0_bnry, (ns->ns_txstart+PKTSZ)/DS_PGSIZE);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(nec+ds1_curr, (ns->ns_txstart+PKTSZ)/DS_PGSIZE);
	ns->ns_cur = (ns->ns_txstart+PKTSZ)/DS_PGSIZE;
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	outb (nec+ds0_rcr, DSRC_AB);
	outb(nec+ds0_dcr, ns->ns_mode);
	outb (nec+ds0_imr, 0xff);

	ns->ns_if.if_flags |= IFF_RUNNING;
	ns->ns_flags &= ~DSF_LOCK;
	ns->ns_oactive = 0; ns->ns_mask = ~0;
	nestart(ifp);
	splx(s);
}

/*
 * Setup output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 * called only at splimp or interrupt level.
 */
nestart(ifp)
	struct ifnet *ifp;
{
	register struct ne_softc *ns = &ne_softc[ifp->if_unit];
	struct mbuf *m0, *m;
	int len, i, total,t;
	register nec = ns->ns_port;
	u_char cmd;
	u_short word;
	int counter;

	if (ns->ns_flags & DSF_LOCK)
		return;

	if ((ns->ns_if.if_flags & IFF_RUNNING) == 0)
		return;

	/*
	 * The DS8390 has only one transmit buffer, if it is busy we
	 * must wait until the transmit interrupt completes.
	 */
	outb(nec+ds_cmd,DSCM_NODMA|DSCM_START);

	if (inb(nec+ds_cmd) & DSCM_TRANS)
		return;

	IF_DEQUEUE(&ns->ns_if.if_snd, m);
 
	if (m == 0)
		return;

	ns->ns_flags |= DSF_LOCK;	/* prevent entering nestart */

	/*
	 * Copy the mbuf chain into the transmit buffer
	 */

	len = i = 0;
	t = 0;
	for (m0 = m; m != 0; m = m->m_next)
		t += m->m_len;
		
	/* next code derived from neput() */
		
	if ((ns->ns_mode & DSDC_WTS) && t&1)
		t++;          /* roundup to words */
		
	cmd = inb(nec+ds_cmd);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);

	/* Setup for remote dma */
	outb (nec+ds0_isr, DSIS_RDC);

	outb (nec+ds0_rbcr0, t);
	outb (nec+ds0_rbcr1, t>>8);
	outb (nec+ds0_rsar0, ns->ns_txstart);
	outb (nec+ds0_rsar1, ns->ns_txstart>>8);

	/* Execute & stuff to card */
	outb (nec+ds_cmd, DSCM_RWRITE|DSCM_PG0|DSCM_START);

			m = m0;
	total = t;
	if (ns->ns_mode & DSDC_WTS) {        /* Word Mode */
		while (m != 0) {
			if (m->m_len > 1)
				outsw(nec+ne_data, m->m_data, m->m_len / 2);
			if (m->m_len & 1) {
				word = (u_char) *(mtod(m, caddr_t) + m->m_len - 1);
				if ((m = m->m_next) != 0) {
					word |= *mtod(m, caddr_t) << 8;
					m->m_len--;
					m->m_data++;
				}
				outsw(nec+ne_data, (caddr_t)&word, 1);
			} else
				m = m->m_next;
		}
	}
	else {                                /* Byte Mode */
		while (m != 0) {
			if (m->m_len > 0)
				outsb(nec+ne_data, mtod(m, caddr_t), m->m_len);
			m = m->m_next;
		}
	}

	m_freem(m0);

	counter = 100000;
	/* Wait till done, then shutdown feature */
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0 && counter-- > 0)
		;
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);

	/*
	 * Init transmit length registers, and set transmit start flag.
	 */

	len = total;
	if (len < ETHER_MIN_LEN) len = ETHER_MIN_LEN;
	outb(nec+ds0_tbcr0,len&0xff);
	outb(nec+ds0_tbcr1,(len>>8)&0xff);
	outb(nec+ds0_tpsr, ns->ns_txstart/DS_PGSIZE);
	outb(nec+ds_cmd, DSCM_TRANS|DSCM_NODMA|DSCM_START);
}

/* buffer successor/predecessor in ring? */
#define succ(n) (((n)+1 >= ns->ns_rxend/DS_PGSIZE) ? (ns->ns_txstart+PKTSZ)/DS_PGSIZE : (n)+1)
#define pred(n) (((n)-1 < (ns->ns_txstart+PKTSZ)/DS_PGSIZE) ? ns->ns_rxend/DS_PGSIZE-1 : (n)-1)

/*
 * Controller interrupt.
 */
neintr(unit)
{
	register struct ne_softc *ns = &ne_softc[unit];
	u_char cmd,isr;
	register nec = ns->ns_port;

	/* Save cmd, clear interrupt */
	cmd = inb (nec+ds_cmd);
loop:
	isr = inb (nec+ds0_isr);
	outb(nec+ds_cmd,DSCM_NODMA|DSCM_START);
	outb(nec+ds0_isr, isr);

	/* Receiver error */
	if (isr & DSIS_RXE) {
		/* need to read these registers to clear status */
		(void) inb(nec+ ds0_rsr);
		(void) inb(nec+ 0xD);
		(void) inb(nec + 0xE);
		(void) inb(nec + 0xF);
		ns->ns_if.if_ierrors++;
	}

	/* We received something; rummage thru tiny ring buffer */
	if (isr & (DSIS_RX|DSIS_RXE|DSIS_ROVRN)) {
		u_char pend,lastfree;

		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
		pend = inb(nec+ds1_curr);
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		lastfree = inb(nec+ds0_bnry);

		/* Have we wrapped? */
		if (lastfree >= ns->ns_rxend/DS_PGSIZE)
			lastfree = (ns->ns_txstart+PKTSZ)/DS_PGSIZE;
		if (pend < lastfree && ns->ns_cur < pend)
			lastfree = ns->ns_cur;
		else	if (ns->ns_cur > lastfree)
			lastfree = ns->ns_cur;

		/* Something in the buffer? */
		while (pend != lastfree) {
			u_char nxt;

			/* Extract header from microcephalic board */
			nefetch(ns, &ns->ns_ph,lastfree*DS_PGSIZE,
				sizeof(ns->ns_ph));
			ns->ns_ba = lastfree*DS_PGSIZE+sizeof(ns->ns_ph);

			/* Incipient paranoia */
			if (ns->ns_ph.pr_status == DSRS_RPC ||
				/* for dequna's */
				ns->ns_ph.pr_status == 0x21)
				nerecv (ns);
#ifdef NEDEBUG
			else  {
				printf("cur %x pnd %x lfr %x ",
					ns->ns_cur, pend, lastfree);
				printf("nxt %x len %x ", ns->ns_ph.pr_nxtpg,
					(ns->ns_ph.pr_sz1<<8)+ ns->ns_ph.pr_sz0);
				printf("Bogus Sts %x\n", ns->ns_ph.pr_status);	
			}
#endif

			nxt = ns->ns_ph.pr_nxtpg ;

			/* Sanity check */
			if ( nxt >= (ns->ns_txstart+PKTSZ)/DS_PGSIZE
				&& nxt <= ns->ns_rxend/DS_PGSIZE && nxt <= pend)
				ns->ns_cur = nxt;
			else	ns->ns_cur = nxt = pend;

			/* Set the boundaries */
			lastfree = nxt;
			outb(nec+ds0_bnry, pred(nxt));
			outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
			pend = inb(nec+ds1_curr);
			outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		}
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA);
	}

	/* Transmit error */
	if (isr & DSIS_TXE) {
		ns->ns_flags &= ~DSF_LOCK;
		/* Need to read these registers to clear status */
		ns->ns_if.if_collisions += inb(nec+ds0_tbcr0);
		ns->ns_if.if_oerrors++;
	}

	/* Packet Transmitted */
	if (isr & DSIS_TX) {
		ns->ns_flags &= ~DSF_LOCK;
		++ns->ns_if.if_opackets;
		ns->ns_if.if_collisions += inb(nec+ds0_tbcr0);
	}

	/* Receiver ovverun? */
	if (isr & DSIS_ROVRN) {
		log(LOG_ERR, "ne%d: error: isr %x\n", ns-ne_softc, isr
			/*, DSIS_BITS*/);
		outb(nec+ds0_rbcr0, 0);
		outb(nec+ds0_rbcr1, 0);
		outb(nec+ds0_tcr, DSTC_LB0);
		outb(nec+ds0_rcr, DSRC_MON);
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA);
		outb(nec+ds0_rcr, DSRC_AB);
		outb(nec+ds0_tcr, 0);
	}

	/* Any more to send? */
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	nestart(&ns->ns_if);
	outb (nec+ds_cmd, cmd);
	outb (nec+ds0_imr, 0xff);

	/* Still more to do? */
	isr = inb (nec+ds0_isr);
	if(isr) goto loop;
}

/*
 * Ethernet interface receiver interface.
 * If input error just drop packet.
 * Otherwise examine packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
nerecv(ns)
	register struct ne_softc *ns;
{
	int len,i;

	ns->ns_if.if_ipackets++;
	len = ns->ns_ph.pr_sz0 + (ns->ns_ph.pr_sz1<<8);
	if(len < ETHER_MIN_LEN || len > ETHER_MAX_LEN)
		return;

	/* this need not be so torturous - one/two bcopys at most into mbufs */
	nefetch(ns, ns->ns_pb, ns->ns_ba, min(len,DS_PGSIZE-sizeof(ns->ns_ph)));
	if (len > DS_PGSIZE-sizeof(ns->ns_ph)) {
		int l = len - (DS_PGSIZE-sizeof(ns->ns_ph)), b ;
		u_char *p = ns->ns_pb + (DS_PGSIZE-sizeof(ns->ns_ph));

		if (++ns->ns_cur >= ns->ns_rxend/DS_PGSIZE)
			ns->ns_cur = (ns->ns_txstart+PKTSZ)/DS_PGSIZE;
		b = ns->ns_cur*DS_PGSIZE;
		
		while (l >= DS_PGSIZE) {
			nefetch(ns, p, b, DS_PGSIZE);
			p += DS_PGSIZE; l -= DS_PGSIZE;
			if (++ns->ns_cur >= ns->ns_rxend/DS_PGSIZE)
				ns->ns_cur = (ns->ns_txstart+PKTSZ)/DS_PGSIZE;
			b = ns->ns_cur*DS_PGSIZE;
		}
		if (l > 0)
			nefetch(ns, p, b, l);
	}
	/* don't forget checksum! */
	len -= sizeof(struct ether_header) + sizeof(long);
			
	neread(ns,(caddr_t)(ns->ns_pb), len);
}

/*
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
neread(ns, buf, len)
	register struct ne_softc *ns;
	char *buf;
	int len;
{
	register struct ether_header *eh;
    	struct mbuf *m;
	int off, resid;
	register struct ifqueue *inq;

	/*
	 * Deal with trailer protocol: if type is trailer type
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	eh = (struct ether_header *)buf;
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	nedataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU) return;		/* sanity */
		eh->ether_type = ntohs(*nedataaddr(eh, off, u_short *));
		resid = ntohs(*(nedataaddr(eh, off+2, u_short *)));
		if (off + resid > len) return;		/* sanity */
		len = off + resid;
	} else	off = 0;

	if (len == 0) return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; neget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = neget(buf, len, off, &ns->ns_if);
	if (m == 0) return;

	ether_input(&ns->ns_if, eh, m);
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
neget(buf, totlen, off0, ifp)
	caddr_t buf;
	int totlen, off0;
	struct ifnet *ifp;
{
	struct mbuf *top, **mp, *m, *p;
	int off = off0, len;
	register caddr_t cp = buf;
	char *epkt;

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
		bcopy(cp, mtod(m, caddr_t), (unsigned)len);
		cp += len;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		if (cp == epkt)
			cp = buf;
	}
	return (top);
}

/*
 * Process an ioctl request.
 */
neioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct ne_softc *ns = &ne_softc[ifp->if_unit];
	struct ifreq *ifr = (struct ifreq *)data;
	int s = splimp(), error = 0;


	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;

		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			neinit(ifp->if_unit);	/* before arpwhohas */
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
				ina->x_host = *(union ns_host *)(ns->ns_addr);
			else {
				/* 
				 * The manual says we can't change the address 
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING; 
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)ns->ns_addr, sizeof(ns->ns_addr));
			}
			neinit(ifp->if_unit); /* does ne_setaddr() */
			break;
		    }
#endif
		default:
			neinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			ifp->if_flags &= ~IFF_RUNNING;
			outb(ns->ns_port + ds_cmd, DSCM_STOP|DSCM_NODMA);
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			neinit(ifp->if_unit);
		break;

#ifdef notdef
	case SIOCGHWADDR:
		bcopy((caddr_t)ns->ns_addr, (caddr_t) &ifr->ifr_data,
			sizeof(ns->ns_addr));
		break;
#endif

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
#endif
