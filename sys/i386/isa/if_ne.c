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
 * NE1000/NE2000 Ethernet driver
 *
 * Parts inspired from Tim Tucker's if_wd driver for the wd8003,
 * insight on the ne2000 gained from Robert Clements PC/FTP driver.
 */

#include "ne.h"
#if NNE > 0

#include "param.h"
#include "systm.h"
#include "errno.h"
#include "ioctl.h"
#include "mbuf.h"
#include "socket.h"
#include "syslog.h"

#include "net/if.h"
#include "net/if_dl.h"
#include "net/if_types.h"
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

#include "i386/isa/isa.h"
#include "i386/isa/isa_device.h"
#include "i386/isa/icu.h"
#include "i386/isa/if_nereg.h"

#include "i386/include/pio.h"

int	neprobe(), neattach(), neintr();
int	nestart(),neinit(), ether_output(), neioctl();

struct	isa_driver nedriver = {
	neprobe, neattach, "ne",
};

struct	mbuf *neget();

#define ETHER_MIN_LEN 64
#define ETHER_MAX_LEN 1536

/* Word Transfers, Burst Mode Select, Fifo at 8 bytes */
#define DCR_CTRL2	(DSDC_WTS|DSDC_BMS|DSDC_FT1)
/* No Word Transfers, Burst Mode Select, Fifo at 8 bytes */
#define DCR_CTRL1	(DSDC_BMS|DSDC_FT1)


/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * arpcom.ac_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	ne_softc {
	struct	arpcom arpcom;		/* Ethernet common part */
	int	ns_flags;
#define	DSF_LOCK	1		/* block re-entering enstart */
	int	ns_mask;
	int	ns_ba;			/* byte addr in buffer ram of inc pkt */
	int	ns_cur;			/* current page being filled */
	u_short ns_iobase;
	u_short ns_board;		/* Board-Type: 0:NE1000, 1:NE2000 */
	u_short ns_tbuf;
	u_short ns_rbuf;
	u_short ns_rbufend;
	struct	prhdr	ns_ph;		/* hardware header of incoming packet*/
	struct	ether_header ns_eh;	/* header of incoming packet */
	u_char	ns_pb[2048 /*ETHERMTU+sizeof(long)*/];
} ne_softc[NNE];
#define	ENBUFSIZE	(sizeof(struct ether_header) + ETHERMTU + 2 + ETHER_MIN_LEN)

u_char boarddata[16];
void nefetch (struct ne_softc *ns, void *up, u_int ad, u_int len);
void neput (struct ne_softc *ns, void *up, u_int ad, u_int len);


neprobe(dvp)
	struct isa_device *dvp;
{
	int val,i, test, unit;
	u_short iobase;
	register struct ne_softc *ns;

	unit = dvp->id_unit;
	if (unit >= NNE)
		return (0);
	ns = &ne_softc[unit];
	if (ns->ns_iobase)
		/* Unit already configured */
		return (0);
	iobase = ns->ns_iobase = dvp->id_iobase;

	/* Reset the bastard */
	val = inb(iobase+ne_reset);
	DELAY(200);
	outb(iobase+ne_reset,val);
	DELAY(200);

	outb(iobase+ds_cmd, DSCM_STOP|DSCM_NODMA);
	
	if (inb (iobase + ds_cmd) != (DSCM_STOP | DSCM_NODMA))
		return (0);
	i = 1000000;
	while ((inb(iobase+ds0_isr)&DSIS_RESET) == 0 && i-- > 0);
	if (i <= 0) return (0);

	outb(iobase+ds0_isr, 0xff);

	/* No Word Transfers, Burst Mode Select, Fifo at 8 bytes */
	outb (iobase+ds0_dcr, DCR_CTRL1);

	outb(iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	DELAY(10000);

	/* Check cmd reg and fail if not right */
	if ((i=inb(iobase+ds_cmd)) != (DSCM_NODMA|DSCM_PG0|DSCM_STOP))
		return(0);

	outb(iobase+ds0_tcr, 0);
	outb(iobase+ds0_rcr, DSRC_MON);
	outb(iobase+ds0_pstart, RBUF1/DS_PGSIZE);
	outb(iobase+ds0_pstop, RBUFEND1/DS_PGSIZE);
	outb(iobase+ds0_bnry, RBUFEND1/DS_PGSIZE);
	outb(iobase+ds0_imr, 0);
	outb(iobase+ds0_isr, 0);
	outb(iobase+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(iobase+ds1_curr, RBUF1/DS_PGSIZE);
	outb(iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	test = 0xA55A55AA;
	neput (ns, &test, TBUF1, sizeof (test));
	nefetch (ns, &test, TBUF1, sizeof (test));
	if (test == 0xA55A55AA) {
		ns->ns_board = 0;	/* NE1000 */
		ns->ns_tbuf = TBUF1;
		ns->ns_rbuf = RBUF1;
		ns->ns_rbufend = RBUFEND1;
	}
	else {
		ns->ns_board = 1;	/* NE2000 */
		ns->ns_tbuf = TBUF2;
		ns->ns_rbuf = RBUF2;
		ns->ns_rbufend = RBUFEND2;
		outb(iobase+ds0_dcr, DCR_CTRL2);
	}

#ifdef NEDEBUG
#define	PAT(n)	(0xa55a + 37*(n))
#define	RCON	37
	{	int i, rom, pat;

		rom=1;
		printf("ne ram ");
		
		for (i = 0; i < 0xfff0; i+=4) {
			pat = PAT(i);
			neput (ns, &pat,i,4);
			nefetch (ns, &pat,i,4);
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
			neput (ns, &pat,i,4);
		}
		printf("\n");
	}
#endif

	/* Extract board address */
	nefetch (ns, boarddata, 0, sizeof(boarddata));
	if (ns->ns_board)
		for (i = 0; i < 6; i++)
			ns->arpcom.ac_enaddr[i] = boarddata[2 * i];
	else
		for (i = 0; i < 6; i++)
			ns->arpcom.ac_enaddr[i] = boarddata[i];
	return (1);
}

/*
 * Fetch from onboard ROM/RAM
 */
void nefetch (struct ne_softc *ns, void *up, u_int ad, u_int len)
{
	u_char cmd;
	const u_short iobase = ns->ns_iobase;

	if (len == 0)
		return;
	cmd = inb(iobase+ds_cmd);
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);

	/* Setup remote dma */
	outb (iobase+ds0_isr, DSIS_RDC);
	outb (iobase+ds0_rbcr0, len);
	outb (iobase+ds0_rbcr1, len>>8);
	outb (iobase+ds0_rsar0, ad);
	outb (iobase+ds0_rsar1, ad>>8);

	/* Execute & extract from card */
	outb (iobase+ds_cmd, DSCM_RREAD|DSCM_PG0|DSCM_START);
	if (ns->ns_board)
		insw (iobase+ne_data, up, len/2);
	else
		insb (iobase+ne_data, up, len/1);

	/* Wait till done, then shutdown feature */
	while ((inb (iobase+ds0_isr) & DSIS_RDC) == 0)
		;
	outb (iobase+ds0_isr, DSIS_RDC);
	outb (iobase+ds_cmd, cmd);
}

/*
 * Put to onboard RAM
 */
void neput (struct ne_softc *ns, void *up, u_int ad, u_int len)
{
	u_char cmd;
	const u_short iobase = ns->ns_iobase;

	if (len == 0)
		return;
	cmd = inb(iobase+ds_cmd);
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);

	/* Setup for remote dma */
	outb (iobase+ds0_isr, DSIS_RDC);
	if(len&1) len++;		/* roundup to words */
	outb (iobase+ds0_rbcr0, len);
	outb (iobase+ds0_rbcr1, len>>8);
	outb (iobase+ds0_rsar0, ad);
	outb (iobase+ds0_rsar1, ad>>8);

	/* Execute & stuff to card */
	outb (iobase+ds_cmd, DSCM_RWRITE|DSCM_PG0|DSCM_START);
	if (ns->ns_board)
		outsw (iobase+ne_data, up, len/2);
	else
		outsb (iobase+ne_data, up, len/1);
	
	/* Wait till done, then shutdown feature */
	while ((inb (iobase+ds0_isr) & DSIS_RDC) == 0)
		;
	outb (iobase+ds0_isr, DSIS_RDC);
	outb (iobase+ds_cmd, cmd);
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
	register struct ifnet *ifp = &ns->arpcom.ac_if;

	ifp->if_unit = unit;
	ifp->if_name = nedriver.name;
	ifp->if_mtu = ETHERMTU;
	printf ("ne%d: address %s, type NE%s\n",
		unit,
		ether_sprintf(ns->arpcom.ac_enaddr),
		(ns->ns_board) ? "1000" : "2000");
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
	struct ifnet *ifp = &ns->arpcom.ac_if;
	int s;
	register i; char *cp;
	const u_short iobase = ns->ns_iobase;

 	if (ifp->if_addrlist == (struct ifaddr *)0) return;
	if (ifp->if_flags & IFF_RUNNING) return;

	s = splimp();

	/* set physical address on ethernet */
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	for (i=0; i < 6; i++) outb(iobase+ds1_par0+i,ns->arpcom.ac_enaddr[i]);

	/* clr logical address hash filter for now */
	for (i=0; i < 8; i++) outb(iobase+ds1_mar0+i,0xff);

	/* init regs */
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	outb (iobase+ds0_rbcr0, 0);
	outb (iobase+ds0_rbcr1, 0);
	outb (iobase+ds0_imr, 0);
	outb (iobase+ds0_isr, 0xff);
 
	outb(iobase+ds0_tcr, 0);
	outb (iobase+ds0_rcr, DSRC_MON);
	outb (iobase+ds0_tpsr, 0);
	outb(iobase+ds0_pstart, ns->ns_rbuf/DS_PGSIZE);
	outb(iobase+ds0_pstop, ns->ns_rbufend/DS_PGSIZE);
	outb(iobase+ds0_bnry, ns->ns_rbuf/DS_PGSIZE);
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(iobase+ds1_curr, ns->ns_rbuf/DS_PGSIZE);
	ns->ns_cur = ns->ns_rbuf/DS_PGSIZE;
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	outb (iobase+ds0_rcr, DSRC_AB);
	if (ns->ns_board) {
		outb(iobase + ds0_dcr, DCR_CTRL2);
	} else {
		outb(iobase+ds0_dcr, DCR_CTRL1);
	}
	outb (iobase+ds0_imr, 0xff);

	ns->arpcom.ac_if.if_flags |= IFF_RUNNING;
	ns->ns_mask = ~0;
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
	int buffer;
	int len = 0, i, total,t;
	const u_short iobase = ns->ns_iobase;

	/*
	 * The DS8390 has only one transmit buffer, if it is busy we
	 * must wait until the transmit interrupt completes.
	 */
	outb(iobase+ds_cmd,DSCM_NODMA|DSCM_START);

	if (ns->ns_flags & DSF_LOCK)
		return;

	if (inb(iobase+ds_cmd) & DSCM_TRANS)
		return;

	if ((ns->arpcom.ac_if.if_flags & IFF_RUNNING) == 0)
		return;

	IF_DEQUEUE(&ns->arpcom.ac_if.if_snd, m);
 
	if (m == 0)
		return;

	/*
	 * Copy the mbuf chain into the transmit buffer
	 */

	ns->ns_flags |= DSF_LOCK;	/* prevent entering nestart */
	buffer = ns->ns_tbuf; len = i = 0;
	t = 0;
	for (m0 = m; m != 0; m = m->m_next)
		t += m->m_len;
		
	m = m0;
	total = t;
	for (m0 = m; m != 0; ) {
		
		if (m->m_len&1 && t > m->m_len) {
			neput (ns, mtod(m, caddr_t), buffer, m->m_len - 1);
			t -= m->m_len - 1;
			buffer += m->m_len - 1;
			m->m_data += m->m_len - 1;
			m->m_len = 1;
			m = m_pullup(m, 2);
		} else {
			if (m->m_len) {
				neput (ns, mtod(m, caddr_t), buffer, m->m_len);
				buffer += m->m_len;
				t -= m->m_len;
			}
			MFREE(m, m0);
			m = m0;
		}
	}

	/*
	 * Init transmit length registers, and set transmit start flag.
	 */

	len = total;
	if (len < ETHER_MIN_LEN) len = ETHER_MIN_LEN;
	outb(iobase+ds0_tbcr0,len&0xff);
	outb(iobase+ds0_tbcr1,(len>>8)&0xff);
	outb(iobase+ds0_tpsr, ns->ns_tbuf/DS_PGSIZE);
	outb(iobase+ds_cmd, DSCM_TRANS|DSCM_NODMA|DSCM_START);
}

/* buffer successor/predecessor in ring? */
#define succ1(n) (((n)+1 >= RBUFEND1/DS_PGSIZE) ? RBUF1/DS_PGSIZE : (n)+1)
#define pred1(n) (((n)-1 < RBUF1/DS_PGSIZE) ? RBUFEND1/DS_PGSIZE-1 : (n)-1)
#define succ2(n) (((n)+1 >= RBUFEND2/DS_PGSIZE) ? RBUF2/DS_PGSIZE : (n)+1)
#define pred2(n) (((n)-1 < RBUF2/DS_PGSIZE) ? RBUFEND2/DS_PGSIZE-1 : (n)-1)

/*
 * Controller interrupt.
 */
neintr(unit)
{
	register struct ne_softc *ns = &ne_softc[unit];
	u_char cmd,isr;
	const u_short iobase = ns->ns_iobase;

	/* Save cmd, clear interrupt */
	cmd = inb (iobase+ds_cmd);
loop:
	isr = inb (iobase+ds0_isr);
	outb(iobase+ds_cmd,DSCM_NODMA|DSCM_START);
	outb(iobase+ds0_isr, isr);

	/* Receiver error */
	if (isr & DSIS_RXE) {
		/* need to read these registers to clear status */
		(void) inb(iobase+ ds0_rsr);
		(void) inb(iobase+ 0xD);
		(void) inb(iobase + 0xE);
		(void) inb(iobase + 0xF);
		ns->arpcom.ac_if.if_ierrors++;
	}

	/* We received something; rummage thru tiny ring buffer */
	if (isr & (DSIS_RX|DSIS_RXE|DSIS_ROVRN)) {
		u_char pend,lastfree;

		outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
		pend = inb(iobase+ds1_curr);
		outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		lastfree = inb(iobase+ds0_bnry);

		/* Have we wrapped? */
		if (lastfree >= ns->ns_rbufend/DS_PGSIZE)
			lastfree = ns->ns_rbuf/DS_PGSIZE;
		if (pend < lastfree && ns->ns_cur < pend)
			lastfree = ns->ns_cur;
		else	if (ns->ns_cur > lastfree)
			lastfree = ns->ns_cur;

		/* Something in the buffer? */
		while (pend != lastfree) {
			u_char nxt;

			/* Extract header from microcephalic board */
			nefetch (ns, &ns->ns_ph,lastfree*DS_PGSIZE,
				sizeof(ns->ns_ph));
			ns->ns_ba = lastfree*DS_PGSIZE+sizeof(ns->ns_ph);

			/* Incipient paranoia */
			if (ns->ns_ph.pr_status == DSRS_RPC ||
				/* for dequna's */
				ns->ns_ph.pr_status == 0x21)
				nerecv (ns);
#ifdef NEDEBUG
			else {
				printf("cur %x pnd %x lfr %x ",
					ns->ns_cur, pend, lastfree);
				printf("nxt %x len %x ", ns->ns_ph.pr_nxtpg,
					(ns->ns_ph.pr_sz1<<8)+ ns->ns_ph.pr_sz0);
				printf("Bogus Sts %x\n", ns->ns_ph.pr_status);	
			}
#endif

			nxt = ns->ns_ph.pr_nxtpg;

			/* Sanity check */
			if ( nxt >= ns->ns_rbuf/DS_PGSIZE && nxt <= ns->ns_rbufend/DS_PGSIZE
				&& nxt <= pend)
				ns->ns_cur = nxt;
			else	ns->ns_cur = nxt = pend;

			/* Set the boundaries */
			lastfree = nxt;
			if (ns->ns_board) {
				outb(iobase+ds0_bnry, pred2(nxt));
			} else {
				outb(iobase+ds0_bnry, pred1(nxt));
			}
			outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
			pend = inb(iobase+ds1_curr);
			outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		}
		outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA);
	}

	/* Transmit error */
	if (isr & DSIS_TXE) {
		ns->ns_flags &= ~DSF_LOCK;
		/* Need to read these registers to clear status */
		ns->arpcom.ac_if.if_collisions += inb(iobase+ds0_tbcr0);
		ns->arpcom.ac_if.if_oerrors++;
	}

	/* Packet Transmitted */
	if (isr & DSIS_TX) {
		ns->ns_flags &= ~DSF_LOCK;
		++ns->arpcom.ac_if.if_opackets;
		ns->arpcom.ac_if.if_collisions += inb(iobase+ds0_tbcr0);
	}

	/* Receiver ovverun? */
	if (isr & DSIS_ROVRN) {
		log(LOG_ERR, "ne%d: error: isr %x\n", ns-ne_softc, isr
			/*, DSIS_BITS*/);
		outb(iobase+ds0_rbcr0, 0);
		outb(iobase+ds0_rbcr1, 0);
		outb(iobase+ds0_tcr, DSTC_LB0);
		outb(iobase+ds0_rcr, DSRC_MON);
		outb(iobase+ds_cmd, DSCM_START|DSCM_NODMA);
		outb(iobase+ds0_rcr, DSRC_AB);
		outb(iobase+ds0_tcr, 0);
	}

	/* Any more to send? */
	outb (iobase+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	nestart(&ns->arpcom.ac_if);
	outb (iobase+ds_cmd, cmd);
	outb (iobase+ds0_imr, 0xff);


	/* Still more to do? */
	isr = inb (iobase+ds0_isr);
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

	ns->arpcom.ac_if.if_ipackets++;
	len = ns->ns_ph.pr_sz0 + (ns->ns_ph.pr_sz1<<8);
	if(len < ETHER_MIN_LEN || len > ETHER_MAX_LEN)
		return;

	/* this need not be so torturous - one/two bcopys at most into mbufs */
	nefetch (ns, ns->ns_pb, ns->ns_ba, min(len,DS_PGSIZE-sizeof(ns->ns_ph)));
	if (len > DS_PGSIZE-sizeof(ns->ns_ph)) {
		int l = len - (DS_PGSIZE-sizeof(ns->ns_ph)), b, m;
		u_char *p = ns->ns_pb + (DS_PGSIZE-sizeof(ns->ns_ph));

		for (;;) {
			if (ns->ns_board == 1)
				ns->ns_cur = succ2 (ns->ns_cur);
			else
				ns->ns_cur = succ1 (ns->ns_cur);
			b = ns->ns_cur*DS_PGSIZE;
			if (l >= DS_PGSIZE) {
				nefetch (ns, p, b, DS_PGSIZE);
				p += DS_PGSIZE; l -= DS_PGSIZE;
				continue;
			}
			if (l > 0)
				nefetch (ns, p, b, l);
			break;
		}
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
	m = neget(buf, len, off, &ns->arpcom.ac_if);
	if (m == 0) return;

	ether_input(&ns->arpcom.ac_if, eh, m);
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
				ina->x_host =
					*(union ns_host *)(ns->arpcom.ac_enaddr);
			else {
				/* 
				 * The manual says we can't change the address 
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING; 
				bcopy((caddr_t)ina->x_host.c_host,
					(caddr_t)ns->arpcom.ac_enaddr,
					sizeof(ns->arpcom.ac_enaddr));
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
		if (((ifp->if_flags & IFF_UP) == 0) &&
		    (ifp->if_flags & IFF_RUNNING)) {
			outb(ns->ns_iobase+ds_cmd,DSCM_STOP|DSCM_NODMA);
			ifp->if_flags &= ~IFF_RUNNING;
		} else {
			if ((ifp->if_flags & IFF_UP) &&
			    ((ifp->if_flags & IFF_RUNNING) == 0))
				neinit(ifp->if_unit);
		}
		break;

#ifdef notdef
	case SIOCGHWADDR:
		bcopy((caddr_t)ns->arpcom.ac_enaddr, (caddr_t) &ifr->ifr_data,
			sizeof(ns->arpcom.ac_enaddr));
		break;
#endif

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
#endif
