#include "ne.h"
#if NNE > 0
/*
 * NE2000 Ethernet driver
 * Copyright (C) 1990 W. Jolitz
 * @(#)if_ne.c	1.5 (Berkeley) %G%
 *
 * Parts inspired from Tim Tucker's if_wd driver for the wd8003,
 * insight on the ne2000 gained from Robert Clements PC/FTP driver.
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

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

#include "machine/isa/isa_device.h"
#include "if_nereg.h"
#include "icu.h"

int	neprobe(), neattach(), neintr();
int	neinit(), neoutput(), neioctl();

#include "dbg.h"
struct	isa_driver nedriver = {
	neprobe, neattach, "ne",
};

struct	mbuf *neget();

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
} ne_softc[NNE] ;
#define	ENBUFSIZE	(sizeof(struct ether_header) + ETHERMTU + 2 + 64)

int nec;

u_short boarddata[16];
 
neprobe(dvp)
	struct isa_device *dvp;
{
	int val,i,s;
	register struct ne_softc *ns = &ne_softc[0];

#ifdef lint
	neintr(0);
#endif

	nec = dvp->id_iobase;
	s = splimp();

	/* reset the bastard */
	val = inb(nec+ne_reset);
	DELAY(2000000);
	outb(nec+ne_reset,val);

	outb(nec+ds_cmd, DSCM_STOP|DSCM_NODMA);
	
	i = 1000000;
	while ((inb(nec+ds0_isr)&DSIS_RESET) == 0 && i-- > 0) nulldev();
	if (i < 0) return (0);

	outb(nec+ds0_isr, 0xff);

	/* Word Transfers, Burst Mode Select, Fifo at 8 bytes */
	outb(nec+ds0_dcr, DSDC_WTS|DSDC_BMS|DSDC_FT1);

	outb(nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	DELAY(10000);

	/* check cmd reg and fail if not right */
	if ((i=inb(nec+ds_cmd)) != (DSCM_NODMA|DSCM_PG0|DSCM_STOP))
		return(0);

	outb(nec+ds0_tcr, 0);
	outb(nec+ds0_rcr, DSRC_MON);
	outb(nec+ds0_pstart, RBUF/DS_PGSIZE);
	outb(nec+ds0_pstop, RBUFEND/DS_PGSIZE);
	outb(nec+ds0_bnry, RBUFEND/DS_PGSIZE);
	outb(nec+ds0_imr, 0);
	outb(nec+ds0_isr, 0);
	outb(nec+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(nec+ds1_curr, RBUF/DS_PGSIZE);
	outb(nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	fetchrom (boarddata, 0, sizeof(boarddata));
#ifdef NEDEBUG
/*{ int i,rom;
	rom=1;
printf("ne ram ");
	for (i = 0; i < 0xfff0; i+=4) {
		int pat;
		pat = 0xa55a+i*37;
		putram(&pat,i,4);
		fetchram(&pat,i,4);
		if (pat == 0xa55a+i*37) {
			if (rom) { rom=0; printf(" %x", i); }
		} else {
			if (!rom) { rom=1; printf("..%x ", i); }
		}
		pat=0;
		putram(&pat,i,4);
	}
printf("\n");
}*/
#endif
/* checksum data? */
	/* extract board address */
	for(i=0; i < 6; i++)  ns->ns_addr[i] = boarddata[i];
	splx(s);
	return (1);
}

fetchrom (up, ad, len) u_short *up; {
	u_char cmd;

	cmd = inb(nec+ds_cmd);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_STOP);
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds0_rbcr0, len&0xff);
	outb (nec+ds0_rbcr1, (len>>8)&0xff);
	outb (nec+ds0_rsar0, ad&0xff);
	outb (nec+ds0_rsar1, (ad>>8)&0xff);
	outb (nec+ds_cmd, DSCM_RREAD|DSCM_PG0|DSCM_START);
	insw (nec+ne_data, up, len/2);
	pausestr ("x",1);
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0) pausestr("fetchrom",0);
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);
}

static recur;
fetchram (up, ad, len) caddr_t up; {
	u_char cmd;

	recur++;
	cmd = inb(nec+ds_cmd);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds0_rbcr0, len&0xff);
	outb (nec+ds0_rbcr1, (len>>8)&0xff);
	outb (nec+ds0_rsar0, ad&0xff);
	outb (nec+ds0_rsar1, (ad>>8)&0xff);
	outb (nec+ds_cmd, DSCM_RREAD|DSCM_PG0|DSCM_START);
	insw (nec+ne_data, up, len/2);
	pausestr ("x",1);
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0) pausestr("fetchram",0);
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);
	recur--;
}

putram (up, ad, len) caddr_t up; {
	u_char cmd;

	recur++;
	cmd = inb(nec+ds_cmd);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	outb (nec+ds0_isr, DSIS_RDC);
	if(len&1) len++;
	outb (nec+ds0_rbcr0, len&0xff);
	outb (nec+ds0_rbcr1, (len>>8)&0xff);
	outb (nec+ds0_rsar0, ad&0xff);
	outb (nec+ds0_rsar1, (ad>>8)&0xff);
	outb (nec+ds_cmd, DSCM_RWRITE|DSCM_PG0|DSCM_START);
	outsw (nec+ne_data, up, len/2);
	pausestr ("x",1);
	while ((inb (nec+ds0_isr) & DSIS_RDC) == 0)
		if(pausestr("putram",0)<0) break;
	outb (nec+ds0_isr, DSIS_RDC);
	outb (nec+ds_cmd, cmd);
	recur--;
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
	printf (" ethernet address %s", ether_sprintf(ns->ns_addr)) ;
	ifp->if_flags = IFF_BROADCAST|IFF_NOTRAILERS;
	ifp->if_init = neinit;
	ifp->if_output = neoutput;
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
	register i; char *cp;

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
	/* Word Transfers, Burst Mode Select, Fifo at 8 bytes */
	outb(nec+ds0_dcr, DSDC_WTS|DSDC_BMS|DSDC_FT1);
	outb(nec+ds0_tcr, 0);
	outb (nec+ds0_rcr, DSRC_MON);
	outb (nec+ds0_tpsr, 0);
	outb(nec+ds0_pstart, RBUF/DS_PGSIZE);
	outb(nec+ds0_pstop, RBUFEND/DS_PGSIZE);
	outb(nec+ds0_bnry, RBUF/DS_PGSIZE);
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG1|DSCM_STOP);
	outb(nec+ds1_curr, RBUF/DS_PGSIZE);
	ns->ns_cur = RBUF/DS_PGSIZE;
	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	outb (nec+ds0_rcr, DSRC_AB);
	outb(nec+ds0_dcr, DSDC_WTS|DSDC_BMS|DSDC_FT1);
	outb (nec+ds0_imr, 0xff);

	ns->ns_if.if_flags |= IFF_RUNNING;
	ns->ns_oactive = 0; ns->ns_mask = ~0;
	if (ns->ns_if.if_snd.ifq_head) nestart(ns);
	splx(s);
}

/*
 * Setup output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 * called only at splimp or interrupt level.
 */
nestart(ns)
	register struct ne_softc *ns;
{
	struct mbuf *m0, *m;
	int buffer;
	int len = 0, i;

	/*
	 * The DS8390 has only one transmit buffer, if it is busy we
	 * must wait until the transmit interrupt completes.
	 */
	outb(nec+ds_cmd,DSCM_NODMA|DSCM_START);

	if (ns->ns_flags & DSF_LOCK)
		return;

	if (inb(nec+ds_cmd) & DSCM_TRANS)
		return;

	if ((ns->ns_if.if_flags & IFF_RUNNING) == 0)
		return;

	IF_DEQUEUE(&ns->ns_if.if_snd, m);
 
	if (m == 0)
		return;

	/*
	 * Copy the mbuf chain into the transmit buffer
	 */

	ns->ns_flags |= DSF_LOCK;	/* prevent entering nestart */
	buffer = TBUF; len = i = 0;
	for (m0 = m; m != 0; m = m->m_next) {
/*int j;*/
		putram(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
		len += m->m_len;
/*for(j=0; i < len;i++,j++) puthex(mtod(m,u_char *)[j]);
printf("|"); */
	}

	/*
	 * If this was a broadcast packet loop it
	 * back because the hardware can't hear its own
	 * transmits.
	 */
	/*if (bcmp((caddr_t)(mtod(m0, struct ether_header *)->ether_dhost),
	   (caddr_t)etherbroadcastaddr,
	   sizeof(etherbroadcastaddr)) == 0) {
		neread(ns, m0);
	} else {
*/
		m_freem(m0);
	/*}*/

	/*
	 * Init transmit length registers, and set transmit start flag.
	 */

#ifdef NEDEBUGx
if(len < 0 || len > 1536)
pg("T Bogus Length %d\n", len);
dprintf(DEXPAND,"snd %d ", len);
#endif
	if (len < 60) len = 60;
	outb(nec+ds0_tbcr0,len&0xff);
	outb(nec+ds0_tbcr1,(len>>8)&0xff);
	outb(nec+ds0_tpsr, TBUF/DS_PGSIZE);
	outb(nec+ds_cmd, DSCM_TRANS|DSCM_NODMA|DSCM_START);
}

#define succ(n) (((n)+1 >= RBUFEND/DS_PGSIZE) ? RBUF/DS_PGSIZE : (n)+1)
#define pred(n) (((n)-1 < RBUF/DS_PGSIZE) ? RBUFEND/DS_PGSIZE-1 : (n)-1)
/*
 * Controller interrupt.
 */
neintr(vec, ppl)
	int vec;
{
	register struct ne_softc *ns = &ne_softc[0];
	u_char cmd,isr;
static cnt;

	/* save cmd, clear interrupt */
	cmd = inb (nec+ds_cmd);
loop:
	isr = inb (nec+ds0_isr);
#ifdef NEDEBUGx
dprintf(DEXPAND,"|ppl %x isr %x ", ppl, isr);
#endif

	outb(nec+ds_cmd,DSCM_NODMA|DSCM_START);
	outb(nec+ds0_isr, isr);


	if (isr & (DSIS_RXE|DSIS_TXE|DSIS_ROVRN))
		log(LOG_ERR, "ne%d: error: isr %x\n", ns-ne_softc, isr/*, DSIS_BITS*/);

#ifdef notdef
	/* receiver ovverun? */
	if (isr & DSIS_ROVRN) {
		u_char pend,lastfree;

		outb(nec+ds_cmd, DSCM_STOP|DSCM_NODMA);
		outb(nec+ds_cmd, DSCM_STOP|DSCM_NODMA|DSCM_PG1);
		pend = inb(nec+ds1_curr);
		outb(nec+ds_cmd, DSCM_STOP|DSCM_NODMA|DSCM_PG0);
		lastfree = inb(nec+ds0_bnry);
#ifdef NEDEBUG
printf("Cur %x pend %x lastfree %x ", ns->ns_cur, pend, lastfree);
#endif
		/* have we wrapped */
		if (lastfree >= RBUFEND/DS_PGSIZE)
			lastfree = RBUF/DS_PGSIZE;
		/* something in the buffer? */
		if (pend != succ(lastfree)) {
			u_char nxt;

			fetchram(&ns->ns_ph,ns->ns_cur*DS_PGSIZE, sizeof(ns->ns_ph));
			ns->ns_ba = ns->ns_cur*DS_PGSIZE+sizeof(ns->ns_ph);

			if (ns->ns_ph.pr_status & DSRS_RPC)
				nerecv (ns);

			nxt = ns->ns_ph.pr_nxtpg ;
#ifdef NEDEBUG
printf("nxt %x ", nxt);
#endif
			/* sanity check */
			if ( nxt >= RBUF/DS_PGSIZE
			&& nxt <= RBUFEND/DS_PGSIZE && nxt <= pend)
				ns->ns_cur = nxt;
			else	ns->ns_cur = nxt = pend;
			lastfree = pred(nxt);
			outb(nec+ds0_bnry, lastfree);
		} else ns->ns_cur = pend;

		outb(nec+ds0_rbcr0,0);
		outb(nec+ds0_rbcr1,0);
		outb(nec+ds0_tcr,DSTC_LB0);
		outb(nec+ds0_rcr, DSRC_MON);
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA);
		outb (nec+ds0_rcr, DSRC_AB);
		outb(nec+ds0_tcr,0);
	}
#endif

	/* receiver error */
	if (isr & DSIS_RXE) {
		/* need to read these registers to clear status */
		(void) inb(nec+ ds0_rsr);
		(void) inb(nec+ 0xD);
		(void) inb(nec + 0xE);
		(void) inb(nec + 0xF);
		ns->ns_if.if_ierrors++;
	}

	if (isr & (DSIS_RX|DSIS_RXE|DSIS_ROVRN)) {
		u_char pend,lastfree;

		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
		pend = inb(nec+ds1_curr);
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		lastfree = inb(nec+ds0_bnry);
#ifdef NEDEBUG
dprintf(DEXPAND,"cur %x pnd %x lfr %x ", ns->ns_cur, pend, lastfree);
#endif
		/* have we wrapped */
		if (lastfree >= RBUFEND/DS_PGSIZE)
			lastfree = RBUF/DS_PGSIZE;
		if (pend < lastfree && ns->ns_cur < pend)
			lastfree = ns->ns_cur;
		else	if (ns->ns_cur > lastfree)
			lastfree = ns->ns_cur;

		/* something in the buffer? */
		while (pend != lastfree) {
			u_char nxt;

			fetchram(&ns->ns_ph,lastfree*DS_PGSIZE,
				sizeof(ns->ns_ph));
			ns->ns_ba = lastfree*DS_PGSIZE+sizeof(ns->ns_ph);

			/* paranoia */
			if (ns->ns_ph.pr_status == DSRS_RPC ||
				ns->ns_ph.pr_status == 0x21)
				nerecv (ns);
#ifdef NEDEBUG
			else  {
printf("cur %x pnd %x lfr %x ", ns->ns_cur, pend, lastfree);
printf("nxt %x len %x ", ns->ns_ph.pr_nxtpg, (ns->ns_ph.pr_sz1<<8)+
	ns->ns_ph.pr_sz0);
pg("Bogus Sts %x ", ns->ns_ph.pr_status);	
			}
#endif

			nxt = ns->ns_ph.pr_nxtpg ;
#ifdef NEDEBUG
dprintf(DEXPAND,"nxt %x ", nxt);
#endif
			/* sanity check */
			if ( nxt >= RBUF/DS_PGSIZE
			&& nxt <= RBUFEND/DS_PGSIZE && nxt <= pend)
				ns->ns_cur = nxt;
			else	ns->ns_cur = nxt = pend;
			lastfree = nxt;
			outb(nec+ds0_bnry, pred(nxt));
			outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG1);
			pend = inb(nec+ds1_curr);
			outb(nec+ds_cmd, DSCM_START|DSCM_NODMA|DSCM_PG0);
		} /*else ns->ns_cur = pend;*/
#ifdef NEDEBUG
dprintf(DEXPAND,"cur %x pnd %x lfR %x ", ns->ns_cur, pend, lastfree);
#endif
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA);
	}
	if (isr & DSIS_TXE) {
		ns->ns_flags &= ~DSF_LOCK;
#ifdef NEDEBUG
dprintf(DEXPAND," clsn");
#endif
		/* need to read these registers to clear status */
		ns->ns_if.if_collisions += inb(nec+ds0_tbcr0);
		ns->ns_if.if_oerrors++;
	}
	if (isr & DSIS_TX) {
#ifdef NEDEBUGx
dprintf(DEXPAND,"tx ");
#endif
		ns->ns_flags &= ~DSF_LOCK;
		++ns->ns_if.if_opackets;
		ns->ns_if.if_collisions += inb(nec+ds0_tbcr0);
	}

	/* receiver ovverun? */
	if (isr & DSIS_ROVRN) {
		outb(nec+ds0_rbcr0, 0);
		outb(nec+ds0_rbcr1, 0);
		outb(nec+ds0_tcr, DSTC_LB0);
		outb(nec+ds0_rcr, DSRC_MON);
		outb(nec+ds_cmd, DSCM_START|DSCM_NODMA);
		outb(nec+ds0_rcr, DSRC_AB);
		outb(nec+ds0_tcr, 0);
	}

	outb (nec+ds_cmd, DSCM_NODMA|DSCM_PG0|DSCM_START);
	nestart(ns);
	outb (nec+ds_cmd, cmd);
	outb (nec+ds0_imr, 0xff);
	isr = inb (nec+ds0_isr);
	if(isr) goto loop;
	
#ifdef NEDEBUG
	if(++cnt % 10 == 0) dprintf(DEXPAND,"\n");
#endif
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
if(len < 60 || len > 1536) {
#ifdef NEDEBUG
pg(DEXPAND,"R Bogus Length %d", len);
#endif
return;
}
	fetchram(ns->ns_pb,ns->ns_ba,min(len,DS_PGSIZE-sizeof(ns->ns_ph)));
#ifdef NEDEBUG
if (!bcmp((caddr_t)ns->ns_pb, (caddr_t)ns->ns_addr, 6)
&& !bcmp((caddr_t)ns->ns_pb, (caddr_t)etherbroadcastaddr, 6)) {
printf("G%x ", ns->ns_cur);
return;
}/* else 
printf("P%x ", ns->ns_cur);*/
#endif
	if(len > DS_PGSIZE-sizeof(ns->ns_ph)) {
		int l = len - (DS_PGSIZE-sizeof(ns->ns_ph)), b ;
		u_char *p = ns->ns_pb + (DS_PGSIZE-sizeof(ns->ns_ph));

#ifdef NEDEBUG
dprintf(DEXPAND,"len %d(%d|", len, p - ns->ns_pb);
#endif
		if(++ns->ns_cur > 0x7f) ns->ns_cur = 0x46;
		b = ns->ns_cur*DS_PGSIZE;
		
		while (l >= DS_PGSIZE) {
			fetchram(p,b,DS_PGSIZE);
			p += DS_PGSIZE; l -= DS_PGSIZE;
		if(++ns->ns_cur > 0x7f) ns->ns_cur = 0x46;
		b = ns->ns_cur*DS_PGSIZE;
#ifdef NEDEBUG
dprintf(DEXPAND,"%d|", p - ns->ns_pb);
#endif
		}
#ifdef NEDEBUG
dprintf(DEXPAND,"%d) ", l);
#endif
		if (l > 0)
			fetchram(p,b,l);
	}
	len -= 
		sizeof(struct ether_header)
		+ sizeof(long);	/* don't forget checksum! */
			

	neread(ns,(caddr_t)(ns->ns_pb), len);
}

pausestr(s,n) char *s; {
static downcnt;

	if(n) { downcnt = 0xffff; return(0); }
	if(--downcnt > 0) return(0);
#ifdef NEDEBUG
	pg(" <%s> recur %d sts %x ", s, recur, inb (nec+ds0_isr));
#endif
	return(-1);
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

	if (off) {
		struct ifnet *ifp;

		ifp = *(mtod(m, struct ifnet **));
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
		*(mtod(m, struct ifnet **)) = ifp;
	}
	switch (eh->ether_type) {
#ifdef INET
	case ETHERTYPE_IP:
		/*if (ns->ns_ac.ac_ipaddr == 0) goto raw;*/
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput(&ns->ns_ac, m);
		return;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif
	default:
		m_freem(m);
		return;
	}

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		return;
	}
	IF_ENQUEUE(inq, m);
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
neoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
	u_char edst[6];
	struct in_addr idst;
	register struct ne_softc *ns = &ne_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	register int off;
	extern struct ifnet loif;
        struct mbuf *mcopy = (struct mbuf *)0;
	int usetrailers;

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}

	switch (dst->sa_family) {
#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		if (!arpresolve(&ns->ns_ac, m, &idst, edst, &usetrailers))
			return (0);	/* if not yet resolved */
		if (!bcmp((caddr_t)edst, (caddr_t)etherbroadcastaddr,
		    sizeof(edst)))
                        mcopy = m_copy(m, 0, (int)M_COPYALL);
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;


		/* need per host negotiation */
		if (usetrailers && off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = ETHERTYPE_TRAIL + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = ntohs((u_short)ETHERTYPE_IP);
			*(mtod(m, u_short *) + 1) = ntohs((u_short)m->m_len);
			goto gottrailertype;
		}
		type = ETHERTYPE_IP;
		off = 0;
		goto gottype;
#endif
#ifdef NS
	case AF_NS:
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		    (caddr_t)edst, sizeof (edst));

		if (!bcmp((caddr_t)edst, (caddr_t)&ns_broadhost,
			sizeof(edst))) {

				mcopy = m_copy(m, 0, (int)M_COPYALL);
		} else if (!bcmp((caddr_t)edst, (caddr_t)&ns_thishost,
			sizeof(edst))) {

				return(looutput(&loif, m, dst));
		}
		type = ETHERTYPE_NS;
		off = 0;
		goto gottype;
#endif

	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		goto gottype;

	default:
		printf("ne%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottrailertype:
	/*
	 * Packet to be sent as trailer: move first packet
	 * (control information) to end of chain.
	 */
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;
	m0 = m;

gottype:
	/*
	 * Add local net header.  If no space in first mbuf,
	 * allocate another.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof (struct ether_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ether_header);
	} else {
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}
	eh = mtod(m, struct ether_header *);
	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof (edst));
	bcopy((caddr_t)ns->ns_addr, (caddr_t)eh->ether_shost,
		sizeof (eh->ether_shost));
	eh->ether_type = htons((u_short)type);

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		m_freem(m);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	nestart(ns);
	splx(s);
        return (mcopy ? looutput(&loif, mcopy, dst) : 0);

bad:
	m_freem(m0);
	if (mcopy)
		m_freem(mcopy);
	return (error);
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

	cp = buf + sizeof(struct ether_header);
	top = 0;
	mp = &top;
	while (totlen > 0) {
		u_char *mcp;

		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
			cp = buf + off + sizeof (struct ether_header);
		} else
			len = totlen;
		if (ifp)
			len += sizeof(ifp);
		if (len >= NBPG) {
			MCLGET(m);
			if (m->m_len == CLBYTES)
				m->m_len = len = MIN(len, CLBYTES);
			else
				m->m_len = len = MIN(MLEN, len);
		} else {
			m->m_len = len = MIN(MLEN, len);
			m->m_off = MMINOFF;
		}
		mcp = mtod(m, u_char *);
		if (ifp) {
			/*
			 * Prepend interface pointer to first mbuf.
			 */
			*(mtod(m, struct ifnet **)) = ifp;
			mcp += sizeof(ifp);
			len -= sizeof(ifp);
			ifp = (struct ifnet *)0;
		}
		bcopy(cp, mcp, len);
		cp += len ; mcp += len ;
		*mp = m;
		mp = &m->m_next;
		if (off == 0) {
			totlen -= len;
			continue;
		}
		off += len;
		if (off == totlen) {
			cp = buf + sizeof (struct ether_header);
			off = 0;
			totlen = off0;
		}
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Map a chain of mbufs onto a network interface
 * in preparation for an i/o operation.
 * The argument chain of mbufs includes the local network
 * header which is copied to be in the mapped, aligned
 * i/o space.
 */
neput(cp, m)
	register caddr_t cp;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register int i;
	int x, cc = 0, t;
	caddr_t dp;

	while (m) {
		dp = mtod(m, char *);
		bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
		cp += m->m_len;
		cc += m->m_len;
		MFREE(m, mp);
		m = mp;
	}
	return (max(cc, ETHERMIN + sizeof(struct ether_header)));
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

		switch (ifa->ifa_addr.sa_family) {
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
			outb(nec+ds_cmd,DSCM_STOP|DSCM_NODMA);
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

nesetaddr(ifp, sin)
	register struct ifnet *ifp;
	register struct sockaddr_in *sin;
{
#ifdef notdef
	ifp->if_addr = *(struct sockaddr *)sin;
	ifp->if_net = in_netof(sin->sin_addr);
	ifp->if_host[0] = in_lnaof(sin->sin_addr);
	if (nepaddr[ifp->if_unit][0] == '3')
		nepaddr[ifp->if_unit][0] = ifp->if_host[0] << 1;
	sin = (struct sockaddr_in *)&ifp->if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = in_makeaddr(ifp->if_net, INADDR_ANY);
	ifp->if_flags |= IFF_BROADCAST;
#endif
}
#endif
