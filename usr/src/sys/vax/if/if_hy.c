/*	@(#)if_hy.c	6.4 (Berkeley) %G% */

/*
 * 4.2 BSD Unix Kernel - Vax Network Interface Support
 *
 * $Header: if_hy.c,v 10.1 84/07/22 21:02:56 steveg Exp $
 * $Locker:  $
 *
 * Modifications from Berkeley 4.2 BSD
 * Copyright (c) 1983, Tektronix Inc.
 * All Rights Reserved
 *
 * $Log:	if_hy.c,v $
 *	Revision 10.1  84/07/22  21:02:56  steveg
 *	define PI13 (moved from if_hyreg.h, somehow got dropped in the process)
 *	rework hywatch to check for power fails first
 *	
 *	Revision 10.0  84/06/30  19:54:27  steveg
 *	Big Build
 *	
 *	Revision 3.17  84/06/20  19:20:28  steveg
 *	increment hy_ntime in hywatch
 *	print out state name, csr, last command, and hy_flags when watchdog timer
 *	expires
 *	
 *	Revision 3.16  84/06/20  19:09:34  steveg
 *	turn on continuous logging by default
 *	
 *	Revision 3.15  84/05/30  22:19:09  steveg
 *	changes to reflect new layout ot statistics data
 *	
 *	Revision 3.14  84/05/30  19:25:15  steveg
 *	move driver states to if_hy.h so log printing programs can use them
 *	
 *	Revision 3.13  84/05/30  17:13:26  steveg
 *	make it compile
 *	
 *	Revision 3.12  84/05/30  13:46:16  steveg
 *	rework logging
 *	
 *	Revision 3.11  84/05/18  19:35:02  steveg
 *	clear IFF_RUNNING and IFF_UP on unibus reset to force resource allocation
 *	by the init routine
 *	
 *	Revision 3.10  84/05/04  12:14:44  steveg
 *	more rework to make it actually work under 4.2
 *	
 *	Revision 3.9  84/05/01  23:34:52  steveg
 *	fix typo so it compiles (unit -> ui->ui_unit)
 *	
 *	Revision 3.8  84/05/01  23:18:30  steveg
 *	changes after talking with rickk
 *	- check power off more closely
 *	- support remote loopback through A710 adapters
 *	- IMPLINK -> HYLINK
 *	- return EHOSTUNREACH on hyroute failure
 *	- bump if_collisions on abnormal interrupts that aren't input or output
 *	
 *
 */


#include "hy.h"
#if NHY > 0

/*
 * Network Systems Copropration Hyperchanel interface
 */
#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"
#include "ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

/*
 * configuration specific paramters
 *	- change as appropriate for particular installaions
 */
#define	HYROUTE
#define	HYELOG
#define	HYLOG
#define	HYMTU	1100
#define PI13

#ifdef	DEBUG
#define	HYLOG
#endif

#include "if_hy.h"
#include "if_hyreg.h"
#include "if_uba.h"

int	hyprobe(), hyattach(), hyinit(), hyioctl();
int	hyoutput(), hyreset(), hywatch();
struct	uba_device *hyinfo[NHY];
u_short hystd[] = { 0772410, 0 };
struct	uba_driver hydriver =
	{ hyprobe, 0, hyattach, 0, hystd, "hy", hyinfo };

/*
 * Hyperchannel software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * hy_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	hy_softc {
	struct	ifnet hy_if;		/* network-visible interface */
	struct	ifuba hy_ifuba;		/* UNIBUS resources */
	short	hy_flags;		/* flags */
	short	hy_state;		/* driver state */
	u_short	hy_host;		/* local host number */
	struct	in_addr hy_addr;	/* internet address */
	int	hy_olen;		/* packet length on output */
	int	hy_lastwcr;		/* last command's word count */
	short	hy_savedstate;		/* saved for reissue after status */
	short	hy_savedcmd;		/* saved command for reissue */
	int	hy_savedcount;		/* saved byte count for reissue */
	int	hy_savedaddr;		/* saved unibus address for reissue */
	int	hy_ntime;		/* number of timeouts since last cmd */
	int	hy_retry;		/* retry counter */
	struct	hy_stat hy_stat;	/* statistics */
	struct	hy_status hy_status;	/* status */
} hy_softc[NHY];

#ifdef HYELOG
u_long	hy_elog[HYE_SIZE];
#endif

#ifdef HYLOG
struct hy_log hy_log;
#endif

#ifdef HYROUTE
struct hy_route hy_route[NHY];
#endif

#ifdef DEBUG
#define printL	printf
#define printD	if (hy_debug_flag) printf
int	hy_debug_flag = 0;
/*
 * hy_nodebug bit 0x01	set hy_debug_flag on hycancel
 * hy_nodebug bit 0x02	set hy_debug_flag on command reissue
 * hy_nodebug bit 0x04	set hy_debug_flag on abnormal interrupt
 */
int	hy_nodebug = 0x0;
#endif

#define SCANINTERVAL	10	/* seconds */
#define MAXINTERVAL	20	/* seconds (max action) */

/*
 * Cause a device interrupt.  This code uses a buffer starting at
 * location zero on the unibus (which is already mapped by the
 * autoconfigure code in the kernel).
 */
hyprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct hydevice *addr = (struct hydevice *) reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	hyint(0);
#endif
	/*
	 * request adapter status to a buffer starting at unibus location 0
	 */
	addr->hyd_bar = 0;
	addr->hyd_wcr = -((sizeof(struct hy_status) + 1) >> 1);
	addr->hyd_dbuf = HYF_STATUS;
#ifdef PI13
	addr->hyd_csr |= S_GO | S_IE | S_IATTN;
#else
	addr->hyd_csr |= S_GO | S_IE;
#endif
	DELAY(10000);
#ifdef PI13
	addr->hyd_csr |= S_CLRINT;	/* clear any stacked interrupts */
#endif
	addr->hyd_csr &= ~(S_IE | S_CLRINT);	/* disable further interrupts */
	return(sizeof(struct hydevice));
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
hyattach(ui)
	struct uba_device *ui;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];
	register struct ifnet *ifp = &is->hy_if;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "hy";
	ifp->if_mtu = HYMTU;
	is->hy_state = STARTUP;		/* don't allow state transitions yet */
	ifp->if_init = hyinit;
	ifp->if_ioctl = hyioctl;
	ifp->if_output = hyoutput;
	ifp->if_reset = hyreset;
	ifp->if_watchdog = hywatch;
	ifp->if_timer = SCANINTERVAL;
	is->hy_ifuba.ifu_flags = UBA_CANTWAIT;
#ifdef notdef
	is->hy_ifuba.ifu_flags |= UBA_NEEDBDP;
#endif
	if_attach(ifp);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
hyreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
	register struct hy_softc *is;

	if (unit >= NHY || (ui = hyinfo[unit]) == 0 || ui->ui_alive == 0 ||
	  ui->ui_ubanum != uban)
		return;
	printf(" hy%d", unit);
	is = &hy_softc[unit];		/* force unibus resource allocation */
	is->hy_if.if_flags &= ~(IFF_UP|IFF_RUNNING);
	hyinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
hyinit(unit)
	int unit;
{
	register struct hy_softc *is = &hy_softc[unit];
	register struct uba_device *ui = hyinfo[unit];
	register struct mbuf *m;
	int s;

	if (is->hy_if.if_addrlist == 0)		/* address still unknown */
		return;
	if (is->hy_if.if_flags & IFF_RUNNING)	/* just reset the device */
		goto justreset;
	if (if_ubainit(&is->hy_ifuba, ui->ui_ubanum,
	    sizeof (struct hym_hdr), (int)btoc(HYMTU)) == 0) { 
#ifdef DEBUG
		if (hy_nodebug & 4)
			hy_debug_flag = 1;
#endif
		printf("hy%d: can't initialize\n", unit);
		is->hy_if.if_flags &= ~IFF_UP;
		return;
	}
	is->hy_if.if_flags |= IFF_RUNNING;

justreset:
	/*
	 * remove any left over outgoing messages, reset the hardware and
	 * start the state machine
	 */
	s = splimp();
#ifdef HYLOG
	hylog(HYL_RESET, 0, (char *)0);
#endif
	is->hy_state = IDLE;
	is->hy_flags = RQ_STATUS | RQ_STATISTICS | RQ_MARKUP;
	is->hy_retry = 0;
	for(;;) {
		IF_DEQUEUE(&is->hy_if.if_snd, m);
		if (m != NULL)
			m_freem(m);
		else
			break;
	}
	hycancel(ui);		/* also bumps the state machine */
	splx(s);
}

/*
 * Issue a command to the adapter
 */
hystart(ui, cmd, count, ubaddr)
	struct uba_device *ui;
	int cmd, count, ubaddr;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];
	register struct hydevice *addr = (struct hydevice *)ui->ui_addr;

#ifdef DEBUG
	printD("hy%d: hystart cmd = 0x%x count=%d ubaddr=0x%x\n",
		ui->ui_unit, cmd, count, ubaddr);
	printD("hy%d: - csr = 0x%b, bar = 0x%x, wcr = 0x%x\n",
		ui->ui_unit, addr->hyd_csr, HY_CSR_BITS, addr->hyd_bar,
		addr->hyd_wcr);
#endif
	if (((is->hy_flags & RQ_REISSUE) == 0) &&
	  (cmd != HYF_STATUS) && (cmd != HYF_END_OP) && (cmd != HYF_RSTATS)) {
		is->hy_savedstate = is->hy_state;
		is->hy_savedcmd = cmd;
		is->hy_savedcount = count;
		is->hy_savedaddr = ubaddr;
	}
#ifdef PI13
	if (addr->hyd_csr & S_POWEROFF) {
		printf("hy%d: \"Soft\" Adapter Power Failure (hystart)\n", ui->ui_unit);
		addr->hyd_csr |= S_POWEROFF;
		DELAY(100);
		if (addr->hyd_csr & S_POWEROFF) {
			printf( "hy%d: \"Hard\" Adapter Power Failure, Network Shutdown (hystart)\n", ui->ui_unit);
			if_down(&is->hy_if);
			is->hy_if.if_flags &= ~IFF_UP;
			is->hy_state = STARTUP;
		} else {
			printf("hy%d: Adapter Power Restored (hystart)\n", ui->ui_unit);
		}
		return;
	}
#endif
	addr->hyd_bar = ubaddr & 0xffff;
	addr->hyd_wcr = is->hy_lastwcr = -((count+1) >> 1);
	addr->hyd_dbuf = cmd;
#ifdef PI13
	addr->hyd_csr = ((ubaddr >> XBASHIFT) & S_XBA) | S_GO | S_IE | S_IATTN;
#else
	addr->hyd_csr = ((ubaddr >> XBASHIFT) & S_XBA) | S_GO | S_IE;
#endif
#ifdef DEBUG
	printD("hy%d: exit hystart - csr = 0x%b, bar = 0x%x, wcr = 0x%x\n",
		ui->ui_unit, addr->hyd_csr, HY_CSR_BITS, addr->hyd_bar,
		addr->hyd_wcr);
#endif
#ifdef HYLOG
	{
		struct {
			u_char	hcmd;
			u_char	hstate;
			short	hcount;
		} hcl;

		hcl.hcmd = cmd;
		hcl.hstate = is->hy_state;
		hcl.hcount = count;
		hylog(HYL_CMD, sizeof(hcl), (char *)&hcl);
	}
#endif
	is->hy_ntime = 0;
}

int hyint_active = 0;		/* set during hy interrupt */
/*
 * Hyperchannel interface interrupt.
 *
 * An interrupt can occur for many reasons.  Examine the status of
 * the hyperchannel status bits to determine what to do next.
 *
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
hyint(unit)
	int unit;
{
	register struct hy_softc *is = &hy_softc[unit];
	register struct uba_device *ui = hyinfo[unit];
	register struct hydevice *addr = (struct hydevice *)ui->ui_addr;

	if (hyint_active)
		panic("RECURSIVE HYPERCHANNEL INTERRUPT");
	hyint_active++;
#ifdef DEBUG
	printD("hy%d: hyint enter - csr = 0x%b, bar = 0x%x, wcr = 0x%x\n",
		unit, addr->hyd_csr, HY_CSR_BITS, addr->hyd_bar, addr->hyd_wcr);
#endif
#ifdef HYLOG
logit:
	{
		struct {
			u_char	hstate;
			u_char	hflags;
			short	hcsr;
			short	hwcr;
		} hil;
		hil.hstate = is->hy_state;
		hil.hflags = is->hy_flags;
		hil.hcsr = addr->hyd_csr;
		hil.hwcr = addr->hyd_wcr;
		hylog(HYL_INT, sizeof(hil), (char *)&hil);
	}
#endif
	if (HYS_ERROR(addr) && ((addr->hyd_csr & S_ATTN) == 0)) {
		/*
		 * Error bit set, some sort of error in the interface.
		 *
		 * The adapter sets attn on command completion so that's not
		 * a real error even though the interface considers it one.
		 */
#ifdef DEBUG
		if (hy_nodebug & 4)
			hy_debug_flag = 1;
#endif
		printf("csr = 0x%b\nbar = 0x%x\nwcr = 0x%x\n",
			addr->hyd_csr, HY_CSR_BITS, addr->hyd_bar,
			addr->hyd_wcr);
		if (addr->hyd_csr & S_NEX) {
			printf("hy%d: NEX - Non Existant Memory\n", unit);
#ifdef PI13
			addr->hyd_csr |= S_NEX;  /* as per PI13 manual */
#else
			addr->hyd_csr &= ~S_NEX;
#endif
			hycancel(ui);
#ifdef PI13
		} else if (addr->hyd_csr & S_POWEROFF) {
			printf("hy%d: \"Soft\" Adapter Power Failure (hyint)\n", unit);
			addr->hyd_csr |= S_POWEROFF;
			DELAY(100);
			if (addr->hyd_csr & S_POWEROFF) {
				printf( "hy%d: \"Hard\" Adapter Power Failure, Network Shutdown (hyint)\n", unit);
				if_down(&is->hy_if);
				is->hy_if.if_flags &= ~IFF_UP;
				is->hy_state = STARTUP;
			} else {
				printf("hy%d: Adapter Power Restored (hyint)\n", unit);
			}
#endif
		} else {
			printf("hy%d:  BAR overflow\n", unit);
			hycancel(ui);
		}
	} else if (HYS_NORMAL(addr)) {
		/*
		 * Normal interrupt, bump state machine unless in state
		 * waiting and no data present (assumed to be word count
		 * zero interrupt or other hardware botch).
		 */
		if (is->hy_state != WAITING || HYS_RECVDATA(addr))
			hyact(ui);
	} else if (HYS_ABNORMAL(addr)) {
		/*
		 * Abnormal termination.
		 * bump error counts, retry the last function
		 * 'MAXRETRY' times before kicking the bucket.
		 *
		 * Don't reissue the cmd if in certain states, abnormal
		 * on a reissued cmd or max retry exceeded.
		 */
#ifdef HYLOG
		if (hy_log.hyl_enable != hy_log.hyl_onerr) {
			hy_log.hyl_enable = hy_log.hyl_onerr;
			goto logit;
		}
#endif
#ifdef DEBUG
		if (hy_nodebug & 4)
			hy_debug_flag = 1;
		printD("hy%d: abnormal interrupt, driver state \"%s\" (%d)\n",
			unit, hy_state_names[is->hy_state], is->hy_state);
		printD("\tflags 0x%x olen %d lastwcr %d retry %d\n",
			is->hy_flags, is->hy_olen, is->hy_lastwcr, is->hy_retry);
		printD("\tsaved: state %d count %d cmd 0x%x ptr 0x%x\n",
			is->hy_savedstate, is->hy_savedcount,
			is->hy_savedaddr, is->hy_savedcmd);
#endif
#ifdef PI13
		addr->hyd_csr &= ~S_C;  /* clear the damned PI-13 */
#endif
		if (is->hy_state == XMITSENT || is->hy_state == XMITDATASENT)
			is->hy_if.if_oerrors++;
		else if (is->hy_state == RECVSENT || is->hy_state == RECVDATASENT)
			is->hy_if.if_ierrors++;
		else
			is->hy_if.if_collisions++;	/* other errors */
		if (is->hy_state == XMITDATASENT ||
		    is->hy_state == RECVSENT ||
		    is->hy_state == RECVDATASENT ||
		    (is->hy_flags & RQ_REISSUE) != 0 || is->hy_retry > MAXRETRY)
			hycancel(ui);
		else {
#ifdef DEBUG
			if (hy_nodebug & 2)
				hy_debug_flag = 1;
#endif
			is->hy_retry++;
			is->hy_flags |= RQ_ENDOP | RQ_STATUS | RQ_REISSUE;
			is->hy_state = IDLE;
			hyact(ui);
		}
	} else {
		/*
		 * Interrupt is neither normal, abnormal, or interface error.
		 * Ignore it. It's either stacked or a word count 0.
		 */
#ifdef HYLOG
		if (hy_log.hyl_enable != hy_log.hyl_onerr) {
			hy_log.hyl_enable = hy_log.hyl_onerr;
			goto logit;
		}
#endif
#ifdef DEBUG
		printD("hy%d: possible stacked interrupt ignored\n", unit);
#endif
	}
#ifdef DEBUG
	printD("hy%d: hyint exit\n\n", unit);
#endif
	hyint_active = 0;

}

int hyoutprint = 0;

/*
 * Encapsulate a packet of type family for the local net.
 */
hyoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct hym_hdr *hym;
	register struct mbuf *m;
	register char *mp;
#ifdef HYROUTE
	register struct hy_route *r = &hy_route[ifp->if_unit];
#endif
	int dlen;	/* packet size, incl hardware header, but not sw header */
	int error = 0;
	int s;

	/*
	 * Calculate packet length for later deciding whether it will fit
	 * in a message proper or we also need associated data.
	 */
	dlen = 0;
	for (m = m0; m; m = m->m_next)
		dlen += m->m_len;
	m = m0;
	if (dst->sa_family == AF_HYLINK) {	/* don't add header */
		dlen -= HYM_SWLEN;
		goto headerexists;
	}

	/*
	 * Add the software and hardware hyperchannel headers.
	 * If there's not enough space in the first mbuf, allocate another.
	 * If that should fail, drop this sucker.
	 * No extra space for headers is allocated.
	 */
	mp = mtod(m, char *);	/* save pointer to real message */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof(struct hym_hdr) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m = m0;
			error = ENOBUFS;
			goto drop;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct hym_hdr);
	} else {
		m->m_off -= sizeof(struct hym_hdr);
		m->m_len += sizeof(struct hym_hdr);
	}

	dlen += sizeof(struct hym_hdr) - HYM_SWLEN;

	hym = mtod(m, struct hym_hdr *);

	bzero((caddr_t)hym, sizeof(struct hym_hdr));

	switch(dst->sa_family) {

#ifdef INET
	case AF_INET: {
		int i;

		/*
		 * if loopback address, swizzle ip header so when
		 * it comes back it looks like it was addressed to us
		 */
		i = hyroute(ifp, (u_long)in_lnaof(((struct sockaddr_in *)dst)->sin_addr), hym);
		if (i < 0)
			goto notfound;
		if (i > 0) {
			struct in_addr temp;

			temp.s_addr = ((struct ip *)mp)->ip_dst.s_addr;
			((struct ip *)mp)->ip_dst.s_addr = ((struct ip *)mp)->ip_src.s_addr;
			((struct ip *)mp)->ip_src.s_addr = temp.s_addr;
		}
		/*
		 * If entire packet won't fit in message proper, just
		 * send hyperchannel hardware header and ip header in
		 * message proper.
		 *
		 * This insures that the associated data is at least a
		 * TCP/UDP header in length and thus prevents potential
		 * problems with very short word counts.
		 */
		if (dlen > MPSIZE)
			hym->hym_mplen = sizeof(struct hy_hdr) + (((struct ip *)mp)->ip_hl << 2);
		hym->hym_type = HYLINK_IP;
		break;
	}
#endif

	default:
		printf("hy%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto drop;
	}


headerexists:

	/*
	 * insure message proper is below the maximum
	 */
	if (hym->hym_mplen > MPSIZE || (dlen > MPSIZE && hym->hym_mplen == 0))
		hym->hym_mplen = MPSIZE;

	hym->hym_from = htons(hy_softc[ifp->if_unit].hy_host);
	if (hym->hym_mplen)
		hym->hym_ctl |= H_ASSOC;
	else
		hym->hym_ctl &= ~H_ASSOC;
	if (hyoutprint) printf("hy%d: output mplen=%x ctl=%x access=%x to=%x from=%x param=%x type=%x\n",
		ifp->if_unit, hym->hym_mplen, hym->hym_ctl,
		hym->hym_access, hym->hym_to, hym->hym_from,
		hym->hym_param, hym->hym_type);
#ifdef DEBUG
	printD("hy%d: output mplen=%x ctl=%x access=%x to=%x from=%x param=%x type=%x\n",
		ifp->if_unit, hym->hym_mplen, hym->hym_ctl,
		hym->hym_access, hym->hym_to, hym->hym_from,
		hym->hym_param, hym->hym_type);
#endif
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		error = ENOBUFS;
		splx(s);
		goto drop;
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if (hy_softc[ifp->if_unit].hy_state == WAITING)
		hyact(hyinfo[ifp->if_unit]);
	splx(s);
	return (0);
notfound:
	error = EHOSTUNREACH;
drop:
	m_freem(m);
	return (error);
}

int
hyroute(ifp, dest, hym)
	register struct ifnet *ifp;
	u_long dest;
	register struct hym_hdr *hym;
{
#ifdef HYROUTE
	register struct hy_route *rt = &hy_route[ifp->if_unit];
	register struct hyr_hash *rhash;
	register int i;
#endif

	hym->hym_param = 0;
#ifdef HYROUTE
	if (rt->hyr_lasttime != 0) {
		i = HYRHASH(dest);
		rhash = &rt->hyr_hash[i];
		i = 0;
		while (rhash->hyr_key != dest) {
			if (rhash->hyr_flags == 0 || i > HYRSIZE)
				return(-1);
			rhash++; i++;
			if (rhash >= &rt->hyr_hash[HYRSIZE])
				rhash = &rt->hyr_hash[0];
		}
		if (rhash->hyr_flags & HYR_GATE) {
			i = rhash->hyr_nextgate;
			if (i >= rhash->hyr_egate)
				rhash->hyr_nextgate = rhash->hyr_pgate;
			else
				rhash->hyr_nextgate++;
			rhash = &rt->hyr_hash[rt->hyr_gateway[i]];
			if ((rhash->hyr_flags & HYR_DIR) == 0)
				return(-1);
		} else if (rhash->hyr_flags & HYR_LOOP) {
			hym->hym_param = H_LOOPBK;	/* adapter loopback */
		} else if (rhash->hyr_flags & HYR_RLOOP) {
			hym->hym_param = H_RLOOPBK;	/* A710 remote loopback */
		}
		hym->hym_ctl = rhash->hyr_ctl;
		hym->hym_access = rhash->hyr_access;
		hym->hym_to = rhash->hyr_dst;
	} else {
#endif
		hym->hym_ctl = H_XTRUNKS | H_RTRUNKS;
		hym->hym_access = 0;
		hym->hym_to = htons((u_short)dest);
		if (dest & 0x010000)
			hym->hym_param = H_LOOPBK;	/* adapter loopback */
		else if (dest & 0x020000)
			hym->hym_param = H_RLOOPBK;	/* A710 remote loopback */
#ifdef HYROUTE
	}
#endif

	if (hym->hym_param == 0)
		return(0);
	else
		return(1);
}

hyact(ui)
	register struct uba_device *ui;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];
	register struct hydevice *addr = (struct hydevice *)ui->ui_addr;

actloop:
#ifdef DEBUG
	printD("hy%d: hyact, enter state \"%s\"\n", ui->ui_unit,
		hy_state_names[is->hy_state]);
#endif
	switch (is->hy_state) {

	case STARTUP:
		goto endintr;

	case IDLE: {
		register rq = is->hy_flags;

		if (rq & RQ_STATUS) {
			is->hy_flags &= ~RQ_STATUS;
			is->hy_state = STATSENT;
			hystart(ui, HYF_STATUS, sizeof (is->hy_status),
			    is->hy_ifuba.ifu_r.ifrw_info);
		} else if (rq & RQ_ENDOP) {
			is->hy_flags &= ~RQ_ENDOP;
			is->hy_state = ENDOPSENT;
			hystart(ui, HYF_END_OP, 0, 0);
		} else if (rq & RQ_STATISTICS) {
			is->hy_flags &= ~RQ_STATISTICS;
			is->hy_state = RSTATSENT;
			hystart(ui, HYF_RSTATS, sizeof (is->hy_stat),
			    is->hy_ifuba.ifu_r.ifrw_info);
		} else if (HYS_RECVDATA(addr)) {
			is->hy_state = RECVSENT;
			is->hy_retry = 0;
			hystart(ui, HYF_INPUTMSG, MPSIZE, is->hy_ifuba.ifu_r.ifrw_info + HYM_SWLEN);
		} else if (rq & RQ_REISSUE) {
			is->hy_flags &= ~RQ_REISSUE;
			is->hy_state = is->hy_savedstate;
#ifdef DEBUG
			printD("hy%d: reissue cmd=0x%x count=%d",
			  ui->ui_unit, is->hy_savedcmd, is->hy_savedcount);
			printD(" ubaddr=0x%x retry=%d\n",
			  is->hy_savedaddr, is->hy_retry);
#endif
			hystart(ui, is->hy_savedcmd, is->hy_savedcount,
			    is->hy_savedaddr);
		} else {
			register struct mbuf *m;

			IF_DEQUEUE(&is->hy_if.if_snd, m);
			if (m != NULL) {
				register struct hym_hdr *hym;
				register int mplen;
				register int cmd;

				is->hy_state = XMITSENT;
				is->hy_retry = 0;
				hym = mtod(m, struct hym_hdr *);
#ifdef HYLOG
				hylog(HYL_XMIT, sizeof(struct hym_hdr),
				    (char *)hym);
#endif
				mplen = hym->hym_mplen;
				if (hym->hym_to_adapter == hym->hym_from_adapter)
					cmd = HYF_XMITLOCMSG;
				else
					cmd = HYF_XMITMSG;
#ifdef DEBUG
				printD("hy%d: hym_hdr = ", ui->ui_unit);
				if (hy_debug_flag)
					hyprintdata((char *)hym,
					    sizeof (struct hym_hdr));
#endif
				is->hy_olen = if_wubaput(&is->hy_ifuba, m) - HYM_SWLEN;
				if (is->hy_ifuba.ifu_flags & UBA_NEEDBDP)
					UBAPURGE(is->hy_ifuba.ifu_uba,
						is->hy_ifuba.ifu_w.ifrw_bdp);
#ifdef DEBUG
				printD(
		"hy%d: sending packet (mplen = %d, hy_olen = %d) data = ",
					ui->ui_unit, mplen, is->hy_olen);
				if (hy_debug_flag)
					hyprintdata(
					    is->hy_ifuba.ifu_w.ifrw_addr,
					    is->hy_olen + HYM_SWLEN);
#endif
				if (mplen == 0) {
					is->hy_flags &= ~RQ_XASSOC;
					mplen = is->hy_olen;
				} else {
					is->hy_flags |= RQ_XASSOC;
				}
				hystart(ui, cmd, mplen, is->hy_ifuba.ifu_w.ifrw_info + HYM_SWLEN);
			} else if (rq & RQ_MARKDOWN) {
				is->hy_flags &= ~(RQ_MARKUP | RQ_MARKDOWN);
				is->hy_state = MARKPORT;
				is->hy_retry = 0;
				/*
				 * Port number is taken from status data
				 */
				hystart(ui,
				 (int)(HYF_MARKP0|(PORTNUM(&is->hy_status)<<2)),
				 0, 0);
			} else if (rq & RQ_MARKUP) {
				register struct ifnet *ifp = &is->hy_if;

				is->hy_flags &= ~RQ_MARKUP;
				is->hy_retry = 0;
				/*
				 * Fill in the host number
				 * from the status buffer
				 */
				printf(
	"hy%d: unit number 0x%x port %d type %x microcode level 0x%x\n",
					ui->ui_unit,
					is->hy_stat.hyc_uaddr,
					PORTNUM(&is->hy_status),
					(is->hy_stat.hyc_atype[0]<<8) |
						is->hy_stat.hyc_atype[1],
					is->hy_stat.hyc_atype[2]);

				is->hy_host =
				  (is->hy_stat.hyc_uaddr << 8) |
					PORTNUM(&is->hy_status);
				ifp->if_flags |= IFF_UP;
#ifdef HYLOG
				hylog(HYL_UP, 0, (char *)0);
#endif
			} else {
				is->hy_state = WAITING;
				is->hy_retry = 0;
				hystart(ui, HYF_WAITFORMSG, 0, 0);
			}
		}
		break;
	}

	case STATSENT:
		bcopy(is->hy_ifuba.ifu_r.ifrw_addr, (caddr_t)&is->hy_status,
		  sizeof (struct hy_status));
#ifdef DEBUG
		printD("hy%d: status - %x %x %x %x %x %x %x %x\n",
			ui->ui_unit, is->hy_status.hys_gen_status,
			is->hy_status.hys_last_fcn,
			is->hy_status.hys_resp_trunk,
			is->hy_status.hys_status_trunk,
			is->hy_status.hys_recd_resp,
			is->hy_status.hys_error,
			is->hy_status.hys_caddr,
			is->hy_status.hys_pad);
#endif
		is->hy_state = IDLE;
#ifdef HYLOG
		hylog(HYL_STATUS, sizeof (struct hy_status),
			(char *)&is->hy_status);
#endif
#ifdef HYELOG
		{
			register int i;
			
			i = is->hy_status.hys_error;
			if (i > HYE_MAX)
				i = HYE_MAX;
			switch (is->hy_status.hys_last_fcn) {
				case HYF_XMITLOCMSG:
					i += HYE_MAX+1;	/* fall through */
				case HYF_XMITLSTDATA:
					i += HYE_MAX+1;	/* fall through */
				case HYF_XMITMSG:
					i += HYE_MAX+1;
			}
			hy_elog[i]++;
		}
#endif
		break;

	case RSTATSENT: {
		register struct hy_stat *p =
			(struct hy_stat *)is->hy_ifuba.ifu_r.ifrw_addr;

		bcopy((caddr_t)p, (caddr_t)&is->hy_stat, sizeof(struct hy_stat));
#ifdef DEBUG

		printD("hy%d: statistics - df0 %d df1 %d df2 %d df3 %d\n",
			ui->ui_unit,
			(is->hy_stat.hyc_df0[0]<<16) | (is->hy_stat.hyc_df0[1]<<8) | is->hy_stat.hyc_df0[2],
			(is->hy_stat.hyc_df1[0]<<16) | (is->hy_stat.hyc_df1[1]<<8) | is->hy_stat.hyc_df1[2],
			(is->hy_stat.hyc_df2[0]<<16) | (is->hy_stat.hyc_df2[1]<<8) | is->hy_stat.hyc_df2[2],
			(is->hy_stat.hyc_df3[0]<<16) | (is->hy_stat.hyc_df3[1]<<8) | is->hy_stat.hyc_df3[2]);
		printD("	ret0 %d ret1 %d ret2 %d ret3 %d\n",
			(is->hy_stat.hyc_ret0[0]<<16) | (is->hy_stat.hyc_ret0[1]<<8) | is->hy_stat.hyc_ret0[2],
			(is->hy_stat.hyc_ret1[0]<<16) | (is->hy_stat.hyc_ret1[1]<<8) | is->hy_stat.hyc_ret1[2],
			(is->hy_stat.hyc_ret2[0]<<16) | (is->hy_stat.hyc_ret2[1]<<8) | is->hy_stat.hyc_ret2[2],
			(is->hy_stat.hyc_ret3[0]<<16) | (is->hy_stat.hyc_ret3[1]<<8) | is->hy_stat.hyc_ret3[2]);
		printD("	cancel %d abort %d atype %x %x %x uaddr %x\n",
			(is->hy_stat.hyc_cancel[0]<<8) | is->hy_stat.hyc_cancel[1],
			(is->hy_stat.hyc_abort[0]<<8) | is->hy_stat.hyc_abort[1],
			is->hy_stat.hyc_atype[0], is->hy_stat.hyc_atype[1],
			is->hy_stat.hyc_atype[2], is->hy_stat.hyc_uaddr);
#endif
		is->hy_state = IDLE;
#ifdef HYLOG
		hylog(HYL_STATISTICS, sizeof (struct hy_stat),
			(char *)&is->hy_stat);
#endif
		break;
	}

	case CLEARSENT:
		is->hy_state = IDLE;
		break;

	case ENDOPSENT:
		is->hy_state = IDLE;
		break;

	case RECVSENT: {
		register struct hym_hdr *hym;
		register unsigned len;

		if (is->hy_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(is->hy_ifuba.ifu_uba,
			    is->hy_ifuba.ifu_r.ifrw_bdp);
		hym = (struct hym_hdr *) (is->hy_ifuba.ifu_r.ifrw_addr);
		len = (0xffff & (addr->hyd_wcr - is->hy_lastwcr)) << 1;
		if (len > MPSIZE) {
			printf("hy%d: RECVD MP > MPSIZE (%d)\n",
			    ui->ui_unit, len);
			is->hy_state = IDLE;
#ifdef DEBUG
			hy_debug_flag = 1;
			printD("hy%d: csr = 0x%b, bar = 0x%x, wcr = 0x%x\n",
				ui->ui_unit, addr->hyd_csr, HY_CSR_BITS,
				addr->hyd_bar, addr->hyd_wcr);
#endif
		}
		hym->hym_mplen = len;
#ifdef DEBUG
		printD("hy%d: recvd mp, len = %d, data = ", ui->ui_unit, len);
		if (hy_debug_flag)
			hyprintdata((char *)hym, len + HYM_SWLEN);
#endif
		if (hym->hym_ctl & H_ASSOC) {
			is->hy_state = RECVDATASENT;
			is->hy_retry = 0;
			hystart(ui, HYF_INPUTDATA,
			    (int)(HYMTU + sizeof (struct hy_hdr) - len),
			    (int)(HYM_SWLEN + is->hy_ifuba.ifu_r.ifrw_info + len));
		} else {
			hyrecvdata(ui, hym, (int)len + HYM_SWLEN);
			is->hy_state = IDLE;
		}
		break;
	}

	case RECVDATASENT: {
		register struct hym_hdr *hym;
		register unsigned len;

		if (is->hy_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(is->hy_ifuba.ifu_uba,
			    is->hy_ifuba.ifu_r.ifrw_bdp);
		hym = (struct hym_hdr *) (is->hy_ifuba.ifu_r.ifrw_addr);
		len = (0xffff & (addr->hyd_wcr - is->hy_lastwcr)) << 1;
#ifdef DEBUG
		printD("hy%d: recvd assoc data, len = %d, data = ",
			ui->ui_unit, len);
		if (hy_debug_flag)
			hyprintdata((char *)hym + hym->hym_mplen, len);
#endif
		hyrecvdata(ui, hym, (int)(len + hym->hym_mplen + HYM_SWLEN));
		is->hy_state = IDLE;
		break;
	}

	case XMITSENT:
		if (is->hy_flags & RQ_XASSOC) {
			register int len;

			is->hy_flags &= ~RQ_XASSOC;
			is->hy_state = XMITDATASENT;
			is->hy_retry = 0;
			len = (0xffff & (addr->hyd_wcr - is->hy_lastwcr)) << 1;
			if (len > is->hy_olen) {
				printf(
				"hy%d: xmit error - len > hy_olen [%d > %d]\n",
				ui->ui_unit, len, is->hy_olen);
#ifdef DEBUG
				hy_debug_flag = 1;
#endif
			}
			hystart(ui, HYF_XMITLSTDATA, is->hy_olen - len,
			    is->hy_ifuba.ifu_w.ifrw_info + HYM_SWLEN + len);
			break;
		}
		/* fall through to ... */
		
	case XMITDATASENT:
		hyxmitdata(ui);
		is->hy_state = IDLE;
		break;

	case WAITING:	/* wait for message complete or output requested */
		if (HYS_RECVDATA(addr))
			is->hy_state = IDLE;
		else {
			is->hy_state = CLEARSENT;
			is->hy_retry = 0;
			hystart(ui, HYF_CLRWFMSG, 0, 0);
		}
		break;

	case MARKPORT:
		is->hy_state = STARTUP;
		if_down(&is->hy_if);
		is->hy_if.if_flags &= ~IFF_UP;
		goto endintr;
	
	default:
		printf("hy%d: DRIVER BUG - INVALID STATE %d\n",
			ui->ui_unit, is->hy_state);
		panic("HYPERCHANNEL IN INVALID STATE");
		/*NOTREACHED*/
	}
	if (is->hy_state == IDLE)
		goto actloop;
endintr:
	;
#ifdef DEBUG
	printD("hy%d: hyact, exit at \"%s\"\n", ui->ui_unit,
		hy_state_names[is->hy_state]);
#endif
}

struct sockproto hypproto = { PF_HYLINK };
struct sockaddr_in hypdst = { AF_HYLINK };
struct sockaddr_in hypsrc = { AF_HYLINK };

/*
 * Called from device interrupt when receiving data.
 * Examine packet to determine type.  Decapsulate packet
 * based on type and pass to type specific higher-level
 * input routine.
 */
hyrecvdata(ui, hym, len)
	struct uba_device *ui;
	register struct hym_hdr *hym;
	int len;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];
    	struct mbuf *m;
	register struct ifqueue *inq;

	is->hy_if.if_ipackets++;
#ifdef DEBUG
	printD("hy%d: recieved packet, len = %d\n", ui->ui_unit, len);
#endif
#ifdef HYLOG
	{
		struct {
			short hlen;
			struct hym_hdr hhdr;
		} hh;
		hh.hlen = len;
		hh.hhdr = *hym;
		hylog(HYL_RECV, sizeof(hh), (char *)&hh);
	}
#endif
	if (len > HYMTU + MPSIZE || len == 0)
		return;			/* sanity */
	/*
	 * Pull packet off interface.
	 */
	m = if_rubaget(&is->hy_ifuba, len, 0);
	if (m == NULL)
		return;

	/*
	 * if normal or adapter loopback response packet believe hym_type,
	 * otherwise, use the raw input queue cause it's a response from an
	 * adapter command.
	 */
	if (hym->hym_param != 0 && (u_short)hym->hym_param != 0x80ff)
		goto rawlinkin;

	switch (hym->hym_type) {

#ifdef INET
	case HYLINK_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
	default:
	rawlinkin:
		{
			struct mbuf *m0;

			MGET(m0, M_DONTWAIT, MT_DATA);
			if (m == 0) {
				m_freem(m);
				return;
			}
			m0->m_off = MMINOFF;
			m0->m_len = sizeof(struct hym_hdr);
			m0->m_next = m;
			bcopy((caddr_t)hym, mtod(m0, caddr_t), sizeof(struct hym_hdr));
			m = m0;
			hypproto.sp_protocol = 0;
			hypdst.sin_addr = is->hy_addr;
			hypsrc.sin_addr = is->hy_addr;
			raw_input(m, &hypproto, (struct sockaddr *)&hypsrc,
				(struct sockaddr *)&hypdst);
			return;
		}
	}
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);
}

/*
 * Transmit done, release resources, bump counters.
 */
hyxmitdata(ui)
	struct uba_device *ui;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];

	is->hy_if.if_opackets++;
	if (is->hy_ifuba.ifu_xtofree) {
		m_freem(is->hy_ifuba.ifu_xtofree);
		is->hy_ifuba.ifu_xtofree = 0;
	}
}

hycancel(ui)
	register struct uba_device *ui;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];

	if (is->hy_ifuba.ifu_xtofree) {
		m_freem(is->hy_ifuba.ifu_xtofree);
		is->hy_ifuba.ifu_xtofree = 0;
	}
#ifdef HYLOG
	hylog(HYL_CANCEL, 0, (char *)0);
#endif
#ifdef DEBUG
	if (hy_nodebug & 1)
		hy_debug_flag = 1;
#endif
#ifdef DEBUG
	printD("hy%d: cancel from state \"%s\" cmd=0x%x count=%d ptr=0x%x\n",
		ui->ui_unit, hy_state_names[is->hy_state], is->hy_savedcmd,
		is->hy_savedcount, is->hy_savedaddr);
	printD("\tflags 0x%x olen %d lastwcr %d retry %d\n",
		is->hy_flags, is->hy_olen, is->hy_lastwcr, is->hy_retry);
	printD("\tsaved: state %d count %d ptr 0x%x cmd 0x%x\n",
		is->hy_savedstate, is->hy_savedcount, is->hy_savedaddr,
		is->hy_savedcmd);
#endif
	is->hy_state = IDLE;
	is->hy_flags |= (RQ_ENDOP | RQ_STATUS);
	hyact(ui);
}

#ifdef DEBUG
hyprintdata(cp, len)
	register char *cp;
	register int len;
{
	register int count = 16;
	register char *fmt;
	static char regfmt[] = "\n\t %x";

	fmt = &regfmt[2];
	while (--len >= 0) {
		printL(fmt, *cp++ & 0xff);
		fmt = &regfmt[2];
		if (--count <= 0) {
			fmt = &regfmt[0];
			count = 16;
		}
	}
	printL("\n");
}
#endif

hywatch(unit)
	int unit;
{
	register struct hy_softc *is = &hy_softc[unit];
	register struct uba_device *ui = hyinfo[unit];
	register struct hydevice *addr = (struct hydevice *)ui->ui_addr;
	int s;

	s = splimp();
#ifdef PI13
	if ((addr->hyd_csr & S_POWEROFF) != 0) {
		addr->hyd_csr |= S_POWEROFF;
		DELAY(100);
		if ((addr->hyd_csr & S_POWEROFF) == 0) {
			printf("hy%d: Adapter Power Restored (hywatch)\n", unit);
			is->hy_state = IDLE;
			is->hy_flags |=
			  (RQ_MARKUP | RQ_STATISTICS | RQ_ENDOP | RQ_STATUS);
			hyact(ui);
		}
	}
#endif
	if (++is->hy_ntime >= 2 && is->hy_state != WAITING &&
	  is->hy_state != STARTUP && is->hy_state != IDLE) {
#ifdef HYLOG
		printf("hy%d: watchdog timer expired in state \"%s\"\n", unit,
			hy_state_names[is->hy_state]);
#else
		printf("hy%d: watchdog timer expired in state %d\n", unit,
			is->hy_state);
#endif
		printf("hy%d: last command 0x%x, flags 0x%x, csr 0x%b\n", unit,
			is->hy_savedcmd, is->hy_flags, addr->hyd_csr, HY_CSR_BITS);
		hycancel(ui);
	}
	splx(s);
	is->hy_if.if_timer = SCANINTERVAL;
}

#ifdef HYLOG
hylog(code, len, ptr)
	int code, len;
	char *ptr;
{
	register unsigned char *p;
	int s;

	s = splimp();
	if (hy_log.hyl_self != &hy_log) {
		hy_log.hyl_eptr = &hy_log.hyl_buf[HYL_SIZE];
		hy_log.hyl_ptr = &hy_log.hyl_buf[0];
		hy_log.hyl_self = &hy_log;
		hy_log.hyl_enable = HYL_CONTINUOUS;
		hy_log.hyl_onerr = HYL_CONTINUOUS;
		hy_log.hyl_count = 0;
		hy_log.hyl_icount = 16;
		hy_log.hyl_filter = 0xffff;	/* enable all */
	}
	if (hy_log.hyl_enable == HYL_DISABLED || ((1 << code) & hy_log.hyl_filter) == 0)
		goto out;
	p = hy_log.hyl_ptr;
	if (p + len + 3 >= hy_log.hyl_eptr) {
		bzero((caddr_t)p, (unsigned)(hy_log.hyl_eptr - p));
		p = &hy_log.hyl_buf[0];
		if (hy_log.hyl_enable != HYL_CONTINUOUS) {
			hy_log.hyl_enable = HYL_DISABLED;
			goto out;
		}
	}
	*p++ = code;
	*p++ = len;
	bcopy((caddr_t)ptr, (caddr_t)p, (unsigned)len);
	if (hy_log.hyl_count != 0 && --hy_log.hyl_count == 0) {
		*p++ = '\0';
		hy_log.hyl_enable = HYL_DISABLED;
		hy_log.hyl_count = hy_log.hyl_icount;
	}
	if (hy_log.hyl_wait != 0) {		/* wakeup HYGETLOG if wanted */
		hy_log.hyl_wait -= (p - hy_log.hyl_ptr) + len;
		if (hy_log.hyl_wait <= 0) {
			wakeup((caddr_t)&hy_log);
			hy_log.hyl_wait = 0;
		}
	}
	hy_log.hyl_ptr = p + len;
out:
	splx(s);
}
#endif

/*ARGSUSED*/
hyioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t	data;
{
	struct ifaddr *ifa = (struct ifaddr *) data;
	struct hyrsetget *sg = (struct hyrsetget *)data;
#if defined(HYLOG) || defined(HYELOG)
	struct hylsetget *sgl = (struct hylsetget *)data;
#endif
	struct hy_route *r = (struct hy_route *)&hy_route[ifp->if_unit];
	int s = splimp(), error = 0;
#ifdef HYLOG
	struct hy_softc *is = &hy_softc[ifp->if_unit];
	struct {
		u_char	hstate;
		u_char	hflags;
		u_short	iflags;
		int	hcmd;	
		int	herror;
		u_long	haddr;
		u_long	hmisc;
	} hil;

	hil.hmisc = -1;
	hil.hstate = is->hy_state;
	hil.hflags = is->hy_flags;
	hil.hcmd = cmd;
#endif

	switch(cmd) {

	case SIOCSIFADDR:
		if (sin->sin_family != AF_INET)
			return(EINVAL);
		if ((ifp->if_flags & IFF_RUNNING) == 0)
			hyinit(ifp->if_unit);
		hy_softc[ifp->if_unit].hy_addr = IA_SIN(ifa)->sin_addr;
#ifdef HYLOG
		hil.haddr = is->hy_addr.s_addr;
#endif
		break;

	case HYSETROUTE:
		if (!suser()) {
			error = EPERM;
			goto out;
		}

		if (sg->hyrsg_len != sizeof(struct hy_route)) {
			error = EINVAL;
			goto out;
		}
		if (copyin((caddr_t)(sg->hyrsg_ptr), (caddr_t)r, sg->hyrsg_len)) {
			r->hyr_lasttime = 0;	/* disable further routing if trouble */
			error = EFAULT;
			goto out;
		}
		r->hyr_lasttime = time.tv_sec;
#ifdef HYLOG
		hil.hmisc = r->hyr_lasttime;
#endif
		break;

	case HYGETROUTE:
		if (sg->hyrsg_len < sizeof(struct hy_route)) {
			error = EINVAL;
			goto out;
		}
		if (copyout((caddr_t)r, (caddr_t) (sg->hyrsg_ptr), sizeof(struct hy_route))) {
			error = EFAULT;
			goto out;
		}
		break;

#ifdef HYELOG
	case HYGETELOG:
		if (sgl->hylsg_len < sizeof(hy_elog)) {
			error = EINVAL;
			goto out;
		}
		if (copyout((caddr_t)hy_elog, sgl->hylsg_ptr, sizeof(hy_elog))) {
			error = EFAULT;
			goto out;
		}
		if (sgl->hylsg_cmd) {
			if (!suser()) {
				error = EPERM;
				goto out;
			}
			bzero((caddr_t)hy_elog, sizeof(hy_elog));
		}
		break;
#endif

#ifdef HYLOG
	case HYSETLOG:
		if (!suser()) {
			error = EPERM;
			goto out;
		}
		hy_log.hyl_enable = HYL_DISABLED;
		hylog(HYL_NOP, 0, (char *)0);		/* force log init */
		hy_log.hyl_enable = sgl->hylsg_cmd & 0x0f;
		hy_log.hyl_onerr = (sgl->hylsg_cmd >> 4) & 0x0f;
		hy_log.hyl_filter = (sgl->hylsg_cmd >> 8) & 0xffffff;
		hy_log.hyl_count = hy_log.hyl_icount = sgl->hylsg_len;
		wakeup((caddr_t)&hy_log);	/* wakeup sleeping HYGETLOG */
		break;

	case HYGETLOG:
		if (sgl->hylsg_len < sizeof(hy_log)) {
			error = EINVAL;
			goto out;
		}
		if (sgl->hylsg_cmd != 0) {
			if (hy_log.hyl_wait) {
				error = EBUSY;
				goto out;
			}
			hy_log.hyl_wait = sgl->hylsg_cmd;
			sleep((caddr_t)&hy_log);
		}

		if (copyout((caddr_t)&hy_log, sgl->hylsg_ptr, sizeof(hy_log))) {
			error = EFAULT;
			goto out;
		}
		break;
#endif

	default:
		error = EINVAL;
		break;
	}
out:
#ifdef HYLOG
	hil.herror = error;
	hil.iflags = ifp->if_flags;
	hil.haddr = is->hy_addr.s_addr;
	hylog(HYL_IOCTL, sizeof(hil), (char *)&hil);
#endif
	splx(s);
	return (error);
}
#endif
