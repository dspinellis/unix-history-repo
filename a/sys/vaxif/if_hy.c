/*	if_hy.c	4.3	83/02/21	*/

#include "hy.h"
#if NHY > 0

/*
 * Network Systems Copropration Hyperchanel interface
 *
 * UNTESTED WITH 4.1C
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/vmmac.h"
#include "../h/errno.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/ioctl.h"

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

#include "../vaxif/if_hy.h"
#include "../vaxif/if_hyreg.h"
#include "../vaxif/if_uba.h"

#define HYROUTE
#define HYELOG
#define	HYMTU	576

int	hyprobe(), hyattach(), hyinit(), hyoutput(), hyreset(), hywatch();
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
	int	hy_ilen;		/* mp length on input */
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
#define HYE_MAX	0x18
u_long	hy_elog[(HYE_MAX+1)*4];
#endif

#ifdef DEBUG
#define printL	lprintf
#define printD	if (hy_debug_flag) lprintf
int	hy_debug_flag = 0;
/*
 * hy_nodebug bit 0x01	set hy_debug_flag on hycancel
 * hy_nodebug bit 0x02	set hy_debug_flag on command reissue
 * hy_nodebug bit 0x04	set hy_debug_flag on abnormal interrupt
 * hy_nodebug bit 0x08	set hy_debug_flag on hyouput
 * hy_nodebug bit 0x10	set hy_debug_flag on hyouput with associated data
 */
int	hy_nodebug = 0x0;
#else
#define printD	hyvoid
#endif

/*
 * Requests for service (in order by descending priority).
 */
#define RQ_ENDOP	001	/* end the last adapter function */
#define RQ_REISSUE	002	/* reissue previous cmd after status */
#define RQ_STATUS	004	/* get the status of the adapter */
#define RQ_STATISTICS	010	/* get the statistics of the adapter */
#define RQ_MARKDOWN	020	/* mark this adapter port down */
#define RQ_MARKUP	040	/* mark this interface up */

#define RQ_XASSOC	0100	/* associated data to transmit */

/* 
 * Driver states.
 */
#define	STARTUP		0	/* initial state (before fully there) */
#define	IDLE		1	/* idle state */
#define	STATSENT	2	/* status cmd sent to adapter */
#define	ENDOPSENT	3	/* end operation cmd sent */
#define	RECVSENT	4	/* input message cmd sent */
#define	RECVDATASENT	5	/* input data cmd sent */
#define	XMITSENT	6	/* transmit message cmd sent */
#define	XMITDATASENT	7	/* transmit data cmd sent */
#define	WAITING		8	/* waiting for messages */
#define	CLEARSENT	9	/* clear wait for message cmd sent */
#define MARKPORT	10	/* mark this host's adapter port down issued */
#define RSTATSENT	11	/* read statistics cmd sent to adapter */

#ifdef DEBUG
char *hy_state_names[] = {
	"Startup",
	"Idle",
	"Status Sent",
	"End op Sent",
	"Recieve Message Proper Sent",
	"Recieve Data Sent",
	"Transmit Message Proper Sent",
	"Transmit Data Sent",
	"Wait for Message Sent",
	"Clear Wait for Message Sent",
	"Mark Port Down Sent",
	"Read Statistics Sent"
};
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
	return(1);
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
	ifp->if_net = ui->ui_flags;
	is->hy_state = STARTUP;		/* don't allow state transitions yet */
	ifp->if_init = hyinit;
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
	register struct uba_device *ui = hyinfo[unit];
	register struct hy_softc *is;

	if (unit >= NHY || ui == 0 || ui->ui_alive == 0 ||
	  ui->ui_ubanum != uban)
		return;
	printf(" hy%d", unit);
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
	int s;

	if (if_ubainit(&is->hy_ifuba, ui->ui_ubanum,
	    sizeof (struct hy_hdr), (int)btoc(HYMTU)) == 0) { 
#ifdef DEBUG
		if (hy_nodebug & 4)
			hy_debug_flag = 1;
#endif
		printf("hy%d: can't initialize\n", unit);
		is->hy_if.if_flags &= ~IFF_UP;
		return;
	}
	/*
	 * Issue wait for message and start the state machine
	 */
	s = splimp();
	is->hy_state = IDLE;
	is->hy_flags = RQ_STATUS | RQ_STATISTICS | RQ_MARKUP;
	is->hy_retry = 0;
	hyact(ui);
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
			printf("hy%d: Power Off bit set, trying to reset\n",
				unit);
			addr->hyd_csr |= S_POWEROFF;
			DELAY(100);
			if (addr->hyd_csr & S_POWEROFF) {
				if_down(&is->hy_if);
				is->hy_state = STARTUP;
				printf(
				  "hy%d: Power Off Error, network shutdown\n",
				  unit);
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
		printD("\tflags 0x%x ilen %d olen %d lastwcr %d retry %d\n",
			is->hy_flags, is->hy_ilen, is->hy_olen,
			is->hy_lastwcr, is->hy_retry);
		printD("\tsaved: state %d count %d cmd 0x%x ptr 0x%x\n",
			is->hy_savedstate, is->hy_savedcount,
			is->hy_savedaddr, is->hy_savedcmd);
#endif
#ifdef PI13
		addr->hyd_csr &= ~S_C;  /* clear the damned PI-13 */
#endif
		if (is->hy_state == XMITSENT || is->hy_state == XMITDATASENT)
			is->hy_if.if_oerrors++;
		if (is->hy_state == RECVSENT || is->hy_state == RECVDATASENT)
			is->hy_if.if_ierrors++;
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

/*
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
hyoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct hym_hdr *hym;
	register struct mbuf *m;
#ifdef HYROUTE
	register struct hyroute *r = &hy_route[ifp->if_unit];
#endif
	short dtype;		/* packet type */
	int dhost;		/* destination adapter address */
	int dlen;
	int mplen = 0;		/* message proper length */
	short loopback = 0;	/* hardware loopback requested */
	int error = 0;
	int s;

#ifdef DEBUG
	if (hy_nodebug & 8)
		hy_debug_flag = 1;
#endif
	dlen = 0;
	for (m = m0; m; m = m->m_next)
		dlen += m->m_len;
	m = m0;
	switch(dst->sa_family) {

#ifdef INET
	case AF_INET: {
		register struct ip *ip = mtod(m, struct ip *);
		register struct sockaddr_in *sin = (struct sockaddr_in *)dst;
		register long hostaddr = in_lnaof(sin->sin_addr);

		dhost = hostaddr & 0xffff;
		dtype = HYLINK_IP;
#ifdef DEBUG
		printD("hy%d: output to host %x, dhost %x\n",
			ifp->if_unit, sin->sin_addr.s_addr, dhost);
#endif
		/*
		 * Debugging loopback support:
		 * upper byte of 24 bit host number interpreted as follows
		 *	0x00 --> no loopback
		 *	0x01 --> hardware loop through remote adapter
		 *	other --> software loop through remote ip layer
		 */
		if (hostaddr & 0xff0000) {
			struct in_addr temp;

			temp = ip->ip_dst;
			ip->ip_dst = ip->ip_src;
			ip->ip_src = temp;
			if ((hostaddr & 0xff0000) == 0x10000)
				loopback = H_LOOPBK;
		}
		/*
		 * If entire packet won't fit in message proper, just
		 * send hyperchannel hardware header and ip header in
		 * message proper.  If that won't fit either, just send
		 * the maximum message proper.
		 *
		 * This insures that the associated data is at least a
		 * TCP/UDP header in length and thus prevents potential
		 * problems with very short word counts.
		 */
		if (dlen > MPSIZE - sizeof (struct hy_hdr)) {
			mplen = sizeof(struct hy_hdr) + (ip->ip_hl << 2);
			if (mplen > MPSIZE)
				mplen = MPSIZE;
		}
		break;
	}
#endif

	default:
		printf("hy%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
#ifdef DEBUG
		if (hy_nodebug & 4)
			hy_debug_flag = 1;
#endif
		error = EAFNOSUPPORT;
		goto drop;
	}

	/*
	 * Add the software and hardware hyperchannel headers.
	 * If there's not enough space in the first mbuf, allocate another.
	 * If that should fail, drop this sucker.
	 * No extra space for headers is allocated.
	 */
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
	hym = mtod(m, struct hym_hdr *);
	hym->hym_mplen = mplen;
	hym->hym_hdr.hyh_type = dtype;
	hym->hym_hdr.hyh_off = 0;
	hym->hym_hdr.hyh_from = htons(ifp->if_host[0]);
	hym->hym_hdr.hyh_param = loopback;
#ifdef HYROUTE
	if (r->hyr_lasttime.tv_sec != 0) {
		register struct hy_hash *rh;
		register int i;

		i = HYRHASH(dhost);
		rh = &r->hyr_hash[i];
		i = 0;
		while (rh->hyr_key != dhost) {
			rh++; i++;
			if (rh > &r->hyr_hash[HYRSIZE])
				rh = &r->hyr_hash[0];
			if (rh->hyr_flags == 0 || i > HYRSIZE)
				goto notfound;
		}
		if (rh->hyr_flags & HYR_GATE) {
			loopback = 0;	/* no hardware loopback on gateways */
			i = rh->hyr_nextgate;
			if (i >= rh->hyr_egate)
				rh->hyr_nextgate = rh->hyr_pgate;
			else
				rh->hyr_nextgate++;
			rh = &r->hyr_hash[r->hyr_gateway[i]];
			if ((rh->hyr_flags & HYR_DIR) == 0)
				goto notfound;
		}
		hym->hym_hdr.hyh_ctl = rh->hyr_ctl;
		hym->hym_hdr.hyh_access = rh->hyr_access;
		hym->hym_hdr.hyh_to = rh->hyr_dst;
	} else {
		hym->hym_hdr.hyh_ctl = H_XTRUNKS | H_RTRUNKS;
		hym->hym_hdr.hyh_access = 0;
		hym->hym_hdr.hyh_to = htons(dhost);
	}
#else
	hym->hym_hdr.hyh_ctl = H_XTRUNKS | H_RTRUNKS;
	hym->hym_hdr.hyh_access = 0;
	hym->hym_hdr.hyh_to = htons(dhost);
#endif

headerexists:
	if (hym->hym_mplen) {
		hym->hym_hdr.hyh_ctl |= H_ASSOC;
#ifdef DEBUG
		if (hy_nodebug & 16)
			hy_debug_flag = 1;
#endif
	} else
		hym->hym_hdr.hyh_ctl &= ~H_ASSOC;
#ifdef DEBUG
	printD("hy%d: output mplen=%x ctl=%x access=%x to=%x",
		ifp->if_unit, hym->hym_mplen, hym->hym_hdr.hyh_ctl,
		hym->hym_hdr.hyh_access, hym->hym_hdr.hyh_to);
	printD(" (adapter %x) from=%x param=%x type=%x off=%x\n",
		hym->hym_hdr.hyh_to_adapter,
		hym->hym_hdr.hyh_from, hym->hym_hdr.hyh_param,
		hym->hym_hdr.hyh_type, hym->hym_hdr.hyh_off);
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
	error = ENETUNREACH;			/* XXX */
drop:
	m_freem(m);
	return (error);
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
			hystart(ui, HYF_INPUTMSG, MPSIZE,
				is->hy_ifuba.ifu_r.ifrw_info);
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
				if (hym->hym_hdr.hyh_to_adapter ==
				  hym->hym_hdr.hyh_from_adapter)
					cmd = HYF_XMITLOCMSG;
				else
					cmd = HYF_XMITMSG;
#ifdef DEBUG
				printD("hy%d: hym_hdr = ", ui->ui_unit);
				if (hy_debug_flag)
					hyprintdata((char *)hym,
					  sizeof (struct hym_hdr));
#endif
				/*
				 * Strip off the software part of
				 * the hyperchannel header
				 */
				m->m_off += sizeof(struct hym_data);
				m->m_len -= sizeof(struct hym_data);
				is->hy_olen = if_wubaput(&is->hy_ifuba, m);
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
					  is->hy_olen);
#endif
				hystart(ui, cmd,
				   (mplen == 0) ? is->hy_olen : mplen,
				   is->hy_ifuba.ifu_w.ifrw_info);
				if (mplen != 0)
					is->hy_flags |= RQ_XASSOC;
			} else if (rq & RQ_MARKDOWN) {
				is->hy_flags &= ~(RQ_MARKUP | RQ_MARKDOWN);
				is->hy_state = MARKPORT;
				is->hy_retry = 0;
				/*
				 * Port number is taken from status data
				 */
				hystart(ui,
				  HYF_MARKP0 | (PORTNUM(&is->hy_status) << 2),
				  0, 0);
			} else if (rq & RQ_MARKUP) {
				register struct ifnet *ifp = &is->hy_if;
				register struct sockaddr_in *sin =
				   (struct sockaddr_in *)&ifp->if_addr;

				is->hy_flags &= ~RQ_MARKUP;
				is->hy_retry = 0;
				/*
				 * Fill in the Internet address
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

				ifp->if_host[0] =
				  (is->hy_stat.hyc_uaddr << 8) |
					PORTNUM(&is->hy_status);
				sin->sin_family = AF_INET;
				sin->sin_addr =
				   if_makeaddr(ifp->if_net, ifp->if_host[0]);
				ifp->if_flags |= IFF_UP;
				if_rtinit(ifp, RTF_UP);
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
		bcopy(is->hy_ifuba.ifu_r.ifrw_addr, &is->hy_status,
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
			if (i < HYE_MAX)
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

		is->hy_stat.hyc_msgcnt = ntohl(p->hyc_msgcnt);
		is->hy_stat.hyc_dbcnt = ntohl(p->hyc_dbcnt);
		is->hy_stat.hyc_tbusy = ntohl(p->hyc_tbusy);
		is->hy_stat.hyc_hwret = ntohl(p->hyc_hwret);
		is->hy_stat.hyc_crcbad = ntohl(p->hyc_crcbad);
		is->hy_stat.hyc_mcret = ntohl(p->hyc_mcret);
		is->hy_stat.hyc_tdabort = ntohl(p->hyc_tdabort);
		is->hy_stat.hyc_atype[0] = p->hyc_atype[0];
		is->hy_stat.hyc_atype[1] = p->hyc_atype[1];
		is->hy_stat.hyc_atype[2] = p->hyc_atype[2];
		is->hy_stat.hyc_uaddr = p->hyc_uaddr;
#ifdef DEBUG
		printD(
	"hy%d: statistics - msgcnt %d dbcnt %d hwret %d tbusy %d crcbad %d\n",
			ui->ui_unit,
			is->hy_stat.hyc_msgcnt, is->hy_stat.hyc_dbcnt,
			is->hy_stat.hyc_tbusy, is->hy_stat.hyc_hwret,
			is->hy_stat.hyc_crcbad);
		printD("	mcret %d tdabort %d atype %x %x %x uaddr %x\n",
			is->hy_stat.hyc_mcret, is->hy_stat.hyc_tdabort,
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
		register struct hy_hdr *hyh;
		register unsigned len;

		if (is->hy_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(is->hy_ifuba.ifu_uba,
				is->hy_ifuba.ifu_r.ifrw_bdp);
		hyh = (struct hy_hdr *) (is->hy_ifuba.ifu_r.ifrw_addr);
		len = (0xffff & (addr->hyd_wcr - is->hy_lastwcr)) << 1;
		if (len > MPSIZE) {
			printf("hy%d: RECVD MP > MPSIZE (%d)\n",
				ui->ui_unit, len);
#ifdef DEBUG
			hy_debug_flag = 1;
			printD("hy%d: csr = 0x%b, bar = 0x%x, wcr = 0x%x\n",
				ui->ui_unit, addr->hyd_csr, HY_CSR_BITS,
				addr->hyd_bar, addr->hyd_wcr);
#endif
		}
#ifdef DEBUG
		printD("hy%d: recvd mp, len = %d, data = ", ui->ui_unit, len);
		if (hy_debug_flag)
			hyprintdata((char *)hyh, len);
#endif
		if (hyh->hyh_ctl & H_ASSOC) {
			is->hy_state = RECVDATASENT;
			is->hy_ilen = len;
			is->hy_retry = 0;
			hystart(ui, HYF_INPUTDATA,
			  HYMTU-len+sizeof (struct hy_hdr),
			  is->hy_ifuba.ifu_r.ifrw_info + len);
		} else {
			hyrecvdata(ui, hyh, len);
			is->hy_state = IDLE;
		}
		break;
	}

	case RECVDATASENT: {
		register struct hy_hdr *hyh;
		register unsigned len;

		if (is->hy_ifuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(is->hy_ifuba.ifu_uba,
				is->hy_ifuba.ifu_r.ifrw_bdp);
		hyh = (struct hy_hdr *) (is->hy_ifuba.ifu_r.ifrw_addr);
		len = (0xffff & (addr->hyd_wcr - is->hy_lastwcr)) << 1;
#ifdef DEBUG
		printD("hy%d: recvd assoc data, len = %d, data = ",
			ui->ui_unit, len);
		if (hy_debug_flag)
			hyprintdata((char *)hyh + is->hy_ilen, len);
#endif
		hyrecvdata(ui, hyh, len + is->hy_ilen);
		is->hy_state = IDLE;
		break;
	}

	case XMITSENT:
		if (is->hy_flags & RQ_XASSOC) {
			register unsigned len;

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
				is->hy_ifuba.ifu_w.ifrw_info + len);
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
#ifdef DEBUG
	printD("hy%d: hyact, exit at \"%s\"\n", ui->ui_unit,
		hy_state_names[is->hy_state]);
#endif
	return (0);
}

/*
 * Called from device interrupt when recieving data.
 * Examine packet to determine type.  Decapsulate packet
 * based on type and pass to type specific higher-level
 * input routine.
 */
hyrecvdata(ui, hyh0, len)
	struct uba_device *ui;
	struct hy_hdr *hyh0;
	int len;
{
	register struct hy_softc *is = &hy_softc[ui->ui_unit];
	register struct hy_hdr *hyh = hyh0;
    	struct mbuf *m;
	register struct ifqueue *inq;

	is->hy_if.if_ipackets++;
#ifdef DEBUG
	printD("hy%d: recieved packet, len = %d (actual %d)\n",
		ui->ui_unit, len,
		len - (hyh->hyh_off + sizeof (struct hy_hdr)));
#endif
#ifdef HYLOG
	{
		struct {
			short hlen;
			struct hy_hdr hhdr;
		} hh;
		hh.hlen = len;
		hh.hhdr = *hyh;
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
	switch (hyh->hyh_type) {

#ifdef INET
	case HYLINK_IP:
		/*
		 * Strip the variable portion of the hyperchannel header
		 * (fixed portion stripped in if_rubaget).
		 */
		m->m_len -= hyh->hyh_off;
		m->m_off += hyh->hyh_off;
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
	default:
		m_freem(m);
		return;
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
#ifdef DEBUG
	if (hy_nodebug & 1)
		hy_debug_flag = 1;
#endif
#ifdef DEBUG
	printD("hy%d: cancel from state \"%s\" cmd=0x%x count=%d ptr=0x%x\n",
		ui->ui_unit, hy_state_names[is->hy_state], is->hy_savedcmd,
		is->hy_savedcount, is->hy_savedaddr);
	printD("\tflags 0x%x ilen %d olen %d lastwcr %d retry %d\n",
		is->hy_flags, is->hy_ilen, is->hy_olen, is->hy_lastwcr,
		is->hy_retry);
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
	is->hy_if.if_timer = SCANINTERVAL;
	if (is->hy_ntime > 2 && is->hy_state != WAITING &&
	  is->hy_state != STARTUP && is->hy_state != IDLE) {
		printf("hy%d: watchdog timer expired\n", unit);
		hycancel(ui);
	}
#ifdef PI13
	if ((addr->hyd_csr & S_POWEROFF) != 0) {
		addr->hyd_csr |= S_POWEROFF;
		DELAY(100);
		if ((addr->hyd_csr & S_POWEROFF) == 0) {
			printf("hy%d: adapter power restored\n", unit);
			is->hy_state = IDLE;
			is->hy_flags |=
			  (RQ_MARKUP | RQ_STATISTICS | RQ_ENDOP | RQ_STATUS);
			hyact(ui);
		}
	}
#endif
	splx(s);
}

#ifdef HYLOG
hylog(code, len, ptr)
	int code;
	int len;
	char *ptr;
{
	register unsigned char *p;
	int s;

	s = splimp();
	if (hy_log.hyl_self != &hy_log) {
		hy_log.hyl_eptr = &hy_log.hyl_buf[HYL_SIZE];
		hy_log.hyl_ptr = &hy_log.hyl_buf[0];
		hy_log.hyl_self = &hy_log;
		hy_log.hyl_enable = HYL_DISABLED;
		hy_log.hyl_onerr = HYL_CATCH1;
	}
	if (hy_log.hyl_enable == HYL_DISABLED ||
	  hy_log.hyl_enable == HYL_CAUGHT1 ||
	  hy_log.hyl_enable == HYL_CAUGHTSTATUS ||
	  (hy_log.hyl_enable == HYL_CATCHSTATUS && code != HYL_STATUS))
		goto out;
	p = hy_log.hyl_ptr;
	if (p + len + 2 >= hy_log.hyl_eptr) {
		bzero(p, hy_log.hyl_eptr - p);
		p = &hy_log.hyl_buf[0];
		if (hy_log.hyl_enable == HYL_CATCH1) {
			hy_log.hyl_enable = hy_log.hyl_onerr = HYL_CAUGHT1;
			goto out;
		}
		if (hy_log.hyl_enable == HYL_CATCHSTATUS) {
			hy_log.hyl_enable = hy_log.hyl_onerr = HYL_CAUGHTSTATUS;
			goto out;
		}
	}
	*p++ = code;
	*p++ = len;
	bcopy(ptr, p, len);
	hy_log.hyl_ptr = p + len;
out:
	splx(s);
}
#endif

hyioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t	data;
	int flag;
{
	int s = splimp(), error = 0;

	if (minor(dev) >= NHY) {
		error = ENXIO;
		goto bad;
	}
	switch(cmd) {

	case HYSETROUTE:
		if (!suser()) {
			error = EPERM;
			goto bad;
		}
		hy_route[minor(dev)] = *(struct hyroute *)data;
		hy_route[minor(dev)].hyr_lasttime = time;
		break;

	case HYGETROUTE:
		*(struct hyroute *)data = hy_route[minor(dev)];
		break;

	default:
		error = ENXIO;
		break;
	}
bad:
	splx(s);
	return (error);
}
#endif
