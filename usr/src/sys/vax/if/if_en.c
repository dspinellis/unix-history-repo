/* if_en.c 4.9 81/11/16 */

#include "en.h"
/*
 * Ethernet interface driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../net/inet.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"			/* XXX */
#include "../net/tcp_var.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/enreg.h"
#include "../h/mtpr.h"
#include "../h/cpu.h"
#include "../h/cmap.h"

int	enrswaps;
/* int	enwswaps; */
int	enprobe(), enattach(), enrint(), enxint(), encollide();
struct	uba_device *eninfo[NEN];
u_short enstd[] = { 0 };
struct	uba_driver endriver =
	{ enprobe, 0, enattach, 0, enstd, "en", eninfo };

#define	ENUNIT(x)	minor(x)

struct	en_packet *xpkt, *rpkt;
struct	en_prefix {
	struct en_header enp_h;
	struct tcpiphdr enp_th;
};
struct	uba_regs *enuba;
struct	pte *enrmr, *enxmr;
int	enrbdp, enwbdp;
int	enrproto, enwproto;
struct	pte enxmap[2];
int	enxswapd;

enprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct endevice *addr = (struct endevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	enrint(), enxint(), encollide();
#endif

	addr->en_istat = 0;
	addr->en_ostat = 0;
	addr->en_owc = -1;
	addr->en_oba = 0;
	addr->en_ostat = EN_IEN|EN_GO;
	DELAY(100000);
	addr->en_ostat = 0;
	printf("ethernet address %d\n", ~addr->en_addr&0xff);
	return (1);
}

/*ARGSUSED*/
enattach(ui)
	struct uba_device *ui;
{

}

eninit(unit)
	int unit;
{	
	register struct endevice *addr;
	register struct uba_device *ui;
	int uban, x;
	static reenter;

	if (unit >= NEN || (ui = eninfo[unit]) == 0 || ui->ui_alive == 0) {
		printf("en%d: not alive\n", unit);
		return;
	}
	x = splimp();
	if (reenter == 0) {
		int n, j, i, k; char *cp;
		reenter = 1;
		n = 10;
		k = n<<1;
		i = rmalloc(mbmap, n*2);
		if (i == 0)
			panic("eninit");
		j = i << 1;
		cp = (char *)pftom(i);
		if (memall(&Mbmap[j], k, proc, CSYS) == 0)
			panic("eninit");
		vmaccess(&Mbmap[j], (caddr_t)cp, k);
		rpkt = (struct en_packet *)
		    (cp + 1024 - sizeof (struct en_prefix));
		xpkt = (struct en_packet *)
		    (cp + 5 * 1024 + 1024 - sizeof (struct en_prefix));
		for (j = 0; j < n; j++)
			mprefcnt[i+j] = 1;
	}
	uban = ui->ui_ubanum;
	addr = (struct endevice *)ui->ui_addr;
	addr->en_istat = 0;
	addr->en_ostat = 0;
	imp_stat.iaddr =
	    uballoc(uban, (caddr_t)rpkt, 1024+512, UBA_NEED16|UBA_NEEDBDP);
	imp_stat.oaddr =
	    uballoc(uban, (caddr_t)xpkt, 1024+512, UBA_NEED16|UBA_NEEDBDP);
	enuba = ui->ui_hd->uh_uba;
	enrbdp = (imp_stat.iaddr >> 28) & 0xf;
	enwbdp = (imp_stat.oaddr >> 28) & 0xf;
	enrproto = UBAMR_MRV | (enrbdp << 21);
	enwproto = UBAMR_MRV | (enwbdp << 21);
	enrmr = &enuba->uba_map[((imp_stat.iaddr>>9)&0x1ff) + 1];
	enxmr = &enuba->uba_map[((imp_stat.oaddr>>9)&0x1ff) + 1];
	enxmap[0] = enxmr[0];
	enxmap[1] = enxmr[1];
	enxswapd = 0;
	printf("enrbdp %x enrproto %x enrmr %x imp_stat.iaddr %x\n",
	    enrbdp, enrproto, enrmr, imp_stat.iaddr);
	imp_stat.impopen = 1;
	imp_stat.flush = 0;
	splx(x);
#ifdef IMPDEBUG
	printf("eninit(%d): iaddr = %x, oaddr = %x\n",
		unit, imp_stat.iaddr, imp_stat.oaddr);
#endif
}

enreset(uban)
	int uban;
{
	register int unit;
	struct uba_device *ui;

	for (unit = 0; unit < NEN; unit++) {
		ui = eninfo[unit];
		if (ui == 0 || ui->ui_ubanum != uban || ui->ui_alive == 0)
			continue;
		if (imp_stat.iaddr)
			ubarelse(uban, &imp_stat.iaddr);
		if (imp_stat.oaddr)
			ubarelse(uban, &imp_stat.oaddr);
		eninit(unit);
		printf("en%d ", unit);
	}
}

int	enlastdel = 25;
int	enlastx = 0;
enstart(dev)
	dev_t dev;
{
	register struct mbuf *m, *mp;
	register struct endevice *addr;
	register caddr_t cp, top;
        int unit;
        register int len;
	struct uba_device *ui;
	int enxswapnow = 0;
COUNT(ENSTART);

	unit = ENUNIT(dev);
	ui = eninfo[unit];
	if (ui == 0 || ui->ui_alive == 0) {
		printf("en%d (imp_output): not alive\n", unit);
		return;
	}
	addr = (struct endevice *)ui->ui_addr;
	if (!imp_stat.outactive) {
		if ((m = imp_stat.outq_head) == NULL)
			return;
		imp_stat.outactive = 1;      /* set myself active */
		imp_stat.outq_head = m->m_act;  /* -> next packet chain */
		/*
		 * Pack mbufs into ethernet packet.
		 */
		cp = (caddr_t)xpkt;
		top = (caddr_t)xpkt + sizeof(struct en_packet);
		while (m != NULL) {
			char *dp;
			if (cp + m->m_len > top) {
				printf("imp_snd: my packet runneth over\n");
				m_freem(m);
				return;
			}
			dp = mtod(m, char *);
			if (((int)cp&0x3ff)==0 && ((int)dp&0x3ff)==0) {
				struct pte *pte = &Mbmap[mtopf(dp)*2];
				*(int *)enxmr = enwproto | pte++->pg_pfnum;
				*(int *)(enxmr+1) = enwproto | pte->pg_pfnum;
				enxswapd = enxswapnow = 1;
			} else
				bcopy(mtod(m, caddr_t), cp,
				    (unsigned)m->m_len);
			cp += m->m_len;
			/* too soon! */
			MFREE(m, mp);
			m = mp;
		}
		if (enxswapnow == 0 && enxswapd) {
			enxmr[0] = enxmap[0];
			enxmr[1] = enxmap[1];
		}
		if (enlastx && enlastx == xpkt->Header.en_dhost)
			imp_stat.endelay = enlastdel;
		else
			enlastx = xpkt->Header.en_dhost;
	}
	len = ntohs((u_short)(((struct ip *)((int)xpkt + L1822))->ip_len)) +
	    L1822;
	if (len > sizeof(struct en_packet)) {
		printf("imp_output: ridiculous IP length %d\n", len);
		return;
	}
#if defined(VAX780) || defined(VAX750)
	switch (cpu) {
#if defined(VAX780)
	case VAX_780:
		UBA_PURGE780(enuba, enwbdp);
		break;
#endif
#if defined(VAX750)
	case VAX_750:
		UBA_PURGE750(enuba, enwbdp);
		break;
#endif
	}
#endif
	addr->en_oba = imp_stat.oaddr;
	addr->en_odelay = imp_stat.endelay;
	addr->en_owc = -((len + 1) >> 1);
#ifdef IMPDEBUG
	printf("en%d: sending packet (%d bytes)\n", unit, len);
	prt_byte(xpkt, len);
#endif
	addr->en_ostat = EN_IEN|EN_GO;
}

/*
 * Output interrupt handler.
 */
enxint(unit)
	int unit;
{
	register struct endevice *addr;
	register struct uba_device *ui;
COUNT(ENXINT);

	ui = eninfo[unit];
	addr = (struct endevice *)ui->ui_addr;

#ifdef IMPDEBUG
	printf("en%d: enxint ostat=%b\n", unit, addr->en_ostat, EN_BITS);
#endif
	if (!imp_stat.outactive) {
		printf("en%d: phantom output intr ostat=%b\n",
			unit, addr->en_ostat, EN_BITS);
		return;
	}
	imp_stat.endelay = 0;
	imp_stat.enmask = ~0;
	if (addr->en_ostat&EN_OERROR)
		printf("en%d: output error ostat=%b\n", unit,
			addr->en_ostat, EN_BITS);
	imp_stat.outactive = 0;
	if (imp_stat.outq_head)
		enstart(unit);
	else
		enlastx = 0;
}

int collisions;
encollide(unit)
	int unit;
{
	register struct endevice *addr;
	register struct uba_device *ui;
COUNT(ENCOLLIDE);

	collisions++;
	ui = eninfo[unit];
	addr = (struct endevice *)ui->ui_addr;

#ifdef IMPDEBUG
	printf("en%d: collision ostat=%b\n", unit, addr->en_ostat, EN_BITS);
#endif
	if (!imp_stat.outactive) {
		printf("en%d: phantom collision intr ostat=%b\n",
			unit, addr->en_ostat, EN_BITS);
		return;
	}
	if (imp_stat.enmask == 0) {
		printf("en%d: output error ostat=%b\n", unit,
			addr->en_ostat, EN_BITS);
	} else {
		imp_stat.enmask <<= 1;
		imp_stat.endelay = mfpr(ICR) & ~imp_stat.enmask;
	}
	enstart(unit);
}

enrint(unit)
	int unit;
{
    	register struct mbuf *m;
	struct mbuf *mp;
	register struct endevice *addr;
	register struct uba_device *ui;
	register int len;
	register caddr_t cp;
	struct mbuf *p, *top = 0;
	struct ip *ip;
	u_int hlen;
COUNT(ENRINT);

	ui = eninfo[unit];
	addr = (struct endevice *)ui->ui_addr;
#ifdef IMPDEBUG
	printf("en%d: enrint istat=%b\n", unit, addr->en_istat, EN_BITS);
#endif
	if (imp_stat.flush)
		goto flush;
	if (addr->en_istat&EN_IERROR) {
#ifdef notdef
		printf("en%d: input error istat=%b\n", unit,
			addr->en_istat, EN_BITS);
#endif
		goto flush;
	}
#if defined(VAX780) || defined(VAX750)
	switch (cpu) {
#if defined(VAX780)
	case VAX_780:
		UBA_PURGE780(enuba, enrbdp);
		break;
#endif
#if defined(VAX750)
	case VAX_750:
		UBA_PURGE750(enuba, enrbdp);
		break;
#endif
	}
#endif
	ip = (struct ip *)((int)rpkt + L1822);
	len = ntohs(ip->ip_len) + L1822;
	if (len > sizeof(struct en_packet) || len < sizeof (struct ip)) {
		printf("enrint: bad ip length %d\n", len);
		goto flush;
	}
	hlen = L1822 + sizeof (struct ip);
	switch (ip->ip_p) {

	case IPPROTO_TCP:
		hlen += ((struct tcpiphdr *)ip)->ti_off << 2;
		break;
	}
	MGET(m, 0);
	if (m == 0)
		goto flush;
	top = m;
	m->m_off = MMINOFF;
	m->m_len = hlen;
	bcopy((caddr_t)rpkt, mtod(m, caddr_t), hlen);
	len -= hlen;
	cp = (caddr_t)rpkt + hlen;
	mp = m;
	while (len > 0) {
		MGET(m, 0);
		if (m == NULL)
			goto flush;
		if (len >= PGSIZE) {
			MPGET(p, 1);
			if (p == 0)
				goto nopage;
			m->m_len = PGSIZE;
			m->m_off = (int)p - (int)m;
			if (((int)cp & 0x3ff) == 0) {
				struct pte *cpte = &Mbmap[mtopf(cp)*2];
				struct pte *ppte = &Mbmap[mtopf(p)*2];
				struct pte t;
				enrswaps++;
				t = *ppte; *ppte++ = *cpte; *cpte++ = t;
				t = *ppte; *ppte = *cpte; *cpte = t;
				mtpr(TBIS, (caddr_t)cp);
				mtpr(TBIS, (caddr_t)cp+512);
				mtpr(TBIS, (caddr_t)p);
				mtpr(TBIS, (caddr_t)p+512);
				*(int *)(enrmr+1) =
				    cpte[0].pg_pfnum | enrproto;
				*(int *)(enrmr) =
				    cpte[-1].pg_pfnum | enrproto;
				goto nocopy;
			}
		} else {
nopage:
			m->m_len = MIN(MLEN, len);
			m->m_off = MMINOFF;
		}
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
nocopy:
		cp += m->m_len;
		len -= m->m_len;
		mp->m_next = m;
		mp = m;
	}
	m = top;
	if (imp_stat.inq_head != NULL)
		imp_stat.inq_tail->m_act = m;
	else
		imp_stat.inq_head = m;
	imp_stat.inq_tail = m;
#ifdef IMPDEBUG
	printf("en%d: received packet (%d bytes)\n", unit, len);
	prt_byte(rpkt, len);
#endif
	setsoftnet();
	goto setup;
flush:
	m_freem(top);
#ifdef IMPDEBUG
	printf("en%d: flushing packet %x\n", unit, top);
#endif
setup:
	addr->en_iba = imp_stat.iaddr;
	addr->en_iwc = -600;
	addr->en_istat = EN_IEN|EN_GO;
}

#ifdef IMPDEBUG
prt_byte(s, ct)
	register char *s;
	int ct;
{
	register i, j, c;

	for (i=0; i<ct; i++) {
		c = *s++;
		for (j=0; j<2 ; j++)
			putchar("0123456789abcdef"[(c>>((1-j)*4))&0xf]);
		putchar(' ');
	}
	putchar('\n');
}
#endif IMPDEBUG
