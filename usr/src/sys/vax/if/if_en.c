/*	if_en.c	4.14	81/11/26	*/

#include "en.h"
/*
 * Ethernet interface driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/enreg.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_en.h"
#include "../net/if_uba.h"
#include "../net/ip.h"
#include "../net/ip_var.h"

#define	ENMTU	1024

int	enprobe(), enattach(), enrint(), enxint(), encollide();
struct	uba_device *eninfo[NEN];
u_short enstd[] = { 0 };
struct	uba_driver endriver =
	{ enprobe, 0, enattach, 0, enstd, "es", eninfo };
#define	ENUNIT(x)	minor(x)

struct	en_softc {
	struct	ifnet *es_if;
	struct	ifuba *es_ifuba;
	short	es_delay;
	short	es_mask;
	u_char	es_addr;
	u_char	es_lastx;
	short	es_oactive;
	short	es_olen;
} en_softc[NEN];

enprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct endevice *addr = (struct endevice *)reg;

COUNT(ENPROBE);
#ifdef lint
	br = 0; cvec = br; br = cvec;
	enrint(0); enxint(0); encollide(0);
#endif
	addr->en_istat = 0;
	addr->en_owc = -1;
	addr->en_oba = 0;
	addr->en_ostat = EN_IEN|EN_GO;
	DELAY(100000);
	addr->en_ostat = 0;
	return (1);
}

/*ARGSUSED*/
enattach(ui)
	struct uba_device *ui;
{

COUNT(ENATTACH);
	eninit(ui->ui_unit);
	/* net initialization, based on ui->ui_flags?!? */
}

eninit(unit)
	int unit;
{
	register struct uba_device *ui;
	register struct endevice *addr;
	register struct en_softc *es;

COUNT(ENINIT);
	if (unit >= NEN || (ui = eninfo[unit]) == 0 || ui->ui_alive == 0) {
		printf("es%d: not alive\n", unit);
		return;
	}
	es = &en_softc[unit];
	if (if_ubainit(es->es_ifuba, ui->ui_ubanum,
	    sizeof (struct en_header), btop(ENMTU)) == 0) { 
		printf("es%d: can't initialize\n", unit);
		return;
	}
	addr = (struct endevice *)ui->ui_addr;
	addr->en_istat = addr->en_ostat = 0;
}

enreset(uban)
	int uban;
{
	register int unit;
	struct uba_device *ui;

COUNT(ENRESET);
	for (unit = 0; unit < NEN; unit++) {
		ui = eninfo[unit];
		if (ui == 0 || ui->ui_ubanum != uban || ui->ui_alive == 0)
			continue;
		eninit(unit);
		printf("es%d ", unit);
	}
}

int	enlastdel = 25;

enstart(dev)
	dev_t dev;
{
        int unit;
	struct uba_device *ui;
	register struct endevice *addr;
	register struct en_softc *es;
	register struct ifuba *ifu;
	struct mbuf *m;
	int dest;
COUNT(ENSTART);

	unit = ENUNIT(dev);
	ui = eninfo[unit];
	es = &en_softc[unit];
	if (es->es_oactive)
		goto restart;
	IF_DEQUEUE(&es->es_if->if_snd, m);
	if (m == 0) {
		es->es_oactive = 0;
		return;
	}
	dest = mtod(m, struct en_header *)->en_dhost;
	es->es_olen = if_wubaput(&es->es_ifuba, m);
	if (es->es_lastx && es->es_lastx == dest)
		es->es_delay = enlastdel;
	else
		es->es_lastx = dest;
restart:
	ifu = es->es_ifuba;
	UBAPURGE(ifu->ifu_uba, ifu->ifu_w.ifrw_bdp);
	addr = (struct endevice *)ui->ui_addr;
	addr->en_oba = (int)ifu->ifu_w.ifrw_addr;
	addr->en_odelay = es->es_delay;
	addr->en_owc = -((es->es_olen + 1) >> 1);
	addr->en_ostat = EN_IEN|EN_GO;
	es->es_oactive = 1;
}

enxint(unit)
	int unit;
{
	register struct endevice *addr;
	register struct uba_device *ui;
	register struct en_softc *es;
COUNT(ENXINT);

	ui = eninfo[unit];
	es = &en_softc[unit];
	if (es->es_oactive == 0)
		return;
	addr = (struct endevice *)ui->ui_addr;
	es = &en_softc[unit];
	es->es_oactive = 0;
	es->es_delay = 0;
	es->es_mask = ~0;
	if (addr->en_ostat&EN_OERROR)
		printf("es%d: output error\n", unit);
	if (es->es_if->if_snd.ifq_head == 0) {
		es->es_lastx = 0;
		return;
	}
	enstart(unit);
}

encollide(unit)
	int unit;
{
	register struct en_softc *es;
COUNT(ENCOLLIDE);

	es = &en_softc[unit];
	es->es_if->if_collisions++;
	if (es->es_oactive == 0)
		return;
	if (es->es_mask == 0) {
		printf("es%d: send error\n", unit);
		enxint(unit);
	} else {
		es->es_mask <<= 1;
		es->es_delay = mfpr(ICR) &~ es->es_mask;
		enstart(unit);
	}
}

enrint(unit)
	int unit;
{
	struct endevice *addr;
	register struct en_softc *es;
	register struct ifuba *ifu;
	struct en_header *en;
    	struct mbuf *m;
	struct ifqueue *inq;
	register int len;
	int off;
COUNT(ENRINT);

	addr = (struct endevice *)eninfo[unit]->ui_addr;
	if (addr->en_istat&EN_IERROR) {
		es->es_if->if_ierrors++;
		printf("es%d: input error\n", unit);
		goto setup;
	}
	ifu = en_softc[unit].es_ifuba;
	UBAPURGE(ifu->ifu_uba, ifu->ifu_r.ifrw_bdp);
	en = (struct en_header *)(ifu->ifu_r.ifrw_addr);
#define	endataaddr(en, off, type)	((type)(((caddr_t)((en)+1)+(off))))
	if (en->en_type >= ENPUP_TRAIL &&
	    en->en_type < ENPUP_TRAIL+ENPUP_NTRAILER) {
		off = (en->en_type - ENPUP_TRAIL) * 512;
		en->en_type = *endataaddr(en, off, u_short *);
		off += 2;
	} else
		off = 0;
	switch (en->en_type) {

#ifdef INET
	case ENPUP_IPTYPE:
		len = endataaddr(en, off, struct ip *)->ip_len;
		setipintr();
		inq = &ipintrq;
		break;
#endif

	default:
		printf("en%d: unknow pkt type 0x%x\n", en->en_type);
		goto setup;
	}
	if (len == 0)
		goto setup;
	m = if_rubaget(&ifu->ifu_r, len, off);
	IF_ENQUEUE(inq, m);
setup:
	addr->en_iba = es->es_ifuba->ifu_r.ifrw_info;
	addr->en_iwc = -(sizeof (struct en_header) + ENMTU) >> 1;
	addr->en_istat = EN_IEN|EN_GO;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 */
enoutput(ifp, m0, pf)
	struct ifnet *ifp;
	struct mbuf *m0;
	int pf;
{
	int type, dest;
	register struct mbuf *m;
	register struct en_header *en;
	int s;

	switch (pf) {

#ifdef INET
	case PF_INET: {
		register struct ip *ip = mtod(m0, struct ip *);
		int off;

		off = ip->ip_len - (ip->ip_hl << 2);
		if (off && off % 512 == 0 && m0->m_off >= MMINOFF + 2) {
			type = ENPUP_TRAIL + (off>>9);
			m0->m_off -= 2;
			m0->m_len += 2;
			*mtod(m0, u_short *) = ENPUP_IPTYPE;
		} else {
			type = ENPUP_IPTYPE;
			off = 0;
		}
		dest = ip->ip_dst.s_addr >> 24;
		}
		break;
#endif

	default:
		printf("en%d: can't encapsulate pf%d\n", ifp->if_unit, pf);
		m_freem(m0);
		return (0);
	}
	if (MMINOFF + sizeof (struct en_header) > m0->m_off) {
		m = m_get(0);
		if (m == 0) {
			m_freem(m0);
			return (0);
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct en_header);
	} else {
		m = m0;
		m->m_off -= sizeof (struct en_header);
		m->m_len += sizeof (struct en_header);
	}
	en = mtod(m, struct en_header *);
	en->en_shost = ifp->if_host[0];
	en->en_dhost = dest;
	en->en_type = type;
	s = splimp();
	IF_ENQUEUE(&ifp->if_snd, m);
	splx(s);
	if (en_softc[ifp->if_unit].es_oactive == 0)
		enstart(ifp->if_unit);
}
