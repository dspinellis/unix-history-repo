
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/clock.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../net/dn_systm.h"
#include "../net/if.h"
#include "../net/tp.h"
#include "../net/tp_var.h"


/*
 * Initialize a few of the Transport variables here.
 */
tp_init()
{
	tp_host = 244;
	tprp.tprp_nn = 255;	/* max node number */
}

/*
 * Attach a DECnet interface.
 * For now, since we are an end node,
 * there can only be one.
 */
tp_attach(ifp)
	register struct ifnet *ifp;
{
	if (tpifp) {
		printf("tp: Only one DECnet interface allowed, ");
		printf("%s%d ignored\n", ifp->if_name, ifp->if_unit);
		return;
	}
	tpifp = ifp;
}

/*
 * Transport input routine.  Decode header, process
 * initialization messages, flush other control messages,
 * and strip route headers and pass to NSP.
 */
tp_input()
{
	register struct mbuf *m;
	register char *p;

next:
	/*
	 * Get next packet off input queue.
	 */
	IF_DEQUEUE(&tpintrq, m);
	if (m == 0)
		return;
	p = mtod(m, char *);
	switch (*p & TP_MSGTYPE) {
	/*
	 * Transport initialization message from neighbor.
	 */
	case TP_INIT:
	{
		register struct tpin *t = (struct tpin *)p;

		printf("tpinit: node %d, %d, blksize %d, ver %o.%o.%o\n",
			D_SHORT(t->tpin_srcnode), t->tpin_tiinfo,
			D_SHORT(t->tpin_blksize),
			t->tpin_ver[0], t->tpin_ver[1], t->tpin_ver[2]);
		/* perform a few consistency checks */
		if (m->m_len < sizeof (struct tpin)) {
			tpstat.tps_badinit++;
			break;
		}
		if (D_SHORT(t->tpin_srcnode) > tprp.tprp_nn) {
			tpstat.tps_badinit++;
			break;
		}
		if (t->tpin_res != 0) {
			tpstat.tps_badinit++;
			break;
		}
		tpstat.tps_init++;
		if (tpstate == TPS_TIS) {
			tpstate = TPS_RUN;
			wakeup((caddr_t)&tpstate);
		} else if (tpstate == TPS_HALT) {
			tp_linit();
			tpstate = TPS_RUN;
		}
		break;
	}

	/*
	 * Route header.  Flush bad ones,
	 * strip good ones and pass to NSP.
	 */
	case TP_RH:
	{
		register struct tprh *t = (struct tprh *)p;

		/*
		 * Is it a reasonable route header?
		 */
		if (tpstate != TPS_RUN) {
			printf("tp: not running!\n");
			break;
		}
		if (t->tprh_rtflg & TPRF_EV) {
			printf("tp: got P2 ASCII header\n");
			tpstat.tps_p2hdr++;
			break;
		}
		if (t->tprh_rtflg & TPRF_RTS) {
			printf("tp: got returned packet\n");
			tpstat.tps_returned++;
			break;
		}
		if (m->m_len <= sizeof (struct tprh)) {
			printf("tp: got short packet, %d\n", m->m_len);
			tpstat.tps_shortpacket++;
			break;
		}
		if (D_SHORT(t->tprh_srcnode) > tprp.tprp_nn) {
			tpstat.tps_badsrc++;
			break;
		}

		/*
		 * Is it for us?  If so, 
		 * add it to the NSP input queue.
		 */
		if (D_SHORT(t->tprh_dstnode) != tp_host) {
			printf("tp: not for me, %d\n", D_SHORT(t->tprh_dstnode));
			tpstat.tps_notforme++;
			break;
		}
		setnspintr();
		IF_ENQUEUE(&nspintrq, m);
		goto next;
	}

	/*
	 * Verification messge.  We should never see one
	 * of these because we never ask for one.  Flush it.
	 */
	case TP_VERIF:
		printf("tp: got verification message\n");
		tpstat.tps_verif++;
		break;

	/*
	 * Hello and test message.  Make sure it's
	 * valid then flush it.
	 */
	case TP_TEST:
	{
		register struct tpht *t = (struct tpht *)p;
		register int i;

		if (D_SHORT(t->tpht_srcnode) > tprp.tprp_nn) {
			tpstat.tps_badsrc++;
			break;
		}
		if ((i = t->tpht_cnt) < 0 || i > 128) {
			printf("tp: test, bad count, %d\n", i);
			tpstat.tps_badtest++;
			break;
		}
		if (m->m_len != sizeof (struct tpht) + i - 1) {
			printf("tp: test, bad len, %d\n", m->m_len);
			tpstat.tps_bad_test++;
			break;
		}
		for (p = t->tpht_data; i--; p++)
			if (*p != 0252) {
				printf("tp: test, bad data, %o\n", *p);
				tpstat.tps_badtest++;
				break;
			}
		break;
	}

	/*
	 * Routing message.  We should never get this,
	 * at least not yet.  Just flush it.
	 */
	case TP_ROUTE:
		printf("tp: got routing message\n");
		tpstat.tps_route++;
		break;

	default:
		printf("tp: unknown packet type, 0x%x\n", *p);
		tpstat.tps_unknown++;
		break;
	}

	/*
	 * Free the current packet and get the next one.
	 */
	m_freem(m);
	goto next;
}
