/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *      @(#)spp_debug.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"

#include "../net/route.h"
#include "../net/if.h"
#include "../netinet/tcp_fsm.h"
#include "../netinet/tcp_timer.h"

#include "ns.h"
#include "ns_pcb.h"
#include "idp.h"
#include "idp_var.h"
#include "sp.h"
#include "spidp.h"
#include "spp_var.h"
#define	SANAMES
#include "spp_debug.h"

int	sppconsdebug = 0;
/*
 * spp debug routines
 */
spp_trace(act, ostate, sp, si, req)
	short act;
	u_char ostate;
	struct sppcb *sp;
	struct spidp *si;
	int req;
{
#ifdef INET
	u_short seq, ack, len, alo;
	unsigned long iptime();
	int flags;
	struct spp_debug *sd = &spp_debug[spp_debx++];
	extern char *prurequests[];
	extern char *sanames[];
	extern char *tcpstates[];
	extern char *tcptimers[];

	if (spp_debx == SPP_NDEBUG)
		spp_debx = 0;
	sd->sd_time = iptime();
	sd->sd_act = act;
	sd->sd_ostate = ostate;
	sd->sd_cb = (caddr_t)sp;
	if (sp)
		sd->sd_sp = *sp;
	else
		bzero((caddr_t)&sd->sd_sp, sizeof (*sp));
	if (si)
		sd->sd_si = *si;
	else
		bzero((caddr_t)&sd->sd_si, sizeof (*si));
	sd->sd_req = req;
	if (sppconsdebug == 0)
		return;
	if (ostate >= TCP_NSTATES) ostate = 0;
	if (act >= SA_DROP) act = SA_DROP;
	if (sp)
		printf("%x %s:", sp, tcpstates[ostate]);
	else
		printf("???????? ");
	printf("%s ", sanames[act]);
	switch (act) {

	case SA_RESPOND:
	case SA_INPUT:
	case SA_OUTPUT:
	case SA_DROP:
		if (si == 0)
			break;
		seq = si->si_seq;
		ack = si->si_ack;
		alo = si->si_alo;
		len = si->si_len;
		if (act == SA_OUTPUT) {
			seq = ntohs(seq);
			ack = ntohs(ack);
			alo = ntohs(alo);
			len = ntohs(len);
		}
#ifndef lint
#define p1(f)  { printf("%s = %x, ", "f", f); }
		p1(seq); p1(ack); p1(alo); p1(len);
#endif
		flags = si->si_cc;
		if (flags) {
			char *cp = "<";
#ifndef lint
#define pf(f) { if (flags&SP_/**/f) { printf("%s%s", cp, "f"); cp = ","; } }
			pf(SP); pf(SA); pf(OB); pf(EM);
#else
			cp = cp;
#endif
			printf(">");
		}
#ifndef lint
#define p2(f)  { printf("%s = %x, ", "f", si->si_/**/f); }
		p2(sid);p2(did);p2(dt);p2(pt);
#endif
		ns_printhost(&si->si_sna);
		ns_printhost(&si->si_dna);

		if (act==SA_RESPOND) {
			printf("idp_len = %x, ",
				((struct idp *)si)->idp_len);
		}
		break;

	case SA_USER:
		printf("%s", prurequests[req&0xff]);
		if ((req & 0xff) == PRU_SLOWTIMO)
			printf("<%s>", tcptimers[req>>8]);
		break;
	}
	if (sp)
		printf(" -> %s", tcpstates[sp->s_state]);
	/* print out internal state of sp !?! */
	printf("\n");
	if (sp == 0)
		return;
#ifndef lint
#define p3(f)  { printf("%s = %x, ", "f", sp->s_/**/f); }
	printf("\t"); p3(rack);p3(ralo);p3(smax);p3(flags); printf("\n");
#endif
#endif
}
