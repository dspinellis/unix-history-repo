/*
 * Copyright (c) 1983,1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)host.c	5.3 (Berkeley) 8/11/86";
#endif not lint

#include <sys/types.h>
#include <sys/mbuf.h>

#include <netinet/in.h>
#include <netimp/if_imp.h>
#include <netimp/if_imphost.h>

extern	int kmem;
extern 	int nflag;
extern	char *inetname();

/*
 * Print the host tables associated with the ARPANET IMP.
 * Symbolic addresses are shown unless the nflag is given.
 */
hostpr(hostsaddr)
	off_t hostsaddr;
{
	struct mbuf *hosts, mb;
	register struct mbuf *m;
	register struct hmbuf *mh;
	register struct host *hp;
	char flagbuf[10], *flags;

	if (hostsaddr == 0) {
		printf("hosts: symbol not in namelist\n");
		return;
	}
	klseek(kmem, hostsaddr, 0);
	read(kmem, (char *)&hosts, sizeof (hosts));
	m = hosts;
	printf("IMP Host Table\n");
	printf("%-5.5s %-15.15s %-4.4s %-9.9s %-4.4s %s\n",
		"Flags", "Host", "Qcnt", "Q Address", "RFNM", "Timer");
	while (m) {
		klseek(kmem, (off_t)m, 0);
		read(kmem, (char *)&mb, sizeof (mb));
		m = &mb;
		mh = mtod(m, struct hmbuf *);
		if (mh->hm_count == 0) {
			m = m->m_next;
			continue;
		}
		for (hp = mh->hm_hosts; hp < mh->hm_hosts + HPMBUF; hp++) {
			if ((hp->h_flags&HF_INUSE) == 0 && hp->h_timer == 0)
				continue;
			flags = flagbuf;
			*flags++ = hp->h_flags&HF_INUSE ? 'A' : 'F';
			if (hp->h_flags&HF_DEAD)
				*flags++ = 'D';
			if (hp->h_flags&HF_UNREACH)
				*flags++ = 'U';
			*flags = '\0';
			printf("%-5.5s %-15.15s %-4d %-9x %-4d %d\n",
				flagbuf,
				inetname(hp->h_addr),
				hp->h_qcnt,
				hp->h_q,
				hp->h_rfnm,
				hp->h_timer);
		}
		m = m->m_next;
	}
}
