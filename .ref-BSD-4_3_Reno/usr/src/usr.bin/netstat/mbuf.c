/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)mbuf.c	5.9 (Berkeley) 6/18/90";
#endif /* not lint */

#include <stdio.h>
#include <sys/param.h>
#include <sys/mbuf.h>
#define	YES	1
typedef int bool;

struct	mbstat mbstat;

static struct mbtypes {
	int	mt_type;
	char	*mt_name;
} mbtypes[] = {
	{ MT_DATA,	"data" },
	{ MT_OOBDATA,	"oob data" },
	{ MT_CONTROL,	"ancillary data" },
	{ MT_HEADER,	"packet headers" },
	{ MT_SOCKET,	"socket structures" },			/* XXX */
	{ MT_PCB,	"protocol control blocks" },		/* XXX */
	{ MT_RTABLE,	"routing table entries" },		/* XXX */
	{ MT_HTABLE,	"IMP host table entries" },		/* XXX */
	{ MT_ATABLE,	"address resolution tables" },
	{ MT_FTABLE,	"fragment reassembly queue headers" },	/* XXX */
	{ MT_SONAME,	"socket names and addresses" },
	{ MT_SOOPTS,	"socket options" },
	{ MT_RIGHTS,	"access rights" },
	{ MT_IFADDR,	"interface addresses" }, 		/* XXX */
	{ 0, 0 }
};

int nmbtypes = sizeof(mbstat.m_mtypes) / sizeof(short);
bool seen[256];			/* "have we seen this type yet?" */

/*
 * Print mbuf statistics.
 */
mbpr(mbaddr)
	off_t mbaddr;
{
	register int totmem, totfree, totmbufs;
	register int i;
	register struct mbtypes *mp;

	if (nmbtypes != 256) {
		fprintf(stderr, "unexpected change to mbstat; check source\n");
		return;
	}
	if (mbaddr == 0) {
		printf("mbstat: symbol not in namelist\n");
		return;
	}
	if (kvm_read(mbaddr, (char *)&mbstat, sizeof (mbstat))
						!= sizeof (mbstat)) {
		printf("mbstat: bad read\n");
		return;
	}
	totmbufs = 0;
	for (mp = mbtypes; mp->mt_name; mp++)
		totmbufs += mbstat.m_mtypes[mp->mt_type];
	printf("%u mbufs in use:\n", totmbufs);
	for (mp = mbtypes; mp->mt_name; mp++)
		if (mbstat.m_mtypes[mp->mt_type]) {
			seen[mp->mt_type] = YES;
			printf("\t%u mbufs allocated to %s\n",
			    mbstat.m_mtypes[mp->mt_type], mp->mt_name);
		}
	seen[MT_FREE] = YES;
	for (i = 0; i < nmbtypes; i++)
		if (!seen[i] && mbstat.m_mtypes[i]) {
			printf("\t%u mbufs allocated to <mbuf type %d>\n",
			    mbstat.m_mtypes[i], i);
		}
	printf("%u/%u mapped pages in use\n",
		mbstat.m_clusters - mbstat.m_clfree, mbstat.m_clusters);
	totmem = totmbufs * MSIZE + mbstat.m_clusters * CLBYTES;
	totfree = mbstat.m_clfree * CLBYTES;
	printf("%u Kbytes allocated to network (%d%% in use)\n",
		totmem / 1024, (totmem - totfree) * 100 / totmem);
	printf("%u requests for memory denied\n", mbstat.m_drops);
	printf("%u requests for memory delayed\n", mbstat.m_wait);
	printf("%u calls to protocol drain routines\n", mbstat.m_drain);
}
