/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mbuf.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/mbuf.h>

#include <stdio.h>
#include "netstat.h"

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
	{ MT_IFADDR,	"interface addresses" },		/* XXX */
	{ 0, 0 }
};

int nmbtypes = sizeof(mbstat.m_mtypes) / sizeof(short);
bool seen[256];			/* "have we seen this type yet?" */

/*
 * Print mbuf statistics.
 */
void
mbpr(mbaddr)
	u_long mbaddr;
{
	register int totmem, totfree, totmbufs;
	register int i;
	register struct mbtypes *mp;

	if (nmbtypes != 256) {
		fprintf(stderr,
		    "%s: unexpected change to mbstat; check source\n", prog);
		return;
	}
	if (mbaddr == 0) {
		fprintf(stderr, "%s: mbstat: symbol not in namelist\n", prog);
		return;
	}
	if (kread(mbaddr, (char *)&mbstat, sizeof (mbstat)))
		return;
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
	totmem = totmbufs * MSIZE + mbstat.m_clusters * MCLBYTES;
	totfree = mbstat.m_clfree * MCLBYTES;
	printf("%u Kbytes allocated to network (%d%% in use)\n",
		totmem / 1024, (totmem - totfree) * 100 / totmem);
	printf("%u requests for memory denied\n", mbstat.m_drops);
	printf("%u requests for memory delayed\n", mbstat.m_wait);
	printf("%u calls to protocol drain routines\n", mbstat.m_drain);
}
