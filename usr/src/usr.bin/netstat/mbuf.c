#ifndef lint
static char sccsid[] = "@(#)mbuf.c	4.3 82/12/06";
#endif

#include <sys/param.h>
#include <sys/mbuf.h>

struct	mbstat mbstat;
extern	int kmem;

/*
 * Print mbuf statistics.
 */
mbpr(mbaddr)
	off_t mbaddr;
{
	register int totmem, totfree;

	if (mbaddr == 0) {
		printf("mbstat: symbol not in namelist\n");
		return;
	}
	printf("memory utilization:\n");
	klseek(kmem, mbaddr, 0);
	if (read(kmem, &mbstat, sizeof (mbstat)) != sizeof (mbstat)) {
		printf("mbstat: bad read\n");
		return;
	}
	printf("\t%d/%d mbufs in use\n", mbstat.m_mbufs - mbstat.m_mbfree,
		mbstat.m_mbufs);
	printf("\t%d/%d mapped pages in use\n",
		mbstat.m_clusters - mbstat.m_clfree, mbstat.m_clusters);
	printf("\t%d requests for memory denied\n", mbstat.m_drops);
	totmem = mbstat.m_mbufs * MSIZE + mbstat.m_clusters * CLBYTES;
	totfree = mbstat.m_mbfree * MSIZE + mbstat.m_clusters * CLBYTES;
	printf("\t%dKbytes allocated to network (%d%% in use)\n",
		totmem / 1024, (totmem - totfree) * 100 / totmem);
}
