#ifndef lint
static char sccsid[] = "@(#)mbuf.c	4.5 82/12/18";
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
	klseek(kmem, mbaddr, 0);
	if (read(kmem, &mbstat, sizeof (mbstat)) != sizeof (mbstat)) {
		printf("mbstat: bad read\n");
		return;
	}
	printf("%d/%d mbufs in use\n", mbstat.m_mbufs - mbstat.m_mbfree,
		mbstat.m_mbufs);
	printf("%d/%d mapped pages in use\n",
		mbstat.m_clusters - mbstat.m_clfree, mbstat.m_clusters);
	printf("%d requests for memory denied\n", mbstat.m_drops);
	totmem = mbstat.m_mbufs * MSIZE + mbstat.m_clusters * CLBYTES;
	totfree = mbstat.m_mbfree * MSIZE + mbstat.m_clusters * CLBYTES;
	printf("%dKbytes allocated to network (%d%% in use)\n",
		totmem / 1024, (totmem - totfree) * 100 / totmem);
}
