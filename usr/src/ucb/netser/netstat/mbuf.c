#ifndef lint
static char sccsid[] = "@(#)mbuf.c	4.5 82/12/18";
#endif

#include <sys/param.h>
#include <sys/mbuf.h>

struct	mbstat mbstat;
extern	int kmem;

static struct mbtypes {
	int	mt_type;
	char	*mt_name;
} mbtypes[] = {
	{ MT_DATA,	"data" },
	{ MT_HEADER,	"packet headers" },
	{ MT_SOCKET,	"socket structures" },
	{ MT_PCB,	"protocol control blocks" },
	{ MT_RTABLE,	"routing table entries" },
	{ MT_HTABLE,	"IMP host table entries" },
#ifdef notdef
	{ MT_ATABLE,	"address resolution tables" },
#endif
	{ MT_FTABLE,	"fragment reassembly queue headers" },
	{ MT_SONAME,	"socket names and addresses" },
	{ MT_ZOMBIE,	"zombie process information" },
	{ MT_SOOPTS,	"socket options" },
	{ 0, 0 }
};

/*
 * Print mbuf statistics.
 */
mbpr(mbaddr)
	off_t mbaddr;
{
	register int totmem, totfree, totmbufs;
	register struct mbtypes *mp;

	if (mbaddr == 0) {
		printf("mbstat: symbol not in namelist\n");
		return;
	}
	klseek(kmem, mbaddr, 0);
	if (read(kmem, &mbstat, sizeof (mbstat)) != sizeof (mbstat)) {
		printf("mbstat: bad read\n");
		return;
	}
	printf("%d/%d mbufs in use:\n",
		mbstat.m_mbufs - mbstat.m_mbfree, mbstat.m_mbufs);
	totmbufs = 0;
	for (mp = mbtypes; mp->mt_name; mp++)
		if (mbstat.m_mtypes[mp->mt_type]) {
			printf("\t%d mbufs allocated to %s\n",
				mbstat.m_mtypes[mp->mt_type], mp->mt_name);
			totmbufs += mbstat.m_mtypes[mp->mt_type];
		}
	if (totmbufs != mbstat.m_mbufs - mbstat.m_mbfree)
		printf("*** %d mbufs missing ***\n",
			(mbstat.m_mbufs - mbstat.m_mbfree) - totmbufs);
	printf("%d/%d mapped pages in use\n",
		mbstat.m_clusters - mbstat.m_clfree, mbstat.m_clusters);
	totmem = mbstat.m_mbufs * MSIZE + mbstat.m_clusters * CLBYTES;
	totfree = mbstat.m_mbfree * MSIZE + mbstat.m_clusters * CLBYTES;
	printf("%d Kbytes allocated to network (%d%% in use)\n",
		totmem / 1024, (totmem - totfree) * 100 / totmem);
	printf("%d requests for memory denied\n", mbstat.m_drops);
}
