/*	mbuf.h	4.12	82/05/18	*/

/*
 * Constants related to memory allocator.
 */
#define	MSIZE		128			/* size of an mbuf */
#define	MMINOFF		12			/* mbuf header length */
#define	MTAIL		4
#define	MMAXOFF		(MSIZE-MTAIL)		/* offset where data ends */
#define	MLEN		(MSIZE-MMINOFF-MTAIL)	/* mbuf data length */
#define	NMBCLUSTERS	256
#define	NMBPCL		(CLBYTES/MSIZE)		/* # mbufs per cluster */

/*
 * Macros for type conversion
 */

/* network cluster number to virtual address, and back */
#define	cltom(x) ((struct mbuf *)((int)mbutl + ((x) << CLSHIFT)))
#define	mtocl(x) (((int)x - (int)mbutl) >> CLSHIFT)

/* address in mbuf to mbuf head */
#define	dtom(x)		((struct mbuf *)((int)x & ~(MSIZE-1)))

/* mbuf head, to typed data */
#define	mtod(x,t)	((t)((int)(x) + (x)->m_off))

struct mbuf {
	struct	mbuf *m_next;		/* next buffer in chain */
	u_long	m_off;			/* offset of data */
	short	m_len;			/* amount of data in this mbuf */
	short	m_free;			/* is mbuf free? (consistency check) */
	u_char	m_dat[MLEN];		/* data storage */
	struct	mbuf *m_act;		/* link in higher-level mbuf list */
};

/* flags to m_get */
#define	M_DONTWAIT	0
#define	M_WAIT		1

/* flags to m_pgalloc */
#define	MPG_MBUFS	0		/* put new mbufs on free list */
#define	MPG_CLUSTERS	1		/* put new clusters on free list */
#define	MPG_SPACE	2		/* don't free; caller wants space */

/* length to m_copy to copy all */
#define	M_COPYALL	1000000000

#define	MGET(m, i) \
	{ int ms = splimp(); \
	  if ((m)=mfree) \
		{ if ((m)->m_free == 0) panic("mget"); (m)->m_free = 0; \
		  mbstat.m_mbfree--; mfree = (m)->m_next; (m)->m_next = 0; } \
	  else \
		(m) = m_more(i); \
	  splx(ms); }
#define	MCLGET(m, i) \
	{ int ms = splimp(); \
	  if ((m)=mclfree) \
	     {++mclrefcnt[mtocl(m)];mbstat.m_clfree--;mclfree = (m)->m_next;} \
	  splx(ms); }
#define	MFREE(m, n) \
	{ int ms = splimp(); \
	  if ((m)->m_free) panic("mfree"); (m)->m_free = 1; \
	  if ((m)->m_off > MSIZE) { \
		(n) = (struct mbuf *)(mtod(m, int)&~0x3ff); \
		if (--mclrefcnt[mtocl(n)] == 0) \
		    { (n)->m_next = mclfree;mclfree = (n);mbstat.m_clfree++;} \
	  } \
	  (n) = (m)->m_next; (m)->m_next = mfree; \
	  (m)->m_off = 0; (m)->m_act = 0; mfree = (m); mbstat.m_mbfree++; \
	  splx(ms); }

/*
 * Mbuf statistics.  Clients can committ hunks of space until we are
 * overcommitted by the fraction represented by MBUFOVERALLOCFRAG.
 * We keep track of the amount of space committed, the number
 * of mbufs and clusters allocated from the free memory pool, and
 * the number of mbufs and clusters on our free lists.
 */
#define	MBUFOVERALLOCFRACTION	3 / 2		/* don't parenthesize ! */
struct mbstat {
	short	m_mbcommitted;		/* most we'll allow pool size to get */
	short	m_mbufs;		/* mbufs obtained from page pool */
	short	m_mbfree;		/* mbufs on our free list */
	short	m_clusters;		/* clusters obtained from page pool */
	short	m_clfree;		/* free clusters */
	short	m_drops;		/* times failed to find space */
};

#ifdef	KERNEL
extern	struct mbuf mbutl[];		/* virtual address of net free mem */
extern	struct pte Mbmap[];		/* page tables to map Netutl */
struct	mbstat mbstat;
extern	int nmbclusters;
struct	mbuf *mfree, *mclfree;
char	mclrefcnt[NMBCLUSTERS];
struct	mbuf *m_get(),*m_getclr(),*m_free(),*m_more(),*m_copy(),*m_pullup();
caddr_t	m_clalloc();
#endif
