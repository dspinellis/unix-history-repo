/* mbuf.h 4.3 81/11/08 */

/*
 * Constants related to memory allocator.
 */
#define	PGSIZE		1024
#define	MSIZE		128			/* size of an mbuf */
#define	MMINOFF		12			/* mbuf header length */
#define	MTAIL		4
#define	MMAXOFF		(MSIZE-MTAIL)		/* offset where data ends */
#define	MLEN		(MSIZE-MMINOFF-MTAIL)	/* mbuf data length */
#define	NMBPAGES	256

/*
 * Macros for type conversion
 *
 * CONSTANTS HERE ARE A CROCK
 */

/* network page map number to virtual address, and back */
#define	pftom(x) ((struct mbuf *)((x << 10) + (int)mbutl))
#define	mtopf(x) ((((int)x & ~0x3ff) - (int)mbutl) >> 10)

/* address in mbuf to mbuf head */
#define	dtom(x)		((struct mbuf *)((int)x & ~0x7f))
#define	mtoff(x) 	(((int)x & 0x3ff) >> 7)
#define	mtod(x,t)	((t)((int)(x) + (x)->m_off))

struct mbuf {
	struct	mbuf *m_next;		/* next buffer in chain */
	u_long	m_off;			/* offset of data */
	short	m_len;			/* amount of data in this mbuf */
	short	m_cnt;			/* reference count */
	u_char	m_dat[MLEN];		/* data storage */
	struct	mbuf *m_act;		/* link in higher-level mbuf list */
};

#define	M_WAIT	1

#define	MGET(m, i) \
	{ int ms = splimp(); \
	  if ((m)=mfree) \
		{ mbstat.m_bufs--; mfree = (m)->m_next; (m)->m_next = 0; } \
	  else \
		(m) = m_more(i); \
	  splx(ms); }
#define	MPGET(m, i) \
	{ int ms = splimp(); \
	  if ((m)=mpfree) \
	      { ++mprefcnt[mtopf(m)]; nmpfree--; mpfree = (m)->m_next; } \
	  splx(ms); }
#define	MFREE(m, n) \
	{ int ms = splimp(); \
	  if ((m)->m_off > MSIZE) { \
		(n) = (struct mbuf *)(mtod(m, int)&~0x3ff); \
		if (--mprefcnt[mtopf(n)] == 0) \
		    { (n)->m_next = mpfree; mpfree = (n); nmpfree++; } \
	  } \
	  (n) = (m)->m_next; (m)->m_next = mfree; \
	  (m)->m_off = 0; (m)->m_act = 0; mfree = (m); mbstat.m_bufs++; \
	  splx(ms); }
#define	NMBPG (PGSIZE/MSIZE)		/* mbufs/page */

struct mbstat {
	short	m_bufs;			/* # free msg buffers */
	short	m_hiwat;		/* # free mbufs allocated */
	short	m_lowat;		/* min. # free mbufs */
	short	m_pages;		/* # pages owned by network */
	short	m_drops;		/* times failed to find space */
};

#ifdef	KERNEL
extern	struct mbuf mbutl[];		/* virtual address of net free mem */
extern	struct pte Mbmap[];		/* page tables to map Netutl */
struct	mbstat mbstat;
int	nmbpages;
struct	mbuf *mfree, *mpfree;
int	nmpfree;
char	mprefcnt[NMBPAGES];
struct	mbuf *m_get(), *m_free(), *m_more();
#endif
