/* mbuf.h 4.1 81/10/29 */

/*
 * Constants related to memory allocator.
 */
#define	PGSIZE		1024
#define	MSIZE		128			/* size of an mbuf */
#define	MMINOFF		12			/* mbuf header length */
#define	MTAIL		4
#define	MMAXOFF		(MSIZE-MTAIL)		/* offset where data ends */
#define	MLEN		(MSIZE-MMINOFF-MTAIL)	/* mbuf data length */
#define	NMPAGES		256

/*
 * Macros for type conversion
 *
 * CONSTANTS HERE ARE A CROCK
 */

/* network page map number to virtual address, and back */
#define	pftom(x) ((struct mbuf *)((x << 10) + (int)netutl))
#define	mtopf(x) ((((int)x & ~0x3ff) - (int)netutl) >> 10)

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
struct	mbuf *mfree, *mpfree;
int	nmpfree;
char	mprefcnt[NMPAGES];
struct	mbuf *m_get(), *m_free(), *m_more();

#define	MGET(m, i) \
	{ int ms = spl_imp(); \
	  if ((m)=mfree) \
		{ netcb.n_bufs--; mfree = (m)->m_next; (m)->m_next = 0; } \
	  else \
		(m) = m_more(i); \
	  splx(ms); }
#define	MPGET(m, i) \
	{ int ms = spl_imp(); \
	  if ((m)=mpfree) \
	      { ++mprefcnt[mtopf(m)]; nmpfree--; mpfree = (m)->m_next; } \
	  splx(ms); }
#define	MFREE(m, n) \
	{ int ms = spl_imp(); \
	  if ((m)->m_off > MSIZE) { \
		(n) = (struct mbuf *)(mtod(m, int)&~0x3ff); \
		if (--mprefcnt[mtopf(n)] == 0) \
		    { (n)->m_next = mpfree; mpfree = (n); nmpfree++; } \
	  } \
	  (n) = (m)->m_next; (m)->m_next = mfree; \
	  (m)->m_off = 0; (m)->m_act = 0; mfree = (m); netcb.n_bufs++; \
	  splx(ms); }
#define	NMBPG (PGSIZE/MSIZE)		/* mbufs/page */

#ifdef	KERNEL
extern	struct mbuf netutl[];		/* virtual address of net free mem */
extern	struct pte Netmap[];		/* page tables to map Netutl */
#endif
	short	n_bufs;				/* # free msg buffers */
	short	n_hiwat;			/* # free mbufs allocated */
	short	n_lowat;			/* min. # free mbufs */
	short	n_pages;			/* # pages owned by network */
