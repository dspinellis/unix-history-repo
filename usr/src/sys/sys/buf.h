/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)buf.h	7.20 (Berkeley) %G%
 */

#ifndef _BUF_H_
#define	_BUF_H_
#include <sys/queue.h>

/*
 * The header for buffers in the buffer pool and otherwise used
 * to describe a block i/o request is given here.
 *
 * Each buffer in the pool is usually doubly linked into 2 lists:
 * hashed into a chain by <dev,blkno> so it can be located in the cache,
 * and (usually) on (one of several) queues.  These lists are circular and
 * doubly linked for easy removal.
 *
 * There are currently three queues for buffers:
 *	one for buffers which must be kept permanently (super blocks)
 * 	one for buffers containing ``useful'' information (the cache)
 *	one for buffers containing ``non-useful'' information
 *		(and empty buffers, pushed onto the front)
 * The latter two queues contain the buffers which are available for
 * reallocation, are kept in lru order.  When not on one of these queues,
 * the buffers are ``checked out'' to drivers which use the available list
 * pointers to keep track of them in their i/o active queues.
 */

struct buf {
	volatile long	b_flags;	/* too much goes here to describe */
	struct 	queue_entry b_hash;	/* hash chain */
	struct 	queue_entry b_vnbufs;	/* associated vnode */
	struct 	queue_entry b_freelist;	/* position on free list if not BUSY */
	struct	buf *b_actf, **b_actb;  /* device driver I/O queue when BUSY */
	long	b_bcount;		/* transfer count */
	long	b_bufsize;		/* size of allocated buffer */
	short	b_error;		/* returned after I/O */
	dev_t	b_dev;			/* major+minor device name */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    struct fs *b_fs;		/* UFS superblocks */
	    struct lfs *b_lfs;		/* LFS superblocks */
	    struct csum *b_cs;		/* superblock summary information */
	    struct cg *b_cg;		/* cylinder group block */
	    struct dinode *b_dino;	/* ilist */
	    daddr_t *b_daddr;		/* indirect block */
	} b_un;
	daddr_t	b_lblkno;		/* logical block number */
	daddr_t	b_blkno;		/* block # on device */
#ifdef SECSIZE
	long	b_blksize;		/* size of device blocks */
#endif SECSIZE
	long	b_resid;		/* words not transferred after error */
	struct  proc *b_proc;		/* proc doing physical or swap I/O */
	void	(*b_iodone)();		/* function called by iodone */
	struct	vnode *b_vp;		/* vnode for dev */
	int	b_pfcent;		/* center page when swapping cluster */
	struct	ucred *b_rcred;		/* ref to read credentials */
	struct	ucred *b_wcred;		/* ref to write credendtials */
	int	b_dirtyoff;		/* offset in buffer of dirty region */
	int	b_dirtyend;		/* offset of end of dirty region */
	caddr_t	b_saveaddr;		/* original b_addr for PHYSIO */
	int	b_validoff;		/* offset in buffer of valid region */
	int	b_validend;		/* offset of end of valid region */
};

/*
 * Defines for device drivers.
 */
#define	b_active b_bcount		/* driver queue head: drive active */
#define	b_errcnt b_resid		/* while i/o in progress: # retries */

#ifdef KERNEL
struct	buf *buf;		/* the buffer pool itself */
char	*buffers;
int	nbuf;			/* number of buffer headers */
int	bufpages;		/* number of memory pages in the buffer pool */
struct	buf *swbuf;		/* swap I/O headers */
int	nswbuf;
struct	buf bswlist;		/* head of free swap header list */
struct	buf *bclnlist;		/* head of cleaned page list */

__BEGIN_DECLS
int	allocbuf __P((struct buf *, int));
int	bawrite __P((struct buf *));
int	bdwrite __P((struct buf *));
void	biodone __P((struct buf *));
int	biowait __P((struct buf *));
int	bread __P((struct vnode *, daddr_t, int,
	    struct ucred *, struct buf **));
int	breadn __P((struct vnode *, daddr_t, int, daddr_t *, int *, int,
	    struct ucred *, struct buf **));
int	brelse __P((struct buf *));
void	bufinit __P((void));
int	bwrite __P((struct buf *));
struct buf *getblk __P((struct vnode *, daddr_t, int));
struct buf *geteblk __P((int));
struct buf *getnewbuf __P((void));
int	incore __P((struct vnode *, daddr_t));
u_int	minphys __P((struct buf *bp));
__END_DECLS
#endif

/*
 * These flags are kept in b_flags.
 */
#define	B_WRITE		0x000000	/* non-read pseudo-flag */
#define	B_READ		0x000001	/* read when I/O occurs */
#define	B_DONE		0x000002	/* transaction finished */
#define	B_ERROR		0x000004	/* transaction aborted */
#define	B_BUSY		0x000008	/* not on av_forw/back list */
#define	B_PHYS		0x000010	/* physical IO */
#define	B_XXX		0x000020	/* was B_MAP, alloc UNIBUS on pdp-11 */
#define	B_WANTED	0x000040	/* issue wakeup when BUSY goes off */
#define	B_AGE		0x000080	/* delayed write for correct aging */
#define	B_ASYNC		0x000100	/* don't wait for I/O completion */
#define	B_DELWRI	0x000200	/* write at exit of avail list */
#define	B_TAPE		0x000400	/* this is a magtape (no bdwrite) */
#define	B_UAREA		0x000800	/* add u-area to a swap operation */
#define	B_PAGET		0x001000	/* page in/out of page table space */
#define	B_DIRTY		0x002000	/* dirty page to be pushed out async */
#define	B_PGIN		0x004000	/* pagein op, so swap() can count it */
#define	B_CACHE		0x008000	/* did bread find us in the cache ? */
#define	B_INVAL		0x010000	/* does not contain valid info  */
#define	B_LOCKED	0x020000	/* locked in core (not reusable) */
#define	B_HEAD		0x040000	/* a buffer header, not a buffer */
#define	B_GATHERED	0x080000	/* LFS: already in a segment */
#define	B_BAD		0x100000	/* bad block revectoring in progress */
#define	B_CALL		0x200000	/* call b_iodone from iodone */
#define	B_RAW		0x400000	/* set by physio for raw transfers */
#define	B_NOCACHE	0x800000	/* do not cache block after use */

#define	iodone	biodone
#define	iowait	biowait

/*
 * Zero out a buffer's data portion.
 */
#define	clrbuf(bp) { \
	blkclr((bp)->b_un.b_addr, (unsigned)(bp)->b_bcount); \
	(bp)->b_resid = 0; \
}
#define B_CLRBUF	0x1	/* request allocated buffer be cleared */
#define B_SYNC		0x2	/* do all allocations synchronously */
#endif /* !_BUF_H_ */
